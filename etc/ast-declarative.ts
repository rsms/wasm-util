type uint = number
type uint32 = uint
type uint16 = uint
type uint8 = uint
type uint7 = uint
type byte = uint8

//——————————————————————————————————————————————————————————————————————————————

interface BinWriter {
  writeUint32(v :uint32)
  writeUint16(v :uint16)
  writeUint8(v :uint8)
  writeVarUint(v :number)
}

//——————————————————————————————————————————————————————————————————————————————

const o = (x :any) => x

function varUintLen(v :number) {
  let len = 0;
  do {
    v >>= 7;
    ++len;
  } while (v != 0);
  return len;
}


interface N {
  repr() :string
  write(w :BinWriter) :void
  length :uint32
}

interface Term extends N {
  value :any
}

interface Branch extends N {
  children :N[]
}

const BaseNode = {
  repr(indent? :string) {
    // indent = indent || ''
    // let s = indent + '(' + this.type
    // if (this.children) {
    //   s += '\n'
    //   this.children.forEach(cn => s += cn.repr(indent + '  '))
    //   s += indent;
    // }
    // s += ')\n'
    // return s;
    return 'node';
  }
}

const Magic = { __proto__:BaseNode,
  value: '\0asm',
  length:4,
  write(w :BinWriter) { w.writeUint32(0x6d736100) }
}

const Uint32 = (value :uint32) => o({
  __proto__:BaseNode,
  value,
  length: 4,
  write(w :BinWriter) { w.writeUint32(this.value) }
})

const VarUint32 = (value :uint32) => {
  // assert(varUintLen(value) <= 5) // ceil(32/7)
  return {
    __proto__:BaseNode,
    value,
    length: varUintLen(value),
    write(w :BinWriter) { w.writeVarUint(this.value) }
  }
}

const VarUint7 = (value :uint7) => {
  // assert(varUintLen(value) == 1) // ceil(7/7)
  return {
    __proto__:BaseNode,
    value,
    length: varUintLen(value),
    write(w :BinWriter) { w.writeVarUint(this.value) }
  }
}

const VarUint1 = (value :uint8) => {
  // assert(value == 1 || value == 0)
  return {
    __proto__:BaseNode,
    value,
    length: 1,
    write(w :BinWriter) { w.writeUint8(this.value) }
  }
}

const Uint8 = (value :uint8) => o({ __proto__:BaseNode,
  value,
  length: 1,
  write(w :BinWriter) { w.writeUint8(this.value) }
})

const ZeroByte = Uint8(0)

const Version = Uint32

const Module = (version :Term, ...sections :Branch[]) => o({
  __proto__: BaseNode,
  children: [Magic, version, ...sections],
  write(w :BinWriter) {} // TODO
})

const CustomSection = (name :string, ...data :N[]) => o({
  // Note: Assumes name is an ASCII string. FIXME utf8 encode
  __proto__: BaseNode,
  children: [ZeroByte, null/*payload_len*/, VarUint32(name.length), String(name), ...data],
  write(w :BinWriter) {} // TODO
})

const StdSection = (id :uint7, ...data :N[]) => o({
  // assert(id > 0)
  __proto__: BaseNode,
  children: [VarUint7(id), null/*payload_len*/, ...data],
  write(w :BinWriter) {}
})

const TypeSection = (...types :Branch[]) => o({
  __proto__: BaseNode,
  
  children: [VarUint7(1), null/*payload_len*/, null/*count*/, ...types],
  write(w :BinWriter) {
    w.writeVarUint(children[0])
  }
})

//——————————————————————————————————————————————————————————————————————————————
//——————————————————————————————————————————————————————————————————————————————

let mod = Module(
  Version(0xd),
  TypeSection(1)
  // gn('sect',
  //   gl('id', 1),
  // )
)

class WasmBuf implements BinWriter {
  buf  :ArrayBuffer
  view :DataView
  offs :number

  constructor(size :uint) {
    this.buf = new ArrayBuffer(size as number);
    this.view = new DataView(this.buf);
    this.offs = 0;
  }

  writeHead(version :uint32) {
    this.writeUint32(0x6d736100) // \0asm
    this.writeUint32(version)
  }

  writeUint32(v :uint32) {
    this.view.setUint32(this.offs, v as number, true)
    this.offs += 4
  }

  writeUint16(v :uint16) {
    this.view.setUint16(this.offs, v as number, true)
    this.offs += 2
  }

  writeUint8(v :uint8) {
    this.view.setUint8(this.offs++, v as number)
  }

  writeVarUint(v :number) {
    do {
      let b = v & 0x7f;
      v >>= 7;
      if (v != 0) { b |= 0x80 }
      this.writeUint8(b)
    } while (v != 0);
  }

  // WriteSLeb128(int32_t value) {
  //   static const int kSignBitMask = 0x40;
  //   bool done;
  //   do {
  //     byte chunk = value & 0x7f;
  //     value >>= 7;
  //     done = ((value == 0) && ((chunk & kSignBitMask) == 0)) ||
  //            ((value == -1) && ((chunk & kSignBitMask) != 0));
  //     if (!done) chunk |= 0x80;
  //     WriteByte(chunk);
  //   } while (!done);
  // }
}

let buf = new WasmBuf(1024)
buf.writeHead(0xd)
buf.writeVarUint(1)


function dump(buf :ArrayBuffer, limit? :number) {
  let a8 = (new Uint8Array(buf, 0, limit || buf.byteLength))
  let s = [];
  for (let b of a8) {
    if (b == 0) {
      s.push('\x1B[2m00\x1B[0m')
    } else {
      let str = b.toString(16);
      s.push(str.length == 1 ? '\x1B[2m0\x1B[0m' + str : str)
    }
    if (s.length == 9) {
      console.log(s.join(' '));
      s = [];
    } else if (s.length == 4) {
      s.push('')
    }
  }
  if (s.length) {
    console.log(s.join(' '));
  }
}
// dump(buf.buf, 40)
