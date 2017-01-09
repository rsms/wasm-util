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
  writeBytes(v :Iterable<byte>)
}

//——————————————————————————————————————————————————————————————————————————————

const o = (x :any) => x

function sumf<A>(a :Array<A>, f :(a:A)=>number, initial? :number) :number {
  return a.reduce((s, v) => s + f(v), initial || 0)
}

class N {
  length   :uint32
  value    :any
  children :N[]
  repr(indent :string) :string { return '' }
  emit(w :BinWriter) :void {}

  static reprn(nodes :N[] | null, indent :string) {
    let s = '';
    if (nodes && nodes.length) {
      indent += '  '
      s += ' ' + nodes.map(c => c.repr(indent)).join(' ');
    }
    return s;
  }
}

class Branch extends N {
  constructor(length :uint32, children :N[]) {
    super()
    this.length = length
    this.children = children
  }
  repr(indent :string) :string {
    return (indent ? '\n' + indent : '') + '(' +
           this.constructor.name.toLowerCase() +
           N.reprn(this.children, indent) + ')'
  }
  emit(w :BinWriter) :void {
    this.children.forEach(c => c.emit(w))
  }
}

class Byte extends N {
  constructor(value :byte, repr? :(indent:string)=>string) { super()
    this.value = value
    this.length = 1
    if (repr) { this.repr = repr }
  }
  repr(indent :string) :string {
    return (indent ? '\n' + indent : '') + '(byte 0x'+this.value.toString(16)+')'
  }
  emit(w :BinWriter) :void { w.writeUint8(this.value) }
}

class Uint32 extends N {
  constructor(value :uint32) { super()
    this.value = value
    this.length = 4
  }
  repr(indent :string) :string { return this.value.toString() }
  emit(w :BinWriter) :void { w.writeUint32(this.value) }
}

class VarUint extends N {
  bytes :byte[]
  constructor(value :uint32) { super()
    this.value = value
    let bytes = []
    do {
      let b = value & 0x7f;
      value >>= 7;
      if (value != 0) { b |= 0x80 }
      bytes.push(b)
    } while (value != 0);
    this.bytes = bytes
    this.length = bytes.length
  }
  repr(indent :string) :string { return this.value.toString() }
  emit(w :BinWriter) :void { w.writeBytes(this.bytes) }
}

class Module extends Branch {
  constructor(version :uint32, ...sections :N[]) {
    super(
      sumf(sections, s => s.length),
      [new Uint32(0x6d736100), new Uint32(version)].concat(sections),
    )
  }
}

class StdSection extends Branch {
  constructor(id :uint7, data :N[]) {
    let idN = new VarUint(id)
    let payloadLen = sumf(data, s => s.length);
    let payloadLenN = new VarUint(payloadLen)
    super(
      idN.length + payloadLenN.length + payloadLen,
      [idN, payloadLenN, ...data]
    )
  }
  static nameOfId(id :uint32) {
    switch (id) {
      case 0:  return 'custom';
      case 1:  return 'type';      // Function signature declarations
      case 2:  return 'import';    // Import declarations
      case 3:  return 'function';  // Function declarations
      case 4:  return 'table';     // Indirect function table and other tables
      case 5:  return 'memory';    // Memory attributes
      case 6:  return 'global';    // Global declarations
      case 7:  return 'export';    // Exports
      case 8:  return 'start';     // Start function declaration
      case 9:  return 'element';   // Elements section
      case 10: return 'code';      // Function bodies (code)
      case 11: return 'data';      // Data segments
      default: return '?'
    }
  }
  repr(indent :string) :string {
    let s = (indent ? '\n' + indent : '') + '(section ';
    let idN = this.children[0];
    return s + StdSection.nameOfId(idN.value) +
       N.reprn(this.children.slice(1), indent) + ')'
  }
}

// language types
const I32  = new Byte(0x7f, (indent :string) => 'i32')
const I64  = new Byte(0x7e, (indent :string) => 'i64')
const F32  = new Byte(0x7d, (indent :string) => 'f32')
const F64  = new Byte(0x7c, (indent :string) => 'f64')
const Func = new Byte(0x60, (indent :string) => 'func')
const AnyFunc = new Byte(0x70, (indent :string) => 'anyfunc')

const ZeroByte = new Byte(0, (indent :string) => '0')
const OneByte = new Byte(1, (indent :string) => '1')

class FuncType extends Branch {
  constructor(paramTypes :N[], returnTypes :N[]) {
    //assert(returnTypes.length < 2) // current version only supports 1 or 0
    let paramCountN = new VarUint(paramTypes.length)
    let returnCountN = new VarUint(returnTypes.length)
    let children = [
      Func, paramCountN, ...paramTypes, returnCountN, ...returnTypes];
    super(sumf(children, n => n.length), children)
  }
  repr(indent :string) :string {
    let s = [(indent ? '\n' + indent : '') + '(func_type'];
    let retCountIndex = this.children[1].value + 2;
    indent += '  '
    this.children.forEach((c, i) => {
      if (i > 1) {
        if (i == retCountIndex) {
          s.push('::');
        } else {
          s.push(c.repr(indent))
        }
      }
    })
    return s.join(' ') + ')';
  }
}

class TypeSection extends StdSection {
  constructor(...funcTypes :FuncType[]) {
    super(1, [new VarUint(funcTypes.length), ...funcTypes])
  }
}

class FunctionSection extends StdSection {
  constructor(typeIndex :uint32[]) {
    super(
      3,
      [new VarUint(typeIndex.length), ...typeIndex.map(i => new VarUint(i))]
    )
  }
}

class ResizableLimits extends Branch {
  constructor(initial :uint32, maximum? :uint32) {
    let initialN = new VarUint(initial)
    let children = maximum ? [OneByte, initialN, new VarUint(maximum)]
                           : [ZeroByte, initialN];
    super(sumf(children, n => n.length), children)
  }
  repr(indent :string) :string {
    let s = (indent ? '\n' + indent : '') + '(resizable_limits ';
    s += this.children[1].value;
    if (this.children.length == 3) {
      s += ' ' + this.children[2].value
    }
    return s + ')'
  }
}

// A table is similar to a linear memory whose elements, instead of
// being bytes, are opaque values of a particular table element type.
// This allows the table to contain values—like GC references,
// raw OS handles, or native pointers—that are accessed by WebAssembly
// code indirectly through an integer index. This feature bridges the
// gap between low-level, untrusted linear memory and high-level opaque
// handles/references at the cost of a bounds-checked table indirection.

class TableType extends Branch {
  constructor(elemType :N, limits :ResizableLimits) {
    let children = [elemType, limits];
    super(sumf(children, n => n.length), children)
  }
}

class TableSection extends StdSection {
  constructor(...tableTypes :TableType[]) {
    super(4, [new VarUint(tableTypes.length), ...tableTypes])
  }
}

//——————————————————————————————————————————————————————————————————————————————

let mod = new Module(0xd,
  new TypeSection(
    new FuncType([I32,I32],[I32]), // (Int32, Int32) Int32
    new FuncType([I32],[I32]),     // (Int32) Int32
    new FuncType([I64,I32],[I64])  // (Int64, Int32) Int64
  ),
  new FunctionSection([0,0,1,2]),
  new TableSection(
    new TableType(AnyFunc, new ResizableLimits(0, 3))
  )
)

console.log(mod.repr(''))

//——————————————————————————————————————————————————————————————————————————————

class WasmBuf implements BinWriter {
  buf      :ArrayBuffer
  byteView :Uint8Array
  view     :DataView
  length   :number

  constructor(size :uint) {
    this.buf = new ArrayBuffer(size as number);
    this.byteView = new Uint8Array(this.buf)
    this.view = new DataView(this.buf);
    this.length = 0;
  }

  writeBytes(bytes :byte[]) {
    this.byteView.set(bytes, this.length)
    this.length += bytes.length
  }

  writeUint32(v :uint32) {
    this.view.setUint32(this.length, v as number, true)
    this.length += 4
  }

  writeUint16(v :uint16) {
    this.view.setUint16(this.length, v as number, true)
    this.length += 2
  }

  writeUint8(v :uint8) {
    this.view.setUint8(this.length++, v as number)
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

  repr(write :(s:string)=>void, limit? :number) {
    limit = Math.min(this.length, limit || Infinity)
    let s = [];
    for (let b of this.byteView) {
      if (b == 0) {
        s.push('\x1B[2m00\x1B[0m')
      } else {
        let str = b.toString(16);
        s.push(str.length == 1 ? '\x1B[2m0\x1B[0m' + str : str)
      }
      if (s.length == 9) {
        write(s.join(' '));
        s = [];
      } else if (s.length == 4) {
        s.push('')
      }
      if (--limit < 1) {
        break;
      }
    }
    if (s.length) {
      write(s.join(' '));
    }
  }
}

let buf = new WasmBuf(1024)
mod.emit(buf)
buf.repr(s => console.log(s))
