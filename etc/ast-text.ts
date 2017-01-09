type uint = Number

class Token {
  value    :string
  length   :uint
  isTrivia :boolean
  constructor(value :string, isTrivia :boolean) {
    this.value = value;
    this.length = value.length;
    this.isTrivia = isTrivia;
  }
}
class Trivia { v:string; constructor(v:string) { this.v = v; } }
const Trv = (value :string) => new Trivia(value)
const Tok = (...values :(string|Trivia)[]) =>
  values.map(v =>
    v instanceof Trivia ? new Token(v.v, true) : new Token(v, false)
  )

class GN {
  value    :string
  length   :uint
  children :GN[] | null
  tokensL  :Token[] | null
  tokensR  :Token[] | null
  tokens   :Token[] | null

  constructor(
    value :string,
    length :uint,
    tokensL :Token[] | null,
    tokensR :Token[] | null,
    ...children :GN[])
  {
    this.value = value;
    this.length = length;
    this.children = children.length ? children : null;
    this.tokensL = tokensL;
    this.tokensR = tokensR;
  }

  repr() {
    let s = '';
    if (this.tokensL) {
      this.tokensL.forEach(t => s += t.value);
      console.log('*s is "'+s+'"')
    }
    console.log('%s is "'+s+'"')
    if (this.children) {
      this.children.forEach(cn => s += cn.repr())
    }
    console.log('^s is "'+s+'"')
    if (this.tokensR) {
      this.tokensR.forEach(t => s += t.value)
      console.log('#s is "'+s+'"')
    }
    console.log('s is "'+s+'"')
    return s;
  }
}

function gn(
  value :string,
  length :uint,
  tokensL :Token[] | null,
  tokensR :Token[] | null,
  ...children :GN[]) :GN
{
  return new GN(value, length, tokensL, tokensR, ...children)
}


interface RN {
  parent   :RN
  children :GN[]
  pos      :uint
}

class Tree {
  g :GN
  r :RN | null

  constructor(g :GN, r? :RN) {
    this.g = g; this.r =  r || null;
  }
  repr() {
    return `Tree{${this.g.repr()}}`
  }
}

//——————————————————————————————————————————————————————————————————————————————
//——————————————————————————————————————————————————————————————————————————————

type uint32 = uint;
type uint16 = uint;
type uint8 = uint;
type byte = uint8;





class WasmBuf {
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


// .from([
//   0x6d,0x73,0x61,0,   // \nasm
//   0,   0,   0,   0xd, // version
// ])

// [ '\0asm', 0xd
// ]

const src = `
func bigger(foo Int, bar Int) Bool {
  return foo > bar
}`

const t = new Tree(
  gn('func', 57, Tok('func', Trv(' ')), [],                      // "func " ...
    gn('bigger', 6, Tok('bigger'), []),                          // "bigger"
    gn('params', 19, Tok('('), Tok(')', Trv(' ')),               // "(" ... ") "
      gn('param', 9, [], Tok(',', Trv(' ')),                     // [0][1] ", "
        gn('id', 3, Tok('foo', Trv(' ')), []),                   // "foo "
        gn('type', 3 , Tok('Int'), [])                           // "Int"
      ),
      gn('param', 7, [], [],                                     // [0][1] ""
        gn('id', 3, Tok('bar', Trv(' ')), []),                   // "bar "
        gn('type', 3 , Tok('Int'), [])                           // "Int"
      )
    ),
    gn('type', 5, Tok('Bool', Trv(' ')), []),                    // "Bool "
    gn('block', 22, Tok(Trv('{')), Tok(Trv('}')),                // "{"..."}"
      gn('ret', 20, Tok(Trv('\n  '), 'return'), Tok(Trv('\n')),  // "\n  return "[0]"\n"
        gn('binop', 9, Tok('>', Trv(' ')), [],                   // [0]"> "[1]
          gn('id', 3, Tok('foo', Trv(' ')), []),                 // "foo "
          gn('id', 3, Tok('bar'), []) // "> "...                 // "bar"
        )
      )
    )
  )
);

// console.log('t.g:', t.g.repr())
