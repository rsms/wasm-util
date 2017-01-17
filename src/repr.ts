// repr(n :N, w :Writer, options? :Options) :void
// Represents a tree visually as plain text (with optional terminal colors.)
//
import { t, N, AnyOp, VarUint7 }                   from './ast'
import { utf8 }                                    from './utf8'
import { uint8, uint16, uint32, float32, float64 } from './basic-types';
import { opcodes }                                 from './info'

export type Writer = (s :string)=>void

export interface Options {
  readonly colors         :boolean  // explicitly enable or disable terminal colors
  readonly immSeparator   :string   // defaults to `:`
  readonly detailedTypes? :boolean, // `vi32(9)` or just `9`
}

interface Ctx {
  readonly write       :(depth :number, s :string)=>void
  readonly writeln     :(depth :number, s :string)=>void
  readonly writeinline :(s :string)=>void
  readonly options :Options
}

function symname(y :Symbol) {
  const s = y.toString()
  return s.length > 8 ? s.substr(7,s.length-8) : 'Symbol(?)';
}

// const indln  = (indent? :string) => (indent ? '\n' + indent : '')
const style  = (str :string, style :string) => '\x1B[' + style + 'm' + str + '\x1B[0m'
const reprop = (op :uint8) => style(opcodes.get(op), '96')
const styleType = (s :string) => style(s, '33')

function readVarInt7(byte :uint8) {
  return byte < 64 ? byte : -(128 - byte)
}

function visitAll(nodes :N[], c :Ctx, depth :number) {
  for (let n of nodes) {
    visit(n, c, depth)
  }
}

function visitCell(n :N, tname :string, c :Ctx, depth :number) {
  c.write(depth, '(' + tname)
  visitAll(n.v, c, depth + 1)
  c.write(depth, ')')
}

function visitValue(v :any, tname :string, c :Ctx, depth :number) {
  const s = String(v)
  return c.write(depth, c.options.detailedTypes ? `${tname}(${s})` : s)
}

function visit(n :N, c :Ctx, depth :number) {
  const tname = style(symname(n.t), '92')
  switch (n.t) {
    // Atoms
    case t.uint8:
    case t.uint16:
    case t.uint32:
    case t.varuint1:
    case t.varuint7:
    case t.varuint32:
    case t.varint32:
    case t.varint64:
    case t.float32:
    case t.float64: {
      return visitValue(n.v, tname, c, depth)
    }
    case t.varint7:       {
      return visitValue(readVarInt7(n.v), tname, c, depth)
    }

    case t.external_kind: {
      switch (n.v) {
        case 0:  return c.write(depth, styleType('external_kind.function'))
        case 1:  return c.write(depth, styleType('external_kind.table'))
        case 2:  return c.write(depth, styleType('external_kind.memory'))
        case 3:  return c.write(depth, styleType('external_kind.global'))
        default: return visitValue(readVarInt7(n.v), tname, c, depth)
      }
    }

    // Instructions
    case t.instr: {
      return c.write(depth, reprop(n.v))
    }

    case t.instr_imm1: {
      c.write(depth, '(' + reprop(n.v) + ' [')
      visit((n as AnyOp).imm as N, c, depth + 1)
      return c.write(depth, '])')
    }

    case t.instr_pre: {
      c.write(depth, '(' + reprop(n.v))
      visitAll((n as AnyOp).pre as N[], c, depth + 1)
      return c.writeln(depth , ')')
    }
    case t.instr_pre1: {
      c.write(depth, '(' + reprop(n.v))
      visit((n as AnyOp).pre as N, c, depth + 1)
      return c.writeln(depth, ')')
    }

    case t.instr_imm1_post: {
      c.write(depth, '(' + reprop(n.v) + ' [')
      visit((n as AnyOp).imm as N, c, depth + 1)
      c.write(depth, ']')
      visitAll((n as AnyOp).post as N[], c, depth + 1)
      return c.write(depth, ')')
    }

    case t.instr_pre_imm: {
      c.write(depth, '(' + reprop(n.v) + ' [')
      visitAll((n as AnyOp).imm as N[], c, depth + 1)
      c.write(depth, ']')
      visitAll((n as AnyOp).pre as N[], c, depth + 1)
      return c.write(depth, ')')
    }

    case t.instr_pre_imm_post: {
      c.write(depth, '(' + reprop(n.v) + ' [')
      visitAll((n as AnyOp).imm as N[], c, depth + 1)
      c.write(depth, ']')
      visitAll((n as AnyOp).pre as N[], c, depth + 1)
      visitAll((n as AnyOp).post as N[], c, depth + 1)
      return c.write(depth, ')')
    }

    // TODO: special case of "if"
    // {
    //   let s = ind(indent) + '(' + reprop(this.v)
    //
    //   if (this.v == 0x04/*if*/) {
    //     let cind = (indent || '') + '  '
    //
    //     s += ' ' + this.imm[0].repr() + // type
    //          this.pre[0].repr(cind) + // cond
    //          ind(cind) + '(then';
    //
    //     let i = 1, ci = (cind || '') + '  ', E = this.imm.length-1 // skip `end`
    //     for (; i < E; ++i) {
    //       let n = this.imm[i]
    //       if (n.v == 0x05/*else*/) {
    //         s += ')' // end of "then"
    //         break
    //       }
    //       s += ' ' + n.repr(ci)
    //     }
    //
    //     if (i < E) {
    //       s += ind(cind) + '(else';
    //       ++i
    //       for (; i < E; ++i) {
    //         let n = this.imm[i]
    //         s += ' ' + n.repr(ci)
    //       }
    //     }
    //
    //     return s + ') end)' // end of "then" or "else"
    //   }
    //
    //   return s + `${reprv(indent, this.imm)}${reprv(indent, this.pre)})`
    // }

    case t.data: {
      let v :string[] = []
      for (let i = 0, L = n.v.length; i != L; ++i) {
        if (v.length == 8) { v.push('...') ;break }
        v.push((n.v[i] as number).toString(16))
      }
      return c.write(depth, '(' + tname + ' ' + v.join(' ') + ')')
    }

    case t.type: {
      switch (n.v) {
        case -1:    return c.write(depth, styleType('i32'))
        case -2:    return c.write(depth, styleType('i64'))
        case -3:    return c.write(depth, styleType('f32'))
        case -4:    return c.write(depth, styleType('f64'))
        case -0x10: return c.write(depth, styleType('anyfunc'))
        case -0x20: return c.write(depth, styleType('func'))
        case -0x40: return c.write(depth, styleType('void')) // aka empty_block
        default:    return c.write(depth, styleType('?'))
      }
    }

    // —————————————————————————————————————————————————————————————————
    // Cells

    case t.module: {
      c.write(depth, '(' + tname + ' ' + n.v[1].v)
      visitAll(n.v.slice(2), c, depth + 1)
      return c.write(depth, ')')
    }

    case t.section: {
      const id = (n.v[0] as VarUint7).v
      const name = [
        'custom',   // 0 Custom named section
        'type',     // 1 Function signature declarations
        'import',   // 2 Import declarations
        'function', // 3 Function declarations
        'table',    // 4 Indirect function table and other tables
        'memory',   // 5 Memory attributes
        'global',   // 6 Global declarations
        'export',   // 7 Exports
        'start',    // 8 Start function declaration
        'element',  // 9 Elements section
        'code',     // 10 Function bodies (code)
        'data',     // 11 Data segments
      ][id] || ('0x' + id.toString(16))

      c.write(depth, '(' + tname + ' ' + name)
      visitAll(n.v.slice(1), c, depth + 1)
      return c.write(depth, ')')
    }

    case t.import_entry:
    case t.export_entry:
    case t.local_entry:
    case t.table_type:
    case t.memory_type: {
      return visitCell(n, tname, c, depth)
    }

    case t.func_type: {
      // func, paramCount, paramT*, retCount, retT* -> "(" paramT* ")" retT
      c.write(depth, '(' + tname + ' (')
      const paramCount = n.v[1].v as number
      const resCount = n.v[1 + paramCount + 1].v as number
      visitAll(n.v.slice(2, 2+paramCount), c, depth)
      c.writeinline(')')
      if (resCount > 0) {
        visitAll(n.v.slice(1 + paramCount + 2), c, depth)
      }
      return c.write(depth, ')')
    }

    case t.global_type: {
      c.write(depth, '(' + tname)
      visitAll(n.v.slice(0, n.v.length-1), c, depth + 1)
      c.write(depth + 1, (n.v[1].v ? ' mutable' : 'immutable'))
      return c.write(depth, ')')
    }

    case t.resizable_limits: {
      return c.write(depth,
        '(' + tname + ' ' + n.v[1].v + '..' + (n.v[0].v ? n.v[2].v : '') + ')'
      )
    }

    case t.global_variable:
    case t.init_expr:
    case t.elem_segment:
    case t.data_segment:
    case t.function_body: {
      return visitCell(n, tname, c, depth)
    }

    case t.str: {
      return c.write(depth, '"' + utf8.decode(n.v) + '"')
    }

    default:
      throw new Error('unexpected type ' + n.t.toString())
  }
}


export function reprBuffer(
  buffer          :ArrayBuffer,
  w               :Writer,
  limit?          :number,
  highlightRange? :number[],
  options?        :Options)
{
  limit = Math.min(buffer.byteLength, limit || Infinity)

  if (highlightRange && !highlightRange[1]) {
    highlightRange[1] = highlightRange[0]+1
  }

  let s = [], i = 0, view = new DataView(buffer)

  for (; i < limit; ++i) {
    let b = view.getUint8(i)
    const inHLRange = (highlightRange && i < highlightRange[1] && i >= highlightRange[0])
    if (b == 0 && !inHLRange) {
      s.push('\x1B[2m00\x1B[0m')
    } else {
      let str = b.toString(16)
      s.push(
        inHLRange ?
          ('\x1B[45;97m' + (str.length == 1 ? '0' : '') + str + '\x1B[0m' ) :
        str.length == 1 ? '\x1B[2m0\x1B[0m' + str :
        str
      )
    }
    if (s.length % 20 == 19) {
      w(s.join(' ') + '\n');
      s = [];
    } else if (s.length % 5 == 4) {
      s.push('')
    }
  }
  const tail = (highlightRange && highlightRange[0] >= i) ? '\x1B[45;97m..\x1B[0m' : ''
  if (s.length) {
    w(s.join(' ') + (tail ? ' ' + tail : '') + '\n');
  } else if (tail) {
    w(tail + '\n')
  }
}


export function repr(n :N, w :Writer, options? :Options) {
  const maxLineLength = 40
  const SP = '                                                      '

  let currLineLen = 0
  let lastCh = ''
  let writer = w

  if (!options || options.colors !== false) {
    // writer that colors immediate brackets
    let immOpen = style('[', '2'),
        immClose = style(']', '2')
    writer = s => {
      w(s.replace(/([^\x1B]|^)[\[\]]/g, m =>
        m.length == 2 ?
          m[0] + (m[1] == '[' ? immOpen : immClose) :
          (m[0] == '[' ? immOpen : immClose)
      ))
    }
  }

  const ctx = {

    options: Object.assign({
      // detailedTypes: true,
      immSeparator: ':',
    }, options || {}) as Options,

    writeln(depth :number, chunk :string) {
      const line = SP.substr(0, depth * 2) + chunk
      currLineLen = line.length
      lastCh = chunk[chunk.length-1]
      writer('\n' + line)
    },

    writeinline(chunk :string) {
      currLineLen += chunk.length
      lastCh = chunk[chunk.length-1]
      writer(chunk)
    },

    write(depth :number, chunk :string) {
      if (currLineLen > 0 && depth > 0) {
        const ch0 = chunk[0]
        if (ch0 == '(') {
          return this.writeln(depth, chunk)
        }
        if (ch0 != ')' && ch0 != ']' &&
            lastCh != '[' && lastCh != '(')
        {
          currLineLen += 1
          chunk = ' ' + chunk
        }
      }
      this.writeinline(chunk)
    },
  }

  visit(n, ctx, 0)
}

// Simple Writer that buffers everything as a string that 
export function BufferedWriter() {
  const buf = []
  const w = s => { buf.push(s) }
  w.toString = () => buf.join('')
  return w
}

// Convenience functions that returns strings.
// Will be slower and use more memory than `repr` but convenient for
// visualizing smaller structures.
export function strRepr(n :N, options? :Options) {
  const w = BufferedWriter()
  repr(n, w, options)
  return w.toString()
}

export function strReprBuffer(
  buffer          :ArrayBuffer,
  limit?          :number,
  highlightRange? :number[],
  options?        :Options)
{
  const w = BufferedWriter()
  reprBuffer(buffer, w, limit, highlightRange, options)
  return w.toString()
}
