// Linear bytecode textual representation.
// https://github.com/WebAssembly/design/blob/master/TextFormat.md
//
import { t, N, AnyOp, VarUint7 }                   from './ast'
import { utf8 }                                    from './utf8'
import { uint8, uint16, uint32, float32, float64 } from './basic-types';
import { opcodes }                                 from './info'

// Maps opname to opcode
const opnames = new Map<string,uint8>()
for (let e of opcodes.entries()) { opnames.set(e[1], e[0]) }


export type Writer = (s :string)=>void

interface Ctx {
  writeln(depth :number, s :string)
}

function readVarInt7(byte :uint8) {
  return byte < 64 ? byte : -(128 - byte)
}

function fmtimm(n :N) {
  switch (n.t) {
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
      return n.v.toString(10)
    }
    case t.varint7:       {
      return readVarInt7(n.v).toString(10)
    }
    case t.type: {
      switch (n.v) {
        case -1:    return 'i32'
        case -2:    return 'i64'
        case -3:    return 'f32'
        case -4:    return 'f64'
        case -0x10: return 'anyfunc'
        case -0x20: return 'func'
        case -0x40: return 'void' // aka empty_block
        default:
          throw new Error('unexpected type ' + n.t.toString())
      }
    }
    default:
      throw new Error('unexpected imm ' + n.t.toString())
  }
}

function fmtimmv(n :N[]) { // " A B" if n=[A,B]; "" if n=[] (leading space)
  return n.map(n => ' ' + fmtimm(n)).join('')
}

function visitOps(nodes :N[], c :Ctx, depth :number) {
  for (let n of nodes) {
    visitOp(n, c, depth)
  }
}

function visitOp(n :N, c :Ctx, depth :number) {
  // const tname = style(symname(n.t), '92')
  switch (n.t) {
    case t.instr: {
      if (n.v == 0x0b/*end*/ || n.v == 0x05/*else*/) {
        depth--
      }
      return c.writeln(depth, opcodes.get(n.v))
    }

    case t.instr_imm1: {
      return c.writeln(depth, opcodes.get(n.v) + ' ' + fmtimm((n as AnyOp).imm as N))
    }

    case t.instr_pre: {
      visitOps((n as AnyOp).pre as N[], c, depth)
      return c.writeln(depth, opcodes.get(n.v))
    }

    case t.instr_pre1: {
      visitOp((n as AnyOp).pre as N, c, depth)
      return c.writeln(depth, opcodes.get(n.v))
    }

    case t.instr_imm1_post: {
      c.writeln(depth, opcodes.get(n.v) + ' ' + fmtimm((n as AnyOp).imm as N))
      return visitOps((n as AnyOp).post as N[], c, depth + 1)
    }

    case t.instr_pre_imm: {
      visitOps((n as AnyOp).pre as N[], c, depth)
      return c.writeln(depth, opcodes.get(n.v) + fmtimmv((n as AnyOp).imm as N[]))
    }

    case t.instr_pre_imm_post: {
      visitOps((n as AnyOp).pre as N[], c, depth)
      c.writeln(depth, opcodes.get(n.v) + fmtimmv((n as AnyOp).imm as N[]))
      visitOps((n as AnyOp).post as N[], c, depth + 1)
      break
    }

    default:
      throw new Error('unexpected op ' + n.t.toString())
  }
}

export function printCode(instructions :N[], writer :Writer) {
  const SP = '                                                      '
  const ctx = {
    writeln(depth :number, chunk :string) {
      writer(SP.substr(0, depth * 2) + chunk + '\n')
    },
  }

  visitOps(instructions, ctx, 0)
}
