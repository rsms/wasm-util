import {utf8} from './utf8'

declare function require(ref:string):any;
require('source-map-support').install()
type AssertFunc = (cond:any)=>void;
const assert :AssertFunc = require('assert')

type uint = number
type int32 = number
type int7 = number
type uint32 = uint
type uint16 = uint
type uint8 = uint
type uint7 = uint
type uint1 = uint
type byte = uint8

interface ForwardBinWriter {
  writeUint32(v :uint32)
  writeUint16(v :uint16)
  writeUint8(v :uint8)
  writeBytes(v :ArrayLike<byte>)
  writeArrayBuffer(b :ArrayBufferView)
}

function sumf<A>(a :Array<A>, f :(a:A)=>number, initial? :number) :number {
  return a.reduce((s, v) => s + f(v), initial || 0)
}

function symname(y :Symbol) {
  const s = y.toString()
  return s.length > 8 ? s.substr(7,s.length-8) : 'Symbol(?)';
}

// Node types
const t = {
  // Value types
  uint8:            Symbol('u8'),
  uint16:           Symbol('u16'),
  uint32:           Symbol('u32'),
  varuint1:         Symbol('vu1'),
  varuint7:         Symbol('vu7'),
  varuint32:        Symbol('vu32'),
  varint7:          Symbol('vs7'),
  varint32:         Symbol('vs32'),
  varint64:         Symbol('vs64'),
  bytes:            Symbol('bytes'),

  // Branch types
  module:           Symbol('module'),
  section:          Symbol('section'),
  import_entry:     Symbol('import_entry'),
  export_entry:     Symbol('export_entry'),
  local_entry:      Symbol('local_entry'),
  func_type:        Symbol('func_type'),
  table_type:       Symbol('table_type'),
  memory_type:      Symbol('memory_type'),
  global_type:      Symbol('global_type'),
  resizable_limits: Symbol('resizable_limits'),
  global_variable:  Symbol('global_variable'),
  init_expr:        Symbol('init_expr'),
  elem_segment:     Symbol('elem_segment'),
  data_segment:     Symbol('data_segment'),
  function_body:    Symbol('function_body'),

  // Instruction
  instr_v:          Symbol('instr_v'), // terminal
  instr_b:          Symbol('instr_b'), // has immediates or children
}

// Opcodes
const op = {
  // Control flow
  unreachable: 0x00, // trap immediately
  nop:         0x01, // no operation
  block:       0x02, // [block_type] begin a sequence of expressions
  loop:        0x03, // [block_type] begin a block which can also form CF loops
  if:          0x04, // [block_type] begin if expression
  else:        0x05, // begin else expression of if
  end:         0x0b, // end a block, loop, or if
  br:          0x0c, // [relative_depth :varuint32]
    // break that targets an outer nested block
  br_if:       0x0d, // [relative_depth :varuint32]
    // conditional break that targets an outer nested block
  br_table:    0x0e, // [br_table_imm] branch table control flow construct
  return:      0x0f, // return zero or one value from this function

  // Calling
  call:          0x10,  // [function_index :varuint32] call function by its index
  call_indirect: 0x11, // [type_index :varuint32, reserved :varuint1]
    // Call a function indirect with an expected signature.
    // Takes a list of function arguments and as the last operand the index into
    // the table.

  // Parametric
  drop:   0x1a, // ignore value
  select: 0x1b, // select one of two values based on condition

  // Variable access
  get_local:  0x20, // [local_index :varuint32] read a local variable or param
  set_local:  0x21, // [local_index :varuint32] write a local variable or param
  tee_local:  0x22, // [local_index :varuint32]
    // write a local variable or parameter and return the same value
  get_global: 0x23, // [global_index :varuint32] read a global variable
  set_global: 0x24, // [global_index :varuint32] write a global variable

  // Memory
  current_memory: 0x3f, // [reserved :varuint1] query the size of memory
  grow_memory:    0x40, // [reserved :varuint1] grow the size of memory

  // Type-specific operators
  i32: {
    // Constants
    const: 0x41, // [value :varint32] a constant value interpreted as i32

    // Memory
    load:     0x28, // [memory_immediate] load from memory
    load16_s: 0x2e, // [memory_immediate] load from memory
    load16_u: 0x2f, // [memory_immediate] load from memory
    load8_s:  0x2c, // [memory_immediate] load from memory
    load8_u:  0x2d, // [memory_immediate] load from memory
    store:    0x36, // [memory_immediate] store to memory
    store16:  0x3b, // [memory_immediate] store to memory
    store8:   0x3a, // [memory_immediate] store to memory

    // Comparison
    eqz:  0x45, // compare equal to zero (return 1 if operand is zero, else 0)
    eq:   0x46, // sign-agnostic compare equal
    ne:   0x47, // sign-agnostic compare unequal
    lt_s: 0x48, // signed less than
    lt_u: 0x49, // unsigned less than
    gt_s: 0x4a, // signed greater than
    gt_u: 0x4b, // unsigned greater than
    le_s: 0x4c, // signed less than or equal
    le_u: 0x4d, // unsigned less than or equal
    ge_s: 0x4e, // signed greater than or equal
    ge_u: 0x4f, // unsigned greater than or equal

    // Numeric
    clz:    0x67,
      // sign-agnostic count leading zero bits (All zero bits are considered
      // leading if the value is zero)
    ctz:    0x68,
      // sign-agnostic count trailing zero bits (All zero bits are considered
      // trailing if the value is zero)
    popcnt: 0x69, // sign-agnostic count number of one bits
    add:    0x6a, // sign-agnostic addition
    sub:    0x6b, // sign-agnostic subtraction
    mul:    0x6c, // sign-agnostic multiplication (lower 32-bits)
    div_s:  0x6d, // signed division (result is truncated toward zero)
    div_u:  0x6e, // unsigned division (result is floored)
    rem_s:  0x6f, // signed remainder (result has the sign of the dividend)
    rem_u:  0x70, // unsigned remainder
    and:    0x71, // sign-agnostic bitwise and
    or:     0x72, // sign-agnostic bitwise inclusive or
    xor:    0x73, // sign-agnostic bitwise exclusive or
    shl:    0x74, // sign-agnostic shift left
    shr_s:  0x75, // sign-replicating (arithmetic) shift right
    shr_u:  0x76, // zero-replicating (logical) shift right
    rotl:   0x77, // sign-agnostic rotate left
    rotr:   0x78, // sign-agnostic rotate right

    // Conversions
    wrap_i64:    0xa7, // wrap a 64-bit integer to a 32-bit integer
    trunc_s_f32: 0xa8, // truncate a 32-bit float to a signed 32-bit integer
    trunc_u_f32: 0xa9, // truncate a 32-bit float to an unsigned 32-bit integer
    trunc_s_f64: 0xaa, // truncate a 64-bit float to a signed 32-bit integer
    trunc_u_f64: 0xab, // truncate a 64-bit float to an unsigned 32-bit integer
    reinterpret_f32: 0xbc, // r.i. the bits of a 32-bit float as a 32-bit integer
  },

  i64: {
    // Constants
    const: 0x42, // [value :varint64] a constant value interpreted as i64

    // Memory
    load:     0x29, // [memory_immediate] load from memory
    load16_s: 0x32, // [memory_immediate] load from memory
    load16_u: 0x33, // [memory_immediate] load from memory
    load32_s: 0x34, // [memory_immediate] load from memory
    load32_u: 0x35, // [memory_immediate] load from memory
    load8_s:  0x30, // [memory_immediate] load from memory
    load8_u:  0x31, // [memory_immediate] load from memory
    store:    0x37, // [memory_immediate] store to memory
    store16:  0x3d, // [memory_immediate] store to memory
    store32:  0x3e, // [memory_immediate] store to memory
    store8:   0x3c, // [memory_immediate] store to memory
    
    // Comparison (matches i32; see description for i32)
    eqz:  0x50, eq:   0x51, ne:   0x52,
    lt_s: 0x53, lt_u: 0x54, gt_s: 0x55, gt_u: 0x56,
    le_s: 0x57, le_u: 0x58, ge_s: 0x59, ge_u: 0x5a,

    // Numeric (matches i32; see description for i32)
    clz: 0x79,   ctz:   0x7a, popcnt: 0x7b, add:   0x7c, sub:   0x7d,
    mul: 0x7e,   div_s: 0x7f, div_u:  0x80, rem_s: 0x81, rem_u: 0x82,
    and: 0x83,   or:    0x84, xor:    0x85, shl:   0x86, shr_s: 0x87,
    shr_u: 0x88, rotl:  0x89, rotr:   0x8a,

    // Conversions
    extend_s_i32: 0xac, // extend a signed 32-bit integer to a 64-bit integer
    extend_u_i32: 0xad, // extend an unsigned 32-bit integer to a 64-bit integer
    trunc_s_f32:  0xae, // truncate a 32-bit float to a signed 64-bit integer
    trunc_u_f32:  0xaf, // truncate a 32-bit float to an unsigned 64-bit integer
    trunc_s_f64:  0xb0, // truncate a 64-bit float to a signed 64-bit integer
    trunc_u_f64:  0xb1, // truncate a 64-bit float to an unsigned 64-bit integer
    reinterpret_f64: 0xbd, // r.i. the bits of a 64-bit float as a 64-bit integer
  },

  f32: {
    // Constants
    const: 0x43, // [value :uint32] a constant value interpreted as f32

    // Memory
    load:  0x2a, // [memory_immediate] load from memory
    store: 0x38, // [memory_immediate] store to memory

    // Comparison
    eq: 0x5b, // compare ordered and equal
    ne: 0x5c, // compare unordered or unequal
    lt: 0x5d, // compare ordered and less than
    gt: 0x5e, // compare ordered and greater than
    le: 0x5f, // compare ordered and less than or equal
    ge: 0x60, // compare ordered and greater than or equal

    // Numeric
    abs:      0x8b, // absolute value
    neg:      0x8c, // negation
    ceil:     0x8d, // ceiling operator
    floor:    0x8e, // floor operator
    trunc:    0x8f, // round to nearest integer towards zero
    nearest:  0x90, // round to nearest integer, ties to even
    sqrt:     0x91, // square root
    add:      0x92, // addition
    sub:      0x93, // subtraction
    mul:      0x94, // multiplication
    div:      0x95, // division
    min:      0x96,
      // minimum (binary operator); if either operand is NaN, returns NaN
    max:      0x97,
      // maximum (binary operator); if either operand is NaN, returns NaN
    copysign: 0x98, // copysign

    // Conversion
    convert_s_i32: 0xb2, // convert a signed 32-bit integer to a 32-bit float
    convert_u_i32: 0xb3, // convert an unsigned 32-bit integer to a 32-bit float
    convert_s_i64: 0xb4, // convert a signed 64-bit integer to a 32-bit float
    convert_u_i64: 0xb5, // convert an unsigned 64-bit integer to a 32-bit float
    demote_f64:    0xb6, // demote a 64-bit float to a 32-bit float
    reinterpret_i32: 0xbe, // r.i. the bits of a 32-bit integer as a 32-bit float
  },

  f64: {
    // Constants
    const: 0x44, // [value :uint64] a constant value interpreted as f64

    // Memory
    load:  0x2b, // [memory_immediate] load from memory
    store: 0x39, // [memory_immediate] store to memory
    
    // Comparison (matches f32; see description for f32)
    eq: 0x61, ne: 0x62, lt: 0x63, gt: 0x64, le: 0x65, ge: 0x66,

    // Numeric (matches f32; see description for f32)
    abs:     0x99, neg:  0x9a, ceil: 0x9b, floor:    0x9c, trunc: 0x9d,
    nearest: 0x9e, sqrt: 0x9f, add:  0xa0, sub:      0xa1, mul:   0xa2,
    div:     0xa3, min:  0xa4, max:  0xa5, copysign: 0xa6,

    // Conversions
    convert_s_i32: 0xb7, // convert a signed 32-bit integer to a 64-bit float
    convert_u_i32: 0xb8, // convert an unsigned 32-bit integer to a 64-bit float
    convert_s_i64: 0xb9, // convert a signed 64-bit integer to a 64-bit float
    convert_u_i64: 0xba, // convert an unsigned 64-bit integer to a 64-bit float
    promote_f32:   0xbb, // promote a 32-bit float to a 64-bit float
    reinterpret_i64: 0xbf, // r.i. the bits of a 64-bit integer as a 64-bit float
  },
   
}

// Maps opcode to name
const opnames = new Map<byte,string>()
for (const k of Object.keys(op)) {
  let v = op[k]
  if (typeof v == 'number') {
    opnames.set(op[k], k)
  } else {
    let kprefix = k + '.';
    for (let k2 of Object.keys(v)) {
      opnames.set(v[k2], kprefix + k2)
    }
  }
}

function sectionName(id :uint32) {
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
    default: return '?#' + id.toString()
  }
}

function langTypeName(v :int7) {
  switch (v) {
    case -1:    return 'i32'
    case -2:    return 'i64'
    case -3:    return 'f32'
    case -4:    return 'f64'
    case -0x10: return 'anyfunc'
    case -0x20: return 'func'
    case -0x40: return 'empty_block'
    default:    return symname(t.varint7) + ':' + v
  }
}

// Code emitter function type
type CodeEmitter = (w:ForwardBinWriter)=>void

// Node interface
interface N {
  readonly type     :Symbol
  readonly byteSize :uint32
  readonly emit     :CodeEmitter
  readonly repr     :(indent?:string)=>string
}

// Pure-branch node
class B implements N {
  readonly type     :Symbol
  readonly byteSize :uint32
  readonly children :N[]

  constructor(type :Symbol, children :N[]) {
    this.type = type
    this.byteSize = sumf(children, c => c.byteSize)
    assert(!isNaN(this.byteSize))
    this.children = children
  }

  repr(indent? :string) {
    const cindent :string = (indent ? indent + '  ' : '  ')
    const s = (indent ? '\n' + indent : '') + '(' + symname(this.type)
    let v :string[]
    switch (this.type) {
      case t.module: {
        v = [
          ' ' + (this.children[1] as V).value.toString(),
          ...this.children.slice(2).map(c => ' ' + c.repr(cindent)) ]
        break
      }
      case t.section: {
        const id = (this.children[0] as V).value as uint7
        v = [
          ' ' + sectionName(id),
          ' ' + (this.children[1] as V).value.toString(),
          ...this.children.slice(2).map(c => ' ' + c.repr(cindent))
        ]
        break
      }
      case t.func_type:
      case t.global_type:
      case t.table_type:
      case t.local_entry:
      case t.import_entry:
      case t.export_entry: {
        v = this.children.map(c =>
          ' ' + (c.type == t.varint7 ? langTypeName((c as V).value)
                                     : c.repr(cindent)))
        break
      }
      default:
        v = this.children.map(c => ' ' + c.repr(cindent))
        break
    }
    return s + v.join('') + ')'
  }

  emit(w :ForwardBinWriter) :void {
    this.children.forEach(c => c.emit(w))
  }
}

// Pure-value node (terminal)
class V implements N {
  readonly type     :Symbol
  readonly byteSize :uint32
  readonly value    :any        // logical value
  readonly emit     :CodeEmitter

  constructor(type :Symbol, byteSize :uint32, value :any, emit :CodeEmitter) {
    this.type = type
    this.byteSize = byteSize
    this.emit = emit
    this.value = value
    assert(!isNaN(this.byteSize))
  }

  repr(indent? :string) :string {
    switch (this.type) {
      case t.bytes: {
        let v :string[] = []
        for (let b of this.value) {
          if (v.length == 8) {
            v.push('...')
            break
          }
          v.push(b.toString(16))
        }
        return (indent ? '\n' + indent : '') + '(bytes ' + v.join(' ') + ')'
      }
      case t.uint8:
        return 'uint8:0x' + this.value.toString(16)
      default:
        return symname(this.type) + ':' + this.value
    }
  }
}

// Instruction node
interface Instr extends N {
  readonly opcode :byte
}

class IV implements Instr {
  readonly type     :Symbol
  readonly byteSize :uint32
  readonly opcode   :byte

  constructor(opcode :byte) {
    assert(opcode >= 0)
    this.type = t.instr_v
    this.opcode = opcode
    this.byteSize = 1
  }

  repr(indent? :string) :string {
    return opnames.get(this.opcode)
  }

  emit(w :ForwardBinWriter) :void {
    w.writeUint8(this.opcode)
  }
}

class IB implements Instr {
  readonly type     :Symbol
  readonly byteSize :uint32
  readonly opcode   :byte
  readonly children :N[]

  constructor(opcode :byte, imm_or_children :N[]) {
    assert(imm_or_children.length > 0) // otherwise use IV
    assert(opcode >= 0)
    this.type = t.instr_b
    this.opcode = opcode
    this.byteSize = 1 + sumf(imm_or_children, c => c.byteSize)
    assert(!isNaN(this.byteSize))
    this.children = imm_or_children
  }

  repr(indent? :string) :string {
    const cindent :string = (indent ? indent + '  ' : '  ')
    return (indent ? '\n' + indent : '') +
      '(' + opnames.get(this.opcode) +
      this.children.map(c => ' ' + c.repr(cindent)).join(' ') + ')'
  }

  emit(w :ForwardBinWriter) :void {
    w.writeUint8(this.opcode)
    this.children && this.children.forEach(c => c.emit(w))
  }
}

//——————————————————————————————————————————————————————————————————————————————
// Constructors

function byte(value :uint8) {
  return new V(t.uint8, 1, value, w => w.writeUint8(value))
}

function mem_op_fn(opcode :byte) {
  return function mem_op_imm(flags :uint32, offset: uint32) {
    // flags: a bitfield which currently contains the alignment in the
    //        least significant bits, encoded as log2(alignment)
    return new IB(opcode, [c.varint32(flags), c.varint32(offset)])
  }
}

const c = {
  byte,

  uint32(value :uint32) {
    return new V(t.uint32, 4, value, w => w.writeUint32(value))
  },

  varuint32_: [
    new V(t.varuint32, 1, 0, w => w.writeUint8(0)),
    new V(t.varuint32, 1, 1, w => w.writeUint8(1)),
  ],
  varuint32(value :uint32) :V {
    if (value < c.varuint32_.length) { return c.varuint32_[value] }
    const bytes :uint32[] = []
    let v = value
    do {
      let b = v & 0x7f
      v >>= 7
      if (v != 0) { b |= 0x80 }
      bytes.push(b)
    } while (v != 0)
    return new V(t.varuint32, bytes.length, value, w => w.writeBytes(bytes))
  },

  varuint7(value :uint) {
    // assert(value < 0x80)
    return new V(t.varuint7, 1, value, w => w.writeUint8(value))
  },

  varuint1_0: new V(t.varuint1, 1, 0, w => w.writeUint8(0)),
  varuint1_1: new V(t.varuint1, 1, 1, w => w.writeUint8(1)),
  varuint1(value :uint) {
    // assert(value == 0 || value == 1)
    return value ? c.varuint1_1 : c.varuint1_0
  },

  varint32(value :int32) :V {
    const bytes :uint32[] = []
    const kSignBitMask = 0x40;
    let v = value
    let done = false;
    do {
      let b = v & 0x7f
      v >>= 7
      done = ((v == 0)  && ((b & kSignBitMask) == 0)) ||
             ((v == -1) && ((b & kSignBitMask) != 0)) ;
      if (!done) { b |= 0x80; }
      bytes.push(b)
    } while (!done)
    return new V(t.varint32, bytes.length, value, w => w.writeBytes(bytes))
  },

  bytes(buf: Uint8Array) {
    return new V(t.bytes, buf.byteLength, buf, w => w.writeBytes(buf))
  },

  // Helper for creating length-prefixed string from text
  stringFromText(text :string) {
    const buf = utf8.encode(text)
    return [c.varuint32(buf.length), c.bytes(buf)]
  },

  // Language Types
  i32_t:       new V(t.varint7, 1, -1, w => w.writeUint8(0x7f)),
  i64_t:       new V(t.varint7, 1, -2, w => w.writeUint8(0x7e)),
  f32_t:       new V(t.varint7, 1, -3, w => w.writeUint8(0x7d)),
  f64_t:       new V(t.varint7, 1, -4, w => w.writeUint8(0x7c)),
  anyfunc:     new V(t.varint7, 1, -0x10, w => w.writeUint8(0x70)),
  func:        new V(t.varint7, 1, -0x20, w => w.writeUint8(0x60)),
  empty_block: new V(t.varint7, 1, -0x40, w => w.writeUint8(0x40)),

  // external_kind
  external_kind: {
    function: byte(0),
    table:    byte(1),
    memory:   byte(2),
    global:   byte(3),
  },

  // value_type = i32_t | i64_t | f32_t | f64_t
  // block_type = empty_block | value_type

  module(version :uint32, ...sections :N[]) {
    return new B(t.module, [
      c.uint32(0x6d736100),
      c.uint32(version),
      ...sections])
  },

  section(id :uint7, data :N[]) {
    return new B(t.section, [
      c.varuint32(id),
      c.varuint32(sumf(data, s => s.byteSize)), // payload_len
      ...data])
  },

  customSection(name :string, data :N[]) {}, // TODO

  typeSection(...func_type :B[]) {
    // Function signature declarations
    return c.section(1, [c.varuint32(func_type.length), ...func_type])
  },

  importSection(...import_entry :B[]) {
    // Import declarations
    return c.section(2, [c.varuint32(import_entry.length), ...import_entry])
  },

  functionSection(type_index :uint32[]) { // Function declarations
    return c.section(3,
      [c.varuint32(type_index.length), ...type_index.map(c.varuint32)])
  },

  tableSection(...table_type :B[]) { // Indirect function table and other tables
    return c.section(4, [c.varuint32(table_type.length), ...table_type])
  },

  memorySection(...memory_type :B[]) { // Memory attributes
    return c.section(5, [c.varuint32(memory_type.length), ...memory_type])
  },

  globalSection(...global_variable :B[]) { // Global declarations
    return c.section(6, [c.varuint32(global_variable.length), ...global_variable])
  },

  exportSection(...export_entry :B[]) { // Exports
    return c.section(7, [c.varuint32(export_entry.length), ...export_entry])
  },

  startSection(index :uint32) { // Start function declaration
    // Declares the start function index
    return c.section(8, [c.varuint32(index)])
  },

  elementSection(...elem_segment :B[]) { // Elements
    // allows a module to initialize (at instantiation time) the elements
    // of any imported or internally-defined table with any other definition
    // in the module
    return c.section(9, [c.varuint32(elem_segment.length), ...elem_segment])
  },

  codeSection(...function_body :B[]) { // Function bodies (code)
    return c.section(10, [c.varuint32(function_body.length), ...function_body])
  },

  dataSection(...data_segment :B[]) { // Data segments
    return c.section(11, [c.varuint32(data_segment.length), ...data_segment])
  },

  data_segment(index :uint32, offset_init_expr :B, buf :Uint8Array) {
    // offset_init_expr is expected to be an i32 initializer expression
    // that computes the offset at which to place the data
    // assert(index == 0) // MVP: only 0
    return new B(t.data_segment, [
      c.varuint32(index),
      offset_init_expr,
      c.varuint32(buf.byteLength),
      c.bytes(buf) ])
  },

  elem_segment(index :uint32, offset_init_expr :B, elems :uint32[]) {
    // offset_init_expr is expected to be an i32 initializer expression
    // that computes the offset at which to place the elements
    // assert(index == 0) // MVP: only 0
    return new B(t.elem_segment, [
      c.varuint32(index),
      offset_init_expr,
      c.varuint32(elems.length),
      ...elems.map(c.varuint32) ])
  },

  global_variable(global_type :B, init_expr: B) {
    // assert(global_type.type == t.global_type)
    return new B(t.global_variable, [global_type, init_expr])
  },

  init_expr(instr :Instr) {
    // The encoding of an initializer expression is the normal encoding of
    // the expression followed by the `end` opcode as a delimiter.
    //
    // Note that get_global in an initializer expression can only refer to
    // immutable imported globals and all uses of init_expr can only appear
    // after the Imports section.
    //
    // An initializer expression is a pure WebAssembly expression that is
    // encoded with the same binary encoding as WebAssembly expressions.
    // Not all WebAssembly operators can or should be supported in initializer
    // expressions; initializer expressions represent a minimal pure subset
    // of WebAssembly expressions.
    //
    // In the MVP, to keep things simple while still supporting the basic
    // needs of dynamic linking, initializer expressions are restricted to
    // the following nullary operators:
    //   - the four constant operators (i32.const et al); and
    //   - get_global, where the global index must refer to an immutable import.
    //
    // In the future, operators like i32.add could be added to allow more
    // expressive base + offset load-time calculations.
    return new B(t.init_expr, [instr, c.end])
  },

  function_body(local_entryv :B[], code :N[]) {
    // Function bodies consist of a sequence of local variable declarations
    // followed by bytecode instructions. Each function body must end with
    // the "end" opcode.
    return new B(t.function_body, [
      c.varuint32(sumf(code, s => s.byteSize)),
      c.varuint32(local_entryv.length), // local_count
      ...local_entryv,
      ...code ])
  },

  local_entry(count :uint32, value_type :V) {
    return new B(t.local_entry, [c.varuint32(count), value_type])
  },

  import_entry(module :string, field: string, external_kind :V, type: N) {
    return new B(t.import_entry, [
      ...c.stringFromText(module),
      ...c.stringFromText(field),
      external_kind,
      type])
  },

  import_entry_function(module :string, field: string, type_index: uint32) {
    return c.import_entry(module, field, c.external_kind.function,
      c.varuint32(type_index))
  },

  import_entry_table(module :string, field: string, table_type: B) {
    return c.import_entry(module, field, c.external_kind.table, table_type)
  },

  import_entry_memory(module :string, field: string, memory_type: B) {
    return c.import_entry(module, field, c.external_kind.table, memory_type)
  },

  import_entry_global(module :string, field: string, global_type: B) {
    return c.import_entry(module, field, c.external_kind.table, global_type)
  },

  export_entry(field: string, external_kind :V, index: uint32) {
    // For example, if the "external_kind" is Function, then "index" is a
    // function index.
    // Note that, in the MVP, the only valid index value for a memory or
    // table export is 0.
    return new B(t.export_entry, [
      ...c.stringFromText(field),
      external_kind,
      c.varuint32(index)])
  },

  resizable_limits(initial :uint32, maximum? :uint32) {
    const initialN = c.varuint32(initial)
    return new B(t.resizable_limits,
      maximum !== undefined ? [c.varuint1_1, initialN, c.varuint32(maximum)]
                            : [c.varuint1_0, initialN])
  },

  func_type(paramTypes :N[], returnTypes :N[]) {
    // assert(returnTypes.length < 2) // current version only supports 1 or 0
    return new B(t.func_type, [
      c.func,
      c.varuint32(paramTypes.length),
      ...paramTypes,
      c.varuint1(returnTypes.length),
      ...returnTypes])
  },

  // A table is similar to a linear memory whose elements, instead of
  // being bytes, are opaque values of a particular table element type.
  // This allows the table to contain values—like GC references,
  // raw OS handles, or native pointers—that are accessed by WebAssembly
  // code indirectly through an integer index. This feature bridges the
  // gap between low-level, untrusted linear memory and high-level opaque
  // handles/references at the cost of a bounds-checked table indirection.
  table_type(elem_type :V, resizable_limits :B) {
    return new B(t.table_type, [elem_type, resizable_limits])
  },

  memory_type(resizable_limits :B) {
    // assert(resizable_limits.type == t.resizable_limits)
    return new B(t.memory_type, [resizable_limits])
  },

  global_type(content_type :V, mutability :uint1) {
    // assert(content_type is one of [i32_t, i64_t, f32_t, f64_t])
    return new B(t.global_type, [content_type, c.varuint1(mutability)])
  },
  global_type_immutable: (content_type :V) => c.global_type(content_type, 0),
  global_type_mutable: (content_type :V) => c.global_type(content_type, 1),

  // Instructions

  unreachable: new IV(op.unreachable),
  nop:         new IV(op.nop),
  block(block_type :V) { return new IB(op.block, [block_type]) },
    // begin a sequence of expressions, yielding 0 or 1 values
  loop(block_type :V) { return new IB(op.loop, [block_type]) },
    // begin a block which can also form control flow loops
  if(block_type :V) { return new IB(op.if, [block_type]) },
    // begin if expression
  else: new IV(op.else), // begin else expression of if
  end:  new IV(op.end),  // end a block, loop, or if
  br(relative_depth :uint32) {
    return new IB(op.br, [c.varuint32(relative_depth)]) },
    // break that targets an outer nested block
  br_if(relative_depth :uint32) {
    return new IB(op.br_if, [c.varuint32(relative_depth)]) },
    // conditional break that targets an outer nested block
  
  br_table(target_table: uint32[], default_target: uint32) {
    // target_table:   target entries that indicate an outer block or loop
    //                 to which to break
    // default_target: an outer block or loop to which to break in the
    //                 default case.
    //
    // The br_table operator implements an indirect branch.
    // It accepts an optional value argument (like other branches) and
    // an additional i32 expression as input, and branches to the block
    // or loop at the given offset within the target_table.
    // If the input value is out of range, br_table branches to the
    // default target.
    return new IB(op.br_table, [
      c.varuint32(target_table.length),
      ...target_table.map(c.varuint32),
      c.varuint32(default_target) ])
  },

  return_void: new IV(op.return),
  return(...what :N[]) {
    // assert(what.length == 1) // limitation of MVP version
    return new IB(op.return, what) },

  // Call operators
  // TODO

  // Parametric operators
  drop:   new IV(op.drop), // ignore value
  select: new IV(op.select), // select one of two values based on condition

  // Variable access
  get_local(local_index :uint32) :Instr {
    return new IB(op.get_local, [c.varuint32(local_index)]) },
  // TODO

  // Type-specific operators
  i32: {
    const(value :int32) { return new IB(op.i32.const, [c.varint32(value)]) },
    
    // Memory; all functions have signature: (flags :uint32, offset: uint32)
    load: mem_op_fn(op.i32.load),

    // Numeric
    mul(...operands :Instr[]) { return new IB(op.i32.mul, operands) },
  },
}

//——————————————————————————————————————————————————————————————————————————————

const mod = c.module(0xd,
  c.typeSection(
    c.func_type([c.i32_t, c.i32_t], [c.i32_t]), // (Int32, Int32) Int32
    c.func_type([c.i32_t],[c.i32_t]),     // (Int32) Int32
    c.func_type([c.i64_t, c.i32_t],[c.i64_t])  // (Int64, Int32) Int64
  ),
  c.importSection(
    c.import_entry_function("utf8", "encode", 1),
    c.import_entry_table("utf8", "utils",
      c.table_type(c.anyfunc, c.resizable_limits(0, 3))),
    c.import_entry_memory("utf8", "whitespace",
      c.memory_type(c.resizable_limits(0, 4))),
    c.import_entry_global("utf8", "version",
      c.global_type_immutable(c.f32_t)),
  ),
  c.functionSection([0,1]),
  c.tableSection(
    c.table_type(c.anyfunc, c.resizable_limits(0, 3))
  ),
  c.memorySection(
    c.memory_type(c.resizable_limits(0, 4))
  ),
  c.globalSection(
    c.global_variable(
      c.global_type_immutable(c.i32_t), c.init_expr(c.i32.const(0)))
  ),
  c.exportSection(
    c.export_entry("foo", c.external_kind.function, 0)
  ),
  c.startSection(0),
  c.elementSection(
    c.elem_segment(0, c.init_expr(c.i32.const(0)), [0,1])
  ),
  c.codeSection(
    // Each function_body's signature corresponds with the Nth entry
    // in the function section
    c.function_body([c.local_entry(0, c.i32_t)], [ // multiply(Int32, Int32) Int32
      c.return(
        c.i32.mul(c.get_local(0), c.get_local(1))
      ),
      c.end,
    ]),
    c.function_body([], [ // identity(Int32) Int32
      c.return(
        c.get_local(0)
      ),
      c.end
    ]),
  ),
  c.dataSection(
    c.data_segment(0, c.init_expr(c.i32.const(0)), new Uint8Array([1,2,3]))
  )
)

console.log(mod.repr(''))

//——————————————————————————————————————————————————————————————————————————————

class AppendBuffer implements ForwardBinWriter {
  buffer   :ArrayBuffer
  byteView :Uint8Array
  view     :DataView
  length   :number

  constructor(size :uint) {
    this.buffer = new ArrayBuffer(size as number)
    this.byteView = new Uint8Array(this.buffer)
    this.view = new DataView(this.buffer)
    this.length = 0
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

  writeBytes(bytes :ArrayLike<byte>) {
    this.byteView.set(bytes, this.length)
    this.length += bytes.length
  }

  writeArrayBuffer(b :ArrayBufferView) {
    this.byteView.set(new Uint8Array(b.buffer), this.length)
    this.length += b.byteLength
  }

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

let buf = new AppendBuffer(mod.byteSize)
mod.emit(buf)
buf.repr(s => console.log(s))
