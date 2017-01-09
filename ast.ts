import {utf8} from './utf8'

declare function require(ref:string):any;
require('source-map-support').install()

type AssertFunc = (cond:any)=>void;
const _assert :AssertFunc = require('assert')
function assert(cond: any) :boolean {
  _assert(!!cond)
  return true // e.g. value = assert(value < 10) && value
}

//——————————————————————————————————————————————————————————————————————————————

export type uint1   = number
export type uint7   = number
export type uint8   = number
export type uint16  = number
export type uint32  = number

export type int7    = number
export type int32   = number
export type int64   = number

export type float32 = number
export type float64 = number

//——————————————————————————————————————————————————————————————————————————————

function sumf<A>(a :Array<A>, f :(a:A)=>number, initial? :number) :number {
  return a.reduce((s, v) => s + f(v), initial || 0)
}

function symname(y :Symbol) {
  const s = y.toString()
  return s.length > 8 ? s.substr(7,s.length-8) : 'Symbol(?)';
}

function maprange<R>(start:number, stop:number, fn:(v:number)=>R|undefined) :Array<R> {
  let a :R[] = []
  while (start < stop) {
    let v :R = fn(start)
    if (v !== undefined) {
      a.push(v)
    }
    start += 1
  }
  return a
}

//——————————————————————————————————————————————————————————————————————————————

// Instruction opcodes.
// Semantics:
// - Control instructions pop their argument value(s) off the stack, may change
//   the program counter, and push result value(s) onto the stack.
// - Simple instructions pop their argument value(s) from the stack, apply an
//   operator to the values, and then push the result value(s) onto the stack,
//   followed by an implicit advancement of the program counter.
type OpCode = uint8
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

// Map opcodes to names
const opnames = (() => {
  const m = new Map<OpCode,string>()
  const f = (t :{[k:string]:any}, kprefix?: string) => {
    for (const k in t) {
      let v = t[k];
      (typeof v == 'number') ?
        m.set(v as OpCode, kprefix ? kprefix + k : k) :
        f(v, (kprefix ? kprefix + k : k) + '.')
    }
  }
  f(op)
  return m
})()

//——————————————————————————————————————————————————————————————————————————————

export interface Emitter {
  // Emits code
  //
  // Each modifying operation returns a potentially new Emitter which is the result of
  // the receiver + modifications, thus modifying operations should be called like this:
  //   e = e.writeU32(1)
  //   e = e.writeU32(2)
  // but NOT like this:
  //   e.writeU32(1)
  //   e.writeU32(2)
  //   // e represents same state as before `e.writeU32(1)`
  //
  // Think of Emitter as being persistent immutable state
  //
  writeU8(v :uint8) :Emitter
  writeU16(v :uint16) :Emitter
  writeU32(v :uint32) :Emitter
  writeF32(v :float32) :Emitter
  writeF64(v :float64) :Emitter
  writeBytes(v :ArrayLike<uint8>) :Emitter
}

export interface Emittable {
  emit(ctx :Emitter) :Emitter
}

//——————————————————————————————————————————————————————————————————————————————
// Node types

export type TypeTag = symbol
export const t = {
  // Atoms
  uint8:            Symbol('u8'),
  uint16:           Symbol('u16'),
  uint32:           Symbol('u32'),
  varuint1:         Symbol('vu1'),
  varuint7:         Symbol('vu7'),
  varuint32:        Symbol('vu32'),
  varint7:          Symbol('vs7'),
  varint32:         Symbol('vs32'),
  varint64:         Symbol('vs64'),
  float32:          Symbol('f32'), // non-standard
  float64:          Symbol('f64'), // non-standard
  instr:            Symbol('instr'), // non-standard
  instr_pre:        Symbol('instr_pre'), // non-standard
  instr_imm:        Symbol('instr_imm'), // non-standard
  instr_pre_imm:    Symbol('instr_pre_imm'), // non-standard
  data:             Symbol('data'), // non-standard

  // Cells
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
  prefix_str:       Symbol('prefix_str'), // non-standard
}

export interface N extends Emittable {
  readonly t :TypeTag  // type
  readonly z :uint32   // size in bytes (includes size of any children)
  readonly v :any      // value
  emit(e :Emitter) :Emitter
  repr(indent? :string) :string
}

export interface Atom<T> extends N {
  readonly v :T
}

export interface Cell<T extends N> extends N {
  readonly v :T[]
}

//——————————————————————————————————————————————————————————————————————————————
// Formal types
//   We use a trick here to get the most out of TypeScripts type checker,
//   namely we specify interfaces that have "type tag" properties.
//   However, concrete types doesn't actually have these properties, so any
//   attempts to access these properties will always yield `undefined`.

interface Module extends Cell<Section> { readonly _Module: undefined
  readonly version :uint32
}

type Section = CustomSection
             | TypeSection     // Function signature declarations
             | ImportSection   // Import declarations
             | FunctionSection // Function declarations
             | TableSection    // Indirect function table and other tables
             | MemorySection   // Memory attributes
             | GlobalSection   // Global declarations
             | ExportSection   // Exports
             | StartSection    // Start function declaration
             | ElementSection  // Elements section
             | CodeSection     // Function bodies (code)
             | DataSection     // Data segments

export interface CustomSection   extends Cell<N> { readonly _CustomSection: undefined }
export interface TypeSection     extends Cell<FuncType> { readonly _TypeSection: undefined }
export interface ImportSection   extends Cell<ImportEntry> { readonly _ImportSection: undefined }
export interface FunctionSection extends Cell<VarUint32> { readonly _FunctionSection: undefined }
export interface TableSection    extends Cell<TableType> { readonly _TableSection: undefined }
export interface MemorySection   extends Cell<MemoryType> { readonly _MemorySection: undefined }
export interface GlobalSection   extends Cell<GlobalVariable> { readonly _GlobalSection: undefined }
export interface ExportSection   extends Cell<ExportEntry> { readonly _ExportSection: undefined }
export interface StartSection    extends Cell<Void> { readonly _StartSection: undefined }
export interface ElementSection  extends Cell<ElemSegment> { readonly _ElementSection: undefined }
export interface CodeSection     extends Cell<FunctionBody> { readonly _CodeSection: undefined }
export interface DataSection     extends Cell<DataSegment> { readonly _DataSection: undefined }

export interface ImportEntry extends Cell<N> { readonly _ImportEntry: undefined }
export interface ExportEntry extends Cell<N> { readonly _ExportEntry: undefined }

export interface FuncType extends Cell<N> { readonly _FuncType: undefined }
export interface TableType extends Cell<N> { readonly _TableType: undefined }
export type MemoryType = ResizableLimits
export interface GlobalType extends Cell<N> { readonly _GlobalType: undefined }

export interface ResizableLimits extends Cell<N> { readonly _ResizableLimits: undefined }
export interface GlobalVariable extends Cell<N> { readonly _GlobalVariable: undefined }
export interface ElemSegment extends Cell<N> { readonly _ElemSegment: undefined }
export interface DataSegment extends Cell<N> { readonly _DataSegment: undefined }

export interface InitExpr extends Cell<N> { readonly _InitExpr: undefined }
export interface FunctionBody extends Cell<N> { readonly _FunctionBody: undefined }
export interface LocalEntry extends Cell<N> { readonly _LocalEntry: undefined }

export interface PrefixStr extends Atom<ArrayLike<uint8>> { readonly _PrefixStr: undefined
  readonly len :VarUint32
}

export interface Data extends Atom<ArrayLike<uint8>> { readonly _Data: undefined }

export interface Uint8     extends Atom<uint8> {}
export interface Uint16    extends Atom<uint16> {}
export interface Uint32    extends Atom<uint32> {}
export interface VarUint32 extends Atom<uint32> {}
export interface VarUint7  extends Atom<uint7> {}
export interface VarUint1  extends Atom<uint1> {}
export interface VarInt7   extends Atom<int7> {}
export interface VarInt32  extends Atom<int32> {}
export interface VarInt64  extends Atom<int64> {}
export interface Float32   extends Atom<float32> {}
export interface Float64   extends Atom<float64> {}

export interface I32 extends VarInt32 { readonly _I32: undefined }
export interface I64 extends VarInt64 { readonly _I64: undefined }
export interface F32 extends Float32  { readonly _F32: undefined }
export interface F64 extends Float64  { readonly _F64: undefined }

export type INative = I32 | I64 // wasm32 | wasm64

export interface ValueType extends Atom<int32|int64|float32|float64> {}
type AnyFunc    = VarInt7
type Func       = VarInt7
type EmptyBlock = VarInt7
type ElemType   = AnyFunc
type ExternalKind = Uint8
type BlockType = ValueType | EmptyBlock

export interface Void extends VarInt7 { readonly _Void :undefined }

// Memory immediate.
// In wasm32, address operands and offset attributes have type i32
export type MemImm = [
  // flags - a bitfield which currently contains the alignment in the least
  // significant bits, encoded as log2(alignment)
  VarUint32,

  // offset - the value of the offset
  INative
]

//——————————————————————————————————————————————————————————————————————————————
//——————————————————————————————————————————————————————————————————————————————
// node structs

const ind    = (indent? :string) => (indent ? '\n' + indent : '')
const style  = (str :string, style :string) => '\x1B[' + style + 'm' + str + '\x1B[0m'
const reprt  = (t :Symbol) => style(symname(t), '92')
const reprop = (op :uint8) => style(opnames.get(op), '96')
const writev = (e :Emitter, objs :Emittable[]) :Emitter => objs.reduce((e, n) => n.emit(e), e)

const sumz = function(n :N[]) {
  let sum = 0
  for (let i = 0, L = n.length; i != L; ++i) {
    sum += n[i].z
  }
  return sum
}

const ReadVarInt7 = (byte :uint8) :int7 =>
  byte < 64 ? byte : -(128 - byte)

class bytes_atom implements Atom<ArrayLike<uint8>> {
  readonly t     :TypeTag
  readonly z     :uint32
  readonly v     :ArrayLike<uint8>

  constructor(t :TypeTag, v :ArrayLike<uint8>) {
    this.t = t
    this.z = v.length
    this.v = v
  }

  emit(e :Emitter) { return e.writeBytes(this.v) }

  repr(indent? :string) {
    let v :string[] = []
    for (let i = 0, L = this.v.length; i != L; ++i) {
      if (v.length == 8) { v.push('...') ;break }
      v.push(this.v[i].toString(16))
    }
    return ind(indent) + '(' + reprt(this.t) + ' ' + v.join(' ') + ')'
  }
}

class val_atom<T> implements Atom<T> {
  readonly t :TypeTag
  readonly z :uint32
  readonly v :T

  constructor(t :TypeTag, z :uint32, v :T) { this.t = t; this.z = z; this.v = v }
  emit(e :Emitter) { return e } // override in subclasses
  repr(indent? :string) { return this.v + ':' + reprt(this.t) }
}

class bytesval_atom<T extends number> extends val_atom<T> {
  readonly bytes :ArrayLike<uint8>
  constructor(t :TypeTag, v :T, bytes :ArrayLike<uint8>) {
    super(t, bytes.length, v)
    this.bytes = bytes
  }
  emit(e :Emitter) { return e.writeBytes(this.bytes) }
}

class u32_atom extends val_atom<uint32> {
  constructor(v :uint32) { super(t.uint32, 4, v) }
  emit(e :Emitter) { return e.writeU32(this.v) }
}

class f32_atom extends val_atom<float32> {
  constructor(v :number) { super(t.float32, 4, v) }
  emit(e :Emitter) { return e.writeF32(this.v) }
}

class f64_atom extends val_atom<float64> {
  constructor(v :number) { super(t.float64, 8, v) }
  emit(e :Emitter) { return e.writeF64(this.v) }
}

class u8_atom<T extends number> extends val_atom<T> {
  constructor(t :TypeTag, v :T) { super(t, 1, v) }
  emit(e :Emitter) { return e.writeU8(this.v) }
  repr(indent? :string) {
    let v = (this.t == t.varint7) ? ReadVarInt7(this.v)
          : this.v
    return v + ':' + reprt(this.t)
  }
}

class type_atom extends u8_atom<int7> {
  readonly b :uint8
  constructor(v :int7, b :uint8) { super(t.varint7, v); this.b = b }
  emit(e :Emitter) { return e.writeU8(this.b) }
  repr(indent? :string) {
    switch (this.v) {
      case -1:    return 'i32'
      case -2:    return 'i64'
      case -3:    return 'f32'
      case -4:    return 'f64'
      case -0x10: return 'anyfunc'
      case -0x20: return 'func'
      case -0x40: return 'void' // aka empty_block
      default:    return '?'
    }
  }
}

class prefix_str_atom implements Atom<ArrayLike<uint8>> {
  readonly t   :TypeTag
  readonly z   :uint32
  readonly v   :ArrayLike<uint8>
  readonly len :VarUint32

  constructor(len: VarUint32, v :ArrayLike<uint8>) {
    assert(len.v == v.length)
    this.t = t.prefix_str
    this.z = len.z + v.length
    this.v = v
    this.len = len
  }
  emit(e :Emitter) { return this.len.emit(e).writeBytes(this.v) }
  repr(indent? :string) { return '"' + utf8.decode(this.v) + '"' }
}


const reprv = function(indent :string, nodes :N[]) :string {
  let s = '', i = (indent || '') + '  '
  for (let c of nodes) { s += ' ' + c.repr(i) }
  return s
}

class cell<T extends N> implements Cell<T> {
  readonly t :TypeTag
  readonly z :uint32
  readonly v :T[]

  constructor(t :TypeTag, v :T[]) {
    this.t = t
    this.z = sumz(v)
    this.v = v
  }

  emit(e :Emitter) { return writev(e, this.v) }

  repr(indent? :string) {
    let s = `${ind(indent)}(${reprt(this.t)}`
    switch (this.t) {
      case t.global_type:
        return s + ' ' + this.v[0].repr() + (this.v[1].v ? ' mutable)' : ')')
      case t.resizable_limits:
        return s + ' ' + this.v[1].v + '..' + (this.v[0].v ? this.v[2].v : '') + ')'
      default:
        return s + reprv(indent, this.v) + ')'
    }
  }
}

class _module extends cell<Section> {
  repr(indent :string) {
    return `${ind(indent)}(${reprt(this.t)} ${this.v[1].v}${reprv(indent, this.v.slice(2))})`
  }
}

//—————————————————————————————————————————————

interface Op<Result> extends N {
  readonly _Op :Result
  readonly v   :uint8
}

//—————————————————————————————————————————————

class instr_atom extends u8_atom<uint8> {
  constructor(v :uint8) { super(t.instr, v) }
  repr(indent? :string) { return reprop(this.v) }
}

class instr_cell implements N {
  readonly t :TypeTag
  readonly z :uint32
  readonly v :uint8
  readonly n :N[]

  constructor(t :TypeTag, op :uint8, n :N[], z :uint32) {
    this.t = t
    this.z = z
    this.v = op
    this.n = n
  }
  emit(e :Emitter) { return writev(e, this.n).writeU8(this.v) }
  repr(indent? :string) {
    return `${ind(indent)}(${reprop(this.v)}${reprv(indent, this.n)})`
  }
}

class instr_pre1 implements N {
  readonly t :TypeTag = t.instr_pre
  readonly z :uint32
  readonly v :uint8
  readonly n :N

  constructor(op :uint8, n :N) {
    this.z = 1 + n.z
    this.v = op
    this.n = n
  }
  emit(e :Emitter) { return this.n.emit(e).writeU8(this.v) }
  repr(indent? :string) {
    return `${ind(indent)}(${reprop(this.v)}${reprv(indent, [this.n])})`
  }
}

class instr_imm1 extends instr_pre1 {
  readonly t :TypeTag = t.instr_imm
  emit(e :Emitter) { return this.n.emit(e.writeU8(this.v)) }
}

class instr_pre extends instr_cell {
  constructor(op :uint8, pre :N[]) {
    super(t.instr_pre, op, pre, 1 + sumz(pre))
  }
}

class instr_imm extends instr_cell {
  constructor(op :uint8, imm :N[]) {
    super(t.instr_imm, op, imm, 1 + sumz(imm))
  }
  emit(e :Emitter) { return writev(e.writeU8(this.v), this.n) }
  repr(indent? :string) {
    switch (this.v) {
      case op.loop:
        return `${ind(indent)}(${reprop(this.v)}${reprv(indent, this.n)})`
      default:
        return super.repr(indent)
    }
  }
}

class instr_pre_imm extends instr_cell {
  readonly imm :N[]
  constructor(op :uint8, pre :N[], imm :N[]) {
    super(t.instr_pre_imm, op, pre, 1 + sumz(pre) + sumz(imm))
    this.imm = imm
  }
  emit(e :Emitter) { return writev(writev(e, this.n).writeU8(this.v), this.imm) }
  repr(indent? :string) {
    let s = ind(indent) + '(' + reprop(this.v)

    if (this.v == op.if) {
      let cind = (indent || '') + '  '

      s += ' ' + this.imm[0].repr() + // type
           this.n[0].repr(cind) + // cond
           ind(cind) + '(then';

      let i = 1, ci = (cind || '') + '  ', E = this.imm.length-1 // skip End
      for (; i < E; ++i) {
        let n = this.imm[i]
        if (n.v == op.else) {
          s += ')' // end of "then"
          break
        }
        s += ' ' + n.repr(ci)
      }

      if (i < E) {
        s += ind(cind) + '(else';
        ++i
        for (; i < E; ++i) {
          let n = this.imm[i]
          s += ' ' + n.repr(ci)
        }
      }
      
      s += ') end' // end of "then" or "else"
      return s
    }

    return s + `${reprv(indent, this.imm)}${reprv(indent, this.n)})`
  }
}

//——————————————————————————————————————————————————————————————————————————————
// constructors

const uint8Cache :Uint8[] = maprange(0,16, v =>
  new u8_atom<uint8>(t.uint8, v as uint8))
const varUint7Cache :VarUint7[] = maprange(0,16, v =>
  new u8_atom<uint7>(t.varuint7, v as uint8))
const varUint32Cache :VarUint7[] = maprange(0,16, v =>
  new u8_atom<uint32>(t.varuint32, v as uint8))
const varuint1_0 = new u8_atom<uint1>(t.varuint1, 0)
const varuint1_1 = new u8_atom<uint1>(t.varuint1, 1)

const Uint8 = (v :uint8) => uint8Cache[v] || new u8_atom<uint8>(t.uint8, v)

const Uint32 = (v :uint32) => new u32_atom(v)
const Float32 = (v :number) => new f32_atom(v)
const Float64 = (v :number) => new f64_atom(v)

// LEB128-encoded variable-length integers: (N = bits)
//   unsigned range: [0, 2^N-1]
//   signed range:   [-2^(N-1), +2^(N-1)-1]

const VarUint1 = (v :any) => v ? varuint1_1 : varuint1_0

const VarUint7 = (v :uint7) =>
  varUint7Cache[v] ||
  new u8_atom<uint7>(t.varuint7, assert(v >= 0 && v <= 128) && v)


const VarUint32 = function(value :uint32) {
  const c = varUint32Cache[value]
  if (c) { return c }
  assert(value >= 0 && value <= 0xffffffff)

  let v = value
  const bytes :uint8[] = []
  while (v >= 0x80) {
    bytes.push((v & 0x7f) | 0x80)
    v >>>= 7
  }
  bytes.push(v)

  return new bytesval_atom<uint32>(t.varuint32, value, bytes) as VarUint32
}

const encVarIntN = function(v :int64) :uint8[] {
  // FIXME: broken for values larger than uint32
  const bytes :uint8[] = []
  while (true) {
    let b = v & 0x7f
    if (-64 <= v && v < 64) {
      bytes.push(b)
      break
    }
    v >>= 7 // Note: sign-propagating right shift
    bytes.push(b | 0x80)
  }
  return bytes
}

const VarInt64 = function(value :int64) :VarInt64 {
  // Here be dragons! Not all negative 64bit numbers can be represented with
  // JavaScript numbers. The ECMAScript double type has 53 bits of integer
  // precision. We thus assert this range
  assert(value >= Number.MIN_SAFE_INTEGER && value <= Number.MAX_SAFE_INTEGER)
  return new bytesval_atom<int64>(t.varint64, value, encVarIntN(value)) as VarInt64
}

const VarInt32 = function(value :int32) :VarInt32 {
  assert(value >= -0x80000000 && value <= 0x7fffffff)
  return new bytesval_atom<int32>(t.varint32, value, encVarIntN(value)) as VarInt32
}

const VarInt7 = (value :int7) =>
  assert(value >= -64 && value <= 63) &&
  new u8_atom<int7>(t.varint7, value < 0 ? (128 + value) : value) as VarInt7


// Language types
const I32        = new type_atom(-0x01, 0x7f) as any as I32
const I64        = new type_atom(-0x02, 0x7e) as any as I64
const F32        = new type_atom(-0x03, 0x7d) as any as F32
const F64        = new type_atom(-0x04, 0x7c) as any as F64
const AnyFunc    = new type_atom(-0x10, 0x70) as any as AnyFunc
const Func       = new type_atom(-0x20, 0x60) as any as Func
const EmptyBlock = new type_atom(-0x40, 0x40) as any as EmptyBlock
const Void       = EmptyBlock as any as Void

// ExternalKind
const ExternalKind = (v :Uint8) => ({
  __proto__: v,
  repr(indent? :string) {
    switch (this.v) {
      case 0: return 'external_kind.function'
      case 1: return 'external_kind.table'
      case 2: return 'external_kind.memory'
      case 3: return 'external_kind.global'
      default: return super.repr(indent)
    }
  }
}) as any as ExternalKind;

const ExternalKindFunction = ExternalKind(Uint8(0)) // Function import or definition
const ExternalKindTable    = ExternalKind(Uint8(1)) // Table import or definition
const ExternalKindMemory   = ExternalKind(Uint8(2)) // Memory import or definition
const ExternalKindGlobal   = ExternalKind(Uint8(3)) // Global import or definition

const Data = (buf: ArrayLike<uint8>) =>
  new bytes_atom(t.data, buf) as any as Data

const PrefixStr = (data: ArrayLike<uint8>) =>
  new prefix_str_atom(VarUint32(data.length), data) as any as PrefixStr

const PrefixStrASCII = function(text: string) {
  const bytes :uint8[] = []
  for (let i = 0, L = text.length; i != L; ++i) {
    bytes[i] = 0xff & text.charCodeAt(i);
  }
  return PrefixStr(bytes)
}

const PrefixStrUTF8 = (text: string) => PrefixStr(utf8.encode(text))

const FuncType = function(paramTypes :ValueType[], returnType? :ValueType|null) {
  const paramLen = VarUint32(paramTypes.length)
  return new cell(t.func_type,
    returnType ? [Func, paramLen, ...paramTypes, varuint1_1, returnType]
               : [Func, paramLen, ...paramTypes, varuint1_0]) as any as FuncType
}

const moduleMagic = Uint32(0x6d736100)

const Module = function(version: uint32, sections :Section[]) {
  return new _module(t.module, [moduleMagic, Uint32(version), ...sections] as Section[]) as any as Module
}

// interface sectionInfo { id :VarUint7, name :string }
const sections = [
  { id: VarUint7(0),  name: 'custom' },
  { id: VarUint7(1),  name: 'type' },     // Function signature declarations
  { id: VarUint7(2),  name: 'import' },   // Import declarations
  { id: VarUint7(3),  name: 'function' }, // Function declarations
  { id: VarUint7(4),  name: 'table' },    // Indirect function table and other tables
  { id: VarUint7(5),  name: 'memory' },   // Memory attributes
  { id: VarUint7(6),  name: 'global' },   // Global declarations
  { id: VarUint7(7),  name: 'export' },   // Exports
  { id: VarUint7(8),  name: 'start' },    // Start function declaration
  { id: VarUint7(9),  name: 'element' },  // Elements section
  { id: VarUint7(10), name: 'code' },     // Function bodies (code)
  { id: VarUint7(11), name: 'data' },     // Data segments
]

class section extends cell<N> {
  constructor(id :uint7, imm :N, payload: N[]) {
    super(t.section, [
      sections[id].id,
      VarUint32(imm.z + sumz(payload)),
      imm,
      ...payload ])
  }
  repr(indent? :string) {
    const typeName = sections[(this.v[0] as VarUint7).v].name
    return `${ind(indent)}(${reprt(this.t)} ${typeName}${reprv(indent, this.v.slice(1))})`
  }
}

const CustomSection = (name :PrefixStr, payload :N[]) =>
  new section(0, name, payload) as any as CustomSection

const TypeSection = (types: FuncType[]) =>
  new section(1, VarUint32(types.length), types) as any as TypeSection

const ImportSection = (entries: ImportEntry[]) =>
  new section(2, VarUint32(entries.length), entries) as any as ImportSection

const FunctionSection = (types: VarUint32[]) =>
  new section(3, VarUint32(types.length), types) as any as FunctionSection

const TableSection = (types: TableType[]) =>
  new section(4, VarUint32(types.length), types) as any as TableSection

const MemorySection = (types: MemoryType[]) =>
  new section(5, VarUint32(types.length), types) as any as MemorySection

const GlobalSection = (globals: GlobalVariable[]) =>
  new section(6, VarUint32(globals.length), globals) as any as GlobalSection

const ExportSection = (exports: ExportEntry[]) =>
  new section(7, VarUint32(exports.length), exports) as any as ExportSection

const StartSection = (funcIndex: VarUint32) =>
  new section(8, funcIndex, []) as any as StartSection

const ElementSection = (entries: ElemSegment[]) =>
  new section(9, VarUint32(entries.length), entries) as any as ElementSection

const CodeSection = (bodies: FunctionBody[]) =>
  new section(10, VarUint32(bodies.length), bodies) as any as CodeSection

const DataSection = (entries: DataSegment[]) =>
  new section(11, VarUint32(entries.length), entries) as any as DataSection


const FunctionImportEntry = (module :PrefixStr, field :PrefixStr, typeIndex: VarUint32) =>
  new cell<N>(t.import_entry, [
    module, field, ExternalKindFunction, typeIndex
  ]) as any as ImportEntry

const TableImportEntry = (module :PrefixStr, field :PrefixStr, type: TableType) =>
  new cell<N>(t.import_entry, [module, field, ExternalKindTable, type]) as any as ImportEntry

const MemoryImportEntry = (module :PrefixStr, field :PrefixStr, limits: ResizableLimits) =>
  new cell<N>(t.import_entry, [module, field, ExternalKindMemory, limits]) as any as ImportEntry

const GlobalImportEntry = (module :PrefixStr, field :PrefixStr, type: GlobalType) =>
  new cell<N>(t.import_entry, [module, field, ExternalKindGlobal, type]) as any as ImportEntry


const ExportEntry = (field :PrefixStr, kind :ExternalKind, index :VarUint32) =>
  new cell<N>(t.export_entry, [field, kind, index]) as any as ExportEntry


const ElemSegment = (index :VarUint32, offset :InitExpr, funcIndex :VarUint32[]) =>
  new cell<N>(t.elem_segment, [
    index, offset, VarUint32(funcIndex.length), ...funcIndex
  ]) as any as ElemSegment

const DataSegment = (index :VarUint32, offset :InitExpr, data :Data) =>
  new cell<N>(t.data_segment, [index, offset, VarUint32(data.z), data]) as any as DataSegment


const TableType = (type :ElemType, limits :ResizableLimits) =>
  assert(type.v == AnyFunc.v) && // WASM MVP limitation
  new cell<N>(t.table_type, [type, limits]) as any as TableType

const GlobalType = (contentType :ValueType, mutable? :boolean) =>
  new cell<N>(t.global_type, [
    contentType, mutable ? varuint1_1 : varuint1_0
  ]) as any as GlobalType


// expressed in number of memory pages (not bytes; pagesize=64B)
const ResizableLimits = (initial :VarUint32, maximum? :VarUint32) =>
  new cell<N>(t.resizable_limits, maximum ?
    [varuint1_1, initial, maximum] : [varuint1_0, initial]
  ) as any as ResizableLimits

export const MemoryType = ResizableLimits

const GlobalVariable = (type :GlobalType, init :InitExpr) =>
  new cell<N>(t.global_variable, [type, init]) as any as GlobalVariable

const End = new instr_atom(op.end) as any as Op<Void>

const InitExpr = (expr :N[]) =>
  new cell<N>(t.init_expr, [...expr, End]) as any as InitExpr

const FunctionBody = function(locals :LocalEntry[], code :N[]) {
  // TODO: spec is not clear on wethere the value of body_size
  // includes localCount and locals. Assuming it does for now.
  const localCount = VarUint32(locals.length)
  return new cell<N>(t.function_body, [
    VarUint32(localCount.z + sumz(locals) + sumz(code) + 1), // body_size
    localCount, ...locals, ...code, End
  ]) as any as FunctionBody
}

const LocalEntry = (count :VarUint32, type :ValueType) =>
  new cell<N>(t.local_entry, [count, type]) as any as LocalEntry


// ————————— Control flow —————————

const Unreachable = new instr_atom(op.unreachable) as any as Op<Void>
const Nop         = new instr_atom(op.nop) as any as Op<Void>

// begin a block which can also form CF loops
const Block = <R extends ValueType>(res :R, body :N[]) =>
  // TODO: find a way to check that the type of body[body.length-1] == R
  new instr_imm(op.block, [res as N, ...body, End]) as any as Op<R>

const VoidBlock = (body :N[]) =>
  new instr_imm(op.block, [EmptyBlock, ...body, End]) as any as Op<Void>


// begin a block which can also form CF loops
// function Loop<R extends ValueType>(res :R, stmt :N[], expr: Op<R>) :Op<R>
// function Loop<R extends ValueType>(res :R, expr :Op<R>) :Op<R>

const Loop = <R extends ValueType>(res :R, body :N[]) =>
  // TODO: find a way to check that the type of body[body.length-1] == R
  new instr_imm(op.loop, [res as N, ...body, End]) as any as Op<R>

const VoidLoop = (body :N[]) =>
  new instr_imm(op.loop, [EmptyBlock, ...body, End]) as any as Op<Void>


const elseOp = new instr_atom(op.else) as any as Op<Void>

const If = <R extends BlockType, A, B>(res :R, cond :Op<I32>, then_ :Op<N>[], else_? :Op<N>[]) =>
  // TODO: find a way to check that the type of
  // 1. last instr of then_, and
  // 2. last instr of else_ (if provided), and
  // 3. res R is of the same type
  new instr_pre_imm(op.if,
    [cond],
    else_ ? [res, ...then_, elseOp, ...else_, End] :
            [res, ...then_, End]
  ) as any as Op<R>

// Note: `End` is defined above

// Branch to a given label (relative depth) in an enclosing construct.
// Note:
// - "branching" to a block correspond to a "break" statement
// - "branching" to a loop correspond to a "continue" statement
const Br = (relDepth :uint32) =>
  new instr_imm1(0x0c, VarUint32(relDepth)) as any as Op<Void>

// Conditionall branch to a given label in an enclosing construct.
// When condition is false, this is equivalent to a "Nop" operation.
// When condition is true, this is equivalent to a "Br" operation.
const BrIf = (relDepth :uint32, cond :Op<I32>) =>
  new instr_pre_imm(0x0d, [cond], [VarUint32(relDepth)]) as any as Op<Void>

// Jump table which jumps to a label in an enclosing construct.
// A br_table consists of a zero-based array of labels, a default label,
// and an index operand. A br_table jumps to the label indexed in the
// array or the default label if the index is out of bounds.
const BrTable = (targetLabels :VarUint32[], defaultLabel :VarUint32, index :Op<N>) =>
  new instr_pre_imm(0x0e,
    [index],
    [VarUint32(targetLabels.length), ...targetLabels, defaultLabel]
  ) as any as Op<Void>

// return zero or one value from this function
const ReturnVoid = new instr_atom(op.return)
const Return = <R extends N>(value :R) =>
  new instr_pre1(op.return, value) as any as Op<R>


// ————————— Calling —————————

const Call = <R extends ValueType>(funcIndex: VarUint32, args :Op<N>[]) =>
  new instr_pre_imm(0x10, args, [funcIndex]) as any as Op<R>

const CallIndirect = <R extends ValueType>(funcIndex: VarUint32, args :Op<N>[]) =>
  new instr_pre_imm(0x11, args, [funcIndex, varuint1_0]) as any as Op<R>


// ————————— Parametric —————————

// drop: discards the value of its operand
// R should be the value on the stack "under" the operand. E.g. with a stack:
//   I32  top
//   F64  ...
//   F32  bottom
// drop
//   F64  top
//   F32  bottom
// i.e. R=F64
const Drop = <R>(n :Op<N>) =>
  new instr_pre1(0x1a, n) as any as Op<R>

// select one of two values based on condition
const Select = <T extends ValueType>(cond :Op<I32>, trueRes :Op<T>, falseRes :Op<T>) =>
  new instr_pre(0x1b, [trueRes, falseRes, cond]) as any as Op<T>


// ————————— Variable access —————————

const GetLocal = <R>(localIndex :uint32) =>
  new instr_imm1(0x20, VarUint32(localIndex)) as any as Op<R>

const SetLocal = <T>(localIndex :uint32, expr :Op<T>) =>
  new instr_pre_imm(0x21, [expr], [VarUint32(localIndex)]) as any as Op<Void>

const TeeLocal = <R>(localIndex :uint32, expr :Op<R>) =>
  new instr_pre_imm(0x22, [expr], [VarUint32(localIndex)]) as any as Op<R>

const GetGlobal = <R>(globalIndex :uint32) =>
  new instr_imm1(0x23, VarUint32(globalIndex)) as any as Op<R>

const SetGlobal = <T>(globalIndex :uint32, expr :Op<T>) =>
  new instr_pre_imm(0x24, [expr], [VarUint32(globalIndex)]) as any as Op<Void>


// ————————— Memory —————————

const CurrentMemory = // query the size of memory (number of pages)
  new instr_imm1(op.current_memory, varuint1_0) as any as Op<INative>

// grow the size of memory by `delta` memory pages
const GrowMemory = (delta :Op<INative>) =>
  assert(delta.v >= 0) &&
  new instr_pre_imm(op.grow_memory, [delta], [varuint1_0]) as any as Op<INative>


// ————————— Numeric operators grouped per type —————————

const memload = <R, A extends Op<N>>(op :OpCode, mi :MemImm, addr :A) =>
  new instr_pre_imm(op, [addr], mi) as any as Op<R>

const memstore = <T, A extends Op<N>>(op :OpCode, mi :MemImm, addr :A, v :Op<T>) =>
  new instr_pre_imm(op, [addr, v], mi) as any as Op<Void>

// MemImm:        flags (alignment)  offset
const Align8  = [ varUint32Cache[0], varUint32Cache[0] ] as [VarUint32,INative]
const Align16 = [ varUint32Cache[1], varUint32Cache[0] ] as [VarUint32,INative]
const Align32 = [ varUint32Cache[2], varUint32Cache[0] ] as [VarUint32,INative]
const Align64 = [ varUint32Cache[3], varUint32Cache[0] ] as [VarUint32,INative]

const i32 = {
  // Constants
  constv(v :VarInt32) { return new instr_imm1(0x41, v) as any as Op<I32> },
  const(v :int32) { return this.constv(VarInt32(v)) },

  // Memory
  load:     memload.bind(null,0x28)  as (mi :MemImm, addr :Op<INative>) => Op<I32>,
  load8_s:  memload.bind(null,0x2c)  as (mi :MemImm, addr :Op<INative>) => Op<I32>,
  load8_u:  memload.bind(null,0x2d)  as (mi :MemImm, addr :Op<INative>) => Op<I32>,
  load16_s: memload.bind(null,0x2e)  as (mi :MemImm, addr :Op<INative>) => Op<I32>,
  load16_u: memload.bind(null,0x2f)  as (mi :MemImm, addr :Op<INative>) => Op<I32>,
  store:    memstore.bind(null,0x36) as (mi :MemImm, addr :Op<INative>, v :Op<I32>) => Op<Void>,
  store8:   memstore.bind(null,0x3a) as (mi :MemImm, addr :Op<INative>, v :Op<I32>) => Op<Void>,
  store16:  memstore.bind(null,0x3b) as (mi :MemImm, addr :Op<INative>, v :Op<I32>) => Op<Void>,

  // Comparison
  eqz (a :Op<I32>)             { return new instr_pre1(0x45,a)    as any as Op<I32> },
  eq  (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x46,[a,b]) as any as Op<I32> },
  ne  (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x47,[a,b]) as any as Op<I32> },
  lt_s(a :Op<I32>, b :Op<I32>) { return new instr_pre(0x48,[a,b]) as any as Op<I32> },
  lt_u(a :Op<I32>, b :Op<I32>) { return new instr_pre(0x49,[a,b]) as any as Op<I32> },
  gt_s(a :Op<I32>, b :Op<I32>) { return new instr_pre(0x4a,[a,b]) as any as Op<I32> },
  gt_u(a :Op<I32>, b :Op<I32>) { return new instr_pre(0x4b,[a,b]) as any as Op<I32> },
  le_s(a :Op<I32>, b :Op<I32>) { return new instr_pre(0x4c,[a,b]) as any as Op<I32> },
  le_u(a :Op<I32>, b :Op<I32>) { return new instr_pre(0x4d,[a,b]) as any as Op<I32> },
  ge_s(a :Op<I32>, b :Op<I32>) { return new instr_pre(0x4e,[a,b]) as any as Op<I32> },
  ge_u(a :Op<I32>, b :Op<I32>) { return new instr_pre(0x4f,[a,b]) as any as Op<I32> },

  // Numeric
  clz   (a :Op<I32>)             { return new instr_pre1(0x67,a)    as any as Op<I32> },
  ctz   (a :Op<I32>)             { return new instr_pre1(0x68,a)    as any as Op<I32> },
  popcnt(a :Op<I32>)             { return new instr_pre1(0x69,a)    as any as Op<I32> },
  add   (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x6a,[a,b]) as any as Op<I32> },
  sub   (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x6b,[a,b]) as any as Op<I32> },
  mul   (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x6c,[a,b]) as any as Op<I32> },
  div_s (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x6d,[a,b]) as any as Op<I32> },
  div_u (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x6e,[a,b]) as any as Op<I32> },
  rem_s (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x6f,[a,b]) as any as Op<I32> },
  rem_u (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x70,[a,b]) as any as Op<I32> },
  and   (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x71,[a,b]) as any as Op<I32> },
  or    (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x72,[a,b]) as any as Op<I32> },
  xor   (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x73,[a,b]) as any as Op<I32> },
  shl   (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x74,[a,b]) as any as Op<I32> },
  shr_s (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x75,[a,b]) as any as Op<I32> },
  shr_u (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x76,[a,b]) as any as Op<I32> },
  rotl  (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x77,[a,b]) as any as Op<I32> },
  rotr  (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x78,[a,b]) as any as Op<I32> },

  // Conversion
  wrap_i64        (a :Op<I64>) { return new instr_pre1(0xa7,a) as any as Op<I32> },
  trunc_s_f32     (a :Op<F32>) { return new instr_pre1(0xa8,a) as any as Op<I32> },
  trunc_u_f32     (a :Op<F32>) { return new instr_pre1(0xa9,a) as any as Op<I32> },
  trunc_s_f64     (a :Op<F64>) { return new instr_pre1(0xaa,a) as any as Op<I32> },
  trunc_u_f64     (a :Op<F64>) { return new instr_pre1(0xab,a) as any as Op<I32> },
  reinterpret_f32 (a :Op<F32>) { return new instr_pre1(0xbc,a) as any as Op<I32> },
}

const i64 = {
  // Constants
  constv(v :VarInt64) { return new instr_imm1(0x42, v) as any as Op<I64> },
  const(v :int64) { return this.constv(VarInt64(v)) },

  // Memory
  load:     memload.bind(null,0x29)  as (mi :MemImm, addr :Op<INative>) => Op<I64>,
  load8_s:  memload.bind(null,0x30)  as (mi :MemImm, addr :Op<INative>) => Op<I64>,
  load8_u:  memload.bind(null,0x31)  as (mi :MemImm, addr :Op<INative>) => Op<I64>,
  load16_s: memload.bind(null,0x32)  as (mi :MemImm, addr :Op<INative>) => Op<I64>,
  load16_u: memload.bind(null,0x33)  as (mi :MemImm, addr :Op<INative>) => Op<I64>,
  load32_s: memload.bind(null,0x34)  as (mi :MemImm, addr :Op<INative>) => Op<I64>,
  load32_u: memload.bind(null,0x35)  as (mi :MemImm, addr :Op<INative>) => Op<I64>,
  store:    memstore.bind(null,0x37) as (mi :MemImm, addr :Op<INative>, v :Op<I64>) => Op<Void>,
  store8:   memstore.bind(null,0x3c) as (mi :MemImm, addr :Op<INative>, v :Op<I64>) => Op<Void>,
  store16:  memstore.bind(null,0x3d) as (mi :MemImm, addr :Op<INative>, v :Op<I64>) => Op<Void>,
  store32:  memstore.bind(null,0x3e) as (mi :MemImm, addr :Op<INative>, v :Op<I64>) => Op<Void>,

  // Comparison
  eqz (a :Op<I64>)             { return new instr_pre1(0x50,a)    as any as Op<I64> },
  eq  (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x51,[a,b]) as any as Op<I64> },
  ne  (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x52,[a,b]) as any as Op<I64> },
  lt_s(a :Op<I64>, b :Op<I64>) { return new instr_pre(0x53,[a,b]) as any as Op<I64> },
  lt_u(a :Op<I64>, b :Op<I64>) { return new instr_pre(0x54,[a,b]) as any as Op<I64> },
  gt_s(a :Op<I64>, b :Op<I64>) { return new instr_pre(0x55,[a,b]) as any as Op<I64> },
  gt_u(a :Op<I64>, b :Op<I64>) { return new instr_pre(0x56,[a,b]) as any as Op<I64> },
  le_s(a :Op<I64>, b :Op<I64>) { return new instr_pre(0x57,[a,b]) as any as Op<I64> },
  le_u(a :Op<I64>, b :Op<I64>) { return new instr_pre(0x58,[a,b]) as any as Op<I64> },
  ge_s(a :Op<I64>, b :Op<I64>) { return new instr_pre(0x59,[a,b]) as any as Op<I64> },
  ge_u(a :Op<I64>, b :Op<I64>) { return new instr_pre(0x5a,[a,b]) as any as Op<I64> },

  // Numeric
  clz   (a :Op<I64>)             { return new instr_pre1(0x79,a)    as any as Op<I64> },
  ctz   (a :Op<I64>)             { return new instr_pre1(0x7a,a)    as any as Op<I64> },
  popcnt(a :Op<I64>)             { return new instr_pre1(0x7b,a)    as any as Op<I64> },
  add   (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x7c,[a,b]) as any as Op<I64> },
  sub   (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x7d,[a,b]) as any as Op<I64> },
  mul   (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x7e,[a,b]) as any as Op<I64> },
  div_s (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x7f,[a,b]) as any as Op<I64> },
  div_u (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x80,[a,b]) as any as Op<I64> },
  rem_s (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x81,[a,b]) as any as Op<I64> },
  rem_u (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x82,[a,b]) as any as Op<I64> },
  and   (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x83,[a,b]) as any as Op<I64> },
  or    (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x84,[a,b]) as any as Op<I64> },
  xor   (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x85,[a,b]) as any as Op<I64> },
  shl   (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x86,[a,b]) as any as Op<I64> },
  shr_s (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x87,[a,b]) as any as Op<I64> },
  shr_u (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x88,[a,b]) as any as Op<I64> },
  rotl  (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x89,[a,b]) as any as Op<I64> },
  rotr  (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x8a,[a,b]) as any as Op<I64> },


  // Conversions
  extend_s_i32    (a :Op<I32>) { return new instr_pre1(0xac,a) as any as Op<I64> },
  extend_u_i32    (a :Op<I32>) { return new instr_pre1(0xad,a) as any as Op<I64> },
  trunc_s_f32     (a :Op<F32>) { return new instr_pre1(0xae,a) as any as Op<I64> },
  trunc_u_f32     (a :Op<F32>) { return new instr_pre1(0xaf,a) as any as Op<I64> },
  trunc_s_f64     (a :Op<F64>) { return new instr_pre1(0xb0,a) as any as Op<I64> },
  trunc_u_f64     (a :Op<F64>) { return new instr_pre1(0xb1,a) as any as Op<I64> },
  reinterpret_f64 (a :Op<F64>) { return new instr_pre1(0xbd,a) as any as Op<I64> },
}

const f32 = {
  // Constants
  constv(v :Float32) { return new instr_imm1(0x43, v) as any as Op<F32> },
  const(v :float32) { return this.constv(Float32(v)) },

  // Memory
  load:  memload.bind(null,0x2a)  as (mi :MemImm, addr :Op<INative>) => Op<F32>,
  store: memstore.bind(null,0x38) as (mi :MemImm, addr :Op<INative>, v :Op<F32>) => Op<Void>,

  // Comparison
  eq(a :Op<F32>, b :Op<F32>) { return new instr_pre(0x5b,[a,b]) as any as Op<F32> },
  ne(a :Op<F32>, b :Op<F32>) { return new instr_pre(0x5c,[a,b]) as any as Op<F32> },
  lt(a :Op<F32>, b :Op<F32>) { return new instr_pre(0x5d,[a,b]) as any as Op<F32> },
  gt(a :Op<F32>, b :Op<F32>) { return new instr_pre(0x5e,[a,b]) as any as Op<F32> },
  le(a :Op<F32>, b :Op<F32>) { return new instr_pre(0x5f,[a,b]) as any as Op<F32> },
  ge(a :Op<F32>, b :Op<F32>) { return new instr_pre(0x60,[a,b]) as any as Op<F32> },

  // Numeric
  abs     (a :Op<F32>) { return new instr_pre1(0x8b,a) as any as Op<F32> },
  neg     (a :Op<F32>) { return new instr_pre1(0x8c,a) as any as Op<F32> },
  ceil    (a :Op<F32>) { return new instr_pre1(0x8d,a) as any as Op<F32> },
  floor   (a :Op<F32>) { return new instr_pre1(0x8e,a) as any as Op<F32> },
  trunc   (a :Op<F32>) { return new instr_pre1(0x8f,a) as any as Op<F32> },
  nearest (a :Op<F32>) { return new instr_pre1(0x90,a) as any as Op<F32> },
  sqrt    (a :Op<F32>) { return new instr_pre1(0x91,a) as any as Op<F32> },
  add     (a :Op<F32>, b :Op<F32>) { return new instr_pre(0x92,[a,b]) as any as Op<F32> },
  sub     (a :Op<F32>, b :Op<F32>) { return new instr_pre(0x93,[a,b]) as any as Op<F32> },
  mul     (a :Op<F32>, b :Op<F32>) { return new instr_pre(0x94,[a,b]) as any as Op<F32> },
  div     (a :Op<F32>, b :Op<F32>) { return new instr_pre(0x95,[a,b]) as any as Op<F32> },
  min     (a :Op<F32>, b :Op<F32>) { return new instr_pre(0x96,[a,b]) as any as Op<F32> },
  max     (a :Op<F32>, b :Op<F32>) { return new instr_pre(0x97,[a,b]) as any as Op<F32> },
  copysign(a :Op<F32>, b :Op<F32>) { return new instr_pre(0x98,[a,b]) as any as Op<F32> },

  // Conversion
  convert_s_i32  (a :Op<I32>) { return new instr_pre1(0xb2,a) as any as Op<F32> },
  convert_u_i32  (a :Op<I32>) { return new instr_pre1(0xb3,a) as any as Op<F32> },
  convert_s_i64  (a :Op<I64>) { return new instr_pre1(0xb4,a) as any as Op<F32> },
  convert_u_i64  (a :Op<I64>) { return new instr_pre1(0xb5,a) as any as Op<F32> },
  demote_f64     (a :Op<F64>) { return new instr_pre1(0xb6,a) as any as Op<F32> },
  reinterpret_i32(a :Op<I32>) { return new instr_pre1(0xbe,a) as any as Op<F32> },
}

const f64 = {
  // Constants
  constv(v :Float64) { return new instr_imm1(0x44, v) as any as Op<F64> },
  const(v :float64) { return this.constv(Float64(v)) },

  // Memory
  load:  memload.bind(null,0x2b)  as (mi :MemImm, addr :Op<INative>) => Op<F64>,
  store: memstore.bind(null,0x39) as (mi :MemImm, addr :Op<INative>, v :Op<F64>) => Op<Void>,

  // Comparison
  eq(a :Op<F64>, b :Op<F64>) { return new instr_pre(0x61,[a,b]) as any as Op<F64> },
  ne(a :Op<F64>, b :Op<F64>) { return new instr_pre(0x62,[a,b]) as any as Op<F64> },
  lt(a :Op<F64>, b :Op<F64>) { return new instr_pre(0x63,[a,b]) as any as Op<F64> },
  gt(a :Op<F64>, b :Op<F64>) { return new instr_pre(0x64,[a,b]) as any as Op<F64> },
  le(a :Op<F64>, b :Op<F64>) { return new instr_pre(0x65,[a,b]) as any as Op<F64> },
  ge(a :Op<F64>, b :Op<F64>) { return new instr_pre(0x66,[a,b]) as any as Op<F64> },

  // Numeric
  abs     (a :Op<F64>) { return new instr_pre1(0x99,a) as any as Op<F64> },
  neg     (a :Op<F64>) { return new instr_pre1(0x9a,a) as any as Op<F64> },
  ceil    (a :Op<F64>) { return new instr_pre1(0x9b,a) as any as Op<F64> },
  floor   (a :Op<F64>) { return new instr_pre1(0x9c,a) as any as Op<F64> },
  trunc   (a :Op<F64>) { return new instr_pre1(0x9d,a) as any as Op<F64> },
  nearest (a :Op<F64>) { return new instr_pre1(0x9e,a) as any as Op<F64> },
  sqrt    (a :Op<F64>) { return new instr_pre1(0x9f,a) as any as Op<F64> },
  add     (a :Op<F64>, b :Op<F64>) { return new instr_pre(0xa0,[a,b]) as any as Op<F64> },
  sub     (a :Op<F64>, b :Op<F64>) { return new instr_pre(0xa1,[a,b]) as any as Op<F64> },
  mul     (a :Op<F64>, b :Op<F64>) { return new instr_pre(0xa2,[a,b]) as any as Op<F64> },
  div     (a :Op<F64>, b :Op<F64>) { return new instr_pre(0xa3,[a,b]) as any as Op<F64> },
  min     (a :Op<F64>, b :Op<F64>) { return new instr_pre(0xa4,[a,b]) as any as Op<F64> },
  max     (a :Op<F64>, b :Op<F64>) { return new instr_pre(0xa5,[a,b]) as any as Op<F64> },
  copysign(a :Op<F64>, b :Op<F64>) { return new instr_pre(0xa6,[a,b]) as any as Op<F64> },

  // Conversions
  convert_s_i32  (a :Op<I32>) { return new instr_pre1(0xb7,a) as any as Op<F64> },
  convert_u_i32  (a :Op<I32>) { return new instr_pre1(0xb8,a) as any as Op<F64> },
  convert_s_i64  (a :Op<I64>) { return new instr_pre1(0xb9,a) as any as Op<F64> },
  convert_u_i64  (a :Op<I64>) { return new instr_pre1(0xba,a) as any as Op<F64> },
  promote_f32    (a :Op<F32>) { return new instr_pre1(0xbb,a) as any as Op<F64> },
  reinterpret_i64(a :Op<I64>) { return new instr_pre1(0xbf,a) as any as Op<F64> },
}


//——————————————————————————————————————————————————————————————————————————————


// const s_crypto = PrefixStrASCII("crypto")

const mod = Module(0xd, [
  TypeSection([
    FuncType([]),         // 0
    FuncType([I32], I32), // 1
    FuncType([F32], F32), // 2
    // FuncType([F32], I32), // 3
  ]),

  // ImportSection([
  //   // Note: MVP only allows one table and one memory
  //   FunctionImportEntry(s_crypto, PrefixStrASCII("rand"), VarUint32(1)),
  //   // GlobalImportEntry(s_crypto, PrefixStrASCII("version"), GlobalType(I32)),
  //   // MemoryImportEntry(s_crypto, PrefixStrASCII("data"),
  //   //   ResizableLimits(VarUint32(0), VarUint32(8))
  //   // ),
  //   // TableImportEntry(s_crypto, PrefixStrASCII("ciphers"),
  //   //   TableType(AnyFunc, ResizableLimits(VarUint32(0), VarUint32(8)))
  //   // ),
  // ]),

  // Function offsets are: imported... + local...
  FunctionSection([
    VarUint32(0),
    VarUint32(1),
    VarUint32(2),
  ]),

  // Note: MVP only allows a total of 1 table; either a TableSection OR a TableImportEntry
  TableSection([
    // must have enought size for ElementSection's element segments
    TableType(AnyFunc, ResizableLimits(VarUint32(1))),
  ]),

  // // Note: MVP only allows a total of 1 memory; either a MemorySection OR a MemoryImportEntry
  MemorySection([
    ResizableLimits(VarUint32(1)),
  ]),
  
  GlobalSection([
    GlobalVariable(GlobalType(I32), InitExpr([
      i32.const(0)
    ])),
  ]),

  ExportSection([
    ExportEntry(PrefixStrASCII("foo"), ExternalKindFunction, VarUint32(1)),
    ExportEntry(PrefixStrASCII("bar"), ExternalKindFunction, VarUint32(2)),
  ]),

  StartSection(VarUint32(0)),

  ElementSection([
    ElemSegment(VarUint32(0), InitExpr([ i32.const(0) ]), [VarUint32(0)]),
  ]),

  CodeSection([

    FunctionBody([], []), // ()

    // func (local0 i32) i32 {
    FunctionBody(
      [LocalEntry(VarUint32(1), I32)], // var local1 = 0
      [
        SetLocal(1,
          i32.mul(
            GetLocal<I32>(0),
            i32.const(4)
          ),
        ),

        // Loop(Void, [
        //   Drop(f32.const(123.4567)),
        //   Nop,
        //   Nop,
        // ]),

        // if (local1 <= 0) { set local1 = 1 }  // avoid infinite loop below
        If(Void, i32.le_s(GetLocal<I32>(1), i32.const(0)), [
          SetLocal(1, i32.const(1))
        ]),

        // do {
        //   set local1 = (local1 * 2)
        // } while (local1 < 9000)
        // Loop(Void, [
        //   SetLocal(1,
        //     i32.mul(GetLocal<I32>(1), i32.const(2)) ), // set local1 = local1 * 2
        //   BrIf(0, // continue if
        //     i32.lt_u(GetLocal<I32>(1), i32.const(9000)) ), // local1 < 9000
        // ]),

        // while ( (set local1 = (local1 * 2)) < 9000 ) {
        // }
        Loop(Void, [
          BrIf(0, // (continue-if (lt (set local1 (* (get local1) 2)) 9000))
            i32.lt_u(
              TeeLocal(1, i32.mul(GetLocal<I32>(1), i32.const(2))),
              i32.const(9000))
            ),
        ]),

        // If(Void,
        //   i32.const(1),                         // cond
        //   [Nop, Drop<Void>(i32.const(10000)) ], // then
        //   [Nop, Drop<Void>(i32.const(0)) ],     // else
        // ),

        SetLocal(1,
          If(I32,
            i32.lt_s(i32.const(1), i32.const(3)), // condition
            [ // then
              Nop,
              i32.wrap_i64(
                i64.mul(
                  i64.extend_s_i32(
                    GetLocal<I32>(1)),
                  i64.const(-0xffffffff)))
            ],[ // else
              Nop,
              i32.const(0)
            ],
          ),
        ),

        SetLocal(1,
          Select(
            GetLocal<I32>(1),
            i32.mul(
              GetLocal<I32>(1),
              i32.trunc_s_f32(
                Call<F32>(VarUint32(2), [
                  f32.const(2)
                ])
              )
            ),
            i32.const(333)
          )
        ),

        i32.store(Align32, i32.const(0),
          GetLocal<I32>(1)
        ),
        GrowMemory(i32.const(2)),
        SetLocal(1, i32.const(0)),
        Return(
          i32.load(Align32, i32.const(0)), // load previosuly stored result
          // i32.load(Align32, i32.const(4)), // load from data section
          // GetLocal<I32>(1)
          // i32.const(1)
          // CurrentMemory // query number of allocated memory pages
        )
      ]
    ),

    // func (local0 f32) f32 {
    //   return local0 + 123.4567
    // }
    FunctionBody(
      [], // no locals
      [
        f32.add(GetLocal<F32>(0), f32.const(123.4567))
      ]
    ),

  ]), // end CodeSection

  DataSection([
    DataSegment(
      VarUint32(0),              // linear memory index (==0 in MVP)
      InitExpr([i32.const(0)]),  // offset at which to place the data
      Data([0,0,0,0,  44,0,0,0])
    ),
  ]),

  // CustomSection(PrefixStrASCII("names"), []),
  // CustomSection(PrefixStrUTF8("Äntligen"), [ Data([1,2,3]) ]),
])

// console.log(require('util').inspect(mod, {colors:true, depth:10}), '\n')
console.log(mod.repr(), '\n')


//——————————————————————————————————————————————————————————————————————————————

function BufferedEmitter(size :number) {
  const buffer = new ArrayBuffer(size as number)
  return {
    buffer,
    view:   new DataView(buffer),
    length: 0,

    writeU8(v :uint8) :Emitter {
      this.view.setUint8(this.length++, v)
      return this
    },

    writeU16(v :uint16) :Emitter {
      this.view.setUint16(this.length, v, true)
      this.length += 2
      return this
    },

    writeU32(v :uint32) :Emitter {
      this.view.setUint32(this.length, v, true)
      this.length += 4
      return this
    },

    writeF32(v :float32) :Emitter {
      this.view.setFloat32(this.length, v, true)
      this.length += 4
      return this
    },

    writeF64(v :float64) :Emitter {
      this.view.setFloat64(this.length, v, true)
      this.length += 8
      return this
    },

    writeBytes(bytes :ArrayLike<uint8>) {
      for (let i = 0, L = bytes.length; i != L; ++i) {
        this.view.setUint8(this.length++, bytes[i])
      }
      return this
    },

    repr(writeln :(s:string)=>void, limit? :number, highlightRange? :number[]) {
      limit = Math.min(this.length, limit || Infinity)
      if (highlightRange && !highlightRange[1]) {
        highlightRange[1] = highlightRange[0]+1
      }
      let s = [], i = 0
      for (; i < limit; ++i) {
        let b = this.view.getUint8(i)
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
          writeln(s.join(' '));
          s = [];
        } else if (s.length % 5 == 4) {
          s.push('')
        }
      }
      const tail = (highlightRange && highlightRange[0] >= i) ? '\x1B[45;97m..\x1B[0m' : ''
      if (s.length) {
        writeln(s.join(' ') + (tail ? ' ' + tail : ''));
      } else if (tail) {
        writeln(tail)
      }
    },
  }
}

// emit WASM code
const emitter = BufferedEmitter(mod.z)
mod.emit(emitter)
emitter.repr(s => console.log(s))


// Copy the output from emitter.repr and paste it into the multiline string
// of this chunk, then paste & run in a JS env with WebAssembly:
//
// WebAssembly.compile(new Uint8Array(
//   `00 61 73 6d  0d 00 00 00  01 09 02 60  00 00 60 01
// 7f 01 7f 03  03 02 00 01  05 04 01 00  80 01 07 07
// 01 03 66 6f  6f 00 01 08  01 00 0a 3a  02 02 00 0b
// 35 01 01 7f  20 00 41 04  6c 21 01 03  40 01 01 01
// 0b 03 7f 41  01 0b 20 01  41 e4 00 6c  41 cd 02 20
// 01 1b 21 01  41 00 20 01  36 02 00 41  00 21 01 41
// 00 28 02 00  0f 0b 0b 0e  01 00 41 00  0b 08 00 00
// 00 00 2c 00  00 00`.split(/[\s\r\n]+/g).map(v => parseInt(v, 16))
// )).then(mod => {
//   let m = new WebAssembly.Instance(mod)
//   console.log('m.exports.foo() =>', m.exports.foo(1))
// })


// Run generated WASM module binary through the spec interpreter and
// finally print the text format generated.
declare interface Buffer {
  buffer :ArrayBuffer
  byteLength :number
  from(a :ArrayLike<number>) :Buffer
  from(a :ArrayBuffer, byteOffset? :number, byteLength? :number) :Buffer
  from(s :string, encoding? :string) :Buffer
  slice(begin :number, end? :number) :ArrayBuffer
  toString(encoding? :string) :string
  [Symbol.toStringTag]: "ArrayBuffer"
}
declare interface NodejsProcess {
  exit(status:number)
}
declare var process :NodejsProcess
declare var Buffer :Buffer
declare var __dirname :string
require('fs').writeFileSync('out.wasm', Buffer.from(emitter.buffer))
require('child_process').execFile(
  __dirname + '/../wasm-spec/interpreter/wasm.opt',
  ['-d', 'out.wasm', '-o', 'out.wast'],
  { stdio: 'inherit' },
  (err :Error, stdout :Buffer, stderr :Buffer|string) => {
    if (err) {
      const errmsg = typeof stderr == 'string' ? stderr as string : stderr.toString('utf8')
      const m = /^[^:]+\.wasm:0x([0-9A-Fa-f]+)(?:-0x([0-9A-Fa-f]+)|):\s*(.+)/.exec(errmsg)
      if (!m || !m[1]) {
        throw err
      }
      console.log('')
      console.error(m[3])
      emitter.repr(s => console.log(s), 0, [parseInt(m[1], 16), parseInt(m[2], 16)])
      process.exit(1)
    } else {
      console.log(require('fs').readFileSync('out.wast', {encoding:'utf8'}))
    }
  }
)
