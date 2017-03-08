import {utf8} from './utf8'
import {Emittable, Emitter} from './emit'

// For some reason we can't import basic-types into this module.
// import {uint1,uint7,uint8,uint16,uint32,int7,int32,int64,float32,float64} from './basic-types'
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

const DEBUG = false
const assert = DEBUG ? function(cond :any, msg? :any) {
  if (!cond) { throw new Error('assertion failure'); }
} : function(){}

//——————————————————————————————————————————————————————————————————————————————
// Basic node types

export type TypeTag = symbol

export interface N extends Emittable {
  readonly t :TypeTag  // type
  readonly z :uint32   // size in bytes (includes size of any children)
  readonly v :any      // value
}

export interface Atom<T> extends N {
  readonly v :T
}

export interface Cell<T extends N> extends N {
  readonly v :T[]
}

//—————————————————————————————————————
// Formal types
//   We use a trick here to get the most out of TypeScripts type checker,
//   namely we specify interfaces that have "type tag" properties.
//   However, concrete types doesn't actually have these properties, so any
//   attempts to access these properties will always yield `undefined`.

export interface Module extends Cell<Section> { readonly _Module: undefined
  readonly version :uint32
}

export type Section = CustomSection
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
export interface MemorySection extends Cell<ResizableLimits>{readonly _MemorySection:undefined}
export interface GlobalSection   extends Cell<GlobalVariable> {readonly _GlobalSection:undefined}
export interface ExportSection   extends Cell<ExportEntry> { readonly _ExportSection: undefined }
export interface StartSection    extends Cell<Void> { readonly _StartSection: undefined }
export interface ElementSection  extends Cell<ElemSegment> { readonly _ElementSection: undefined}
export interface CodeSection     extends Cell<FunctionBody> { readonly _CodeSection: undefined }
export interface DataSection     extends Cell<DataSegment> { readonly _DataSection: undefined }

export interface ImportEntry extends Cell<N> { readonly _ImportEntry: undefined }
export interface ExportEntry extends Cell<N> { readonly _ExportEntry: undefined }

export interface FuncType extends Cell<N> { readonly _FuncType: undefined }
export interface TableType extends Cell<N> { readonly _TableType: undefined }
export interface GlobalType extends Cell<N> { readonly _GlobalType: undefined }

export interface ResizableLimits extends Cell<N> { readonly _ResizableLimits: undefined }
export interface GlobalVariable extends Cell<N> { readonly _GlobalVariable: undefined }
export interface ElemSegment extends Cell<N> { readonly _ElemSegment: undefined }
export interface DataSegment extends Cell<N> { readonly _DataSegment: undefined }

export interface InitExpr extends Cell<N> { readonly _InitExpr: undefined }
export interface FunctionBody extends Cell<N> { readonly _FunctionBody: undefined }
export interface LocalEntry extends Cell<N> { readonly _LocalEntry: undefined }

export interface Str extends Atom<ArrayLike<uint8>> { readonly _Str: undefined
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
export interface Void extends VarInt7 { readonly _Void :undefined }

export type Int = I32 | I64 // wasm32 | wasm64
export type Result = I32 | I64 | F32 | F64
export type AnyResult = Result | Void
export type AnyOp = Op<AnyResult>

export interface ValueType extends Atom<int32|int64|float32|float64> {}
type AnyFunc    = VarInt7
type Func       = VarInt7
type EmptyBlock = VarInt7
type ElemType   = AnyFunc
type ExternalKind = Uint8
type BlockType = ValueType | EmptyBlock

// Memory immediate.
// In wasm32, address operands and offset attributes have type i32
export type MemImm = [
  // flags - a bitfield which currently contains the alignment in the least
  // significant bits, encoded as log2(alignment)
  VarUint32,

  // offset - added to the address to form effective address.
  // Useful when the address is dynamic and the compiler wants to add some
  // constant amount of offset to the dynamically-produced address.
  // I.e. effective_address = address + offset
  Int
]

// Instruction opcodes
export type OpCode = uint8

export interface Op<R> extends N {
  readonly _Op   :R
  readonly r     :AnyResult
  readonly v     :OpCode
  readonly pre?  :N[] | N  // instrs. pushing values onto the stack, used by "pre" types
  readonly imm?  :N[] | N  // immediates, used by "imm" types
  readonly post? :N[]      // used by "post" types
}

// Operations on all number types
export interface NumOps<R extends Result> {
  const(v :number) :Op<R>
  load(mi :MemImm, addr :Op<Int>) :Op<R>
  store(mi :MemImm, addr :Op<Int>, v :Op<R>) :Op<Void>
  addrIsAligned(mi :MemImm, addr :number) :boolean
  eq(a :Op<R>, b :Op<R>) :Op<I32>
  ne(a :Op<R>, b :Op<R>) :Op<I32>
  add(a :Op<R>, b :Op<R>) :Op<R>
  sub(a :Op<R>, b :Op<R>) :Op<R>
  mul(a :Op<R>, b :Op<R>) :Op<R>
}

// Operations on all integer number types
export interface IntOps<R extends Result> extends NumOps<R> {
  // Memory
  load8_s(mi :MemImm, addr :Op<Int>) :Op<R>
  load8_u(mi :MemImm, addr :Op<Int>) :Op<R>
  load16_s(mi :MemImm, addr :Op<Int>) :Op<R>
  load16_u(mi :MemImm, addr :Op<Int>) :Op<R>
  store8(mi :MemImm, addr :Op<Int>, v :Op<R>) :Op<Void>
  store16(mi :MemImm, addr :Op<Int>, v :Op<R>) :Op<Void>

  // Comparison
  eqz (a :Op<R>)           :Op<I32>
  lt_s(a :Op<R>, b :Op<R>) :Op<I32>
  lt_u(a :Op<R>, b :Op<R>) :Op<I32>
  gt_s(a :Op<R>, b :Op<R>) :Op<I32>
  gt_u(a :Op<R>, b :Op<R>) :Op<I32>
  le_s(a :Op<R>, b :Op<R>) :Op<I32>
  le_u(a :Op<R>, b :Op<R>) :Op<I32>
  ge_s(a :Op<R>, b :Op<R>) :Op<I32>
  ge_u(a :Op<R>, b :Op<R>) :Op<I32>

  // Numeric
  clz   (a :Op<R>) :Op<R>
  ctz   (a :Op<R>) :Op<R>
  popcnt(a :Op<R>) :Op<R>
  add   (a :Op<R>, b :Op<R>) :Op<R>
  sub   (a :Op<R>, b :Op<R>) :Op<R>
  mul   (a :Op<R>, b :Op<R>) :Op<R>
  div_s (a :Op<R>, b :Op<R>) :Op<R>
  div_u (a :Op<R>, b :Op<R>) :Op<R>
  rem_s (a :Op<R>, b :Op<R>) :Op<R>
  rem_u (a :Op<R>, b :Op<R>) :Op<R>
  and   (a :Op<R>, b :Op<R>) :Op<R>
  or    (a :Op<R>, b :Op<R>) :Op<R>
  xor   (a :Op<R>, b :Op<R>) :Op<R>
  shl   (a :Op<R>, b :Op<R>) :Op<R>
  shr_s (a :Op<R>, b :Op<R>) :Op<R>
  shr_u (a :Op<R>, b :Op<R>) :Op<R>
  rotl  (a :Op<R>, b :Op<R>) :Op<R>
  rotr  (a :Op<R>, b :Op<R>) :Op<R>

  // Conversion
  trunc_s_f32(a :Op<F32>) :Op<R>
  trunc_u_f32(a :Op<F32>) :Op<R>
  trunc_s_f64(a :Op<F64>) :Op<R>
  trunc_u_f64(a :Op<F64>) :Op<R>
}

export interface FloatOps<R extends Result> extends NumOps<R> {
  // Comparison
  eq(a :Op<R>, b :Op<R>) :Op<I32>
  ne(a :Op<R>, b :Op<R>) :Op<I32>
  lt(a :Op<R>, b :Op<R>) :Op<I32>
  gt(a :Op<R>, b :Op<R>) :Op<I32>
  le(a :Op<R>, b :Op<R>) :Op<I32>
  ge(a :Op<R>, b :Op<R>) :Op<I32>

  // Numeric
  add     (a :Op<R>, b :Op<R>) :Op<R>
  sub     (a :Op<R>, b :Op<R>) :Op<R>
  mul     (a :Op<R>, b :Op<R>) :Op<R>
  abs     (a :Op<R>) :Op<R>
  neg     (a :Op<R>) :Op<R>
  ceil    (a :Op<R>) :Op<R>
  floor   (a :Op<R>) :Op<R>
  trunc   (a :Op<R>) :Op<R>
  nearest (a :Op<R>) :Op<R>
  sqrt    (a :Op<R>) :Op<R>
  div     (a :Op<R>, b :Op<R>) :Op<R>
  min     (a :Op<R>, b :Op<R>) :Op<R>
  max     (a :Op<R>, b :Op<R>) :Op<R>
  copysign(a :Op<R>, b :Op<R>) :Op<R>

  // Conversion
  convert_s_i32(a :Op<I32>) :Op<R>
  convert_u_i32(a :Op<I32>) :Op<R>
  convert_s_i64(a :Op<I64>) :Op<R>
  convert_u_i64(a :Op<I64>) :Op<R>
}

export interface I32ops extends I32, IntOps<I32> {
  constv(v :VarInt32) :Op<I32>
  const(v :int32) :Op<I32>
  wrap_i64(a :Op<I64>) :Op<I32>
  reinterpret_f32(a :Op<F32>) :Op<I32>
}

export interface I64ops extends I64, IntOps<I64> {
  constv(v :VarInt64) :Op<I64>
  const(v :int64) :Op<I64>
  load32_s(mi :MemImm, addr :Op<Int>) :Op<I64>
  load32_u(mi :MemImm, addr :Op<Int>) :Op<I64>
  store32(mi :MemImm, addr :Op<Int>, v :Op<Result>) :Op<Void>
  extend_s_i32(a :Op<I32>) :Op<I64>
  extend_u_i32(a :Op<I32>) :Op<I64>
  reinterpret_f64(a :Op<F64>) :Op<I64>
}

export interface F32ops extends F32, FloatOps<F32> {
  constv(v :Float32) :Op<F32>
  const(v :float32) :Op<F32>
  demote_f64(a :Op<F64>) :Op<F32>
  reinterpret_i32(a :Op<I32>) :Op<F32>
}

export interface F64ops extends F64, FloatOps<F64> {
  constv(v :Float64) :Op<F64>
  const(v :float64) :Op<F64>
  promote_f32(a :Op<F32>) :Op<F64>
  reinterpret_i64(a :Op<I64>) :Op<F64>
}

//——————————————————————————————————————————————————————————————————————————————
// Type tags

const T = {
  // Atoms
  uint8:         Symbol('u8'),
  uint16:        Symbol('u16'),
  uint32:        Symbol('u32'),
  varuint1:      Symbol('vu1'),
  varuint7:      Symbol('vu7'),
  varuint32:     Symbol('vu32'),
  varint7:       Symbol('vs7'),
  varint32:      Symbol('vs32'),
  varint64:      Symbol('vs64'),
  float32:       Symbol('f32'), // non-standard
  float64:       Symbol('f64'), // non-standard
  data:          Symbol('data'), // non-standard
  type:          Symbol('type'), // non-standard, signifies a varint7 type constant
  external_kind: Symbol('type'),

  // Instructions
  instr:              Symbol('instr'), // non-standard
  instr_pre:          Symbol('instr_pre'), // non-standard
  instr_pre1:         Symbol('instr_pre1'), // non-standard
  instr_imm1:         Symbol('instr_imm1'), // non-standard
  instr_imm1_post:    Symbol('instr_imm1_post'), // non-standard
  instr_pre_imm:      Symbol('instr_pre_imm'), // non-standard
  instr_pre_imm_post: Symbol('instr_pre_imm_post'), // non-standard

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
  str:              Symbol('str'), // non-standard
}

//——————————————————————————————————————————————————————————————————————————————
// node structs

const writev = (e :Emitter, objs :Emittable[]) :Emitter => objs.reduce((e, n) => n.emit(e), e)

const sumz = function(n :N[]) {
  let sum = 0
  for (let i = 0, L = n.length; i != L; ++i) {
    sum += n[i].z
  }
  return sum
}

const readVarInt7 = (byte :uint8) :int7 =>
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
}

class val_atom<T> implements Atom<T> {
  readonly t :TypeTag
  readonly z :uint32
  readonly v :T

  constructor(t :TypeTag, z :uint32, v :T) { this.t = t; this.z = z; this.v = v }
  emit(e :Emitter) { return e } // override in subclasses
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
  constructor(v :uint32) { super(T.uint32, 4, v) }
  emit(e :Emitter) { return e.writeU32(this.v) }
}

class f32_atom extends val_atom<float32> {
  constructor(v :number) { super(T.float32, 4, v) }
  emit(e :Emitter) { return e.writeF32(this.v) }
}

class f64_atom extends val_atom<float64> {
  constructor(v :number) { super(T.float64, 8, v) }
  emit(e :Emitter) { return e.writeF64(this.v) }
}

class u8_atom<T extends number> extends val_atom<T> {
  constructor(t :TypeTag, v :T) { super(t, 1, v) }
  emit(e :Emitter) { return e.writeU8(this.v) }
}

class type_atom extends u8_atom<int7> {
  readonly b :uint8
  constructor(v :int7, b :uint8) { super(T.type, v); this.b = b }
  emit(e :Emitter) { return e.writeU8(this.b) }
}

class str_atom implements Atom<ArrayLike<uint8>> {
  readonly t   :TypeTag
  readonly z   :uint32
  readonly v   :ArrayLike<uint8>
  readonly len :VarUint32

  constructor(len: VarUint32, v :ArrayLike<uint8>) {
    assert(len.v == v.length)
    this.t = T.str
    this.z = len.z + v.length
    this.v = v
    this.len = len
  }
  emit(e :Emitter) { return this.len.emit(e).writeBytes(this.v) }
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
}

//—————————————————————————————————————————————

// Instructions

class instr_atom extends u8_atom<uint8> {
  readonly r :AnyResult
  constructor(v :uint8, r :AnyResult) { super(T.instr, v); this.r = r }
}

class instr_cell implements N {
  readonly t   :TypeTag
  readonly z   :uint32
  readonly v   :OpCode
  readonly r   :AnyResult

  constructor(t :TypeTag, op :uint8, r :AnyResult, z :uint32) {
    this.t = t
    this.z = z
    this.v = op
    this.r = r
  }
  emit(e :Emitter) { return e }
}

class instr_pre1 extends instr_cell {
  readonly pre :N
  constructor(op :uint8, r :AnyResult, pre :N) {
    super(T.instr_pre1, op, r, 1 + pre.z)
    this.pre = pre
  }
  emit(e :Emitter) { return this.pre.emit(e).writeU8(this.v) }
}

class instr_imm1 extends instr_cell {
  readonly imm :N
  constructor(op :uint8, r :AnyResult, imm :N) {
    super(T.instr_imm1, op, r, 1 + imm.z)
    this.imm = imm
  }
  emit(e :Emitter) { return this.imm.emit(e.writeU8(this.v)) }
}

class instr_pre extends instr_cell {
  readonly pre :N[]
  constructor(op :uint8, r :AnyResult, pre :N[]) {
    super(T.instr_pre, op, r, 1 + sumz(pre))
    this.pre = pre
  }
  emit(e :Emitter) { return writev(e, this.pre).writeU8(this.v) }
}

class instr_imm1_post extends instr_cell {
  readonly imm :N
  readonly post :N[]
  constructor(op :uint8, r :AnyResult, imm :N, post :N[]) {
    super(T.instr_imm1_post, op, r, 1 + imm.z + sumz(post))
    this.imm = imm
    this.post = post
  }
  emit(e :Emitter) { return writev(this.imm.emit(e.writeU8(this.v)), this.post) }
}

class instr_pre_imm extends instr_cell {
  readonly pre :N[]
  readonly imm :N[]
  constructor(op :uint8, r :AnyResult, pre :N[], imm :N[]) {
    super(T.instr_pre_imm, op, r, 1 + sumz(pre) + sumz(imm))
    this.pre = pre
    this.imm = imm
  }
  emit(e :Emitter) { return writev(writev(e, this.pre).writeU8(this.v), this.imm) }
}

class instr_pre_imm_post extends instr_cell {
  readonly pre  :N[]
  readonly imm  :N[]
  readonly post :N[]
  constructor(op :uint8, r :AnyResult, pre :N[], imm :N[], post :N[]) {
    super(T.instr_pre_imm_post, op, r, 1 + sumz(pre) + sumz(imm) + sumz(post))
    this.pre = pre
    this.imm = imm
    this.post = post
  }
  emit(e :Emitter) {
    return writev(writev(writev(e, this.pre).writeU8(this.v), this.imm), this.post)
  }
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
// constructors

const uint8Cache :Uint8[] = maprange(0,16, v =>
  new u8_atom<uint8>(T.uint8, v as uint8))
const varUint7Cache :VarUint7[] = maprange(0,16, v =>
  new u8_atom<uint7>(T.varuint7, v as uint8))
const varUint32Cache :VarUint7[] = maprange(0,16, v =>
  new u8_atom<uint32>(T.varuint32, v as uint8))
const varuint1_0 = new u8_atom<uint1>(T.varuint1, 0) as Atom<uint1>
const varuint1_1 = new u8_atom<uint1>(T.varuint1, 1) as Atom<uint1>

function uint8(v :uint8) {
  return uint8Cache[v] || new u8_atom<uint8>(T.uint8, v) as Uint8
}
function uint32(v :uint32)  { return new u32_atom(v) as Uint32 }
function float32(v :float32) { return new f32_atom(v) as Float32 }
function float64(v :float64) { return new f64_atom(v) as Float64 }

// LEB128-encoded variable-length integers: (N = bits)
//   unsigned range: [0, 2^N-1]
//   signed range:   [-2^(N-1), +2^(N-1)-1]

function varuint1(v :any) {
  return v ? varuint1_1 : varuint1_0
}

function varuint7(v :uint7) {
  assert(v >= 0 && v <= 128)
  return varUint7Cache[v] || new u8_atom<uint7>(T.varuint7, v) as VarUint7
}

function varuint32(value :uint32) {
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

  return new bytesval_atom<uint32>(T.varuint32, value, bytes) as VarUint32
}

function varint7(value :int7) {
  assert(value >= -64 && value <= 63);
  return new u8_atom<int7>(T.varint7, value < 0 ? (128 + value) : value) as VarInt7
}

function encVarIntN(v :int64) :uint8[] {
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

function varint32(value :int32) :VarInt32 {
  assert(value >= -0x80000000 && value <= 0x7fffffff)
  return new bytesval_atom<int32>(T.varint32, value, encVarIntN(value)) as VarInt32
}

function varint64(value :int64) :VarInt64 {
  // Here be dragons! Not all negative 64bit numbers can be represented with
  // JavaScript numbers. The ECMAScript double type has 53 bits of integer
  // precision. We thus assert this range
  assert(value >= Number.MIN_SAFE_INTEGER && value <= Number.MAX_SAFE_INTEGER)
  return new bytesval_atom<int64>(T.varint64, value, encVarIntN(value)) as VarInt64
}


// Language types
const AnyFunc    = new type_atom(-0x10, 0x70) as any as AnyFunc
const Func       = new type_atom(-0x20, 0x60) as any as Func
const EmptyBlock = new type_atom(-0x40, 0x40) as any as EmptyBlock
const Void       = EmptyBlock as any as Void

const external_kind_function = new u8_atom<uint8>(T.external_kind, 0) as any as ExternalKind
const external_kind_table    = new u8_atom<uint8>(T.external_kind, 1) as any as ExternalKind
const external_kind_memory   = new u8_atom<uint8>(T.external_kind, 2) as any as ExternalKind
const external_kind_global   = new u8_atom<uint8>(T.external_kind, 3) as any as ExternalKind

const str = (data: ArrayLike<uint8>) =>
  new str_atom(varuint32(data.length), data) as any as Str

const sect_id_custom = varuint7(0)
const sect_id_type = varuint7(1)
const sect_id_import = varuint7(2)
const sect_id_function = varuint7(3)
const sect_id_table = varuint7(4)
const sect_id_memory = varuint7(5)
const sect_id_global = varuint7(6)
const sect_id_export = varuint7(7)
const sect_id_start = varuint7(8)
const sect_id_element = varuint7(9)
const sect_id_code = varuint7(10)
const sect_id_data = varuint7(11)

export const sect_id = {
  custom: sect_id_custom,
  type: sect_id_type,
  import: sect_id_import,
  function: sect_id_function,
  table: sect_id_table,
  memory: sect_id_memory,
  global: sect_id_global,
  export: sect_id_export,
  start: sect_id_start,
  element: sect_id_element,
  code: sect_id_code,
  data: sect_id_data,
}

function section(id :VarUint7, imm :N, payload :N[]) {
  return new cell<N>(T.section,
    [id, varuint32(imm.z + sumz(payload)), imm, ...payload]
  )
}


const memload = <R extends Result>(op :OpCode, r :R, mi :MemImm, addr :Op<Int>) =>
  new instr_pre_imm(op, r, [addr], mi) as any as Op<R>

const memstore = (op :OpCode, mi :MemImm, addr :Op<Int>, v :Op<Result>) =>
  new instr_pre_imm(op, Void, [addr, v], mi) as any as Op<Void>

// memAddrIsAligned returns true if the memory operation will actually be aligned.
// Note: natAl and al should be encoded as log2(bits), i.e. 32bit = 2
const addrIsAligned = (natAl :uint32, al :uint32, offs :number, addr :number) =>
  al <= natAl &&
  ((addr + offs) % [1, 2, 4, 8][al]) == 0


class i32ops extends type_atom implements I32ops { readonly _I32: undefined;
  // Constants
  constv(v :VarInt32) { return new instr_imm1(0x41, this, v) as any as Op<I32> }
  const(v :int32) :Op<I32> { return this.constv(varint32(v)) }

  // Memory
  load(mi :MemImm, addr :Op<Int>) { return memload(0x28, this, mi, addr) }
  load8_s(mi :MemImm, addr :Op<Int>) { return memload(0x2c, this, mi, addr) }
  load8_u(mi :MemImm, addr :Op<Int>) { return memload(0x2d, this, mi, addr) }
  load16_s(mi :MemImm, addr :Op<Int>) { return memload(0x2e, this, mi, addr) }
  load16_u(mi :MemImm, addr :Op<Int>) { return memload(0x2f, this, mi, addr) }
  store(mi :MemImm, addr :Op<Int>, v :Op<I32>) { return memstore(0x36, mi, addr, v) }
  store8(mi :MemImm, addr :Op<Int>, v :Op<I32>) { return memstore(0x3a, mi, addr, v) }
  store16(mi :MemImm, addr :Op<Int>, v :Op<I32>) { return memstore(0x3b, mi, addr, v) }
  addrIsAligned(mi :MemImm, addr :number) { return addrIsAligned(2, mi[0].v, mi[1].v, addr) }

  // Comparison
  eqz (a :Op<I32>)             { return new instr_pre1(0x45,this,a)    as any as Op<I32> }
  eq  (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x46,this,[a,b]) as any as Op<I32> }
  ne  (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x47,this,[a,b]) as any as Op<I32> }
  lt_s(a :Op<I32>, b :Op<I32>) { return new instr_pre(0x48,this,[a,b]) as any as Op<I32> }
  lt_u(a :Op<I32>, b :Op<I32>) { return new instr_pre(0x49,this,[a,b]) as any as Op<I32> }
  gt_s(a :Op<I32>, b :Op<I32>) { return new instr_pre(0x4a,this,[a,b]) as any as Op<I32> }
  gt_u(a :Op<I32>, b :Op<I32>) { return new instr_pre(0x4b,this,[a,b]) as any as Op<I32> }
  le_s(a :Op<I32>, b :Op<I32>) { return new instr_pre(0x4c,this,[a,b]) as any as Op<I32> }
  le_u(a :Op<I32>, b :Op<I32>) { return new instr_pre(0x4d,this,[a,b]) as any as Op<I32> }
  ge_s(a :Op<I32>, b :Op<I32>) { return new instr_pre(0x4e,this,[a,b]) as any as Op<I32> }
  ge_u(a :Op<I32>, b :Op<I32>) { return new instr_pre(0x4f,this,[a,b]) as any as Op<I32> }

  // Numeric
  clz   (a :Op<I32>)             { return new instr_pre1(0x67,this,a)    as any as Op<I32> }
  ctz   (a :Op<I32>)             { return new instr_pre1(0x68,this,a)    as any as Op<I32> }
  popcnt(a :Op<I32>)             { return new instr_pre1(0x69,this,a)    as any as Op<I32> }
  add   (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x6a,this,[a,b]) as any as Op<I32> }
  sub   (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x6b,this,[a,b]) as any as Op<I32> }
  mul   (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x6c,this,[a,b]) as any as Op<I32> }
  div_s (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x6d,this,[a,b]) as any as Op<I32> }
  div_u (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x6e,this,[a,b]) as any as Op<I32> }
  rem_s (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x6f,this,[a,b]) as any as Op<I32> }
  rem_u (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x70,this,[a,b]) as any as Op<I32> }
  and   (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x71,this,[a,b]) as any as Op<I32> }
  or    (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x72,this,[a,b]) as any as Op<I32> }
  xor   (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x73,this,[a,b]) as any as Op<I32> }
  shl   (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x74,this,[a,b]) as any as Op<I32> }
  shr_s (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x75,this,[a,b]) as any as Op<I32> }
  shr_u (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x76,this,[a,b]) as any as Op<I32> }
  rotl  (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x77,this,[a,b]) as any as Op<I32> }
  rotr  (a :Op<I32>, b :Op<I32>) { return new instr_pre(0x78,this,[a,b]) as any as Op<I32> }

  // Conversion
  wrap_i64        (a :Op<I64>) { return new instr_pre1(0xa7,this,a) as any as Op<I32> }
  trunc_s_f32     (a :Op<F32>) { return new instr_pre1(0xa8,this,a) as any as Op<I32> }
  trunc_u_f32     (a :Op<F32>) { return new instr_pre1(0xa9,this,a) as any as Op<I32> }
  trunc_s_f64     (a :Op<F64>) { return new instr_pre1(0xaa,this,a) as any as Op<I32> }
  trunc_u_f64     (a :Op<F64>) { return new instr_pre1(0xab,this,a) as any as Op<I32> }
  reinterpret_f32 (a :Op<F32>) { return new instr_pre1(0xbc,this,a) as any as Op<I32> }
}

class i64ops extends type_atom implements I64ops { readonly _I64: undefined
  // Constants
  constv(v :VarInt64) { return new instr_imm1(0x42, this, v) as any as Op<I64> }
  const(v :int64) :Op<I64> { return this.constv(varint64(v)) }

  // Memory
  load(mi :MemImm, addr :Op<Int>)     { return memload(0x29, this, mi, addr) }
  load8_s(mi :MemImm, addr :Op<Int>)  { return memload(0x30, this, mi, addr) }
  load8_u(mi :MemImm, addr :Op<Int>)  { return memload(0x31, this, mi, addr) }
  load16_s(mi :MemImm, addr :Op<Int>) { return memload(0x32, this, mi, addr) }
  load16_u(mi :MemImm, addr :Op<Int>) { return memload(0x33, this, mi, addr) }
  load32_s(mi :MemImm, addr :Op<Int>) { return memload(0x34, this, mi, addr) }
  load32_u(mi :MemImm, addr :Op<Int>) { return memload(0x35, this, mi, addr) }
  store(mi :MemImm, addr :Op<Int>, v :Op<I64>)   { return memstore(0x37, mi, addr, v) }
  store8 (mi :MemImm, addr :Op<Int>, v :Op<I64>) { return memstore(0x3c, mi, addr, v) }
  store16(mi :MemImm, addr :Op<Int>, v :Op<I64>) { return memstore(0x3d, mi, addr, v) }
  store32(mi :MemImm, addr :Op<Int>, v :Op<I64>) { return memstore(0x3e, mi, addr, v) }
  addrIsAligned(mi :MemImm, addr :number) { return addrIsAligned(3, mi[0].v, mi[1].v, addr) }

  // Comparison
  eqz (a :Op<I64>)             { return new instr_pre1(0x50,this,a)    as any as Op<I32> }
  eq  (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x51,this,[a,b]) as any as Op<I32> }
  ne  (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x52,this,[a,b]) as any as Op<I32> }
  lt_s(a :Op<I64>, b :Op<I64>) { return new instr_pre(0x53,this,[a,b]) as any as Op<I32> }
  lt_u(a :Op<I64>, b :Op<I64>) { return new instr_pre(0x54,this,[a,b]) as any as Op<I32> }
  gt_s(a :Op<I64>, b :Op<I64>) { return new instr_pre(0x55,this,[a,b]) as any as Op<I32> }
  gt_u(a :Op<I64>, b :Op<I64>) { return new instr_pre(0x56,this,[a,b]) as any as Op<I32> }
  le_s(a :Op<I64>, b :Op<I64>) { return new instr_pre(0x57,this,[a,b]) as any as Op<I32> }
  le_u(a :Op<I64>, b :Op<I64>) { return new instr_pre(0x58,this,[a,b]) as any as Op<I32> }
  ge_s(a :Op<I64>, b :Op<I64>) { return new instr_pre(0x59,this,[a,b]) as any as Op<I32> }
  ge_u(a :Op<I64>, b :Op<I64>) { return new instr_pre(0x5a,this,[a,b]) as any as Op<I32> }

  // Numeric
  clz   (a :Op<I64>)             { return new instr_pre1(0x79,this,a)    as any as Op<I64> }
  ctz   (a :Op<I64>)             { return new instr_pre1(0x7a,this,a)    as any as Op<I64> }
  popcnt(a :Op<I64>)             { return new instr_pre1(0x7b,this,a)    as any as Op<I64> }
  add   (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x7c,this,[a,b]) as any as Op<I64> }
  sub   (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x7d,this,[a,b]) as any as Op<I64> }
  mul   (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x7e,this,[a,b]) as any as Op<I64> }
  div_s (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x7f,this,[a,b]) as any as Op<I64> }
  div_u (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x80,this,[a,b]) as any as Op<I64> }
  rem_s (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x81,this,[a,b]) as any as Op<I64> }
  rem_u (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x82,this,[a,b]) as any as Op<I64> }
  and   (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x83,this,[a,b]) as any as Op<I64> }
  or    (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x84,this,[a,b]) as any as Op<I64> }
  xor   (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x85,this,[a,b]) as any as Op<I64> }
  shl   (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x86,this,[a,b]) as any as Op<I64> }
  shr_s (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x87,this,[a,b]) as any as Op<I64> }
  shr_u (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x88,this,[a,b]) as any as Op<I64> }
  rotl  (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x89,this,[a,b]) as any as Op<I64> }
  rotr  (a :Op<I64>, b :Op<I64>) { return new instr_pre(0x8a,this,[a,b]) as any as Op<I64> }

  // Conversions
  extend_s_i32    (a :Op<I32>) { return new instr_pre1(0xac,this,a) as any as Op<I64> }
  extend_u_i32    (a :Op<I32>) { return new instr_pre1(0xad,this,a) as any as Op<I64> }
  trunc_s_f32     (a :Op<F32>) { return new instr_pre1(0xae,this,a) as any as Op<I64> }
  trunc_u_f32     (a :Op<F32>) { return new instr_pre1(0xaf,this,a) as any as Op<I64> }
  trunc_s_f64     (a :Op<F64>) { return new instr_pre1(0xb0,this,a) as any as Op<I64> }
  trunc_u_f64     (a :Op<F64>) { return new instr_pre1(0xb1,this,a) as any as Op<I64> }
  reinterpret_f64 (a :Op<F64>) { return new instr_pre1(0xbd,this,a) as any as Op<I64> }
}


class f32ops extends type_atom implements F32ops { readonly _F32: undefined;
  // Constants
  constv(v :Float32) { return new instr_imm1(0x43, this, v) as any as Op<F32> }
  const(v :float32) :Op<F32> { return this.constv(float32(v)) }

  // Memory
  load(mi :MemImm, addr :Op<Int>) { return memload(0x2a, this, mi, addr) }
  store(mi :MemImm, addr :Op<Int>, v :Op<F32>) { return memstore(0x38, mi, addr, v) }
  addrIsAligned(mi :MemImm, addr :number) { return addrIsAligned(2, mi[0].v, mi[1].v, addr) }

  // Comparison
  eq(a :Op<F32>, b :Op<F32>) { return new instr_pre(0x5b,this,[a,b]) as any as Op<I32> }
  ne(a :Op<F32>, b :Op<F32>) { return new instr_pre(0x5c,this,[a,b]) as any as Op<I32> }
  lt(a :Op<F32>, b :Op<F32>) { return new instr_pre(0x5d,this,[a,b]) as any as Op<I32> }
  gt(a :Op<F32>, b :Op<F32>) { return new instr_pre(0x5e,this,[a,b]) as any as Op<I32> }
  le(a :Op<F32>, b :Op<F32>) { return new instr_pre(0x5f,this,[a,b]) as any as Op<I32> }
  ge(a :Op<F32>, b :Op<F32>) { return new instr_pre(0x60,this,[a,b]) as any as Op<I32> }

  // Numeric
  abs     (a :Op<F32>) { return new instr_pre1(0x8b,this,a) as any as Op<F32> }
  neg     (a :Op<F32>) { return new instr_pre1(0x8c,this,a) as any as Op<F32> }
  ceil    (a :Op<F32>) { return new instr_pre1(0x8d,this,a) as any as Op<F32> }
  floor   (a :Op<F32>) { return new instr_pre1(0x8e,this,a) as any as Op<F32> }
  trunc   (a :Op<F32>) { return new instr_pre1(0x8f,this,a) as any as Op<F32> }
  nearest (a :Op<F32>) { return new instr_pre1(0x90,this,a) as any as Op<F32> }
  sqrt    (a :Op<F32>) { return new instr_pre1(0x91,this,a) as any as Op<F32> }
  add     (a :Op<F32>, b :Op<F32>) { return new instr_pre(0x92,this,[a,b]) as any as Op<F32> }
  sub     (a :Op<F32>, b :Op<F32>) { return new instr_pre(0x93,this,[a,b]) as any as Op<F32> }
  mul     (a :Op<F32>, b :Op<F32>) { return new instr_pre(0x94,this,[a,b]) as any as Op<F32> }
  div     (a :Op<F32>, b :Op<F32>) { return new instr_pre(0x95,this,[a,b]) as any as Op<F32> }
  min     (a :Op<F32>, b :Op<F32>) { return new instr_pre(0x96,this,[a,b]) as any as Op<F32> }
  max     (a :Op<F32>, b :Op<F32>) { return new instr_pre(0x97,this,[a,b]) as any as Op<F32> }
  copysign(a :Op<F32>, b :Op<F32>) { return new instr_pre(0x98,this,[a,b]) as any as Op<F32> }

  // Conversion
  convert_s_i32  (a :Op<I32>) { return new instr_pre1(0xb2,this,a) as any as Op<F32> }
  convert_u_i32  (a :Op<I32>) { return new instr_pre1(0xb3,this,a) as any as Op<F32> }
  convert_s_i64  (a :Op<I64>) { return new instr_pre1(0xb4,this,a) as any as Op<F32> }
  convert_u_i64  (a :Op<I64>) { return new instr_pre1(0xb5,this,a) as any as Op<F32> }
  demote_f64     (a :Op<F64>) { return new instr_pre1(0xb6,this,a) as any as Op<F32> }
  reinterpret_i32(a :Op<I32>) { return new instr_pre1(0xbe,this,a) as any as Op<F32> }
}

class f64ops extends type_atom implements F64ops { readonly _F64: undefined;
  // Constants
  constv(v :Float64) { return new instr_imm1(0x44, this, v) as any as Op<F64> }
  const(v :float64) :Op<F64> { return this.constv(float64(v)) }

  // Memory
  load(mi :MemImm, addr :Op<Int>) { return memload(0x2b, this, mi, addr) }
  store(mi :MemImm, addr :Op<Int>, v :Op<F64>) { return memstore(0x39, mi, addr, v) }
  addrIsAligned(mi :MemImm, addr :number) { return addrIsAligned(3, mi[0].v, mi[1].v, addr) }

  // Comparison
  eq(a :Op<F64>, b :Op<F64>) { return new instr_pre(0x61,this,[a,b]) as any as Op<I32> }
  ne(a :Op<F64>, b :Op<F64>) { return new instr_pre(0x62,this,[a,b]) as any as Op<I32> }
  lt(a :Op<F64>, b :Op<F64>) { return new instr_pre(0x63,this,[a,b]) as any as Op<I32> }
  gt(a :Op<F64>, b :Op<F64>) { return new instr_pre(0x64,this,[a,b]) as any as Op<I32> }
  le(a :Op<F64>, b :Op<F64>) { return new instr_pre(0x65,this,[a,b]) as any as Op<I32> }
  ge(a :Op<F64>, b :Op<F64>) { return new instr_pre(0x66,this,[a,b]) as any as Op<I32> }

  // Numeric
  abs     (a :Op<F64>) { return new instr_pre1(0x99,this,a) as any as Op<F64> }
  neg     (a :Op<F64>) { return new instr_pre1(0x9a,this,a) as any as Op<F64> }
  ceil    (a :Op<F64>) { return new instr_pre1(0x9b,this,a) as any as Op<F64> }
  floor   (a :Op<F64>) { return new instr_pre1(0x9c,this,a) as any as Op<F64> }
  trunc   (a :Op<F64>) { return new instr_pre1(0x9d,this,a) as any as Op<F64> }
  nearest (a :Op<F64>) { return new instr_pre1(0x9e,this,a) as any as Op<F64> }
  sqrt    (a :Op<F64>) { return new instr_pre1(0x9f,this,a) as any as Op<F64> }
  add     (a :Op<F64>, b :Op<F64>) { return new instr_pre(0xa0,this,[a,b]) as any as Op<F64> }
  sub     (a :Op<F64>, b :Op<F64>) { return new instr_pre(0xa1,this,[a,b]) as any as Op<F64> }
  mul     (a :Op<F64>, b :Op<F64>) { return new instr_pre(0xa2,this,[a,b]) as any as Op<F64> }
  div     (a :Op<F64>, b :Op<F64>) { return new instr_pre(0xa3,this,[a,b]) as any as Op<F64> }
  min     (a :Op<F64>, b :Op<F64>) { return new instr_pre(0xa4,this,[a,b]) as any as Op<F64> }
  max     (a :Op<F64>, b :Op<F64>) { return new instr_pre(0xa5,this,[a,b]) as any as Op<F64> }
  copysign(a :Op<F64>, b :Op<F64>) { return new instr_pre(0xa6,this,[a,b]) as any as Op<F64> }

  // Conversions
  convert_s_i32  (a :Op<I32>) { return new instr_pre1(0xb7,this,a) as any as Op<F64> }
  convert_u_i32  (a :Op<I32>) { return new instr_pre1(0xb8,this,a) as any as Op<F64> }
  convert_s_i64  (a :Op<I64>) { return new instr_pre1(0xb9,this,a) as any as Op<F64> }
  convert_u_i64  (a :Op<I64>) { return new instr_pre1(0xba,this,a) as any as Op<F64> }
  promote_f32    (a :Op<F32>) { return new instr_pre1(0xbb,this,a) as any as Op<F64> }
  reinterpret_i64(a :Op<I64>) { return new instr_pre1(0xbf,this,a) as any as Op<F64> }
}

const magic = uint32(0x6d736100)
const latestVersion = uint32(0x1)
const end = new instr_atom(0x0b, Void) as any as Op<Void>
const elseOp = new instr_atom(0x05, Void) as any as Op<Void>

function if_<R extends AnyResult>(r :R, cond :Op<I32>, then_ :AnyOp[], else_? :AnyOp[]) {
  assert(r === then_[then_.length-1].r)
  assert(!else_ || else_.length == 0 || r === else_[else_.length-1].r)
  return new instr_pre_imm_post(0x04, r,
    [cond], // pre
    [r],    // imm
    // post:
    else_ ? [...then_, elseOp, ...else_, end] :
            [...then_, end]
  ) as any as Op<R>
}

const return_ = <R extends Result>(value :Op<R>) =>
  new instr_pre1(0x0f, value.r, value) as any as Op<R>

export const t = T

export const c = {
  uint8,
  uint32,
  float32,
  float64,
  varuint1,
  varuint7,
  varuint32,
  varint7,
  varint32,
  varint64,

  any_func: AnyFunc,
  func: Func,
  empty_block: EmptyBlock,
  void: Void, void_: Void,

  external_kind: {
    function: external_kind_function, // Function import or definition
    table:    external_kind_table,    // Table import or definition
    memory:   external_kind_memory,   // Memory import or definition
    global:   external_kind_global,   // Global import or definition
  },

  data(buf: ArrayLike<uint8>) {
    return new bytes_atom(T.data, buf) as any as Data
  },

  str,

  str_ascii(text: string) {
    const bytes :uint8[] = []
    for (let i = 0, L = text.length; i != L; ++i) {
      bytes[i] = 0xff & text.charCodeAt(i);
    }
    return str(bytes)
  },

  str_utf8: (text: string) =>
    str(utf8.encode(text)),

  // If you are targeting a pre-MVP version, provide the desired version number (e.g. `0xd`).
  // If not provided or falsy, the latest stable version is used.
  module(sections :Section[], version? :uint32) {
    const v = version ? uint32(version) : latestVersion
    return new cell<Section>(T.module,
      [magic, v, ...sections] as Section[]) as any as Module
  },

  custom_section: (name :Str, payload :N[]) =>
    section(sect_id_custom, name, payload) as any as CustomSection,

  type_section: (types: FuncType[]) =>
    section(sect_id_type, varuint32(types.length), types) as any as TypeSection,

  import_section: (entries: ImportEntry[]) =>
    section(sect_id_import, varuint32(entries.length), entries) as any as ImportSection,

  function_section: (types: VarUint32[]) =>
    section(sect_id_function, varuint32(types.length), types) as any as FunctionSection,

  table_section: (types: TableType[]) =>
    section(sect_id_table, varuint32(types.length), types) as any as TableSection,

  memory_section: (limits: ResizableLimits[]) =>
    section(sect_id_memory, varuint32(limits.length), limits) as any as MemorySection,

  global_section: (globals: GlobalVariable[]) =>
    section(sect_id_global, varuint32(globals.length), globals) as any as GlobalSection,

  export_section: (exports: ExportEntry[]) =>
    section(sect_id_export, varuint32(exports.length), exports) as any as ExportSection,

  start_section: (funcIndex: VarUint32) =>
    section(sect_id_start, funcIndex, []) as any as StartSection,

  element_section: (entries: ElemSegment[]) =>
    section(sect_id_element, varuint32(entries.length), entries) as any as ElementSection,

  code_section: (bodies: FunctionBody[]) =>
    section(sect_id_code, varuint32(bodies.length), bodies) as any as CodeSection,

  data_section: (entries: DataSegment[]) =>
    section(sect_id_data, varuint32(entries.length), entries) as any as DataSection,


  function_import_entry: (module :Str, field :Str, typeIndex: VarUint32) =>
    new cell<N>(T.import_entry, [
      module, field, external_kind_function, typeIndex
    ]) as any as ImportEntry,

  table_import_entry: (module :Str, field :Str, type: TableType) =>
    new cell<N>(T.import_entry,
      [module, field, external_kind_table, type]) as any as ImportEntry,

  memory_import_entry: (module :Str, field :Str, limits: ResizableLimits) =>
    new cell<N>(T.import_entry,
      [module, field, external_kind_memory, limits]) as any as ImportEntry,

  global_import_entry: (module :Str, field :Str, type: GlobalType) =>
    new cell<N>(T.import_entry,
      [module, field, external_kind_global, type]) as any as ImportEntry,


  export_entry: (field :Str, kind :ExternalKind, index :VarUint32) =>
    new cell<N>(T.export_entry, [field, kind, index]) as any as ExportEntry,


  elem_segment: (index :VarUint32, offset :InitExpr, funcIndex :VarUint32[]) =>
    new cell<N>(T.elem_segment, [
      index, offset, varuint32(funcIndex.length), ...funcIndex
    ]) as any as ElemSegment,

  data_segment: (index :VarUint32, offset :InitExpr, data :Data) =>
    new cell<N>(T.data_segment, [index, offset, varuint32(data.z), data]) as any as DataSegment,


  func_type(paramTypes :ValueType[], returnType? :ValueType|null) {
    const paramLen = varuint32(paramTypes.length)
    return new cell(T.func_type,
      returnType ? [Func, paramLen, ...paramTypes, varuint1_1, returnType]
                 : [Func, paramLen, ...paramTypes, varuint1_0]) as any as FuncType
  },

  table_type(type :ElemType, limits :ResizableLimits) {
    assert(type.v == AnyFunc.v) // WASM MVP limitation
    return new cell<N>(T.table_type, [type, limits]) as any as TableType
  },

  global_type: (contentType :ValueType, mutable? :boolean) =>
    new cell<N>(T.global_type, [
      contentType, mutable ? varuint1_1 : varuint1_0
    ]) as any as GlobalType,


  // expressed in number of memory pages (not bytes; pagesize=64KiB)
  resizable_limits: (initial :VarUint32, maximum? :VarUint32) =>
    new cell<N>(T.resizable_limits, maximum ?
      [varuint1_1, initial, maximum] : [varuint1_0, initial]
    ) as any as ResizableLimits,

  global_variable: (type :GlobalType, init :InitExpr) =>
    new cell<N>(T.global_variable, [type, init]) as any as GlobalVariable,

  init_expr: (expr :N[]) =>
    new cell<N>(T.init_expr, [...expr, end]) as any as InitExpr,

  function_body(locals :LocalEntry[], code :N[]) {
    const localCount = varuint32(locals.length)
    return new cell<N>(T.function_body, [
      varuint32(localCount.z + sumz(locals) + sumz(code) + 1), // body_size
      localCount, ...locals, ...code, end
    ]) as any as FunctionBody
  },

  local_entry: (count :VarUint32, type :ValueType) =>
    new cell<N>(T.local_entry, [count, type]) as any as LocalEntry,

  // Semantics of the WebAssembly stack machine:
  // - Control instructions pop their argument value(s) off the stack, may change
  //   the program counter, and push result value(s) onto the stack.
  // - Simple instructions pop their argument value(s) from the stack, apply an
  //   operator to the values, and then push the result value(s) onto the stack,
  //   followed by an implicit advancement of the program counter.

  unreachable: new instr_atom(0x00, Void) as any as Op<Void>,
  nop:         new instr_atom(0x01, Void) as any as Op<Void>,

  // begin a block which can also form CF loops
  block<R extends AnyResult>(r :R, body :AnyOp[]) {
    assert(r === body[body.length-1].r)
    return new instr_imm1_post(0x02, r, r as N, [...body, end]) as any as Op<R>
  },

  void_block(body :AnyOp[]) {
    assert(body.length == 0 || Void === body[body.length-1].r)
    return new instr_imm1_post(0x02, Void, EmptyBlock, [...body, end]) as any as Op<Void>
  },


  // begin a block which can also form CF loops
  loop<R extends AnyResult>(r :R, body :AnyOp[]) {
    assert(r === body[body.length-1].r)
    return new instr_imm1_post(0x03, r, r as N, [...body, end]) as any as Op<R>
  },

  void_loop(body :AnyOp[]) {
    assert(body.length == 0 || Void === body[body.length-1].r)
    return new instr_imm1_post(0x03, Void, EmptyBlock, [...body, end]) as any as Op<Void>
  },

  if: if_, if_,
  end: end,

  // Branch to a given label (relative depth) in an enclosing construct.
  // Note:
  // - "branching" to a block correspond to a "break" statement
  // - "branching" to a loop correspond to a "continue" statement
  br: (relDepth :uint32) =>
    new instr_imm1(0x0c, Void, varuint32(relDepth)) as any as Op<Void>,

  // Conditionall branch to a given label in an enclosing construct.
  // When condition is false, this is equivalent to a "Nop" operation.
  // When condition is true, this is equivalent to a "Br" operation.
  br_if: (relDepth :uint32, cond :Op<I32>) =>
    new instr_pre_imm(0x0d, Void, [cond], [varuint32(relDepth)]) as any as Op<Void>,

  // Jump table which jumps to a label in an enclosing construct.
  // A br_table consists of a zero-based array of labels, a default label,
  // and an index operand. A br_table jumps to the label indexed in the
  // array or the default label if the index is out of bounds.
  br_table: (targetLabels :VarUint32[], defaultLabel :VarUint32, index :AnyOp) =>
    new instr_pre_imm(0x0e, Void,
      [index],
      [varuint32(targetLabels.length), ...targetLabels, defaultLabel]
    ) as any as Op<Void>,

  // return zero or one value from this function
  return: return_, return_,
  return_void: new instr_atom(0x0f, Void) as Op<Void>,

  // Calling
  call<R extends Result>(r :R, funcIndex: VarUint32, args :AnyOp[]) {
    return new instr_pre_imm(0x10, r, args, [funcIndex]) as any as Op<R>
  },

  call_indirect<R extends Result>(r :R, funcIndex: VarUint32, args :AnyOp[]) {
    return new instr_pre_imm(0x11, r, args, [funcIndex, varuint1_0]) as any as Op<R>
  },

  // drop discards the value of its operand
  // R should be the value on the stack "under" the operand. E.g. with a stack:
  //   I32  top
  //   F64  ...
  //   F32  bottom
  // drop
  //   F64  top
  //   F32  bottom
  // i.e. R=F64
  drop<R extends AnyResult>(r :R, n :Op<Result>) {
    return new instr_pre1(0x1a, r, n) as any as Op<R>
  },

  // select one of two values based on condition
  select<R extends Result>(cond :Op<I32>, trueRes :Op<R>, falseRes :Op<R>) {
    assert(trueRes.r === falseRes.r)
    return new instr_pre(0x1b, trueRes.r, [trueRes, falseRes, cond]) as any as Op<R>
  },

  // Variable access
  get_local<R extends Result>(r :R, localIndex :uint32) {
    return new instr_imm1(0x20, r, varuint32(localIndex)) as any as Op<R>
  },

  set_local(localIndex :uint32, expr :Op<Result>) {
    return new instr_pre_imm(0x21, Void, [expr], [varuint32(localIndex)]) as any as Op<Void>
  },

  tee_local<R extends Result>(localIndex :uint32, expr :Op<R>) {
    return new instr_pre_imm(0x22, expr.r, [expr], [varuint32(localIndex)]) as any as Op<R>
  },

  get_global<R extends Result>(r :R, globalIndex :uint32) {
    return new instr_imm1(0x23, r, varuint32(globalIndex)) as any as Op<R>
  },

  set_global(globalIndex :uint32, expr :Op<Result>) {
    return new instr_pre_imm(0x24, Void, [expr], [varuint32(globalIndex)]) as any as Op<Void>
  },

  // Memory
  current_memory() { // query the size of memory (number of pages)
    return new instr_imm1(0x3f, c.i32, varuint1_0) as any as Op<Int>
  },

  // Grow the size of memory by `delta` memory pages.
  // Returns the previous memory size in units of pages or -1 on failure.
  grow_memory(delta :Op<Int>) {
    assert(delta.v >= 0)
    return new instr_pre_imm(0x40, c.i32, [delta], [varuint1_0]) as any as Op<Int>
  },

  // MemImm: Alignment          Offset
  align8:  [ varUint32Cache[0], varUint32Cache[0] ] as [VarUint32,Int],
  align16: [ varUint32Cache[1], varUint32Cache[0] ] as [VarUint32,Int],
  align32: [ varUint32Cache[2], varUint32Cache[0] ] as [VarUint32,Int],
  align64: [ varUint32Cache[3], varUint32Cache[0] ] as [VarUint32,Int],

  i32: new i32ops(-0x01, 0x7f) as I32ops,
  i64: new i64ops(-0x02, 0x7e) as I64ops,
  f32: new f32ops(-0x03, 0x7d) as F32ops,
  f64: new f64ops(-0x04, 0x7c) as F64ops,
}


export interface FunctionBodyInfo {
  index  :number // module function index
  locals :N[]
  code   :AnyOp[]
}

// access helpers
export const get = {
  sections(m :Module) :Section[] {
    return m.v.slice(2) // 0=magic, 1=version, 2...=Section[]
  },

  section(m :Module, id :VarUint7|uint7) :Section {
    let ido = (typeof id != 'object') ? varuint7(id as uint7) : (id as VarUint7)
    for (let i = 2; i < m.v.length; ++i) {
      let section = m.v[i]
      if (section.v[0] === ido) {
        return section
      }
    }
  },

  function_bodies(s :CodeSection) :Iterable<FunctionBodyInfo> {
    return {
      [Symbol.iterator](startIndex? :number) {
        let index = 3 + (startIndex || 0)
        return {
          next() {
            const funcBody = s.v[index]
            if (!funcBody) {
              return {done: true, value: null}
            }
            let localCount = funcBody.v[1]
            return {
              done: false,
              value: {
                index: index++,
                locals: funcBody.v.slice(2, localCount.v + 2),
                code: funcBody.v.slice(2 + localCount.v, funcBody.v.length - 1) as AnyOp[]
                  //  -1 to skip terminating `end`
              }
            }
          }
        }
      }
    }
  },

}