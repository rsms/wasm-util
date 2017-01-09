import {utf8} from './utf8'

declare function require(ref:string):any;
require('source-map-support').install()

type AssertFunc = (cond:any)=>void;
const _assert :AssertFunc = require('assert')
function assert(cond: any) :boolean {
  _assert(!!cond)
  return true // e.g. value = assert(value < 10) && value
}

type int64  = number
type int32  = number
type int7   = number
type uint   = number
type uint1  = number
type uint7  = number
type uint8  = number
type uint16 = number
type uint32 = number

function sumf<A>(a :Array<A>, f :(a:A)=>number, initial? :number) :number {
  return a.reduce((s, v) => s + f(v), initial || 0)
}

function symname(y :Symbol) {
  const s = y.toString()
  return s.length > 8 ? s.substr(7,s.length-8) : 'Symbol(?)';
}

type OpCode = uint8

// Instruction opcodes.
// Semantics:
// - Control instructions pop their argument value(s) off the stack, may change
//   the program counter, and push result value(s) onto the stack.
// - Simple instructions pop their argument value(s) from the stack, apply an
//   operator to the values, and then push the result value(s) onto the stack,
//   followed by an implicit advancement of the program counter.
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

interface IntOps {
  // Constants
  readonly const :OpCode // [value :VarIntN] a constant value

  // Memory
  readonly load     :OpCode // [memory_immediate] load from memory
  readonly load16_s :OpCode // [memory_immediate] load from memory
  readonly load16_u :OpCode // [memory_immediate] load from memory
  readonly load8_s  :OpCode // [memory_immediate] load from memory
  readonly load8_u  :OpCode // [memory_immediate] load from memory
  readonly store    :OpCode // [memory_immediate] store to memory
  readonly store16  :OpCode // [memory_immediate] store to memory
  readonly store8   :OpCode // [memory_immediate] store to memory

  // Comparison
  readonly eqz  :OpCode // compare equal to zero (return 1 if operand is zero, else 0)
  readonly eq   :OpCode // sign-agnostic compare equal
  readonly ne   :OpCode // sign-agnostic compare unequal
  readonly lt_s :OpCode // signed less than
  readonly lt_u :OpCode // unsigned less than
  readonly gt_s :OpCode // signed greater than
  readonly gt_u :OpCode // unsigned greater than
  readonly le_s :OpCode // signed less than or equal
  readonly le_u :OpCode // unsigned less than or equal
  readonly ge_s :OpCode // signed greater than or equal
  readonly ge_u :OpCode // unsigned greater than or equal

  // Numeric
  readonly clz :OpCode
    // sign-agnostic count leading zero bits (All zero bits are considered
    // leading if the value is zero)
  readonly ctz :OpCode
    // sign-agnostic count trailing zero bits (All zero bits are considered
    // trailing if the value is zero)
  readonly popcnt :OpCode // sign-agnostic count number of one bits
  readonly add    :OpCode // sign-agnostic addition
  readonly sub    :OpCode // sign-agnostic subtraction
  readonly mul    :OpCode // sign-agnostic multiplication (lower N-bits)
  readonly div_s  :OpCode // signed division (result is truncated toward zero)
  readonly div_u  :OpCode // unsigned division (result is floored)
  readonly rem_s  :OpCode // signed remainder (result has the sign of the dividend)
  readonly rem_u  :OpCode // unsigned remainder
  readonly and    :OpCode // sign-agnostic bitwise and
  readonly or     :OpCode // sign-agnostic bitwise inclusive or
  readonly xor    :OpCode // sign-agnostic bitwise exclusive or
  readonly shl    :OpCode // sign-agnostic shift left
  readonly shr_s  :OpCode // sign-replicating (arithmetic) shift right
  readonly shr_u  :OpCode // zero-replicating (logical) shift right
  readonly rotl   :OpCode // sign-agnostic rotate left
  readonly rotr   :OpCode // sign-agnostic rotate right

  // Conversions
  readonly trunc_s_f32 :OpCode // truncate a 32-bit float to a signed N-bit integer
  readonly trunc_u_f32 :OpCode // truncate a 32-bit float to an unsigned N-bit integer
  readonly trunc_s_f64 :OpCode // truncate a 64-bit float to a signed N-bit integer
  readonly trunc_u_f64 :OpCode // truncate a 64-bit float to an unsigned N-bit integer
}

interface IntOps32 extends IntOps {
  readonly wrap_i64        :OpCode // wrap a 64-bit integer to a 32-bit integer
  readonly reinterpret_f32 :OpCode // r.i. the bits of a 32-bit float as a 32-bit integer
}

interface IntOps64 extends IntOps {
  readonly extend_s_i32    :OpCode // extend a signed 32-bit integer to a 64-bit integer
  readonly extend_u_i32    :OpCode // extend an unsigned 32-bit integer to a 64-bit integer
  readonly reinterpret_f64 :OpCode // r.i. the bits of a 64-bit float as a 64-bit integer
}


interface Emitter {
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
  writeBytes(v :ArrayLike<uint8>) :Emitter
  writeAll(objs :Emittable[]) :Emitter
  write(obj :Emittable) :Emitter
}

interface Emittable {
  emit(ctx :Emitter) :Emitter
}

// Node interface
interface N extends Emittable {
  readonly byteSize :uint32
}

interface Uint8 extends N { readonly value :uint8 }
interface Uint16 extends N { readonly value :uint16 }
interface Uint32 extends N { readonly value :uint32 }
interface VarUint32 extends N { readonly value :uint32 }
interface VarUint7 extends N { readonly value :uint7 }
interface VarUint1 extends N { readonly value :uint1 }
interface VarInt32 extends N { readonly value :int32 }
interface VarInt7 extends N { readonly value :int7 }

type Bytes = ArrayLike<uint8> // e.g. Uint8Array or uint8[]

interface PrefixStr extends N {
  readonly length :VarUint32        // length in bytes
  readonly bytes  :ArrayLike<uint8> // data
}

type Type = ValueType | AnyFunc | Func | EmptyBlock
          | FuncType
          | GlobalType
          | TableType
          | MemoryType

type ValueType = I32 | I64 | F32 | F64
type BlockType = ValueType | EmptyBlock // A varint7 indicating a block signature
type ElemType  = AnyFunc // types of elements in a table

type langTypeBase = {readonly byteSize:1; emit(ctx:Emitter):Emitter}
type I32        = langTypeBase & { readonly value: -0x01; readonly b:0x7f }
type I64        = langTypeBase & { readonly value: -0x02; readonly b:0x7e }
type F32        = langTypeBase & { readonly value: -0x03; readonly b:0x7d }
type F64        = langTypeBase & { readonly value: -0x04; readonly b:0x7c }
type AnyFunc    = langTypeBase & { readonly value: -0x10; readonly b:0x70 }
type Func       = langTypeBase & { readonly value: -0x20; readonly b:0x60 }
type EmptyBlock = langTypeBase & { readonly value: -0x40; readonly b:0x40 }

interface FuncType extends N {
  readonly opcode       :Func
  readonly param_count  :VarUint32   // the number of parameters to the function
  readonly param_types  :ValueType[] // the parameter types of the function
  readonly return_count :VarUint1    // the number of results from the function
  readonly return_type? :ValueType   // the result type of the function (if return_count is 1)
}

interface GlobalType extends N { // a global variable
  content_type :ValueType // type of the value
  mutability   :VarUint1  // 0 if immutable, 1 if mutable
}

interface TableType extends N {
  element_type :ElemType        // the type of elements
  limits       :ResizableLimits
}

interface MemoryType extends N {
  limits :ResizableLimits
}

// ExternalKind indicates the kind of definition being imported or defined
type ExternalKind = ExternalKindFunction // a Function import or definition
                  | ExternalKindTable    // a Table import or definition
                  | ExternalKindMemory   // a Memory import or definition
                  | ExternalKindGlobal   // a Global import or definition
type externalKindBase = {readonly byteSize:1; emit(ctx:Emitter):Emitter}
type ExternalKindFunction = externalKindBase & {readonly value:0}
type ExternalKindTable    = externalKindBase & {readonly value:1}
type ExternalKindMemory   = externalKindBase & {readonly value:2}
type ExternalKindGlobal   = externalKindBase & {readonly value:3}

interface ResizableLimits extends N {
  // A packed tuple that describes the limits of a table or memory:
  flags    :VarUint1  // 1 if the maximum field is present, 0 otherwise
  initial  :VarUint32 // initial length (in units of table elements or wasm pages)
  maximum? :VarUint32 // only present if specified by flags
}

interface Section extends N {
  readonly id          :VarUint7
  readonly payload_len :VarUint32 // size of this section in bytes
}

interface CustomSection extends Section {
  readonly name         :PrefixStr // section name string
  readonly payload_data :Bytes     // content of this section
    // of length payload_len - sizeof(name) - sizeof(name_len)
}

interface TypeSection extends Section {
  readonly types: FuncType[]
}

interface ImportSection extends Section {
  readonly entries: ImportEntry[]
}

interface ImportEntry extends N {
  module :PrefixStr    // module name
  field  :PrefixStr    // field name
  kind   :ExternalKind // the kind of definition being imported
}
  interface FuncImportEntry extends ImportEntry {
    type :VarUint32 // type index of the function signature
  }
  interface TableImportEntry extends ImportEntry { type :TableType }
  interface MemoryImportEntry extends ImportEntry { type :MemoryType }
  interface GlobalImportEntry extends ImportEntry { type :GlobalType }

interface Module extends N {
  readonly version  :uint32
  readonly sections :Section[]
}

type i32 = InstrRes<I32>
type i64 = InstrRes<I64>
type f32 = InstrRes<F32>
type f64 = InstrRes<F64>

interface Instr extends N { readonly c :OpCode }
interface InstrN<T extends N> extends Instr { readonly pre :T[] }
interface InstrImm<T extends N> extends Instr { readonly imm :T[] }
// interface InstrNImm<TN extends N, TI extends N> extends InstrN<TN>, InstrImm<TI> {}
interface InstrRes<R extends ValueType> extends Instr { readonly r :R }
interface InstrResN<R extends ValueType, T extends N> extends InstrRes<R>, InstrN<T> {}
interface InstrResImm<R extends ValueType, T extends N> extends InstrRes<R>, InstrImm<T> {}
// interface InstrResNImm<R extends ValueType, TN extends N, TI extends N>
//   extends InstrRes<R>, InstrN<TN>, InstrImm<TI> {}

interface InstrBlock<T extends N> extends Instr { readonly exprs :T[] }
interface InstrResBlock<R extends ValueType, T extends N> extends InstrBlock<T> {
  readonly r :R
}

interface InitExpr extends InstrN<Instr> {}

//——————————————————————————————————————————————————————————————————————————————

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

const Uint32 = (value :uint32) => ({
  byteSize: 4,
  value: assert(value <= 0xffffffff) && value,
  emit(e:Emitter) { return e.writeU32(this.value) }
})

let uint8Cache :Uint8[] = []

const Uint8 = function(value :uint8) {
  const c = uint8Cache[value]
  if (c) { return c as Uint8 }
  return {
    byteSize: 1, value: value,
    emit(e:Emitter) { return e.writeU8(this.value) }
  }
}

uint8Cache = maprange(0,16, v => Uint8(v as uint8))
const zeroByte = uint8Cache[0]
const oneByte = uint8Cache[1]

const VarUint1 = (v :any) => (v ? oneByte as VarUint1 : zeroByte as VarUint1)

const VarUint7 = function(value :uint7) {
  const c = uint8Cache[value]
  if (c) { return c as VarUint7 }
  return {
    byteSize: 1,
    value: assert(value >= 0 && value <= 128) && value,
    emit(e:Emitter) { return e.writeU8(this.value) }
  }
}

const VarUint32 = function(value :uint32) {
  const c = uint8Cache[value]
  if (c) { return c as VarUint32 }
  assert(value >= 0 && value <= 0xffffffff)
  const bytes :uint8[] = []
  let v = value
  do {
    let b = v & 0x7f
    v >>= 7
    if (v != 0) { b |= 0x80 }
    bytes.push(b)
  } while (v != 0)
  return {
    byteSize: bytes.length,
    value: v,
    emit(e:Emitter) { return e.writeBytes(bytes) }
  }
}

const VarInt32 = function(value :int32) :VarInt32 {
  assert(value >= -0x80000000 && value <= 0x7fffffff)
  const bytes :uint8[] = []
  let v = value
  let done = false
  do {
    let b = v & 0x7f
    v >>= 7
    done = ((v == 0)  && ((b & 0x40) == 0)) ||
           ((v == -1) && ((b & 0x40) != 0)) ;
    if (!done) { b |= 0x80; }
    bytes.push(b)
  } while (!done)
  return {
    byteSize: bytes.length,
    value,
    emit(e:Emitter) { return e.writeBytes(bytes) }
  }
}

const VarInt7 = function(value :int7) :VarInt7 {
  assert(value >= -128 && value <= 127)
  return VarInt32(value as int32) as VarInt7
}

const PrefixStr = function(bytes: ArrayLike<uint8>) {
  const length = VarUint32(bytes.length)
  return {
    byteSize: length.byteSize + bytes.length,
    length,
    bytes,
    emit(e:Emitter) { return e.write(this.length).writeBytes(bytes) }
  }
}

const PrefixStrUTF8 = (text: string) => PrefixStr(utf8.encode(text))

// Types (VarInt7)
const bemit = function(e:Emitter) { return e.writeU8(this.b) }
const I32        :I32        = { byteSize: 1, value:-0x01, b:0x7f, emit:bemit }
const I64        :I64        = { byteSize: 1, value:-0x02, b:0x7e, emit:bemit }
const F32        :F32        = { byteSize: 1, value:-0x03, b:0x7d, emit:bemit }
const F64        :F64        = { byteSize: 1, value:-0x04, b:0x7c, emit:bemit }
const AnyFunc    :AnyFunc    = { byteSize: 1, value:-0x10, b:0x70, emit:bemit }
const Func       :Func       = { byteSize: 1, value:-0x20, b:0x60, emit:bemit }
const EmptyBlock :EmptyBlock = { byteSize: 1, value:-0x40, b:0x40, emit:bemit }

// ExternalKind
const ExternalKindFunction = zeroByte as ExternalKindFunction
const ExternalKindTable    = oneByte  as ExternalKindTable
const ExternalKindMemory   = Uint8(2) as ExternalKindMemory
const ExternalKindGlobal   = Uint8(3) as ExternalKindGlobal

const FuncType = (param_types :ValueType[], return_type? :ValueType) => {
  const param_count = VarUint32(param_types.length)
  return {
    byteSize:
      2 + // opcode.byteSize + return_count.byteSize
      param_count.byteSize +
      sumf(param_types, t => t.byteSize) +
      (return_type ? return_type.byteSize : 0),
    opcode: Func,
    param_count,
    param_types,
    return_count: VarUint1(return_type),
    return_type,
    emit(e:Emitter) {
      e = e.writeAll([this.opcode, this.param_count]).writeAll(this.param_types)
      return this.return_type ? e.writeU8(1).write(this.return_type) : e.writeU8(0)
    }
  }
}

const Module = (version: uint32, ...sections :Section[]) :Module => ({
  byteSize: 8 + sumf(sections, s => s.byteSize),
  version,
  sections,
  emit(e:Emitter) {
    return e.writeU32(0x6d736100).writeU32(this.version).writeAll(this.sections)
  }
})

interface SectionInfo { id :VarUint7, name :string }
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

const StdSection = (id :uint7, payload: N[]) => {
  const payload_len = VarUint32(payload.length)
  return {
    byteSize: 1 + payload_len.byteSize + sumf(payload, n => n.byteSize),
    id:       sections[id].id,
    payload_len,
    payload,
    emit(e:Emitter) {
      return e.writeAll([this.id, this.payload_len]).writeAll(this.payload)
    }
  }
}

const TypeSection = (...types: FuncType[]) => StdSection(0, types)

const ImportSection = (...entries: ImportEntry[]) => StdSection(1, entries)

const ImportEntry = (kind :ExternalKind, module :PrefixStr, field :PrefixStr, type :N) => ({
  byteSize: module.byteSize + field.byteSize + type.byteSize + kind.byteSize,
  module, field, kind, type,
  emit(e:Emitter) { return e.writeAll([this.module, this.field, this.type]) }
})

const FuncImportEntry = (module :PrefixStr, field :PrefixStr, type :VarUint32) =>
  ImportEntry(ExternalKindFunction, module, field, type)

// Create a simple instruction. Does not affect the stack.
const Instr = (c :OpCode) => ({
  byteSize: 1, c,
  emit(e:Emitter) { return e.writeU8(this.c) } })

// Create an instruction that depends on `pre`ceeding values on the stack.
// Does not push to the stack.
const InstrN = (c :OpCode, pre :N[]) => ({
  byteSize: 1 + sumf(pre, n => n.byteSize), c, pre,
  emit(e:Emitter) { return e.writeAll(this.pre).writeU8(this.c) } })

// Create an instruction with immediate values. Does not affect the stack.
const InstrImm = <T extends N>(c :OpCode, imm :T[]) => ({
  byteSize: 1 + sumf(imm, n => n.byteSize), c, imm,
  emit(e:Emitter) { return e.writeU8(this.c).writeAll(this.imm) } })

// Like InstrN, but creates an instruction with a result of type R that is pushed to the stack.
const InstrResN = <R extends ValueType, T extends N>(r :R, c :OpCode, pre :T[]) => ({
  byteSize: 1 + sumf(pre, n => n.byteSize), c, r, pre,
  emit(e:Emitter) { return e.writeAll(this.pre).writeU8(this.c) } })

// Create an instruction with immediate values with a result of type R that is pushed to the stack.
const InstrResImm = <R extends ValueType, T extends N>(r :R, c :OpCode, imm :T[]) => ({
  byteSize: 1 + sumf(imm, n => n.byteSize),
  c, r, imm,
  emit(e:Emitter) { return e.writeU8(this.c).writeAll(this.imm) } })

const InstrBlock = <T extends N>(c :OpCode, exprs :T[]) => ({
  byteSize: 3 + sumf(exprs, n => n.byteSize), c, exprs,
  emit(e:Emitter) {
    return e.writeU8(this.c).writeU8(0x40/*EmptyBlock*/)
            .writeAll(this.exprs).writeU8(0x0b/*end*/)
  } })

const InstrResBlock = <R extends ValueType, T extends N>(r :R, c :OpCode, exprs :T[]) => ({
  byteSize: 3 + sumf(exprs, n => n.byteSize), c, exprs, r,
  emit(e:Emitter) {
    return e.writeU8(this.c).writeU8(this.r.b)
            .writeAll(this.exprs).writeU8(0x0b/*end*/)
  } })

const InitExpr = (pre :Instr[]) => InstrN(0x0b/*end*/, pre)
  // Note that get_global in an initializer expression can only refer to
  // immutable imported globals and all uses of init_expr can only appear
  // after the Imports section.


// Instruction constructors
const instr = {
  // Control flow
  unreachable: Instr(0x00), // trap immediately
  nop:         Instr(0x01), // no operation

  // block: begin a sequence of expressions
  block<R extends ValueType>(t :R, expr :N[]) { return InstrResBlock(t, 0x02, expr) },
    // TODO: assert that expr causes one value of type R to be placed on top of the stack
  void_block(expr :N[]) { return InstrBlock(0x02, expr) },
    // TODO: assert that expr leaves stack as same depth as on entry

  // begin a block which can also form CF loops
  loop<R extends ValueType>(t :R, expr :N[]) { return InstrResBlock(t, 0x03, expr) },
    // TODO: assert that expr causes one value of type R to be placed on top of the stack
  void_loop(expr :N[]) { return InstrBlock(0x03, expr) },
    // TODO: assert that expr leaves stack as same depth as on entry

  // if:          0x04, // [block_type] begin if expression
  // else:        0x05, // begin else expression of if
  end: Instr(0x0b), // end a block, loop, or if
  // br:          0x0c, // [relative_depth :varuint32]
  //   // break that targets an outer nested block
  // br_if:       0x0d, // [relative_depth :varuint32]
  //   // conditional break that targets an outer nested block
  // br_table:    0x0e, // [br_table_imm] branch table control flow construct
  
  // return zero or one value from this function
  return_void: Instr(op.return),
  return(value? :Instr) :InstrN<Instr> | Instr {
    return value ? InstrN(op.return, [value]) : instr.return_void },

  // Type-specific operators
  i32: {
    const(v :VarInt32) :InstrResImm<I32,VarInt32> {
      return InstrResImm(I32, op.i32.const, [v]) },

    load(flags :VarInt32, offset :VarInt32) :InstrResImm<I32,VarInt32> {
      return InstrResImm(I32, op.i32.load, [flags, offset]) },

    mul(a :i32, b :i32) { return InstrResN(I32, op.i32.mul, [a, b]) },
  },

  f32: {
    reinterpret_i32: (v :i32) :InstrResN<F32,i32> =>
      InstrResN(F32, op.f32.reinterpret_i32, [v]),
  },
}


//——————————————————————————————————————————————————————————————————————————————


let f :f32 = instr.f32.reinterpret_i32(
  instr.i32.mul(
    instr.i32.const(VarInt32(1)),
    instr.i32.load(VarInt32(0), VarInt32(0))
  )
)
let retexpr = instr.return(f)
console.log(require('util').inspect(retexpr, {colors:true, depth:10}))

const mod = Module(0xd,
  TypeSection(
    FuncType([I32], F32),
    FuncType([I32], I32),
    // FuncType([I64, I32], I64)
  ),
  ImportSection(
    FuncImportEntry(PrefixStrUTF8("utf8"), PrefixStrUTF8("encodechar"), VarUint32(1)),
  ),
  // CodeSection(retexpr)
)
console.log(require('util').inspect(mod, {colors:true, depth:10}))

//——————————————————————————————————————————————————————————————————————————————

function BufferedEmitter(size :uint) {
  const buffer = new ArrayBuffer(size as number)
  return {
    buffer,
    byteView: new Uint8Array(buffer),
    view:     new DataView(buffer),
    length:   0,

    writeU8(v :uint8) :Emitter {
      this.view.setUint8(this.length++, v as number)
      return this
    },

    writeU16(v :uint16) :Emitter {
      this.view.setUint16(this.length, v as number, true)
      this.length += 2
      return this
    },

    writeU32(v :uint32) :Emitter {
      this.view.setUint32(this.length, v as number, true)
      this.length += 4
      return this
    },

    writeBytes(bytes :ArrayLike<uint8>) :Emitter {
      this.byteView.set(bytes, this.length)
      this.length += bytes.length
      return this
    },

    writeAll(objs :Emittable[]) :Emitter {
      return objs.reduce((e, s) => s.emit(e), this)
    },

    write(obj :Emittable) :Emitter {
      return obj.emit(this)
    },

    // writeArrayBuffer(b :ArrayBufferView) {
    //   this.byteView.set(new Uint8Array(b.buffer), this.length)
    //   this.length += b.byteLength
    // },

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
    },
  }
}

const emitter = BufferedEmitter(mod.byteSize)
mod.emit(emitter)
emitter.repr(s => console.log(s))
