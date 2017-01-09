// Version 13
import {utf8} from './utf8'
import './types'

function byte(value :uint8) {
  return new V(t.uint8, 1, value, w => w.writeUint8(value))
}

function mem_op_fn(opcode :byte) {
  return function mem_op_imm(flags :uint32, offset: uint32) {
    // flags: a bitfield which currently contains the alignment in the
    //        least significant bits, encoded as log2(alignment)
    return new Instr(opcode, [c.varint32(flags), c.varint32(offset)])
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
    //   - the four constant operators; and
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

  unreachable: new Instr(op.unreachable),
  nop:         new Instr(op.nop),
  block(block_type :V) { return new Instr(op.block, [block_type]) },
    // begin a sequence of expressions, yielding 0 or 1 values
  loop(block_type :V) { return new Instr(op.loop, [block_type]) },
    // begin a block which can also form control flow loops
  if(block_type :V) { return new Instr(op.if, [block_type]) },
    // begin if expression
  else: new Instr(op.else), // begin else expression of if
  end:  new Instr(op.end),  // end a block, loop, or if
  br(relative_depth :uint32) {
    return new Instr(op.br, [c.varuint32(relative_depth)]) },
    // break that targets an outer nested block
  br_if(relative_depth :uint32) {
    return new Instr(op.br_if, [c.varuint32(relative_depth)]) },
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
    return new Instr(op.br_table, [
      c.varuint32(target_table.length),
      ...target_table.map(c.varuint32),
      c.varuint32(default_target) ])
  },

  return(what? :N) { return new Instr(op.return, what ? [what] : null) },

  // Call operators
  // TODO

  // Parametric operators
  drop:   new Instr(op.drop), // ignore value
  select: new Instr(op.select), // select one of two values based on condition

  // Variable access
  get_local(local_index :uint32) {
    return new Instr(op.get_local, [c.varuint32(local_index)]) },
  // TODO

  // Type-specific operators
  i32: {
    const(value :int32) { return new Instr(op.i32.const, [c.varint32(value)]) },
    
    // Memory; all functions have signature: (flags :uint32, offset: uint32)
    load: mem_op_fn(op.i32.load),

    // Numeric
    mul(...operands :N[]) { return new Instr(op.i32.mul, operands) },
  },
}

default export c
