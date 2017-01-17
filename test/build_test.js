"use strict";
require('source-map-support').install()

const assert = require('assert')
const ast = require('../build/ast.js')
const {
  uint8, uint32, float32, float64, varuint1, varuint7, varuint32, varint7, varint32, varint64,
  i32, i64, f32, f64, any_func, func, empty_block, void_,
  external_kind,
  data, str, str_ascii, str_utf8,
  custom_section, type_section, import_section, function_section, table_section, memory_section,
  global_section, export_section, start_section, element_section, code_section, data_section,
  function_import_entry, table_import_entry, memory_import_entry, global_import_entry, local_entry,
  export_entry, elem_segment, data_segment,
  func_type, table_type, global_type,
  resizable_limits, global_variable, init_expr, function_body,

  unreachable, nop, end,
  block, void_block, loop, void_loop,
  if_, br, br_if, br_table, return_,
  call, call_indirect,
  drop, select, get_local, set_local, tee_local, get_global, set_global,
  current_memory, grow_memory, align8, align16, align32, align64
} = ast.c;

// const s_crypto = str_ascii("crypto")

const mod = ast.c.module([
  type_section([
    func_type([]),         // 0
    func_type([i32], i32), // 1
    func_type([f32], f32), // 2
    func_type([i32]),      // 3
  ]),

  import_section([
    // Note: MVP only allows one table and one memory
    function_import_entry(str_ascii("spectest"), str_ascii("print"), varuint32(3)),
    // global_import_entry(s_crypto, str_ascii("version"), global_type(i32)),
    // memory_import_entry(s_crypto, str_ascii("data"),
    //   resizable_limits(varuint32(0), varuint32(8))
    // ),
    // table_import_entry(s_crypto, str_ascii("ciphers"),
    //   table_type(AnyFunc, resizable_limits(varuint32(0), varuint32(8)))
    // ),
  ]),

  // Function offsets are: imported... + local...
  function_section([
    varuint32(0), // 1
    varuint32(1), // 2
    varuint32(2), // 3
  ]),

  // Note: MVP only allows a total of 1 table; either a table_section OR a table_import_entry
  table_section([
    // must have enought size for element_section's element segments
    table_type(any_func, resizable_limits(varuint32(1))),
  ]),

  // Note: MVP only allows a total of 1 memory; either a MemorySection OR a MemoryImportEntry
  memory_section([
    resizable_limits(varuint32(1)),
  ]),
  
  global_section([
    global_variable(global_type(i32), init_expr([
      i32.const(0)
    ])),
  ]),

  export_section([
    export_entry(str_ascii("foo"), external_kind.function, varuint32(2)),
    export_entry(str_ascii("bar"), external_kind.function, varuint32(3)),
  ]),

  start_section(varuint32(1)),

  element_section([
    elem_segment(varuint32(0), init_expr([ i32.const(0) ]), [varuint32(0)]),
  ]),

  code_section([

    function_body([], []), // ()

    // func (local0 i32) i32 {
    function_body(
      [local_entry(varuint32(1), i32)], // var local1 i32 = 0
      [
        set_local(1, i32.mul(
          get_local(i32,0),
          i32.const(4)
        )),

        // loop(void_, [
        //   Drop(f32.const(123.4567)),
        //   nop,
        //   nop,
        // ]),

        // if (local1 <= 0) { set local1 = 1 }  // avoid infinite loop below
        if_(void_, i32.le_s(get_local(i32,1), i32.const(0)), [
          set_local(1, i32.const(1))
        ]),

        // do {
        //   set local1 = (local1 * 2)
        // } while (local1 < 9000)
        // loop(void_, [
        //   set_local(1,
        //     i32.mul(get_local(i32,1), i32.const(2)) ), // set local1 = local1 * 2
        //   br_if(0, // continue if
        //     i32.lt_u(get_local(i32,1), i32.const(9000)) ), // local1 < 9000
        // ]),

        // while ( (set local1 = (local1 * 2)) < 9000 ) {
        // }
        loop(void_, [
          br_if(0, // (continue-if (lt (set local1 (* (get local1) 2)) 9000))
            i32.lt_u(
              tee_local(1, i32.mul(get_local(i32,1), i32.const(2))),
              i32.const(9000)
            )
          ),
        ]),

        void_block([
          nop,
        ]),

        // if_(void_,
        //   i32.const(1),                         // cond
        //   [nop, drop<void_>(i32.const(10000)) ], // then
        //   [nop, drop<void_>(i32.const(0)) ],     // else
        // ),

        // Calls spectest.print (i32) void
        call(void_, varuint32(0), [
          i32.const(123)
        ]),

        set_local(1,
          if_(i32,
            i32.lt_s(i32.const(1), i32.const(3)), // condition
            [ // then
              nop,
              i32.wrap_i64(
                i64.mul(
                  i64.extend_s_i32(
                    get_local(i32,1)
                  ),
                  i64.const(-0xffffffff)
                )
              )
            ],[ // else
              nop,
              i32.const(0)
            ]
          )
        ),

        set_local(1,
          select(
            get_local(i32,1),
            i32.mul(
              get_local(i32,1),
              i32.trunc_s_f32(
                call(f32, varuint32(3), [
                  f32.const(2)
                ])
              )
            ),
            i32.const(333)
          )
        ),

        i32.store(align32, i32.const(0),
          get_local(i32,1)
        ),
        drop(void_, grow_memory(i32.const(2))),
        set_local(1, i32.const(0)),
        // Note: no need for `return` if the stack contains N values where N == return_count
        i32.load(align32, i32.const(0)) // load previosuly stored result
        // i32.load(align32, i32.const(4)) // load from data section
        // get_local(i32,1)
        // i32.const(1)
        // current_memory // query number of allocated memory pages
      ]
    ),

    // func (local0 f32) f32 {
    //   return local0 + 123.4567
    // }
    function_body(
      [], // no locals
      [
        f32.add(get_local(f32,0), f32.const(123.4567))
      ]
    ),

  ]), // end code_section

  data_section([
    data_segment(
      varuint32(0),              // linear memory index (==0 in MVP)
      init_expr([i32.const(0)]),  // offset at which to place the data
      data([0,0,0,0,  44,0,0,0])
    ),
  ]),

  custom_section(str_ascii("names"), []),
  custom_section(str_utf8("Äntligen"), [ data([1,2,3]) ]),
])

if (!process.isUnitTest) {
  // repr
  const {repr} = require('../build/repr.js')
  repr(mod, s => { process.stdout.write(s) })
  process.stdout.write('\n')

  // lbtext print code of each function body
  const lbtext = require('../build/lbtext.js')
  const codeSection = ast.get.section(mod, ast.sect_id.code)
  for (let funcBody of ast.get.function_bodies(codeSection)) {
    console.log('———— function ————')
    lbtext.printCode(funcBody.code, s => { process.stdout.write(s) })
  }
}

// emit WASM code
const {BufferedEmitter} = require('../build/emit.js')
const emitter = new BufferedEmitter(new ArrayBuffer(mod.z))
mod.emit(emitter)

if (!process.isUnitTest) {
  const {reprBuffer} = require('../build/repr.js')
  reprBuffer(emitter.buffer, s => { process.stdout.write(s) })
  //
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
}

function Test() {
  // Run generated WASM module binary through the spec interpreter and
  // finally print the text format generated.
  const { specEval } = require('../build/eval.js')
  return specEval(emitter.buffer, {
    eval: '(invoke "foo" (i32.const 3))',
    logErrors: !process.isUnitTest,
    trace: !process.isUnitTest,
  }).then(stdout => {
    if (!process.isUnitTest) {
      console.log(stdout)
    }
    const expected = '1536000 : i32'
    assert.equal(stdout.substr(stdout.length - expected.length), expected)
  }).catch(err => {
    console.error(err.stack || String(err))
    if (!process.isUnitTest) {
      process.exit(1)
    } else {
      throw err
    }
  })
}

if (!process.isUnitTest) {
  Test()
}
