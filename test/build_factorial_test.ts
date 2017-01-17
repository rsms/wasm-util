import { c, get, sect_id, CodeSection } from '../build/ast'
import { BufferedEmitter } from '../build/emit'
import { specEval } from '../build/eval'
import { printCode } from '../build/lbtext'


declare function require(ref:string):any;
const assert :(cond:any, msg?:any)=>void = require('assert')
const isUnitTest = this.isUnitTest

const {
  type_section, function_section, code_section, export_section,
  func_type, varuint32, function_body, str_ascii, external_kind, export_entry,
  i64, if_, get_local, call
} = c

const mod = c.module([

  type_section([
    func_type([i64], i64), // type index = 0
  ]),
  
  function_section([
    varuint32(0), // function index = 0, using type index 0
  ]),
  
  export_section([
    // exports "factorial" as function at index 0
    export_entry(str_ascii("factorial"), external_kind.function, varuint32(0)),
  ]),

  code_section([
    // body of function at index 0:
    function_body([ /* additional local variables here */ ], [
      if_(i64, // i64 = result type of `if` expression
        i64.eq(get_local(i64, 0), i64.const(0)), // condition
        [ // then
          i64.const(1)
        ], [ // else
          i64.mul(
            get_local(i64, 0),
            call(i64, varuint32(0), [ // 0 = function index
              i64.sub(get_local(i64, 0), i64.const(1))
            ])
          )
        ]
      )
    ])
  ])
])

function factorial(n :number) {
  return (n == 0) ?
    1
  :
    n * factorial(n - 1)
}

function Test() {
  // lbtext print code of each function body
  const codeSection = get.section(mod, sect_id.code) as CodeSection
  for (let funcBody of get.function_bodies(codeSection)) {
    printCode(funcBody.code, s => { console.log(s.replace(/[\r\n]+$/, '')) })
  }

  // codegen
  const emitbuf = new BufferedEmitter(new ArrayBuffer(mod.z))
  mod.emit(emitbuf)

  // eval with spec interpreter
  return specEval(emitbuf.buffer, {
    eval: '(invoke "factorial" (i64.const 8))',
    logErrors: !isUnitTest,
    trace:     !isUnitTest,
  }).then(stdout => {
    if (!isUnitTest) {
      console.log(stdout)
    }
    const expected = '40320 : i64'
    assert(stdout.substr(stdout.length - expected.length) == expected)
  }).catch(err => {
    console.error(err.stack || String(err))
    throw err
  })
}

if (!isUnitTest) {
  Test()
}