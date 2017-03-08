# wasm-util

Utilities for working with WebAssembly (aka WASM), to be used with TypeScript and JavaScript.
This code currently supports version MVP-13 (candidate for v1) of WebAssembly.

- Want to learn more about WebAssembly? Check out ["Introduction to WebAssembly"](https://rsms.me/wasm-intro)
- You can also skip the reading and [jump to "Building and testing"](#building-and-testing)

**Overview:**

- [`ast`](src/ast.ts) provides a full TypeScript type system for [the complete WebAssembly specification](https://github.com/WebAssembly/design).
- `ast.c` provides constructors for all parts of a WebAssembly module.
- `ast.t` is a table of AST node types and their respective internal symbols.
- `ast.sect_id` is a table of section names and their respective identifiers as `varunit7` objects.
- `ast.get` provides helper functions for convenient access and traversal of an AST.
- [`emit`](src/emit.ts) provides helpers for emitting WASM byte code from an AST.
- [`repr`](src/repr.ts) generates human-readable text representations of an AST or ArrayBuffer.
- [`lbtext`](src/lbtext.ts) generates [Linear Bytecode text](https://github.com/WebAssembly/design/blob/master/TextFormat.md) from AST instructions

I found myself relying on a very complicated tool chain (source build of llvm, binaryen, etc) while all I was looking for was to get close to WebAssembly. The prime component of wasm-util is `ast` which provides a convenient way of building complete WASM modules, with full static type-checking if you're using TypeScript.

Following is an example of building a module that provides the `factorial` function.
Let's start by describing the function we're making in a C-like syntax:

```cc
int64 factorial(int64 n) {
  return (n == 0) ?
    1
  :
    n * factorial(n-1);
}
```

The equivalent WebAssembly code looks like this:

```wasm
get_local 0    // push parameter #0 on stack.
i64.const 0    // push constant int64 "0" on stack.
i64.eq         // execute "eq" which pops two operands from stack
               //  and pushes int32 "1" or "0" on stack.
if i64         // pops one int32 from stack; if its not "0":
  i64.const 1  //   push constant int64 "0" on stack.
else           // else (if operand was "0"):
  get_local 0  //   push parameter #0 on stack. $1
  get_local 0  //   push parameter #0 on stack.
  i64.const 1  //   push constant int64 "0" on stack.
  i64.sub      //   execute "sub[tract]" which pops two operands
               //    from stack (parameter #0 and constant int64 "1")
               //    and finally pushes the result int64 on stack.
  call 0       //   call function #0 ("factorial") which pops one
               //    int64 from the stack and when it returns an
               //    int64 has been pushed on stack
  i64.mul      //   execute "sub[tract]" which pops two operands
               //    from stack ($1 and result from function call)
               //    and finally pushes the resulting int64 on stack
end            // ends function, returning one int64 result (on stack.)
               // Stack now contains one int64 value that's the result from one of
               // the two branches above.
```

The above code was printed by [lbtext](src/lbtext.ts), for which we provided an AST built with the [ast](src/ast.ts) module:

```js
import ... 'wasm-util/ast'
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
            ]))])])]
  )]
)
```

We can now generate WASM bytecode through the `Emittable` interface:

```ts
const emitbuf = new BufferedEmitter(new ArrayBuffer(mod.z))
mod.emit(emitbuf)
// the array buffer (emitbuf.buffer) now contains the complete module code
```

Or print a human-readable representation of the AST:

```ts
import { strRepr } from 'wasm-util/repr'
console.log(strRepr(mod))
```

Which yields the following in the console:

```lisp
(module 13
  (section type 6 1
    (func_type (i64) i64))
  (section function 2 1 0)
  (section export 13 1
    (export_entry "factorial" external_kind.function 0))
  (section code 25 1
    (function_body 23 0
      (if [i64]
        (i64.eq
          (get_local [0])
          (i64.const [0])
        )
        (then
          (i64.const [1]))
        (else
          (i64.mul
            (get_local [0])
            (call [0]
              (i64.sub
                (get_local [0])
                (i64.const [1])
              )))) end) end)))
```

A complete version of this "factorial" demo can be found at [test/build_factorial_test.ts](test/build_factorial_test.ts).

## ast

`ast` provides a full TypeScript type system for [the complete WebAssembly specification](https://github.com/WebAssembly/design) including AST constructors and access functions.

Noteworthy properties of the AST:

- Nodes are immutable and contains no parents, meaning that any subtree can be used in multiple locations without the need to copy any data (e.g. macros can be trivially "implemented" by building a structure once and using it multiple times.)
- Nodes' underlying type structures are uniform to allow efficient JavaScript VM optimizations.
- Nodes' TypeScript types are rich in expression but with almost zero effect on runtime code — i.e. the `type_section` constructor returns the same kind of underlying structure as `import_section`, but the two functions when operated in TypeScript returns two exclusive, incompatible types (`TypeSection` and `ImportSection`, respectively.)
- Each node has the ability to emit WASM bytecode that represents itself in a very efficient and side-effect-free manner.

The AST is built in a way that makes it as portable and light-weight as possible, with two basic types: atoms and cells. An atom is a single value and represents some amount of bytes corresponding to actual WASM bytecode. A cell is a compount structure that contains other atoms and cells, possibly also represents WASM bytecode.

```ts
// N is the common type of all AST nodes
interface N extends Emittable {
  readonly t :TypeTag  // type
  readonly z :uint32   // size in bytes (includes size of any children)
  readonly v :any      // value
}
interface Atom<T> extends N {
  readonly v :T
}
interface Cell<T extends N> extends N {
  readonly v :T[]
}
interface Module ...
```

For the full type system, see [`ast.ts`](src/ast.ts)


### Static type checking with TypeScript

When used in TypeScript, the operand types, immediate types and result types of opcode and compound instructions are checked at compile time. For instance, say that we're converting a certain code path from i32 to i64 and forget something:

```ts
i64.eq(i64.const(1), i32.const(3))
```

Then the TypeScript compiler will complain:

> error TS2345: Argument of type 'Op<I64>' is not assignable to parameter of type 'Op<I32>'.
>   Type 'I64' is not assignable to type 'I32'.
>     Property '_I32' is missing in type 'I64'.

We can correct the error by replacing `i32.const(3)` with `i64.const(3)` in this case since the `i64.eq` function has the type signature `(i64, i64) i32`.


## emit

AST nodes has the ability to efficiently produce its corresponding
WASM bytecode through the `Emittable` interface:

```ts
interface Emittable {
  emit(e :Emitter) :Emitter
}
```

Which takes an Emitter as its parameter and returns a potentially different Emitter which reflects the state after emitting code for the callee node. The Emitter interface looks like this:

```ts
interface Emitter {
  writeU8(v :uint8) :Emitter
  writeU16(v :uint16) :Emitter
  writeU32(v :uint32) :Emitter
  writeF32(v :float32) :Emitter
  writeF64(v :float64) :Emitter
  writeBytes(v :ArrayLike<uint8>) :Emitter
}
```

Each modifying operation returns a potentially different Emitter which is the result of
the receiver + modifications, thus modifying operations should be called like this:

```js
e = e.writeU32(1)
e = e.writeU32(2)
```

However **NOT** like this:

```js
e.writeU32(1)
e.writeU32(2)
// e represents same state as before `e.writeU32(1)`
```

This interface makes it possible to implement emitters with immutable persistent data structures.

A concrete implementation of an Emitter is provided by `emit` which writes to an `ArrayBuffer`:

```ts
class BufferedEmitter implements Emitter {
  readonly buffer :ArrayBuffer
  readonly view   :DataView
           length :uint32
  constructor(buffer :ArrayBuffer)
}
```

## repr

[repr](src/repr.ts) has the ability to generate human-readable text representations of AST nodes.

There's an example of using `repr` to visualize an AST earlier in this document.

The `repr` function generates a S-expression representation of the AST in a form that is similar to how the AST would have been built using `ast`.

The `reprBuffer` function generates rows and columns of bytes values representing
an `ArrayBuffer` with optional terminal-color higlighting of a range of bytes.
Useful for visualizing what an `Emitter` produces, or for pointing out bytes in a module
that causes an error with the spec interpreter.

```ts
function repr(n :N, w :Writer, options? :Options)

function reprBuffer(
  buffer          :ArrayBuffer,
  w               :Writer,
  limit?          :number,
  highlightRange? :number[],
  options?        :Options)

type Writer = (s :string)=>void
interface Options {
  readonly colors         :boolean  // explicitly enable or disable terminal colors
  readonly immSeparator   :string   // defaults to `:`
  readonly detailedTypes? :boolean, // `vi32(9)` or just `9`
}

// Convenience function that returns a string
function strRepr(n :N, options? :Options) :string

// Convenience function that returns a string
function strReprBuffer(
  buffer          :ArrayBuffer,
  limit?          :number,
  highlightRange? :number[],
  options?        :Options) :string
```


## eval

`eval` is rather specialized module for executing the WebAssembly spec interpreter. It only works with Nodejs as it needs access to both the file system and process spawning.

The `specEval` function evaluates a WASM module and resolves a promise with any stdout output from the spec interpreter.

```ts
function specEval(buf :ArrayBuffer, options? :SpecOptions) :Promise<string>

interface SpecOptions {
  eval?      :string  // S-expression to evaluate after loading the module
  timeout?   :number  // 0 = no timeout. Defaults to 30000ms.
  logErrors? :boolean // when true, logs errors to stderr
  trace?     :boolean // trace execution, printing to stdout
}
```

Have a look at [test/build_test.js](test/build_test.js) for an example where `specEval` is used to test the functionality of a module built with `ast`.


## lbtext

`lbtext` can be used to generate [Linear Bytecode text](https://github.com/WebAssembly/design/blob/master/TextFormat.md) from AST code. E.g.

```wasm
get_local 0
i64.const 2
i64.div_s
end
```

The `printCode` function takes a list of operations to print.

```ts
function printCode(instructions :N[], writer :Writer)
type Writer = (s :string)=>void
```

## Building and testing

First-time setup:

```bash
git clone https://github.com/WebAssembly/spec.git wasm-spec
cd wasm-spec/interpreter
# install ocaml in some way, perhaps with homebrew or aptitude, then
make test && make opt
cd ../..
yarn || npm
```

Building JavaScript from TypeScript source:

```
$ node_modules/.bin/tsc  // puts things in "build" directory
```

Running tests:

```
$ test/test.js
```

Upgrading the spec interpreter:

```bash
git -C wasm-spec pull origin
cd wasm-spec/interpreter
make clean && make test && make opt
```
