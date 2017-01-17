# wasm-utils

Utilities for working with WebAssembly (aka WASM), to be used with TypeScript and JavaScript.

> WebAssembly versions supported: MVP-13

**Overview:**

- `ast` provides a full TypeScript type system for [the complete WebAssembly specification](https://github.com/WebAssembly/design).
- `ast.c` provides constructors for all parts of a WebAssembly module.
- `ast.t` is a table of AST node types and their respective internal symbols.
- `ast.sect_id` is a table of section names and their respective identifiers as `varunit7` objects.
- `ast.get` provides helper functions for convenient access and traversal of an AST.
- `emit` provides helpers for emitting WASM byte code from an AST.
- `repr.repr` generates a human-readable text representation of an AST.
- `repr.reprBuffer` generates a human-readable text representation of an ArrayBuffer.
- `lbtext.printCode` generates [Linear Bytecode text](https://github.com/WebAssembly/design/blob/master/TextFormat.md) from AST instructions


## ast

`ast` provides a full TypeScript type system for [the complete WebAssembly specification](https://github.com/WebAssembly/design) including AST constructors and access functions.

Noteworthy properties of the AST:

- Nodes are immutable and contains no parents, meaning that any subtree can be used in multiple locations without the need to copy any data (e.g. macros can be trivially "implemented" by building a structure once and using it multiple times.)
- Nodes' underlying type structures are uniform to allow efficient JavaScript VM optimizations.
- Nodes' TypeScript types are rich in expression but with almost zero effect on runtime code — i.e. the `type_section` constructor returns the same kind of underlying structure as `import_section`, but the two functions when operated in TypeScript returns two exclusive, incompatible types (`TypeSection` and `ImportSection`, respectively.)
- Each node has the ability to emit WASM bytecode that represents itself in a very efficient and side-effect-free manner.

The AST is built in a way that makes it as portable and light-weight as possible, with two basic types: atoms and cells. An atom is a single value and represents some amount of bytes corresponding to actual WASM bytecode. A cell is a compount structure that contains other atoms and cells, possibly also represents WASM bytecode.


```ts
// Each AST node has the ability to efficiently produce its corresponding
// WASM bytecode through the `Emittable` interface
interface Emittable {
  emit(ctx :Emitter) :Emitter
}

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

Following is an example of building a module that provides the `factorial` function. Let's first describe the function in a C-like syntax:

```cc
int64 factorial(int64 n) {
  return (n == 0) ?
    1
  :
    n * factorial(n-1);
}
```

The equivalent WebAssembly code looks like this (printed by `lbtext.printCode`):

```wasm
get_local 0    // push parameter #0 on stack.
i64.const 0    // push contant int64 "0" on stack.
i64.eq         // execute "eq" which pops two operands from stack
               //  and pushes int32 "1" or "0" on stack.
if i64         // pops one int32 from stack; if its not "0":
  i64.const 1  //   push contant int64 "0" on stack.
else           // else (if operand was "0"):
  get_local 0  //   push parameter #0 on stack. $1
  get_local 0  //   push parameter #0 on stack.
  i64.const 1  //   push contant int64 "0" on stack.
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

Here's how we can build a module that exports this function:

```ts
import { c } from './build/ast'
const { type_section, ... } = c

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
              i64.sub(get_local(i64, 0), i64.const(1))  ]))])])])]
)
```

Notice how we specify operands as arguments to operations rather than keeping track of the stack manually. This makes it harder to make mistakes about what's on the stack (at runtime.)

TODO: lbtext.printCode

TODO: repr

TODO: maybe mention S-expr representation?

- Cell nodes keeps track of operands, immediates and their execution order
- Cell node constructors take operands and immediates as arguments, rather than assuming the correct type 


This example can be found in its full form at [`test/build_factorial_test.ts`](test/build_factorial_test.ts)


## Static type checking with TypeScript

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

```ts
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
  writeF32(v :float32) :Emitter
  writeF64(v :float64) :Emitter
  writeBytes(v :ArrayLike<uint8>) :Emitter
}
```