irc.w3.org#webassembly Jan 11, 2017

TL;DR: mem imm's offset is simply a way for the assembler/compiler to add a constant offset in addition to the dynamic address provided by the addr operand.

```
11:58  rsms:  Question: memory address semantics (https://github.com/WebAssembly/design/blob/master/Semantics.md#addressing) doesn't make it clear if the `address` operand signifies memory index, memory-page index or virtual address. I'm _guessing_ it signified memory-page index, but I'm _hoping_ it's virtual address (byte offset into total default memory segment.)
13:04  jfb: rsms it's the address within the WebAssembly.Memory's buffer. Address 0 is the start of that buffer. It's *not* virtual address from the process' POV, but you can think of it as the virtual address from the WebAssembly VM's perspective (certainly, from C++ code's POV when executing in a wasm VM).
13:09  rsms:  jfb: Thanks for your reply. So `(i32.const 8) (i32.const 132) (i32.store (alignment 2) (offset 0))` would cause the value "123" to be stored at bytes [8..11] in the first memory page?
13:10  rsms:  And `(i32.const 72) (i32.const 132) (i32.store (alignment 2) (offset 0))` would cause the value "123" to be stored at bytes [72..75] in the second memory page, e.g. at effective address 72?
13:42  dschuff: rsms: not in the second memory page
13:42  dschuff: but at effective address 72 yes
13:43  dschuff: page size in wasm is defined as 64k but it's only meaningful for specifying the size of the linear memory
13:50  rsms:  dschuff: Sorry, I meant 72000 th byte in the second example, i.e. `(i32.const 72000) (i32.const 132) (i32.store (alignment 2) (offset 0))` writes i32 "123" at bytes [72000..75000] within the second memory page, assuming the second page starts at byte 64000
13:50  rsms:  72000..72003 *
13:51  rsms:  If this is correct, then when would I ever use the "offset" property of the memory immediate?
14:03  dschuff: rsms: that's correct. and yes, `(i32.const 8)(i32.const 132)(i32.store (offset 0))` is equivalent to `(i32.const 0)(i32.const 132)(i32.store (offset 8))`
14:04  dschuff: but if you imagine that instead of (i32.const 8) you have some non-constant expression, then it might still be convenient for your compiler to additionaly have the constant offset
```
