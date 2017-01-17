"use strict";
const assert = require('assert')
const v8 = require('v8')

if (typeof gc != 'function') {
  console.error('gc not exposed. Run node with --expose_gc')
  process.exit(1)
}

const range = function(start, end, fn) {
  for (let i = 0, v = start, incr = (start < end ? 1 : -1); v != end; v += incr) {
    fn(v, i++)
  }
}


const VarInt32a = function(value) {
  // assert(value >= -0x80000000 && value <= 0x7fffffff)
  const bytes = []
  let v = value, done = false
  do {
    let b = v & 0x7f
    v >>= 7 // Note: sign-propagating right shift
    done = ((v == 0)  && ((b & 0x40) == 0)) ||
           ((v == -1) && ((b & 0x40) != 0)) ;
    if (!done) { b |= 0x80; }
    bytes.push(b)
  } while (!done)
  return bytes
}

const VarInt32a2 = function(v) {
  // assert(value >= -0x80000000 && value <= 0x7fffffff)
  const bytes = []
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

const VarInt7 = function(value) {
  // varint range: [-2^(N-1), +2^(N-1)-1]  where N is bits
  assert(value >= -64 && value <= 63)
  return value < 0 ?
    [ 128 - value ] : [ value ]

  if (value > 64) {
    return [ (value & 0x7f) | 0x80 , 0 ]
  }
  if (value < -64) {
    return [ (value & 0x7f) | 0x80 , 127 ]
  }
  if (value < 64 && value >= 0) {
    return [value]
  }
  const bytes = []
  let v = value, done = false
  do {
    let b = v & 0x7f
    v >>= 7 // Note: sign-propagating right shift
    done = ((v == 0)  && ((b & 0x40) == 0)) ||
           ((v == -1) && ((b & 0x40) != 0)) ;
    if (!done) { b |= 0x80; }
    bytes.push(b)
  } while (!done)
  return bytes
}

const ReadVarUint32 = function(bytes) {
  let value = 0, shift = 0, b, i = 0
  do {
    b = bytes[i++]
    value += (shift == 28) ?
      (b & 0x7f) * 0x10000000 : // because JS numbers are the worst
      (b & 0x7f) << shift ;
    // console.log(`... (b=${b}, shift=${shift}) ${value} + ${d} = ${value + d}`)
    shift += 7
  } while (b >= 128)
  return value // {value, length: i}
}

const VarUint32a = function(v) {
  const bytes = []
  while (true) {
    let b = v & 0x7f
    v >>>= 7
    if (v != 0) {
      bytes.push(b | 0x80)
    } else {
      bytes.push(b)
      break
    }
  }
  return bytes
}

const VarUint32a2 = function(v) {
  // assert(value >= -0x80000000 && value <= 0x7fffffff)
  const bytes = []
  while (v >= 0x80) {
    bytes.push((v & 0x7f) | 0x80)
    v >>>= 7
  }
  bytes.push(v)
  return bytes
}

let value = 0xffffffff
let bytes = VarUint32a(value)
console.log('bytes:', bytes)
let readValue = ReadVarUint32(bytes)
console.log(`values:\n  read:     ${readValue}\n  expected: ${value}`)
assert.equal(ReadVarUint32(VarUint32a(value)), value)

const VarUint32b = function(v) {
  const bytes = new Uint8Array(5)
  let i = 0
  do {
    let b = v & 0x7f
    v >>>= 7
    bytes[i++] = (v != 0) ? (b | 0x80) : b
  } while (v != 0)
  return bytes
}

const VarUint32b2 = (function(){
  let buf, u8, bi
  function resetBuf() {
    buf = new ArrayBuffer(4096)
    u8 = new Uint8Array(buf)
    bi = 0
  }
  resetBuf()
  return function(v) {
    if (bi + 5 >= buf.byteLength) {
      resetBuf()
    }
    const starti = bi
    do {
      let b = v & 0x7f
      v >>>= 7
      u8[bi++] = (v != 0) ? (b | 0x80) : b
    } while (v != 0)
    return u8.subarray(starti, bi)
  }
})()


console.log('VarInt32a(-0x80000000):  ', VarInt32a(-0x80000000))
console.log('VarInt32a(0x7fffffff):  ', VarInt32a(0x7fffffff))
console.log('VarInt32a2(-0x80000000):  ', VarInt32a2(-0x80000000))
console.log('VarInt32a2(0x7fffffff):  ', VarInt32a2(0x7fffffff))

console.log('VarUint32a(0xffffffff): ', VarUint32a(0xffffffff))
console.log('VarUint32a2(0xffffffff): ', VarUint32a2(0xffffffff))
console.log('VarUint32b(0xffffffff): ', VarUint32b(0xffffffff))
console.log('VarUint32b2(0xffffffff):', VarUint32b2(0xffffffff))

const iterations = 0xfffff

function benchmark(name, f) {
  for (let i = 0, x = console.time(name); i != iterations; ++i) {
    f(i)
  }
  console.timeEnd(name)
}


function Buffer(size) {
  const buffer = new ArrayBuffer(size)
  return {
    head: buffer,   // linked list by ._nextBuffer
    u8array:  new Uint8Array(buffer),
    view:     new DataView(buffer),
    restlen:  0, // sum of lengths of all used buffers
    headlen:  0, // length of head

    get length() {
      return this.headlen + this.restlen
      // let len = this.index
      // let b = this.buffer
      // while (b = b._nextBuffer) {
      //   len += b.byteLength
      // }
      // return len
    },

    allocNewBuffer() {
      this.restlen += this.headlen
      this.headlen = 0
      const buf = new ArrayBuffer(size)
      buf._nextBuffer = this.head
      this.head  = buf
      this.u8array = new Uint8Array(buf)
      this.view    = new DataView(buf)
    },

    writeBytes(bytes /*:ArrayLike<uint8>*/) {
      if (this.head.byteLength - this.headlen < bytes.length) {
        this.allocNewBuffer()
      }
      for (let i = 0, L = bytes.length; i != L; ++i) {
        this.view.setUint8(this.headlen++, bytes[i])
      }
      return this
    },

    setBytes(bytes /*:ArrayBufferView*/) {
      if (this.head.byteLength - this.headlen < bytes.byteLength) {
        this.allocNewBuffer()
      }
      this.u8array.set(bytes, this.headlen)
      this.headlen += bytes.byteLength
      return this
    }
  }
}

function Buffer2() {
  return {
    a:      [],
    get length() { return this.a.length },
    writeBytes(bytes /*:ArrayLike<uint8>*/) {
      for (let i = 0, L = bytes.length; i != L; ++i) {
        this.a.push(bytes[i])
      }
      return this
    }
  }
}

// Disable these to get accurate heap allocation readings
const iterations_2 = iterations/2
benchmark('assert eq VarInt32a == VarInt32a2', i => {
  let v = i < iterations_2 ? (-i*2) : (i*2);
  let r1 = VarInt32a(v)
  let r2 = VarInt32a2(v)
  assert.deepEqual(r1, r2)
})
benchmark('assert eq VarUint32a == VarUint32a2', v => {
  let r1 = VarUint32a(v)
  let r2 = VarUint32a2(v)
  assert.deepEqual(r1, r2)
})
;(function(){
  let bytes = []
  for (let i = 0; i != iterations; ++i) {
    bytes.push(VarUint32a(i*2))
  }
  for (let i = 0; i != iterations; ++i) {
    assert.equal(ReadVarUint32(bytes[i]), i*2)
  }
  benchmark('ReadVarUint32',  i => { ReadVarUint32(bytes[i]) })
})()
benchmark('VarInt32a',   i => { VarInt32a(i < iterations_2 ? (-i*2) : (i*2)) })
benchmark('VarInt32a2',  i => { VarInt32a2(i < iterations_2 ? (-i*2) : (i*2)) })
benchmark('VarUint32a',  i => { VarUint32a(i*2) })
benchmark('VarUint32a2', i => { VarUint32a2(i*2) })
benchmark('VarUint32b',  i => { VarUint32b(i*2) })
benchmark('VarUint32b2', i => { VarUint32b2(i*2) })


// ——————————————————————————————————————————————————
let initStats, deltaStats

gc()
initStats = v8.getHeapStatistics()

let buf = Buffer(512)
let expectedLen = 0
benchmark('buf.writeBytes VarUint32a',  i => {
  let b = VarUint32a(i*2)
  expectedLen += b.length
  buf.writeBytes(b)
})
benchmark('buf.writeBytes VarUint32b2', i => {
  let b = VarUint32b2(i*2)
  expectedLen += b.length
  buf.writeBytes(b)
})
benchmark('buf.setBytes   VarUint32b2', i => {
  let b = VarUint32b2(i*2)
  expectedLen += b.length
  buf.setBytes(b)
})

assert.equal(buf.length, expectedLen)

deltaStats = v8.getHeapStatistics()
for (let k in deltaStats) { deltaStats[k] -= initStats[k] }
console.log('heap allocations:', fmtByteSize(deltaStats.total_heap_size))

console.log('——————————————————————————————————————————————————')

gc()
initStats = v8.getHeapStatistics()

let buf2 = Buffer2()
let expectedLen2 = 0
benchmark('buf2.writeBytes VarUint32a',  i => {
  let b = VarUint32a(i*2)
  expectedLen2 += b.length
  buf2 = buf2.writeBytes(b)
})
benchmark('buf2.writeBytes VarUint32b2', i => {
  let b = VarUint32b2(i*2)
  expectedLen2 += b.length
  buf2 = buf2.writeBytes(b)
})

assert.equal(buf2.length, expectedLen2)
// assert.equal(expectedLen, expectedLen2) // Buffer vs Buffer2

deltaStats = v8.getHeapStatistics()
for (let k in deltaStats) { deltaStats[k] -= initStats[k] }
console.log('heap allocations:', fmtByteSize(deltaStats.total_heap_size))


function fmtByteSize(n) {
  return (
    n >= 1024000 ? parseInt((n / 1024000).toFixed(2)) + ' MB' :
    n >=    1024 ? parseInt((n / 1024).toFixed(3)) + ' kB' :
                   n + ' B'
  )
}


// const VarInt32 = function(value :int32) :VarInt32 {
//   assert(value >= -0x80000000 && value <= 0x7fffffff)
//   const bytes :uint8[] = []
//   let v = value
//   let done = false
//   do {
//     let b = v & 0x7f
//     v >>= 7
//     done = ((v == 0)  && ((b & 0x40) == 0)) ||
//            ((v == -1) && ((b & 0x40) != 0)) ;
//     if (!done) { b |= 0x80; }
//     bytes.push(b)
//   } while (!done)
//   return new bytes_atom<int32>(t.varint32, value, bytes) as VarInt32
// }