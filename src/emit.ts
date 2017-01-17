import {uint8, uint16, uint32, float32, float64} from './basic-types'

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

// Emitter that writes to an ArrayBuffer
export class BufferedEmitter implements Emitter {
  readonly buffer :ArrayBuffer
  readonly view   :DataView
           length :uint32

  constructor(buffer :ArrayBuffer) {
    this.buffer = buffer
    this.view   = new DataView(this.buffer)
    this.length = 0
  }

  writeU8(v :uint8) :Emitter {
    this.view.setUint8(this.length++, v)
    return this
  }

  writeU16(v :uint16) :Emitter {
    this.view.setUint16(this.length, v, true)
    this.length += 2
    return this
  }

  writeU32(v :uint32) :Emitter {
    this.view.setUint32(this.length, v, true)
    this.length += 4
    return this
  }

  writeF32(v :float32) :Emitter {
    this.view.setFloat32(this.length, v, true)
    this.length += 4
    return this
  }

  writeF64(v :float64) :Emitter {
    this.view.setFloat64(this.length, v, true)
    this.length += 8
    return this
  }

  writeBytes(bytes :ArrayLike<uint8>) {
    for (let i = 0, L = bytes.length; i != L; ++i) {
      this.view.setUint8(this.length++, bytes[i])
    }
    return this
  }
}

// Note: you can use repr.reprBuffer(ArrayBuffer, Writer)
// to print an ASCII representation of a buffer.
