import {reprBuffer} from './repr'

declare function require(ref:string):any;

declare interface Buffer {
  buffer :ArrayBuffer
  byteLength :number
  from(a :ArrayLike<number>) :Buffer
  from(a :ArrayBuffer, byteOffset? :number, byteLength? :number) :Buffer
  from(s :string, encoding? :string) :Buffer
  slice(begin :number, end? :number) :ArrayBuffer
  toString(encoding? :string) :string
  [Symbol.toStringTag]: "ArrayBuffer"
}
declare var Buffer :Buffer
declare var __dirname :string
declare var process :{
  on(ev:string, f:(v? :any)=>any),
  removeListener(ev:string, f:(v? :any)=>any),
  env :{[k:string]:any},
}


export interface SpecOptions {
  eval?      :string  // S-expression to evaluate after loading the module
  timeout?   :number  // 0 = no timeout. Defaults to 30000ms.
  logErrors? :boolean // when true, logs errors to stderr
  trace?     :boolean // trace execution, printing to stdout
}

// Evaluate a WASM module using the WebAssembly spec reference interpreter.
export function specEval(buf :ArrayBuffer, options? :SpecOptions) :Promise<string> {
  if (typeof require === 'undefined' || !require('child_process')) {
    throw new Error('eval not supported by current platform')
  }

  if (!options) { options = {} }

  return new Promise(function (resolve, reject) {
    let tmpdir = process.env['TMPDIR'] || ''
    if (!tmpdir) {
      tmpdir = '.'
    } else if (tmpdir[tmpdir.length-1] != '/') {
      tmpdir += '/'
    }
    const randname = Math.random().toString() + Date.now().toString(36)
    const tmpname = tmpdir + 'tmp-' + randname + '.wasm';

    // create temporary file with WASM code
    let rmtmpExecuted = false
    const rmtmp = function() {
      if (!rmtmpExecuted) {
        require('fs').unlinkSync(tmpname)
        rmtmpExecuted = true
        process.removeListener('exit', rmtmp)
      }
    }
    process.on('exit', rmtmp)
    require('fs').writeFileSync(tmpname, Buffer.from(buf))

    let args = []
    
    if (options.trace) {
      args.push('-t')
    }
    
    args.push(tmpname)

    if (options.eval) {
      args = args.concat(['-e', options.eval])
    }

    require('child_process').execFile(
      __dirname + '/../wasm-spec/interpreter/wasm.opt',
      args,
      {
        stdio: 'inherit',
        timeout: options.timeout !== undefined ? options.timeout : 30000,
      },
      (err :Error|null, stdout :string, stderr :string) => {
        rmtmp()
        handleResult(resolve, reject, buf, err, stdout, stderr, options)
      }
    )
  })
}


function handleResult(
  resolve :(value? :{} | PromiseLike<{}>)=>void,
  reject  :(e:Error)=>void,
  buf     :ArrayBuffer,
  err     :Error|null,
  stdout  :string,
  stderr  :string,
  options :SpecOptions)
{
  if (err) {
    if ((err as any).errno === 'ENOENT') {
      // wasm-spec/interpreter/wasm.opt not found
      return reject(new Error(
        `missing "wasm-spec/interpreter/wasm.opt" ${String(err)}`
      ))
    }

    const m = /^[^:]+\.wasm:0x([0-9A-Fa-f]+)(?:-0x([0-9A-Fa-f]+)|):\s*(.+)/.exec(stderr)
    
    if (!m || !m[1]) {
      reject(err)
    } else {

      const e :any = new Error(m[3] || String(err))
      const byteRange = [
        parseInt(m[1], 16),
        m[2] ? parseInt(m[2], 16) : parseInt(m[1], 16) + 1
      ]
      e.byteRange = byteRange

      if (options.logErrors) {
        console.error(e.message)
        const limit = byteRange[1] ? byteRange[1] + 2 : 500
        reprBuffer(buf, s => { (process as any).stderr.write(s) }, limit, byteRange)
      }

      reject(e as Error)
    }

  } else {
    resolve(stdout.replace(/[\r\n\t\s ]+$/, ''))
  }
}
