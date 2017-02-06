#!/usr/bin/env node
"use strict";
const assert = require('assert')
const vm = require('vm')
const fs = require('fs')
const { basename, dirname, extname } = require('path')

let logDebug = function(){};
let debugMode = false;
let isInteractive = null; // [stdoutistty:bool, stderristty:bool]

function style(streamno, style, what) {
  if (!isInteractive[streamno]) {
    return what;
  }
  const styles = {
    'bold'      : ['1', '22'],
    'italic'    : ['3', '23'],
    'underline' : ['4', '24'],
    'inverse'   : ['7', '27'],

    'white'     : ['37', '39'],
    'grey'      : ['90', '39'],
    'black'     : ['30', '39'],
    'blue'      : ['34', '39'],
    'cyan'      : ['36', '39'],
    'green'     : ['32', '39'],
    'magenta'   : ['35', '39'],
    'red'       : ['31', '39'],
    'yellow'    : ['33', '39'],

    'boldWhite'     : ['1;37', '0;39'],
    'boldGrey'      : ['1;90', '0;39'],
    'boldBlack'     : ['1;30', '0;39'],
    'boldBlue'      : ['1;34', '0;39'],
    'boldCyan'      : ['1;36', '0;39'],
    'boldGreen'     : ['1;32', '0;39'],
    'boldMagenta'   : ['1;35', '0;39'],
    'boldRed'       : ['1;31', '0;39'],
    'boldYellow'    : ['1;33', '0;39'],

    'italicWhite'     : ['3;37', '0;39'],
    'italicGrey'      : ['3;90', '0;39'],
    'italicBlack'     : ['3;30', '0;39'],
    'italicBlue'      : ['3;34', '0;39'],
    'italicCyan'      : ['3;36', '0;39'],
    'italicGreen'     : ['3;32', '0;39'],
    'italicMagenta'   : ['3;35', '0;39'],
    'italicRed'       : ['3;31', '0;39'],
    'italicYellow'    : ['3;33', '0;39'],
  };
  let v = styles[style];
  return `\x1b[${v[0]}m${what}\x1b[${v[1]}m`;
}


function readfile(filename) {
  return new Promise((resolve, reject) => {
    fs.readFile(filename, {encoding:'utf8'}, (e, s) => {
      if (e) { reject(e) } else { resolve(s) }
    })
  })
}

function writefile(filename, data) {
  return new Promise((resolve, reject) => {
    let triedmkdir = false
    function tryw() {
      fs.writeFile(filename, data, e => {
        if (e) {
          if (!triedmkdir && e.code == 'ENOENT') {
            triedmkdir = true
            fs.mkdir(dirname(filename), e2 => {
              if (e2) { reject(e) } else { tryw() }
            })
          } else {
            reject(e)
          }
        } else {
          resolve()
        }
      })
    }
    tryw()
  })
}


function runJSTestInSandbox(src, filename, name, sandbox) { // :Promise<void>
  logDebug('run', name);
  return new Promise((resolve, reject) => {
    let expectError = null;
    let m = src.match(/\/\/\!error\s+(.+)\s*(?:\n|$)/);
    if (m) {
      expectError = m[1];
      if (expectError[0] != '/') {
        throw new Error('"//!error" value must be a regex (/.../)');
      }
      expectError = vm.runInNewContext(expectError);
    }

    let script = new vm.Script(src, {
      filename,
      displayErrors: true,
      timeout: 5000,
    });

    let ctx = {
      require,
      console,
      assert,
      setTimeout,
      clearTimeout,
      Buffer,
      isUnitTest: true,
      __dirname: dirname(filename),
      process: {
        stdout: process.stdout,
        stderr: process.stderr,
        isUnitTest: true,
        nextTick: process.nextTick,
        exit(code){ throw new Error('EXIT ' + code) },
      }
    }
    if (sandbox) {
      Object.extend(ctx, sandbox)
    }

    if (expectError) {
      assert.throws(() => {
        script.runInNewContext(ctx);
      }, expectError);
    } else {
      logDebug(`script.runInNewContext("${filename}") ENTER`)
      script.runInNewContext(ctx);
      logDebug(`script.runInNewContext("${filename}") EXIT`)

      if (ctx.Test) {
        let p = ctx.Test();
        if (typeof p == 'object' && typeof p.then == 'function') {
          // Test function returned a promise
          p.then(resolve).catch(reject);
          return;
        }
      }
    }

    resolve();
  });
}

function tsc(source, filename) { // :Promise<string>
  return new Promise((resolve, reject) => {
    const ts = require(__dirname + '/../node_modules/typescript/lib/typescript.js')
    const compilerOptions = {
      module: ts.ModuleKind.CommonJS,
      target: ts.ScriptTarget.Latest,
      noImplicitAny: false,
      inlineSourceMap: true,
      alwaysStrict: true,
      forceConsistentCasingInFileNames: true,
      noEmitOnError: true,
      lib: [
        "dom",
        "es6",
      ],
      pretty: true
    }

    let r = ts.transpileModule(source, {
      compilerOptions,
      fileName: filename,
      reportDiagnostics: true,
      moduleName: basename(filename, extname(filename)),
    })

    if (r.diagnostics && r.diagnostics.length) {
      const log = {
        [ts.DiagnosticCategory.Warning]: console.warn.bind(console,  '[tsc warning]'),
        [ts.DiagnosticCategory.Error]:   console.error.bind(console, '[tsc error]'),
        [ts.DiagnosticCategory.Message]: console.log.bind(console,   '[tsc]'),
      }
      let firstError = null
      for (let d of r.diagnostics) {
        let prefix = ''
        if (d.file) {
          const pos = d.file.getLineAndCharacterOfPosition(d.start)
          prefix += (d.file.fileName.endsWith(filename) ?
                     basename(filename) : d.file.fileName) +
                    ':' + pos.line + ':' + pos.character + ': '
        }
        prefix += 'TS'+d.code+':'
        log[d.category](prefix, d.messageText)
        if (!firstError && d.category == ts.DiagnosticCategory.Error) {
          firstError = d.messageText
        }
      }
      if (firstError) {
        return reject(new Error(firstError))
      }
    }

    resolve(r.outputText)
  })
}

function getCompiledTypeScript(filename) { // :Promise<string>
  assert.equal(extname(filename), '.ts')

  const outfilename = __dirname + '/build/' + basename(filename, extname(filename)) + '.js'

  // has input been modified since output was modified?
  const instat = fs.statSync(filename)
  try {
    const outstat = fs.statSync(outfilename)
    if (instat.mtime.getTime() <= outstat.mtime.getTime()) {
      // infile hasn't changed since outfile did
      return readfile(outfilename)
    }
  } catch (_) {}

  logDebug('tsc', filename, '->', outfilename)

  return readfile(filename)
         .then(source => tsc(source, filename))
         .then(source => writefile(outfilename, source)
                         .then(() => source))
}

function runJSTest(filename, name) {
  return (
    extname(filename) == '.ts'
    ? getCompiledTypeScript(filename)
    : readfile(filename)
  ).catch(e => {
    console.error(e.stack || String(e))
    throw e
  }).then(source =>
    runJSTestInSandbox(source, filename, name).then(() => {
      process.stdout.write(
        `${style(0,'boldGreen','pass')} ${name}\n`
      )
    }).catch(err => {
      process.stderr.write(
        `${style(1,'boldRed','FAIL')} ${name}:\n${err.stack || String(err)}\n`
      )
      throw err;
    })
  )
}

const suffix = '_test';
const fileexts = ['.js', '.ts']

function runAllTests() {
  return Promise.all(
    fs.readdirSync(__dirname).filter(fn => {
      const ext = extname(fn)
      return ext.includes(ext) && basename(fn, ext).substr(-suffix.length) == suffix
    }).map(fn =>
      runJSTest(__dirname + '/' + fn, fn.substr(0, fn.length - extname(fn).length - suffix.length))
    )
  )
}

function isFileSync(filename) {
  try {
    const st = fs.statSync(filename)
    return st.isFile()
  } catch (_) {}
  return false
}

function testNameToFilename(test) {
  const mkname = s => {
    const b = basename(s, ext)
    return b.substr(0, b.length - suffix.length)
  }

  let ext = extname(test)
  if (test[0] == '/' || isFileSync(test)) {
    return { path: test, name: mkname(test) }
  }

  for (let ext of fileexts) {
    let name = test + suffix + ext
    let path = __dirname + '/' + name
    if (isFileSync(path)) {
      return { path, name: test }
    }
  }
  
  let path = __dirname + '/' + test;
  if (isFileSync(path)) {
    return {path, name: mkname(test)}
  }
    
  throw new Error(`test not found: "${test}"`);
}

function runSomeTests(tests) {
  return Promise.all(
    tests.map(testNameToFilename).map(f =>
      runJSTest(f.path, f.name))
  )
}

let args = process.argv.slice(2);
let timeout = 30000;
let timeoutTimer;

let onerr = err => {
  clearTimeout(timeoutTimer);
  console.error(style(1,'boldRed', 'aborted by error'));
  process.exit(1);
};

let onallpass = () => {
  console.log(style(0,'boldGreen','all pass'));
  clearTimeout(timeoutTimer);
}

timeoutTimer = setTimeout(function(){
  console.error(style(1,'boldRed', `timeout after ${(timeout/1000).toFixed(2)}s`));
  onerr();
}, timeout);

function checkflag(flag) {
  let i = args.indexOf('-' + flag)
  if (i == -1) { i = args.indexOf('--' + flag) }
  if (i != -1) {
    args.splice(i,1)
    return true
  }
  return false
}

if (args.indexOf('-h') != -1 || args.indexOf('--help') != -1) {
  console.error([
    'Usage: test [options] [<test> ...]',
    'options:',
    '  --debug              Enable debug mode; prints detailed information.',
    '  --[non-]interactive  Instead of checking if stdout is interactive, be explicit',
    '                       about whether colors and other TTY-related things are output.',
  ].join('\n'));
  process.exit(1);
}

if (checkflag('non-interactive')) {
  isInteractive = [false,false];
}
if (checkflag('interactive')) {
  isInteractive = [true,true];
}
if (isInteractive === null) {
  // auto-detect
  isInteractive = [process.stdout.isTTY,process.stderr.isTTY];
}

if (checkflag('debug')) {
  debugMode = true;
  logDebug = function() {
    let args = Array.prototype.slice.call(arguments);
    args.unshift(style(0,'boldMagenta','D'));
    console.log.apply(console, args);
  };
}


(args.length ? runSomeTests(args) : runAllTests()).catch(onerr).then(onallpass);
