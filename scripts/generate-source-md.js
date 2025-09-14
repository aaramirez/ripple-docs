#!/usr/bin/env node

var fs = require('fs');
var path = require('path');

// Generate a Markdown file concatenating all source files under @ripple (packages/ripple)

function main() {
  // Defaults are generic and not tied to any repository layout
  var cwd = process.cwd();
  var defaultInputDir = cwd;
  var defaultOutputFile = path.resolve(cwd, 'source-concatenated.md');

  var opts = parseArgs({
    inputDir: defaultInputDir,
    outputFile: defaultOutputFile,
  });

  var entries = collectFilesSync(opts.inputDir, {
    ignore: opts.ignore || [
      'node_modules',
      '.git',
      '.DS_Store',
      'dist',
      'build',
      '.turbo',
      '.next',
    ],
    extensions: opts.extensions || defaultExtensions(),
  });

  // Output order follows directory traversal: depth-first, directories then files, alphabetically.

  var lines = [];
  var title = 'Concatenated Source Code';
  var timestamp = new Date().toISOString();
  lines.push('# ' + title);
  lines.push('');
  lines.push('Generated on ' + timestamp);
  lines.push('');
  lines.push('Input directory: ' + normalizePath(opts.inputDir));
  lines.push('');

  for (var i = 0; i < entries.length; i++) {
    var file = entries[i];
    var relFromInput = path.relative(opts.inputDir, file.abs);
    var language = languageForExtension(path.extname(file.abs), relFromInput);
    lines.push('## ' + normalizePath(relFromInput));
    lines.push('');
    lines.push('```' + language);
    lines.push(file.content);
    lines.push('```');
    lines.push('');
  }

  ensureDirSync(path.dirname(opts.outputFile));
  fs.writeFileSync(opts.outputFile, lines.join('\n'), 'utf8');
  process.stdout.write('Wrote ' + entries.length + ' file(s) to ' + opts.outputFile + '\n');
}

function parseArgs(defaults) {
  var args = process.argv.slice(2);
  var inputDir = defaults.inputDir;
  var outputFile = defaults.outputFile;
  var positionalConsumed = false;
  var extensions = null; // null means use defaults
  var ignore = null; // null means use defaults

  for (var i = 0; i < args.length; i++) {
    var arg = args[i];
    if (arg === '--dir' || arg === '-d') {
      var val = args[++i] || '';
      inputDir = path.resolve(val);
    } else if (arg === '--out' || arg === '-o') {
      outputFile = path.resolve(args[++i] || '');
    } else if (arg === '--ext' || arg === '--extensions') {
      extensions = parseExtensions(args[++i] || '');
    } else if (arg === '--ignore') {
      ignore = parseList(args[++i] || '');
    } else if (arg === '--help' || arg === '-h') {
      printHelpAndExit(defaults);
    } else if (!positionalConsumed) {
      inputDir = path.resolve(arg);
      positionalConsumed = true;
    }
  }

  var opts = { inputDir: inputDir, outputFile: outputFile };
  if (extensions) opts.extensions = extensions;
  if (ignore) opts.ignore = ignore;
  return opts;
}

function printHelpAndExit(defaults) {
  var script = path.basename(__filename);
  var help = [
    'Usage: ' + script + ' [dir] [--dir <path>] [--out <file>] [--ext js,ts,tsx,...] [--ignore dir1,dir2]',
    '',
    'Options:',
    '  -d, --dir         Directory to process (positional arg also supported)',
    '  -o, --out         Output Markdown file (default: source-concatenated.md in CWD)',
    '      --ext         Comma-separated list of file extensions to include',
    '      --ignore      Comma-separated list of directory names to ignore',
    '  -h, --help  Show this help',
    '',
    'Defaults resolved to:',
    '  dir: ' + defaults.inputDir,
    '  out: ' + defaults.outputFile,
  ].join('\n');
  process.stdout.write(help + '\n');
  process.exit(0);
}

function parseExtensions(list) {
  var exts = parseList(list);
  var map = {};
  for (var i = 0; i < exts.length; i++) {
    var e = exts[i].trim();
    if (!e) continue;
    if (e[0] !== '.') e = '.' + e;
    map[e] = true;
  }
  return map;
}

function parseList(val) {
  if (!val) return [];
  return String(val).split(',').map(function (s) { return s.trim(); }).filter(Boolean);
}

function defaultExtensions() {
  return {
    '.js': true,
    '.jsx': true,
    '.ts': true,
    '.tsx': true,
    '.mjs': true,
    '.cjs': true,
    '.d.ts': true,
    '.d.mts': true,
    '.d.cts': true,
    '.json': true,
    '.css': true,
    '.ripple': true,
  };
}

function collectFilesSync(rootDir, options) {
  var ignore = options.ignore || [];
  var extensions = options.extensions || {};
  var results = [];

  function walk(currentDir) {
    var names = safeReaddirSync(currentDir);
    var dirs = [];
    var files = [];

    for (var i = 0; i < names.length; i++) {
      var name = names[i];
      if (ignore.indexOf(name) !== -1) continue;
      var absPath = path.join(currentDir, name);
      var st = safeLstatSync(absPath);
      if (!st) continue;
      if (st.isDirectory()) {
        dirs.push({ name: name, abs: absPath });
      } else if (st.isFile()) {
        files.push({ name: name, abs: absPath });
      }
    }

    dirs.sort(function (a, b) { return a.name.localeCompare(b.name); });
    files.sort(function (a, b) { return a.name.localeCompare(b.name); });

    for (var d = 0; d < dirs.length; d++) {
      walk(dirs[d].abs);
    }

    for (var f = 0; f < files.length; f++) {
      var file = files[f];
      var ext = extensionOf(file.name);
      if (Object.keys(extensions).length > 0 && !extensions[ext]) continue;
      var content = safeReadFileSync(file.abs);
      if (content == null) continue;
      results.push({
        abs: file.abs,
        relative: path.relative(rootDir, file.abs),
        content: content,
      });
    }
  }

  walk(rootDir);
  return results;
}

function extensionOf(filename) {
  if (endsWith(filename, '.d.ts')) return '.d.ts';
  if (endsWith(filename, '.d.mts')) return '.d.mts';
  if (endsWith(filename, '.d.cts')) return '.d.cts';
  return path.extname(filename);
}

function languageForExtension(ext, filePath) {
  switch (ext) {
    case '.ts':
    case '.tsx':
    case '.d.ts':
      return 'ts';
    case '.js':
    case '.jsx':
    case '.mjs':
    case '.cjs':
      return 'js';
    case '.json':
      return 'json';
    case '.css':
      return 'css';
    default:
      if (endsWith(filePath, '.ripple')) return 'ripple';
      return '';
  }
}

function normalizePath(p) {
  return p.split(path.sep).join('/');
}

function ensureDirSync(dir) {
  if (fs.existsSync(dir)) return;
  ensureDirSync(path.dirname(dir));
  try { fs.mkdirSync(dir); } catch (e) {}
}

function safeReaddirSync(dir) {
  try { return fs.readdirSync(dir); } catch (e) { return []; }
}

function safeLstatSync(p) {
  try { return fs.lstatSync(p); } catch (e) { return null; }
}

function safeReadFileSync(p) {
  try { return fs.readFileSync(p, 'utf8'); } catch (e) { return null; }
}

function endsWith(str, suffix) {
  return str.indexOf(suffix, str.length - suffix.length) !== -1;
}

try {
  main();
} catch (err) {
  process.stderr.write(String(err && err.stack ? err.stack : err) + '\n');
  process.exit(1);
}


