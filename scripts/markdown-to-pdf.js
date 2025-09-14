#!/usr/bin/env node

var fs = require('fs');
var path = require('path');
var cp = require('child_process');
var os = require('os');

function main() {
  var args = process.argv.slice(2);
  if (args.length === 0 || hasFlag(args, '--help') || hasFlag(args, '-h')) {
    printHelp();
    process.exit(0);
  }

  var input = null;
  var output = null;
  var stylesheet = null;
  var marginAll = null;
  var marginTop = null, marginRight = null, marginBottom = null, marginLeft = null;

  for (var i = 0; i < args.length; i++) {
    var a = args[i];
    if (a === '--out' || a === '-o') {
      output = path.resolve(args[++i] || '');
    } else if (a === '--css' || a === '--stylesheet') {
      stylesheet = path.resolve(args[++i] || '');
    } else if (a === '--margin') {
      marginAll = String(args[++i] || '');
    } else if (a === '--margin-top') {
      marginTop = String(args[++i] || '');
    } else if (a === '--margin-right') {
      marginRight = String(args[++i] || '');
    } else if (a === '--margin-bottom') {
      marginBottom = String(args[++i] || '');
    } else if (a === '--margin-left') {
      marginLeft = String(args[++i] || '');
    } else if (a[0] === '-') {
      // ignore unknown flags for now
    } else if (!input) {
      input = path.resolve(a);
    }
  }

  if (!input) fail("Missing input markdown file. See --help.");
  if (!fs.existsSync(input)) fail('Input not found: ' + input);
  if (!output) output = replaceExt(input, '.pdf');

  var margins = resolveMargins({ all: marginAll, top: marginTop, right: marginRight, bottom: marginBottom, left: marginLeft });

  // Only: Pandoc to HTML + Chrome headless (honors CSS and @page margins)
  var used = tryPandocHtmlThenChrome(input, output, stylesheet, margins);
  if (!used) {
    fail(
      'Could not convert. Requirements:\n' +
      '  - Google Chrome (or Chromium) with headless printing support\n' +
      '  - pandoc'
    );
  }

  process.stdout.write('Wrote PDF to ' + output + '\n');
}

function tryMdToPdf(input, output, stylesheet, margins) {
  var hasMdToPdf = commandAvailable('md-to-pdf');
  var baseDir = path.dirname(input);
  var cssPath = ensureStylesheet(stylesheet, input, margins);

  if (hasMdToPdf) {
    var args = [input, '--basedir', baseDir, '--out', output];
    if (cssPath) args.push('--stylesheet', cssPath);
    // md-to-pdf respects @page margins; additional CLI control is limited, rely on CSS.
    var res = spawnSyncPrint('md-to-pdf', args);
    cleanupTempCss(stylesheet, cssPath);
    return res === 0;
  }

  return false;
}

// Removed wkhtmltopdf and direct pandoc PDF fallbacks

function tryPandocHtmlThenChrome(input, output, stylesheet, margins) {
  if (!commandAvailable('pandoc')) return false;
  var chromeBin = findChromeBinary();
  if (!chromeBin) return false;
  var htmlOut = replaceExt(output, '.html');
  var cssPath = ensureStylesheet(stylesheet, input, margins);
  var cssArgs = cssPath ? ['--css', cssPath] : [];
  var pandocArgs = ['--from=markdown', '--to=html5', '-s', '--self-contained']
    .concat(cssArgs)
    .concat([input, '-o', htmlOut]);
  var code1 = spawnSyncPrint('pandoc', pandocArgs, { stdio: 'ignore' });
  if (code1 !== 0) return false;
  // Replace tabs with a single space and compress space-indentation to 1-space per level
  try {
    replaceTabsInCodeBlocks(htmlOut);
    normalizeIndentationInCodeBlocks(htmlOut);
  } catch (e) {}
  var fileUrl = pathToFileUrl(htmlOut);
  var chromeArgs = [
    '--headless=new',
    '--disable-gpu',
    '--no-sandbox',
    '--print-to-pdf=' + output,
    fileUrl,
  ];
  var code2 = spawnSyncPrint(chromeBin, chromeArgs);
  try { fs.unlinkSync(htmlOut); } catch (e) {}
  cleanupTempCss(stylesheet, cssPath);
  return code2 === 0;
}

function ensureStylesheet(userStylesheet, inputPath, margins) {
  if (userStylesheet && fs.existsSync(userStylesheet)) return userStylesheet;
  // Create a temp CSS with sensible defaults for print and code wrapping
  var computedMargin = '10mm';
  if (margins && margins.top && margins.right && margins.bottom && margins.left) {
    computedMargin = margins.top + ' ' + margins.right + ' ' + margins.bottom + ' ' + margins.left;
  }
  var css = [
    '@page { margin: ' + computedMargin + '; }',
    'html, body { margin: 0; padding: 0; }',
    'body { font-family: -apple-system, BlinkMacSystemFont, Segoe UI, Roboto, Helvetica, Arial, sans-serif; font-size: 12px; line-height: 1.35; padding: 0; }',
    'h1, h2, h3, h4, h5, h6 { page-break-after: avoid; }',
    'pre, code, kbd, samp { font-family: SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace; font-size: 12px; }',
    'pre { white-space: pre-wrap; word-break: break-all; overflow-wrap: anywhere; padding: 6px 8px; background: #fafafa; tab-size: 1; }',
    'pre > code { white-space: inherit; tab-size: 1; }',
    'code { white-space: pre-wrap; word-break: break-all; overflow-wrap: anywhere; tab-size: 1; }',
    'table { width: 100%; border-collapse: collapse; table-layout: fixed; }',
    'table, th, td { border: 1px solid #ddd; }',
    'th, td { padding: 6px; word-wrap: break-word; }',
    'img { max-width: 100%; }'
  ].join('\n');
  var tmpFile = path.join(os.tmpdir(), path.basename(inputPath, path.extname(inputPath)) + '.print.css');
  try { fs.writeFileSync(tmpFile, css, 'utf8'); } catch (e) { return null; }
  return tmpFile;
}

function cleanupTempCss(userStylesheet, cssPath) {
  if (!cssPath) return;
  if (userStylesheet && path.resolve(userStylesheet) === path.resolve(cssPath)) return;
  try { fs.unlinkSync(cssPath); } catch (e) {}
}

function resolveMargins(m) {
  // Default 10mm per side
  var d = '10mm';
  var all = normalizeUnit(m.all) || null;
  return {
    top: normalizeUnit(m.top) || all || d,
    right: normalizeUnit(m.right) || all || d,
    bottom: normalizeUnit(m.bottom) || all || d,
    left: normalizeUnit(m.left) || all || d,
  };
}

function normalizeUnit(v) {
  if (!v) return null;
  var s = String(v).trim();
  if (!s) return null;
  if (/mm$|cm$|in$|px$/i.test(s)) return s;
  if (/^\d+(\.\d+)?$/.test(s)) return s + 'mm';
  return s; // fallback
}

// wkhtmltopdf margin args removed with wkhtml path

function findChromeBinary() {
  var candidates = [
    '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
    '/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary',
    'google-chrome-stable',
    'google-chrome',
    'chromium',
    'chromium-browser',
  ];
  for (var i = 0; i < candidates.length; i++) {
    var bin = candidates[i];
    try {
      var res = cp.spawnSync(bin, ['--version'], { stdio: 'ignore' });
      if (res && res.status === 0) return bin;
    } catch (e) {}
  }
  return null;
}

function pathToFileUrl(p) {
  var abs = path.resolve(p);
  var prefix = process.platform === 'win32' ? 'file:///' : 'file://';
  return prefix + abs.split(path.sep).map(encodeURIComponent).join('/');
}

function replaceTabsInCodeBlocks(htmlPath) {
  var html = fs.readFileSync(htmlPath, 'utf8');
  // Inside <pre><code> blocks, replace literal tab characters with a single space
  html = html.replace(/(<pre[^>]*>\s*<code[^>]*>)([\s\S]*?)(<\/code>\s*<\/pre>)/g, function(_, open, content, close) {
    var normalized = content.replace(/\t/g, ' ');
    return open + normalized + close;
  });
  fs.writeFileSync(htmlPath, html, 'utf8');
}

function normalizeIndentationInCodeBlocks(htmlPath) {
  var html = fs.readFileSync(htmlPath, 'utf8');
  html = html.replace(/(<pre[^>]*>\s*<code[^>]*>)([\s\S]*?)(<\/code>\s*<\/pre>)/g, function(_, open, content, close) {
    var lines = content.split('\n');
    var indents = [];
    for (var i = 0; i < lines.length; i++) {
      var m = lines[i].match(/^( +)/);
      if (m && m[1]) indents.push(m[1].length);
    }
    var unit = computeIndentUnit(indents);
    if (!unit || unit <= 1) return open + content + close;
    for (var j = 0; j < lines.length; j++) {
      var mm = lines[j].match(/^( +)/);
      if (!mm) continue;
      var len = mm[1].length;
      var levels = Math.max(1, Math.floor(len / unit));
      lines[j] = lines[j].replace(/^ +/, new Array(levels + 1).join(' '));
    }
    return open + lines.join('\n') + close;
  });
  fs.writeFileSync(htmlPath, html, 'utf8');
}

function computeIndentUnit(indents) {
  if (!indents || indents.length === 0) return 0;
  // Filter zeros and compute GCD
  var filtered = [];
  for (var i = 0; i < indents.length; i++) if (indents[i] > 0) filtered.push(indents[i]);
  if (filtered.length === 0) return 0;
  var g = filtered[0];
  for (var j = 1; j < filtered.length; j++) g = gcd(g, filtered[j]);
  return g || 1;
}

function gcd(a, b) {
  a = Math.abs(a); b = Math.abs(b);
  while (b) { var t = b; b = a % b; a = t; }
  return a;
}

function commandAvailable(cmd) {
  try {
    var which = process.platform === 'win32' ? 'where' : 'which';
    var res = cp.spawnSync(which, [cmd], { stdio: 'ignore' });
    return res && res.status === 0;
  } catch (e) {
    return false;
  }
}

function spawnSyncPrint(cmd, args) {
  var res = cp.spawnSync(cmd, args, { stdio: 'inherit' });
  if (res.error) return 1;
  return typeof res.status === 'number' ? res.status : 1;
}

function replaceExt(file, newExt) {
  var dir = path.dirname(file);
  var base = path.basename(file, path.extname(file));
  return path.join(dir, base + newExt);
}

function hasFlag(args, flag) {
  return args.indexOf(flag) !== -1;
}

function fail(msg) {
  process.stderr.write(String(msg) + '\n');
  process.exit(1);
}

function printHelp() {
  var script = path.basename(__filename);
  var help = [
    'Usage: ' + script + ' <input.md> [--out <output.pdf>] [--css <stylesheet.css>] [--margin <size>] [--margin-top <size>] [--margin-right <size>] [--margin-bottom <size>] [--margin-left <size>]',
    '',
    'Converts a Markdown file to PDF using one of the available tools:',
    '  - md-to-pdf (preferred; via local install or npx)',
    '  - pandoc (fallback; requires a PDF engine like wkhtmltopdf or LaTeX)',
    '',
    'Examples:',
    '  node ' + __filename + ' ./README.md',
    '  node ' + __filename + ' ./README.md --out ./README.pdf',
    '  node ' + __filename + ' ./long.md --css ./print.css --margin 10mm',
  ].join('\n');
  process.stdout.write(help + '\n');
}

try { main(); } catch (err) {
  process.stderr.write(String(err && err.stack ? err.stack : err) + '\n');
  process.exit(1);
}


