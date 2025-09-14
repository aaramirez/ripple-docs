## Source Concatenation Script

Generate a single Markdown file containing the concatenated source code for a directory. Each file is listed with its path followed by a language-tagged code block.

### Script

`generate-source-md.js`

`markdown-to-pdf.js`

### Requirements

- Node.js (no external dependencies)

### Usage

Run from anywhere. If no directory is provided, the current working directory is used. By default, the output is written to `source-concatenated.md` in the current working directory.

```bash
node /Users/administrador/P/opensource/ripple/ripple-docs/scripts/generate-source-md.js [dir] [--dir <path>] [--out <file>] [--ext js,ts,tsx,...] [--ignore dir1,dir2]
```

### Options

- **dir / --dir**: Directory to process. You can provide it positionally or with `--dir`.
- **--out**: Output Markdown file path. Default: `source-concatenated.md` in the current working directory.
- **--ext**: Comma-separated list of file extensions to include. Defaults include: `js, jsx, ts, tsx, mjs, cjs, d.ts, d.mts, d.cts, json, css, ripple`.
- **--ignore**: Comma-separated list of directory names to ignore. Default: `node_modules, .git, .DS_Store, dist, build, .turbo, .next`.
- **-h / --help**: Show help.

### Examples

- Process current directory, write to default output:

```bash
node /Users/administrador/P/opensource/ripple/ripple-docs/scripts/generate-source-md.js
```

- Process a specific directory and choose an output file:

```bash
node /Users/administrador/P/opensource/ripple/ripple-docs/scripts/generate-source-md.js /Users/administrador/P/opensource/ripple/ripple/packages/ripple --out /Users/administrador/P/opensource/ripple/ripple-docs/ripple-first-commit.md
```

- Limit to certain extensions and ignore additional folders:

```bash
node /Users/administrador/P/opensource/ripple/ripple-docs/scripts/generate-source-md.js /Users/administrador/P/opensource/ripple/ripple/packages/ripple --ext js,ts,css,json --ignore node_modules,dist,.git
```

### Output format

- The Markdown begins with a title, timestamp, and the input directory.
- For each file:
  - A second-level heading with the path relative to the input directory
  - A fenced code block tagged by language (derived from the file extension)

### Ordering

- Traversal is stable and alphabetical: directories first, then files, processed depth-first.

### Notes

- Only files matching the chosen extensions are included.
- Binary or unsupported files are skipped.
- Large repositories can produce very large Markdown files; consider narrowing `--ext` and using `--ignore` for speed and size control.

## Markdown to PDF

Convert a Markdown file to PDF using `pandoc` → HTML → Chrome headless printing.

### Script

`markdown-to-pdf.js`

### Usage

```bash
node /Users/administrador/P/opensource/ripple/ripple-docs/scripts/markdown-to-pdf.js <input.md> [--out <output.pdf>] [--css <stylesheet.css>] [--margin <size>] [--margin-top <size>] [--margin-right <size>] [--margin-bottom <size>] [--margin-left <size>]
```

### Options

- **--out**: Output PDF path. Default: same as input with `.pdf` extension.
- **--css**: Optional print CSS file. If not provided, a default print CSS is auto-generated to reduce margins and wrap long code lines.
- **--margin**: Set all page margins (e.g., `10mm`, `1cm`, `0.5in`, `12px`). Default: `10mm` per side.
- **--margin-top/--margin-right/--margin-bottom/--margin-left**: Override individual margins.
- **-h / --help**: Show help.

### Examples

```bash
node /Users/administrador/P/opensource/ripple/ripple-docs/scripts/markdown-to-pdf.js /Users/administrador/P/opensource/ripple/ripple-docs/ripple-first-commit.md
node /Users/administrador/P/opensource/ripple/ripple-docs/scripts/markdown-to-pdf.js ./README.md --out ./README.pdf
node /Users/administrador/P/opensource/ripple/ripple-docs/scripts/markdown-to-pdf.js ./notes.md --css ./print.css
node /Users/administrador/P/opensource/ripple/ripple-docs/scripts/markdown-to-pdf.js ./big.md --margin 8mm
node /Users/administrador/P/opensource/ripple/ripple-docs/scripts/markdown-to-pdf.js ./big.md --margin-top 6mm --margin-right 6mm --margin-bottom 10mm --margin-left 6mm
```

### Notes

- Requires `pandoc` and Google Chrome (or Chromium) for headless printing.
- A default print CSS is injected when `--css` is not provided. It sets `@page { margin: 10mm }`, removes body padding, and forces line wrapping for code blocks and tables to avoid content being cut off.

### Troubleshooting

- **Large left margin or content cut off**: Use `--margin 8mm` (or smaller) or per-side flags; ensure the fallback uses wkhtmltopdf (installed) so margins are applied. Provide a custom CSS with your own `@page` and `pre/code` rules if needed.
- **Code lines not wrapping**: Provide a CSS with:

```css
pre { white-space: pre-wrap; word-break: break-all; overflow-wrap: anywhere; }
pre > code { white-space: inherit; }
code { white-space: pre-wrap; word-break: break-all; overflow-wrap: anywhere; }
table { table-layout: fixed; }
td, th { word-wrap: break-word; }
```

- **Chrome not found**: Install Google Chrome or Chromium; the script auto-detects common binaries.
- **Pandoc reader errors (e.g., Unknown reader: gfm)**: The script automatically falls back to a supported reader.


