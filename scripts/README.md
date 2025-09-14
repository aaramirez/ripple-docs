## Source Concatenation Script

Generate a single Markdown file containing the concatenated source code for a directory. Each file is listed with its path followed by a language-tagged code block.

### Script

`generate-source-md.js`

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


