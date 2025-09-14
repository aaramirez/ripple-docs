# Ripple documentation

## Overview

Ripple is a TypeScript UI framework for the web. It uses a dedicated `.ripple` module format that blends TypeScript with a JSX-like, statement-based template syntax and a signal-style reactivity system based on `$`-prefixed identifiers.

> Status: early development. Expect sharp edges; APIs may change.

## Features

- **Reactive state with `$`**: variables and object properties prefixed with `$` are tracked.
- **Statement-based templates**: JSX-like syntax used as statements inside `component`/`fragment` bodies.
- **TypeScript-first**: `.ripple` files support types; a JSX runtime is provided.
- **Scoped styles**: a single top-level `<style>` per component with automatic scoping and pruning.
- **Ergonomic events and DOM ops**: delegated events, controlled attributes, and efficient updates.
- **VSCode integration**: syntax highlighting, diagnostics, and virtual TS for tooling.

## Installation

```bash
pnpm i --save ripple
```

## Hello World

```ripple
component App() {
  <h1>{"Hello Ripple"}</h1>
}
```

Mount in your app using the runtime `mount()`:

```js
import { mount } from 'ripple';
import { App } from './App.ripple';

mount(App, { target: document.getElementById('root')! });
```

## Key Concepts

### Components

Components are declared with the `component` keyword. They accept props and contain template statements in their body. They do not return JSX.

```ripple
component Button(props: { text: string, onClick: () => void }) {
  <button onClick={props.onClick}>{props.text}</button>
}

// Usage
<Button text={"Click me"} onClick={() => console.log("Clicked!")} />
```

### Reactive variables

Prefix a variable with `$` to make it reactive. Object properties can be reactive the same way.

```ripple
let $count = 0;
let state = { $value: 1 };

// Triggers updates
$count++;
state.$value++;
```

Computed values are just expressions of reactive values:

```ripple
let $count = 0;
let $double = $count * 2;
```

When initializing from a reactive prop, use `untrack` to opt out of ongoing reactivity:

```ripple
import { untrack } from 'ripple';

component Counter({ $initial }) {
  let $count = untrack(() => $initial);
  <button onClick={() => $count++}>{$count}</button>
}
```

### Effects

Side effects run in response to reactive changes via `effect`:

```ripple
import { effect } from 'ripple';

component App() {
  let $count = 0;
  effect(() => {
    console.log($count);
  });
  <button onClick={() => $count++}>{"Increment"}</button>
}
```

### Templates (statement-based JSX)

Templates only appear inside `component` or `fragment` bodies and are statement-based. You can use native control flow such as `if` and `for...of` directly around template blocks.

```ripple
component Truthy({ x }) {
  <div>
    if (x) {
      <span>{"x is truthy"}</span>
    } else {
      const msg = "x is falsy";
      {msg}
    }
  </div>
}
```

Strings in templates must be wrapped in `{"..."}` (no bare JSX text).

### Props and reactivity

Make props reactive by prefixing them with `$` and read them as such:

```ripple
component Label(props: { $text: string }) {
  <label>{props.$text}</label>
}

<Label $text={some_text} />
``;
```

### Fragments

Fragments are reusable imperative template functions declared with `fragment` and rendered with `{@fragment ...}`.

```ripple
fragment Logo() {
  <svg>{"..."}</svg>
}

component Header() {
  <header>{@fragment Logo()}</header>
}
```

### Children

Children are provided as an implicit `$children` fragment prop.

```ripple
import type { Fragment } from 'ripple';

component Card(props: { $children: Fragment }) {
  <div class="card">{@fragment props.$children()}</div>
}

<Card>
  <p>{"Card content"}</p>
</Card>
```

### Styling

Each component may declare a single top-level `<style>` block. Styles are scoped and pruned to used selectors.

```ripple
component Box() {
  <div class="container"><h1>{"Hello"}</h1></div>

  <style>
    .container { padding: 1rem; background: cornflowerblue; }
    h1 { color: white; }
  </style>
}
```

## VSCode extension

The `ripple-vscode-plugin` provides:

- Syntax highlighting for `.ripple` files
- Diagnostics and errors with precise ranges
- IntelliSense via a virtual TS service

Install from `packages/ripple-vscode-plugin/` in this repo.

## Playground

Try Ripple locally via the playground:

```bash
pnpm i && cd playground && pnpm dev
```

Edit files under `playground/src` to experiment.

## Architecture (high level)

- **Compiler** (`packages/ripple/src/compiler`):
  - Parse (Acorn + custom plugin) → `parse()` builds a Ripple AST.
  - Analyze → scope creation, validations, event delegation analysis, CSS pruning setup.
  - Transform → generates JS/TS using `esrap`, build-time DOM template strings, and runtime calls.
- **Runtime** (`packages/ripple/src/runtime`):
  - Block scheduler and reactions, tracked and computed values, delegated events, DOM ops, control blocks (`if`, `for`, `try/async`).
  - Utilities like `array()`, `keyed()`, `Portal`, `flushSync`, `untrack`, `deferred`.
- **Styles**: CSS is parsed, selectors matched against elements discovered during analysis, then rendered; unused rules are commented or dropped.

## Limitations / Missing features

- Server-side rendering (SSR) not implemented yet.
- Limited tests and types in parts of the codebase; rapid iteration in progress.

## Links

- [Ripple Repo](https://github.com/trueadm/ripple)
- [DeepWiki](https://deepwiki.com/trueadm/ripple)
- [Core design principles - DeepWiki](https://deepwiki.com/search/which-are-the-principles-and-p_f1bc0b11-2914-4f5a-93d3-1708f0f6fd10)

## License

MIT
