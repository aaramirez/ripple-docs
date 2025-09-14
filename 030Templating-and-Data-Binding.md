# Templating and data binding

- **Purpose**: Binds dynamic data (state) to the UI so the UI updates automatically when data changes.

## Principles

- **Clarity over cleverness**: Keep templates readable; prefer simple expressions over deeply nested logic.
- **Unidirectional data flow**: Prefer one-way binding from state to view; use two-way binding only for forms and controlled inputs.
- **Escape by default**: Interpolate text safely; treat HTML injection as opt-in and audited.
- **Key dynamic lists**: Provide stable keys/trackers to avoid DOM churn.

## Patterns and mechanisms

- **JSX (React/Next.js, Solid)**: Write HTML-like markup in JavaScript; expressions embed state and computed values.
- **Template syntax (Angular, Svelte)**: Use directive-style bindings and control-flow blocks in markup.
- **Binding types**:
  - **One-way binding**: State → view (most content rendering).
  - **Event binding**: View → state (handlers like `onClick`, `(click)`).
  - **Two-way binding**: View ⇄ state (forms). Use selectively to avoid hidden flows.
- **Control flow**:
  - Conditional display (`if`/`?:`/`*ngIf`/`{#if}`/`<Show>`)
  - List rendering with stable keys (`map` with `key`, `*ngFor trackBy`, `{#each ... (key)}`, `<For each>`)
- **Slots/children**: Compose UIs via slots/children rather than hardcoding structure.

## State interpolation and escaping

- Interpolate plain text safely; avoid concatenating raw HTML.
- For raw HTML needs, use framework-specific escape hatches (`dangerouslySetInnerHTML`, `[innerHTML]`) with trusted input only.

## Forms and two-way binding

- Prefer controlled inputs or explicit change handlers to keep data flow visible.
- Validate and sanitize at the boundary; debounce expensive validations.

## Performance

- Keep expressions cheap; move heavy computation to memoized selectors/computed values.
- Always key list items; avoid re-creating functions in hot paths when it affects diffing.
- Favor compile-time templating (Svelte) or fine-grained updates (Solid) for very dynamic UIs.

## SSR/SSG and hydration

- Ensure templates render identically on server and client to avoid hydration mismatches.
- Avoid accessing `window`/DOM in render paths; defer to effects.

## Accessibility (a11y)

- Use semantic elements; bind `aria-*` and `role` attributes appropriately.
- Ensure labels are connected to inputs; announce dynamic updates when needed.

## Framework-specific guidance

- **React/Next.js**:
  - JSX expressions are JavaScript; keep them pure and side-effect free.
  - Use fragments and short-circuiting for simple conditionals; extract complex logic to helpers.
  - For HTML injection, limit `dangerouslySetInnerHTML` to trusted content.
- **Angular**:
  - Use `*ngIf`, `*ngFor` with `trackBy` for lists; prefer the `async` pipe for observables.
  - Bind with `[prop]` and `(event)`; reserve `[(ngModel)]` for form fields.
- **Svelte**:
  - Use `{#if}`, `{#each}`, `{#await}` blocks; prefer `bind:` for form elements when appropriate.
  - Keep reactive declarations (`$:`) out of markup if complex; compute in script.
- **Solid.js**:
  - JSX plus control-flow components like `<Show>` and `<For>`; rely on signals for updates.
  - Avoid creating arrays of JSX in hot paths without keys; prefer `<For>`.

## Anti-patterns

- Logic-heavy templates that obscure intent.
- Unkeyed dynamic lists leading to flicker and state loss.
- Blanket two-way binding that hides data flow and causes surprising updates.
- Unsafe HTML injection from untrusted sources.
- Side effects inside template expressions.

## Checklist

- Bindings are one-way by default; two-way used only where warranted.
- Lists are keyed/trackBy’d; conditionals are simple and readable.
- Expensive computations are memoized or moved out of templates.
- No direct DOM/window usage during render; SSR-safe.
- a11y attributes and labels are present where needed.