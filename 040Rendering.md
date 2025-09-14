# Rendering mechanism

- **Purpose**: To efficiently update the Document Object Model (DOM) when the state changes, a process that can be expensive.

## Principles

- **Determinism**: Render output is a pure function of inputs and state.
- **Minimize work**: Update the fewest DOM nodes possible; avoid full re-renders.
- **Locality of updates**: Propagate changes only to affected subtrees.
- **Stable identity**: Use stable keys/identifiers to avoid destroying and recreating DOM unnecessarily.
- **Prioritize UX**: Schedule rendering so interactions remain responsive.

## Strategies (by framework family)

- **Virtual DOM (React/Next.js)**: Compute a new tree and diff with the previous one to decide minimal DOM operations.
- **Compilation (Svelte)**: Compile templates to direct DOM operations; no VDOM at runtime.
- **Fine‑grained reactivity (Solid.js)**: Track dependencies at the node/computation level; update only the exact bindings that changed.

## Advanced techniques

- **Incremental rendering & streaming**: Send HTML progressively (e.g., Next.js streaming/Suspense) to reduce TTFB and improve perceived performance.
- **Partial hydration/Islands**: Hydrate only interactive parts of a page; leave static HTML untouched.
- **Transitions & scheduling**: Defer non‑urgent updates to keep input responsive (e.g., React transitions).
- **Memoization**: Cache expensive computations and component outputs when inputs are stable.

## Performance guidelines

- **Keys for lists**: Always provide stable keys for dynamic lists and repeated fragments.
- **Batching**: Prefer batched state updates to reduce reflows and re-renders.
- **Avoid layout thrashing**: Group reads, then writes; minimize sync layout reads (offset/scroll/measure) between writes.
- **Virtualize large lists**: Render only what’s visible; windowing for thousands of rows.
- **Memoize and split**: Memoize subtrees; split large components into smaller ones with narrower update scope.
- **Images/assets**: Use responsive images, lazy loading, and caching; avoid layout shifts by reserving space.

## SSR/SSG and hydration

- **Markup parity**: Ensure server HTML matches client expectations to avoid hydration mismatches.
- **Hydration boundaries**: Hydrate only where necessary; consider streaming and islands for large pages.
- **Data strategy**: Align data fetching with render phases (server components/loader APIs) to reduce waterfalls.
- **Progressive enhancement**: The server-rendered page should be usable before hydration completes.

## Framework‑specific guidance

- **React/Next.js**
  - Use stable `key`s in lists; avoid array indices for reorderable items.
  - Use `React.memo`/`useMemo`/`useCallback` to avoid recomputation and re-renders when props/state are stable.
  - Use `useTransition`/`useDeferredValue` to keep interactions responsive during heavy updates.
  - Prefer server components and streaming where applicable; avoid client-only rendering for static content.
  - Do not read layout synchronously during render; measure in effects.

- **Svelte**
  - Leverage compile-time reactivity; avoid unnecessary derived computations in markup.
  - Use keyed `{#each item (key)}` for stable identity; avoid unkeyed lists when order can change.
  - Move heavy computations out of the template; compute once in `<script>` and bind results.

- **Solid.js**
  - Prefer signals and `createMemo` for precise updates; avoid object mutations that hide dependency changes.
  - Use control‑flow primitives (`<For>`, `<Show>`) instead of ad‑hoc array maps for hot paths.
  - Split props and isolate reactive scopes to minimize recomputation.

## Anti‑patterns

- Unkeyed dynamic lists causing DOM churn and state loss.
- Reading and writing layout in the same tick (layout thrashing).
- Recomputing large trees due to unstable props (new object/array/function each render) without memoization.
- Global mutable state read during render that changes outside the reactive graph.
- Hydration mismatches from conditional markup differences server vs client.

## Checklist

- Stable keys provided for all dynamic lists and fragments.
- Render logic is pure; side effects run in lifecycle/effect phases.
- Heavy computations are memoized or moved out of render paths.
- Large lists are virtualized; images and assets are optimized.
- No layout thrashing; reads and writes are separated and batched.
- SSR markup parity verified; hydration errors are zero.