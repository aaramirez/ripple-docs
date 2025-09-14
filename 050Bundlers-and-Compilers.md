# Bundlers and compilers

- **Purpose**: Process raw source into optimized, production‑ready assets that browsers execute efficiently.

## Principles

- **Least work at runtime**: Prefer moving work to build time (precompilation, treeshaking) over runtime.
- **Incremental & fast dev**: Hot reload with near‑instant feedback; rebuild only what changed.
- **Deterministic output**: Stable chunk names/hashes for better caching and reproducible builds.
- **Standards first**: Align with ES modules, import maps, and modern browser capabilities.

## Roles

- **Compiler**: Transforms a high‑level language/template into JS/CSS/HTML (e.g., Svelte/Solid compilers, TS).
- **Bundler**: Resolves modules and produces chunks (Vite/Rollup/webpack/ESBuild).
- **Minifier**: Optimizes output via dead‑code elimination, mangling, compression (Terser/ESBuild).
- **Packager**: Emits assets, injects hashes, builds manifest, prerenders pages.

## Recommended stack

- **Vite + Rollup** for web apps and libraries (fast dev server, rich plugin ecosystem).
- **ESBuild** for extremely fast transformations (TS, JSX) and test pipelines.
- **SWC/Babel** only when specific transforms are needed (legacy targets, macros).

## Optimizations

- **Tree‑shaking**: Use ESM and `sideEffects` annotations; avoid dynamic require.
- **Code splitting**: Split by route and heavy feature areas; prefer dynamic import at boundaries.
- **Preload/Prefetch**: Emit `<link rel="preload|prefetch">` for critical and next‑hop resources.
- **CSS handling**: Extract critical CSS, code‑split CSS per route; purge unused styles.
- **Asset optimization**: Optimize images (responsive, WebP/AVIF), inline tiny assets, cache‑bust others.
- **Source maps**: Enable in development; generate separate production maps with restricted access.

## Server rendering integration

- **SSR/SSG**: Provide SSR entry points; configure externals to avoid bundling server deps unnecessarily.
- **Islands/partial hydration**: Produce islands as independent chunks to hydrate selectively.
- **Manifests**: Emit SSR manifests to map modules → chunks for HTML injection.

## DX and maintainability

- **Fast feedback**: Keep cold starts <1s and HMR <200ms.
- **Clear errors**: Surface compiler and bundler diagnostics with file/line context.
- **Type safety**: Integrate TS type‑checking separately from fast transpilation.
- **Consistent paths**: Use aliases (e.g., `@/`) sparingly and document them.

## Library output (if publishing)

- **Multiple formats**: Ship ESM and CJS (if required); prefer ESM‑only when possible.
- **Bundled vs external**: Mark peer deps as externals; avoid double‑bundling React/etc.
- **Types**: Publish `.d.ts` with accurate typings.

## Build performance checklist

- Remove unused polyfills; target modern browsers first.
- Avoid large JSON‑in‑code; load data at runtime or split.
- Limit asset inlining thresholds; large inlines bloat JS.
- Profile bundle size; set budgets and fail CI on regressions.

## Anti‑patterns

- Monolithic single bundle for the entire app.
- Extensive dynamic imports inside tight loops or hot paths.
- Relying on CommonJS for new code (breaks tree‑shaking).
- Shipping dev‑only helpers into production without dead‑code removal.

## Checklist

- ESM everywhere; `sideEffects` correctly set for packages.
- Route‑level code splitting with lazy/dynamic imports.
- CSS extracted, minimized, and split per route where useful.
- Images optimized; fonts subsetted; long‑term caching headers configured.
- Source maps generated appropriately; errors map back to source in dev.