# Routing

- **Purpose**: Enable navigation between views without full page reload, while preserving state and performance.

## Principles

- **Declarative routes**: Define routes as data; avoid imperative push/replace scattered across code.
- **URL is state**: Reflect navigable state in the URL (path, query, hash) for shareability and reload safety.
- **Accessibility**: Manage focus on navigation; announce route changes for assistive tech.
- **Performance**: Preload likely next routes; avoid blocking data waterfalls.

## Patterns

- **File‑system routing**: Prefer conventions (e.g., Next.js) for simplicity and discoverability.
- **Nested routes/layouts**: Compose pages from nested areas that can update independently.
- **Data loaders**: Fetch data per route (server components/loaders) to align data with rendering.
- **Transitions**: Preserve UI responsiveness with route transition indicators and deferred non‑critical work.

## SSR/SSG and hydration

- **SSG**: Pre‑render static routes; use incremental static regeneration where appropriate.
- **SSR**: Render the shell and critical data on the server to improve FCP/TTFB.
- **Streaming**: Stream route HTML progressively for faster interactivity.

## Client‑side behavior

- **Link prefetch**: Prefetch assets for links in viewport or on hover.
- **Scroll & focus**: Restore scroll and move focus to main content on navigation.
- **Error boundaries**: Isolate route-level errors; show friendly fallbacks.

## Internationalization (i18n)

- **Locales in URL**: Encode locale in path or domain; avoid ambiguous state.
- **Static + dynamic**: Pre-generate common locale routes; lazily load less common ones.

## Security

- **Param validation**: Validate and sanitize route params and query before use.
- **Auth guards**: Protect private routes; redirect unauthenticated users predictably.

## Framework‑specific notes

- **Next.js**: Use app‑router with server components; co‑locate `loading`, `error`, and `layout` per segment. Use `Link` for prefetching and accessibility.
- **React Router**: Use data routers with loaders/actions; leverage nested routes and `Suspense`.
- **Angular**: Use `RouterModule` with guards/resolvers; lazy‑load feature modules.
- **SvelteKit**: Use `+page`/`+layout` and `load` functions; leverage adapters for SSR/SSG.

## Anti‑patterns

- Global `window.location` mutations scattered across the app.
- Deep linking disabled or state hidden outside the URL.
- Blocking navigations on slow data without progress feedback.
- Losing scroll/focus on navigation.

## Checklist

- Routes are declared and discoverable (prefer convention‑based).
- URL reflects the meaningful state; deep links work.
- Prefetching enabled for likely next routes.
- Focus and scroll restored on navigation; a11y announcements in place.
- Route‑level errors isolated behind error boundaries.