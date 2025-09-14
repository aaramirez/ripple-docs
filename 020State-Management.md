# State management

- **Purpose**: State is the data that changes over time and affects how components are rendered. State management provides a predictable way to update and access state across different components, preventing bugs as the app grows.

## Principles

- **Single source of truth**: For any given concern, have one authoritative state owner.
- **Co-locate by default**: Keep state as close as possible to where it’s used; lift only when needed.
- **Immutability**: Prefer immutable updates to simplify change detection and enable time-travel/debugging.
- **Explicit flows**: Make state transitions explicit and type-safe (reducers, events, actions).
- **Derive, don’t duplicate**: Compute derived data from primitives; avoid duplicating state.

## Mechanisms

- **Hooks (React/Next.js)**: `useState`, `useReducer` for local state; Context for scoped sharing.
- **Built-in reactivity (Svelte/Solid)**: Signals/stores trigger updates automatically; derived/computed values express dependencies.
- **Services/DI (Angular)**: Injectable services hold shared state and business logic; components subscribe via RxJS.

## Local vs shared state

- Keep state local when it only affects one component subtree.
- Promote to shared (context/store/service) when multiple distant consumers need it or when cross-cutting concerns emerge (auth, theme, feature flags).
- Treat server cache (e.g., queries) as a separate tier from UI state.

## Immutability and updates

- Use structural sharing (spreads, utilities) for predictable updates.
- Encapsulate mutations in reducers/actions or dedicated setters (signals/stores).
- Prefer narrow updates (fields/items) over wholesale object replacement when possible for performance.

## Derivations and selectors

- Centralize expensive computations with memoized selectors/derived stores.
- Expose selectors instead of raw state shape to reduce coupling.

## Effects and async

- Keep side effects out of pure update paths; trigger them in effects/subscriptions reacting to state changes.
- Model async with explicit states (idle/loading/success/error) and include metadata (timestamps, errors, retries).
- Cancel stale requests on dependency change; debounce user-driven updates where appropriate.

## Performance

- Minimize re-renders by scoping providers/stores narrowly.
- Split large state into smaller atoms/slices; subscribe only to what’s needed.
- Use memoization for selectors and computed values.

## Persistence and caching

- Distinguish between UI state (ephemeral), app state (navigable), and server cache (remote source of truth).
- Persist only what’s necessary (e.g., user prefs) with clear versioning and migration.

## Framework-specific guidance

- **React/Next.js**:
  - Local: `useState`/`useReducer`; share via Context sparingly to avoid broad invalidation.
  - Server data: prefer a fetch/cache library (e.g., React Query/TanStack Query) over hand-rolled global stores; keep mutations colocated with queries.
  - Global: if needed, use Redux Toolkit/Zustand/Jotai with typed slices/atoms and memoized selectors.
  - Avoid storing derived view state in global stores; compute in component/selectors.
- **Svelte**:
  - Use `writable`, `readable`, and `derived` stores; avoid keeping both raw and derived values.
  - Prefer store composition (derived) over manual subscriptions in components.
- **Solid.js**:
  - Use `createSignal`/`createStore` for fine-grained updates; derive with `createMemo`.
  - Avoid object mutations that bypass dependency tracking; update via setters.
- **Angular**:
  - Use services + RxJS for shared state; expose `Observable`/`Signal` interfaces, hide subjects internally.
  - For complex domains, use NgRx with feature slices, actions, reducers, and selectors.

## Anti-patterns

- Duplicating state instead of deriving it.
- Overusing global stores for local concerns.
- Triggering effects in reducers/pure update paths.
- Storing non-serializable data in global state (breaks persistence/time-travel).
- Prop drilling through many layers instead of introducing a scoped provider.

## Checklist

- State has a clear owner and scope (local/shared/server cache).
- Updates are immutable and type-safe.
- Derived data computed via selectors/derived stores.
- Async modeled with explicit statuses; stale work is cancelled.
- Re-render scope minimized; subscriptions are narrow.
- Persistence is intentional and versioned.