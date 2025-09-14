# Components

## Components

- **Purpose**: The fundamental unit of all these frameworks is the component. Components are reusable, self-contained building blocks that encapsulate a piece of the UI along with its own logic, state, and styling.
- **Example**: A "Like" button, a user profile card, or a navigation bar would all be individual components.

## Philosophy

- **Single responsibility**: Each component does one thing well. Prefer smaller, focused components over large, multipurpose ones.
- **Composition over inheritance**: Build features by composing components instead of deep class hierarchies.
- **Predictable data flow**: Favor unidirectional data flow. Inputs come in via props/attributes; outputs are emitted via events/callbacks.

## Structure and granularity

- **Granularity**: Keep components small enough to be understandable at a glance (roughly 50–200 lines of core logic is a good heuristic, excluding markup/styles).
- **Folder layout**: Co-locate a component’s view, logic, styles, and tests under one directory.
- **Naming**: Use clear nouns for components (e.g., `UserCard`, `LoginForm`).

## State management

- **Local vs shared**: Keep state local by default. Lift state only when multiple components require the same source of truth.
- **Controlled vs uncontrolled**: Prefer controlled components for form elements in React-like systems; allow uncontrolled only for simple cases or performance.
- **Immutability**: Treat state as immutable values to simplify change detection and rendering.
- **Side effects**: Isolate effects (subscriptions, timers, network) in lifecycle/effect primitives; clean them up predictably.

## Data flow and APIs

- **Inputs (props/attributes)**: Keep props minimal, well-typed, and descriptive. Provide sensible defaults; avoid optional booleans when an enum conveys intent.
- **Outputs (events/callbacks)**: Emit explicit events for user actions; avoid reaching into parents. Name callbacks consistently (e.g., `onSubmit`, `onChange`).
- **Context/injection**: Use context/DI sparingly to avoid prop drilling. Never hide critical data flows behind global singletons.

## Rendering and performance

- **Pure rendering**: Make render output a pure function of inputs and local state.
- **List keys**: Provide stable keys for dynamic lists.
- **Memoization**: Memoize expensive computed values and stable handlers; avoid premature optimization.
- **Virtualization**: For large lists, virtualize to reduce DOM cost.
- **Code splitting**: Lazy-load non-critical components; provide fallbacks for SSR.

## Styling

- **Encapsulation**: Prefer co-located styles. Choose one approach per project (CSS Modules, CSS-in-JS, Tailwind, or scoped styles) and stay consistent.
- **Theming**: Use CSS variables or a theme context to support dark mode and customization without coupling components to global CSS.
- **Sizing/layout**: Keep components layout-agnostic when possible; accept className/style hooks instead of baking layout decisions inside.

## Accessibility (a11y)

- **Semantics first**: Use semantic elements and ARIA only when necessary.
- **Keyboard**: Ensure tab order, focus states, and keyboard interactions work.
- **Labels**: Provide accessible names/labels and associate inputs with labels.
- **Announcements**: Use `aria-live` for async status; manage focus on route/dialog changes.

## Composition and reuse

- **Slots/children**: Expose composition points (slots/children) instead of hardcoding structure.
- **Headless patterns**: Consider headless components for logic (state + behaviors) with render-as-you-like UIs.
- **Variants**: Model component variations via props (e.g., `variant="primary"`), not separate components, unless behavior diverges significantly.

## Testing and documentation

- **Unit tests**: Test behaviors and contracts (props in → DOM/events out). Avoid testing implementation details.
- **Stories/examples**: Provide examples for common states (empty, loading, error, long content, RTL).
- **Types**: Fully type public props and events. Prefer discriminated unions over booleans where intent matters.

## Error handling

- **Boundaries**: Wrap complex trees with error boundaries (React) or equivalent patterns to contain failures.
- **User feedback**: Show actionable messages; avoid silent failures.

## Framework-specific guidance

- **React/Next.js**:
  - Use function components. Keep hooks at top level; follow the Rules of Hooks.
  - Memoize handlers with `useCallback` only when passing to memoized children or DOM listeners frequently.
  - For SSR/SSG, avoid relying on `window` during render; guard with effect or dynamic import.
- **Svelte**:
  - Leverage built-in reactivity; prefer `$:` reactive declarations and `stores` for shared state.
  - Avoid unnecessary derived computations in markup; compute once in script.
- **Solid.js**:
  - Prefer signals for fine-grained updates. Avoid object mutation that hides dependencies; derive with `createMemo`.
  - Components are setup functions; keep heavy work outside reactive contexts.
- **Angular**:
  - Use `OnPush` change detection where practical. Prefer `@Input()`/`@Output()` for data flow and services for cross-cutting concerns.
  - Keep templates declarative; extract logic to components/services.

## Anti-patterns to avoid

- **God components**: Mixing data fetching, business logic, and complex layout in one component.
- **Prop drilling**: Threading the same values many levels deep; prefer context/DI with clear boundaries.
- **Hidden coupling**: Reading from global singletons in render paths.
- **Unstable keys**: Using indices as keys for reorderable lists.
- **Booleans everywhere**: Use enums for mutually exclusive visual states.

## Checklist before merging

- Inputs and outputs are clearly typed and documented.
- No prop drilling without justification; consider context.
- A11y: keyboard, focus, labels, roles checked.
- Performance: keys set; avoid unnecessary re-renders; large lists virtualized.
- Tests and story/examples cover key states.
- Styles are encapsulated; theming supported via tokens/vars.
- Errors handled gracefully; boundaries in place where needed.
