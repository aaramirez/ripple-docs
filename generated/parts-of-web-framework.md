# Core building blocks

## Components

- **Purpose**: The fundamental unit of all these frameworks is the component. Components are reusable, self-contained building blocks that encapsulate a piece of the UI along with its own logic, state, and styling.
- **Example**: A "Like" button, a user profile card, or a navigation bar would all be individual components.

## State management

- **Purpose**: State is the data that changes over time and affects how components are rendered. State management provides a predictable way to update and access state across different components, preventing bugs as the app grows.
- **Mechanisms**:
  - **Hooks**: React uses hooks like `useState` and `useReducer` to manage component-level state.
  - **Built-in reactivity**: Svelte and Solid have built-in reactivity, so variables automatically trigger updates when their values change. Svelte uses "writable stores" for sharing state between components.
  - **Services**: Angular uses services and dependency injection to manage state and share logic throughout the application.

## Templating and data binding

- **Purpose**: Binds dynamic data (state) to the UI. When the data changes, the UI automatically updates to reflect the new state.
- **Mechanisms**:
  - **JSX**: React and Next.js use JSX, which allows you to write HTML-like markup directly inside JavaScript files.
  - **Templates**: Angular and Svelte use a template system with special syntax that allows developers to embed logic and state directly into HTML markup.

## Rendering and performance components

### Rendering mechanism

- **Purpose**: To efficiently update the Document Object Model (DOM) when the state changes, a process that can be expensive.
- **Mechanisms**:
  - **Virtual DOM (VDOM)**: Frameworks like React and Next.js use a VDOM—an in-memory representation of the actual DOM. When state changes, a new VDOM is created and compared to the old one. This "diffing" process identifies the most efficient way to update the real DOM, minimizing costly browser operations.
  - **Compilation**: Svelte takes a different approach by running at compile time rather than in the browser at runtime. It compiles components into highly optimized, vanilla JavaScript that directly and surgically updates the real DOM. This eliminates the need for a VDOM entirely.
  - **Fine-grained reactivity**: Solid.js also avoids a VDOM. It uses a signal-based system for fine-grained reactivity, where only the parts of the DOM dependent on a specific signal (reactive data) are updated.

## Bundlers and compilers

- **Purpose**: These tools process the raw component code into optimized, production-ready bundles that browsers can execute.
- **Example**: Compilers in Svelte and Solid, as well as build tools like Vite (which Next.js and others can use), perform this optimization.

## Routing

- **Purpose**: Routing allows navigation between different pages or views of an application without requiring a full page reload.
- **Example**: Next.js has a built-in file-system-based router, while React often relies on a third-party library like React Router.

## Next.js-specific components

- Next.js is a "full-stack" web framework built on top of React that requires several additional components for its server-side features.
- **Server-side rendering (SSR)**: Next.js can generate HTML on the server for each request. This is critical for improving performance and search engine optimization (SEO).
- **Static site generation (SSG)**: This allows developers to pre-render pages into static HTML at build time. This is ideal for content that does not change frequently, such as blog posts or documentation.
- **File-system routing**: Next.js uses a directory structure to define routes automatically, streamlining the routing process.
- **API routes**: In addition to handling the front-end, Next.js can create back-end API endpoints to handle server-side logic and data fetching. 
