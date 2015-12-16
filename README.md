# transmorphic
A Morphic centered around immutable view descriptions, but still allowing
the programmer to evolve applications through direct manipulation.

Transmorphic takes Morphic's Halo concept and applies
it to views written in a purely immutable fashion by mapping
direct manipulation to changes in Code instead of State.

Immutability is one of the core strengths of functional programming,
since it enforces explicit reasoning about the evolvement of state.
This reduces the complexity of object interactions, prevents the introduction
of transient bugs and makes the behavior of functions more predictable and
easier to verify.

Facebooks [React](https://github.com/facebook/react), [Flux](https://github.com/facebook/flux) and [Relay](https://github.com/facebook/relay) are prominent examples that derive most of their strength from
immutable state.
While the departure from mutable state in views provides various benefits, it also takes
away the ability to model visual elements close to their "real world" counterparts.
Visual elements are merely projections of data, no longer first class entities the evolve
by themselves.

Contrary to that, the Morphic framework, first implemented in Self, and nowadays still widely used in Environments
such as Squeak or Lively, combines state and visual representation within the same object.
This enables the programmer to conceive visual applications by tinkering with different visual
parts, assembling them together, manipulating behavior and finally share the result with others.
A view is not derived from the model, but can also be conceived from its visual parts. 

The Halo is one of the core mechanisms in Morphic, that allows the programmer to decompose
applications and combine different parts in the visual domain. It is a very
powerful tool for both analyzing and conceving applications since it combines the
"what can be observed" with "what actually is" of the application.

