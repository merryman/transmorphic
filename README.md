# transmorphic
A Morphic centered around immutable view descriptions, while still allowing
the programmer to evolve applications through direct manipulation.
Transmorphic takes Morphic's Halo concept and applies
it to views written in a purely immutable fashion by mapping
direct manipulation to changes in Code instead of State.

## Why a "functional" Morphic?
Immutability is one of the core strengths of functional programming,
since it enforces explicit reasoning about the evolvement of state.
This reduces the complexity of object interactions, prevents the introduction
of transient bugs, makes the code more declarative and the behavior of functions more predictable and
easier to verify.
Facebooks [React](https://github.com/facebook/react), [Flux](https://github.com/facebook/flux) and [Relay](https://github.com/facebook/relay) are prominent examples that derive most of their strength from
pushing mutation of data to the border of the application.
While the departure from mutable state provides various benefits, it also takes
away the ability to model visual elements close to their "real world" counterparts.
Visual elements are merely projections of data, no longer encapsulating state that can
evolve by itself.
Contrary to that, the Morphic framework (used for example in the [LivelyKernel](https://github.com/LivelyKernel)), 
combines state and visual representation into the same object.
This enables the programmer to conceive visual applications by tinkering with different visual
parts, assembling them together, manipulating behavior and finally share the result with others.
A view is not derived from the model, but can also be conceived from its visual parts.

## Why tinkering is great
Once we find a great abstraction in programming, it usually serves two purposes: One is
to improve the maintainability and also correctness of the code we have actually written.
This is mainly why functional programming languages and ML like type systems are becoming
so popular recently, as many have discovered that functional abstractions (once found) make it far easier
to enforce correct behavior inside an application.
The other component, is about how the mind of the programmer is able to connect to the
application and its symbolic description. This is of great importance when we still
need to actually find an abstraction. Creating applications by assembling parts as if they
were first class objects, is still one of the most intuitive ways to build new stuff of
existing things.
