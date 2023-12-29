# Went: PureScript diagrams in GoJS

## What is it?
Went is a wrapper library over [`purescript-gojs`](link-to-purescript-gojs), a library containing bare bindings to [GoJS](link-to-gojs). It supports defining diagrams declaratively with a monadic DSL that looks similar to GoJS' `make` functionality.

## Why is it?
The motivation for Went is twofold:
1. To support writing GoJS diagrams in PureScript applications. (duh)
2. As long as we're doing that, to address shortcomings of JavaScript and TypeScript via added type safety. For instance, `Panel`s in GoJS that are of type `Auto` should not have their `isOpposite` property ever set - this is not currently expressible in that library, whereas in Went it is, by moving as much information as possible to the type level.

## Example
Consider the following typical GoJS code:
```typescript
function nodeStyle() {
  return [
    new go.Binding("location", "loc", go.Point.parse).makeTwoWay(go.Point.stringify),
    {
      locationSpot: go.Spot.Center
    }
  ];
}
function textStyle() {
  return { 
    font: "bold 11pt Lato, Helvetica, Arial, sans-serif",
    stroke: "#F8F8F8"
  }
}
const $ = go.GraphObject.make;
const myNode = $(go.Node, "Table", nodeStyle(),
  $(go.Panel, "Spot",
    $(go.Shape, "Circle",
      { desiredSize: new go.Size(70, 70), fill: "#282c34", stroke: "#09d3ac", strokeWidth: 3.5 }),
    $(go.TextBlock, "Start", textStyle(),
      new go.Binding("text"))
  ))
```
The `make` function is variadic and takes many different kinds of arguments: JavaScript class-inheriting objects, plain objects/records, arrays, strings, `Binding`s, etc. 

In Went, the "variadicity" corresponds to monadic actions, and this code translates to the following:
```purescript
nodeStyle = do
  binding @"location" @"loc" (Just Point.parse_) (Just Point.stringify_)
  set { locationSpot: Spot.center }
textStyle = set
  { font: "bold 11pt Lato, Helvetica, Arial, sans-serif"
  , stroke: "#F8F8F8"
  }
myNode = node @Table' $ do
  nodeStyle
  panel @Spot' $ do
    shape Circle $ do
      set { desiredSize: SizeBoth 70.0, fill: "#282c34", stroke: "#09d3ac", strokeWidth: 3.5 }
    textBlock "Start" $ do
      textStyle
      binding1 @"text"
```
There's several things to notice:
1. `"Table"` and `Spot` have gone from plain strings passed to calls to `make` in the context of making a `Node` and a `Panel` respectively to *types* which are applied to the functions `node` and `panel`. This allows us to define row types in the implementation of each of these constructors' that respect certain sets of fields and not others.
2. `Shape`'s argument went from a string to a sum type.
3. Plainly passing a record to `make` corresponds to calling the function `set` with that record as an argument.
4. The arguments to the creation of `Binding` objects are now function calls; whereas the constructor `Binding()` in GoJS expects strings as its arguments, `Went`'s version expects *type-level* strings (denoted by the `@` symbol before the string). This allows us to only `bind` *actual properties* to *actual fields of a `Model`'s `nodeDataArray`'s elements*.

Several more examples can be found in [this repo](link-to-purescript-went-example).

## Shortcomings
1. Not everything can be declarative. GoJS diagrams can be customized extensively. One of the ways this is done is by passing functions as properties of certain objects, an example is `mouseEnter` of `GraphObject` objects. Because these fields are sent to the FFI, they must run in the plain `Effect` monad and therefore rely on `purescript-gojs` to perform their logic, where Went's interface and added type safety is not present, and the code is necessarily very imperative.
2. Bundle sizes are currently quite large, especially when used with frameworks like Halogen. This is a [general problem in PureScript](link-to-bundle-sizes)
3. Type errors, especially due to type inference, can be mysterious: when in doubt, try to give type annotations and use the PureScript compiler's built-in linting to generate them.
4. Often when dealing with `purescript-gojs`-related code, types MUST be given explicitly, usually when a function is polymorphic in its output type. This is somewhat solved by existential types (see the WIP section) but not completely.


## WIP:
*This library is in an experimental phase*. We're still ironing out several of its details and filling them in.

- "Monadic" fields setting - for example, creating a GraphObject or an Adornment for a Shape or a Part (examples: pathPattern and selectionAdornmentTemplate) is not supported yet; only "plain" fields (numbers, booleans, sum types etc). This can already go quite a long way however.
- Inheriting from classes can be modeled via modifying the behavior of its methods with the `Override` construct, but if an intended subclass has extra fields for example, then it will need its own `Settable` instance and possibly `IsPanel`, `IsNode` etc. GoJS only contains examples of extending Tools, Layouts and Links, but in principle any class can be inherited from since Typescript is object-oriented - our aim (at least with this release) is to support the most common use cases.
- Several fields/methods from Diagram (and therefore Overview and Palette) are not yet supported; in particular, Diagram's `attach` method (whose argument can also be given as an argument to the constructor) supports many type-checked deeply nested properties.
- Some fields use existential types to address the fact that many GoJS functions return heterogeneous data structures - for example, an array of type `Node[]` can contain both `Node`s and `Group`s. This is a common conflict between pure FP and OO approaches; one might want a collection of different types that have one thing in common: they implement a certain interface. This is sometimes an [antipattern](link-to-existential-antipattern), and deserves looking at.
- Several fields are not polymorphic enough; i.e. they do not use the existential approach delineated above.
- There's a lot of documentation missing (prototypes, overrides, the different `Make*` monads), but hopefully the extensive [GoJS docs](link-to-gojs-docs) can fill in a lot of the missing pieces.


Not supported:
1. Defining panel layouts
2. Extending Links, LayoutNetworks
3. Nested records in Settable instances must be filled out totally
4. Diagram's settable instance is not type safe, it accepts anything due to us wanting to nest sets very deeply sometimes

5.  boundsComputation in Layout and other fields use existential types, others use concrete types. Need to decide