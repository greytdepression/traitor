# traitor

A small public-domain trait framework for zig

This software is in the public domain for jursidictions in which the public
domain exists. Alternatively, it is available under the Zero-Clause BSD
license.

## Quick Start
Zig's standard library already has trait helpers in the form of std.meta.trait that
lets you query a type for declarations and fields by name.
Traitor works in a more Rust inspired way. You define a trait by building a struct
that contains all the desired fields, declarations, and functions.
```zig
const GraphTrait = struct {
    pub const Directed = false;

    num_vertices: usize,

    // This is just a dummy implementation used to obtain the function signature.
    // Traits are only meant to be a compile time construct and not used at runtime.
    pub fn hasEdge(i: usize, j: usize) bool {
        _ = j; _ = i; unreachable;
    }
};
```
*Note that (function) declarations must be marked `pub` in order for traitor to see them.*
Now if you want to check another type against those trait bounds, you can call
`traitor.checkTrait(GraphTrait, MyOtherType)`. Traitor will then check that `MyOtherType` also
has a declaration `Directed` of type `bool`, a field `num_vertices` of type `usize`, and a function
`hasEdge` taking two `usize` and returning a `bool`.
This is intended to be used in generic functions, e.g.
```zig
fn dijkstra(graph: anytype, start: usize, end: usize) -> Path {
    comptime traitor.checkTrait(GraphTrait, @TypeOf(graph));

    // code here
}
```
Make sure to call `checkTrait` with the `comptime` keyword to ensure that the zig compiler does not
print additional compile errors that arise from your code trying to access missing declarations or
fields if the trait bounds are not met.

Additionally, traitor allows you to add some meta data to your traits. Any declaration whose identifier
starts with `__traitor` will be skipped when checking trait bounds. Currently, the only meta data traitor
recognizes is `__traitor_trait_name` which can be set to a string (literal) and overrides the name
traitor will call the trait in error messages.
```zig
const GraphTrait2 = struct {
    pub const __traitor_trait_name = "Graph II";
    // ...
};
```