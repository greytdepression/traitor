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
**Warning: the prefix `__traitor_internal` is reserved for internal meta data. Using it yourself may
result unforseen errors.**

## Error Codes
Traitor prints a two digit error code (e.g. `[E01]`) at the start of each error message. These can be
cross referenced here for a more detailed explanation of the kind of error traitor encountered.

### `[E00] Unknown`
This is the default error value and should never be encountered. If you see an `[E00]` error message,
traitor itself has a bug. Please file an issue about it :)

### `[E01] TraitNotAStruct`
This is an error with the trait you supplied, not the type that is supposed to be checked. Traits are
defined using structs (see above in the Quick Start guide). You or the library that you are using supplied
something different from a trait.
As an example, the following code will produce this error as `u32` is not a struct:
```zig
// [E01] The trait must be a struct but 'u32' is not.
traitor.checkTrait(u32, TypeToCheck);
```

### `[E02] TypeNotAStruct`
This is an error with the type you supplied. Similarly to the trait itself, the type that you check must
also be a struct. The following code will produce this error:
```zig
// [E02] The type implementing the trait must be a struct but 'u32' is not.
traitor.checkTrait(GraphTrait, u32);
```

### `[E03] DeclarationIncompatibleType`
This is an error with the type you supplied. The type you supplied contains a declaration that has the same
name as one of the required declarations of your trait, but it has the wrong type. With the graph example
above, the following implementation would cause this error:
```zig
const MyGraph = struct {

    // [E03] Declaration 'Directed' has the wrong type. Expected 'bool', found '*const [5:0]u8'.
    pub const Directed = "maybe";

    // ...
};
```

### `[E04] FunctionIncompatibleSignature`
This is an error with the type you supplied. The type you supplied contains a function declaration that has the
same name as one of the required function declarations of your trait, but it has the wrong signature. With the
graph example above, the following implementation would cause this error:
```zig
const MyGraph = struct {

    // [E04] Function 'hasEdge' has the wrong signature. Expected 'fn(usize, usize) bool', found 'fn(bool) void'.
    pub fn hasEdge(foo: bool) void {}

    // ...
};
```

### `[E05] FieldIncompatibleType`
This is an error with the type you supplied. The type you supplied contains a field that has the same
name as one of the required fields of your trait, but it has the wrong type. With the graph example
above, the following implementation would cause this error:
```zig
const MyGraph = struct {

    // [E05] Field 'num_vertices' has the wrong type. Expected 'usize', found 'bool'.
    num_vertices: bool,

    // ...
};
```

### `[E06] MissingDeclaration`
### `[E07] MissingFunction`
### `[E08] MissingField`
This is an error with the type you supplied. The type is missing a declaration/function declaration/field.
With the graph example above, the following implementation would cause this error:
```zig
// [E06] Missing declaration 'pub Directed: bool'.
// [E07] Missing function declaration 'pub fn hasEdge(usize, usize) bool {}'.
// [E08] Missing field 'num_vertices: usize'.
const MyGraph = struct {};
```

### `[E09] TraitMetaDataHasIncorrectType`
This is an error with the trait you supplied, not the type that is supposed to be checked. One of the
meta data declarations of the trait has the wrong type. You or the library that you are using supplied
added incompatible meta data.
As an example, the following code will produce this error as `__traitor_trait_name` must be a string.
```zig
const GraphTrait = struct {
    // [E09] The type of the trait's '__traitor_trait_name' declaration must be compatible with
    // '[]const u8', found 'bool' instead.
    pub const __traitor_trait_name = false;
};
```
