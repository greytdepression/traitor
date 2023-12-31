# traitor

A small public-domain trait framework for zig

This software is in the public domain for jursidictions in which the public
domain exists. Alternatively, it is available under the Zero-Clause BSD
license.

## Quick Start
Zig's standard library already has trait helpers in the form of std.meta.trait that
let you query a type for declarations and fields by name.
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
result in unforseen errors.**

## Optional Fields and Declarations
Sometimes you might want to allow an implementation of a trait to choose to not implement a certain
field or declaration. For this case traitor allows you to hint to it that a certain field/declaration
is optional. It will not throw a compile error if the type implementing your trait does not have said
field/declaration, however, if a field/declaration of said name exists it will be type checked.

To mark a field/declaration as optional you simply wrap its type in `traitor.Optional`.
```zig
const Trait = struct {
    foo: traitor.Optional(usize),

    pub const Bar: traitor.Optional(type) = .{};

    pub const foobar: traitor.Optional(fn (usize, usize) bool) = .{};
};
```
This trait above is then e.g. implemented by the empty struct `struct {}` or by
```zig
const Type = struct {
    foo: usize,

    pub const Bar: type = void;

    pub fn foobar(i: usize, j: usize) bool {
        // code
    }
};
```

## Associated Types
One limitation that might become obvious pretty quickly is that defining methods is not possible
with the tools laid out above. Specifically, if we had a trait
```zig
const GraphTrait = struct {
    pub fn numEdges(self: GraphTrait) usize {
        // code
    }
};
```
then any type implementing `GraphTrait` would have to add a function `numEdges` that takes as input
parameter an instance of `GraphTrait` -- not of the type that implements it.
```zig
const MyGraph = struct {
    num_edges: usize,

    pub fn numEdges(self: GraphTrait) usize {
        // no access to the field `num_edges` :(
    }
};
```
To allow for this, traitor can handle 'associated types' that get substituted at comptime to the
relevant actual types.

### `GenericSelf`
The type `traitor.GenericSelf` will be substituted to whatever type that is being checked against trait
bounds. So in the above example, we could actually implement methods using `traitor.GenericSelf`:
```zig
const GraphTrait = struct {
    pub fn numEdges(self: traitor.GenericSelf) usize {
        return 0;
    }
};

const MyGraph = struct {
    num_edges: usize,

    pub fn numEdges(self: MyGraph) usize {
        return self.num_edges; // :)
    }
};
```
Since type substitution works recursively, this also works with compound types and we can thus even take pointers
to `self` and modify the struct.
```zig
const GraphTrait = struct {
    pub fn addEdge(self: *traitor.GenericSelf, i: usize, j: usize) void {
        // ...
    }
};

const MyGraph = struct {
    pub fn addEdge(self: *MyGraph, i: usize, j: usize) void {
        // code
    }
};
```

### `AssociatedType("AssociatedTypeName")`
We can use the same type substitution as in `traitor.GenericSelf` with arbitrary other associated types.
To add an associated type, first the trait needs to have a declaration of type `type` and whose name is
the name of the associated type.
```zig
const GraphTrait = struct {
    pub const Payload = usize;
};
```
Note that the types implementing your trait will be able to override the value
of the declaration (i.e. what type the payload will be in this example), but they will still need to supply
a `type` (e.g. they might use `Payload = []const u8` to store strings instead of integers).
After that, you can reference this associated type throughout your trait using `traitor.AssociatedType("<name of AT>")`.
```zig
const GraphTrait = struct {
    pub const Payload = usize;
    pub const DefaultPayload: traitor.AssociatedType("Payload") = undefined;

    some_node_payload: traitor.AssociatedType("Payload"),

    pub fn getPayload(self: traitor.GenericSelf, i: usize) traitor.AssociatedType("Payload") {
        // ...
    }
};

const MyGraph = struct {
    pub const Payload = []const u8;
    pub const DefaultPayload: []const u8 = "foobar";

    some_node_payload: []const u8,

    pub fn getPayload(self: MyGraph, i: usize) []const u8 {
        return self.some_node_payload;
    }
};
```


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

### `[E10] TraitIllegalUseOfTraitorInternalDecl`
This is an error with the trait you supplied, not the type that is supposed to be checked. The trait has
a declaration whose name starts with `__traitor_internal`. This prefix is reserved for internal mechanisms
and thus this declaration caused an error in the internal logic.
As an example, the following code will produce this error as `__traitor_internal_associated_type_decl_name`
is used internally.
```zig
const GraphTrait = struct {
    // [E10] Illegal use of `__traitor_internal_associated_type_decl_name` declaration in trait.
    pub const Foobar: struct {
        pub const __traitor_internal_associated_type_decl_name = 9;
    } = .{};
};
```

### `[E11] TraitMissingAssociatedTypeDeclaration`
This is an error with the trait you supplied, not the type that is supposed to be checked. The trait references
an associated type, but never declares it.
As an example, the following code will produce this error as `Payload` is never declared.
```zig
const GraphTrait = struct {
    // [E11] Expected declaration of associated type 'Payload' in trait.
    pub fn defaultPayload() traitor.AssociatedType("Payload") {
        // code
    }

    // To fix this, uncomment type following line and add a type
    // pub const Payload: type = ...;
};
```

### `[E12] TraitAssociatedTypeNotAType`
This is an error with the trait you supplied, not the type that is supposed to be checked. The trait references
an associated type, but the declaration is not of type `type`.
As an example, the following code will produce this error as `Payload` is an `i32` and not a `type`. It would
be correct to define `Payload = i32`.
```zig
const GraphTrait = struct {
    // [E12] Expected declaration of associated type 'Payload' to be of type 'type', got 'i32' instead.
    pub fn defaultPayload() traitor.AssociatedType("Payload") {
        // code
    }

    pub const Payload: i32 = 0;
};
```

### `[E13] TraitGenericTypeLayoutNotAuto`
This is an error with the trait you supplied, not the type that is supposed to be checked. The trait has a field
or declaration that has a type that makes use of an associated type internally while also having non-auto layout.
As an example, the following code will produce this error as `GenericAssociatedFoo` has type that is an
`extern struct` (thus layout `extern` and not `auto`) but also contains a field whose type is defined by the
associated type `Payload`. If you remove `extern`, the below code would compile.
```zig
const GraphTrait = struct {
    // [E13] Structs making use of associated types must have automatic layout. Found issue in
    // 'example.GraphTrait.GenericAssociatedFoo'.
    pub const GenericAssociatedFoo: extern struct {
        bar: traitor.AssociatedType("Payload"),
    } = undefined;

    pub const Payload = void;
};
```

### `[E14] TraitGenericTypeHasDecls`
This is an error with the trait you supplied, not the type that is supposed to be checked. The trait has a field
or declaration that has a type that makes use of an associated type internally while also having a declaration.
This is currently not possible due to a limitation of how `@Type` works in zig
(https://github.com/ziglang/zig/issues/6709).
As an example, the following code will produce this error as `GenericAssociatedFoo` has type that contains a field
whose type is defined by the associated type `Payload` while also containing a declaration `SomeDeclaration`.
```zig
const GraphTrait = struct {
    // [E14] Structs making use of associated types must not have declarations.
    // Found issue in 'example.GraphTrait.GenericAssociatedFoo'.
    pub const GenericAssociatedFoo: struct {
        const SomeDeclaration: i32 = 5;

        bar: traitor.AssociatedType("Payload"),
    } = undefined;

    pub const Payload = void;
};
```
