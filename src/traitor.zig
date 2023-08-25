//! traitor v0.1
//!
//! A small public-domain trait framework for zig
//!
//! The usage of this framework is explained in a comment at a top of
//! the source file.
//!
//! This software is in the public domain for jursidictions in which the
//! public domain exists. Alternatively, it is available under the Zero-
//! Clause BSD license.
//!
//! ----------------------------------------------------------------------
//! BSD Zero Clause License (SPDX: 0BSD)
//! ----------------------------------------------------------------------
//!
//! Permission to use, copy, modify, and/or distribute this software
//! for any purpose with or without fee is hereby granted.
//!
//! THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
//! WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
//! WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL
//! THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
//! CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
//! LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
//! NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
//! WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

// More information in the readme on traitor's github repo:
//  https://github.com/greytdepression/traitor/tree/main
//
// ## Quick Start
// Zig's standard library already has trait helpers in the form of std.meta.trait that
// lets you query a type for declarations and fields by name.
// Traitor works in a more Rust inspired way. You define a trait by building a struct
// that contains all the desired fields, declarations, and functions.
// ```zig
// const GraphTrait = struct {
//     pub const Directed = false;
//
//     num_vertices: usize,
//
//     // This is just a dummy implementation used to obtain the function signature.
//     // Traits are only meant to be a compile time construct and not used at runtime.
//     pub fn hasEdge(i: usize, j: usize) bool {
//         _ = j; _ = i; unreachable;
//     }
// };
// ```
// *Note that (function) declarations must be marked `pub` in order for traitor to see them.*
// Now if you want to check another type against those trait bounds, you can call
// `traitor.checkTrait(GraphTrait, MyOtherType)`. Traitor will then check that `MyOtherType` also
// has a declaration `Directed` of type `bool`, a field `num_vertices` of type `usize`, and a function
// `hasEdge` taking two `usize` and returning a `bool`.
// This is intended to be used in generic functions, e.g.
// ```zig
// fn dijkstra(graph: anytype, start: usize, end: usize) -> Path {
//     comptime traitor.checkTrait(GraphTrait, @TypeOf(graph));
//
//     // code here
// }
// ```
// Make sure to call `checkTrait` with the `comptime` keyword to ensure that the zig compiler does not
// print additional compile errors that arise from your code trying to access missing declarations or
// fields if the trait bounds are not met.
//
// Additionally, traitor allows you to add some meta data to your traits. Any declaration whose identifier
// starts with `__traitor` will be skipped when checking trait bounds. Currently, the only meta data traitor
// recognizes is `__traitor_trait_name` which can be set to a string (literal) and overrides the name
// traitor will call the trait in error messages.
// ```zig
// const GraphTrait2 = struct {
//     pub const __traitor_trait_name = "Graph II";
//     // ...
// };
// ```
// **Warning: the prefix `__traitor_internal` is reserved for internal meta data. Using it yourself may
// result unforseen errors.**
//
// For an explanation of error codes, see below.

const major_version = 0;
const minor_version = 1;

const meta_declaration_modifier = "__traitor";
const meta_trait_name = meta_declaration_modifier ++ "_trait_name";

/// The different error codes traitor may emit.
///
/// ## Error Codes
/// Traitor prints a two digit error code (e.g. `[E01]`) at the start of each error message. These can be
/// cross referenced here for a more detailed explanation of the kind of error traitor encountered.
///
/// ### `[E00] Unknown`
/// This is the default error value and should never be encountered. If you see an `[E00]` error message,
/// traitor itself has a bug. Please file an issue about it :)
///
/// ### `[E01] TraitNotAStruct`
/// This is an error with the trait you supplied, not the type that is supposed to be checked. Traits are
/// defined using structs (see above in the Quick Start guide). You or the library that you are using supplied
/// something different from a trait.
/// As an example, the following code will produce this error as `u32` is not a struct:
/// ```zig
/// // [E01] The trait must be a struct but 'u32' is not.
/// traitor.checkTrait(u32, TypeToCheck);
/// ```
///
/// ### `[E02] TypeNotAStruct`
/// This is an error with the type you supplied. Similarly to the trait itself, the type that you check must
/// also be a struct. The following code will produce this error:
/// ```zig
/// // [E02] The type implementing the trait must be a struct but 'u32' is not.
/// traitor.checkTrait(GraphTrait, u32);
/// ```
///
/// ### `[E03] DeclarationIncompatibleType`
/// This is an error with the type you supplied. The type you supplied contains a declaration that has the same
/// name as one of the required declarations of your trait, but it has the wrong type. With the graph example
/// above, the following implementation would cause this error:
/// ```zig
/// const MyGraph = struct {
///
///     // [E03] Declaration 'Directed' has the wrong type. Expected 'bool', found '*const [5:0]u8'.
///     pub const Directed = "maybe";
///
///     // ...
/// };
/// ```
///
/// ### `[E04] FunctionIncompatibleSignature`
/// This is an error with the type you supplied. The type you supplied contains a function declaration that has the
/// same name as one of the required function declarations of your trait, but it has the wrong signature. With the
/// graph example above, the following implementation would cause this error:
/// ```zig
/// const MyGraph = struct {
///
///     // [E04] Function 'hasEdge' has the wrong signature. Expected 'fn(usize, usize) bool', found 'fn(bool) void'.
///     pub fn hasEdge(foo: bool) void {}
///
///     // ...
/// };
/// ```
///
/// ### `[E05] FieldIncompatibleType`
/// This is an error with the type you supplied. The type you supplied contains a field that has the same
/// name as one of the required fields of your trait, but it has the wrong type. With the graph example
/// above, the following implementation would cause this error:
/// ```zig
/// const MyGraph = struct {
///
///     // [E05] Field 'num_vertices' has the wrong type. Expected 'usize', found 'bool'.
///     num_vertices: bool,
///
///     // ...
/// };
/// ```
///
/// ### `[E06] MissingDeclaration`
/// ### `[E07] MissingFunction`
/// ### `[E08] MissingField`
/// This is an error with the type you supplied. The type is missing a declaration/function declaration/field.
/// With the graph example above, the following implementation would cause this error:
/// ```zig
/// // [E06] Missing declaration 'pub Directed: bool'.
/// // [E07] Missing function declaration 'pub fn hasEdge(usize, usize) bool {}'.
/// // [E08] Missing field 'num_vertices: usize'.
/// const MyGraph = struct {};
/// ```
///
/// ### `[E09] TraitMetaDataHasIncorrectType`
/// This is an error with the trait you supplied, not the type that is supposed to be checked. One of the
/// meta data declarations of the trait has the wrong type. You or the library that you are using supplied
/// added incompatible meta data.
/// As an example, the following code will produce this error as `__traitor_trait_name` must be a string.
/// ```zig
/// const GraphTrait = struct {
///     // [E09] The type of the trait's '__traitor_trait_name' declaration must be compatible with
///     // '[]const u8', found 'bool' instead.
///     pub const __traitor_trait_name = false;
/// };
/// ```
const ErrorCode = enum(u8) {
    Unknown = 0,
    TraitNotAStruct = 1,
    TypeNotAStruct = 2,
    DeclarationIncompatibleType = 3,
    FunctionIncompatibleSignature = 4,
    FieldIncompatibleType = 5,
    MissingDeclaration = 6,
    MissingFunction = 7,
    MissingField = 8,
    MetaDataHasIncorrectType = 9,
};

pub fn checkTrait(comptime Trait: type, comptime T: type) void {

    //--------------------------------------------------
    // Error Messaging Setup
    //--------------------------------------------------
    var success = true;

    var error_message_buffer = std.io.bufferedWriter(std.io.null_writer);
    var error_writer = error_message_buffer.writer();

    const error_message_header =
        \\traitor v{}.{} -- zig trait framework
        \\
        \\An explanation of error codes can be found at the top of this file ({s}).
        \\
        \\
    ;

    error_writer.print(error_message_header, .{
        major_version,
        minor_version,
        @src().file,
    }) catch unreachable;

    defer {
        if (!success) {
            const message = error_message_buffer.buf[0..error_message_buffer.end];

            @compileError(message);
        }
    }

    //--------------------------------------------------
    // Input Parameter Validation
    //--------------------------------------------------
    const trait_info = @typeInfo(Trait);
    const t_info = @typeInfo(T);

    if (trait_info != .Struct) {
        printError("The trait must be a struct but '{s}' is not.", &error_writer, .TraitNotAStruct, .{@typeName(Trait)});
        success = false;

        // This is a fatal error. We cannot do anything past this point.
        return;
    }

    if (t_info != .Struct) {
        printError("The type implementing the trait must be a struct but '{s}' is not.", &error_writer, .TypeNotAStruct, .{@typeName(T)});
        success = false;

        // This is a fatal error. We cannot do anything past this point.
        return;
    }

    const trait_struct_info = trait_info.Struct;

    //--------------------------------------------------
    // Trait Meta Data Validation
    //--------------------------------------------------

    var trait_name: []const u8 = @typeName(Trait);
    const type_name = @typeName(T);
    {
        // In case there are no errors with the trait itself, reset `error_message_buffer.end` to `backup_point`
        // to erase those messages.
        const backup_point = error_message_buffer.end;
        error_writer.print("The trait '{s}' is not well-formed. Encountered the following error(s):\n", .{@typeName(Trait)}) catch unreachable;
        defer if (success) {
            error_message_buffer.end = backup_point;
        };

        // Check if the trait contains a `__traitor_trait_name` declaration
        if (@hasDecl(Trait, meta_trait_name)) {
            const value = @field(Trait, meta_trait_name);

            // Check if the declaration is compatible with `[]const u8`
            if (isCoercibleToString(@TypeOf(value))) {
                trait_name = value;
            } else {
                printError("The type of the trait's '{s}' declaration must be compatible with '[]const u8', found '{s}' instead.", &error_writer, .MetaDataHasIncorrectType, .{
                    meta_trait_name,
                    @typeName(@TypeOf(value)),
                });
                success = false;

                // We encountered errors with the trait itself. Do not check T.
                return;
            }
        }
    }

    //--------------------------------------------------
    // Check if T satisfies the trait boundary
    //--------------------------------------------------

    error_writer.print(
        \\The type '{s}' does not satisfy the trait bounds of trait '{s}' due to the following errors:
        \\
    , .{ type_name, trait_name }) catch unreachable;

    // Declarations
    inline for (trait_struct_info.decls) |trait_decl| {
        const trait_decl_name = trait_decl.name;

        // If the declaration name starts with '__traitor', it is meta information for this function and
        // we do not expect the object to implement this declaration.
        if (std.mem.startsWith(u8, trait_decl_name, meta_declaration_modifier)) {
            continue;
        }

        const trait_decl_type = @TypeOf(@field(Trait, trait_decl_name));

        if (@hasDecl(T, trait_decl_name)) {
            const t_decl_type = @TypeOf(@field(T, trait_decl_name));

            if (t_decl_type != trait_decl_type) {
                success = false;

                declTypeErrorMessage(&error_writer, trait_decl_name, trait_decl_type, t_decl_type);
            }
        } else {
            success = false;

            declMissingErrorMessage(&error_writer, trait_decl_name, trait_decl_type);
        }
    }

    // Fields
    inline for (trait_struct_info.fields) |trait_field| {
        const trait_field_name = trait_field.name;
        const trait_field_type = trait_field.type;

        if (@hasField(T, trait_field_name)) {
            const dummy: T = undefined;
            const t_field_type = @TypeOf(@field(dummy, trait_field_name));

            if (t_field_type != trait_field_type) {
                success = false;

                printError("Field '{s}' has the wrong type. Expected '{s}', found '{s}'.", &error_writer, .FieldIncompatibleType, .{
                    trait_field_name,
                    @typeName(trait_field_type),
                    @typeName(t_field_type),
                });
            }
        } else {
            success = false;

            printError("Missing field '{s}: {s}'.", &error_writer, .MissingField, .{
                trait_field_name,
                @typeName(trait_field_type),
            });
        }
    }
}

const std = @import("std");
const Type = std.builtin.Type;

const Writer = blk: {
    var error_message_buffer = std.io.bufferedWriter(std.io.null_writer);
    break :blk @TypeOf(error_message_buffer.writer());
};

fn declTypeErrorMessage(writer: *Writer, name: []const u8, comptime expected_type: type, comptime actual_type: type) void {
    const expected_type_info = @typeInfo(expected_type);

    switch (expected_type_info) {
        .Fn => {
            printError("Function '{s}' has the wrong signature. Expected '{s}', found '{s}'.", writer, .FunctionIncompatibleSignature, .{
                name,
                @typeName(expected_type),
                @typeName(actual_type),
            });
        },
        else => {
            printError("Declaration '{s}' has the wrong type. Expected '{s}', found '{s}'.", writer, .DeclarationIncompatibleType, .{
                name,
                @typeName(expected_type),
                @typeName(actual_type),
            });
        },
    }
}

fn declMissingErrorMessage(writer: *Writer, name: []const u8, comptime expected_type: type) void {
    const expected_type_info = @typeInfo(expected_type);

    switch (expected_type_info) {
        .Fn => {
            const function_signature_full = if (std.mem.startsWith(u8, @typeName(expected_type), "fn("))
                "fn " ++ name ++ @typeName(expected_type)[2..] ++ " {}"
            else
                name ++ ": " ++ @typeName(expected_type);

            printError("Missing function declaration 'pub {s}'.", writer, .MissingFunction, .{function_signature_full});
        },
        else => {
            printError("Missing declaration 'pub {s}: {s}'.", writer, .MissingDeclaration, .{
                name,
                @typeName(expected_type),
            });
        },
    }
}

fn printError(comptime fmt: []const u8, writer: *Writer, code: ErrorCode, args: anytype) void {
    writer.print(" - [E{:0>2}] " ++ fmt ++ "\n", .{@as(u8, @intFromEnum(code))} ++ args) catch unreachable;
}

fn isCoercibleToString(comptime T: type) bool {
    const info = @typeInfo(T);

    switch (info) {
        .Pointer => |ptr| {
            if (ptr.child == u8)
                return true;

            const child_info = @typeInfo(ptr.child);
            switch (child_info) {
                .Array => |array| {
                    if (array.child == u8)
                        return true;
                },
                else => {},
            }
        },
        else => {},
    }

    return false;
}
