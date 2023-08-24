// This software is in the public domain for jursidictions in which the public
// domain exists. Alternatively, it is available under the Zero-Clause BSD
// license.
// ------------------------------------------------------------------------------
// Zero-Clause BSD
// Copyright (c) 2023 greytdepression <greysdevmail@gmail.com>
// =============
//
// Permission to use, copy, modify, and/or distribute this software for
// any purpose with or without fee is hereby granted.
//
// THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL
// WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE
// FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY
// DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
// AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
// OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

const std = @import("std");
const trait = std.meta.trait;

const Type = std.builtin.Type;

const major_version = 0;
const minor_version = 1;

const meta_declaration_modifier = "__traitor";
const meta_trait_name = meta_declaration_modifier ++ "_trait_name";

const ErrorCode = enum(u8) {
    Unknown = 0,
    TraitNotAStruct,
    TypeNotAStruct,
    DeclarationIncompatibleType,
    FunctionIncompatibleSignature,
    MissingDeclaration,
    MissingFunction,
    MetaDataHasIncorrectType,
};

const Writer = blk: {
    var error_message_buffer = std.io.bufferedWriter(std.io.null_writer);
    var error_writer = error_message_buffer.writer();

    break :blk @TypeOf(error_writer);
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

    // In case there are no errors with the trait itself, reset `error_message_buffer.end` to `backup_point`
    // to erase those messages.
    const backup_point = error_message_buffer.end;
    error_writer.print("The trait '{s}' is not well-formed. Encountered the following error(s):\n", .{@typeName(Trait)}) catch unreachable;

    // Check for trait metadata
    const trait_name = comptime blk: {

        // Check if the trait contains a `__traitor_trait_name` declaration
        if (@hasDecl(Trait, meta_trait_name)) {
            const value = @field(Trait, meta_trait_name);

            // Check if the declaration is compatible with `[]const u8`
            if (isCoercibleToString(@TypeOf(value))) {
                break :blk @as([]const u8, value);
            } else {
                printError("The type of the trait's '{s}' declaration must be compatible with '[]const u8', found '{s}' instead.", &error_writer, .MetaDataHasIncorrectType, .{
                    meta_trait_name,
                    @typeName(@TypeOf(value)),
                });
                success = false;
            }
        }

        break :blk @as([]const u8, @typeName(Trait));
    };

    const type_name = @typeName(T);

    if (success) {
        // There were no errors with the trait itself. Reset the message about the trait being not well-formed.
        error_message_buffer.end = backup_point;
    } else {
        // We encountered errors with the trait itself. Do not check T.
        return;
    }

    // Knowing the trait's name, we can now give some additional info in the print out.
    error_writer.print(
        \\The type '{s}' does not satisfy the trait bounds of trait '{s}' due to the following errors:
        \\
    , .{ type_name, trait_name }) catch unreachable;

    //--------------------------------------------------
    // Check if T satisfies the trait boundary
    //--------------------------------------------------

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
                @compileError("Field '" ++ trait_field_name ++ "' has the wrong type. Expected '" ++ @typeName(trait_field_type) ++ "', found '" ++ @typeName(t_field_type) ++ "'.");
            }
        } else {
            success = false;
            @compileError("Missing field '" ++ trait_field_name ++ "'.");
        }
    }
}

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
