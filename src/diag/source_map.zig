const std = @import("std");

pub const FileId = u32;

pub const Span = struct {
    file_id: FileId,
    start: usize, // byte offset
    end: usize, // byte offset (exclusive)
};

/// Simple source map that stores file contents for diagnostics and span mapping.
pub const SourceMap = struct {
    allocator: std.mem.Allocator,
    files: std.ArrayListUnmanaged(File),

    pub const File = struct {
        path: []const u8,
        contents: []const u8,
    };

    pub fn init(allocator: std.mem.Allocator) SourceMap {
        return .{ .allocator = allocator, .files = .{} };
    }

    pub fn deinit(self: *SourceMap) void {
        for (self.files.items) |file| {
            self.allocator.free(file.path);
            self.allocator.free(file.contents);
        }
        self.files.deinit(self.allocator);
    }

    pub fn addFile(self: *SourceMap, path: []const u8, contents: []const u8) !FileId {
        const owned_path = try self.allocator.dupe(u8, path);
        const owned_contents = try self.allocator.dupe(u8, contents);
        const id: FileId = @intCast(self.files.items.len);
        try self.files.append(self.allocator, .{ .path = owned_path, .contents = owned_contents });
        return id;
    }

    pub fn getFile(self: *SourceMap, id: FileId) ?File {
        if (id >= self.files.items.len) return null;
        return self.files.items[id];
    }
};
