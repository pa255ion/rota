//! Thin wrappers over the raw Linux syscalls the host tool needs
//! (std.os.linux), turning error returns into Zig errors.

const std = @import("std");
const linux = std.os.linux;

pub const Error = error{ Again, NoDevice, Interrupted, Failed };

pub fn check(rc: usize) Error!usize {
    return switch (linux.errno(rc)) {
        .SUCCESS => rc,
        .AGAIN => error.Again,
        .NODEV, .NOENT, .IO => error.NoDevice,
        .INTR => error.Interrupted,
        else => error.Failed,
    };
}

pub fn open(path: [*:0]const u8, rw: bool) Error!i32 {
    const flags: linux.O = .{ .ACCMODE = if (rw) .RDWR else .RDONLY, .NOCTTY = true, .NONBLOCK = true, .CLOEXEC = true };
    return @intCast(try check(linux.open(path, flags, 0)));
}

pub fn close(fd: i32) void {
    _ = linux.close(fd);
}

pub fn read(fd: i32, buf: []u8) Error!usize {
    return check(linux.read(fd, buf.ptr, buf.len));
}

pub fn write(fd: i32, buf: []const u8) Error!usize {
    return check(linux.write(fd, buf.ptr, buf.len));
}

pub fn ioctl(fd: i32, req: u32, arg: usize) Error!usize {
    return check(linux.ioctl(fd, req, arg));
}

pub fn sleepMs(ms: u32) void {
    const req: linux.timespec = .{ .sec = @intCast(ms / 1000), .nsec = @intCast(@as(u64, ms % 1000) * 1_000_000) };
    _ = linux.nanosleep(&req, null);
}

/// Monotonic time in microseconds.
pub fn nowUs() i64 {
    var ts: linux.timespec = undefined;
    _ = linux.clock_gettime(.MONOTONIC, &ts);
    return @as(i64, ts.sec) * 1_000_000 + @divTrunc(@as(i64, ts.nsec), 1000);
}

/// Wait for fds; returns the number ready (0 on timeout or a signal).
pub fn poll(fds: []linux.pollfd, timeout_ms: i32) usize {
    const rc = linux.poll(fds.ptr, fds.len, timeout_ms);
    return check(rc) catch 0;
}
