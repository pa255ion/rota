//! The USB-UART (J14) as a raw serial port: 8N1, no echo, no line
//! discipline, non-blocking, at one of the relay's rates (protocol.rates).

const std = @import("std");
const linux = std.os.linux;
const sys = @import("sys.zig");

pub const Serial = struct {
    fd: i32,
    rx: [512]u8 = undefined,
    rx_len: usize = 0,

    pub fn open(path: [*:0]const u8) !Serial {
        const fd = try sys.open(path, true);
        var s: Serial = .{ .fd = fd };
        try s.setBaud(0);
        return s;
    }

    /// Rate by protocol.rates index: 115200, 921600, 460800, 230400. The
    /// kernel's struct termios has no speed fields; the rate is the CBAUD
    /// bits of c_cflag.
    pub fn setBaud(s: *Serial, code: u3) !void {
        var t = std.posix.tcgetattr(s.fd) catch return error.Failed;
        t.iflag = .{};
        t.oflag = .{};
        t.lflag = .{};
        t.cflag = .{ .CSIZE = .CS8, .CREAD = true, .CLOCAL = true };
        const speed: linux.speed_t = switch (code) {
            0 => .B115200,
            1 => .B921600,
            2 => .B460800,
            3 => .B230400,
            else => .B2000000,
        };
        var c: u32 = @bitCast(t.cflag);
        c = (c & ~@as(u32, 0o010017)) | @intFromEnum(speed);
        t.cflag = @bitCast(c);
        t.ispeed = speed;
        t.ospeed = speed;
        t.cc[@intFromEnum(linux.V.MIN)] = 0;
        t.cc[@intFromEnum(linux.V.TIME)] = 0;
        std.posix.tcsetattr(s.fd, .NOW, t) catch return error.Failed;
        s.flush();
    }

    /// Drop anything not yet sent or received.
    pub fn flush(s: *Serial) void {
        _ = sys.ioctl(s.fd, linux.T.CFLSH, 2) catch {}; // TCIOFLUSH
        s.rx_len = 0;
    }

    pub fn writeAll(s: *Serial, data: []const u8) !void {
        var rest = data;
        while (rest.len > 0) {
            const n = sys.write(s.fd, rest) catch |e| switch (e) {
                error.Again, error.Interrupted => {
                    var p = [_]linux.pollfd{.{ .fd = s.fd, .events = linux.POLL.OUT, .revents = 0 }};
                    _ = sys.poll(&p, 100);
                    continue;
                },
                else => return e,
            };
            rest = rest[n..];
        }
    }

    /// Read what has arrived; returns the next complete line (without
    /// CR/LF) if there is one. The slice is valid until the next call.
    pub fn line(s: *Serial, out: []u8) ?[]const u8 {
        if (s.rx_len < s.rx.len) {
            const n = sys.read(s.fd, s.rx[s.rx_len..]) catch 0;
            s.rx_len += n;
        }
        const nl = std.mem.indexOfScalar(u8, s.rx[0..s.rx_len], '\n') orelse {
            if (s.rx_len == s.rx.len) s.rx_len = 0; // garbage without newlines
            return null;
        };
        const raw = std.mem.trim(u8, s.rx[0..nl], "\r ");
        const len = @min(raw.len, out.len);
        @memcpy(out[0..len], raw[0..len]);
        std.mem.copyForwards(u8, s.rx[0 .. s.rx_len - nl - 1], s.rx[nl + 1 .. s.rx_len]);
        s.rx_len -= nl + 1;
        return out[0..len];
    }

    /// Exactly `out.len` raw bytes (what is buffered first), waiting up to
    /// timeout_ms; false if they don't all come.
    pub fn readExact(s: *Serial, out: []u8, timeout_ms: i64) bool {
        const k = @min(s.rx_len, out.len);
        @memcpy(out[0..k], s.rx[0..k]);
        std.mem.copyForwards(u8, s.rx[0 .. s.rx_len - k], s.rx[k..s.rx_len]);
        s.rx_len -= k;
        var got = k;
        const end = sys.nowUs() + timeout_ms * 1000;
        while (got < out.len) {
            const n = sys.read(s.fd, out[got..]) catch 0;
            got += n;
            if (n > 0) continue;
            const left = @divTrunc(end - sys.nowUs(), 1000);
            if (left <= 0) return false;
            var p = [_]linux.pollfd{.{ .fd = s.fd, .events = linux.POLL.IN, .revents = 0 }};
            _ = sys.poll(&p, @intCast(@min(left, 1000)));
        }
        return true;
    }

    /// Wait up to timeout_ms for a line starting with `prefix`; other lines
    /// are printed. Returns the line, or null on timeout.
    pub fn expect(s: *Serial, prefix: []const u8, timeout_ms: i64, out: []u8) ?[]const u8 {
        const end = sys.nowUs() + timeout_ms * 1000;
        while (true) {
            while (s.line(out)) |l| {
                if (std.mem.startsWith(u8, l, prefix)) return l;
                if (l.len > 0) std.debug.print("board: {s}\n", .{l});
            }
            const left = @divTrunc(end - sys.nowUs(), 1000);
            if (left <= 0) return null;
            var p = [_]linux.pollfd{.{ .fd = s.fd, .events = linux.POLL.IN, .revents = 0 }};
            _ = sys.poll(&p, @intCast(@min(left, 1000)));
        }
    }
};
