const std = @import("std");

pub const FactorialErrors = error{Overflow};

pub fn factorial(comptime Num: type, n: i8) FactorialErrors!Num {
    if (@typeInfo(Num) != .Int) {
        @compileError("factorial called with non-integral type: " ++ @typeName(Num));
    }

    if (n < 0) {
        return error.Overflow;
    }
    return calc: {
        var i: i8 = 1;
        var fac: Num = 1;
        while (i <= n) : (i += 1) {
            const tmp = @mulWithOverflow(fac, i);
            if (tmp[1] != 0)
                break :calc error.Overflow;
            fac = tmp[0];
        } else break :calc fac;
    };
}

test "Test Factorial Base Case 1" {
    std.debug.assert(try factorial(i64, 0) == 1);
}

test "Test Factorial Base Case 2" {
    std.debug.assert(try factorial(i64, 1) == 1);
}

test "Test Factorial Normal Cases" {
    std.debug.assert(try factorial(i64, 2) == 2);
    std.debug.assert(try factorial(i64, 3) == 6);
    std.debug.assert(try factorial(i64, 4) == 24);
    std.debug.assert(try factorial(i64, 5) == 120);
    std.debug.assert(try factorial(i64, 6) == 720);
    std.debug.assert(try factorial(i64, 7) == 5040);
}
