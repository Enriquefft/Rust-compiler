// Demonstrates functions, structs, type aliases, and main entry point
struct Point { x: i32, y: i32 }

type MyInt = i32;

fn add(a: MyInt, b: MyInt) -> MyInt {
    a + b
}

impl Point {
    fn offset(self, dx: i32, dy: i32) -> Point {
        Point { x: self.x + dx, y: self.y + dy }
    }
}

fn main() {
    let start = Point { x: 1, y: 2 };
    let moved = start.offset(3, 4);
    let sum = add(moved.x, moved.y);
    println!("{} {} {}", start.x, moved.y, sum);
}
