// Tests struct initialization shorthand syntax
struct Point {
    x: i32,
    y: i32,
}

fn main() {
    let x = 10;
    let y = 20;
    
    // Shorthand syntax: { x } is equivalent to { x: x }
    let p = Point { x, y };
    
    println!("{} {}", p.x, p.y);
}
