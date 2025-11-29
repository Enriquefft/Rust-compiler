// Tests mixed struct initialization (shorthand and full syntax)
struct Rectangle {
    width: i32,
    height: i32,
    x: i32,
    y: i32,
}

fn main() {
    let width = 100;
    let height = 50;
    
    // Mix of shorthand and explicit field initialization
    let rect = Rectangle { width, height, x: 0, y: 0 };
    
    println!("{} {} {} {}", rect.width, rect.height, rect.x, rect.y);
}
