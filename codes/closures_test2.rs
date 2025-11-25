// Tests passing closures to functions without captures
fn operate(f: fn(i32, i32) -> i32, x: i32, y: i32) -> i32 {
    f(x, y)
}

fn main() {
    let sum = |a: i32, b: i32| a + b;
    let product = |a: i32, b: i32| { a * b };
    println!("{} {}", operate(sum, 2, 4), operate(product, 3, 5));
}
