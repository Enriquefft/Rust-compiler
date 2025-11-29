// Tests const items with various primitive types
const NEGATIVE: i64 = -42;
const UNSIGNED: u32 = 255;
const FLOAT32: f32 = 2.5;
const BOOLEAN: bool = true;

fn main() {
    let a = NEGATIVE;
    let b = UNSIGNED;
    let c = FLOAT32;
    let d = BOOLEAN;
    println!("{} {} {} {}", a, b, c, d);
}
