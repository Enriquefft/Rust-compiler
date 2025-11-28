// Tests arithmetic, comparison, and logical operators
fn main() {
    let a:i32 = 8;
    let b:i32 = 3;
    let arithmetic = (a + b) * (a - b) / b;
    let comparisons = (a > b) as i32 + (a == b) as i32 + (a <= b) as i32;
    let boolean = (a > b && b % 2 == 1) || false;
    println!("{} {} {}", arithmetic, comparisons, boolean);
}
