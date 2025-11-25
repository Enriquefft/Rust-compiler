// Tests arithmetic, comparison, and logical operators
fn main() {
    let a = 8i32;
    let b = 3i32;
    let arithmetic = (a + b) * (a - b) / b;
    let comparisons = (a > b) as i32 + (a == b) as i32 + (a <= b) as i32;
    let boolean = (a > b && b % 2 == 1) || false;
    println!("{} {} {}", arithmetic, comparisons, boolean);
}
