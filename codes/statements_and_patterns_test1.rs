// Tests let bindings and wildcard pattern
fn main() {
    let x = 2;
    let y = 3;
    let mut sum: i32;
    sum = x + y;
    let _ = sum * 2; // wildcard pattern
    println!("{} {}", x, sum);
}
