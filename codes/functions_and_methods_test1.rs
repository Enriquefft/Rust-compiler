// Tests function parameters and return values
fn add(a: i32, b: i32) -> i32 { a + b }
fn scale(value: &i32, factor: &i32) -> i32 { *value * *factor }

fn main() {
    let base = 4;
    let factor = 3;
    let total = add(base, scale(&base, &factor));
    println!("{}", total);
}
