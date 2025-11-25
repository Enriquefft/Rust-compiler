// Tests conditional expressions and range iteration
fn main() {
    let n = 6;
    let parity = if n % 2 == 0 { "even" } else { "odd" };
    let mut total = 0;
    for i in 1..=n {
        if i % 2 == 0 { total += i; }
    }
    println!("{} {}", parity, total);
}
