// Tests for-loops and implicit return from blocks
fn sum_range(limit: i32) -> i32 {
    let mut acc = 0;
    for v in 0..limit {
        acc += v;
    }
    acc
}

fn main() {
    let result = { let base = 5; sum_range(base) };
    println!("{}", result);
}
