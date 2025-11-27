// Tests range expressions and nested conditionals
fn main() {
    let mut product: i32 = 1;
    for i in 2..5 { // 2,3,4
        let factor = if i > 3 { i - 1 } else { i + 1 };
        product *= factor;
    }
    let label = if product > 20 { "big" } else { "small" };
    println!("{} {}", product, label);
}
