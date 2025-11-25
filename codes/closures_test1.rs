// Tests closure expression assignment and invocation
fn apply(f: fn(i32) -> i32, x: i32) -> i32 { f(x) }

fn main() {
    let add_one = |n: i32| n + 1;
    let double_then_add = |n: i32| { let doubled = n * 2; doubled + 3 };
    println!("{} {}", apply(add_one, 5), double_then_add(4));
}
