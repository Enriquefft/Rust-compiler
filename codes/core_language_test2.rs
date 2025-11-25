// Demonstrates blocks with implicit return and nested functions
fn double(n: i32) -> i32 {
    let result = { let interim = n + 1; interim * 2 };
    result
}

fn main() {
    let outer = 3;
    let inner_sum = { let temp = outer * 2; double(temp) };
    println!("{} {}", double(outer), inner_sum);
}
