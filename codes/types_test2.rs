// Tests arrays, references, raw pointers, and function types
fn apply_twice(f: fn(i32) -> i32, x: i32) -> i32 {
    f(f(x))
}

fn main() {
    let numbers: [i32; 4] = [1, 2, 3, 4];
    let first_ref: &i32 = &numbers[0];
    let mut value = 10;
    let mutable_ref: &mut i32 = &mut value;
    *mutable_ref += *first_ref;
    let ptr: *const i32 = first_ref as *const i32;
    let doubled = apply_twice(|n| n * 2, unsafe { *ptr });
    println!("{} {} {} {}", numbers[3], value, ptr.is_null(), doubled);
}
