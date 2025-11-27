// Tests assignments, compound operations, dereference, and references
fn main() {
    let mut data = [10, 20, 30];
    let mut ptr: *mut i32 = &mut data[1];
    unsafe { *ptr += 5; };
    let mut total = 0;
    for i in 0..data.len() {
        let value_ref = &data[i];
        total += *value_ref;
    }
    let mut accumulator = 1;
    accumulator *= total;
    accumulator -= data[0];
    println!("{} {} {}", total, accumulator, unsafe { *ptr });
}
