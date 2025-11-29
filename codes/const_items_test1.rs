// Tests const item declarations
const MAX_SIZE: usize = 100;
const PI: f64 = 3.14159;
const ZERO: i32 = 0;

fn main() {
    let arr_size = MAX_SIZE;
    let pi_value = PI;
    let zero_val = ZERO;
    println!("{} {} {}", arr_size, pi_value, zero_val);
}
