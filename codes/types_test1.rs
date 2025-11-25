// Tests primitive types and literal usage
fn main() {
    let int_val: i64 = -42;
    let uint_val: usize = 7;
    let float_val: f32 = 3.5;
    let truth: bool = int_val < 0;
    let letter: char = 'z';
    let slice: &str = "hello";
    let owned: String = String::from("world");
    println!("{} {} {} {} {} {} {}", int_val, uint_val, float_val, truth, letter, slice, owned);
}
