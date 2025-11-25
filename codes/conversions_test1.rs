// Tests explicit numeric and pointer casts
fn main() {
    let small: i32 = -12;
    let large: i64 = small as i64 * 2;
    let floaty: f32 = 5.75;
    let truncated: i32 = floaty as i32;
    let ptr: *const i32 = &small as *const i32;
    println!("{} {} {} {}", small, large, truncated, ptr.is_null());
}
