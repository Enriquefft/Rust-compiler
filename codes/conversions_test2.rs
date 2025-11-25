// Tests casting between pointers and integers
fn main() {
    let value: u32 = 255;
    let ptr: *const u32 = &value;
    let addr = ptr as usize;
    let round_trip: *const u32 = addr as *const u32;
    let as_signed: i64 = (value as i64) - 10;
    println!("{} {} {}", addr > 0, as_signed, unsafe { *round_trip });
}
