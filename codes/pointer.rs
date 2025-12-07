fn main() {

    unsafe{
    let mut x: i32 = 4;
    let mut ptr: *mut i32 = &mut x;
        println!("{}", *ptr);
    }
}
