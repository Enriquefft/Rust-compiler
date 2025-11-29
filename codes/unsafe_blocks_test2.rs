// Tests unsafe block with statements
fn main() {
    let mut value: i32 = 10;
    
    unsafe {
        value = 20;
    }
    
    println!("{}", value);
}
