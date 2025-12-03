fn main() {
    let s = String::from("hi");
    let t = s;            // move
    println!("{}", t);    // OK
}
