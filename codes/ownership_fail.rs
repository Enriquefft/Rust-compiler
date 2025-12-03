fn main() {
    let s = String::from("hi");
    let t = s;
    println!("{}", s);    // ERROR: use of moved value `s`
}
