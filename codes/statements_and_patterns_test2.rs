// Tests mutable let bindings, type annotations, and empty statement
fn main() {
    let mut counter: i32 = 0;
    counter += 1;
    counter += 2;
    {
        counter += 3;
        ; // empty statement
    }
    println!("{}", counter);
}
