// Tests generic impl methods
struct Wrapper<T> { value: T }

impl<T> Wrapper<T> {
    fn new(value: T) -> Wrapper<T> { Wrapper { value } }
    fn get(&self) -> &T { &self.value }
}

fn main() {
    let text = Wrapper::new(String::from("hi"));
    let number = Wrapper::new(42u64);
    println!("{} {}", text.get(), number.get());
}
