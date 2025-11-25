// Tests inherent impl methods with self reference
struct Counter { value: i32 }

impl Counter {
    fn new() -> Counter { Counter { value: 0 } }
    fn inc(&mut self) { self.value += 1; }
    fn current(&self) -> i32 { self.value }
}

fn main() {
    let mut counter = Counter::new();
    counter.inc();
    counter.inc();
    println!("{}", counter.current());
}
