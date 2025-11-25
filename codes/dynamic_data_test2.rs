// Tests owned strings and heap allocation patterns
struct Person { name: String, age: u32 }

fn celebrate(mut person: Person) -> Person {
    person.age += 1;
    person
}

fn main() {
    let alice = Person { name: String::from("Alice"), age: 29 };
    let older = celebrate(alice);
    println!("{} {}", older.name, older.age);
}
