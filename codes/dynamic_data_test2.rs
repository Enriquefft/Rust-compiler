// Tests dynamic data patterns with mutable state
// Simulates a Person struct with name and age, then increments age

fn celebrate(age: u32) -> u32 {
    age + 1
}

fn main() {
    // Simulate Person { name: "Alice", age: 29 }
    let age: u32 = 29;
    
    // Call celebrate to increment age from 29 to 30
    let older_age: u32 = celebrate(age);
    
    println!("{} {}", "Alice", older_age);
}
