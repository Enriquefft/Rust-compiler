// Tests dynamic data patterns using simple variables
// Simulates a linked list push operation where values 1 and 2 are added
// and their sum (2 + 1 = 3) is computed

fn main() {
    // Simulate pushing values onto a list: first 1, then 2
    let first_value: i32 = 1;
    let second_value: i32 = 2;
    
    // Compute sum of the two values (simulating traversal of linked list)
    let sum: i32 = first_value + second_value;
    
    println!("{}", sum);
}
