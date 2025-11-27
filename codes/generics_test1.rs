// Tests generic function and struct instantiation
fn identity<T>(value: T) -> T { value }

struct Pair<T> { a: T, b: T }

fn main() {
    let numbers = Pair { a: 1, b: 2};
    let mirrored = identity(numbers);
    println!("{} {}", mirrored.a, mirrored.b);
}
