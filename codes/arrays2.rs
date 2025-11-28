fn main() {
    let mut values = [10, 20, 30, 40, 50];
    for i in 0..values.len() {
        values[i] *= 2;
    }

    println!("doubled: {}", values[1]);
}
