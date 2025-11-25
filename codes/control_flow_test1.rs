// Tests while loops and returns
fn countdown(mut start: i32) -> i32 {
    let mut steps = 0;
    while start > 0 {
        start -= 1;
        steps += 1;
    }
    steps
}

fn main() {
    let steps = countdown(4);
    println!("{}", steps);
}
