fn main() {
    let nums: [i32; 4] = [3, 6, 1, 8];


    let mut sum = 0;
    for n in nums {
        sum += n;
    }

    println!("sum = {}", sum);
}
