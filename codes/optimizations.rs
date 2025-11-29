const USE_FAST_PATH: bool = true;
const SCALE_A: i64 = 2 * 2 * 2;      // 8  → constant folding
const SCALE_B: i64 = 10 - 3;         // 7  → constant folding

fn slow_path(x: i64) -> i64 {
    // Potentially large, but will become unreachable if USE_FAST_PATH is true
    let mut acc = 0;
    let mut i = 0;

    while i < 100 {
        acc += (x * i) % 7;
        i += 1;
    }

    // Entire function body can become dead if never called
    acc
}

fn fast_path(x: i64) -> i64 {
    // Dead code: never used
    let unused_product = x * 12345; // DCE: result not used

    if x == 0 {
        // Simple, but reachable path
        return 0;
    }

    let doubled = x + x;            // may be kept
    let triple = doubled * 3;       // may be kept
    let useless = triple - triple;  // always 0 → DCE after constant folding
    let final_val = triple + useless;

    final_val
}

fn choose_path(x: i64) -> i64 {
    // Condition is a compile-time constant → CFG simplification after folding
    if USE_FAST_PATH {
        // x * SCALE_A + SCALE_B can be folded partially
        // SCALE_A = 8, SCALE_B = 7
        let scaled = x * SCALE_A + SCALE_B;
        fast_path(scaled)
    } else {
        // Entire else branch becomes unreachable → CFG simplification + DCE
        slow_path(x)
    }
}

fn main() {
    // Known-length array
    let data = [1, 2, 3, 4, 5, 6, 7, 8]; // len() is a small known constant
    let mut total: i64 = 0;

    // for-loop over 0..data.len() → canonical CFG for your compiler
    for i in 0..data.len() {
        let v = data[i] as i64;
        let score = choose_path(v);
        total += score;
    }

    // Unreachable block: condition is compile-time false
    if false {
        // Entire block should be removed by CFG simplification + DCE
        println!("Debug total: {}", total);
    }

    // Dead computation: result never used
    let debug_only = total * 9999;   // DCE

    // Some constant folding may happen here (e.g., modulo with small constant)
    println!("Result: {}", total % 1000);
}
