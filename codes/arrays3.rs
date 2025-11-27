struct Point {
    x: i32,
    y: i32,
}

fn main() {
    let mut points: [Point; 3] = [
        Point { x: 1, y: 2 },
        Point { x: -3, y: 4 },
        Point { x: 10, y: -5 },
    ];

    for i in 0..points.len() {
        // shift each point by (1, 1)
        points[i].x += 1;
        points[i].y += 1;
    }

    for p in &points {
        println!("({}, {})", p.x, p.y);
    }
}
