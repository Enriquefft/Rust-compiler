// Tests heap allocation with Box and recursive types
struct Node { value: i32, next: Option<Box<Node>> }

fn push(front: Option<Box<Node>>, value: i32) -> Option<Box<Node>> {
    Some(Box::new(Node { value, next: front }))
}

fn main() {
    let mut list: Option<Box<Node>> = None;
    list = push(list, 1);
    list = push(list, 2);
    let sum = match list {
        Some(ref node) => node.value + node.next.as_ref().unwrap().value,
        None => 0,
    };
    println!("{}", sum);
}
