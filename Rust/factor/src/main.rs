fn factor(num: i128) -> Vec<i128> {
    let mut factors: Vec<i128> = Vec::new();
    let mid = (num as f64).sqrt() as i128;
    let r = if num % 2 == 0 { (2..=mid).step_by(1) } else { (3..=mid).step_by(2) };

    for i in r {
        if num % i == 0 {
            factors.push(i);
            if i != num / i {
                factors.push(num / i);
            }
        }
    }

    factors
}

fn main() {
    let mut input = String::new();
    let mut num: i128 = 0;

    print!("Enter a number: ");

    std::io::stdin()
        .read_line(&mut input)
        .expect("Failed to read from stdin");

    let input_t = input.trim();

    match input_t.parse::<i128>() {
        Ok(i) => num = i,
        Err(..) => println!("This was not an integer: {}", input_t),
    };

    let res = factor(num);
    println!("{:?}", res);
}
