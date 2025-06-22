use std::process::exit;

fn main() {
    let text = std::env::args().skip(1).fold(String::new(), |acc, c| {
        if acc.is_empty() {
            c
        } else {
            format!("{acc} {c}")
        }
    });
    if text.is_empty() {
        println!("You might want to add some arguments to encode...");
        exit(1)
    }
    if let Some(code) = barcode_gen::make_barcode(&text) {
        println!("{code}");
    }
}
