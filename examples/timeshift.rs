use skrt::{Srt, Timestamp};

fn main() {
    let mut srt = Srt::new();

    srt.add_subtitle(
        Timestamp::from_millis(0).unwrap(),
        Timestamp::from_millis(1000).unwrap(),
        "First".into(),
    );

    srt.add_subtitle(
        Timestamp::from_millis(1000).unwrap(),
        Timestamp::from_millis(2000).unwrap(),
        "Second".into(),
    );

    for sub in &mut srt {
        sub.set_start(sub.start().shift_millis(300).unwrap());
        sub.set_end(sub.end().shift_millis(300).unwrap());
    }

    println!("{}", srt.serialize());
}
