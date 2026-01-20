use skrt::Srt;

fn main() {
    let data = r#"2
00:00:01,000 --> 00:00:04,000
Hello,

1
00:00:05,000 --> 00:00:08,000
World!

"#;

    let mut srt = Srt::try_parse(data).unwrap();
    srt.resequence();

    println!("{srt:#?}");
}
