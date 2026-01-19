# skrt

A fast, zero-copy parser and serializer for SubRip Text (SRT) subtitle files in Rust.

## Features

- Zero-copy parsing. Subtitle text borrows directly from the input string when possible
- Flexible input handling. Supports LF and CRLF line endings, optional UTF-8 BOM, trailing whitespace, and missing final newlines
- Timestamp manipulation. Shift timestamps forward or backward with millisecond precision
- Create subtitle files programmatically
- Detailed error reporting

## Installation

Add skrt to your Cargo.toml:

```toml
[dependencies]
skrt = "0.1"
```

## Usage

### Parsing an SRT file

```rust
use skrt::Srt;

let data = r#"1
00:00:01,000 --> 00:00:04,000
Hello, world!

2
00:00:05,000 --> 00:00:08,000
This is a subtitle.

"#;

let srt = Srt::try_parse(data).unwrap();

for subtitle in srt.iter() {
    println!("[{}] {}", subtitle.start(), subtitle.text());
}
```

### Building subtitles programmatically

```rust
use skrt::{Srt, Timestamp};

let mut srt = Srt::new();

srt.add_subtitle(
    Timestamp::from_millis(1000).unwrap(),
    Timestamp::from_millis(4000).unwrap(),
    "First subtitle".into(),
);

srt.add_subtitle(
    Timestamp::from_millis(5000).unwrap(),
    Timestamp::from_millis(8000).unwrap(),
    "Second subtitle".into(),
);

let output = srt.serialize();
println!("{output}");
```

### Shifting timestamps

```rust
use skrt::Srt;

let data = "1\n00:00:01,000 --> 00:00:04,000\nHello\n\n";
let mut srt = Srt::try_parse(data).unwrap();

// Shift all subtitles forward by 5 seconds
for sub in srt.iter_mut() {
    sub.set_start(sub.start().shift_millis(5000).unwrap());
    sub.set_end(sub.end().shift_millis(5000).unwrap());
}

println!("{}", srt.serialize());

```

### Working with timestamps

```rust
use skrt::Timestamp;

let ts = Timestamp::from_millis(5025000).unwrap(); // 1h 23m 45s
assert_eq!("01:23:45,000", ts.to_string());
assert_eq!(5025000, ts.to_millis());

// Timestamps are comparable
let ts2 = Timestamp::from_millis(5025500).unwrap();
assert!(ts < ts2);
```

## Format Details

SRT files consist of subtitle blocks separated by blank lines. Each block contains:

1. A sequence number (positive integer)
1. A timestamp line: HH:MM:SS,mmm --> HH:MM:SS,mmm
1. One or more lines of text

This crate handles common real-world variations:

- Both LF (\n) and CRLF (\r\n) line endings
- Optional UTF-8 BOM at the start of the file
- Trailing whitespace after timestamp lines
- Files that don't end with a trailing blank line

## Development

### Fuzz Testing

Install cargo-fuzz with `cargo +nightly install cargo-fuzz`, then `cd fuzz && cargo +nightly fuzz run -j $(grep -c processor /proc/cpuinfo) parse_srt`.
