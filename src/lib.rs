use std::borrow::Cow;

#[derive(Debug)]
pub struct Srt<'a> {
    subtitles: Vec<Subtitle<'a>>,
}

#[derive(Debug)]
pub struct Subtitle<'a> {
    seq: u32,
    start_time: [u16; 4],
    end_time: [u16; 4],
    text: Cow<'a, str>,
}

impl Srt<'_> {
    pub fn try_parse(mut srt: &str) -> Result<Srt<'_>, &'static str> {
        let mut subtitles = Vec::new();

        while !srt.is_empty() {
            // sequence number
            let end = srt
                .find(|c: char| !c.is_ascii_digit())
                .ok_or("invalid sequence number")?;
            let seq: u32 = srt[..end].parse().map_err(|_| "expected sequence number")?;
            srt = &srt[end..];
            let discard = newline_discard(srt)?;
            srt = &srt[discard..];

            // time start
            let start_time = parse_time(srt)?;
            srt = &srt[12..];

            // time separator
            let b = srt.as_bytes();
            if b.len() < 5 {
                return Err("not enough data");
            }
            if !matches!(&b[0..5], b" --> ") {
                return Err("expected time separator");
            }
            srt = &srt[5..];

            // time end
            let end_time = parse_time(srt)?;
            srt = &srt[12..];
            let discard = newline_discard(srt)?;
            srt = &srt[discard..];

            // text
            let mut it = srt.chars();
            let mut end = 0;
            let mut discard = 0;
            while let Some(c) = it.next() {
                if c == '\n' {
                    // check for consecutive LF
                    let mut it2 = it.clone();
                    let c2 = it2.next();
                    if let Some('\n') = c2 {
                        discard = 2;
                        break;
                    }
                } else if c == '\r' {
                    // check for consecutive CRLF
                    let mut it2 = it.clone();
                    let (c2, c3, c4) = (it2.next(), it2.next(), it2.next());
                    if let (Some('\n'), Some('\r'), Some('\n')) = (c2, c3, c4) {
                        discard = 4;
                        break;
                    }
                }

                end += c.len_utf8();
            }
            let text = &srt[..end];
            srt = &srt[end + discard..];

            subtitles.push(Subtitle {
                seq,
                start_time,
                end_time,
                text: text.into(),
            });
        }

        Ok(Srt { subtitles })
    }
}

fn parse_time(s: &str) -> Result<[u16; 4], &'static str> {
    let b = s.as_bytes();
    if b.len() < 12 {
        return Err("not enough data");
    }

    let mut valid = b[0].is_ascii_digit();
    valid &= b[1].is_ascii_digit();
    valid &= matches!(b[2], b':');
    valid &= matches!(b[3], b'0'..=b'5');
    valid &= b[4].is_ascii_digit();
    valid &= matches!(b[5], b':');
    valid &= matches!(b[6], b'0'..=b'5');
    valid &= b[7].is_ascii_digit();
    valid &= matches!(b[8], b',');
    valid &= b[9].is_ascii_digit();
    valid &= b[10].is_ascii_digit();
    valid &= b[11].is_ascii_digit();
    if !valid {
        return Err("invalid time");
    }

    Ok([
        (b[0] as u16 - b'0' as u16) * 10 + (b[1] as u16 - b'0' as u16),
        (b[3] as u16 - b'0' as u16) * 10 + (b[4] as u16 - b'0' as u16),
        (b[6] as u16 - b'0' as u16) * 10 + (b[7] as u16 - b'0' as u16),
        (b[9] as u16 - b'0' as u16) * 100
            + (b[10] as u16 - b'0' as u16) * 10
            + (b[11] as u16 - b'0' as u16),
    ])
}

fn newline_discard(s: &str) -> Result<usize, &'static str> {
    let mut discard = 0;
    let mut it = s.chars();
    while let Some(c) = it.next() {
        match c {
            ' ' | '\t' => discard += 1,
            '\r' => {
                if let Some('\n') = it.next() {
                    discard += 2;
                    break;
                }
                return Err("expected whitespace or newline");
            }
            '\n' => {
                discard += 1;
                break;
            }
            _ => {
                return Err("expected whitespace or newline");
            }
        }
    }
    Ok(discard)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let data = r#"1
00:00:52,119 --> 00:00:56,658
<i>Lorum ipsum dolor sit amet,
consectetur adipiscing elit,</i>
sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

2
00:00:59,794 --> 00:01:03,430
Ut enim ad minim veniam, quis

3
00:01:04,566 --> 00:01:05,667
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in
voluptate velit esse cillum
dolore
eu fugiat
nulla pariatur
.

"#;

        let parsed = Srt::try_parse(data).unwrap();
    }
}
