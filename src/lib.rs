use std::borrow::Cow;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Srt<'a> {
    subtitles: Vec<Subtitle<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Subtitle<'a> {
    seq: u32,
    start: Timestamp,
    end: Timestamp,
    text: Cow<'a, str>,
}

#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
pub struct Timestamp {
    hours: u16,
    minutes: u16,
    seconds: u16,
    milliseconds: u16,
}

impl<'a> Srt<'a> {
    pub fn new() -> Srt<'a> {
        Srt {
            subtitles: Vec::new(),
        }
    }

    pub fn add_subtitle(&mut self, start: Timestamp, end: Timestamp, text: Cow<'a, str>) {
        let sub = Subtitle {
            seq: self.subtitles.len() as u32 + 1,
            start,
            end,
            text,
        };

        self.subtitles.push(sub);
    }

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
                start: start_time,
                end: end_time,
                text: text.into(),
            });
        }

        Ok(Srt { subtitles })
    }
}

fn parse_time(s: &str) -> Result<Timestamp, &'static str> {
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

    Ok(Timestamp {
        hours: (b[0] as u16 - b'0' as u16) * 10 + (b[1] as u16 - b'0' as u16),
        minutes: (b[3] as u16 - b'0' as u16) * 10 + (b[4] as u16 - b'0' as u16),
        seconds: (b[6] as u16 - b'0' as u16) * 10 + (b[7] as u16 - b'0' as u16),
        milliseconds: (b[9] as u16 - b'0' as u16) * 100
            + (b[10] as u16 - b'0' as u16) * 10
            + (b[11] as u16 - b'0' as u16),
    })
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
    fn parse_srt() {
        let data = r#"1
00:00:52,119 --> 00:00:56,658
<i>Lorum ipsum dolor sit amet,
consectetur adipiscing elit,</i>
sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

2
01:20:59,794 --> 02:01:03,430
Ut enim ad minim veniam, quis

3
99:01:04,566 --> 99:01:05,667
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in
voluptate velit esse cillum
dolore
eu fugiat
nulla pariatur
.

"#;

        let srt = Srt::try_parse(data).unwrap();

        assert_eq!(3, srt.subtitles.len());
        assert_eq!(
            Timestamp {
                hours: 1,
                minutes: 20,
                seconds: 59,
                milliseconds: 794
            },
            srt.subtitles[1].start
        );
    }
}
