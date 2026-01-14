use std::{
    borrow::Cow,
    cmp::Ordering,
    fmt::{Display, Write},
};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Srt<'a> {
    subtitles: Vec<Subtitle<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Subtitle<'a> {
    seq: usize,
    start: Timestamp,
    end: Timestamp,
    text: Cow<'a, str>,
}

#[derive(Debug, Default, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Timestamp {
    hours: u16,
    minutes: u16,
    seconds: u16,
    milliseconds: u16,
}

impl Ord for Timestamp {
    fn cmp(&self, other: &Self) -> Ordering {
        self.to_millis().cmp(&other.to_millis())
    }
}

impl PartialOrd for Timestamp {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Display for Timestamp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:02}:{:02}:{:02},{:03}",
            self.hours, self.minutes, self.seconds, self.milliseconds
        )
    }
}

impl Timestamp {
    pub fn from_millis(mut millis: u64) -> Result<Self, &'static str> {
        let hours = millis / (3_600_000);
        millis -= hours * 3_600_000;
        let minutes = millis / (60_000);
        millis -= minutes * 60_000;
        let seconds = millis / 1_000;
        millis -= seconds * 1_000;

        if hours > 99 {
            return Err("timestamp out of range");
        }

        Ok(Timestamp {
            hours: hours as u16,
            minutes: minutes as u16,
            seconds: seconds as u16,
            milliseconds: millis as u16,
        })
    }

    pub fn to_millis(&self) -> u64 {
        self.hours as u64 * 3_600_000
            + self.minutes as u64 * 60_000
            + self.seconds as u64 * 1_000
            + self.milliseconds as u64
    }

    pub fn shift_millis(&self, millis: i64) -> Result<Timestamp, &'static str> {
        let t1 = self.to_millis() as i64;
        let t2 = t1 + millis;
        if t2 < 0 {
            return Err("timestamp can't be negative");
        }

        Timestamp::from_millis(t2 as u64)
    }
}

impl<'a> Srt<'a> {
    /// Construct an empty `Srt`.
    pub fn new() -> Srt<'a> {
        Srt {
            subtitles: Vec::new(),
        }
    }

    pub fn add_subtitle(&mut self, start: Timestamp, end: Timestamp, text: Cow<'a, str>) {
        let sub = Subtitle {
            seq: self.subtitles.len() + 1,
            start,
            end,
            text,
        };

        self.subtitles.push(sub);
    }

    /// Parses an `Srt`.
    pub fn try_parse(mut srt: &str) -> Result<Srt<'_>, &'static str> {
        let mut subtitles = Vec::new();

        // strip optional UTF-8 BOM
        if srt.starts_with("\u{feff}") {
            srt = &srt[3..];
        }

        while !srt.is_empty() {
            // sequence number
            let end = srt
                .find(|c: char| !c.is_ascii_digit())
                .ok_or("invalid sequence number")?;
            let seq = srt[..end].parse().map_err(|_| "expected sequence number")?;
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

    pub fn renumber(&mut self) {
        for (i, sub) in self.subtitles.iter_mut().enumerate() {
            sub.seq = i + 1;
        }
    }

    pub fn serialize(&self) -> String {
        let mut s = String::new();

        for subtitle in &self.subtitles {
            let _ = writeln!(&mut s, "{}", subtitle.seq);
            let _ = writeln!(&mut s, "{} --> {}", subtitle.start, subtitle.end);
            let _ = writeln!(&mut s, "{}\n", subtitle.text);
        }

        s
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
        assert_eq!(
            Timestamp {
                hours: 2,
                minutes: 1,
                seconds: 3,
                milliseconds: 430
            },
            srt.subtitles[1].end
        );

        assert_eq!(data, srt.serialize());
    }

    #[test]
    fn timestamp_from_millis() {
        let ts = Timestamp::from_millis(177428182).unwrap();

        assert_eq!(
            Timestamp {
                hours: 49,
                minutes: 17,
                seconds: 8,
                milliseconds: 182
            },
            ts
        );
    }

    #[test]
    fn timestamp_from_millis_overflow() {
        let ts = Timestamp::from_millis(9999999999999999);
        assert!(ts.is_err());
    }

    #[test]
    fn timestamp_to_millis() {
        let ts = Timestamp {
            hours: 49,
            minutes: 17,
            seconds: 8,
            milliseconds: 182,
        };

        assert_eq!(177428182, ts.to_millis());
    }

    #[test]
    fn timestamp_shift_millis() {
        let t1 = Timestamp::from_millis(12345).unwrap();

        assert_eq!(12346, t1.shift_millis(1).unwrap().to_millis());
        assert_eq!(12344, t1.shift_millis(-1).unwrap().to_millis());
        assert_eq!(0, t1.shift_millis(-12345).unwrap().to_millis());
        assert!(t1.shift_millis(-12346).is_err());
        assert!(t1.shift_millis(9999999999999999).is_err());
    }

    #[test]
    fn timestamp_ordering() {
        assert!(Timestamp::from_millis(0).unwrap() < Timestamp::from_millis(1).unwrap());
        assert!(Timestamp::from_millis(1).unwrap() > Timestamp::from_millis(0).unwrap());
        assert!(Timestamp::from_millis(1).unwrap() == Timestamp::from_millis(1).unwrap());
        assert!(
            Timestamp::from_millis(1234567).unwrap() < Timestamp::from_millis(1234568).unwrap()
        );
        assert!(
            Timestamp::from_millis(1234568).unwrap() > Timestamp::from_millis(1234567).unwrap()
        );
        assert!(
            Timestamp::from_millis(1234568).unwrap() == Timestamp::from_millis(1234568).unwrap()
        );
    }

    #[test]
    fn timestamp_display() {
        assert_eq!(
            "00:00:00,000",
            Timestamp::from_millis(0).unwrap().to_string()
        );
        assert_eq!(
            "00:00:00,001",
            Timestamp::from_millis(1).unwrap().to_string()
        );
        assert_eq!(
            "49:17:08,182",
            Timestamp::from_millis(177428182).unwrap().to_string()
        );
    }
}
