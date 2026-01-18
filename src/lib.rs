use std::{
    borrow::Cow,
    cmp::Ordering,
    error::Error,
    fmt::{self, Display, Write},
};

/// Error type for SRT parsing and manipulation operations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SrtError {
    /// Input ended unexpectedly.
    UnexpectedEof,
    /// Invalid or missing sequence number.
    InvalidSequenceNumber { position: usize },
    /// Timestamp format is invalid (expected `HH:MM:SS,mmm`).
    InvalidTimestamp { position: usize },
    /// Timestamp value exceeds maximum representable range (99:59:59,999).
    TimestampOutOfRange,
    /// Shift operation would result in a negative timestamp.
    NegativeTimestamp,
    /// Expected ` --> ` separator between timestamps.
    ExpectedTimeSeparator { position: usize },
    /// Expected whitespace or newline character.
    ExpectedNewline { position: usize },
}

impl Display for SrtError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SrtError::UnexpectedEof => write!(f, "unexpected end of input"),
            SrtError::InvalidSequenceNumber { position } => {
                write!(f, "invalid sequence number at byte {position}")
            }
            SrtError::InvalidTimestamp { position } => {
                write!(f, "invalid timestamp at byte {position}")
            }
            SrtError::TimestampOutOfRange => {
                write!(f, "timestamp exceeds maximum range (99:59:59,999)")
            }
            SrtError::NegativeTimestamp => {
                write!(f, "operation would result in negative timestamp")
            }
            SrtError::ExpectedTimeSeparator { position } => {
                write!(f, "expected ' --> ' at byte {position}")
            }
            SrtError::ExpectedNewline { position } => {
                write!(f, "expected newline at byte {position}")
            }
        }
    }
}

impl Error for SrtError {}

/// A convenient Result type alias for SRT operations.
pub type Result<T> = std::result::Result<T, SrtError>;

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
    pub fn from_millis(mut millis: u64) -> Result<Self> {
        let hours = millis / (3_600_000);
        millis -= hours * 3_600_000;
        let minutes = millis / (60_000);
        millis -= minutes * 60_000;
        let seconds = millis / 1_000;
        millis -= seconds * 1_000;

        if hours > 99 {
            return Err(SrtError::TimestampOutOfRange);
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

    pub fn shift_millis(&self, millis: i64) -> Result<Timestamp> {
        let t1 = self.to_millis() as i64;
        let t2 = t1 + millis;
        if t2 < 0 {
            return Err(SrtError::NegativeTimestamp);
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
    pub fn try_parse(mut srt: &str) -> Result<Srt<'_>> {
        let mut subtitles = Vec::new();

        let original = srt;
        let pos = |remaining: &str| original.len() - remaining.len();

        srt = strip_prefix_bom(srt);
        while !srt.is_empty() {
            // sequence number
            let end = srt
                .find(|c: char| !c.is_ascii_digit())
                .ok_or(SrtError::InvalidSequenceNumber { position: pos(srt) })?;
            let seq = srt[..end]
                .parse()
                .map_err(|_| SrtError::InvalidSequenceNumber { position: pos(srt) })?;
            srt = &srt[end..];
            let discard = newline_discard(srt, pos(srt))?;
            srt = &srt[discard..];

            // time start
            let start_time = parse_time(srt, pos(srt))?;
            srt = &srt[12..];

            // time separator
            let b = srt.as_bytes();
            if b.len() < 5 {
                return Err(SrtError::UnexpectedEof);
            }
            if !matches!(&b[0..5], b" --> ") {
                return Err(SrtError::ExpectedTimeSeparator { position: pos(srt) });
            }
            srt = &srt[5..];

            // time end
            let end_time = parse_time(srt, pos(srt))?;
            srt = &srt[12..];
            let discard = newline_discard(srt, pos(srt))?;
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

    pub fn resequence(&mut self) {
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

fn parse_time(s: &str, position: usize) -> Result<Timestamp> {
    let b = s.as_bytes();
    if b.len() < 12 {
        return Err(SrtError::UnexpectedEof);
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
        return Err(SrtError::InvalidTimestamp { position });
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

fn newline_discard(s: &str, position: usize) -> Result<usize> {
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
                return Err(SrtError::ExpectedNewline {
                    position: position + discard,
                });
            }
            '\n' => {
                discard += 1;
                break;
            }
            _ => {
                return Err(SrtError::ExpectedNewline {
                    position: position + discard,
                });
            }
        }
    }
    Ok(discard)
}

fn strip_prefix_bom(s: &str) -> &str {
    s.strip_prefix("\u{feff}").unwrap_or(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_empty_input() {
        let srt = Srt::try_parse("").unwrap();
        assert!(srt.subtitles.is_empty());
    }

    #[test]
    fn parse_with_bom() {
        let data = "\u{feff}1\n00:00:00,000 --> 00:00:01,000\nHello\n\n";
        let srt = Srt::try_parse(data).unwrap();
        assert_eq!(1, srt.subtitles.len());
    }

    #[test]
    fn parse_crlf_line_endings() {
        let data = "1\r\n00:00:00,000 --> 00:00:01,000\r\nHello\r\n\r\n";
        let srt = Srt::try_parse(data).unwrap();
        assert_eq!(1, srt.subtitles.len());
        assert_eq!("Hello", srt.subtitles[0].text);
    }

    #[test]
    fn parse_single_subtitle() {
        let data = "1\n00:00:00,000 --> 00:00:01,000\nSingle line\n\n";
        let srt = Srt::try_parse(data).unwrap();
        assert_eq!(1, srt.subtitles.len());
        assert_eq!("Single line", srt.subtitles[0].text);
    }

    #[test]
    fn parse_multiline_text() {
        let data = "1\n00:00:00,000 --> 00:00:01,000\nLine 1\nLine 2\nLine 3\n\n";
        let srt = Srt::try_parse(data).unwrap();
        assert_eq!("Line 1\nLine 2\nLine 3", srt.subtitles[0].text);
    }

    #[test]
    fn parse_trailing_whitespace_after_timestamp() {
        let data = "1\n00:00:00,000 --> 00:00:01,000   \nHello\n\n";
        let srt = Srt::try_parse(data).unwrap();
        assert_eq!(1, srt.subtitles.len());
    }

    #[test]
    fn parse_no_trailing_newline() {
        // File ends without double newline after last subtitle
        let data = "1\n00:00:00,000 --> 00:00:01,000\nHello";
        let srt = Srt::try_parse(data).unwrap();
        assert_eq!(1, srt.subtitles.len());
        assert_eq!("Hello", srt.subtitles[0].text);
    }

    #[test]
    fn parse_error_invalid_sequence_number() {
        let data = "abc\n00:00:00,000 --> 00:00:01,000\nHello\n\n";
        let err = Srt::try_parse(data).unwrap_err();
        assert!(matches!(err, SrtError::InvalidSequenceNumber { .. }));
    }

    #[test]
    fn parse_error_missing_sequence_number() {
        let data = "\n00:00:00,000 --> 00:00:01,000\nHello\n\n";
        let err = Srt::try_parse(data).unwrap_err();
        assert!(matches!(err, SrtError::InvalidSequenceNumber { .. }));
    }

    #[test]
    fn parse_error_invalid_timestamp_format() {
        let data = "1\n0:00:00,000 --> 00:00:01,000\nHello\n\n";
        let err = Srt::try_parse(data).unwrap_err();
        assert!(matches!(err, SrtError::InvalidTimestamp { .. }));
    }

    #[test]
    fn parse_error_invalid_minutes() {
        let data = "1\n00:60:00,000 --> 00:00:01,000\nHello\n\n";
        let err = Srt::try_parse(data).unwrap_err();
        assert!(matches!(err, SrtError::InvalidTimestamp { .. }));
    }

    #[test]
    fn parse_error_invalid_seconds() {
        let data = "1\n00:00:60,000 --> 00:00:01,000\nHello\n\n";
        let err = Srt::try_parse(data).unwrap_err();
        assert!(matches!(err, SrtError::InvalidTimestamp { .. }));
    }

    #[test]
    fn parse_error_missing_separator() {
        let data = "1\n00:00:00,000 -> 00:00:01,000\nHello\n\n";
        let err = Srt::try_parse(data).unwrap_err();
        assert!(matches!(err, SrtError::ExpectedTimeSeparator { .. }));
    }

    #[test]
    fn parse_error_truncated_input() {
        let data = "1\n00:00:00,000 --> 00:00";
        let err = Srt::try_parse(data).unwrap_err();
        assert!(matches!(err, SrtError::UnexpectedEof));
    }

    #[test]
    fn parse_error_position_is_accurate() {
        let data = "1\n00:00:00,000 --> 00:XX:01,000\nHello\n\n";
        let err = Srt::try_parse(data).unwrap_err();
        // "1\n00:00:00,000 --> " ; 19 bytes
        assert!(matches!(err, SrtError::InvalidTimestamp { position: 19 }));
    }

    #[test]
    fn srt_new_is_empty() {
        let srt = Srt::new();
        assert!(srt.subtitles.is_empty());
    }

    #[test]
    fn srt_add_subtitle() {
        let mut srt = Srt::new();
        srt.add_subtitle(
            Timestamp::from_millis(0).unwrap(),
            Timestamp::from_millis(1000).unwrap(),
            "Hello".into(),
        );
        srt.add_subtitle(
            Timestamp::from_millis(1000).unwrap(),
            Timestamp::from_millis(2000).unwrap(),
            "World".into(),
        );

        assert_eq!(2, srt.subtitles.len());
        assert_eq!(1, srt.subtitles[0].seq);
        assert_eq!(2, srt.subtitles[1].seq);
    }

    #[test]
    fn srt_serialize_roundtrip() {
        let mut srt = Srt::new();
        srt.add_subtitle(
            Timestamp::from_millis(500).unwrap(),
            Timestamp::from_millis(1500).unwrap(),
            "Test subtitle".into(),
        );

        let serialized = srt.serialize();
        let parsed = Srt::try_parse(&serialized).unwrap();

        assert_eq!(srt, parsed);
    }

    #[test]
    fn timestamp_zero() {
        let ts = Timestamp::default();
        assert_eq!(0, ts.to_millis());
        assert_eq!("00:00:00,000", ts.to_string());
    }

    #[test]
    fn timestamp_max_valid() {
        let ts = Timestamp::from_millis(359_999_999).unwrap(); // 99:59:59,999
        assert_eq!(99, ts.hours);
        assert_eq!(59, ts.minutes);
        assert_eq!(59, ts.seconds);
        assert_eq!(999, ts.milliseconds);
        assert_eq!("99:59:59,999", ts.to_string());
    }

    #[test]
    fn timestamp_just_over_max() {
        let result = Timestamp::from_millis(360_000_000); // 100:00:00,000
        assert!(matches!(result, Err(SrtError::TimestampOutOfRange)));
    }

    #[test]
    fn timestamp_one_millisecond() {
        let ts = Timestamp::from_millis(1).unwrap();
        assert_eq!(0, ts.hours);
        assert_eq!(0, ts.minutes);
        assert_eq!(0, ts.seconds);
        assert_eq!(1, ts.milliseconds);
    }

    #[test]
    fn timestamp_one_second() {
        let ts = Timestamp::from_millis(1000).unwrap();
        assert_eq!(1, ts.seconds);
        assert_eq!(0, ts.milliseconds);
    }

    #[test]
    fn timestamp_one_minute() {
        let ts = Timestamp::from_millis(60_000).unwrap();
        assert_eq!(1, ts.minutes);
        assert_eq!(0, ts.seconds);
    }

    #[test]
    fn timestamp_one_hour() {
        let ts = Timestamp::from_millis(3_600_000).unwrap();
        assert_eq!(1, ts.hours);
        assert_eq!(0, ts.minutes);
    }

    #[test]
    fn timestamp_shift_to_zero() {
        let ts = Timestamp::from_millis(1000).unwrap();
        let shifted = ts.shift_millis(-1000).unwrap();
        assert_eq!(0, shifted.to_millis());
    }

    #[test]
    fn timestamp_shift_negative_result() {
        let ts = Timestamp::from_millis(1000).unwrap();
        let err = ts.shift_millis(-1001).unwrap_err();
        assert!(matches!(err, SrtError::NegativeTimestamp));
    }

    #[test]
    fn timestamp_shift_overflow() {
        let ts = Timestamp::from_millis(359_999_999).unwrap();
        let err = ts.shift_millis(1).unwrap_err();
        assert!(matches!(err, SrtError::TimestampOutOfRange));
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

    #[test]
    fn error_is_error_trait() {
        fn assert_error<E: std::error::Error>() {}
        assert_error::<SrtError>();
    }
}
