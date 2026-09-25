# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- crates.io `categories` and `keywords` in the package metadata.

## [0.1.1] - 2026-01-20

### Added

- `homepage`, `repository`, and `documentation` links in the package metadata.

## [0.1.0] - 2026-01-20

### Added

- `Srt::try_parse` for parsing SRT text without copying subtitle text. Handles
  LF and CRLF line endings, an optional UTF-8 BOM, trailing whitespace after
  timestamps, and files that don't end with a blank line.
- `Srt::serialize` for writing SRT text.
- `Srt::new` and `Srt::add_subtitle` for building subtitles programmatically.
- `Srt::resequence` for renumbering subtitles sequentially.
- `Srt::subtitles`, `Srt::iter`, `Srt::iter_mut`, and `IntoIterator` for
  `Srt`, `&Srt`, and `&mut Srt`.
- `Subtitle` getters and setters for the sequence number, start and end
  timestamps, and text.
- `Timestamp` with `from_millis`, `to_millis`, overflow-checked
  `shift_millis`, `Display`, and ordering.
- `SrtError` error type, reporting the byte offset of parse errors where
  applicable.

[unreleased]: https://github.com/smackysnacks/skrt/compare/v0.1.1...HEAD
[0.1.1]: https://github.com/smackysnacks/skrt/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/smackysnacks/skrt/releases/tag/v0.1.0
