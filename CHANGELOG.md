# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/).

## [Unreleased]

### Fixed
- Empty JSON objects round-trip correctly: `{}` no longer reads as the
  empty list and is rewritten as `[]` by `json-write`. It now reads as
  the distinct `json-empty-object` value (tested with
  `json-empty-object?`), mirroring how `json-null`/`json-null?` handle
  JSON `null`.

## [0.1.0] - 2026-07-26

### Added
- JSON parser and serializer — `json-read`/`json-read-string` and
  `json-write`/`json-write-string`, with `json-null`/`json-null?` for
  distinguishing JSON `null`
- Pure Scheme implementation, no C dependencies or build step
- `kaappi.pkg` manifest for the thottam package manager
- CI workflow for automated testing
