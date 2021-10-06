# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## Added

- Support for parameters inside components and operations
- Support for responses inside components and operations
- Support for ad-hoc object and enum definitions (inside e.g. Parameters, RequestBody, Response)
- A bunch of tests

## Changed

- Fetch: rename `ClientConfig` -> `FetchConfig`

### Removed

- Enum Generator

## [0.1.0] - 2021-09-27

### Added

- Initial release! Successfuly generates clients for the famous Petstore API as well as other non-public APIs.
- Fetch, Interface and Enum generator
- Path, query, headers parameters support
- Bearer token auth support
