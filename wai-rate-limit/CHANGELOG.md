# Changelog for `wai-rate-limit`

## 0.3

- Use the `Second` type from `Data.Time.Units` to more accurately represent the expected arguments for the rate-limiting strategies.

## 0.2

- Change how errors are handled by `Backend`s to use exceptions

## 0.1

- Support for fixed and sliding window strategies
