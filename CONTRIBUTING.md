# Contributing 

Thank you for your interest in this library! All contributions to this library are welcome and will be reviewed as soon as possible.

## Guidelines for pull requests

These guidelines are intended to help contributors do the right thing when making changes and to avoid them having to rewrite code at a later point. Do not worry about getting things wrong at first though, since issues can always be addressed in the code review. If you have any questions, feel free to ask as part of the pull request or open an issue.

Try to stick to one improvement, fix, new feature, etc. per pull request. Several pull requests are preferred over one which tries to do multiple things. 

### Style

Do not worry about code style too much, but try to stick to the existing code style, particularly ensuring to use the same amount of indentation (4 spaces) and a limit of 80 characters per line. Otherwise, there are no real preferences for layout within definitions as long as the code is readable. Do not change the formatting of parts of the code that are unrelated to the main improvement you are making.

### Dependencies 

It is preferable not to add any new dependencies unless absolutely necessary. If adding a new dependency is required, add as broad as possible version bounds for it in `package.yaml`.

### Documentation

All changes should be well documented in line with the existing parts of the library and should at least have Haddock comments (including definitions that are not exported). If making changes to the public interface of a module, use `@since` annotations to indicate the version that introduces them. Adding examples to either the Haddock comments or the README is encouraged. 

### Versioning 

This package follows the [Haskell Package Versioning Policy](https://pvp.haskell.org). 

### Changelog

Update the top of `CHANGELOG.md` with a section for the new version and a list of bullet points documenting your changes (documentation changes do not need to be documented in the changelog). If your change does not require a bump of the version number, just add a section titled "Unreleased". 
