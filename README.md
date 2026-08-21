# lenguar

<!-- badges: start -->
[![R-CMD-check](https://github.com/jprybylski/lenguar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jprybylski/lenguar/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**lenguar** is an R package (via [extendr](https://extendr.github.io/)) for
[lengua](https://github.com/jprybylski/lengua), a git-backed library for templated text and
snippets: Jinja-style `{{ variable }}` rendering, YAML frontmatter fields, and full history for
free since every change is a real git commit.

lenguar links against `lengua-core` directly through Rust FFI — it does not shell out to the
`lengua` CLI binary.

## Installation

lenguar is not on CRAN. Install the development version from GitHub (requires a
[Rust toolchain](https://rustup.rs), stable channel, since the package compiles `lengua-core`
from source):

```r
# install.packages("pak")
pak::pak("jprybylski/lenguar")
```

`devtools::install_github()` also works, but pass `submodules = TRUE` explicitly since lenguar
vendors `lengua`'s source as a git submodule under `src/rust/lengua`:

```r
devtools::install_github("jprybylski/lenguar", submodules = TRUE)
```

## Usage

Every function is prefixed `lq_`:

```r
library(lenguar)

store <- withr::local_tempdir()
lq_init(store)

lq_add(
  store, "letters/thank-you.md",
  "Dear {{ name }},\n\nThank you for {{ reason }}.",
  title = "Thank You",
  fields = c(tone = "formal")
)

lq_get(store, "letters/thank-you.md", vars = c(name = "Ada", reason = "your thoughtful review"))
#> [1] "Dear Ada,\n\nThank you for your thoughtful review."

lq_search(store, c(tone = "formal"))
#>                    name     title
#> 1 letters/thank-you.md Thank You

lq_log(store, "letters/thank-you.md")
#>                                     commit           message
#> 1 <full sha>                              add/update template
```

See `vignette("lenguar")` (or the [pkgdown site](https://jprybylski.github.io/lenguar/)) for a
fuller walkthrough, and [lengua's own docs](https://jprybylski.github.io/lengua/) for the
underlying storage model and template syntax — lenguar is a thin FFI layer, not a
reimplementation, so anything documented there about how templates are stored, tagged, and
versioned applies here unchanged.

## Relationship to lengua

- **lengua** (Rust): the storage engine and standalone CLI. lenguar has no dependency on the
  `lengua` binary — the R package and the CLI are two independent consumers of the same
  `lengua-core` library.
- **lenguar** (this package): ergonomic `lq_*` R functions wrapping raw extendr bindings
  (`R/extendr-wrappers.R`, auto-generated — never hand-edited) to `lengua-core`, plus typed
  result classes (`R/result.R`) and a single classed error surface (`R/conditions.R`) so
  failures from the underlying store are catchable as `lenguar_error` conditions in R.

## License

MIT
