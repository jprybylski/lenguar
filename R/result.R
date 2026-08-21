new_lq_init_result <- function(path, source) {
  structure(list(path = path, source = source), class = "lenguar_init_result")
}

#' @export
print.lenguar_init_result <- function(x, ...) {
  cli::cli_text(
    "{.strong lengua init}: created a library at {.path {x$path}} (source {.field {x$source}})"
  )
  invisible(x)
}

new_lq_fetch_result <- function(source, warnings) {
  structure(list(source = source, warnings = warnings), class = "lenguar_fetch_result")
}

#' @export
print.lenguar_fetch_result <- function(x, ...) {
  cli::cli_text("{.strong lengua fetch}: added source {.field {x$source}}")
  for (w in x$warnings) {
    cli::cli_alert_warning(w)
  }
  invisible(x)
}

new_lq_update <- function(df) {
  structure(df, class = c("lenguar_update", class(df)))
}

#' @export
print.lenguar_update <- function(x, ...) {
  cli::cli_text("{.strong lengua update}: {nrow(x)} source{?s}")
  NextMethod()
}

new_lq_add_result <- function(name, commit) {
  structure(list(name = name, commit = commit), class = "lenguar_add_result")
}

#' @export
print.lenguar_add_result <- function(x, ...) {
  cli::cli_text("{.strong lengua add}: {x$name} ({substr(x$commit, 1, 12)})")
  invisible(x)
}

#' @export
format.lenguar_add_result <- function(x, ...) x$commit

new_lq_templates <- function(df) {
  structure(df, class = c("lenguar_templates", class(df)))
}

#' @export
print.lenguar_templates <- function(x, ...) {
  cli::cli_text("{.strong lengua}: {nrow(x)} template{?s}")
  NextMethod()
}

new_lq_log <- function(df) {
  structure(df, class = c("lenguar_log", class(df)))
}

#' @export
print.lenguar_log <- function(x, ...) {
  cli::cli_text("{.strong lengua log}: {nrow(x)} commit{?s}")
  NextMethod()
}

new_lq_diff <- function(df) {
  structure(df, class = c("lenguar_diff", class(df)))
}

#' @export
print.lenguar_diff <- function(x, ...) {
  # Color palette mirrors lengua-cli's `output.rs` so both CLIs read as one
  # brand: green = inserted lines, red = deleted lines. `cli::col_green()`/
  # `col_red()` already no-op (emit no ANSI codes) when the terminal/NO_COLOR
  # doesn't support color, so no extra `num_ansi_colors()` gate is needed.
  prefix <- c(equal = "  ", insert = "+ ", delete = "- ")
  color <- list(equal = identity, insert = cli::col_green, delete = cli::col_red)
  for (i in seq_len(nrow(x))) {
    tag <- x$tag[[i]]
    line <- paste0(prefix[[tag]], x$line[[i]])
    cli::cat_line(color[[tag]](line))
  }
  invisible(x)
}

new_lq_tag_result <- function(name, tag, commit) {
  structure(list(name = name, tag = tag, commit = commit), class = "lenguar_tag_result")
}

#' @export
print.lenguar_tag_result <- function(x, ...) {
  cli::cli_text("{.strong lengua tag}: {x$name} @ {substr(x$commit, 1, 12)} as {.field {x$tag}}")
  invisible(x)
}

#' @export
format.lenguar_tag_result <- function(x, ...) x$commit

new_lq_tags <- function(df) {
  structure(df, class = c("lenguar_tags", class(df)))
}

#' @export
print.lenguar_tags <- function(x, ...) {
  cli::cli_text("{.strong lengua tag list}: {nrow(x)} tag{?s}")
  NextMethod()
}
