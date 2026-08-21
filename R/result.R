new_lq_init_result <- function(path) {
  structure(list(path = path), class = "lenguar_init_result")
}

#' @export
print.lenguar_init_result <- function(x, ...) {
  cli::cli_text("{.strong lengua init}: created a store at {.path {x$path}}")
  invisible(x)
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
  prefix <- c(equal = "  ", insert = "+ ", delete = "- ")
  colors <- list(
    equal = identity,
    insert = if (cli::num_ansi_colors() > 1) cli::col_green else identity,
    delete = if (cli::num_ansi_colors() > 1) cli::col_red else identity
  )
  for (i in seq_len(nrow(x))) {
    tag <- x$tag[[i]]
    line <- paste0(prefix[[tag]], x$line[[i]])
    cat(colors[[tag]](line), "\n", sep = "")
  }
  invisible(x)
}
