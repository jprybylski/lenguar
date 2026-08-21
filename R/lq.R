check_named_character <- function(x, arg) {
  if (is.null(x)) {
    return(invisible(NULL))
  }
  if (!is.character(x)) {
    abort_input(arg, "Must be a named character vector or `NULL`.")
  }
  nms <- names(x)
  if (length(x) > 0 && (is.null(nms) || any(!nzchar(nms)) || anyNA(nms))) {
    abort_input(arg, "Every element must be named.")
  }
  invisible(NULL)
}

#' Initialize a new lengua template-library store
#'
#' Creates a git repository at `path` with an empty `templates/` directory
#' inside it, ready for [lq_add()].
#'
#' @param path Directory to initialize. Must not already contain a `.git`
#'   directory.
#' @return Invisibly, a `lenguar_init_result`.
#' @export
#' @examples
#' store <- withr::local_tempdir()
#' lq_init(store)
lq_init <- function(path) {
  path <- as.character(path)
  with_store_errors("init", store = path, expr = rs_init(path))
  invisible(new_lq_init_result(path))
}

#' Add or update a template
#'
#' Writes `body` (with optional YAML frontmatter fields) to `name` under
#' `path`'s `templates/` directory and commits the change. lengua is
#' git-backed, so every call to `lq_add()` for the same `name` is a new
#' point in that template's history (see [lq_log()] / [lq_diff()]).
#'
#' @param path Path to the store (as created by [lq_init()]).
#' @param name Relative path/id under `templates/`, e.g. `"letters/thank-you.md"`.
#' @param body The template body, using `{{ variable }}` interpolation
#'   ([minijinja](https://github.com/mitsuhiko/minijinja) / Jinja2 syntax).
#' @param title Optional frontmatter title.
#' @param fields Optional named character vector of additional frontmatter
#'   fields, e.g. `c(tone = "formal")`.
#' @param message Commit message.
#' @return Invisibly, a `lenguar_add_result` (has `$name` and `$commit`).
#' @export
#' @examples
#' store <- withr::local_tempdir()
#' lq_init(store)
#' lq_add(store, "hello.md", "Hi {{ name }}!", title = "Hello")
lq_add <- function(path, name, body, title = NULL, fields = NULL,
                    message = "add/update template") {
  path <- as.character(path)
  name <- as.character(name)
  check_named_character(fields, "fields")
  commit <- with_store_errors(
    "add",
    store = path,
    name = name,
    expr = rs_add(path, name, title, fields, as.character(body), as.character(message))
  )
  invisible(new_lq_add_result(name, commit))
}

#' Render a template
#'
#' @param path Path to the store.
#' @param name Template name, as passed to [lq_add()].
#' @param vars Named character vector of template variables, e.g.
#'   `c(name = "Ada")`.
#' @param raw If `TRUE`, return the unrendered body instead of substituting
#'   `vars`.
#' @return A length-1 character vector: the rendered (or raw) body.
#' @export
#' @examples
#' store <- withr::local_tempdir()
#' lq_init(store)
#' lq_add(store, "hello.md", "Hi {{ name }}!")
#' lq_get(store, "hello.md", vars = c(name = "Ada"))
lq_get <- function(path, name, vars = NULL, raw = FALSE) {
  path <- as.character(path)
  name <- as.character(name)
  check_named_character(vars, "vars")
  with_store_errors(
    "get",
    store = path,
    name = name,
    expr = rs_get(path, name, vars, isTRUE(raw))
  )
}

#' List every template in a store
#'
#' @param path Path to the store.
#' @return A `lenguar_templates` data frame with `name` and `title` columns,
#'   sorted by name.
#' @export
#' @examples
#' store <- withr::local_tempdir()
#' lq_init(store)
#' lq_add(store, "hello.md", "Hi {{ name }}!", title = "Hello")
#' lq_list(store)
lq_list <- function(path) {
  path <- as.character(path)
  df <- with_store_errors("list", store = path, expr = rs_list(path))
  new_lq_templates(df)
}

#' Search templates by frontmatter field
#'
#' @param path Path to the store.
#' @param fields Named character vector of frontmatter fields to match;
#'   results must match every field (AND).
#' @return A `lenguar_templates` data frame with `name` and `title` columns.
#' @export
#' @examples
#' store <- withr::local_tempdir()
#' lq_init(store)
#' lq_add(store, "formal.md", "Dear Sir,", fields = c(tone = "formal"))
#' lq_search(store, c(tone = "formal"))
lq_search <- function(path, fields) {
  path <- as.character(path)
  check_named_character(fields, "fields")
  if (is.null(fields) || length(fields) == 0) {
    abort_input("fields", "Must supply at least one field to search on.")
  }
  df <- with_store_errors("search", store = path, expr = rs_search(path, fields))
  new_lq_templates(df)
}

#' Show a template's commit history
#'
#' @param path Path to the store.
#' @param name Template name.
#' @return A `lenguar_log` data frame with `commit` and `message` columns,
#'   newest first.
#' @export
#' @examples
#' store <- withr::local_tempdir()
#' lq_init(store)
#' lq_add(store, "hello.md", "v1", message = "first cut")
#' lq_log(store, "hello.md")
lq_log <- function(path, name) {
  path <- as.character(path)
  name <- as.character(name)
  df <- with_store_errors("log", store = path, name = name, expr = rs_log(path, name))
  new_lq_log(df)
}

#' Diff a template between two revisions
#'
#' @param path Path to the store.
#' @param name Template name.
#' @param from,to Revisions to compare (any revspec lengua understands, e.g.
#'   `"HEAD"`, `"HEAD~1"`, a commit sha).
#' @return A `lenguar_diff` data frame with `tag` (`"equal"`/`"insert"`/`"delete"`)
#'   and `line` columns.
#' @export
#' @examples
#' store <- withr::local_tempdir()
#' lq_init(store)
#' lq_add(store, "hello.md", "v1", message = "v1")
#' lq_add(store, "hello.md", "v2", message = "v2")
#' lq_diff(store, "hello.md")
lq_diff <- function(path, name, from = "HEAD~1", to = "HEAD") {
  path <- as.character(path)
  name <- as.character(name)
  df <- with_store_errors(
    "diff",
    store = path,
    name = name,
    expr = rs_diff(path, name, as.character(from), as.character(to))
  )
  new_lq_diff(df)
}
