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

# Parses `from_repo`'s value into a full clone URL plus any `subdir`/`ref`
# embedded in it: either a full git URL, passed through unchanged, or the
# `[host/]owner/repo[/subdir][@ref]` shorthand (host defaults to
# `github.com`; an explicit host is how GitHub Enterprise is supported).
# Mirrors lengua-cli's `from_repo.rs` parser so both tools agree on syntax.
parse_from_repo <- function(spec) {
  looks_like_url <- grepl("^(https?|ssh|file)://", spec) ||
    grepl("^git@", spec) ||
    grepl("://", spec, fixed = TRUE)
  if (looks_like_url) {
    return(list(url = spec, subdir = NULL, ref = NULL))
  }

  at_split <- strsplit(spec, "@", fixed = TRUE)[[1]]
  path_part <- at_split[[1]]
  ref <- if (length(at_split) > 1) paste(at_split[-1], collapse = "@") else NULL

  segments <- strsplit(path_part, "/", fixed = TRUE)[[1]]
  segments <- segments[nzchar(segments)]
  if (length(segments) == 0) {
    abort_input("from_repo", "Must be `[host/]owner/repo[/subdir][@ref]` or a URL.")
  }

  host <- "github.com"
  if (length(segments) > 2 && grepl(".", segments[[1]], fixed = TRUE)) {
    host <- segments[[1]]
    segments <- segments[-1]
  }
  if (length(segments) < 2) {
    abort_input("from_repo", "Must be `[host/]owner/repo[/subdir][@ref]` or a URL.")
  }
  owner <- segments[[1]]
  repo <- segments[[2]]
  subdir <- if (length(segments) > 2) paste(segments[-(1:2)], collapse = "/") else NULL

  list(url = sprintf("https://%s/%s/%s.git", host, owner, repo), subdir = subdir, ref = ref)
}

#' Initialize a new lengua template-library store
#'
#' Creates a git repository at `path` with an empty `templates/` directory
#' inside it, ready for [lq_add()] — or, with `from_dir`/`from_repo`, adopts
#' an existing store instead of starting empty.
#'
#' @details
#' `from_dir` (and `from_repo` when it names a local path rather than a
#' genuine remote) adopts the source with a plain filesystem copy — real,
#' independent history and tags, but no network protocol involved. This
#' isn't just an optimization: `gix`'s network-transport clone shells out to
#' a `git-upload-pack` child process even for a local source, and that
#' process fails under R's `SIGPIPE = SIG_IGN` startup disposition (a
#' long-standing R embedding quirk) with "ignoring SIGPIPE signal" — the
#' filesystem copy sidesteps that entirely rather than working around it.
#' `from_repo` against a genuine remote host (`https://`, `ssh://`, ...)
#' still goes through `gix`'s transport, which is socket-based rather than a
#' subprocess and isn't affected. The one remaining edge case: `from_repo`
#' naming a local path *together with* an explicit `ref`/`@ref` still uses
#' the transport-based clone (checking out an arbitrary ref after a plain
#' copy isn't implemented), so that specific combination could still hit the
#' same failure under R — prefer `from_dir` for local sources when you don't
#' need to select a non-default ref.
#'
#' @param path Directory to initialize. Must not already contain a `.git`
#'   directory (unless importing with `subdir` and `force = TRUE`).
#' @param from_dir Use a local directory as the store source instead of
#'   starting empty. Full history and every tag are preserved. Mutually
#'   exclusive with `from_repo`.
#' @param from_repo Clone a git repo as the store source: a full URL, or
#'   `"[host/]owner/repo[/subdir][@ref]"` shorthand (host defaults to
#'   `github.com` — give one explicitly for GitHub Enterprise). Mutually
#'   exclusive with `from_dir`.
#' @param ref Branch or tag to check out (not a commit id). Only valid with
#'   `from_repo`; overrides any `@ref` embedded in its shorthand. See
#'   Details for a residual limitation when combined with a local
#'   `from_repo` path.
#' @param subdir Import only this subdirectory of the resolved source as the
#'   new store. Overrides any subdirectory embedded in `from_repo`'s
#'   shorthand. Unlike a plain `from_dir`/`from_repo` adoption, this imports
#'   the subdirectory's *current* content only — the source's history and
#'   any tags under it are not preserved (there's no lossless way to extract
#'   a subdirectory's history without a full `git subtree`-style rewrite).
#' @param force With `subdir`, allow re-importing into a store that already
#'   exists, overwriting templates with the same name.
#' @return Invisibly, a `lenguar_init_result`.
#' @export
#' @examples
#' store <- withr::local_tempdir()
#' lq_init(store)
#'
#' # adopting an existing store:
#' copy <- withr::local_tempdir()
#' lq_init(copy, from_dir = store)
lq_init <- function(path, from_dir = NULL, from_repo = NULL, ref = NULL,
                     subdir = NULL, force = FALSE) {
  path <- as.character(path)
  if (!is.null(from_dir) && !is.null(from_repo)) {
    abort_input("from_repo", "`from_dir` and `from_repo` are mutually exclusive.")
  }

  resolved_from_repo <- NULL
  resolved_ref <- if (is.null(ref)) NULL else as.character(ref)
  resolved_subdir <- if (is.null(subdir)) NULL else as.character(subdir)
  if (!is.null(from_repo)) {
    parsed <- parse_from_repo(as.character(from_repo))
    resolved_from_repo <- parsed$url
    if (is.null(resolved_ref)) resolved_ref <- parsed$ref
    if (is.null(resolved_subdir)) resolved_subdir <- parsed$subdir
  }

  with_store_errors(
    "init",
    store = path,
    expr = rs_init(
      path,
      if (is.null(from_dir)) NULL else as.character(from_dir),
      resolved_from_repo,
      resolved_ref,
      resolved_subdir,
      isTRUE(force)
    )
  )
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
#' @param rev Read the template as it existed at this revision instead of
#'   the working tree — a tag name (see [lq_tag()]), or any revspec lengua
#'   understands (e.g. `"HEAD~1"`, a commit sha).
#' @return A length-1 character vector: the rendered (or raw) body.
#' @export
#' @examples
#' store <- withr::local_tempdir()
#' lq_init(store)
#' lq_add(store, "hello.md", "Hi {{ name }}!")
#' lq_get(store, "hello.md", vars = c(name = "Ada"))
lq_get <- function(path, name, vars = NULL, raw = FALSE, rev = NULL) {
  path <- as.character(path)
  name <- as.character(name)
  check_named_character(vars, "vars")
  with_store_errors(
    "get",
    store = path,
    name = name,
    expr = rs_get(path, name, vars, isTRUE(raw), if (is.null(rev)) NULL else as.character(rev))
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

#' Tag a specific revision of a template
#'
#' Points a named tag at `name`'s current revision, or at `rev` — this is
#' how you retroactively tag a *prior* revision, e.g. tagging the version
#' before your latest edit. These are lengua's own tags
#' (`refs/lengua/tags/<name>/<tag>`), not git tags: they're scoped per
#' template, so the same tag name can exist independently on several
#' templates. A tag name works anywhere [lq_get()]'s `rev` or [lq_diff()]'s
#' `from`/`to` accept a revision.
#'
#' @param path Path to the store.
#' @param name Template name.
#' @param tag Tag name. Can't be `"HEAD"` (any casing) or look like a commit
#'   id — both would be ambiguous as a revision.
#' @param rev Revision to tag instead of the current `HEAD`.
#' @param force Overwrite the tag if it already exists.
#' @return Invisibly, a `lenguar_tag_result` (has `$name`, `$tag`, `$commit`).
#' @export
#' @examples
#' store <- withr::local_tempdir()
#' lq_init(store)
#' lq_add(store, "letter.md", "We had so much fun.", message = "past tense")
#' lq_add(store, "letter.md", "We will have so much fun.", message = "future tense")
#' lq_tag(store, "letter.md", "tense-future")
#' lq_tag(store, "letter.md", "tense-past", rev = "HEAD~1")
lq_tag <- function(path, name, tag, rev = NULL, force = FALSE) {
  path <- as.character(path)
  name <- as.character(name)
  tag <- as.character(tag)
  commit <- with_store_errors(
    "tag",
    store = path,
    name = name,
    expr = rs_tag(path, name, tag, if (is.null(rev)) NULL else as.character(rev), isTRUE(force))
  )
  invisible(new_lq_tag_result(name, tag, commit))
}

#' List every tag on a template
#'
#' @param path Path to the store.
#' @param name Template name.
#' @return A `lenguar_tags` data frame with `tag` and `commit` columns.
#' @export
#' @examples
#' store <- withr::local_tempdir()
#' lq_init(store)
#' lq_add(store, "letter.md", "v1")
#' lq_tag(store, "letter.md", "final")
#' lq_tag_list(store, "letter.md")
lq_tag_list <- function(path, name) {
  path <- as.character(path)
  name <- as.character(name)
  df <- with_store_errors("tag_list", store = path, name = name, expr = rs_tag_list(path, name))
  new_lq_tags(df)
}

#' Remove a tag from a template
#'
#' @param path Path to the store.
#' @param name Template name.
#' @param tag Tag name.
#' @return Invisibly, `NULL`.
#' @export
#' @examples
#' store <- withr::local_tempdir()
#' lq_init(store)
#' lq_add(store, "letter.md", "v1")
#' lq_tag(store, "letter.md", "final")
#' lq_tag_rm(store, "letter.md", "final")
lq_tag_rm <- function(path, name, tag) {
  path <- as.character(path)
  name <- as.character(name)
  tag <- as.character(tag)
  with_store_errors("tag_rm", store = path, name = name, expr = rs_tag_rm(path, name, tag))
  invisible(NULL)
}
