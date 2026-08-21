abort_lenguar <- function(message, class, ..., call = NULL, .envir = parent.frame()) {
  fields <- list(...)
  formatted <- tryCatch(
    cli::format_message(message, .envir = .envir),
    error = function(error) {
      plain <- gsub("\\{\\.[^ ]+ ([^}]*)\\}", "\\1", unname(message))
      paste(plain, collapse = "\n")
    }
  )
  condition <- c(
    list(message = formatted, call = call),
    fields
  )
  class(condition) <- c(class, "lenguar_error", "error", "condition")
  stop(condition)
}

abort_input <- function(argument, problem, call = NULL) {
  abort_lenguar(
    c(
      "Invalid {.arg {argument}}.",
      "x" = problem
    ),
    "lenguar_input_error",
    argument = argument,
    problem = problem,
    call = call,
    .envir = environment()
  )
}

# lengua-core surfaces one error type across every operation, so on the R
# side this is a single condition class (not one per lengua_core::Error
# variant) - operation-specific context (store path, template name) is
# attached so callers can act on it without needing to parse the message.
abort_store <- function(op, message, store = NULL, name = NULL, call = NULL) {
  abort_lenguar(
    "{.strong lengua} {op} failed: {message}",
    "lenguar_store_error",
    op = op,
    store = store,
    name = name,
    call = call,
    .envir = environment()
  )
}

# Runs `expr` (a call into the compiled extension) and rethrows any error
# raised there - by extendr itself (e.g. a malformed `fields`/`vars`
# argument) or forwarded from lengua-core (a git/render/frontmatter/not-found
# error) - as a classed `lenguar_store_error` carrying the operation name.
with_store_errors <- function(op, store = NULL, name = NULL, expr) {
  tryCatch(
    expr,
    error = function(e) {
      if (inherits(e, "lenguar_error")) {
        stop(e)
      }
      abort_store(op, conditionMessage(e), store = store, name = name, call = sys.call(-1))
    }
  )
}
