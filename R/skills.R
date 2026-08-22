#' Export lenguar's bundled coding-agent skill files
#'
#' Copies lengua-core's bundled coding-agent skill file(s) (Claude
#' Skills-format `SKILL.md`s) into `directory`, one per skill, each under its
#' own `<directory>/<skill-name>/SKILL.md` subdirectory (a `SKILL.md`'s
#' filename is fixed by the Skills convention) -- point `directory` at
#' `.claude/skills` for Claude Code to auto-discover them, or anywhere else
#' for a different coding agent/tool, or just to inspect the content.
#' Doesn't touch a template library -- there's no `path` argument here.
#'
#' @param directory Target directory. Defaults to the current directory.
#' @param force Overwrite an existing `SKILL.md` at the destination.
#'   Default `FALSE`.
#' @return Invisibly, a `lenguar_skills_result` (has `$directory` and
#'   `$created`, a character vector of the files written).
#' @export
#' @examples
#' \dontrun{
#' lq_export_skills(".claude/skills")
#' }
lq_export_skills <- function(directory = ".", force = FALSE) {
  directory <- as.character(directory)
  result <- with_store_errors(
    "skills",
    store = directory,
    expr = rs_export_skills(directory, isTRUE(force))
  )
  invisible(new_lq_skills_result(result$directory, result$created))
}
