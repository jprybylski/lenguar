# lenguar

# lenguar (development version)

## New features

* Initial release. R bindings (via extendr) to `lengua`, a git-backed
  library for templated text and snippets: `lq_init()`, `lq_add()`,
  `lq_get()`, `lq_list()`, `lq_search()`, `lq_log()`, `lq_diff()`.
* Tagging: `lq_tag()`, `lq_tag_list()`, `lq_tag_rm()` name a specific
  revision of a template (optionally retroactively, via `rev`), without
  duplicating content — matching `lengua` CLI's `tag add`/`list`/`rm`.
* `lq_init()` gained `from_dir`/`from_repo` (with `ref`/`subdir`/`force`) to
  adopt an existing store instead of starting empty, matching `lengua`
  CLI's `init --from-dir`/`--from-repo`. Local sources are adopted via a
  plain filesystem copy, so this works reliably from R regardless of the
  embedding process's signal disposition.
* `lq_get()` gained a `rev` argument to read a template at a prior revision
  or tag instead of the working tree.
* Output for `lq_list()`/`lq_search()`/`lq_log()`/`lq_diff()`/tag results
  goes through `cli`, matching the `lengua` CLI's color palette (respects
  `NO_COLOR`).
