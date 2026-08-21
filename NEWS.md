# lenguar

# lenguar 0.1.0

## Breaking changes

* `lq_init()`'s on-disk layout moves from `<path>/{.git,templates/}` to
  `<path>/.lengua/<source>/{.git,templates/}`, matching `lengua` CLI's
  `.lengua/` restructure (lengua issue #3). `path` is now a *library* that
  can hold more than one named *source* instead of a single git repo — the
  unit lenguar has always managed (`.git` + `templates/`) is unchanged,
  it's just nested one level deeper. Old-layout stores are not migrated.

## New features

* `lq_fetch()` adds another source to an already-initialized library, using
  the same `from_dir`/`from_repo`/`ref`/`subdir`/`force` arguments as
  `lq_init()` — this is how a library pools templates from more than one
  existing store without merging their git history.
* `lq_update()` refreshes one (`source =`) or every source from its
  recorded origin via a fast-forward-only git fetch (never a hard reset);
  returns a `lenguar_update` data frame reporting every source's outcome
  rather than stopping at the first failure.
* `source` argument on `lq_add()`/`lq_get()`/`lq_list()`/`lq_search()`/
  `lq_log()`/`lq_diff()`/`lq_tag()`/`lq_tag_list()`/`lq_tag_rm()`: required
  to disambiguate a write or single-source read once a library has more
  than one source; optional on `lq_get()`/`lq_list()`/`lq_search()` to read
  one source directly instead of the merged view.
* `lq_list()`/`lq_search()` gain a `source` column, and merge across every
  source (last-fetched wins on a name collision) unless scoped with
  `source =`.
* `lq_init()` gains a `name` argument to name the library's first source
  (defaults to `"local"` when starting empty).

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
