test_that("init creates a library", {
  store <- withr::local_tempdir()
  result <- lq_init(store)
  expect_s3_class(result, "lenguar_init_result")
  expect_equal(result$source, "local")
  expect_true(file.exists(file.path(store, ".lengua", "sources.toml")))
  expect_true(dir.exists(file.path(store, ".lengua", "local", ".git")))
  expect_true(dir.exists(file.path(store, ".lengua", "local", "templates")))
})

test_that("init with a custom name uses it for the first source", {
  store <- withr::local_tempdir()
  result <- lq_init(store, name = "mine")
  expect_equal(result$source, "mine")
  expect_true(dir.exists(file.path(store, ".lengua", "mine", ".git")))
})

test_that("init rejects an already-initialized store", {
  store <- withr::local_tempdir()
  lq_init(store)
  expect_error(lq_init(store), class = "lenguar_store_error")
})

test_that("add then get renders variables", {
  store <- withr::local_tempdir()
  lq_init(store)

  added <- lq_add(store, "hello.md", "Hi {{ name }}!",
    title = "Hello", fields = c(tone = "casual")
  )
  expect_s3_class(added, "lenguar_add_result")
  expect_equal(added$name, "hello.md")
  expect_match(added$commit, "^[0-9a-f]{40}$")

  rendered <- lq_get(store, "hello.md", vars = c(name = "Ada"))
  expect_equal(rendered, "Hi Ada!")
})

test_that("get --raw returns the unrendered body", {
  store <- withr::local_tempdir()
  lq_init(store)
  lq_add(store, "hello.md", "Hi {{ name }}!")

  expect_equal(lq_get(store, "hello.md", raw = TRUE), "Hi {{ name }}!")
})

test_that("get on a missing template errors", {
  store <- withr::local_tempdir()
  lq_init(store)
  expect_error(lq_get(store, "nope.md"), class = "lenguar_store_error")
})

test_that("list and search reflect added templates", {
  store <- withr::local_tempdir()
  lq_init(store)
  lq_add(store, "formal.md", "Dear Sir,", fields = c(tone = "formal"))
  lq_add(store, "casual.md", "Hey!", fields = c(tone = "casual"))

  all <- lq_list(store)
  expect_s3_class(all, "lenguar_templates")
  expect_s3_class(all, "data.frame")
  expect_setequal(all$name, c("formal.md", "casual.md"))

  formal_only <- lq_search(store, c(tone = "formal"))
  expect_equal(formal_only$name, "formal.md")
})

test_that("search requires at least one field", {
  store <- withr::local_tempdir()
  lq_init(store)
  expect_error(lq_search(store, NULL), class = "lenguar_input_error")
  expect_error(lq_search(store, character()), class = "lenguar_input_error")
})

test_that("fields/vars must be named", {
  store <- withr::local_tempdir()
  lq_init(store)
  expect_error(
    lq_add(store, "x.md", "body", fields = c("formal")),
    class = "lenguar_input_error"
  )
})

test_that("log and diff track history", {
  store <- withr::local_tempdir()
  lq_init(store)
  lq_add(store, "letter.md", "First version.", message = "v1")
  lq_add(store, "letter.md", "Second version.", message = "v2")

  log <- lq_log(store, "letter.md")
  expect_s3_class(log, "lenguar_log")
  expect_equal(nrow(log), 2)
  expect_equal(log$message, c("v2", "v1"))

  diff <- lq_diff(store, "letter.md")
  expect_s3_class(diff, "lenguar_diff")
  expect_true("First version." %in% diff$line[diff$tag == "delete"])
  expect_true("Second version." %in% diff$line[diff$tag == "insert"])
})

test_that("tag add/list/rm roundtrip, including a retroactive tag", {
  store <- withr::local_tempdir()
  lq_init(store)
  lq_add(store, "letter.md", "We had so much fun.", message = "past tense")
  lq_add(store, "letter.md", "We will have so much fun.", message = "future tense")

  future <- lq_tag(store, "letter.md", "tense-future")
  expect_s3_class(future, "lenguar_tag_result")
  expect_equal(future$tag, "tense-future")

  past <- lq_tag(store, "letter.md", "tense-past", rev = "HEAD~1")
  expect_equal(past$tag, "tense-past")

  tags <- lq_tag_list(store, "letter.md")
  expect_s3_class(tags, "lenguar_tags")
  expect_setequal(tags$tag, c("tense-future", "tense-past"))

  expect_equal(lq_get(store, "letter.md", rev = "tense-past"), "We had so much fun.")
  expect_equal(lq_get(store, "letter.md", rev = "tense-future"), "We will have so much fun.")

  lq_tag_rm(store, "letter.md", "tense-past")
  expect_false("tense-past" %in% lq_tag_list(store, "letter.md")$tag)
})

test_that("tag add refuses to overwrite without force", {
  store <- withr::local_tempdir()
  lq_init(store)
  lq_add(store, "letter.md", "v1")

  lq_tag(store, "letter.md", "final")
  expect_error(lq_tag(store, "letter.md", "final"), class = "lenguar_store_error")
  expect_no_error(lq_tag(store, "letter.md", "final", force = TRUE))
})

test_that("init with from_dir adopts an existing store's history and tags", {
  # `from_dir` is a local filesystem copy in lengua-core (not a `gix`
  # network-transport clone), specifically so this works under R: gix's
  # transport-based clone shells out to `git-upload-pack` for local sources
  # too, which fails under R's SIGPIPE = SIG_IGN startup disposition
  # ("ignoring SIGPIPE signal") - see lengua-core's `source.rs` module docs.
  source <- withr::local_tempdir()
  lq_init(source)
  lq_add(source, "letter.md", "v1", message = "v1")
  lq_add(source, "letter.md", "v2", message = "v2")
  lq_tag(source, "letter.md", "final")

  dest <- file.path(withr::local_tempdir(), "adopted")
  result <- lq_init(dest, from_dir = source)
  expect_s3_class(result, "lenguar_init_result")

  expect_equal(nrow(lq_log(dest, "letter.md")), 2)
  expect_equal(lq_tag_list(dest, "letter.md")$tag, "final")
})

test_that("init rejects specifying both from_dir and from_repo", {
  store <- withr::local_tempdir()
  expect_error(
    lq_init(store, from_dir = "somewhere", from_repo = "acme/templates"),
    class = "lenguar_input_error"
  )
})

test_that("fetch adds a second source, add/log/diff/tag then require --source", {
  store <- withr::local_tempdir()
  lq_init(store)
  lq_add(store, "a.md", "local body")

  other <- withr::local_tempdir()
  lq_init(other)
  lq_add(other, "b.md", "other body")

  fetched <- lq_fetch(store, from_dir = other, name = "other")
  expect_s3_class(fetched, "lenguar_fetch_result")
  expect_equal(fetched$source, "other")
  expect_equal(fetched$warnings, character())

  expect_error(lq_add(store, "c.md", "body"), class = "lenguar_store_error")
  lq_add(store, "c.md", "body", source = "local")

  expect_error(lq_log(store, "a.md"), class = "lenguar_store_error")
  expect_equal(nrow(lq_log(store, "a.md", source = "local")), 1)
})

test_that("fetching a name collision reports it and last-fetched wins", {
  store <- withr::local_tempdir()
  lq_init(store)
  lq_add(store, "shared.md", "local body", title = "Local")

  other <- withr::local_tempdir()
  lq_init(other)
  lq_add(other, "shared.md", "other body", title = "Other")

  fetched <- lq_fetch(store, from_dir = other, name = "other")
  expect_length(fetched$warnings, 1)
  expect_match(fetched$warnings, "shared\\.md")

  all <- lq_list(store)
  row <- all[all$name == "shared.md", ]
  expect_equal(row$source, "other")
  expect_equal(row$title, "Other")

  local_row <- lq_get(store, "shared.md", source = "local")
  expect_equal(local_row, "local body")
})

test_that("update fast-forwards a fetched source and reports local as not-updatable", {
  store <- withr::local_tempdir()
  lq_init(store)

  other <- withr::local_tempdir()
  lq_init(other)
  lq_add(other, "a.md", "v1")
  lq_fetch(store, from_dir = other, name = "other")

  lq_add(other, "b.md", "v2")

  result <- lq_update(store)
  expect_s3_class(result, "lenguar_update")
  expect_setequal(result$source, c("local", "other"))
  expect_equal(result$status[result$source == "local"], "not-updatable")
  expect_equal(result$status[result$source == "other"], "fast-forwarded")

  expect_equal(lq_get(store, "b.md", source = "other"), "v2")
})
