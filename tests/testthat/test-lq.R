test_that("init creates a store", {
  store <- withr::local_tempdir()
  result <- lq_init(store)
  expect_s3_class(result, "lenguar_init_result")
  expect_true(dir.exists(file.path(store, ".git")))
  expect_true(dir.exists(file.path(store, "templates")))
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
