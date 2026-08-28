test_that("multi-store creation and isolation in R", {
  base_dir <- withr::local_tempdir()

  legal <- file.path(base_dir, "acme-legal")
  eng <- file.path(base_dir, "acme-eng")

  lq_init(legal)
  lq_init(eng)

  lq_add(legal, "contracts/nda.md", "NDA for {{ party }}.", title = "NDA", fields = c(category = "legal"))
  lq_add(eng, "rfc/template.md", "# RFC: {{ title }}", title = "RFC", fields = c(category = "engineering"))

  legal_list <- lq_list(legal)
  expect_equal(nrow(legal_list), 1)
  expect_equal(legal_list$name, "contracts/nda.md")

  eng_list <- lq_list(eng)
  expect_equal(nrow(eng_list), 1)
  expect_equal(eng_list$name, "rfc/template.md")
})

test_that("semantic tagging release lifecycle in R", {
  store <- withr::local_tempdir()
  lq_init(store)

  # v1 release
  res_v1 <- lq_add(store, "release.md", "Version 1.0 notes: initial feature set.", message = "v1.0.0")
  lq_tag(store, "release.md", "v1.0.0")
  lq_tag(store, "release.md", "latest")

  # v2 release
  res_v2 <- lq_add(store, "release.md", "Version 2.0 notes: upgraded engine and new tools.", message = "v2.0.0")
  lq_tag(store, "release.md", "v2.0.0")
  lq_tag(store, "release.md", "latest", force = TRUE)

  # Retroactive tag on v1
  lq_tag(store, "release.md", "v1.0-legacy", rev = "HEAD~1")

  # Tag listing
  tags <- lq_tag_list(store, "release.md")
  expect_s3_class(tags, "lenguar_tags")
  expect_equal(nrow(tags), 4)
  expect_setequal(tags$tag, c("latest", "v1.0-legacy", "v1.0.0", "v2.0.0"))

  # Verify commits
  latest_commit <- tags$commit[tags$tag == "latest"]
  expect_equal(latest_commit, res_v2$commit)

  legacy_commit <- tags$commit[tags$tag == "v1.0-legacy"]
  expect_equal(legacy_commit, res_v1$commit)

  # Diff between releases
  diff <- lq_diff(store, "release.md", from = "v1.0.0", to = "v2.0.0")
  expect_s3_class(diff, "lenguar_diff")
  expect_true(any(diff$tag == "delete" & grepl("Version 1.0", diff$line)))
  expect_true(any(diff$tag == "insert" & grepl("Version 2.0", diff$line)))

  # Remove tag
  lq_tag_rm(store, "release.md", "v1.0-legacy")
  updated_tags <- lq_tag_list(store, "release.md")
  expect_false("v1.0-legacy" %in% updated_tags$tag)
})

test_that("storekeeper automated CI validation pipeline pattern in R", {
  store <- withr::local_tempdir()
  lq_init(store)

  lq_add(
    store,
    "reports/summary.md",
    "Summary for {{ quarter }}:\nTotal Revenue: {{ revenue }}",
    title = "Quarterly Summary",
    fields = c(schema_version = "1.0", status = "approved")
  )

  lq_add(
    store,
    "emails/alert.md",
    "Alert: {{ message }} (Severity: {{ severity }})",
    title = "System Alert",
    fields = c(schema_version = "1.0", status = "approved")
  )

  # Storekeeper validation:
  templates <- lq_list(store)
  expect_equal(nrow(templates), 2)

  mock_vars <- c(quarter = "Q3", revenue = "$1M", message = "Disk 90% full", severity = "high")

  for (i in seq_len(nrow(templates))) {
    name <- templates$name[i]
    title <- templates$title[i]

    # Check 1: Non-empty title
    expect_true(nzchar(title))

    # Check 2: Raw body can be fetched
    raw_body <- lq_get(store, name, raw = TRUE)
    expect_true(nzchar(raw_body))

    # Check 3: Template renders without error with mock variables
    expect_no_error({
      rendered <- lq_get(store, name, vars = mock_vars)
      expect_true(nzchar(rendered))
    })
  }
})
