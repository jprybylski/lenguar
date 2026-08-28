test_that("pure consumer single upstream lifecycle in R", {
  # 1. Storekeeper initializes upstream store
  upstream <- withr::local_tempdir()
  lq_init(upstream)
  lq_add(
    upstream,
    "guidelines/conduct.md",
    "All team members agree to treat everyone with respect: {{ rule }}.",
    title = "Conduct",
    fields = c(dept = "hr", status = "active")
  )
  lq_tag(upstream, "guidelines/conduct.md", "v1.0")

  # 2. End-user initializes pure consumer library
  consumer <- withr::local_tempdir()
  result <- lq_init(consumer, from_dir = upstream, name = "corp-base")
  expect_s3_class(result, "lenguar_init_result")
  expect_equal(result$source, "corp-base")

  # Verify no local source was created
  expect_false(dir.exists(file.path(consumer, ".lengua", "local")))

  # 3. Query and search
  templates <- lq_list(consumer)
  expect_equal(nrow(templates), 1)
  expect_equal(templates$name, "guidelines/conduct.md")
  expect_equal(templates$source, "corp-base")

  rendered <- lq_get(
    consumer,
    "guidelines/conduct.md",
    vars = c(rule = "listen actively")
  )
  expect_match(rendered, "listen actively")

  search_res <- lq_search(consumer, c(dept = "hr"))
  expect_equal(nrow(search_res), 1)
  expect_equal(search_res$title, "Conduct")

  # 4. Upstream updates template and tags new version
  lq_add(
    upstream,
    "guidelines/conduct.md",
    "All team members agree to treat everyone with dignity and respect: {{ rule }}.",
    title = "Conduct",
    fields = c(dept = "hr", status = "active")
  )
  lq_tag(upstream, "guidelines/conduct.md", "v2.0")

  # 5. Consumer updates
  update_res <- lq_update(consumer)
  expect_s3_class(update_res, "lenguar_update")
  expect_equal(update_res$source, "corp-base")
  expect_equal(update_res$status, "fast-forwarded")

  # 6. Consumer sees updated version and can read both tags
  latest <- lq_get(consumer, "guidelines/conduct.md", vars = c(rule = "integrity"))
  expect_match(latest, "dignity and respect")

  v1_body <- lq_get(consumer, "guidelines/conduct.md", raw = TRUE, rev = "v1.0")
  expect_false(grepl("dignity", v1_body))
  expect_true(grepl("respect", v1_body))

  v2_body <- lq_get(consumer, "guidelines/conduct.md", raw = TRUE, rev = "v2.0")
  expect_true(grepl("dignity and respect", v2_body))
})

test_that("three-tier multi-upstream layering and precedence in R", {
  # Layer 1: Corporate Standards
  corp <- withr::local_tempdir()
  lq_init(corp)
  lq_add(corp, "welcome.md", "Welcome to Acme Corp, {{ name }}!", title = "Corp Welcome", fields = c(tier = "corp"))
  lq_add(corp, "footer.md", "Copyright Acme Corp.", title = "Corp Footer", fields = c(tier = "corp"))
  lq_add(corp, "legal/nda.md", "NDA terms for {{ party }}.", title = "Standard NDA", fields = c(tier = "corp"))

  # Layer 2: Sales Department (overrides welcome.md, adds pitch.md)
  sales <- withr::local_tempdir()
  lq_init(sales)
  lq_add(sales, "welcome.md", "Welcome to Sales, {{ name }}!", title = "Sales Welcome", fields = c(tier = "sales"))
  lq_add(sales, "sales/pitch.md", "Proposal for {{ product }}.", title = "Sales Pitch", fields = c(tier = "sales"))

  # Layer 3: Project Local (overrides footer.md, adds summary.md)
  project <- withr::local_tempdir()
  lq_init(project)
  lq_add(project, "footer.md", "Project Alpha Confidential.", title = "Project Footer", fields = c(tier = "project"))
  lq_add(project, "project/summary.md", "Sprint {{ sprint }} summary.", title = "Project Summary", fields = c(tier = "project"))

  # End user layers: corp -> sales -> project
  lib <- withr::local_tempdir()
  lq_init(lib, from_dir = corp, name = "corp")

  fetch_sales <- lq_fetch(lib, from_dir = sales, name = "sales")
  expect_length(fetch_sales$warnings, 1)
  expect_match(fetch_sales$warnings, "welcome\\.md")

  fetch_proj <- lq_fetch(lib, from_dir = project, name = "project")
  expect_length(fetch_proj$warnings, 1)
  expect_match(fetch_proj$warnings, "footer\\.md")

  # Merged list has 5 entries with winning sources
  all_templates <- lq_list(lib)
  expect_equal(nrow(all_templates), 5)

  # Check winning sources
  expect_equal(all_templates$source[all_templates$name == "welcome.md"], "sales")
  expect_equal(all_templates$source[all_templates$name == "footer.md"], "project")
  expect_equal(all_templates$source[all_templates$name == "legal/nda.md"], "corp")
  expect_equal(all_templates$source[all_templates$name == "sales/pitch.md"], "sales")
  expect_equal(all_templates$source[all_templates$name == "project/summary.md"], "project")

  # Unscoped get resolves to winning source
  welcome_out <- lq_get(lib, "welcome.md", vars = c(name = "Ada"))
  expect_match(welcome_out, "Welcome to Sales, Ada!")

  # Explicit source gets shadowed corp copy
  corp_welcome <- lq_get(lib, "welcome.md", vars = c(name = "Ada"), source = "corp")
  expect_match(corp_welcome, "Welcome to Acme Corp, Ada!")

  # Search by field across layers
  sales_res <- lq_search(lib, c(tier = "sales"))
  expect_setequal(sales_res$name, c("welcome.md", "sales/pitch.md"))
})

test_that("data frame integration and parameterized rendering in R", {
  store <- withr::local_tempdir()
  lq_init(store)
  lq_add(
    store,
    "email/notification.md",
    "Dear {{ name }},\nYour {{ report }} is ready for download at {{ link }}.\nBest,\n{{ sender }}",
    title = "Notification"
  )

  recipients <- data.frame(
    name = c("Alice", "Bob", "Charlie"),
    report = c("Q1 Financials", "Q2 Budget", "Q3 Forecast"),
    link = c("https://example.com/r1", "https://example.com/r2", "https://example.com/r3"),
    stringsAsFactors = FALSE
  )

  rendered_emails <- vapply(seq_len(nrow(recipients)), function(i) {
    row <- recipients[i, ]
    lq_get(
      store,
      "email/notification.md",
      vars = c(name = row$name, report = row$report, link = row$link, sender = "Finance Team")
    )
  }, character(1))

  expect_length(rendered_emails, 3)
  expect_match(rendered_emails[1], "Dear Alice,")
  expect_match(rendered_emails[1], "Q1 Financials")
  expect_match(rendered_emails[2], "Dear Bob,")
  expect_match(rendered_emails[3], "Dear Charlie,")
})
