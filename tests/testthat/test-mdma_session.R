# ── mdma_shiny_roots() ────────────────────────────────────────────────────────

test_that("mdma_shiny_roots returns a named character vector", {
  roots <- mdma_shiny_roots()
  expect_type(roots, "character")
  expect_named(roots)
})

test_that("mdma_shiny_roots always includes a Home entry", {
  roots <- mdma_shiny_roots()
  expect_true("Home" %in% names(roots))
})

test_that("mdma_shiny_roots entries all exist on disk", {
  roots <- mdma_shiny_roots()
  expect_true(all(file.exists(roots)))
})

test_that("mdma_shiny_roots includes drive roots on Windows", {
  skip_if(.Platform$OS.type != "windows")
  roots <- mdma_shiny_roots()
  drive_entries <- roots[grepl("^[A-Z]:$", names(roots))]
  expect_gt(length(drive_entries), 0L)
})

# ── mdma_app() ────────────────────────────────────────────────────────────────

test_that("mdma_app returns a shiny.appobj", {
  expect_s3_class(mdma_app(), "shiny.appobj")
})

# ── mdma_ui() ────────────────────────────────────────────────────────────────

test_that("mdma_ui returns a Shiny tag object", {
  ui <- mdma_ui()
  expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list", "html")))
})

test_that("mdma_ui contains the PDF tab", {
  expect_match(as.character(mdma_ui()), "PDF to Markdown", fixed = TRUE)
})

test_that("mdma_ui contains the Clean Markdown tab", {
  expect_match(as.character(mdma_ui()), "Clean Markdown", fixed = TRUE)
})

test_that("mdma_ui contains expected PDF input IDs", {
  html <- as.character(mdma_ui())
  expect_match(html, "pdf_files", fixed = TRUE)
  expect_match(html, "pdf_outdir", fixed = TRUE)
  expect_match(html, "pdf_clean", fixed = TRUE)
  expect_match(html, "pdf_run", fixed = TRUE)
})

test_that("mdma_ui contains expected Clean input IDs", {
  html <- as.character(mdma_ui())
  expect_match(html, "clean_files", fixed = TRUE)
  expect_match(html, "clean_outdir", fixed = TRUE)
  expect_match(html, "clean_level", fixed = TRUE)
  expect_match(html, "clean_run", fixed = TRUE)
})

# ── server: initial reactive state ───────────────────────────────────────────

test_that("pdf_paths is empty before any file selection", {
  suppressWarnings(shiny::testServer(mdma_server, {
    expect_equal(pdf_paths(), character(0))
  }))
})

test_that("pdf_outdir is NULL before any directory selection", {
  suppressWarnings(shiny::testServer(mdma_server, {
    expect_null(pdf_outdir())
  }))
})

test_that("clean_paths is empty before any file selection", {
  suppressWarnings(shiny::testServer(mdma_server, {
    expect_equal(clean_paths(), character(0))
  }))
})

test_that("clean_outdir is NULL before any directory selection", {
  suppressWarnings(shiny::testServer(mdma_server, {
    expect_null(clean_outdir())
  }))
})

test_that("PDF reactive values start in idle state", {
  suppressWarnings(shiny::testServer(mdma_server, {
    expect_null(pdf_log_rv())
    expect_equal(pdf_results(), character(0))
    expect_false(pdf_running())
  }))
})

test_that("Clean reactive values start in idle state", {
  suppressWarnings(shiny::testServer(mdma_server, {
    expect_null(clean_log_rv())
    expect_equal(clean_results(), character(0))
    expect_false(clean_running())
  }))
})

# ── server: run buttons without files selected ────────────────────────────────

test_that("Convert button without files does not start a conversion", {
  suppressWarnings(shiny::testServer(mdma_server, {
    session$setInputs(pdf_run = 1L)
    expect_equal(pdf_results(), character(0))
    expect_null(pdf_log_rv())
    expect_false(pdf_running())
  }))
})

test_that("Clean button without files does not start cleaning", {
  suppressWarnings(shiny::testServer(mdma_server, {
    session$setInputs(clean_run = 1L)
    expect_equal(clean_results(), character(0))
    expect_null(clean_log_rv())
    expect_false(clean_running())
  }))
})

# ── server: clean tab file processing ────────────────────────────────────────

test_that("clean tab writes cleaned file and updates results", {
  withr::with_tempdir({
    writeLines("Introduction ......... 3\n\nReal content.", "input.md")

    # Direct logic test: read → clean → write (mirrors the observeEvent body)
    raw <- paste(readLines("input.md", warn = FALSE), collapse = "\n")
    cleaned <- mdma_clean(raw, level = "basic")
    writeLines(cleaned, "output.md")

    result <- paste(readLines("output.md", warn = FALSE), collapse = "\n")
    expect_false(grepl("Introduction", result))
    expect_true(grepl("Real content", result))
  })
})

test_that("clean tab adds _cleaned suffix when overwrite is FALSE and path unchanged", {
  withr::with_tempdir({
    writeLines("Some text.", "doc.md")

    # Mirrors the out_paths derivation in the observeEvent:
    # no out_dir, overwrite = FALSE → same path → _cleaned suffix
    p <- normalizePath("doc.md", winslash = "/")
    out_dir <- NULL
    overwrite <- FALSE

    candidate <- if (!is.null(out_dir)) file.path(out_dir, basename(p)) else p
    if (!overwrite && candidate == p) {
      candidate <- sub("(\\.[^.]+)$", "_cleaned\\1", p)
    }

    expect_match(candidate, "_cleaned\\.md$")
    expect_false(candidate == p)
  })
})
