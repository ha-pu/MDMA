test_that("non-existent file throws an error", {
  expect_snapshot(pdf_to_md("nonexistent.pdf"), error = TRUE)
})

test_that("non-PDF file throws an error", {
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeLines("hello", tmp)
  # Volatile temp path in message: match on stable substring only
  expect_error(pdf_to_md(tmp), "does not appear to be a PDF file")
})

test_that("existing output without overwrite throws an error", {
  pdf <- file.path(R.home("doc"), "NEWS.pdf")
  skip_if_not(file.exists(pdf))
  tmp_out <- withr::local_tempfile(fileext = ".md")
  writeLines("existing content", tmp_out)
  # Volatile temp path in message: match on stable substring only
  expect_error(pdf_to_md(pdf, output = tmp_out), "Output file already exists")
})

test_that("converts PDF and writes markdown file", {
  pdf <- file.path(R.home("doc"), "NEWS.pdf")
  skip_if_not(file.exists(pdf))
  output <- withr::local_tempfile(fileext = ".md")
  result <- pdf_to_md(pdf, output = output)
  expect_equal(result, output)
  expect_true(file.exists(output))
  expect_gt(length(readLines(output)), 0L)
})

test_that("OCR fallback is triggered when text is below min_chars", {
  pdf <- file.path(R.home("doc"), "NEWS.pdf")
  skip_if_not(file.exists(pdf))
  output <- withr::local_tempfile(fileext = ".md")
  expect_message(
    pdf_to_md(pdf, output = output, min_chars = .Machine$integer.max),
    "falling back to OCR"
  )
  expect_true(file.exists(output))
  expect_gt(length(readLines(output)), 0L)
})

# ── two-column detection ──────────────────────────────────────────────────────

make_page_data <- function(x_positions, y_range = seq(10, 200, by = 10),
                           word_width = 50, word_height = 10) {
  nx <- length(x_positions)
  ny <- length(y_range)
  data.frame(
    x      = rep(x_positions, each = ny),
    y      = rep(y_range, times = nx),
    width  = word_width,
    height = word_height,
    space  = TRUE,
    text   = paste0("w", seq_len(nx * ny))
  )
}

test_that("pdf_detect_two_column returns TRUE for two-column layout", {
  page <- make_page_data(c(10, 300))
  expect_true(pdf_detect_two_column(list(page)))
})

test_that("pdf_detect_two_column returns FALSE for single-column layout", {
  page <- make_page_data(seq(10, 200, by = 10))
  expect_false(pdf_detect_two_column(list(page)))
})

test_that("pdf_detect_two_column returns FALSE when pages have too few words", {
  page <- make_page_data(c(10, 300), y_range = seq(10, 30, by = 10))
  expect_false(pdf_detect_two_column(list(page)))
})

# ── words_to_text ─────────────────────────────────────────────────────────────

test_that("words_to_text groups words into lines and orders left to right", {
  words <- data.frame(
    x = c(10, 50, 10, 50), y = c(10, 10, 25, 25),
    width = 30, height = 10, space = TRUE,
    text = c("Hello", "World", "foo", "bar")
  )
  expect_equal(words_to_text(words), "Hello World\nfoo bar")
})

test_that("words_to_text handles a single word", {
  words <- data.frame(
    x = 10, y = 10, width = 30, height = 10, space = TRUE, text = "only"
  )
  expect_equal(words_to_text(words), "only")
})

test_that("words_to_text handles empty input", {
  words <- data.frame(
    x = integer(), y = integer(), width = integer(),
    height = integer(), space = logical(), text = character()
  )
  expect_equal(words_to_text(words), "")
})

# ── pdf_count_cid_artifacts ──────────────────────────────────────────────────

test_that("pdf_count_cid_artifacts counts CID patterns", {
  expect_equal(pdf_count_cid_artifacts("(cid:0)\n(cid:1)\n(cid:2)"), 3L)
  expect_equal(pdf_count_cid_artifacts("(cid:123) normal (cid:0)"), 2L)
  expect_equal(pdf_count_cid_artifacts("normal text without CID"), 0L)
})

# ── pdf_extract_two_column ────────────────────────────────────────────────────

test_that("pdf_extract_two_column places left column text before right", {
  page <- data.frame(
    x = c(10, 10, 300, 300), y = c(10, 25, 10, 25),
    width = 50, height = 10, space = TRUE,
    text = c("left1", "left2", "right1", "right2")
  )
  result <- pdf_extract_two_column(list(page))
  expect_lt(regexpr("left1", result)[[1L]], regexpr("right1", result)[[1L]])
})

# ── vector input ─────────────────────────────────────────────────────────────

test_that("pdf_to_md vectorises over a path vector", {
  pdf <- file.path(R.home("doc"), "NEWS.pdf")
  skip_if_not(file.exists(pdf))
  out1 <- withr::local_tempfile(fileext = ".md")
  out2 <- withr::local_tempfile(fileext = ".md")
  results <- pdf_to_md(c(pdf, pdf), output = c(out1, out2))
  expect_equal(results, c(out1, out2))
  expect_true(all(file.exists(results)))
})

test_that("pdf_to_md vectorises with default output paths", {
  pdf <- file.path(R.home("doc"), "NEWS.pdf")
  skip_if_not(file.exists(pdf))
  tmp1 <- withr::local_tempfile(fileext = ".pdf")
  tmp2 <- withr::local_tempfile(fileext = ".pdf")
  file.copy(pdf, tmp1)
  file.copy(pdf, tmp2)
  results <- pdf_to_md(c(tmp1, tmp2))
  expect_equal(results, sub("\\.pdf$", ".md", c(tmp1, tmp2), ignore.case = TRUE))
  expect_true(all(file.exists(results)))
})

# ── integration ───────────────────────────────────────────────────────────────

test_that("overwrite = TRUE replaces existing output", {
  pdf <- file.path(R.home("doc"), "NEWS.pdf")
  skip_if_not(file.exists(pdf))
  output <- withr::local_tempfile(fileext = ".md")
  writeLines("old content", output)
  pdf_to_md(pdf, output = output, overwrite = TRUE)
  content <- readLines(output)
  expect_false(identical(content, "old content"))
})
