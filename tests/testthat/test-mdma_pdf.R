test_that("non-existent file throws an error", {
  expect_snapshot(mdma_pdf("nonexistent.pdf"), error = TRUE)
})

test_that("non-PDF file throws an error", {
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeLines("hello", tmp)
  # Volatile temp path in message: match on stable substring only
  expect_error(mdma_pdf(tmp), "does not appear to be a PDF file")
})

test_that("existing output without overwrite throws an error", {
  pdf <- file.path(R.home("doc"), "NEWS.pdf")
  skip_if_not(file.exists(pdf))
  tmp_out <- withr::local_tempfile(fileext = ".md")
  writeLines("existing content", tmp_out)
  # Volatile temp path in message: match on stable substring only
  expect_error(mdma_pdf(pdf, output = tmp_out), "Output file already exists")
})

test_that("converts PDF and writes markdown file", {
  pdf <- file.path(R.home("doc"), "NEWS.pdf")
  skip_if_not(file.exists(pdf))
  output <- withr::local_tempfile(fileext = ".md")
  result <- mdma_pdf(pdf, output = output)
  expect_equal(result, output)
  expect_true(file.exists(output))
  expect_gt(length(readLines(output)), 0L)
})

test_that("OCR fallback is triggered when text is below min_chars", {
  pdf <- file.path(R.home("doc"), "NEWS.pdf")
  skip_if_not(file.exists(pdf))
  output <- withr::local_tempfile(fileext = ".md")
  expect_message(
    mdma_pdf(pdf, output = output, min_chars = .Machine$integer.max),
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

test_that("pdf_classify_page returns 'two-column' for clear two-column layout", {
  page <- make_page_data(c(10, 300))
  expect_equal(pdf_classify_page(page), "two-column")
})

test_that("pdf_classify_page returns 'single-column' for wide single-column", {
  page <- make_page_data(seq(10, 200, by = 10))
  expect_equal(pdf_classify_page(page), "single-column")
})

test_that("pdf_classify_page returns 'single-column' for pages with too few words", {
  page <- make_page_data(c(10, 300), y_range = seq(10, 30, by = 10))
  expect_equal(pdf_classify_page(page), "single-column")
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

# ── pdf_count_encoding_artifacts ─────────────────────────────────────────────

test_that("pdf_count_encoding_artifacts counts CID patterns", {
  expect_equal(pdf_count_encoding_artifacts("(cid:0)\n(cid:1)\n(cid:2)"), 3L)
  expect_equal(pdf_count_encoding_artifacts("(cid:123) normal (cid:0)"), 2L)
  expect_equal(pdf_count_encoding_artifacts("normal text without CID"), 0L)
})

test_that("pdf_count_encoding_artifacts counts Unicode replacement characters", {
  expect_equal(pdf_count_encoding_artifacts("���"), 3L)
  expect_equal(pdf_count_encoding_artifacts("ok � text"), 1L)
})

test_that("pdf_count_encoding_artifacts counts both CID and replacement characters", {
  expect_equal(pdf_count_encoding_artifacts("(cid:1) � (cid:2)"), 3L)
})

# ── pdf_extract_page_unified ──────────────────────────────────────────────────

test_that("unified extraction places left column text before right", {
  page <- data.frame(
    x = c(rep(c(10, 50), 10L), rep(c(310, 350), 10L)),
    y = rep(rep(seq(100, by = 20, length.out = 10L), each = 2L), 2L),
    width = 30, height = 10, space = TRUE,
    text = c(paste0("L", 1:20), paste0("R", 1:20))
  )
  anchor <- data.frame(x = 490, y = 790, width = 20, height = 10,
                       space = TRUE, text = ".")
  page <- rbind(page, anchor)
  result <- pdf_extract_page_unified(page, doc_median_height = 10)
  expect_lt(regexpr("L1", result)[[1L]], regexpr("R1", result)[[1L]])
})

test_that("unified extraction does not garble two-column body text", {
  page <- data.frame(
    x = c(rep(c(10, 50, 90), 10L), rep(c(310, 350, 390), 10L)),
    y = rep(rep(seq(100, by = 20, length.out = 10L), each = 3L), 2L),
    width = 30, height = 10, space = TRUE,
    text = c(paste0("L", 1:30), paste0("R", 1:30))
  )
  anchor <- data.frame(x = 490, y = 790, width = 20, height = 10,
                       space = TRUE, text = ".")
  page <- rbind(page, anchor)
  result <- pdf_extract_page_unified(page, doc_median_height = 10)
  # Left words should NOT be interleaved with right words
  expect_false(grepl("L1[^\n]*R1", result))
})

# ── vector input ─────────────────────────────────────────────────────────────

test_that("mdma_pdf vectorises over a path vector", {
  pdf <- file.path(R.home("doc"), "NEWS.pdf")
  skip_if_not(file.exists(pdf))
  out1 <- withr::local_tempfile(fileext = ".md")
  out2 <- withr::local_tempfile(fileext = ".md")
  results <- mdma_pdf(c(pdf, pdf), output = c(out1, out2))
  expect_equal(results, c(out1, out2))
  expect_true(all(file.exists(results)))
})

test_that("mdma_pdf vectorises with default output paths", {
  pdf <- file.path(R.home("doc"), "NEWS.pdf")
  skip_if_not(file.exists(pdf))
  tmp1 <- withr::local_tempfile(fileext = ".pdf")
  tmp2 <- withr::local_tempfile(fileext = ".pdf")
  file.copy(pdf, tmp1)
  file.copy(pdf, tmp2)
  results <- mdma_pdf(c(tmp1, tmp2))
  expect_equal(results, sub("\\.pdf$", ".md", c(tmp1, tmp2), ignore.case = TRUE))
  expect_true(all(file.exists(results)))
})

# ── integration ───────────────────────────────────────────────────────────────

test_that("overwrite = TRUE replaces existing output", {
  pdf <- file.path(R.home("doc"), "NEWS.pdf")
  skip_if_not(file.exists(pdf))
  output <- withr::local_tempfile(fileext = ".md")
  writeLines("old content", output)
  mdma_pdf(pdf, output = output, overwrite = TRUE)
  content <- readLines(output)
  expect_false(identical(content, "old content"))
})

# ── pdf_strip_headers_footers ────────────────────────────────────────────────

make_word_df <- function(x, y, w = 20, h = 10, text = NULL) {
  n <- length(x)
  if (n == 0L) {
    return(data.frame(
      x = integer(), y = integer(), width = integer(),
      height = integer(), space = logical(), text = character()
    ))
  }
  data.frame(
    x = x, y = y, width = w, height = h, space = TRUE,
    text = if (is.null(text)) paste0("t", seq_len(n)) else text
  )
}

make_page_with_header <- function(header_text, body_texts,
                                  page_height = 800, header_y = 20,
                                  body_y_start = 100) {
  header <- make_word_df(10, header_y, text = header_text)
  body <- make_word_df(
    x = rep(10, length(body_texts)),
    y = seq(body_y_start, by = 20, length.out = length(body_texts)),
    text = body_texts
  )
  # Add a word near the bottom to set page_height
  anchor <- make_word_df(10, page_height - 10, text = ".")
  rbind(header, body, anchor)
}

test_that("pdf_strip_headers_footers removes repeated top-zone text", {
  pages <- lapply(1:5, function(i) {
    make_page_with_header("HEADER", paste0("body", i, "_", 1:5))
  })
  result <- pdf_strip_headers_footers(pages)
  for (k in seq_along(result)) {
    expect_false("HEADER" %in% result[[k]]$text)
    expect_true(any(grepl("^body", result[[k]]$text)))
  }
})

test_that("pdf_strip_headers_footers preserves unique page content", {
  pages <- lapply(1:5, function(i) {
    make_page_with_header(paste0("unique", i), paste0("body", i, "_", 1:5))
  })
  result <- pdf_strip_headers_footers(pages)
  for (k in seq_along(result)) {
    expect_true(paste0("unique", k) %in% result[[k]]$text)
  }
})

test_that("pdf_strip_headers_footers removes repeated bottom-zone text", {
  pages <- lapply(1:5, function(i) {
    body <- make_word_df(
      x = rep(10, 5),
      y = seq(100, 180, by = 20),
      text = paste0("body", i, "_", 1:5)
    )
    footer <- make_word_df(10, 780, text = "Journal 2022")
    anchor <- make_word_df(10, 790, text = ".")
    rbind(body, footer, anchor)
  })
  result <- pdf_strip_headers_footers(pages)
  for (k in seq_along(result)) {
    expect_false("Journal" %in% result[[k]]$text)
    expect_false("2022" %in% result[[k]]$text)
  }
})

test_that("pdf_strip_headers_footers removes footers with varying page numbers", {
  pages <- lapply(1:5, function(i) {
    body <- make_word_df(
      x = rep(10, 5),
      y = seq(100, 180, by = 20),
      text = paste0("body", i, "_", 1:5)
    )
    footer <- make_word_df(c(10, 50), c(780, 780), text = c("Page", as.character(i)))
    anchor <- make_word_df(10, 790, text = ".")
    rbind(body, footer, anchor)
  })
  result <- pdf_strip_headers_footers(pages)
  for (k in seq_along(result)) {
    expect_false("Page" %in% result[[k]]$text)
  }
})

test_that("pdf_strip_headers_footers returns input unchanged for < 3 pages", {
  pages <- list(
    make_page_with_header("HEADER", paste0("body", 1:5)),
    make_page_with_header("HEADER", paste0("body", 1:5))
  )
  result <- pdf_strip_headers_footers(pages)
  expect_equal(result, pages)
})

# ── pdf_classify_page ────────────────────────────────────────────────────────

make_two_col_page <- function(n_rows = 20L, page_width = 500,
                              page_height = 800) {
  left <- make_word_df(
    x = rep(c(10, 50, 90), n_rows),
    y = rep(seq(100, by = 20, length.out = n_rows), each = 3L),
    text = paste0("L", seq_len(n_rows * 3L))
  )
  right <- make_word_df(
    x = rep(c(310, 350, 390), n_rows),
    y = rep(seq(100, by = 20, length.out = n_rows), each = 3L),
    text = paste0("R", seq_len(n_rows * 3L))
  )
  anchor <- make_word_df(page_width - 10, page_height - 10, text = ".")
  rbind(left, right, anchor)
}

test_that("pdf_classify_page returns 'two-column' for two-column layout", {
  page <- make_two_col_page()
  expect_equal(pdf_classify_page(page), "two-column")
})

test_that("pdf_classify_page returns 'single-column' for full-width layout", {
  # page_width will be 510 (490 + 20). Middle zone = 0.45*510 to 0.55*510 =

  # 229.5 to 280.5. Include words at x=230 (center 240) in the gap zone.
  x_pos <- c(10, 100, 220, 260, 350, 470)
  n_per_row <- length(x_pos)
  n_rows <- 20L
  page <- make_word_df(
    x = rep(x_pos, n_rows),
    y = rep(seq(100, by = 20, length.out = n_rows), each = n_per_row),
    text = paste0("w", seq_len(n_per_row * n_rows))
  )
  anchor <- make_word_df(490, 790, text = ".")
  page <- rbind(page, anchor)
  expect_equal(pdf_classify_page(page), "single-column")
})

test_that("pdf_classify_page returns 'single-column' for pages with too few words", {
  page <- make_word_df(c(10, 300), c(100, 100), text = c("A", "B"))
  anchor <- make_word_df(490, 790, text = ".")
  page <- rbind(page, anchor)
  expect_equal(pdf_classify_page(page), "single-column")
})

test_that("pdf_classify_page returns 'mixed' for page with full-width table in two-col body", {
  two_col <- make_two_col_page(n_rows = 10L)
  x_pos <- c(10, 100, 220, 260, 350, 470)
  n_per_row <- length(x_pos)
  n_fw_rows <- 8L
  full_width <- make_word_df(
    x = rep(x_pos, n_fw_rows),
    y = rep(seq(320, by = 20, length.out = n_fw_rows), each = n_per_row),
    text = paste0("F", seq_len(n_per_row * n_fw_rows))
  )
  page <- rbind(two_col[two_col$text != ".", ], full_width)
  anchor <- make_word_df(490, 790, text = ".")
  page <- rbind(page, anchor)
  expect_equal(pdf_classify_page(page), "mixed")
})

# ── pdf_split_page_zones ────────────────────────────────────────────────────

test_that("pdf_split_page_zones separates full-width table from two-col body", {
  two_col_words <- make_word_df(
    x = rep(c(10, 310), 10L),
    y = rep(seq(100, by = 20, length.out = 10L), each = 2L),
    text = paste0("TC", seq_len(20L))
  )
  x_pos <- c(10, 100, 220, 260, 350, 470)
  n_per_row <- length(x_pos)
  n_fw_rows <- 5L
  full_width_words <- make_word_df(
    x = rep(x_pos, n_fw_rows),
    y = rep(seq(320, by = 20, length.out = n_fw_rows), each = n_per_row),
    text = paste0("FW", seq_len(n_per_row * n_fw_rows))
  )
  page <- rbind(two_col_words, full_width_words)
  anchor <- make_word_df(490, 790, text = ".")
  page <- rbind(page, anchor)

  zones <- pdf_split_page_zones(page)
  expect_gte(length(zones), 2L)
  types <- vapply(zones, `[[`, character(1L), "type")
  expect_true("two-column" %in% types)
  expect_true("single-column" %in% types)
})

test_that("pdf_split_page_zones returns single zone for uniform layout", {
  page <- make_two_col_page()
  zones <- pdf_split_page_zones(page)
  expect_equal(length(zones), 1L)
  expect_equal(zones[[1L]]$type, "two-column")
})

# ── pdf_group_into_rows ───────────────────────────────────────────────────────

test_that("pdf_group_into_rows groups words on the same y into one row", {
  page <- make_word_df(c(10, 50, 10, 50), c(10, 10, 30, 30))
  rows <- pdf_group_into_rows(page)
  expect_length(rows, 2L)
  expect_equal(nrow(rows[[1L]]), 2L)
  expect_equal(nrow(rows[[2L]]), 2L)
})

test_that("pdf_group_into_rows orders words left to right within row", {
  page <- make_word_df(c(50, 10), c(10, 10), text = c("second", "first"))
  rows <- pdf_group_into_rows(page)
  expect_equal(rows[[1L]]$text, c("first", "second"))
})

test_that("pdf_group_into_rows handles single word", {
  page <- make_word_df(10, 10, text = "only")
  rows <- pdf_group_into_rows(page)
  expect_length(rows, 1L)
  expect_equal(rows[[1L]]$text, "only")
})

test_that("pdf_group_into_rows returns empty list for empty page", {
  page <- make_word_df(integer(), integer())
  expect_equal(pdf_group_into_rows(page), list())
})

# ── pdf_row_column_bounds ─────────────────────────────────────────────────────

test_that("pdf_row_column_bounds detects gap-separated columns", {
  # Two words with a 60-unit gap (threshold 20*2.5 = 50)
  row <- make_word_df(c(10, 90), c(10, 10))  # right edge of first: 30, gap: 60
  bounds <- pdf_row_column_bounds(row, gap_threshold = 50)
  expect_equal(bounds, c(10, 90))
})

test_that("pdf_row_column_bounds returns single x for one word", {
  row <- make_word_df(15, 10)
  expect_equal(pdf_row_column_bounds(row, gap_threshold = 50), 15)
})

test_that("pdf_row_column_bounds returns empty for empty row", {
  row <- make_word_df(integer(), integer())
  expect_equal(pdf_row_column_bounds(row, gap_threshold = 50), numeric(0L))
})

test_that("pdf_row_column_bounds does not split tightly spaced words", {
  row <- make_word_df(c(10, 35), c(10, 10))  # gap = 35 - 30 = 5, below 50
  bounds <- pdf_row_column_bounds(row, gap_threshold = 50)
  expect_equal(bounds, 10)
})

# ── pdf_columns_compatible ────────────────────────────────────────────────────

test_that("pdf_columns_compatible returns TRUE for matching columns", {
  expect_true(pdf_columns_compatible(c(10, 90), c(10, 90), tolerance = 5))
})

test_that("pdf_columns_compatible returns TRUE for one fewer column (empty cell)", {
  expect_true(pdf_columns_compatible(c(10, 90, 170), c(10, 90), tolerance = 5))
})

test_that("pdf_columns_compatible returns FALSE for two fewer columns", {
  expect_false(pdf_columns_compatible(c(10, 90, 170), c(10), tolerance = 5))
})

test_that("pdf_columns_compatible returns FALSE for more columns than reference", {
  expect_false(pdf_columns_compatible(c(10, 90), c(10, 90, 170), tolerance = 5))
})

test_that("pdf_columns_compatible allows tolerance in x positions", {
  expect_true(pdf_columns_compatible(c(10, 90), c(12, 88), tolerance = 5))
})

test_that("pdf_columns_compatible returns FALSE when offset exceeds tolerance", {
  expect_false(pdf_columns_compatible(c(10, 90), c(20, 100), tolerance = 5))
})

# ── pdf_detect_table_regions ──────────────────────────────────────────────────

make_table_rows <- function(n_data_rows = 3L) {
  # Three columns at x = 10, 100, 200; each row has height 10
  lapply(seq_len(n_data_rows), function(i) {
    make_word_df(c(10, 100, 200), rep(i * 15L, 3L))
  })
}

test_that("pdf_detect_table_regions marks aligned rows as table", {
  rows <- make_table_rows(3L)
  is_tbl <- pdf_detect_table_regions(rows)
  expect_true(all(is_tbl))
})

test_that("pdf_detect_table_regions does not mark single-column rows", {
  rows <- lapply(1:3, function(i) make_word_df(10, i * 15L))
  is_tbl <- pdf_detect_table_regions(rows)
  expect_false(any(is_tbl))
})

test_that("pdf_detect_table_regions does not flag too few rows", {
  rows <- make_table_rows(1L)
  is_tbl <- pdf_detect_table_regions(rows, min_table_rows = 2L)
  expect_false(any(is_tbl))
})

# ── pdf_assign_words_to_cols ──────────────────────────────────────────────────

test_that("pdf_assign_words_to_cols places words in correct cells", {
  row <- make_word_df(c(10, 100, 200), c(10, 10, 10), text = c("A", "B", "C"))
  cells <- pdf_assign_words_to_cols(row, col_starts = c(10, 100, 200))
  expect_equal(cells, c("A", "B", "C"))
})

test_that("pdf_assign_words_to_cols concatenates multiple words in one cell", {
  row <- make_word_df(c(10, 30, 100), c(10, 10, 10), text = c("Hello", "World", "B"))
  cells <- pdf_assign_words_to_cols(row, col_starts = c(10, 100))
  expect_equal(cells[[1L]], "Hello World")
  expect_equal(cells[[2L]], "B")
})

test_that("pdf_assign_words_to_cols leaves empty string for missing cell", {
  row <- make_word_df(c(10, 200), c(10, 10), text = c("A", "C"))
  cells <- pdf_assign_words_to_cols(row, col_starts = c(10, 100, 200))
  expect_equal(cells[[2L]], "")
})

# ── pdf_rows_to_markdown_table ────────────────────────────────────────────────

test_that("pdf_rows_to_markdown_table emits header, separator, and data rows", {
  rows <- make_table_rows(3L)
  result <- pdf_rows_to_markdown_table(rows, gap_threshold = 50)
  lines <- strsplit(result, "\n")[[1L]]
  expect_match(lines[[1L]], "^\\|")
  expect_match(lines[[2L]], "^\\|[[:space:]]*---")
  expect_length(lines, 4L)  # header + sep + 2 data rows
})

test_that("pdf_rows_to_markdown_table handles single row gracefully", {
  rows <- list(make_word_df(c(10, 100), c(10, 10), text = c("A", "B")))
  result <- pdf_rows_to_markdown_table(rows, gap_threshold = 50)
  expect_false(grepl("---", result, fixed = TRUE))
})

# ── pdf_row_to_text ───────────────────────────────────────────────────────────

test_that("pdf_row_to_text returns plain text for normal-height row", {
  row <- make_word_df(c(10, 40), c(10, 10), h = 10, text = c("Hello", "World"))
  expect_equal(pdf_row_to_text(row, doc_median_height = 10), "Hello World")
})

test_that("pdf_row_to_text prefixes ## for large heading row", {
  row <- make_word_df(10, 10, h = 18, text = "Title")
  result <- pdf_row_to_text(row, doc_median_height = 10)
  expect_match(result, "^## Title")
})

test_that("pdf_row_to_text prefixes ### for medium heading row", {
  row <- make_word_df(10, 10, h = 13, text = "Section")
  result <- pdf_row_to_text(row, doc_median_height = 10)
  expect_match(result, "^### Section")
})

test_that("pdf_row_to_text returns empty string for empty row", {
  row <- make_word_df(integer(), integer())
  expect_equal(pdf_row_to_text(row, doc_median_height = 10), "")
})

# ── pdf_extract_zone ─────────────────────────────────────────────────────────

test_that("pdf_extract_zone produces table markdown for grid data", {
  words <- data.frame(
    x = c(10, 100, 200, 10, 100, 200, 10, 100, 200),
    y = c(10, 10, 10, 30, 30, 30, 50, 50, 50),
    width = 60, height = 10, space = TRUE,
    text = c("H1", "H2", "H3", "a", "b", "c", "d", "e", "f")
  )
  result <- pdf_extract_zone(words, doc_median_height = 10)
  expect_match(result, "\\|")
  expect_match(result, "---")
})

test_that("pdf_extract_zone returns empty string for empty input", {
  words <- data.frame(
    x = integer(), y = integer(), width = integer(),
    height = integer(), space = logical(), text = character()
  )
  expect_equal(pdf_extract_zone(words, doc_median_height = 10), "")
})

test_that("pdf_extract_zone emits prose for non-table text", {
  words <- make_word_df(
    x = rep(c(10, 50, 90), 5L),
    y = rep(seq(10, by = 20, length.out = 5L), each = 3L),
    text = paste0("w", 1:15)
  )
  result <- pdf_extract_zone(words, doc_median_height = 10)
  expect_false(grepl("\\|", result))
  expect_true(grepl("w1", result))
})

# ── pdf_row_has_formulas ─────────────────────────────────────────────────────

test_that("pdf_row_has_formulas detects row with small math words", {
  row <- data.frame(
    x = c(10, 30, 50), y = c(10, 14, 10),
    width = 20, height = c(12, 6, 12), space = TRUE,
    text = c("R", "i,t", "=")
  )
  expect_true(pdf_row_has_formulas(row, doc_median_height = 12))
})

test_that("pdf_row_has_formulas returns FALSE for plain text", {
  row <- make_word_df(c(10, 50, 90), c(10, 10, 10),
                      text = c("Hello", "World", "test"))
  expect_false(pdf_row_has_formulas(row, doc_median_height = 10))
})

# ── pdf_reconstruct_formula ──────────────────────────────────────────────────

test_that("pdf_reconstruct_formula attaches subscripts", {
  row <- data.frame(
    x = c(10, 30, 60, 80), y = c(10, 14, 10, 14),
    width = 20, height = c(12, 6, 12, 6), space = TRUE,
    text = c("R", "i,t", "=", "0")
  )
  result <- pdf_reconstruct_formula(row, doc_median_height = 12)
  expect_match(result, "R_\\{i,t\\}")
})

test_that("pdf_reconstruct_formula attaches superscripts", {
  row <- data.frame(
    x = c(10, 30), y = c(14, 8),
    width = 20, height = c(12, 6), space = TRUE,
    text = c("x", "2")
  )
  result <- pdf_reconstruct_formula(row, doc_median_height = 12)
  expect_match(result, "x\\^\\{2\\}")
})

test_that("pdf_reconstruct_formula leaves normal-height words unchanged", {
  row <- make_word_df(c(10, 50, 90), c(10, 10, 10),
                      text = c("a", "+", "b"))
  result <- pdf_reconstruct_formula(row, doc_median_height = 10)
  expect_equal(result, "a + b")
})
