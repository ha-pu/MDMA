# ── basic: page numbers ────────────────────────────────────────────────────────

test_that("remove_page_numbers removes standalone digit lines", {
  expect_equal(mdma_clean("text\n\n42\n\nmore"), "text\n\nmore")
  expect_equal(mdma_clean("text\n\n  123  \n\nmore"), "text\n\nmore")
})

test_that("remove_page_numbers does not touch digits mid-line", {
  md <- "Section 3 discusses results"
  expect_equal(mdma_clean(md), md)
})

# ── basic: TOC leaders ────────────────────────────────────────────────────────

test_that("remove_toc_leaders removes dot leader lines", {
  md <- "Introduction ......... 3\n\nReal content"
  result <- mdma_clean(md)
  expect_false(grepl("Introduction", result))
  expect_true(grepl("Real content", result))
})

test_that("remove_toc_leaders removes dash leader lines", {
  md <- "Methods ----------- 12\n\nReal content"
  result <- mdma_clean(md)
  expect_false(grepl("Methods", result))
})

test_that("remove_toc_leaders preserves horizontal rules", {
  md <- "text\n\n---\n\nmore"
  expect_true(grepl("---", mdma_clean(md)))
})

# ── basic: soft hyphens ───────────────────────────────────────────────────────

test_that("repair_soft_hyphens joins hyphenated line breaks", {
  expect_equal(mdma_clean("algo-\nrithm"), "algorithm")
  expect_equal(mdma_clean("compre-\nhensive study"), "comprehensive study")
})

test_that("repair_soft_hyphens preserves mid-line hyphens", {
  expect_equal(mdma_clean("state-of-the-art"), "state-of-the-art")
})

test_that("repair_soft_hyphens only joins when next char is lowercase", {
  # Uppercase after the hyphen: no join expected (test at basic so line-joining
  # is not also applied)
  md <- "word-\nUpper case follows"
  expect_true(grepl(
    "word-\nUpper",
    mdma_clean(md, level = "basic"),
    fixed = TRUE
  ))
})

# ── moderate: join wrapped lines ──────────────────────────────────────────────

test_that("join_wrapped_lines merges hard-wrapped paragraph lines", {
  md <- "This is a long\nline that wraps"
  expect_equal(
    mdma_clean(md, level = "moderate"),
    "This is a long line that wraps"
  )
})

test_that("join_wrapped_lines does not merge across blank lines", {
  md <- "Paragraph one.\n\nParagraph two."
  result <- mdma_clean(md, level = "moderate")
  expect_true(grepl("\n\n", result))
})

test_that("join_wrapped_lines does not merge into headings", {
  md <- "Some text\n## Heading"
  result <- mdma_clean(md, level = "moderate")
  expect_true(grepl("Some text\n## Heading", result))
})

test_that("join_wrapped_lines does not merge into list items", {
  md <- "Some text\n- item one"
  result <- mdma_clean(md, level = "moderate")
  expect_true(grepl("Some text\n- item one", result))
})

test_that("join_wrapped_lines does not merge lines inside code blocks", {
  md <- "```\nline one\nline two\n```"
  expect_equal(mdma_clean(md, level = "moderate"), md)
})

test_that("join_wrapped_lines stops merging after sentence-ending punctuation", {
  md <- "First sentence.\nSecond sentence."
  result <- mdma_clean(md, level = "moderate")
  expect_true(grepl("First sentence\\.\nSecond sentence\\.", result))
})

# ── moderate: deduplicate running lines ───────────────────────────────────────

test_that("deduplicate_running_lines removes repeated short lines", {
  # Blank lines between entries so join_wrapped_lines does not collapse them
  md <- paste(
    c("Header", "", "Content 1.", "", "Header", "", "Content 2.", "", "Header"),
    collapse = "\n"
  )
  result <- mdma_clean(md, level = "moderate")
  expect_equal(sum(grepl("^Header$", strsplit(result, "\n")[[1L]])), 1L)
})

test_that("deduplicate_running_lines preserves long repeated lines", {
  long <- paste(rep("x", 90), collapse = "")
  # Blank lines between entries prevent join_wrapped_lines from merging them
  md <- paste(rep(long, 4), collapse = "\n\n")
  result <- mdma_clean(md, level = "moderate")
  expect_equal(sum(grepl(long, strsplit(result, "\n")[[1L]], fixed = TRUE)), 4L)
})

# ── moderate: collapse duplicate headings ────────────────────────────────────

test_that("collapse_duplicate_headings removes consecutive duplicate headings", {
  md <- "# Title\n\n# Title\n\nContent"
  result <- mdma_clean(md, level = "moderate")
  expect_equal(sum(grepl("^# Title$", strsplit(result, "\n")[[1L]])), 1L)
})

test_that("collapse_duplicate_headings removes plain title before matching heading", {
  md <- "Title\n\n# Title\n\nContent"
  result <- mdma_clean(md, level = "moderate")
  expect_false(grepl("^Title$", result))
  expect_true("# Title" %in% strsplit(result, "\n")[[1L]])
})

test_that("collapse_duplicate_headings preserves different consecutive headings", {
  md <- "# Introduction\n\n## Background\n\nContent"
  result <- mdma_clean(md, level = "moderate")
  expect_true(grepl("# Introduction", result))
  expect_true(grepl("## Background", result))
})

# ── extreme: boilerplate ───────────────────────────────────────────────────

test_that("remove_blank_page_boilerplate removes standard phrasing", {
  md <- "Page one\n\nThis page intentionally left blank\n\nPage two"
  result <- mdma_clean(md, level = "extreme")
  expect_false(grepl("intentionally", result, ignore.case = TRUE))
  expect_true(grepl("Page two", result))
})

test_that("remove_blank_page_boilerplate handles bracketed variant", {
  md <- "[This page intentionally left blank.]"
  result <- mdma_clean(md, level = "extreme")
  expect_false(grepl("intentionally", result, ignore.case = TRUE))
})

# ── extreme: copyright lines ───────────────────────────────────────────────

test_that("remove_copyright_lines strips copyright symbol lines", {
  md <- "Content\n\n© 2024 Author Inc.\n\nMore content"
  result <- mdma_clean(md, level = "extreme")
  expect_false(grepl("©", result))
  expect_true(grepl("More content", result))
})

test_that("remove_copyright_lines strips DOI lines", {
  md <- "Content\n\ndoi: 10.1000/xyz123\n\nMore content"
  result <- mdma_clean(md, level = "extreme")
  expect_false(grepl("doi:", result, ignore.case = TRUE))
})

# ── level inheritance ─────────────────────────────────────────────────────────

test_that("moderate applies all basic steps too", {
  md <- "Introduction ......... 3\n\nSome text\nthat wraps"
  result <- mdma_clean(md, level = "moderate")
  expect_false(grepl("Introduction", result))
  expect_true(grepl("Some text that wraps", result))
})

test_that("extreme applies all moderate and basic steps too", {
  md <- "42\n\nHeader\nlines\n\nHeader\nlines\n\nHeader\nlines\n\n© 2024 Corp"
  result <- mdma_clean(md, level = "extreme")
  expect_false(grepl("^42$", result))
  expect_false(grepl("©", result))
})

# ── level validation ──────────────────────────────────────────────────────────

test_that("invalid level throws an error", {
  expect_snapshot(mdma_clean("text", level = "aggressive"), error = TRUE)
})

# ── vector input ─────────────────────────────────────────────────────────────

test_that("mdma_clean vectorises over a character vector", {
  input <- c("algo-\nrithm", "word\n\n42\n\nmore")
  result <- mdma_clean(input)
  expect_equal(result, c("algorithm", "word\n\nmore"))
})

# ── edge cases ────────────────────────────────────────────────────────────────

test_that("empty string returns empty string", {
  expect_equal(mdma_clean(""), "")
})

test_that("excess blank lines are collapsed", {
  md <- "a\n\n\n\nb"
  expect_equal(mdma_clean(md), "a\n\nb")
})

test_that("CRLF line endings are normalised", {
  md <- "line one\r\nline two"
  expect_equal(mdma_clean(md), "line one\nline two")
})
