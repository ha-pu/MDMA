#' Convert a PDF file to Markdown
#'
#' Converts a PDF file to Markdown and writes the result to a `.md` file. By
#' default the output is placed in the same directory as the input with the
#' extension replaced.
#'
#' When the \pkg{pdftools} package is installed, the PDF is first checked for
#' a two-column layout using word bounding-box coordinates. If a two-column
#' structure is detected, text is extracted column by column via
#' [pdftools::pdf_data()] so that column order is preserved. Otherwise,
#' [ragnar::read_as_markdown()] is used.
#'
#' If the extracted text contains fewer than `min_chars` non-whitespace
#' characters, or if it contains more than 10 CID font artefacts
#' (`(cid:N)` patterns produced by unresolvable font encodings), the PDF is
#' assumed to require OCR and is processed via the
#' [tesseract](https://cran.r-project.org/package=tesseract) and
#' [pdftools](https://cran.r-project.org/package=pdftools) packages.
#'
#' @param path `[character]` Path to a PDF file, or a character vector of paths
#'   to convert multiple files. When length is greater than 1, files are
#'   converted in a for loop with a progress bar.
#' @param output `[character]` Path for the output Markdown file, or a
#'   character vector of the same length as `path`. Defaults to each `path`
#'   with the `.pdf` extension replaced by `.md`.
#' @param overwrite `[logical(1)]` If `FALSE` (default), an error is thrown
#'   when `output` already exists.
#' @param min_chars `[integer(1)]` Minimum number of non-whitespace characters
#'   that must be present in the extracted text for OCR to be skipped.
#'   Defaults to `100L`.
#' @param language `[string]` Tesseract language code used when OCR is
#'   triggered (e.g. `"eng"`, `"nld"`). Defaults to `"eng"`.
#' @param dpi `[integer(1)]` Resolution used when rendering PDF pages to images
#'   for OCR. Higher values improve accuracy at the cost of speed. Defaults to
#'   `300L`.
#' @param clean `[string]` Aggressiveness level passed to [clean_markdown()].
#'   One of `"basic"` (default), `"moderate"`, `"aggressive"`, or `"none"` to
#'   skip cleaning entirely.
#' @param ... Passed on to [ragnar::read_as_markdown()] when single-column
#'   layout is detected or `pdftools` is unavailable.
#'
#' @return The output file path invisibly, or a character vector of output
#'   paths when `length(path) > 1`.
#' @export
#'
#' @examples
#' \dontrun{
#' pdf_to_md("report.pdf")
#' pdf_to_md("report.pdf", output = "llm_ready/report.md")
#' pdf_to_md("report.pdf", output = "report.md", overwrite = TRUE)
#' pdf_to_md("scan.pdf", language = "nld")
#' pdf_to_md("report.pdf", clean = "moderate")
#' pdf_to_md("report.pdf", clean = "none")
#' }
pdf_to_md <- function(
  path,
  output = NULL,
  overwrite = FALSE,
  min_chars = 100L,
  language = "eng",
  dpi = 300L,
  clean = "basic",
  ...
) {
  if (length(path) > 1) {
    out_list <- if (is.null(output)) vector("list", length(path)) else as.list(output)
    results <- character(length(path))
    for (i in cli::cli_progress_along(path, name = "Converting")) {
      results[[i]] <- pdf_to_md(
        path[[i]], output = out_list[[i]],
        overwrite = overwrite, min_chars = min_chars,
        language = language, dpi = dpi, clean = clean, ...
      )
    }
    return(invisible(results))
  }
  clean <- match.arg(clean, c("basic", "moderate", "aggressive", "none"))
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }
  if (!grepl("\\.pdf$", path, ignore.case = TRUE)) {
    cli::cli_abort("{.path {path}} does not appear to be a PDF file.")
  }

  if (is.null(output)) {
    output <- sub("\\.pdf$", ".md", path, ignore.case = TRUE)
  }

  if (!overwrite && file.exists(output)) {
    cli::cli_abort(c(
      "Output file already exists: {.path {output}}",
      "i" = "Set {.arg overwrite = TRUE} to overwrite."
    ))
  }

  md <- pdf_extract(path, ...)
  text <- as.character(md)
  text_chars <- nchar(gsub("[[:space:]]", "", text))
  cid_count <- pdf_count_cid_artifacts(text)

  if (text_chars < min_chars) {
    cli::cli_inform(c(
      "i" = "Text extraction returned {text_chars} non-whitespace \\
             character{?s}; falling back to OCR."
    ))
    text <- pdf_ocr(path, language = language, dpi = dpi)
  } else if (cid_count > 10L) {
    cli::cli_inform(c(
      "i" = "Text contains CID font artefacts ({cid_count} found); \\
             falling back to OCR."
    ))
    text <- pdf_ocr(path, language = language, dpi = dpi)
  }
  if (clean != "none") text <- clean_markdown(text, level = clean)

  writeLines(text, output)
  invisible(output)
}

pdf_count_cid_artifacts <- function(text) {
  m <- gregexpr("\\(cid:\\d+\\)", text)[[1L]]
  if (m[[1L]] == -1L) 0L else length(m)
}

pdf_extract <- function(path, ...) {
  page_data <- tryCatch(pdftools::pdf_data(path), error = function(e) NULL)
  if (is.null(page_data) || !pdf_detect_two_column(page_data)) {
    return(ragnar::read_as_markdown(path, ...))
  }
  cli::cli_inform(c("i" = "Two-column layout detected; using coordinate-aware extraction."))
  pdf_extract_two_column(page_data)
}

pdf_detect_two_column <- function(page_data, sample_pages = 5L) {
  pages <- head(page_data, sample_pages)
  is_two_col <- vapply(pages, function(page) {
    if (nrow(page) < 20L) return(NA)
    page_width <- max(page$x + page$width, na.rm = TRUE)
    if (!is.finite(page_width) || page_width == 0) return(NA)
    x_mid <- (page$x + page$width / 2) / page_width
    n_left   <- sum(x_mid < 0.4)
    n_middle <- sum(x_mid >= 0.4 & x_mid < 0.6)
    n_right  <- sum(x_mid >= 0.6)
    n_total  <- n_left + n_middle + n_right
    n_middle / n_total < 0.15 && n_left >= 5L && n_right >= 5L
  }, logical(1L))
  is_two_col <- is_two_col[!is.na(is_two_col)]
  length(is_two_col) > 0L && mean(is_two_col) > 0.5
}

pdf_extract_two_column <- function(page_data) {
  page_texts <- vapply(page_data, function(page) {
    if (nrow(page) == 0L) return("")
    page_width <- max(page$x + page$width, na.rm = TRUE)
    x_mid <- (page$x + page$width / 2) / page_width
    left_text  <- words_to_text(page[x_mid <  0.5, , drop = FALSE])
    right_text <- words_to_text(page[x_mid >= 0.5, , drop = FALSE])
    paste(c(left_text, right_text), collapse = "\n\n")
  }, character(1L))
  paste(page_texts, collapse = "\n\n")
}

words_to_text <- function(words) {
  if (nrow(words) == 0L) return("")
  if (nrow(words) == 1L) return(words$text)
  words <- words[order(words$y, words$x), ]
  threshold <- max(median(words$height, na.rm = TRUE) * 0.5, 1)
  line_id <- integer(nrow(words))
  line_id[[1L]] <- 1L
  for (i in seq_len(nrow(words))[-1L]) {
    line_id[[i]] <- line_id[[i - 1L]] + (words$y[[i]] - words$y[[i - 1L]] > threshold)
  }
  line_texts <- tapply(seq_len(nrow(words)), line_id, function(idx) {
    w <- words[idx[order(words$x[idx])], , drop = FALSE]
    paste(w$text, collapse = " ")
  })
  paste(unname(line_texts), collapse = "\n")
}

pdf_ocr <- function(path, language = "eng", dpi = 300L) {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE))

  n_pages <- pdftools::pdf_info(path)$pages
  filenames <- file.path(tmpdir, sprintf("page_%04d.png", seq_len(n_pages)))
  pdftools::pdf_convert(path, format = "png", dpi = dpi, filenames = filenames, verbose = FALSE)

  engine <- tesseract::tesseract(language)
  texts <- vapply(filenames, \(p) tesseract::ocr(p, engine = engine), character(1L))
  paste(texts, collapse = "\n\n")
}
