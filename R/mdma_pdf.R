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
#' characters, or if it contains more than 10 encoding artefacts — CID font
#' patterns `(cid:N)` or Unicode replacement characters (U+FFFD) — the PDF
#' is assumed to require OCR and is processed via the
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
#' @param clean `[string]` Intrusion level passed to [mdma_clean()].
#'   One of `"basic"` (default), `"moderate"`, `"extreme"`, or `"none"` to
#'   skip cleaning entirely.
#' @param tables `[logical(1)]` If `TRUE` (default), uses coordinate-aware
#'   extraction via [pdftools::pdf_data()] to detect table regions and emit
#'   proper Markdown tables (`| col | col |`). Headings are inferred from
#'   word height relative to the document median. Has no effect when the PDF
#'   falls back to OCR or when `pdftools` is unavailable.
#' @param formulas `[logical(1)]` If `TRUE`, attempts to reconstruct
#'   mathematical formulas by detecting subscript and superscript positions
#'   from word coordinates. Subscripts are emitted as `_{text}` and
#'   superscripts as `^{text}`. Defaults to `FALSE`.
#' @param ... Passed on to [ragnar::read_as_markdown()] when `tables = FALSE`
#'   and single-column layout is detected, or when `pdftools` is unavailable.
#'
#' @return The output file path invisibly, or a character vector of output
#'   paths when `length(path) > 1`.
#' @export
#'
#' @examples
#' \dontrun{
#' mdma_pdf("report.pdf")
#' mdma_pdf("report.pdf", output = "llm_ready/report.md")
#' mdma_pdf("report.pdf", output = "report.md", overwrite = TRUE)
#' mdma_pdf("scan.pdf", language = "nld")
#' mdma_pdf("report.pdf", clean = "moderate")
#' mdma_pdf("report.pdf", clean = "none")
#' mdma_pdf("report.pdf", tables = FALSE)
#' }
mdma_pdf <- function(
  path,
  output = NULL,
  overwrite = FALSE,
  min_chars = 1000L,
  language = "eng",
  dpi = 300L,
  clean = "basic",
  tables = TRUE,
  formulas = FALSE,
  ...
) {
  if (length(path) > 1) {
    out_list <- if (is.null(output)) {
      vector("list", length(path))
    } else {
      as.list(output)
    }
    results <- character(length(path))
    for (i in cli::cli_progress_along(path, name = "Converting")) {
      results[[i]] <- mdma_pdf(
        path[[i]],
        output = out_list[[i]],
        overwrite = overwrite,
        min_chars = min_chars,
        language = language,
        dpi = dpi,
        clean = clean,
        tables = tables,
        formulas = formulas,
        ...
      )
    }
    return(invisible(results))
  }
  clean <- match.arg(clean, c("basic", "moderate", "extreme", "none"))
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

  md <- pdf_extract(path, tables = tables, formulas = formulas, ...)
  text <- as.character(md)
  text_chars <- nchar(gsub("[[:space:]]", "", text))
  artifact_count <- pdf_count_encoding_artifacts(text)

  if (text_chars < min_chars) {
    cli::cli_inform(c(
      "i" = "Text extraction returned {text_chars} non-whitespace \\
             character{?s}; falling back to OCR."
    ))
    text <- pdf_ocr(path, language = language, dpi = dpi)
  } else if (artifact_count > 10L) {
    cli::cli_inform(c(
      "i" = "Text contains encoding artefacts ({artifact_count} found); \\
             falling back to OCR."
    ))
    text <- pdf_ocr(path, language = language, dpi = dpi)
  }
  if (clean != "none") {
    text <- mdma_clean(text, level = clean)
  }

  writeLines(text, output)
  invisible(output)
}

pdf_count_encoding_artifacts <- function(text) {
  cid <- gregexpr("\\(cid:\\d+\\)", text)[[1L]]
  n_cid <- if (cid[[1L]] == -1L) 0L else length(cid)
  repl <- gregexpr("�", text, fixed = TRUE)[[1L]]
  n_repl <- if (repl[[1L]] == -1L) 0L else length(repl)
  n_cid + n_repl
}

pdf_extract <- function(path, tables = TRUE, formulas = FALSE, ...) {
  page_data <- tryCatch(pdftools::pdf_data(path), error = function(e) NULL)
  if (is.null(page_data)) {
    return(ragnar::read_as_markdown(path, ...))
  }
  page_data <- pdf_strip_headers_footers(page_data)

  all_heights <- unlist(lapply(page_data, `[[`, "height"))
  doc_median_height <- median(all_heights[all_heights > 0], na.rm = TRUE)

  page_texts <- vapply(
    page_data,
    pdf_extract_page_unified,
    character(1L),
    doc_median_height = doc_median_height,
    tables = tables,
    formulas = formulas
  )
  paste(page_texts[nzchar(trimws(page_texts))], collapse = "\n\n")
}

pdf_extract_page_unified <- function(page, doc_median_height, tables = TRUE,
                                    formulas = FALSE) {
  if (nrow(page) == 0L) return("")

  layout <- pdf_classify_page(page)

  if (layout == "single-column") {
    return(pdf_extract_zone(page, doc_median_height, tables, formulas))
  }

  page_width <- max(page$x + page$width, na.rm = TRUE)

  if (layout == "two-column") {
    x_mid <- (page$x + page$width / 2) / page_width
    left_words  <- page[x_mid < 0.5, , drop = FALSE]
    right_words <- page[x_mid >= 0.5, , drop = FALSE]
    left_text  <- pdf_extract_zone(left_words, doc_median_height, tables,
                                   formulas)
    right_text <- pdf_extract_zone(right_words, doc_median_height, tables,
                                   formulas)
    return(paste(
      c(left_text, right_text)[nzchar(trimws(c(left_text, right_text)))],
      collapse = "\n\n"
    ))
  }

  # "mixed" layout: split into zones
  zones <- pdf_split_page_zones(page)
  zone_texts <- vapply(zones, function(z) {
    if (z$type == "two-column") {
      x_mid <- (z$words$x + z$words$width / 2) / page_width
      left_words  <- z$words[x_mid < 0.5, , drop = FALSE]
      right_words <- z$words[x_mid >= 0.5, , drop = FALSE]
      left_text  <- pdf_extract_zone(left_words, doc_median_height, tables,
                                     formulas)
      right_text <- pdf_extract_zone(right_words, doc_median_height, tables,
                                     formulas)
      paste(
        c(left_text, right_text)[nzchar(trimws(c(left_text, right_text)))],
        collapse = "\n\n"
      )
    } else {
      pdf_extract_zone(z$words, doc_median_height, tables, formulas)
    }
  }, character(1L))
  paste(zone_texts[nzchar(trimws(zone_texts))], collapse = "\n\n")
}

pdf_extract_zone <- function(words, doc_median_height, tables = TRUE,
                             formulas = FALSE) {
  if (nrow(words) == 0L) return("")
  rows <- pdf_group_into_rows(words)
  if (length(rows) == 0L) return("")

  if (tables) {
    is_tbl <- pdf_detect_table_regions(rows)
  } else {
    is_tbl <- logical(length(rows))
  }

  parts <- character(0L)
  i     <- 1L

  while (i <= length(rows)) {
    if (is_tbl[[i]]) {
      j <- i
      while (j <= length(rows) && is_tbl[[j]]) j <- j + 1L
      gap_thr <- pdf_gap_threshold(rows[i:(j - 1L)])
      parts   <- c(parts, pdf_rows_to_markdown_table(rows[i:(j - 1L)], gap_thr))
      i <- j
    } else {
      if (formulas && pdf_row_has_formulas(rows[[i]], doc_median_height)) {
        line <- pdf_reconstruct_formula(rows[[i]], doc_median_height)
      } else {
        line <- pdf_row_to_text(rows[[i]], doc_median_height)
      }
      if (nzchar(trimws(line))) parts <- c(parts, line)
      i <- i + 1L
    }
  }

  paste(parts, collapse = "\n")
}

# ── Header / footer removal ─────────────────────────────────────────────────

pdf_strip_headers_footers <- function(page_data, margin = 0.08,
                                      min_pages_frac = 0.5) {
  n_pages <- length(page_data)
  if (n_pages < 3L) return(page_data)

  top_sigs    <- character(n_pages)
  bottom_sigs <- character(n_pages)

  for (k in seq_len(n_pages)) {
    page <- page_data[[k]]
    if (nrow(page) == 0L) next
    page_height <- max(page$y + page$height, na.rm = TRUE)
    if (!is.finite(page_height) || page_height == 0) next

    top_zone <- page[page$y < page_height * margin, , drop = FALSE]
    if (nrow(top_zone) > 0L) {
      top_zone <- top_zone[order(top_zone$y, top_zone$x), ]
      top_sigs[[k]] <- paste(top_zone$text, collapse = " ")
    }

    bottom_zone <- page[page$y > page_height * (1 - margin), , drop = FALSE]
    if (nrow(bottom_zone) > 0L) {
      bottom_zone <- bottom_zone[order(bottom_zone$y, bottom_zone$x), ]
      sig <- paste(bottom_zone$text, collapse = " ")
      # Normalise page numbers to a placeholder so "Page 1" and "Page 2" match
      sig <- gsub("\\b\\d+\\b", "<N>", sig)
      bottom_sigs[[k]] <- sig
    }
  }

  min_hits   <- max(2L, ceiling(n_pages * min_pages_frac))
  top_counts <- table(top_sigs[nzchar(top_sigs)])
  bot_counts <- table(bottom_sigs[nzchar(bottom_sigs)])

  repeated_top <- names(top_counts[top_counts >= min_hits])
  repeated_bot <- names(bot_counts[bot_counts >= min_hits])

  if (!length(repeated_top) && !length(repeated_bot)) return(page_data)

  for (k in seq_len(n_pages)) {
    page <- page_data[[k]]
    if (nrow(page) == 0L) next
    page_height <- max(page$y + page$height, na.rm = TRUE)
    if (!is.finite(page_height) || page_height == 0) next

    keep <- rep(TRUE, nrow(page))

    if (length(repeated_top) && top_sigs[[k]] %in% repeated_top) {
      keep <- keep & !(page$y < page_height * margin)
    }
    if (length(repeated_bot) && bottom_sigs[[k]] %in% repeated_bot) {
      keep <- keep & !(page$y > page_height * (1 - margin))
    }

    page_data[[k]] <- page[keep, , drop = FALSE]
  }

  page_data
}

# ── Per-page layout classification ───────────────────────────────────────────

pdf_classify_page <- function(page, margin = 0.08) {
  if (nrow(page) < 20L) return("single-column")

  page_width <- max(page$x + page$width, na.rm = TRUE)
  page_height <- max(page$y + page$height, na.rm = TRUE)
  if (!is.finite(page_width) || page_width == 0) return("single-column")
  if (!is.finite(page_height) || page_height == 0) return("single-column")

  body <- page[
    page$y > page_height * margin & page$y < page_height * (1 - margin), ,
    drop = FALSE
  ]
  if (nrow(body) < 20L) return("single-column")

  rows <- pdf_group_into_rows(body)
  if (length(rows) == 0L) return("single-column")

  row_types <- vapply(rows, pdf_classify_row, character(1L),
                      page_width = page_width)

  frac_two_col <- mean(row_types == "two-col")
  if (frac_two_col > 0.6)  return("two-column")
  if (frac_two_col < 0.2)  return("single-column")
  "mixed"
}

pdf_classify_row <- function(r, page_width) {
  if (nrow(r) < 2L) return("full-width")
  x_mid <- (r$x + r$width / 2) / page_width
  n_left   <- sum(x_mid < 0.42)
  n_right  <- sum(x_mid > 0.58)
  n_middle <- sum(x_mid >= 0.42 & x_mid <= 0.58)
  n_total  <- n_left + n_middle + n_right
  if (n_left >= 1L && n_right >= 1L && n_middle / n_total < 0.25) {
    "two-col"
  } else {
    "full-width"
  }
}

pdf_split_page_zones <- function(page, margin = 0.08,
                                 min_full_width_run = 3L) {
  page_width  <- max(page$x + page$width, na.rm = TRUE)
  page_height <- max(page$y + page$height, na.rm = TRUE)

  body <- page[
    page$y > page_height * margin & page$y < page_height * (1 - margin), ,
    drop = FALSE
  ]
  if (nrow(body) == 0L) {
    return(list(list(type = "single-column", words = body)))
  }

  rows <- pdf_group_into_rows(body)
  if (length(rows) == 0L) {
    return(list(list(type = "single-column", words = body)))
  }

  row_types <- vapply(rows, pdf_classify_row, character(1L),
                      page_width = page_width)

  # Smooth: only treat full-width runs of min_full_width_run+ rows as zones;
  # isolated full-width rows inside two-column text are reclassified as
  # two-column to avoid zone fragmentation.
  smoothed <- row_types
  n <- length(smoothed)
  i <- 1L
  while (i <= n) {
    if (smoothed[[i]] == "full-width") {
      j <- i + 1L
      while (j <= n && smoothed[[j]] == "full-width") j <- j + 1L
      if (j - i < min_full_width_run) {
        smoothed[i:(j - 1L)] <- "two-col"
      }
      i <- j
    } else {
      i <- i + 1L
    }
  }

  zones  <- list()
  i      <- 1L
  while (i <= length(rows)) {
    cur_type <- smoothed[[i]]
    j <- i + 1L
    while (j <= length(rows) && smoothed[[j]] == cur_type) j <- j + 1L
    zone_words <- do.call(rbind, rows[i:(j - 1L)])
    zone_label <- if (cur_type == "two-col") "two-column" else "single-column"
    zones <- c(zones, list(list(type = zone_label, words = zone_words)))
    i <- j
  }

  zones
}

words_to_text <- function(words) {
  if (nrow(words) == 0L) {
    return("")
  }
  if (nrow(words) == 1L) {
    return(words$text)
  }
  words <- words[order(words$y, words$x), ]
  threshold <- max(median(words$height, na.rm = TRUE) * 0.5, 1)
  line_id <- integer(nrow(words))
  line_id[[1L]] <- 1L
  for (i in seq_len(nrow(words))[-1L]) {
    line_id[[i]] <- line_id[[i - 1L]] +
      (words$y[[i]] - words$y[[i - 1L]] > threshold)
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
  pdftools::pdf_convert(
    path,
    format = "png",
    dpi = dpi,
    filenames = filenames,
    verbose = FALSE
  )

  engine <- tesseract::tesseract(language)
  texts <- vapply(
    filenames,
    \(p) tesseract::ocr(p, engine = engine),
    character(1L)
  )
  paste(texts, collapse = "\n\n")
}

# ── Table helpers ────────────────────────────────────────────────────────────

pdf_group_into_rows <- function(page) {
  if (nrow(page) == 0L) return(list())
  page      <- page[order(page$y, page$x), ]
  threshold <- max(median(page$height, na.rm = TRUE) * 0.5, 1)

  row_id       <- integer(nrow(page))
  row_id[[1L]] <- 1L
  for (i in seq_len(nrow(page))[-1L]) {
    row_id[[i]] <- row_id[[i - 1L]] +
      (page$y[[i]] - page$y[[i - 1L]] > threshold)
  }

  lapply(split(page, row_id), function(r) r[order(r$x), ])
}

pdf_gap_threshold <- function(rows) {
  all_h <- unlist(lapply(rows, `[[`, "height"))
  median(all_h, na.rm = TRUE) * 2.5
}

pdf_detect_table_regions <- function(rows, min_table_rows = 2L, min_cols = 3L) {
  n      <- length(rows)
  is_tbl <- logical(n)
  if (n < min_table_rows) return(is_tbl)

  gap_thr   <- pdf_gap_threshold(rows)
  tolerance <- gap_thr * 0.5
  col_bnds  <- lapply(rows, pdf_row_column_bounds, gap_threshold = gap_thr)

  i <- 1L
  while (i <= n) {
    if (length(col_bnds[[i]]) < min_cols) {
      i <- i + 1L
      next
    }
    ref <- col_bnds[[i]]
    j   <- i + 1L
    while (j <= n && pdf_columns_compatible(ref, col_bnds[[j]], tolerance)) {
      j <- j + 1L
    }
    if (j - i >= min_table_rows) {
      candidate_rows <- rows[i:(j - 1L)]
      if (pdf_region_is_tabular(candidate_rows, gap_thr)) {
        is_tbl[i:(j - 1L)] <- TRUE
      }
      i <- j
    } else {
      i <- i + 1L
    }
  }
  is_tbl
}

pdf_region_is_tabular <- function(rows, gap_threshold, max_median_words = 8L,
                                  max_cols = 10L, min_fill_ratio = 0.4) {
  ref_cols <- pdf_row_column_bounds(rows[[1L]], gap_threshold)
  n_cols <- length(ref_cols)
  if (n_cols < 2L) return(FALSE)
  if (n_cols > max_cols) return(FALSE)

  total_cells <- 0L
  filled_cells <- 0L
  cell_word_counts <- vapply(rows, function(row_df) {
    cells <- pdf_assign_words_to_cols(row_df, ref_cols)
    total_cells <<- total_cells + length(cells)
    non_empty <- cells[nzchar(trimws(cells))]
    filled_cells <<- filled_cells + length(non_empty)
    if (length(non_empty) == 0L) return(0L)
    as.integer(median(vapply(
      non_empty,
      function(c) length(strsplit(trimws(c), "\\s+")[[1L]]),
      integer(1L)
    )))
  }, integer(1L))

  if (total_cells > 0L && filled_cells / total_cells < min_fill_ratio) {
    return(FALSE)
  }

  median(cell_word_counts, na.rm = TRUE) <= max_median_words
}

pdf_row_column_bounds <- function(row_df, gap_threshold) {
  row_df <- row_df[order(row_df$x), ]
  n      <- nrow(row_df)
  if (n == 0L) return(numeric(0L))
  if (n == 1L) return(row_df$x[[1L]])

  right_edges   <- row_df$x + row_df$width
  gaps          <- row_df$x[-1L] - right_edges[-n]
  col_start_idx <- c(1L, which(gaps > gap_threshold) + 1L)
  row_df$x[col_start_idx]
}

pdf_columns_compatible <- function(ref, cand, tolerance) {
  n_ref  <- length(ref)
  n_cand <- length(cand)
  if (n_cand == 0L || n_ref == 0L)       return(FALSE)
  if (n_cand < n_ref - 1L || n_cand > n_ref) return(FALSE)
  n_check <- min(n_ref, n_cand)
  all(abs(ref[seq_len(n_check)] - cand[seq_len(n_check)]) <= tolerance)
}

pdf_rows_to_markdown_table <- function(rows, gap_threshold) {
  if (length(rows) == 0L) return("")
  if (length(rows) == 1L) return(paste(rows[[1L]]$text, collapse = " "))

  ref_cols <- pdf_row_column_bounds(rows[[1L]], gap_threshold)
  n_cols   <- length(ref_cols)
  if (n_cols < 2L) {
    return(paste(
      vapply(rows, function(r) paste(r$text, collapse = " "), character(1L)),
      collapse = "\n"
    ))
  }

  md_rows <- vapply(rows, function(row_df) {
    cells <- pdf_assign_words_to_cols(row_df, ref_cols)
    paste0("| ", paste(cells, collapse = " | "), " |")
  }, character(1L))

  sep <- paste0("| ", paste(rep("---", n_cols), collapse = " | "), " |")
  paste(c(md_rows[[1L]], sep, md_rows[-1L]), collapse = "\n")
}

pdf_assign_words_to_cols <- function(row_df, col_starts) {
  n_cols <- length(col_starts)
  cells  <- character(n_cols)

  for (i in seq_len(nrow(row_df))) {
    idx <- findInterval(row_df$x[[i]], col_starts)
    idx <- max(1L, min(n_cols, idx))
    cells[[idx]] <- if (nzchar(cells[[idx]])) {
      paste(cells[[idx]], row_df$text[[i]])
    } else {
      row_df$text[[i]]
    }
  }
  cells
}

pdf_row_to_text <- function(row_df, doc_median_height) {
  if (nrow(row_df) == 0L) return("")
  text  <- paste(row_df$text, collapse = " ")
  row_h <- mean(row_df$height, na.rm = TRUE)
  if (row_h > doc_median_height * 1.5) {
    paste0("## ", text)
  } else if (row_h > doc_median_height * 1.25) {
    paste0("### ", text)
  } else {
    text
  }
}

# ── Formula reconstruction ───────────────────────────────────────────────────

pdf_row_has_formulas <- function(row_df, doc_median_height,
                                 height_ratio = 0.75) {
  if (nrow(row_df) < 2L) return(FALSE)
  heights <- row_df$height
  small <- heights < doc_median_height * height_ratio & heights > 0
  if (!any(small)) return(FALSE)
  math_re <- paste0(
    "[=+<>\\-×÷∑∫√≤≥≠±",
    "α-ωΑ-Ω",
    "∈∉∪∩∞",
    "⁰¹²³⁴-⁹",
    "₀-₉]"
  )
  any(grepl(math_re, row_df$text, perl = TRUE))
}

pdf_reconstruct_formula <- function(row_df, doc_median_height,
                                    height_ratio = 0.75) {
  if (nrow(row_df) == 0L) return("")
  row_df <- row_df[order(row_df$x), ]

  med_h <- doc_median_height
  parts <- character(nrow(row_df))

  for (i in seq_len(nrow(row_df))) {
    w <- row_df[i, ]
    is_small <- w$height < med_h * height_ratio && w$height > 0

    if (is_small && i > 1L) {
      prev <- row_df[i - 1L, ]
      prev_baseline <- prev$y + prev$height
      cur_baseline  <- w$y + w$height
      cur_top       <- w$y

      if (cur_top > prev$y + prev$height * 0.3) {
        parts[[i]] <- paste0("_{", w$text, "}")
      } else if (cur_baseline < prev$y + prev$height * 0.7) {
        parts[[i]] <- paste0("^{", w$text, "}")
      } else {
        parts[[i]] <- w$text
      }
    } else {
      parts[[i]] <- w$text
    }
  }

  # Join: attach sub/superscripts to preceding token without space
  result <- parts[[1L]]
  for (i in seq_along(parts)[-1L]) {
    if (grepl("^[_^]\\{", parts[[i]])) {
      result <- paste0(result, parts[[i]])
    } else {
      result <- paste(result, parts[[i]])
    }
  }
  result
}
