#' Clean Markdown text for LLM consumption
#'
#' Applies a sequence of cleaning steps to Markdown text produced from PDF
#' conversion. Steps are grouped into three aggressiveness levels; each level
#' includes all steps from the levels below it.
#'
#' @details
#' ## `"basic"` — high value, low risk
#' * Remove isolated page numbers (lines containing only digits).
#' * Remove table-of-contents leader lines (e.g. `Introduction ......... 3`).
#' * Repair soft-hyphenation at line breaks (`algo-\nrithm` → `algorithm`).
#'
#' ## `"moderate"` — medium value, some risk
#' * Join hard-wrapped paragraph lines split by PDF column layout.
#' * Remove repeated running headers/footers (lines appearing 3+ times that
#'   are no longer than 80 characters).
#' * Collapse redundant duplicate headings (consecutive identical headings, or
#'   a plain-text title immediately preceding a matching heading).
#'
#' ## `"aggressive"` — targeted but potentially lossy
#' * Remove "This page intentionally left blank" boilerplate.
#' * Strip standalone copyright and DOI lines.
#'
#' All levels also normalize line endings and collapse runs of three or more
#' consecutive blank lines to two.
#'
#' @param text `[character]` A character string of Markdown content, such as
#'   the output of [pdf_to_md()] or [ragnar::read_as_markdown()]. A character
#'   vector of length greater than 1 is processed element by element with a
#'   progress bar.
#' @param level `[string]` Aggressiveness level: `"basic"`, `"moderate"`, or
#'   `"aggressive"`. Each level includes all steps from the levels below it.
#'   Defaults to `"basic"`.
#'
#' @return A cleaned character string, or a character vector of the same length
#'   as `text` when `length(text) > 1`.
#' @export
#'
#' @examples
#' md <- "Introduction ......... 3\n\nalgo-\nrithm\n\n   42\n"
#' clean_markdown(md)
#'
#' clean_markdown(md, level = "moderate")
clean_markdown <- function(text, level = "basic") {
  if (length(text) > 1) {
    results <- character(length(text))
    for (i in cli::cli_progress_along(text, name = "Cleaning")) {
      results[[i]] <- clean_markdown(text[[i]], level = level)
    }
    return(results)
  }
  level <- match.arg(level, c("basic", "moderate", "aggressive"))
  text <- as.character(text)
  text <- gsub("\r\n|\r", "\n", text)

  text <- remove_page_numbers(text)
  text <- remove_toc_leaders(text)
  text <- repair_soft_hyphens(text)

  if (level %in% c("moderate", "aggressive")) {
    text <- join_wrapped_lines(text)
    text <- deduplicate_running_lines(text)
    text <- collapse_duplicate_headings(text)
  }

  if (level == "aggressive") {
    text <- remove_blank_page_boilerplate(text)
    text <- remove_copyright_lines(text)
  }

  text <- gsub("\n{3,}", "\n\n", text)
  trimws(text)
}

remove_page_numbers <- function(text) {
  gsub("(?m)^[ \t]*\\d+[ \t]*$", "", text, perl = TRUE)
}

remove_toc_leaders <- function(text) {
  # Dot leaders: "Introduction ......... 3"
  text <- gsub("(?m)^\\S[^\n]*[.]{4,}[ \t]*\\d*[ \t]*$", "", text, perl = TRUE)
  # Dash leaders: "Methods ---------- 12" (6+ to avoid matching --- rules)
  text <- gsub("(?m)^\\S[^\n]*[-]{6,}[ \t]*\\d*[ \t]*$", "", text, perl = TRUE)
  text
}

repair_soft_hyphens <- function(text) {
  gsub("-\n([a-z])", "\\1", text)
}

join_wrapped_lines <- function(text) {
  lines <- strsplit(text, "\n")[[1L]]
  in_code <- FALSE
  out <- character(length(lines))
  n_out <- 0L

  md_block_re <- "^(#{1,6}[[:space:]]|[*][[:space:]]|[-][[:space:]]|>[[:space:]]|[[:digit:]]+\\.[[:space:]]|```|\\|)"
  sentence_end_re <- "[.?!:;][\"']?[[:space:]]*$"

  for (line in lines) {
    if (grepl("^```", line)) in_code <- !in_code

    merge <- !in_code &&
      n_out > 0L &&
      nzchar(trimws(out[[n_out]])) &&
      nzchar(trimws(line)) &&
      !grepl(sentence_end_re, out[[n_out]]) &&
      !grepl(md_block_re, trimws(out[[n_out]])) &&
      !grepl(md_block_re, trimws(line))

    if (merge) {
      out[[n_out]] <- paste0(trimws(out[[n_out]]), " ", trimws(line))
    } else {
      n_out <- n_out + 1L
      out[[n_out]] <- line
    }
  }

  paste(out[seq_len(n_out)], collapse = "\n")
}

deduplicate_running_lines <- function(text, min_occurrences = 3L, max_chars = 80L) {
  lines <- strsplit(text, "\n")[[1L]]
  trimmed <- trimws(lines)

  candidates <- trimmed[nzchar(trimmed) & nchar(trimmed) <= max_chars]
  counts <- table(candidates)
  repeated <- names(counts[counts >= min_occurrences])

  if (!length(repeated)) {
    return(text)
  }

  seen <- character(0L)
  keep <- logical(length(lines))

  for (i in seq_along(lines)) {
    t <- trimmed[[i]]
    if (t %in% repeated) {
      if (t %in% seen) {
        keep[[i]] <- FALSE
      } else {
        seen <- c(seen, t)
        keep[[i]] <- TRUE
      }
    } else {
      keep[[i]] <- TRUE
    }
  }

  paste(lines[keep], collapse = "\n")
}

collapse_duplicate_headings <- function(text) {
  lines <- strsplit(text, "\n")[[1L]]
  result <- character(length(lines))
  n <- 0L

  strip_heading <- \(x) trimws(sub("^#{1,6}[[:space:]]+", "", x))

  for (line in lines) {
    if (grepl("^#{1,6}[[:space:]]", line) && n > 0L) {
      heading_text <- strip_heading(line)
      for (j in rev(seq_len(n))) {
        if (nzchar(trimws(result[[j]]))) {
          if (strip_heading(result[[j]]) == heading_text) result[[j]] <- ""
          break
        }
      }
    }
    n <- n + 1L
    result[[n]] <- line
  }

  paste(result[seq_len(n)], collapse = "\n")
}

remove_blank_page_boilerplate <- function(text) {
  gsub(
    "(?im)^[ \t]*\\[?this page (is )?intentionally left blank\\.?\\]?[ \t]*$",
    "",
    text,
    perl = TRUE
  )
}

remove_copyright_lines <- function(text) {
  gsub(
    "(?im)^[ \t]*(©|copyright\\b|\\(c\\)[[:space:]]+[[:digit:]]{4}|doi:[[:space:]]*10\\.|cc[[:space:]]+by).*$",
    "",
    text,
    perl = TRUE
  )
}
