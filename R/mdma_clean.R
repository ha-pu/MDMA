#' Clean Markdown text for LLM consumption
#'
#' Applies a sequence of cleaning steps to Markdown text produced from PDF
#' conversion. Steps are grouped into three intrusion levels; each level
#' includes all steps from the levels below it.
#'
#' @details
#' ## `"basic"` — high value, low risk
#' * Remove spurious Markdown table blocks produced when `ragnar` misinterprets
#'   two-column PDF layout as tables (small blocks with many empty cells are
#'   collapsed to prose).
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
#' ## `"extreme"` — targeted but potentially lossy
#' * Remove "This page intentionally left blank" boilerplate.
#' * Strip standalone copyright and DOI lines.
#'
#' ## `flag_math = TRUE` — inline math wrapping (opt-in)
#' * Detects Greek letters (α–ω, Α–Ω), math Unicode operators (∑ ∫ √ ≤ ≥
#'   ≠ ± × ÷ → ∞ ∈ ∪ …), and superscript/subscript digits (¹²³ ₀₁₂ …).
#' * Lines where ≥ 15 % of characters are math symbols are wrapped in
#'   `$$...$$` (display math). Math-bearing tokens in prose lines are wrapped
#'   as `$...$` (inline math). Already-delimited content is left untouched.
#' * Disabled by default because false positives (non-math content wrapped in
#'   LaTeX delimiters) are disruptive in plain-text documents.
#'
#' All levels also normalize line endings and collapse runs of three or more
#' consecutive blank lines to two.
#'
#' @param text `[character]` A character string of Markdown content, such as
#'   the output of [mdma_pdf()] or [ragnar::read_as_markdown()]. A character
#'   vector of length greater than 1 is processed element by element with a
#'   progress bar.
#' @param level `[string]` Intrusion level: `"basic"`, `"moderate"`, or
#'   `"extreme"`. Each level includes all steps from the levels below it.
#'   Defaults to `"basic"`.
#' @param flag_math `[logical(1)]` When `TRUE`, detects likely mathematical
#'   expressions — Greek letters (α–ω, Α–Ω), math Unicode symbols (∑ ∫ √ ≤ ≥
#'   ≠ ± × ÷ → ∞ ∈ ∪ …), and superscript/subscript digits (¹²³ ₀₁₂ …) —
#'   and wraps them in `$...$` (inline) or `$$...$$` (display) LaTeX
#'   delimiters. Disabled by default.
#'
#' @return A cleaned character string, or a character vector of the same length
#'   as `text` when `length(text) > 1`.
#' @export
#'
#' @examples
#' md <- "Introduction ......... 3\n\nalgo-\nrithm\n\n   42\n"
#' mdma_clean(md)
#'
#' mdma_clean(md, level = "moderate")
#'
#' mdma_clean("where α = 0.05", flag_math = TRUE)
mdma_clean <- function(text, level = "basic", flag_math = FALSE) {
  if (length(text) > 1) {
    results <- character(length(text))
    for (i in cli::cli_progress_along(text, name = "Cleaning")) {
      results[[i]] <- mdma_clean(text[[i]], level = level, flag_math = flag_math)
    }
    return(results)
  }
  level <- match.arg(level, c("basic", "moderate", "extreme"))
  text <- as.character(text)
  text <- gsub("\r\n|\r", "\n", text)

  text <- remove_spurious_tables(text)
  text <- remove_page_numbers(text)
  text <- remove_toc_leaders(text)
  text <- repair_soft_hyphens(text)

  if (level %in% c("moderate", "extreme")) {
    text <- join_wrapped_lines(text)
    text <- deduplicate_running_lines(text)
    text <- collapse_duplicate_headings(text)
  }

  if (level == "extreme") {
    text <- remove_blank_page_boilerplate(text)
    text <- remove_copyright_lines(text)
  }

  if (flag_math) text <- flag_math_spans(text)

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
    if (grepl("^```", line)) {
      in_code <- !in_code
    }

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

deduplicate_running_lines <- function(
  text,
  min_occurrences = 3L,
  max_chars = 80L
) {
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
          if (strip_heading(result[[j]]) == heading_text) {
            result[[j]] <- ""
          }
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

# Collapse markdown table blocks that were created by ragnar when it
# misinterprets two-column PDF layout as tables.  A block of consecutive
# `|`-starting lines is treated as spurious when:
#   - it has fewer than 3 rows (no room for header + separator + ≥1 data row), OR
#   - it has ≥ min_cols columns and ≥ empty_threshold fraction of empty /
#     separator-only cells (regardless of row count).
# Spurious blocks are collapsed to a single prose line of the non-empty cells.
remove_spurious_tables <- function(
  text,
  min_cols = 7L,
  empty_threshold = 0.65
) {
  lines <- strsplit(text, "\n")[[1L]]
  is_table_line <- grepl("^\\s*\\|", lines)

  result <- character(length(lines))
  n_out <- 0L
  i <- 1L

  parse_cells <- function(l) {
    l2 <- sub("^\\s*\\|", "", l)
    l2 <- sub("\\|\\s*$", "", l2)
    trimws(strsplit(l2, "\\|", fixed = TRUE)[[1L]])
  }

  while (i <= length(lines)) {
    if (is_table_line[[i]]) {
      j <- i
      while (j <= length(lines) && is_table_line[[j]]) j <- j + 1L
      block <- lines[i:(j - 1L)]
      n_rows <- length(block)

      spurious <- n_rows < 3L
      if (!spurious) {
        all_cells <- unlist(lapply(block, parse_cells))
        n_cols <- max(vapply(block, function(l) length(parse_cells(l)), integer(1L)))
        is_empty <- grepl("^[-[:space:]]*$", all_cells)
        spurious <- n_cols >= min_cols &&
          sum(is_empty) / length(all_cells) >= empty_threshold
      }

      if (spurious) {
        all_cells <- unlist(lapply(block, parse_cells))
        content <- all_cells[!grepl("^[-[:space:]]*$", all_cells)]
        content <- content[nzchar(content)]
        if (length(content) > 0L) {
          n_out <- n_out + 1L
          result[[n_out]] <- paste(content, collapse = " ")
        }
      } else {
        for (tl in block) {
          n_out <- n_out + 1L
          result[[n_out]] <- tl
        }
      }
      i <- j
    } else {
      n_out <- n_out + 1L
      result[[n_out]] <- lines[[i]]
      i <- i + 1L
    }
  }

  paste(result[seq_len(n_out)], collapse = "\n")
}

# -- Math detection ------------------------------------------------------------

# Character class matching Greek letters, math operators, and
# superscript/subscript digits that survive PDF extraction.
.math_char_re <- paste0(
  "[",
  "α-ω",                    # α-ω (Greek lowercase)
  "Α-Ω",                    # Α-Ω (Greek uppercase)
  "∑∫∂√",         # ∑∫∂√
  "≤≥≠≈",         # ≤≥≠≈
  "±×÷",               # ±×÷
  "→←↔",               # →←↔
  "∞∝",                     # ∞∝
  "∀∃",                     # ∀∃
  "∈∉",                     # ∈∉
  "⊂⊃∪∩",         # ⊂⊃∪∩
  "⊥⊕⊗",               # ⊥⊕⊗
  "¹²³",               # ¹²³
  "⁰⁴-⁹",              # ⁰⁴⁵⁶⁷⁸⁹
  "₀-₉",                    # ₀-₉
  "]"
)

flag_math_spans <- function(text) {
  lines   <- strsplit(text, "\n")[[1L]]
  in_code <- FALSE
  out     <- character(length(lines))
  for (i in seq_along(lines)) {
    l <- lines[[i]]
    if (grepl("^```", trimws(l))) in_code <- !in_code
    out[[i]] <- if (in_code) l else flag_math_line(l)
  }
  paste(out, collapse = "\n")
}

flag_math_line <- function(line) {
  stripped <- trimws(line)
  if (!nzchar(stripped))           return(line)
  if (grepl("^\\$\\$", stripped)) return(line)  # already display math
  if (grepl("^\\|",    stripped)) return(line)  # table row

  n_math <- lengths(regmatches(line, gregexpr(.math_char_re, line, perl = TRUE)))[[1L]]
  if (n_math == 0L) return(line)

  density <- n_math / nchar(line)

  # High-density line that isn't a structural Markdown element → display math
  if (density >= 0.15 &&
      !grepl("^[#>*`]|^-[[:space:]]|^[[:digit:]]+\\.[[:space:]]", stripped)) {
    return(paste0("$$", stripped, "$$"))
  }

  # Low-density line → wrap math-bearing tokens inline, protecting existing $
  gsub(
    paste0("(?<!\\$)([^[:space:]$]*", .math_char_re, "[^[:space:]$]*)(?!\\$)"),
    "$\\1$",
    line,
    perl = TRUE
  )
}
