# MDMA: Markdown Maker for LLMs

**MDMA** is a means for spiritual expansion and allows you to talk to computers
more easily. To facilitate such human-to-silicon conversations, MDMA converts
PDF files to Markdown for use with large language models.
It wraps [`ragnar::read_as_markdown()`](https://github.com/tidyverse/ragnar)
for text extraction, falls back to OCR for image-based PDFs, and includes a
cleaning pipeline that removes common PDF artefacts before you pass the text
to an LLM.

## Installation

```r
# install.packages("pak")
pak::pak("ha-pu/mdma")
```

## Usage

### Convert a PDF to Markdown

```r
library(mdma)

# Output written next to the input file (report.md)
mdma_pdf("report.pdf")

# Write to a specific location
mdma_pdf("report.pdf", output = "llm_ready/report.md")

# Overwrite an existing file
mdma_pdf("report.pdf", output = "report.md", overwrite = TRUE)
```

By default, `mdma_pdf()` applies `"basic"` cleaning to the extracted text.
Control this with the `clean` argument:

```r
mdma_pdf("report.pdf", clean = "moderate")   # join wrapped lines, deduplicate headers
mdma_pdf("report.pdf", clean = "extreme") # also strip copyright and blank-page lines
mdma_pdf("report.pdf", clean = "none")       # raw extraction, no cleaning
```

### OCR for image-based PDFs

When the extracted text contains fewer than `min_chars` non-whitespace
characters (default: 100), `mdma_pdf()` automatically falls back to OCR via
`pdftools` and `tesseract`. You can tune the threshold, language, and
resolution:

```r
mdma_pdf("scan.pdf", language = "nld")           # Dutch OCR
mdma_pdf("scan.pdf", dpi = 600L)                 # higher resolution
mdma_pdf("report.pdf", min_chars = 500L)         # stricter text threshold
```

### Convert multiple PDFs at once

Pass a character vector of paths to process a batch. A progress bar is shown
automatically:

```r
pdfs <- list.files("papers/", pattern = "\\.pdf$", full.names = TRUE)
mdma_pdf(pdfs)
```

Output paths default to the same directory as each input file. Supply a
matching vector to redirect them:

```r
outputs <- file.path("llm_ready", sub("\\.pdf$", ".md", basename(pdfs)))
mdma_pdf(pdfs, output = outputs)
```

### Clean Markdown text directly

`mdma_clean()` can be used on any Markdown string, not just output from
`mdma_pdf()`. It also accepts a character vector to clean multiple strings in
one call:

```r
md <- readLines("extracted.md") |> paste(collapse = "\n")
mdma_clean(md)
mdma_clean(md, level = "moderate")

# Clean a batch of strings
mdma_clean(c(text_a, text_b, text_c))
```

## Cleaning levels

Each level includes all steps from the levels below it.

| Level | Steps |
|---|---|
| `"basic"` | Remove isolated page numbers; remove TOC leader lines (`Introduction ......... 3`); repair soft-hyphenated line breaks (`algo-\nrithm` → `algorithm`) |
| `"moderate"` | Join hard-wrapped paragraph lines; remove repeated running headers/footers (lines appearing 3+ times, ≤ 80 characters); collapse redundant duplicate headings |
| `"extreme"` | Remove "This page intentionally left blank" boilerplate; strip standalone copyright and DOI lines |

All levels also normalize line endings and collapse runs of three or more
blank lines.

## License

MIT
