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

### Interactive session

`mdma_session()` opens a Shiny application — the primary way to use MDMA.

```r
library(mdma)
mdma_session()
```

The app has two tabs:

- **PDF to Markdown** — select one or more PDF files, choose an output folder,
  set the cleaning level, and click *Convert*. A progress bar tracks each file,
  and you can inspect any output `.md` file directly in the app.
- **Clean Markdown** — select `.md` or `.txt` files and apply the cleaning
  pipeline without re-extracting from a PDF. Useful when you already have
  Markdown text that needs artefact removal.

Both tabs write files to disk and let you review the results before passing
them to an LLM.

### Programmatic use

`mdma_pdf()` and `mdma_clean()` are available for scripting and pipeline use.

#### Convert a PDF to Markdown

```r
# Output written next to the input file (report.md)
mdma_pdf("report.pdf")

# Write to a specific location
mdma_pdf("report.pdf", output = "llm_ready/report.md")

# Control the cleaning level
mdma_pdf("report.pdf", clean = "moderate")   # join wrapped lines, deduplicate headers
mdma_pdf("report.pdf", clean = "extreme")    # also strip copyright and blank-page lines
mdma_pdf("report.pdf", clean = "none")       # raw extraction, no cleaning
```

#### OCR for image-based PDFs

When the extracted text contains fewer than `min_chars` non-whitespace
characters (default: 100), `mdma_pdf()` automatically falls back to OCR via
`pdftools` and `tesseract`:

```r
mdma_pdf("scan.pdf", language = "nld")       # Dutch OCR
mdma_pdf("scan.pdf", dpi = 600L)             # higher resolution
mdma_pdf("report.pdf", min_chars = 500L)     # stricter text threshold
```

#### Convert multiple PDFs at once

Pass a character vector of paths to process a batch:

```r
pdfs <- list.files("papers/", pattern = "\\.pdf$", full.names = TRUE)
outputs <- file.path("llm_ready", sub("\\.pdf$", ".md", basename(pdfs)))
mdma_pdf(pdfs, output = outputs)
```

#### Clean Markdown text directly

`mdma_clean()` works on any Markdown string, not just output from `mdma_pdf()`:

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
