# mdma (development version)

## v0.1.0

* `mdma_pdf()` now also falls back to OCR when the extracted text contains more
  than 10 CID font artefacts (`(cid:N)` patterns), which occur when a PDF's
  font encoding cannot be resolved by the text extractor.
* `mdma_clean()` and `mdma_pdf()` now accept a character vector as their first
  argument. When the input has length greater than 1, files are processed in a
  `for` loop with a `cli` progress bar.
* Added `mdma_clean()` for cleaning PDF-extracted Markdown text for LLM
  consumption. Supports three intrusion levels (`"basic"`, `"moderate"`,
  `"extreme"`), each a superset of the previous.
* Added `mdma_pdf()` for converting PDF files to Markdown using
  `ragnar::read_as_markdown()`. Image-based PDFs are automatically detected
  and processed with OCR via the `tesseract` and `pdftools` packages. The
  `clean` argument controls post-conversion text cleaning via `mdma_clean()`
  (defaults to `"basic"`).
* `mdma_pdf()` now automatically detects two-column PDF layouts when `pdftools`
  is installed, using coordinate-aware extraction via `pdftools::pdf_data()` to
  correctly order text column by column.
