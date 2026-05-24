# mdma (development version)

* `clean_markdown()` and `pdf_to_md()` now accept a character vector as their first argument. When the input has length greater than 1, files are processed in a `for` loop with a `cli` progress bar.

* Added `clean_markdown()` for cleaning PDF-extracted Markdown text for LLM
  consumption. Supports three aggressiveness levels (`"basic"`, `"moderate"`,
  `"aggressive"`), each a superset of the previous.

* Added `pdf_to_md()` for converting PDF files to Markdown using
  `ragnar::read_as_markdown()`. Image-based PDFs are automatically detected
  and processed with OCR via the `tesseract` and `pdftools` packages. The
  `clean` argument controls post-conversion text cleaning via `clean_markdown()`
  (defaults to `"basic"`).

* `pdf_to_md()` now automatically detects two-column PDF layouts when `pdftools` is installed, using coordinate-aware extraction via `pdftools::pdf_data()` to correctly order text column by column.
