`%||%` <- function(x, y) if (is.null(x)) y else x

#' Launch the MDMA Shiny application
#'
#' Opens an interactive Shiny application for running [mdma_pdf()] and
#' [mdma_clean()].
#'
#' @param launch.browser `[logical(1)]` Open a browser window automatically.
#'   Defaults to `TRUE`.
#' @param ... Passed to [shiny::runApp()].
#'
#' @return Called for its side effect.
#' @export
mdma_session <- function(launch.browser = TRUE, ...) {
  shiny::runApp(mdma_app(), launch.browser = launch.browser, ...)
}

mdma_app <- function() {
  shiny::shinyApp(ui = mdma_ui(), server = mdma_server)
}

mdma_ui <- function() {
  bslib::page_navbar(
    title = "MDMA",
    theme = bslib::bs_theme(version = 5, bootswatch = "vapor"),

    # ── Tab 1: PDF to Markdown ────────────────────────────────────────────────
    bslib::nav_panel(
      "PDF to Markdown",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 340,
          shiny::tags$h6("Input", class = "text-muted fw-bold"),
          shinyFiles::shinyFilesButton(
            "pdf_files",
            "Select PDF files…",
            title = "Select PDF files",
            multiple = TRUE,
            class = "btn btn-outline-primary w-100 mb-1"
          ),
          shiny::uiOutput("pdf_files_info"),
          shiny::tags$hr(class = "my-2"),
          shiny::tags$h6("Output", class = "text-muted fw-bold"),
          shinyFiles::shinyDirButton(
            "pdf_outdir",
            "Output folder…",
            title = "Select output folder",
            class = "btn btn-outline-secondary w-100 mb-1"
          ),
          shiny::uiOutput("pdf_outdir_info"),
          shiny::checkboxInput(
            "pdf_overwrite",
            "Overwrite existing .md files",
            FALSE
          ),
          shiny::tags$hr(class = "my-2"),
          shiny::tags$h6("Cleaning", class = "text-muted fw-bold"),
          shiny::selectInput(
            "pdf_clean",
            NULL,
            choices = c("basic", "moderate", "extreme", "none"),
            selected = "basic"
          ),
          shiny::tags$details(
            class = "mb-3",
            shiny::tags$summary(
              class = "text-muted small",
              style = "cursor:pointer;",
              "Advanced options"
            ),
            shiny::div(
              class = "mt-2",
              shiny::numericInput(
                "pdf_min_chars",
                "OCR threshold (chars)",
                100L,
                min = 0L
              ),
              shiny::selectInput(
                "pdf_language",
                "OCR language",
                choices = c(
                  "English" = "eng",
                  "Dutch" = "nld",
                  "German" = "deu",
                  "French" = "fra",
                  "Spanish" = "spa",
                  "Norwegian" = "nor"
                )
              ),
              shiny::numericInput(
                "pdf_dpi",
                "OCR DPI",
                300L,
                min = 72L,
                max = 600L,
                step = 50L
              )
            )
          ),
          shiny::actionButton(
            "pdf_run",
            "Convert",
            class = "btn btn-primary w-100",
            icon = shiny::icon("play")
          )
        ),
        shiny::div(
          class = "p-3",
          shiny::uiOutput("pdf_status_ui"),
          shiny::verbatimTextOutput("pdf_log"),
          shiny::uiOutput("pdf_viewer_ui")
        )
      )
    ),

    # ── Tab 2: Clean Markdown ─────────────────────────────────────────────────
    bslib::nav_panel(
      "Clean Markdown",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 340,
          shiny::tags$h6("Input", class = "text-muted fw-bold"),
          shinyFiles::shinyFilesButton(
            "clean_files",
            "Select Markdown files…",
            title = "Select Markdown files",
            multiple = TRUE,
            class = "btn btn-outline-primary w-100 mb-1"
          ),
          shiny::uiOutput("clean_files_info"),
          shiny::tags$hr(class = "my-2"),
          shiny::tags$h6("Output", class = "text-muted fw-bold"),
          shinyFiles::shinyDirButton(
            "clean_outdir",
            "Output folder…",
            title = "Select output folder",
            class = "btn btn-outline-secondary w-100 mb-1"
          ),
          shiny::uiOutput("clean_outdir_info"),
          shiny::checkboxInput(
            "clean_overwrite",
            "Overwrite existing files",
            TRUE
          ),
          shiny::tags$hr(class = "my-2"),
          shiny::tags$h6("Cleaning level", class = "text-muted fw-bold"),
          shiny::selectInput(
            "clean_level",
            NULL,
            choices = c("basic", "moderate", "extreme"),
            selected = "basic"
          ),
          shiny::actionButton(
            "clean_run",
            "Clean",
            class = "btn btn-primary w-100",
            icon = shiny::icon("broom")
          )
        ),
        shiny::div(
          class = "p-3",
          shiny::uiOutput("clean_status_ui"),
          shiny::verbatimTextOutput("clean_log"),
          shiny::uiOutput("clean_viewer_ui")
        )
      )
    )
  )
}

mdma_server <- function(input, output, session) {
  roots <- mdma_shiny_roots()

  # ── PDF tab ────────────────────────────────────────────────────────────────
  shinyFiles::shinyFileChoose(
    input,
    "pdf_files",
    roots = roots,
    filetypes = c("pdf", "PDF")
  )
  shinyFiles::shinyDirChoose(input, "pdf_outdir", roots = roots)

  pdf_paths <- shiny::reactive({
    x <- input$pdf_files
    if (is.null(x) || is.integer(x)) {
      return(character(0))
    }
    as.character(shinyFiles::parseFilePaths(roots, x)$datapath)
  })

  pdf_outdir <- shiny::reactive({
    x <- input$pdf_outdir
    if (is.null(x) || is.integer(x)) {
      return(NULL)
    }
    d <- shinyFiles::parseDirPath(roots, x)
    if (!length(d) || !nzchar(d)) {
      return(NULL)
    }
    as.character(d)
  })

  output$pdf_files_info <- shiny::renderUI({
    p <- pdf_paths()
    if (!length(p)) {
      return(shiny::tags$p(
        "No files selected.",
        class = "text-muted small mb-0"
      ))
    }
    shiny::tags$p(paste(length(p), "file(s) selected."), class = "small mb-0")
  })

  output$pdf_outdir_info <- shiny::renderUI({
    d <- pdf_outdir()
    if (is.null(d)) {
      return(shiny::tags$p(
        "Same folder as input.",
        class = "text-muted small mb-0"
      ))
    }
    shiny::tags$p(d, class = "small mb-0 text-truncate", title = d)
  })

  pdf_log_rv <- shiny::reactiveVal(NULL)
  pdf_results <- shiny::reactiveVal(character(0))
  pdf_running <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(input$pdf_run, {
    paths <- pdf_paths()
    if (!length(paths)) {
      shiny::showNotification(
        "Select at least one PDF file first.",
        type = "warning"
      )
      return()
    }

    out_dir <- pdf_outdir()
    out_paths <- vapply(
      paths,
      function(p) {
        nm <- sub("\\.pdf$", ".md", basename(p), ignore.case = TRUE)
        if (!is.null(out_dir)) {
          file.path(out_dir, nm)
        } else {
          file.path(dirname(p), nm)
        }
      },
      character(1L)
    )

    pdf_log_rv(NULL)
    pdf_results(character(0))
    pdf_running(TRUE)

    log_lines <- character(0)
    results <- character(length(paths))

    shiny::withProgress(message = "Converting PDFs…", value = 0, {
      for (i in seq_along(paths)) {
        shiny::incProgress(1 / length(paths), detail = basename(paths[[i]]))
        captured <- character(0)

        ok <- tryCatch(
          {
            msgs <- capture.output(type = "message", {
              mdma_pdf(
                paths[[i]],
                output = out_paths[[i]],
                overwrite = isTRUE(input$pdf_overwrite),
                min_chars = as.integer(input$pdf_min_chars %||% 100L),
                language = input$pdf_language %||% "eng",
                dpi = as.integer(input$pdf_dpi %||% 300L),
                clean = input$pdf_clean %||% "basic"
              )
            })
            captured <- trimws(msgs[nzchar(trimws(msgs))])
            TRUE
          },
          error = function(e) {
            captured <<- conditionMessage(e)
            FALSE
          }
        )

        tag <- if (isTRUE(ok)) "[OK]  " else "[FAIL]"
        log_lines <- c(log_lines, paste(tag, basename(paths[[i]])))
        if (length(captured)) {
          log_lines <- c(log_lines, paste0("       ", captured))
        }
        if (isTRUE(ok)) {
          results[[i]] <- out_paths[[i]]
        }
        pdf_log_rv(paste(log_lines, collapse = "\n"))
      }
    })

    results <- results[nzchar(results)]
    pdf_results(results)
    pdf_running(FALSE)

    if (length(results)) {
      shiny::updateSelectInput(
        session,
        "pdf_view_file",
        choices = stats::setNames(results, basename(results)),
        selected = results[[1L]]
      )
    }
  })

  output$pdf_log <- shiny::renderText(pdf_log_rv() %||% "")

  output$pdf_status_ui <- shiny::renderUI({
    if (pdf_running()) {
      shiny::div(class = "alert alert-info py-2 mb-2", "Running…")
    } else if (length(pdf_results()) > 0L) {
      n <- length(pdf_results())
      shiny::div(
        class = "alert alert-success py-2 mb-2",
        shiny::icon("check"),
        " ",
        n,
        " file(s) converted."
      )
    } else if (!is.null(pdf_log_rv())) {
      shiny::div(
        class = "alert alert-danger py-2 mb-2",
        "Conversion failed — see log."
      )
    }
  })

  output$pdf_viewer_ui <- shiny::renderUI({
    results <- pdf_results()
    if (!length(results)) {
      return(NULL)
    }
    shiny::tagList(
      shiny::tags$hr(),
      shiny::selectInput(
        "pdf_view_file",
        "Inspect file:",
        choices = stats::setNames(results, basename(results)),
        selected = results[[1L]]
      ),
      shiny::div(
        class = "border rounded p-2",
        style = paste0(
          "max-height:450px; overflow-y:auto; white-space:pre-wrap;",
          " font-family:monospace; font-size:0.85rem;"
        ),
        shiny::textOutput("pdf_file_content", inline = TRUE)
      )
    )
  })

  output$pdf_file_content <- shiny::renderText({
    shiny::req(input$pdf_view_file)
    f <- input$pdf_view_file
    if (!file.exists(f)) {
      return("(file not found)")
    }
    paste(readLines(f, warn = FALSE), collapse = "\n")
  })

  # ── Clean tab ──────────────────────────────────────────────────────────────
  shinyFiles::shinyFileChoose(
    input,
    "clean_files",
    roots = roots,
    filetypes = c("md", "txt")
  )
  shinyFiles::shinyDirChoose(input, "clean_outdir", roots = roots)

  clean_paths <- shiny::reactive({
    x <- input$clean_files
    if (is.null(x) || is.integer(x)) {
      return(character(0))
    }
    as.character(shinyFiles::parseFilePaths(roots, x)$datapath)
  })

  clean_outdir <- shiny::reactive({
    x <- input$clean_outdir
    if (is.null(x) || is.integer(x)) {
      return(NULL)
    }
    d <- shinyFiles::parseDirPath(roots, x)
    if (!length(d) || !nzchar(d)) {
      return(NULL)
    }
    as.character(d)
  })

  output$clean_files_info <- shiny::renderUI({
    p <- clean_paths()
    if (!length(p)) {
      return(shiny::tags$p(
        "No files selected.",
        class = "text-muted small mb-0"
      ))
    }
    shiny::tags$p(paste(length(p), "file(s) selected."), class = "small mb-0")
  })

  output$clean_outdir_info <- shiny::renderUI({
    d <- clean_outdir()
    if (is.null(d)) {
      return(shiny::tags$p(
        "Same folder as input.",
        class = "text-muted small mb-0"
      ))
    }
    shiny::tags$p(d, class = "small mb-0 text-truncate", title = d)
  })

  clean_log_rv <- shiny::reactiveVal(NULL)
  clean_results <- shiny::reactiveVal(character(0))
  clean_running <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(input$clean_run, {
    paths <- clean_paths()
    if (!length(paths)) {
      shiny::showNotification(
        "Select at least one Markdown file first.",
        type = "warning"
      )
      return()
    }

    out_dir <- clean_outdir()
    overwrite <- isTRUE(input$clean_overwrite)

    out_paths <- vapply(
      paths,
      function(p) {
        candidate <- if (!is.null(out_dir)) {
          file.path(out_dir, basename(p))
        } else {
          p
        }
        # Writing back to same file without overwrite → add _cleaned suffix
        if (!overwrite && candidate == p) {
          candidate <- sub("(\\.[^.]+)$", "_cleaned\\1", p)
        }
        candidate
      },
      character(1L)
    )

    # Pre-flight collision check (exclude in-place overwrites)
    if (!overwrite) {
      collisions <- out_paths[file.exists(out_paths) & out_paths != paths]
      if (length(collisions)) {
        shiny::showNotification(
          paste0(
            length(collisions),
            " output file(s) already exist. ",
            "Enable ‘Overwrite’ or choose a different output folder."
          ),
          type = "error",
          duration = 10L
        )
        return()
      }
    }

    clean_log_rv(NULL)
    clean_results(character(0))
    clean_running(TRUE)

    log_lines <- character(0)
    results <- character(length(paths))

    shiny::withProgress(message = "Cleaning files…", value = 0, {
      for (i in seq_along(paths)) {
        shiny::incProgress(1 / length(paths), detail = basename(paths[[i]]))

        ok <- tryCatch(
          {
            raw <- paste(readLines(paths[[i]], warn = FALSE), collapse = "\n")
            cleaned <- mdma_clean(raw, level = input$clean_level %||% "basic")
            writeLines(cleaned, out_paths[[i]])
            TRUE
          },
          error = function(e) {
            log_lines <<- c(
              log_lines,
              paste("[FAIL]", basename(paths[[i]]), ":", conditionMessage(e))
            )
            FALSE
          }
        )

        if (isTRUE(ok)) {
          results[[i]] <- out_paths[[i]]
          log_lines <- c(log_lines, paste("[OK]  ", basename(paths[[i]])))
        }
        clean_log_rv(paste(log_lines, collapse = "\n"))
      }
    })

    results <- results[nzchar(results)]
    clean_results(results)
    clean_running(FALSE)

    if (length(results)) {
      shiny::updateSelectInput(
        session,
        "clean_view_file",
        choices = stats::setNames(results, basename(results)),
        selected = results[[1L]]
      )
    }
  })

  output$clean_log <- shiny::renderText(clean_log_rv() %||% "")

  output$clean_status_ui <- shiny::renderUI({
    if (clean_running()) {
      shiny::div(class = "alert alert-info py-2 mb-2", "Running…")
    } else if (length(clean_results()) > 0L) {
      n <- length(clean_results())
      shiny::div(
        class = "alert alert-success py-2 mb-2",
        shiny::icon("check"),
        " ",
        n,
        " file(s) cleaned."
      )
    } else if (!is.null(clean_log_rv())) {
      shiny::div(
        class = "alert alert-danger py-2 mb-2",
        "Cleaning failed — see log."
      )
    }
  })

  output$clean_viewer_ui <- shiny::renderUI({
    results <- clean_results()
    if (!length(results)) {
      return(NULL)
    }
    shiny::tagList(
      shiny::tags$hr(),
      shiny::selectInput(
        "clean_view_file",
        "Inspect file:",
        choices = stats::setNames(results, basename(results)),
        selected = results[[1L]]
      ),
      shiny::div(
        class = "border rounded p-2",
        style = paste0(
          "max-height:450px; overflow-y:auto; white-space:pre-wrap;",
          " font-family:monospace; font-size:0.85rem;"
        ),
        shiny::textOutput("clean_file_content", inline = TRUE)
      )
    )
  })

  output$clean_file_content <- shiny::renderText({
    shiny::req(input$clean_view_file)
    f <- input$clean_view_file
    if (!file.exists(f)) {
      return("(file not found)")
    }
    paste(readLines(f, warn = FALSE), collapse = "\n")
  })
}

mdma_shiny_roots <- function() {
  home <- path.expand("~")
  roots <- c(Home = home)
  if (.Platform$OS.type == "windows") {
    drives <- paste0(LETTERS, ":/")
    avail <- drives[file.exists(drives)]
    names(avail) <- sub("/$", "", avail)
    roots <- c(roots, avail)
  }
  roots
}
