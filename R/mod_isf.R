#' In-Source Fragmentation Module UI
#' @param id module id
isf_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns('file_feat'), 'Choose Feature File (CSV / Excel)',
                  accept = c('.csv', '.xlsx', '.xls',
                             'text/csv',
                             'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                             'application/vnd.ms-excel')),
        uiOutput(ns("file_feat_status")),
        fileInput(ns('file_frag'), 'Choose Fragment File (MGF/MSP)', accept = c('.mgf', '.msp')),
        uiOutput(ns("file_frag_status")),

        # --- Shown on "Plots & Table" — static subtab (ISF Processing + Column Mapping) ---
        # Static UI (no renderUI) so Process/Plot input IDs are bound immediately.
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Plots_Table'", ns("result_sections")),
          tabsetPanel(type = "pills", id = ns("plot_tab_subtabs"),
            tabPanel("ISF Processing",
              div(class = "mzx-section mzx-isf",
                tags$h4("ISF processing"),
                numericInput(ns("insource_ppm"), "m/z tolerance (ppm)", value = 15, min = 1),
                numericInput(ns("rt_diff"), "RT tolerance", value = 0.05, min = 0, step = 0.01),
                actionButton(ns('go_process'), 'Process', width = "100%", class = "btn-info"),
                fluidRow(style = "margin-top:6px;",
                  column(6, actionButton(ns("cancel_isf"), "Cancel",
                                         class = "btn-outline-warning btn-sm",
                                         width = "100%")),
                  column(6, tags$button("Reload session",
                                         id = ns("reload_session"),
                                         class = "btn btn-outline-danger btn-sm",
                                         style = "width:100%;",
                                         onclick = "window.location.reload(true); return false;",
                                         title = "Force-reload the app if a run is stuck. Discards all state."))
                ),
                tags$small(style = "color:#777;",
                           "Cancel halts before the next processing phase (the heavy fragment match cannot be interrupted mid-run). Use Reload if truly stuck."),
                tags$br(), tags$br(),
                checkboxInput(ns('ins'), 'Show intensity as size', FALSE),
                checkboxInput(ns("show_leg"), "Show plot legends", TRUE),
                uiOutput(ns("slide_ui")),
                actionButton(ns('go_plot'), 'Plot', width = "100%", class = "btn-info")
              )
            ),
            tabPanel("Column Mapping",
              div(class = "mzx-section mzx-cmap",
                uiOutput(ns("col_map_ui"))
              )
            )
          )
        ),

        # --- Shown on "Sample comparison" ---
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Sample_comparison'", ns("result_sections")),
          div(class = "mzx-section mzx-sample",
            tags$h4("Sample comparison"),
            checkboxInput(ns("enable_sample_comp"), "Enable sample comparison", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("enable_sample_comp")),
              tags$small(style = "color:#555;",
                         "Assign one column per sample. The plot shows those values for the currently selected feature(s)."),
              tags$br(), tags$br(),
              numericInput(ns("n_samples"), "Number of samples", value = 2, min = 2, max = 20, step = 1),
              uiOutput(ns("sample_slots_ui")),
              selectInput(ns("sample_plot_type"), "Plot type",
                          choices = c("Grouped bars" = "bar",
                                      "Lines + markers" = "line"),
                          selected = "bar"),
              actionButton(ns("go_sample_comp"), "Plot sample comparison",
                           width = "100%", class = "btn-secondary")
            )
          )
        ),

        width = 2
      ),
      mainPanel(
        uiOutput(ns("isf_summary_stat")),
        tabsetPanel(
          id = ns("result_sections"),
          type = "tabs",
          tabPanel(
            "Plots & Table", value = "Plots_Table",
            div(class = "mzx-pane mzx-isf",
              tags$h4("Interactive Plots for ISF exploration"),
              uiOutput(ns("plot_controls")),
              fluidRow(
                column(width = 6, plotly::plotlyOutput(ns("ISFPlot1"))),
                column(width = 6, plotly::plotlyOutput(ns("ISFPlot2")))
              ),
              div(style = "margin-top:10px;",
                DT::DTOutput(ns("table_selected")),
                fluidRow(style = "margin-top:6px;",
                  column(6, checkboxInput(ns("cleanup_isf_export"),
                                          "Exclude ISF features from export (keep non-ISF only)",
                                          value = TRUE)),
                  column(3, downloadButton(ns("export_btn"), "Export Selected")),
                  column(3, actionButton(ns("clear_sel"), "Reset selection", class = "btn-outline-secondary"))
                )
              )
            ),
            div(class = "mzx-pane mzx-isf",
              checkboxInput(ns("net_only_matches"),
                            "Show only ISF features and their precursors (hide unmatched)",
                            value = FALSE),
              plotly::plotlyOutput(ns("isf_network_plot"), height = "500px")
            ),
            div(class = "mzx-pane mzx-export",
              tags$h4("Processed feature table (export)"),
              tags$small(style = "color:#555;",
                         "Full processed feature list — not affected by point selection. Double-click the ",
                         tags$b("ISF_annotation"),
                         " cell to manually re-tag a feature (e.g. mark a false-positive as 'not ISF'). Edits are kept for the export below. Use the radio filter to include/exclude ISF-tagged features before exporting."),
              tags$br(), tags$br(),
              radioButtons(ns("table_filter"), "Filter Table Results",
                           choices = c("only include ISF", "exclude ISF, keep non-ISF", "include both"),
                           selected = "exclude ISF, keep non-ISF", inline = TRUE),
              DT::DTOutput(ns("ISF_table")),
              tags$br(),
              downloadButton(ns("export_all"), "Export Processed Table (Full CSV Columns)")
            )
          ),
          tabPanel(
            "Sample comparison", value = "Sample_comparison",
            div(class = "mzx-pane mzx-sample",
              tags$h4("Sample comparison"),
              tags$small("Showing selected features across the chosen sample columns. Make a selection in any plot above to filter."),
              fluidRow(
                column(3, uiOutput(ns("sample_x_var_ui")))
              ),
              plotly::plotlyOutput(ns("sample_comp_plot"), height = "450px")
            )
          )
        ),
        width = 10
      )
    )
  )
}

#' In-Source Fragmentation Module Server
#' @param id module id
isf_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    vals <- reactiveValues(d = NULL, df_annotated = NULL, df_filtered = NULL, orig_df = NULL)
    # Snapshot of sizing settings captured at the moment of the last Plot
    # click; renderers read this instead of input$ins directly so the
    # checkbox only takes effect on the next Plot press.
    isf_size_state <- reactiveVal(list(enabled = FALSE, col = NULL))

    # --- Filter helper: keep rows where the chosen column has data
    # (not NA, and > 0 if numeric). "(none)" or empty -> unchanged.
    .apply_col_filter <- function(df, col) {
      if (is.null(df) || !nrow(df)) return(df)
      if (is.null(col) || !length(col) || col %in% c("", "(none)") ||
          !col %in% names(df)) return(df)
      v <- df[[col]]
      keep <- !is.na(v)
      if (is.numeric(v)) keep <- keep & v > 0
      df[keep, , drop = FALSE]
    }

    # --- File upload status badges ---
    # Small green "read OK" / red "read failed" indicator that appears
    # under each fileInput as soon as the file has been fully uploaded
    # AND parsed. This gives clear visual confirmation that heavy work
    # can start, rather than relying on the (subtle) native progress bar.
    .status_ok <- function(msg) {
      div(style = "margin: -8px 0 10px 2px; color: #2e7d32; font-size: 12px;",
          tags$span(style = "font-weight: 600;", "\u2714 "), msg)
    }
    .status_err <- function(msg) {
      div(style = "margin: -8px 0 10px 2px; color: #c62828; font-size: 12px;",
          tags$span(style = "font-weight: 600;", "\u2716 "), msg)
    }
    .status_pending <- function(msg) {
      div(style = "margin: -8px 0 10px 2px; color: #6a6a6a; font-size: 12px;",
          tags$span(style = "font-weight: 600;", "\u23F3 "), msg)
    }

    output$file_feat_status <- renderUI({
      if (is.null(input$file_feat)) return(NULL)
      df <- tryCatch(feat_raw(), error = function(e) e)
      if (inherits(df, "error"))
        return(.status_err(paste("Feature file error:", conditionMessage(df))))
      if (is.null(df))
        return(.status_pending("Reading feature file..."))
      .status_ok(sprintf("Feature file loaded: %s  (%s rows, %s columns)",
                         input$file_feat$name,
                         format(nrow(df), big.mark = " "),
                         format(ncol(df), big.mark = " ")))
    })

    # For the fragment file we only know it's "ready" once we've
    # actually parsed it (heavy). Rather than parse eagerly on every
    # upload (which would defeat the point of the Process button), we
    # just show file size + name once the upload itself has completed.
    output$file_frag_status <- renderUI({
      f <- input$file_frag
      if (is.null(f)) return(NULL)
      sz <- file.info(f$datapath)$size
      sz_txt <- if (is.na(sz)) "unknown size"
                else if (sz > 1024^3) sprintf("%.2f GB", sz / 1024^3)
                else if (sz > 1024^2) sprintf("%.1f MB", sz / 1024^2)
                else if (sz > 1024)    sprintf("%.0f KB", sz / 1024)
                else sprintf("%d B", as.integer(sz))
      .status_ok(sprintf("Fragment file uploaded: %s  (%s). Click Process to parse.",
                         f$name, sz_txt))
    })
    
    # --- Column mapping ---
    # Read the feature CSV once when uploaded so we can offer the user a
    # column-mapping UI (their column names -> internal targets mz/rt/etc.).
    feat_raw <- reactive({
      req(input$file_feat)
      df <- read_feature_file(input$file_feat$datapath,
                              orig_name = input$file_feat$name)
      validate(need(!is.null(df) && ncol(df) > 0,
                    "Could not read the feature file. Please provide a CSV or TSV text file."))
      df
    })

    output$col_map_ui <- renderUI({
      if (is.null(feat_raw())) {
        return(tags$div(style = "color:#777;",
                        tags$em("Upload a feature file first, then map its columns here.")))
      }
      nms <- names(feat_raw())
      defaults <- guess_default_mapping(nms)
      choices <- c("(none)", nms)
      tagList(
        tags$h4("Column mapping"),
        tags$small(style = "color:#555;",
                   "Map your file's columns to the required fields."),
        tags$br(), tags$br(),
        selectInput(ns("map_mz"),        "m/z column",       choices = choices,
                    selected = if (!is.na(defaults$mz))        defaults$mz        else "(none)"),
        selectInput(ns("map_rt"),        "RT column",        choices = choices,
                    selected = if (!is.na(defaults$rt))        defaults$rt        else "(none)"),
        selectInput(ns("map_intensity"), "Intensity column", choices = choices,
                    selected = if (!is.na(defaults$intensity)) defaults$intensity else "(none)"),
        selectInput(ns("map_id"),        "ID column (optional)",  choices = choices,
                    selected = if (!is.na(defaults$id))        defaults$id        else "(none)"),
        selectInput(ns("map_ccs"),       "CCS / ion mobility column (optional)", choices = choices,
                    selected = if (!is.na(defaults$ccs))       defaults$ccs       else "(none)")
      )
    })

    # (plot_tab_subtabs_ui is now static UI in isf_ui — no renderUI here.)

    # --- Cancel flag for long ISF processing runs.
    # NOTE: R runs synchronously on a single thread, so this flag can
    # only be checked BETWEEN phases (before we hand off to the heavy
    # C-level data.table join). It cannot interrupt an already-running
    # annotate_isf() call. If a run is truly stuck, use the "Reload
    # session" escape hatch below.
    cancel_isf <- reactiveVal(FALSE)
    observeEvent(input$cancel_isf, {
      cancel_isf(TRUE)
      showNotification("Cancel requested — will stop before the next phase.",
                       type = "warning", duration = 3, id = ns("nn_cancel_isf"))
    })
    observeEvent(input$reload_session, { session$reload() })

    # --- Processing ---
    observeEvent(input$go_process, {
      if (is.null(input$file_feat)) {
        showNotification("Please upload a feature file first.",
                         type = "warning", duration = 4); return()
      }
      if (is.null(input$file_frag)) {
        showNotification("Please upload a fragment file first.",
                         type = "warning", duration = 4); return()
      }
      # If the column-mapping renderUI has not yet round-tripped to the
      # client (fast first click), `input$map_*` will be NULL. Fall back
      # to the auto-guessed mapping so that a single click is enough.
      df_names <- names(feat_raw())
      guess    <- guess_default_mapping(df_names)
      pick     <- function(inp, guess_val) {
        v <- if (!is.null(inp) && inp != "(none)") inp else guess_val
        if (is.null(v) || is.na(v)) NA_character_ else v
      }
      map_mz  <- pick(input$map_mz,        guess$mz)
      map_rt  <- pick(input$map_rt,        guess$rt)
      map_int <- pick(input$map_intensity, guess$intensity)
      map_id  <- pick(input$map_id,        guess$id)
      map_ccs <- pick(input$map_ccs,       guess$ccs)
      miss <- c()
      if (is.na(map_mz))  miss <- c(miss, "m/z")
      if (is.na(map_rt))  miss <- c(miss, "RT")
      if (is.na(map_int)) miss <- c(miss, "intensity")
      if (length(miss)) {
        showNotification(paste0("Could not auto-detect these required columns: ",
                                paste(miss, collapse = ", "),
                                ". Please set them in the Column mapping panel."),
                         type = "warning", duration = 6); return()
      }
      cancel_isf(FALSE)  # reset at the start of each run

      withProgress(message = "Annotating ISF...", value = 0, {
        incProgress(0.05, detail = "Loading feature file...")
        if (isTRUE(cancel_isf())) {
          showNotification("Run cancelled.", type = "warning",
                           duration = 3, id = ns("nn_cancel_isf"))
          return()
        }
        df_raw <- data.table::copy(feat_raw())
        vals$orig_df <- data.table::copy(df_raw)

        mapping <- list(
          mz        = map_mz,
          rt        = map_rt,
          intensity = map_int,
          id        = map_id,
          ccs       = map_ccs
        )
        ms1 <- apply_column_mapping(df_raw, mapping)

        # Validate required columns
        required <- c("mz", "rt", "intensity")
        missing <- setdiff(required, names(ms1))
        if (length(missing) > 0) {
          showNotification(paste("Missing required columns:", paste(missing, collapse = ", ")),
                           type = "error")
          return()
        }
        for (col in intersect(c("mz", "rt", "intensity", "ccs"), names(ms1))) {
          suppressWarnings(ms1[, (col) := as.numeric(get(col))])
        }
        # Ensure ID exists internally
        if (!"id" %in% names(ms1)) ms1$id <- seq_len(nrow(ms1))

        incProgress(0.05, detail = sprintf(
          "Loaded %s features. Reading fragment file...",
          format(nrow(ms1), big.mark = " ")))
        if (isTRUE(cancel_isf())) {
          showNotification("Run cancelled before fragment parsing.",
                           type = "warning", duration = 3,
                           id = ns("nn_cancel_isf"))
          return()
        }
        t_frag <- Sys.time()
        spectra <- read_fragment_file(input$file_frag$datapath)
        dt_frag <- round(as.numeric(difftime(Sys.time(), t_frag, units = "secs")), 1)
        n_peaks_total <- sum(vapply(spectra,
                                    function(s) length(s$peaks_mz), integer(1)))

        incProgress(0.15, detail = sprintf(
          "Parsed %s spectra (%s peaks) in %ss. Starting match...",
          format(length(spectra), big.mark = " "),
          format(n_peaks_total,   big.mark = " "),
          dt_frag))
        if (isTRUE(cancel_isf())) {
          showNotification("Run cancelled before ISF matching (heavy step).",
                           type = "warning", duration = 3,
                           id = ns("nn_cancel_isf"))
          return()
        }

        # Match-phase progress: annotate_isf drives us via a callback so
        # the user sees per-chunk detail like "Chunk 4/12 done - 12,431
        # partial matches".
        # Total budget for the match phase inside the progress bar:
        match_budget <- 0.65
        match_state <- list(spent = 0)
        prog_cb <- function(phase, i, n, detail) {
          # Distribute the budget roughly across the phases:
          # build_peaks(5%) + match(85%) + filter(5%) + aggregate(5%)
          step <- switch(phase,
                         build_peaks = match_budget * 0.05 / max(1, n),
                         match       = match_budget * 0.85 / max(1, n),
                         filter      = match_budget * 0.05,
                         aggregate   = match_budget * 0.05,
                         0)
          match_state$spent <<- match_state$spent + step
          incProgress(step, detail = detail)
        }

        t_match <- Sys.time()
        annotated <- tryCatch(
          annotate_isf(ms1, spectra, input$insource_ppm, input$rt_diff,
                       progress = prog_cb),
          error = function(e) {
            showNotification(
              sprintf("ISF matching failed: %s", conditionMessage(e)),
              type = "error", duration = 10)
            NULL
          })
        if (is.null(annotated)) return()
        dt_match <- round(as.numeric(difftime(Sys.time(), t_match, units = "secs")), 1)
        vals$df_annotated <- annotated

        # Ensure the bar tops out at 100% regardless of chunk-rounding.
        remaining <- max(0, 1 - 0.05 - 0.05 - 0.15 - match_state$spent - 0.05)
        n_hit <- sum(annotated$ISF_annotation != "not ISF")
        incProgress(remaining + 0.05,
                    detail = sprintf("Done in %ss. %s ISF fragments found.",
                                     dt_match, format(n_hit, big.mark = " ")))
        showNotification(sprintf(
          "Finished processing in %ss. Found %s ISF fragments out of %s features. Click 'Plot' to render the interactive plots.",
          dt_match,
          format(n_hit,           big.mark = " "),
          format(nrow(annotated), big.mark = " ")),
                         type = "message", duration = 8)
      })
    })
    
    output$isf_summary_stat <- renderUI({
      req(vals$df_annotated)
      is_isf <- vals$df_annotated$ISF_annotation != "not ISF"
      n_frag <- sum(is_isf)
      n_total <- nrow(vals$df_annotated)
      wellPanel(
        p(icon("info-circle"), strong("ISF Summary:"),
          sprintf("Identified %d potential fragments out of %d total features (%.1f%%).",
                  n_frag, n_total, 100 * n_frag / max(1, n_total)))
      )
    })
    
    # --- UI & Plotting ---
    output$slide_ui <- renderUI({
      req(vals$df_annotated); df <- vals$df_annotated
      tagList(
        sliderInput(ns("slide_int"), "Intensity", min = min(df$intensity, na.rm=T), max = max(df$intensity, na.rm=T), value = range(df$intensity, na.rm=T)),
        sliderInput(ns("slide_mz"), "m/z", min = min(df$mz, na.rm=T), max = max(df$mz, na.rm=T), value = range(df$mz, na.rm=T)),
        sliderInput(ns("slide_rt"), "RT", min = min(df$rt, na.rm=T), max = max(df$rt, na.rm=T), value = range(df$rt, na.rm=T))
      )
    })
    
    output$plot_controls <- renderUI({
      req(vals$df_annotated); df <- vals$df_annotated
      nms <- names(df)
      # All columns are valid axis choices. Long categorical labels (e.g.
      # `ISF_annotation`) are truncated at draw time in make_isf_scatter().
      pick <- function(preferred, i) {
        if (preferred %in% nms) preferred
        else nms[min(i, length(nms))]
      }
      tagList(
        fluidRow(
          column(6, selectInput(ns('filter_col'),
                                'Filter by sample (select sample column). Filter applies to network plot as well.',
                                choices = c("(none)", nms), selected = "(none)"))
        ),
        fluidRow(
          column(3, selectInput(ns('x1'), 'X Plot 1', choices = nms, selected = pick("mz", 1))),
          column(3, selectInput(ns('y1'), 'Y Plot 1', choices = nms, selected = pick("ISF_annotation", 2))),
          column(3, selectInput(ns('x2'), 'X Plot 2', choices = nms, selected = pick("rt", 2))),
          column(3, selectInput(ns('y2'), 'Y Plot 2', choices = nms, selected = pick("mz", 1)))
        )
      )
    })
    
    observeEvent(input$go_plot, {
      # Friendly checks (avoid silent `req()` halts).
      if (is.null(vals$df_annotated)) {
        showNotification("Please click Process first to annotate ISF, then Plot.",
                         type = "warning", duration = 5); return()
      }
      withProgress(message = "Applying filters & rebuilding plots...", value = 0, {
        n_start <- nrow(vals$df_annotated)
        incProgress(0.2, detail = sprintf("Filtering %s features by sliders...",
                                          format(n_start, big.mark = " ")))
        df <- vals$df_annotated

        .filt <- function(x, slider_val) {
          if (is.null(slider_val) || length(slider_val) < 2 ||
              any(!is.finite(slider_val))) return(rep(TRUE, length(x)))
          x >= slider_val[1] & x <= slider_val[2]
        }
        keep <- .filt(df$intensity, input$slide_int) &
                .filt(df$mz,        input$slide_mz)  &
                .filt(df$rt,        input$slide_rt)
        df <- df[keep, ]

        if (!nrow(df)) {
          showNotification(
            "Filter produced 0 features. Widen the intensity / m/z / RT sliders.",
            type = "warning", duration = 6, id = ns("nn_plot_empty"))
          return()
        }

        incProgress(0.3, detail = sprintf("Kept %s of %s features. Preparing keys...",
                                          format(nrow(df), big.mark = " "),
                                          format(n_start, big.mark = " ")))
        df$.key <- sprintf("id%05d", seq_len(nrow(df)))

        incProgress(0.3, detail = "Building SharedData for linked selection...")
        vals$df_filtered <- df
        vals$d <- crosstalk::SharedData$new(df, key = ~.key, group = ns("isf"))
        # Snapshot the "Show intensity as size" setting at Plot-time so
        # the checkbox only takes effect on the next Plot press (not live).
        isf_size_state(list(
          enabled = isTRUE(input$ins),
          col     = if ("intensity" %in% names(df)) "intensity" else NULL
        ))
        incProgress(0.2, detail = sprintf("%d features ready", nrow(df)))
      })
      showNotification(sprintf("Plotted %d features.", nrow(vals$df_filtered)),
                       type = "message", duration = 3, id = ns("nn_plot_isf"))
    })
    
    output$ISFPlot1 <- plotly::renderPlotly({
      req(vals$df_filtered)
      nms <- names(vals$df_filtered)
      xv  <- if (!is.null(input$x1) && input$x1 %in% nms) input$x1
             else if ("mz" %in% nms) "mz" else nms[1]
      yv  <- if (!is.null(input$y1) && input$y1 %in% nms) input$y1
             else if ("ISF_annotation" %in% nms) "ISF_annotation" else nms[min(2, length(nms))]
      withProgress(message = "Rendering scatter plot 1...", value = 0, {
        incProgress(0.3, detail = sprintf("Filtering %s features by sample column...",
                                          format(nrow(vals$df_filtered), big.mark = " ")))
        dff <- .apply_col_filter(vals$df_filtered, input$filter_col)
        if (!nrow(dff)) return(plotly::plot_ly() %>%
                                 plotly::layout(title = "No features with data in chosen column"))
        incProgress(0.3, detail = sprintf("Building SharedData for %s points (%s vs %s)...",
                                          format(nrow(dff), big.mark = " "), xv, yv))
        d_f <- crosstalk::SharedData$new(dff, key = ~.key, group = ns("isf"))
        incProgress(0.4, detail = "Drawing traces...")
        make_isf_scatter(d_f, xv, yv, ns("p1"),
                         intensity_enabled = isTRUE(isf_size_state()$enabled),
                         intensity_col = {
                           c <- isf_size_state()$col
                           if (!is.null(c) && c %in% names(dff)) c else NULL
                         })
      })
    })
    output$ISFPlot2 <- plotly::renderPlotly({
      req(vals$df_filtered)
      nms <- names(vals$df_filtered)
      xv  <- if (!is.null(input$x2) && input$x2 %in% nms) input$x2
             else if ("rt" %in% nms) "rt" else nms[1]
      yv  <- if (!is.null(input$y2) && input$y2 %in% nms) input$y2
             else if ("mz" %in% nms) "mz" else nms[min(2, length(nms))]
      withProgress(message = "Rendering scatter plot 2...", value = 0, {
        incProgress(0.3, detail = sprintf("Filtering %s features by sample column...",
                                          format(nrow(vals$df_filtered), big.mark = " ")))
        dff <- .apply_col_filter(vals$df_filtered, input$filter_col)
        if (!nrow(dff)) return(plotly::plot_ly() %>%
                                 plotly::layout(title = "No features with data in chosen column"))
        incProgress(0.3, detail = sprintf("Building SharedData for %s points (%s vs %s)...",
                                          format(nrow(dff), big.mark = " "), xv, yv))
        d_f <- crosstalk::SharedData$new(dff, key = ~.key, group = ns("isf"))
        incProgress(0.4, detail = "Drawing traces...")
        make_isf_scatter(d_f, xv, yv, ns("p2"),
                         intensity_enabled = isTRUE(isf_size_state()$enabled),
                         intensity_col = {
                           c <- isf_size_state()$col
                           if (!is.null(c) && c %in% names(dff)) c else NULL
                         })
      })
    })
    
    # --- Selection tracking (backed by reactiveVal so it can be reset) ---
    sel_keys_rv  <- reactiveVal(character(0))
    sel_gen_isf  <- reactiveVal(0L)

    # Independent per-plot observers so a fresh selection on one plot fully
    # REPLACES the previous selection instead of unioning with stale keys
    # from the other (event_data is sticky per source).
    .apply_sel_isf <- function(ed) {
      sel_keys_rv(if (!is.null(ed) && length(ed$key)) unique(as.character(ed$key))
                  else character(0))
    }
    observeEvent(plotly::event_data("plotly_selected", source = ns("p1")),
                 .apply_sel_isf(plotly::event_data("plotly_selected", source = ns("p1"))),
                 ignoreNULL = FALSE, ignoreInit = TRUE)
    observeEvent(plotly::event_data("plotly_selected", source = ns("p2")),
                 .apply_sel_isf(plotly::event_data("plotly_selected", source = ns("p2"))),
                 ignoreNULL = FALSE, ignoreInit = TRUE)
    observeEvent(plotly::event_data("plotly_selected", source = ns("isfnet")),
                 .apply_sel_isf(plotly::event_data("plotly_selected", source = ns("isfnet"))),
                 ignoreNULL = FALSE, ignoreInit = TRUE)

    observeEvent(input$clear_sel, {
      sel_keys_rv(character(0))
      if (!is.null(vals$df_filtered)) {
        vals$d <- crosstalk::SharedData$new(as.data.frame(vals$df_filtered),
                                            key = ~.key, group = ns("isf"))
      }
      sel_gen_isf(sel_gen_isf() + 1L)
      showNotification("Selection reset.", type = "message",
                       duration = 2, id = ns("nn_reset_isf"))
    })

    selected_keys <- reactive({
      sel_gen_isf()
      sel_keys_rv()
    })
    
    # Helper: put id first, ISF-annotation second, then dppm/drt.
    .reorder_isf_cols <- function(df) {
      lead <- intersect(
        c("id", "ISF_annotation", "ISF_dppm", "ISF_drt"),
        names(df))
      df[, c(lead, setdiff(names(df), lead)), drop = FALSE]
    }

    output$table_selected <- DT::renderDT({
      req(vals$df_filtered); keys <- selected_keys()
      df <- if(length(keys)) vals$df_filtered[vals$df_filtered$.key %in% keys, ] else vals$df_filtered
      df <- .reorder_isf_cols(as.data.frame(df))
      DT::datatable(df, options = list(scrollX = TRUE), rownames = FALSE)
    })
    
    # Track the currently displayed ISF_table view so cell edits can be
    # mapped back to the master annotated data frame by row key.
    isf_table_view <- reactiveVal(NULL)

    output$ISF_table <- DT::renderDT({
      req(vals$df_filtered); df <- vals$df_filtered
      withProgress(message = "Building ISF table...", value = 0, {
        incProgress(0.3, detail = sprintf("Filtering %s features (%s)...",
                                          format(nrow(df), big.mark = " "),
                                          input$table_filter))
        res <- switch(input$table_filter,
                      "only include ISF" = df[df$ISF_annotation != "not ISF", ],
                      "exclude ISF, keep non-ISF" = df[df$ISF_annotation == "not ISF", ],
                      df)

        incProgress(0.3, detail = sprintf("Preparing %s rows for display...",
                                          format(nrow(res), big.mark = " ")))
        res <- .reorder_isf_cols(as.data.frame(res))
        isf_table_view(res)

        incProgress(0.4, detail = "Rendering DataTable...")
        col_names <- names(res)
        isf_idx <- match("ISF_annotation", col_names) - 1L
        disable_idx <- setdiff(seq_along(col_names) - 1L, isf_idx)

      dt <- DT::datatable(res,
                          rownames = FALSE,
                          # Performance for large datasets: paginate on the
                          # server, defer row rendering until visible, and
                          # show a processing indicator while DT works.
                          options = list(scrollX = TRUE,
                                         pageLength = 25,
                                         deferRender = TRUE,
                                         processing  = TRUE,
                                         lengthMenu  = c(10, 25, 50, 100, 250)),
                          editable = list(target = "cell",
                                          disable = list(columns = disable_idx)))
        dt
      })  # close withProgress
    })

    # Persist manual re-tagging of ISF_annotation back to the master data.
    observeEvent(input$ISF_table_cell_edit, {
      info <- input$ISF_table_cell_edit
      view <- isf_table_view()
      req(view, vals$df_annotated)
      col_name <- names(view)[info$col + 1L]
      if (!identical(col_name, "ISF_annotation")) return()

      # info$row is 1-based within the currently displayed (filtered) view.
      key_col <- if (".key" %in% names(view)) ".key" else "id"
      row_key <- view[[key_col]][info$row]
      new_val <- as.character(info$value)

      # Update master annotated data
      m_idx <- which(vals$df_annotated[[key_col]] == row_key)
      if (length(m_idx)) vals$df_annotated$ISF_annotation[m_idx] <- new_val

      # Also update the filtered/display data (drives plots + selected table)
      f_idx <- which(vals$df_filtered[[key_col]] == row_key)
      if (length(f_idx)) vals$df_filtered$ISF_annotation[f_idx] <- new_val
    })
    
    # --- Clean Export Logic ---
    output$export_all <- downloadHandler(
      filename = function() { paste("mzXplorer_ISF_Cleaned_", Sys.Date(), ".csv", sep="") },
      content = function(file) {
        req(vals$df_annotated, vals$orig_df)
        res_df <- vals$df_annotated

        # Merge annotation back to original data to preserve all columns
        out <- vals$orig_df
        out$ISF_annotation <- res_df$ISF_annotation

        data.table::fwrite(out, file)
      }
    )

    output$export_btn <- downloadHandler(
      filename = "mzXplorer_Selected_Export.csv",
      content = function(file) {
        req(vals$df_filtered)
        keys <- selected_keys()
        out <- if(length(keys)) vals$df_filtered[vals$df_filtered$.key %in% keys, ] else vals$df_filtered
        if (isTRUE(input$cleanup_isf_export) && "ISF_annotation" %in% names(out)) {
          out <- out[out$ISF_annotation == "not ISF", ]
        }
        data.table::fwrite(out, file)
      }
    )
    
    # Network Plot (linked via crosstalk to the two scatter plots)
    output$isf_network_plot <- plotly::renderPlotly({
      sel_gen_isf()  # re-render on selection reset
      req(vals$df_filtered); df <- vals$df_filtered
      withProgress(message = "Rendering ISF network...", value = 0, {
        incProgress(0.1, detail = sprintf("Applying sample filter to %s features...",
                                          format(nrow(df), big.mark = " ")))
        df <- .apply_col_filter(df, input$filter_col)
        if (!nrow(df)) return(plotly::plot_ly() %>%
                                plotly::layout(title = "No features with data in chosen column"))
        not_isf <- df[df$ISF_annotation == "not ISF", ]
        is_isf  <- df[df$ISF_annotation != "not ISF", ]

        if (nrow(is_isf) == 0) return(NULL)

        # If the user only wants matched pairs, restrict `not_isf` (precursors)
        # to those actually referenced by an ISF feature's annotation.
        if (isTRUE(input$net_only_matches)) {
          annot_ids_all <- sub("^ISF of ID\\s*", "", is_isf$ISF_annotation)
          referenced <- suppressWarnings(as.numeric(unlist(
            strsplit(annot_ids_all, "\\s*,\\s*"), use.names = FALSE)))
          referenced <- unique(referenced[!is.na(referenced)])
          not_isf <- not_isf[not_isf$id %in% referenced, , drop = FALSE]
          incProgress(0, detail = sprintf("Only-matches: kept %s precursors referenced by ISF features.",
                                          format(nrow(not_isf), big.mark = " ")))
        }

        incProgress(0.3, detail = sprintf(
          "Building edges for %s ISF features (against %s non-ISF)...",
          format(nrow(is_isf),  big.mark = " "),
          format(nrow(not_isf), big.mark = " ")))
        annot_ids <- sub("^ISF of ID\\s*", "", is_isf$ISF_annotation)
        id_lists  <- strsplit(annot_ids, "\\s*,\\s*")
        lens      <- lengths(id_lists)

        from_row  <- rep(seq_len(nrow(is_isf)), lens)
        to_id_raw <- suppressWarnings(as.numeric(unlist(id_lists, use.names = FALSE)))
        to_idx    <- match(to_id_raw, df$id)
        ok        <- !is.na(to_idx) & !is.na(to_id_raw)

        if (any(ok)) {
          from_row <- from_row[ok]; to_idx <- to_idx[ok]
          max_edges <- 50000L
          n_edges   <- length(from_row)
          edge_note <- NULL
          if (n_edges > max_edges) {
            samp <- sample.int(n_edges, max_edges)
            from_row <- from_row[samp]; to_idx <- to_idx[samp]
            edge_note <- sprintf(" (showing %s of %s edges)",
                                 format(max_edges, big.mark = ","),
                                 format(n_edges,   big.mark = ","))
          }
          incProgress(0.3, detail = sprintf(
            "Assembling %s edge segments%s...",
            format(length(from_row), big.mark = " "),
            if (is.null(edge_note)) "" else edge_note))
          lx <- as.vector(rbind(is_isf$rt[from_row], df$rt[to_idx], NA_real_))
          ly <- as.vector(rbind(is_isf$mz[from_row], df$mz[to_idx], NA_real_))
        } else {
          lx <- numeric(0); ly <- numeric(0)
          edge_note <- NULL
        }

        incProgress(0.3, detail = "Drawing traces...")
        sd_not <- crosstalk::SharedData$new(not_isf, key = ~.key, group = ns("isf"))
        sd_isf <- crosstalk::SharedData$new(is_isf,  key = ~.key, group = ns("isf"))

      plot_ly(source = ns("isfnet")) %>%
        add_trace(type = "scattergl", mode = "lines", x = lx, y = ly,
                  line = list(color = "rgba(120,120,120,0.4)", width = 1),
                  showlegend = FALSE, hoverinfo = "skip") %>%
        add_markers(data = sd_not, x = ~rt, y = ~mz, name = "Precursor / clean",
                    type = "scattergl",
                    marker = list(color = "rgba(0, 114, 178, 0.7)", size = 8,
                                  line = list(color = "rgba(0,0,0,0.35)", width = 0.4)),
                    hovertemplate = "id=%{customdata}<br>rt=%{x:.3f}<br>m/z=%{y:.4f}<extra></extra>",
                    customdata = ~id) %>%
        add_markers(data = sd_isf, x = ~rt, y = ~mz, name = "Fragment / ISF",
                    type = "scattergl",
                    marker = list(color = "#D55E00", size = 8, opacity = 0.9,
                                  line = list(color = "#7a2f00", width = 0.5)),
                    hovertemplate = "id=%{customdata}<br>rt=%{x:.3f}<br>m/z=%{y:.4f}<extra></extra>",
                    customdata = ~id) %>%
        mzx_style(xtitle = "Retention time",
                  ytitle = "m/z",
                  title  = paste0("ISF network (m/z vs. RT)",
                                  if (!is.null(edge_note)) edge_note else "")) %>%
        plotly::highlight(on = "plotly_selected", off = "plotly_deselect",
                          color = I(mzx_highlight_color), opacityDim = 0.25)
      })  # close withProgress
    })

    # --- Sample comparison ---
    output$sample_slots_ui <- renderUI({
      req(feat_raw(), input$n_samples)
      nms <- names(feat_raw())
      mapped <- unique(c(input$map_mz, input$map_rt, input$map_intensity,
                         input$map_id, input$map_ccs))
      mapped <- mapped[nzchar(mapped) & mapped != "(none)"]
      numeric_cols <- nms[vapply(nms,
                                 function(cn) is.numeric(feat_raw()[[cn]]),
                                 logical(1))]
      candidates <- setdiff(numeric_cols, mapped)
      if (!length(candidates)) candidates <- setdiff(nms, mapped)
      n <- max(2, as.integer(input$n_samples))
      tagList(lapply(seq_len(n), function(i) {
        selectInput(ns(paste0("sample_col_", i)),
                    label = paste0("Sample ", i, " column"),
                    choices = c("(none)", nms),
                    selected = if (i <= length(candidates)) candidates[i] else "(none)")
      }))
    })

    sample_map <- reactive({
      req(input$n_samples)
      n <- max(2, as.integer(input$n_samples))
      vals2 <- vapply(seq_len(n), function(i) {
        v <- input[[paste0("sample_col_", i)]]
        if (is.null(v)) NA_character_ else as.character(v)
      }, character(1))
      names(vals2) <- paste0("Sample ", seq_len(n))
      vals2[!is.na(vals2) & vals2 != "(none)" & nzchar(vals2)]
    })

    output$sample_ref_ui <- renderUI({
      req(length(sample_map()) > 0)
      selectInput(ns("sample_ref"), "Reference sample",
                  choices = names(sample_map()),
                  selected = names(sample_map())[1])
    })

    sample_settings <- eventReactive(input$go_sample_comp, {
      req(sample_map())
      showNotification("Building sample comparison...", type = "message",
                       duration = 2, id = ns("nn_sample_isf"))
      list(sm = sample_map(),
           plot_type = input$sample_plot_type,
           ref = input$sample_ref)
    }, ignoreNULL = FALSE)

    output$sample_x_var_ui <- renderUI({
      # Prefer the plotted / annotated data if available (has mapped
      # columns like mz/rt/id), but fall back to feat_raw() so the
      # selector is populated as soon as a file is uploaded — the user
      # doesn't have to run Process/Plot first.
      src <- if (!is.null(vals$df_filtered)) vals$df_filtered
             else if (!is.null(vals$df_annotated)) vals$df_annotated
             else feat_raw()
      req(src)
      cols <- setdiff(names(src), ".key")
      sel  <- if ("mz" %in% cols) "mz"
              else if ("id" %in% cols) "id"
              else cols[1]
      selectInput(ns("sample_x_var"), "X variable",
                  choices = cols, selected = sel)
    })

    output$sample_comp_plot <- plotly::renderPlotly({
      req(input$enable_sample_comp, vals$df_filtered)
      s <- sample_settings()
      req(s, s$sm)
      keys <- selected_keys()
      df <- if (length(keys)) vals$df_filtered[vals$df_filtered$.key %in% keys, , drop = FALSE] else vals$df_filtered
      if (!nrow(df)) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No features to compare"))
      }
      withProgress(message = "Rendering sample comparison...", value = 0.3, {
        fig <- make_sample_comp_plot(df,
                                     sample_map = s$sm,
                                     plot_type  = s$plot_type,
                                     x_var      = input$sample_x_var,
                                     ref_label  = s$ref)
        incProgress(0.7)
        fig
      })
    })
  })
}

make_isf_scatter <- function(sd, xv, yv, sid,
                             intensity_enabled = FALSE,
                             intensity_col = NULL) {
  # Build truncated tick labels for any categorical (character/factor) axis
  # so that long values like "ISF of ID 130, 1450, 2201, ..." don't blow out
  # the plot area. Full text remains visible in the hover.
  df <- tryCatch(sd$data(withSelection = FALSE), error = function(e) NULL)

  # ---- Intensity-as-size (robust) --------------------------------------
  # Same rules as MD: guard non-numeric / all-NA / zero-range and fall
  # back to a constant size. sizemode="diameter" + sizemin so tiny
  # points remain visible in scattergl.
  size_const  <- 7
  point_sizes <- if (!is.null(df)) rep(size_const, nrow(df)) else size_const
  use_size    <- FALSE
  if (isTRUE(intensity_enabled) && !is.null(df) &&
      !is.null(intensity_col) && length(intensity_col) == 1 &&
      intensity_col %in% names(df)) {
    v <- suppressWarnings(as.numeric(df[[intensity_col]]))
    finite_v <- v[is.finite(v)]
    if (length(finite_v) && diff(range(finite_v)) > 0) {
      rng <- range(finite_v)
      sz  <- 5 + 17 * (v - rng[1]) / diff(rng)
      med <- stats::median(sz, na.rm = TRUE)
      sz[!is.finite(sz)] <- med
      point_sizes <- sz
      use_size    <- TRUE
    }
  }

  .cat_axis <- function(colname, maxlen = 28, max_ticks = 20) {
    if (is.null(df) || !colname %in% names(df)) return(NULL)
    v <- df[[colname]]
    if (!(is.character(v) || is.factor(v))) return(NULL)
    uv <- unique(as.character(v))
    # Sub-sample tick labels when there are too many categories, so the
    # axis stays readable at full zoom. Plotly still lays out ALL
    # categories along the axis; we just decide which ones get a label.
    if (length(uv) > max_ticks) {
      pick_idx <- unique(round(seq(1, length(uv), length.out = max_ticks)))
      tv <- uv[pick_idx]
    } else {
      tv <- uv
    }
    tt <- ifelse(nchar(tv) > maxlen,
                 paste0(substr(tv, 1, maxlen - 1), "\u2026"),
                 tv)
    list(type = "category",
         tickmode = "array",
         tickvals = tv,
         ticktext = tt,
         automargin = TRUE)
  }
  xaxis_ov <- .cat_axis(xv)
  yaxis_ov <- .cat_axis(yv)

  # Numeric axes format with .4f; categorical axes keep raw string via %{x}/%{y}.
  is_num <- function(cn) !is.null(df) && cn %in% names(df) && is.numeric(df[[cn]])
  xfmt <- if (is_num(xv)) "%{x:.4f}" else "%{x}"
  yfmt <- if (is_num(yv)) "%{y:.4f}" else "%{y}"

  marker_spec <- list(size = point_sizes,
                      color = "rgba(0, 114, 178, 0.7)",  # Okabe-Ito blue
                      line = list(color = "rgba(0,0,0,0.35)", width = 0.4))
  if (use_size) {
    marker_spec$sizemode <- "diameter"
    marker_spec$sizemin  <- 3
  }
  p <- plot_ly(sd, x = as.formula(paste0("~", xv)), y = as.formula(paste0("~", yv)),
          key = ~.key, source = sid, type = "scattergl", mode = "markers",
          marker = marker_spec,
          hovertemplate = paste0(xv, "=", xfmt, "<br>", yv, "=", yfmt, "<extra></extra>")) %>%
    mzx_style(xtitle = xv, ytitle = yv, legend = FALSE) %>%
    plotly::highlight(on = "plotly_selected", off = "plotly_deselect",
                      color = I(mzx_highlight_color), opacityDim = 0.25)
  if (!is.null(xaxis_ov)) p <- p %>% plotly::layout(xaxis = xaxis_ov)
  if (!is.null(yaxis_ov)) p <- p %>% plotly::layout(yaxis = yaxis_ov)
  p
}
