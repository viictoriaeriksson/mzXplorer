#' In-Source Fragmentation Module UI
#' @param id module id
isf_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns('file_feat'), 'Choose Feature File (.csv / .xlsx / .xls)',
                  accept = c('.csv', '.xlsx', '.xls',
                             'text/csv',
                             'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                             'application/vnd.ms-excel')),
        uiOutput(ns("file_feat_status")),
        fileInput(ns('file_frag'), 'Choose Fragment File (.mgf/ .msp)', accept = c('.mgf', '.msp')),
        uiOutput(ns("file_frag_status")),

        # --- Shown on "Plots & Table" — 
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Plots_Table'", ns("result_sections")),
          tabsetPanel(type = "pills", id = ns("plot_tab_subtabs"),
            tabPanel("ISF Processing",
              div(class = "mzx-section mzx-isf",
                tags$h4("ISF processing"),
                numericInput(ns("insource_da"), "m/z tolerance (Da)", value = 0.01, min = 0, step = 0.001),
                numericInput(ns("rt_diff"), "RT tolerance", value = 0.05, min = 0, step = 0.01),
                selectInput(ns("nl_presets"), "Neutral losses",
                            multiple = TRUE,
                            choices = c(
                              "H2O (18.0106)"  = "H2O:18.0106",
                              "NH3 (17.0265)"  = "NH3:17.0265",
                              "CO2 (43.9898)"  = "CO2:43.9898",
                              "SO3 (79.9568)"  = "SO3:79.9568",
                              "HCl (35.9767)"  = "HCl:35.9767",
                              "HF (20.0062)"   = "HF:20.0062"
                            ),
                            selected = NULL),
                textInput(ns("nl_custom"), "Custom neutral losses",
                          value = "",
                          placeholder = "Label,mass; Label,mass"),
                tags$small(style = "color:#777;",
                           "Format: name,mass in Da separated by ';'. Example: CH4,16.0313; CO,27.9949"),
                tags$br(), tags$br(),
                selectInput(ns("adduct_presets"), "Adducts",
                            multiple = TRUE,
                            choices = c(
                              "[M+H]+ (1.007276)"    = "[M+H]+:1.007276",
                              "[M+Na]+ (22.989218)"  = "[M+Na]+:22.989218",
                              "[M+K]+ (38.963158)"   = "[M+K]+:38.963158",
                              "[M+NH4]+ (18.033823)" = "[M+NH4]+:18.033823",
                              "[M-H]- (-1.007276)"   = "[M-H]-:-1.007276",
                              "[M+HCOO]- (44.998201)"= "[M+HCOO]-:44.998201",
                              "[M+Cl]- (34.969402)"  = "[M+Cl]-:34.969402"
                            ),
                            selected = NULL),
                textInput(ns("adduct_custom"), "Custom adducts",
                          value = "",
                          placeholder = "Label,mass; Label,mass"),
                tags$small(style = "color:#777;",
                           "Format: name,mass in Da separated by ';'. Adduct pairs are matched on mass differences."),
                tags$br(), tags$br(),
                actionButton(ns('go_process'), 'Process', width = "100%", class = "btn-info"),
                fluidRow(style = "margin-top:6px;",
                  column(12, actionButton(ns("cancel_isf"), "Cancel",
                                         class = "btn-outline-warning btn-sm",
                                         width = "100%"))
                ),
                tags$small(style = "color:#777;",
                           "Cancel halts before the next processing phase (the heavy fragment match cannot be interrupted mid-run)."),
                tags$br(), tags$br(),
                checkboxInput(ns('ins'), 'Show intensity as size', FALSE),
                checkboxInput(ns('show_leg'), 'Show plot legends', TRUE),
                checkboxInput(ns("show_filters"), "Filters", FALSE),
                conditionalPanel(
                  condition = sprintf("input['%s'] == true", ns("show_filters")),
                  uiOutput(ns("slide_ui"))
                ),
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
            div(class = "mzx-pane mzx-isf",
              tags$h4("ISF intensity ratio vs. retention time"),
              tags$small(style = "color:#555;",
                         "One point per (fragment, precursor) pair. Fragment ISF matches in orange, neutral-loss ISF matches in green. Adducts are excluded."),
              plotly::plotlyOutput(ns("isf_ratio_plot"), height = "400px")
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
    isf_size_state <- reactiveVal(list(enabled = FALSE, col = NULL))

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


    # --- Cancel flag for long ISF processing runs.
    cancel_isf <- reactiveVal(FALSE)
    observeEvent(input$cancel_isf, {
      cancel_isf(TRUE)
      showNotification("Cancel requested — will stop before the next phase.",
                       type = "warning", duration = 3, id = ns("nn_cancel_isf"))
    })


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
      cancel_isf(FALSE) 

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
        match_budget <- 0.65
        match_state <- list(spent = 0)
        prog_cb <- function(phase, i, n, detail) {
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
          annotate_isf(ms1, spectra, input$insource_da, input$rt_diff,
                       progress = prog_cb),
          error = function(e) {
            showNotification(
              sprintf("ISF matching failed: %s", conditionMessage(e)),
              type = "error", duration = 10)
            NULL
          })
        if (is.null(annotated)) return()
        dt_match <- round(as.numeric(difftime(Sys.time(), t_match, units = "secs")), 1)

        # Post-annotate: neutral-loss + adduct matching.
        nl_list <- parse_mass_input(input$nl_presets %||% character(0),
                                    input$nl_custom %||% "")
        ad_list <- parse_mass_input(input$adduct_presets %||% character(0),
                                    input$adduct_custom %||% "")
        annotated <- tryCatch(
          annotate_extra(annotated, nl_list, ad_list,
                         da_tol = input$insource_da,
                         rt_tol = input$rt_diff),
          error = function(e) {
            showNotification(
              sprintf("Neutral-loss / adduct annotation failed: %s",
                      conditionMessage(e)),
              type = "error", duration = 10)
            annotated
          })
        vals$df_annotated <- annotated

        # Ensure the bar tops out at 100%.
        remaining <- max(0, 1 - 0.05 - 0.05 - 0.15 - match_state$spent - 0.05)
        is_frag_hit <- annotated$ISF_annotation != "not ISF"
        is_nl_hit   <- if (!is.null(annotated$ISF_NL_annotation)) annotated$ISF_NL_annotation != "not ISF" else rep(FALSE, nrow(annotated))
        n_hit <- sum(is_frag_hit | is_nl_hit)
        incProgress(remaining + 0.05,
                    detail = sprintf("Done in %ss. %s potential ISF found.",
                                     dt_match, format(n_hit, big.mark = " ")))
        showNotification(sprintf(
          "Finished processing in %ss. Found %s potential ISF (fragment or neutral-loss) out of %s features. Click 'Plot' to render the interactive plots.",
          dt_match,
          format(n_hit,           big.mark = " "),
          format(nrow(annotated), big.mark = " ")),
                         type = "message", duration = 8)
      })
    })
    
    output$isf_summary_stat <- renderUI({
      req(vals$df_annotated)
      is_frag <- vals$df_annotated$ISF_annotation != "not ISF"
      is_nl   <- if (!is.null(vals$df_annotated$ISF_NL_annotation)) vals$df_annotated$ISF_NL_annotation != "not ISF" else rep(FALSE, nrow(vals$df_annotated))
      is_any  <- is_frag | is_nl
      n_any   <- sum(is_any)
      n_frag  <- sum(is_frag)
      n_nl    <- sum(is_nl)
      n_total <- nrow(vals$df_annotated)
      wellPanel(
        p(icon("info-circle"), strong("ISF Summary:"),
          sprintf("Identified %d potential ISF out of %d total features (%.1f%%) - %d via ISF_annotation, %d via ISF_NL_annotation.",
                  n_any, n_total, 100 * n_any / max(1, n_total), n_frag, n_nl))
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
      pick <- function(preferred, i) {
        if (preferred %in% nms) preferred
        else nms[min(i, length(nms))]
      }
      tagList(
        fluidRow(
          column(6, selectInput(ns('filter_col'),
                                'Filter by sample (select sample column). Filter applies to network plot as well.',
                                choices = c("(none)", nms), selected = "(none)")),
          column(6, tags$div(style = "margin-top:28px;",
                             checkboxInput(ns("table_only_selected"),
                                           "Show only selected points in table",
                                           value = FALSE)))
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
                         },
                         show_legend = isTRUE(input$show_leg),
                         sel_keys = sel_keys_rv())
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
                         },
                         show_legend = isTRUE(input$show_leg),
                         sel_keys = sel_keys_rv())
      })
    })
    
    sel_keys_rv  <- reactiveVal(character(0))
    sel_gen_isf  <- reactiveVal(0L)
    sel_source   <- reactiveVal("init")

    .apply_sel_isf <- function(ed) {
      # Ignore points from traces without a crosstalk key (overlay traces
      # and edge lines), which arrive as NA and would corrupt the selection.
      k <- if (is.null(ed)) character(0) else as.character(ed$key)
      k <- k[!is.na(k) & nzchar(k) & k != "NA"]
      sel_source("plot")
      sel_keys_rv(unique(k))
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

    observeEvent(plotly::event_data("plotly_selected", source = ns("isfratio")),
                 .apply_sel_isf(plotly::event_data("plotly_selected", source = ns("isfratio"))),
                 ignoreNULL = FALSE, ignoreInit = TRUE)

    observeEvent(input$clear_sel, {
      sel_keys_rv(character(0))
      sel_source("plot")
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
    
    .reorder_isf_cols <- function(df) {
      lead <- intersect(
        c("id", "ISF_annotation", "ISF_dDa", "ISF_drt", "ISF_ratio",
          "ISF_NL_annotation", "ISF_NL_type", "ISF_NL_dDa", "ISF_NL_drt",
          "ISF_NL_ratio", "Adduct_annotation", "Adduct_type"),
        names(df))
      df[, c(lead, setdiff(names(df), lead)), drop = FALSE]
    }

    table_selected_view <- reactiveVal(NULL)

    plot_sel_snapshot <- reactiveVal(list(keys = character(0), src = "init"))
    observeEvent(sel_keys_rv(), {
      if (!identical(sel_source(), "table")) {
        plot_sel_snapshot(list(keys = sel_keys_rv(), src = sel_source()))
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    output$table_selected <- DT::renderDT({
      req(vals$df_filtered)
      snap <- plot_sel_snapshot()
      keys <- snap$keys
      src  <- snap$src
      df_all <- .reorder_isf_cols(as.data.frame(vals$df_filtered))
      key_col <- if (".key" %in% names(df_all)) ".key" else "id"

      if (isTRUE(input$table_only_selected) &&
          length(keys) && identical(src, "plot")) {
        df <- df_all[as.character(df_all[[key_col]]) %in% keys, , drop = FALSE]
        pre_sel <- seq_len(nrow(df))
      } else {
        df <- df_all
        pre_sel <- if (length(keys))
          which(as.character(df[[key_col]]) %in% keys)
        else
          integer(0)
      }
      table_selected_view(df)

      DT::datatable(df,
                    rownames = FALSE,
                    selection = list(mode = "multiple",
                                     selected = pre_sel),
                    options = list(scrollX = TRUE,
                                   pageLength = 25,
                                   deferRender = TRUE,
                                   processing  = TRUE,
                                   lengthMenu  = c(10, 25, 50, 100, 250)))
    })

    observeEvent(input$table_selected_rows_selected, {
      view <- table_selected_view()
      if (is.null(view) || !nrow(view)) return()
      rows <- input$table_selected_rows_selected
      key_col <- if (".key" %in% names(view)) ".key" else "id"
      new_keys <- if (length(rows)) unique(as.character(view[[key_col]][rows]))
                  else character(0)
      if (!setequal(new_keys, sel_keys_rv())) {
        sel_source("table")
        sel_keys_rv(new_keys)
        sel_gen_isf(sel_gen_isf() + 1L)
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    isf_table_view <- reactiveVal(NULL)

    output$ISF_table <- DT::renderDT({
      req(vals$df_filtered); df <- vals$df_filtered
      withProgress(message = "Building ISF table...", value = 0, {
        incProgress(0.3, detail = sprintf("Filtering %s features (%s)...",
                                          format(nrow(df), big.mark = " "),
                                          input$table_filter))
        is_frag_row <- if ("ISF_annotation"    %in% names(df)) df$ISF_annotation    != "not ISF" else rep(FALSE, nrow(df))
        is_nl_row   <- if ("ISF_NL_annotation" %in% names(df)) df$ISF_NL_annotation != "not ISF" else rep(FALSE, nrow(df))
        is_any_isf  <- is_frag_row | is_nl_row
        res <- switch(input$table_filter,
                      "only include ISF" = df[is_any_isf, ],
                      "exclude ISF, keep non-ISF" = df[!is_any_isf, ],
                      df)

        incProgress(0.3, detail = sprintf("Preparing %s rows for display...",
                                          format(nrow(res), big.mark = " ")))
        res <- .reorder_isf_cols(as.data.frame(res))
        isf_table_view(res)

        incProgress(0.4, detail = "Rendering DataTable...")
        col_names <- names(res)
        editable_cols <- intersect(c("ISF_annotation", "ISF_NL_annotation"), col_names)
        editable_idx  <- match(editable_cols, col_names) - 1L
        disable_idx   <- setdiff(seq_along(col_names) - 1L, editable_idx)

      dt <- DT::datatable(res,
                          rownames = FALSE,
                          # Performance for large datasets
                          options = list(scrollX = TRUE,
                                         pageLength = 25,
                                         deferRender = TRUE,
                                         processing  = TRUE,
                                         lengthMenu  = c(10, 25, 50, 100, 250)),
                          editable = list(target = "cell",
                                          disable = list(columns = disable_idx)))
        dt
      }) 
    })

    # Manual re-tagging of ISF_annotation / ISF_NL_annotation
    observeEvent(input$ISF_table_cell_edit, {
      info <- input$ISF_table_cell_edit
      view <- isf_table_view()
      req(view, vals$df_annotated)
      col_name <- names(view)[info$col + 1L]
      if (!col_name %in% c("ISF_annotation", "ISF_NL_annotation")) return()

      row_id  <- if ("id"   %in% names(view)) view[["id"]][info$row]   else NA
      row_key <- if (".key" %in% names(view)) view[[".key"]][info$row] else NA
      new_val <- as.character(info$value)

      if (!is.na(row_id) && "id" %in% names(vals$df_annotated)) {
        m_idx <- which(vals$df_annotated[["id"]] == row_id)
        if (length(m_idx)) {
          d <- vals$df_annotated
          d[[col_name]][m_idx] <- new_val
          vals$df_annotated <- d
        }
      }

      if (!is.null(vals$df_filtered)) {
        f_key_col <- if (".key" %in% names(vals$df_filtered)) ".key" else "id"
        f_lookup  <- if (f_key_col == ".key") row_key else row_id
        if (!is.na(f_lookup)) {
          f_idx <- which(vals$df_filtered[[f_key_col]] == f_lookup)
          if (length(f_idx)) {
            d <- vals$df_filtered
            d[[col_name]][f_idx] <- new_val
            vals$df_filtered <- d
          }
        }
      }
    })
    
    # --- Export Logic ---
    output$export_all <- downloadHandler(
      filename = function() { paste("mzXplorer_ISF_ProcessedTable_", Sys.Date(), ".csv", sep="") },
      content = function(file) {
        req(vals$df_annotated, vals$orig_df)
        res_df <- vals$df_annotated

        out <- vals$orig_df
        for (cn in c("ISF_annotation", "ISF_dDa", "ISF_drt", "ISF_ratio",
                     "ISF_NL_annotation", "ISF_NL_type", "ISF_NL_dDa",
                     "ISF_NL_drt", "ISF_NL_ratio",
                     "Adduct_annotation", "Adduct_type")) {
          if (!is.null(res_df[[cn]])) out[[cn]] <- res_df[[cn]]
        }

        tf <- isolate(input$table_filter)
        if (!is.null(tf)) {
          is_frag_row <- if ("ISF_annotation"    %in% names(out)) out$ISF_annotation    != "not ISF" else rep(FALSE, nrow(out))
          is_nl_row   <- if ("ISF_NL_annotation" %in% names(out)) out$ISF_NL_annotation != "not ISF" else rep(FALSE, nrow(out))
          is_any_isf  <- is_frag_row | is_nl_row
          out <- switch(tf,
                        "only include ISF"          = out[is_any_isf, ],
                        "exclude ISF, keep non-ISF" = out[!is_any_isf, ],
                        out)
        }

        data.table::fwrite(out, file)
      }
    )

    output$export_btn <- downloadHandler(
      filename = "mzXplorer_Selected_Export.csv",
      content = function(file) {
        req(vals$df_filtered)
        keys <- selected_keys()
        out <- if(length(keys)) vals$df_filtered[vals$df_filtered$.key %in% keys, ] else vals$df_filtered
        if (isTRUE(input$cleanup_isf_export)) {
          is_frag_out <- if ("ISF_annotation"    %in% names(out)) out$ISF_annotation    != "not ISF" else FALSE
          is_nl_out   <- if ("ISF_NL_annotation" %in% names(out)) out$ISF_NL_annotation != "not ISF" else FALSE
          out <- out[!(is_frag_out | is_nl_out), , drop = FALSE]
        }
        data.table::fwrite(out, file)
      }
    )
    
    output$isf_network_plot <- plotly::renderPlotly({
      sel_gen_isf()  # re-render on selection reset
      req(vals$df_filtered); df <- vals$df_filtered
      sel_keys <- sel_keys_rv()
      has_sel  <- length(sel_keys) > 0L
      dim_op   <- if (has_sel) 0.3 else 1
      withProgress(message = "Rendering ISF network...", value = 0, {
        incProgress(0.1, detail = sprintf("Applying sample filter to %s features...",
                                          format(nrow(df), big.mark = " ")))
        df <- .apply_col_filter(df, input$filter_col)
        if (!nrow(df)) return(plotly::plot_ly() %>%
                                plotly::layout(title = "No features with data in chosen column"))
        # Ensure columns exist even if legacy annotation.
        if (is.null(df$ISF_NL_annotation)) df$ISF_NL_annotation <- "not ISF"
        if (is.null(df$Adduct_annotation)) df$Adduct_annotation <- "no adduct"

        has_frag  <- df$ISF_annotation    != "not ISF"
        has_nl    <- df$ISF_NL_annotation != "not ISF"
        is_both   <- has_frag & has_nl
        is_frag   <- has_frag & (!has_nl)
        is_nl     <- (!has_frag) & has_nl
        is_clean  <- (!has_frag) & (!has_nl)

        not_isf  <- df[is_clean, ]    # precursor / clean pool (incl. adducts)
        is_isf   <- df[is_frag,  ]    # fragment-only ISF
        nl_isf   <- df[is_nl,    ]    # NL-only ISF
        both_df  <- df[is_both,  ]    # both fragment + NL

        if (nrow(is_isf) + nrow(nl_isf) + nrow(both_df) == 0)
          return(NULL)

        if (isTRUE(input$net_only_matches)) {
          annot_ids_all <- c(
            sub("^ISF of ID\\s*", "", is_isf$ISF_annotation),
            sub("^ISF of ID\\s*", "", nl_isf$ISF_NL_annotation),
            sub("^ISF of ID\\s*", "", both_df$ISF_annotation),
            sub("^ISF of ID\\s*", "", both_df$ISF_NL_annotation)
          )
          referenced <- trimws(unlist(strsplit(annot_ids_all, "\\s*,\\s*"),
                                      use.names = FALSE))
          referenced <- unique(referenced[nzchar(referenced) &
                                          referenced != "Unknown" &
                                          referenced != "NA"])
          not_isf <- not_isf[as.character(not_isf$id) %in% referenced, , drop = FALSE]
          incProgress(0, detail = sprintf("Only-matches: kept %s precursors referenced by ISF features.",
                                          format(nrow(not_isf), big.mark = " ")))
        }

        incProgress(0.3, detail = sprintf(
          "Building edges for %s fragment + %s NL + %s both ISF features...",
          format(nrow(is_isf),  big.mark = " "),
          format(nrow(nl_isf),  big.mark = " "),
          format(nrow(both_df), big.mark = " ")))

        .build_edges <- function(src, annot_col) {
          if (!nrow(src)) return(list(lx = numeric(0), ly = numeric(0)))
          annot_ids <- sub("^ISF of ID\\s*", "", src[[annot_col]])
          id_lists  <- strsplit(annot_ids, "\\s*,\\s*")
          lens      <- lengths(id_lists)
          from_row  <- rep(seq_len(nrow(src)), lens)
          to_id_raw <- suppressWarnings(as.numeric(unlist(id_lists, use.names = FALSE)))
          to_idx    <- match(to_id_raw, df$id)
          ok        <- !is.na(to_idx) & !is.na(to_id_raw)
          if (!any(ok)) return(list(lx = numeric(0), ly = numeric(0)))
          from_row <- from_row[ok]; to_idx <- to_idx[ok]
          list(
            lx = as.vector(rbind(src$rt[from_row], df$rt[to_idx], NA_real_)),
            ly = as.vector(rbind(src$mz[from_row], df$mz[to_idx], NA_real_))
          )
        }

        e_frag      <- .build_edges(is_isf,  "ISF_annotation")
        e_nl        <- .build_edges(nl_isf,  "ISF_NL_annotation")
        e_both_frag <- .build_edges(both_df, "ISF_annotation")
        e_both_nl   <- .build_edges(both_df, "ISF_NL_annotation")

        # Cap total edges for perf.
        max_edges <- 50000L
        n_edges_pairs <- (length(e_frag$lx) + length(e_nl$lx)) %/% 3L
        edge_note <- NULL
        if (n_edges_pairs > max_edges) {
          edge_note <- sprintf(" (capped edges at %s)",
                               format(max_edges, big.mark = ","))
        }

        incProgress(0.3, detail = "Drawing traces...")

        not_sel  <- not_isf$.key %in% sel_keys
        isf_sel  <- is_isf$.key  %in% sel_keys
        nl_sel   <- nl_isf$.key  %in% sel_keys
        both_sel <- both_df$.key %in% sel_keys
        un_not  <- not_isf[!not_sel , , drop = FALSE]
        un_isf  <- is_isf [!isf_sel , , drop = FALSE]
        un_nl   <- nl_isf [!nl_sel  , , drop = FALSE]
        un_both <- both_df[!both_sel, , drop = FALSE]
        se_df   <- rbind(not_isf [ not_sel , , drop = FALSE],
                         is_isf  [ isf_sel , , drop = FALSE],
                         nl_isf  [ nl_sel  , , drop = FALSE],
                         both_df [ both_sel, , drop = FALSE])

        sd_not  <- crosstalk::SharedData$new(un_not , key = ~.key, group = ns("isf"))
        sd_isf  <- crosstalk::SharedData$new(un_isf , key = ~.key, group = ns("isf"))
        sd_nl   <- crosstalk::SharedData$new(un_nl  , key = ~.key, group = ns("isf"))
        sd_both <- crosstalk::SharedData$new(un_both, key = ~.key, group = ns("isf"))

      p <- plot_ly(source = ns("isfnet")) %>%
        add_trace(type = "scattergl", mode = "lines",
                  x = e_frag$lx, y = e_frag$ly,
                  line = list(color = "rgba(213,94,0,0.35)", width = 1),
                  showlegend = FALSE, hoverinfo = "skip") %>%
        add_trace(type = "scattergl", mode = "lines",
                  x = e_nl$lx, y = e_nl$ly,
                  line = list(color = "rgba(0,158,115,0.35)", width = 1),
                  showlegend = FALSE, hoverinfo = "skip") %>%
        add_trace(type = "scattergl", mode = "lines",
                  x = c(e_both_frag$lx, e_both_nl$lx),
                  y = c(e_both_frag$ly, e_both_nl$ly),
                  line = list(color = "rgba(120,94,240,0.45)", width = 1),
                  showlegend = FALSE, hoverinfo = "skip") %>%
        add_markers(data = sd_not, x = ~rt, y = ~mz, name = "Precursor / clean",
                    type = "scattergl",
                    marker = list(color = "rgba(0, 114, 178, 0.75)", size = 8,
                                  opacity = dim_op,
                                  line = list(color = "rgba(0,0,0,0.35)", width = 0.4)),
                    hovertemplate = "id=%{customdata}<br>rt=%{x:.3f}<br>m/z=%{y:.4f}<extra></extra>",
                    customdata = ~id) %>%
        add_markers(data = sd_isf, x = ~rt, y = ~mz, name = "ISF (fragment)",
                    type = "scattergl",
                    marker = list(color = "rgba(213, 94, 0, 0.9)", size = 8,
                                  opacity = dim_op,
                                  line = list(color = "#7a2f00", width = 0.5)),
                    hovertemplate = "id=%{customdata}<br>rt=%{x:.3f}<br>m/z=%{y:.4f}<extra></extra>",
                    customdata = ~id) %>%
        add_markers(data = sd_nl, x = ~rt, y = ~mz, name = "ISF (neutral loss)",
                    type = "scattergl",
                    marker = list(color = "rgba(0, 158, 115, 0.9)", size = 8,
                                  opacity = dim_op,
                                  line = list(color = "#005c40", width = 0.5)),
                    hovertemplate = "id=%{customdata}<br>rt=%{x:.3f}<br>m/z=%{y:.4f}<extra></extra>",
                    customdata = ~id) %>%
        add_markers(data = sd_both, x = ~rt, y = ~mz,
                    name = "ISF (fragment + NL)",
                    type = "scattergl",
                    marker = list(color = "rgba(120, 94, 240, 0.95)", size = 9,
                                  opacity = dim_op,
                                  line = list(color = "#3d2f80", width = 0.6)),
                    hovertemplate = "id=%{customdata}<br>rt=%{x:.3f}<br>m/z=%{y:.4f}<extra></extra>",
                    customdata = ~id) %>%
        add_markers(x = se_df$rt, y = se_df$mz,
                    name = "Selected",
                    type = "scatter", mode = "markers",
                    marker = list(color = mzx_highlight_color, size = 8, opacity = 0.95,
                                  line = list(color = "#8a7a00", width = 1.2)),
                    hovertemplate = "id=%{customdata}<br>rt=%{x:.3f}<br>m/z=%{y:.4f}<extra></extra>",
                    customdata = if (nrow(se_df)) se_df$id else integer(0),
                    inherit = FALSE, showlegend = has_sel) %>%
        mzx_style(xtitle = "Retention time",
                  ytitle = "m/z",
                  title  = paste0("ISF network (m/z vs. RT)",
                                  if (!is.null(edge_note)) edge_note else "")) %>%
        plotly::layout(dragmode = "select")
      p
      }) 
    })

    # --- ISF intensity ratio vs. RT plot ---
    # One point per (fragment, precursor) pair. 
    output$isf_ratio_plot <- plotly::renderPlotly({
      req(vals$df_filtered); df <- vals$df_filtered
      df <- .apply_col_filter(df, input$filter_col)
      if (!nrow(df)) return(NULL)
      if (is.null(df$ISF_ratio))         df$ISF_ratio         <- NA_character_
      if (is.null(df$ISF_NL_annotation)) df$ISF_NL_annotation <- "not ISF"
      if (is.null(df$ISF_NL_ratio))      df$ISF_NL_ratio      <- NA_character_
      if (is.null(df$Adduct_annotation)) df$Adduct_annotation <- "no adduct"

      .explode <- function(rows, annot_col, ratio_col, kind) {
        if (!length(rows)) return(NULL)
        ids_raw <- sub("^ISF of ID\\s*", "", df[[annot_col]][rows])
        id_lists <- strsplit(ids_raw, "\\s*,\\s*")
        r_lists  <- strsplit(df[[ratio_col]][rows] %||% "", "\\s*,\\s*")
        data.table::rbindlist(lapply(seq_along(rows), function(k) {
          ids <- suppressWarnings(as.numeric(id_lists[[k]]))
          rs  <- suppressWarnings(as.numeric(r_lists[[k]]))
          if (!length(ids)) return(NULL)
          if (length(rs) != length(ids)) rs <- rep(NA_real_, length(ids))
          data.table::data.table(
            .key      = df$.key[rows[k]],
            frag_id   = df$id[rows[k]],
            frag_rt   = df$rt[rows[k]],
            precursor = ids,
            ratio     = rs,
            kind      = kind
          )
        }), use.names = TRUE, fill = TRUE)
      }

      not_ad    <- df$Adduct_annotation == "no adduct"
      has_frag  <- df$ISF_annotation    != "not ISF"
      has_nl    <- df$ISF_NL_annotation != "not ISF"
      both_rows <- which(not_ad & has_frag &  has_nl)
      frag_rows <- which(not_ad & has_frag & !has_nl)
      nl_rows   <- which(not_ad & has_nl   & !has_frag)

      pts <- data.table::rbindlist(list(
        .explode(frag_rows, "ISF_annotation",    "ISF_ratio",    "Fragment"),
        .explode(nl_rows,   "ISF_NL_annotation", "ISF_NL_ratio", "Neutral loss"),
        .explode(both_rows, "ISF_annotation",    "ISF_ratio",    "Both (fragment)"),
        .explode(both_rows, "ISF_NL_annotation", "ISF_NL_ratio", "Both (NL)")
      ), use.names = TRUE, fill = TRUE)

      if (!nrow(pts)) {
        return(plot_ly() %>%
          mzx_style(xtitle = "Retention time",
                    ytitle = "ISF / precursor intensity ratio",
                    title  = "No ISF pairs to display"))
      }

      pts <- pts[is.finite(ratio)]
      pts$kind <- factor(pts$kind,
                         levels = c("Fragment", "Neutral loss",
                                    "Both (fragment)", "Both (NL)"))

      col_map <- c(
        "Fragment"               = "#D55E00",
        "Neutral loss"           = "#009E73",
        "Both (fragment)"        = "#785EF0",
        "Both (NL)"              = "#785EF0"
      )
      legend_name <- c(
        "Fragment"        = "Fragment",
        "Neutral loss"    = "Neutral loss",
        "Both (fragment)" = "Fragment + Neutral loss",
        "Both (NL)"       = "Fragment + Neutral loss"
      )
      shown_both <- FALSE

      sel_keys <- sel_keys_rv()
      has_sel  <- length(sel_keys) > 0
      base_op  <- if (has_sel) 0.25 else 1

      p <- plot_ly(source = ns("isfratio"))
      for (k in levels(pts$kind)) {
        sub <- pts[kind == k]
        if (!nrow(sub)) next
        is_both <- k %in% c("Both (fragment)", "Both (NL)")
        show_leg <- if (is_both) { s <- !shown_both; shown_both <- TRUE; s } else TRUE
        p <- p %>% add_trace(
          data = as.data.frame(sub),
          x = ~frag_rt, y = ~ratio,
          key = ~.key,                
          type = "scattergl", mode = "markers",
          name = legend_name[[k]],
          legendgroup = legend_name[[k]],
          showlegend = show_leg,
          opacity = base_op,
          marker = list(color = col_map[[k]], size = 8,
                        line = list(color = "rgba(0,0,0,0.4)", width = 0.5)),
          text = ~paste0("frag id=", frag_id,
                         "<br>precursor id=", precursor,
                         "<br>ratio=", sprintf("%.2f", ratio),
                         "<br>type=", k),
          hoverinfo = "text"
        )
      }
      if (has_sel) {
        sel_pts <- pts[as.character(.key) %in% sel_keys]
        if (nrow(sel_pts)) {
          p <- p %>% add_trace(
            data = as.data.frame(sel_pts),
            x = ~frag_rt, y = ~ratio,
            key = ~.key,
            type = "scattergl", mode = "markers",
            name = "Selected",
            marker = list(color = mzx_highlight_color, size = 8,
                          line = list(color = "rgba(0,0,0,0.7)", width = 1)),
            text = ~paste0("frag id=", frag_id,
                           "<br>precursor id=", precursor,
                           "<br>ratio=", sprintf("%.2f", ratio),
                           "<br>type=", kind),
            hoverinfo = "text"
          )
        }
      }
      p %>% mzx_style(xtitle = "Retention time",
                      ytitle = "ISF / precursor intensity ratio",
                      title  = "ISF intensity ratio vs. RT") %>%
        plotly::layout(dragmode = "select")
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


    sample_settings <- eventReactive(input$go_sample_comp, {
      req(sample_map())
      showNotification("Building sample comparison...", type = "message",
                       duration = 2, id = ns("nn_sample_isf"))
      list(sm = sample_map(),
           plot_type = input$sample_plot_type,
           ref = input$sample_ref)
    }, ignoreNULL = FALSE)

    output$sample_x_var_ui <- renderUI({
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
      req(vals$df_filtered)
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

    observeEvent(list(input$file_feat, input$file_frag), {
      vals$d <- NULL; vals$df_annotated <- NULL
      vals$df_filtered <- NULL; vals$orig_df <- NULL

      sel_source("init")
      sel_keys_rv(character(0))
      sel_gen_isf(sel_gen_isf() + 1L)
      plot_sel_snapshot(list(keys = character(0), src = "init"))
      table_selected_view(NULL)
    }, ignoreInit = TRUE)

  })
}

make_isf_scatter <- function(sd, xv, yv, sid,
                             intensity_enabled = FALSE,
                             intensity_col = NULL,
                             show_legend = TRUE,
                             sel_keys = NULL) {
  df <- tryCatch(sd$data(withSelection = FALSE), error = function(e) NULL)

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

  is_num <- function(cn) !is.null(df) && cn %in% names(df) && is.numeric(df[[cn]])
  xfmt <- if (is_num(xv)) "%{x:.4f}" else "%{x}"
  yfmt <- if (is_num(yv)) "%{y:.4f}" else "%{y}"

  marker_spec <- list(size = point_sizes,
                      color = "rgba(0, 114, 178, 0.75)",  # Okabe-Ito blue (match MD)
                      line = list(color = "rgba(0,0,0,0.35)", width = 0.4))
  if (use_size) {
    marker_spec$sizemode <- "diameter"
    marker_spec$sizemin  <- 3
  }

  if (!is.null(df) && !is.null(sel_keys) && length(sel_keys) &&
      ".key" %in% names(df)) {
    is_sel <- as.character(df$.key) %in% as.character(sel_keys)
  } else {
    is_sel <- rep(FALSE, if (is.null(df)) 0 else nrow(df))
  }
  has_selection <- any(is_sel)
  base_color <- if (has_selection) "rgba(0, 114, 178, 0.20)"
                else                "rgba(0, 114, 178, 0.75)"
  base_line  <- if (has_selection) "rgba(0,0,0,0.15)" else "rgba(0,0,0,0.35)"

  df_unsel <- if (!is.null(df)) df[!is_sel, , drop = FALSE] else NULL
  df_sel   <- if (!is.null(df)) df[ is_sel, , drop = FALSE] else NULL
  sizes_unsel <- if (length(point_sizes) == 1) point_sizes else point_sizes[!is_sel]
  sizes_sel   <- if (length(point_sizes) == 1) point_sizes else point_sizes[ is_sel]

  marker_unsel <- list(size = sizes_unsel, color = base_color,
                       line = list(color = base_line, width = 0.4))
  marker_sel   <- list(size = sizes_sel,   color = mzx_highlight_color,
                       line = list(color = "#8a7a00", width = 1.2))
  if (use_size) {
    marker_unsel$sizemode <- "diameter"; marker_unsel$sizemin <- 3
    marker_sel$sizemode   <- "diameter"; marker_sel$sizemin   <- 3
  }

  p <- plotly::plot_ly(source = sid)
  if (is.null(df_unsel) || nrow(df_unsel)) {
    sd_unsel <- if (has_selection)
      crosstalk::SharedData$new(df_unsel, key = ~.key, group = sd$groupName())
    else sd
    p <- p %>% plotly::add_markers(
      data = sd_unsel,
      x = as.formula(paste0("~", xv)),
      y = as.formula(paste0("~", yv)),
      key = ~.key, type = "scattergl", mode = "markers",
      name = "All features", showlegend = show_legend,
      marker = marker_unsel,
      hovertemplate = paste0(xv, "=", xfmt, "<br>", yv, "=", yfmt, "<extra></extra>"))
  }
  if (!is.null(df_sel) && nrow(df_sel)) {
    p <- p %>% plotly::add_markers(
      x = df_sel[[xv]], y = df_sel[[yv]],
      type = "scattergl", mode = "markers",
      key = if (".key" %in% names(df_sel)) as.character(df_sel$.key) else NULL,
      name = "Selected", showlegend = show_legend,
      marker = marker_sel, inherit = FALSE,
      hovertemplate = paste0(xv, "=", xfmt, "<br>", yv, "=", yfmt, "<extra></extra>"))
  }
  p <- p %>%
    mzx_style(xtitle = xv, ytitle = yv, legend = show_legend) %>%
    plotly::highlight(on = "plotly_selected", off = "plotly_deselect",
                      color = I(mzx_highlight_color), opacityDim = 0.30)
  if (!is.null(xaxis_ov)) p <- p %>% plotly::layout(xaxis = xaxis_ov)
  if (!is.null(yaxis_ov)) p <- p %>% plotly::layout(yaxis = yaxis_ov)
  p
}
