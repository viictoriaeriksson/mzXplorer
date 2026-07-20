#' Mass Defect Analysis Module UI
#' @param id module id
mass_defect_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        checkboxInput(ns("show_isf_tab"), "Show ISF analysis tab", FALSE),
        fileInput(ns('file1'), 'Choose Feature File (CSV / Excel)',
                  accept = c('.csv', '.xlsx', '.xls',
                             'text/csv', 'text/comma-separated-values,text/plain',
                             'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                             'application/vnd.ms-excel')),
        uiOutput(ns("file1_status")),

        # --- Shown on "Plot & Table" — static subtab (Mass Defect + Column Mapping) ---
        # Kept as plain static UI (no renderUI) so the Process/Plot input IDs are
        # bound immediately on page load. This eliminates the click-does-nothing
        # race that can happen when observers are attached to dynamically
        # rendered actionButtons.
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Plot_Table'", ns("result_sections")),
          tabsetPanel(type = "pills", id = ns("plot_tab_subtabs"),
            tabPanel("Mass Defect",
              div(class = "mzx-section mzx-plot",
                tags$h4("Mass Defect formulas"),
                fluidRow(
                  column(12, textInput(ns("cus1"), "MD formula 1", value = "CH2,O"))
                ),
                fluidRow(
                  column(12, textInput(ns("cus2"), "MD formula 2", value = "Cl-H"))
                ),
                radioButtons(ns("rounding"), "Rounding",
                             choices  = c("round", "ceiling", "floor"),
                             selected = "round", inline = TRUE),
                actionButton(ns('go_process'), 'Process', width = "100%", class = "btn-primary"),
                tags$br(), tags$br(),
                checkboxInput(ns('ins'), 'Show intensity/variable as size', FALSE),
                checkboxInput(ns("show_leg"), "Show plot legends", TRUE),
                uiOutput(ns("slide1")),
                uiOutput(ns("slide2")),
                uiOutput(ns("slide3")),
                actionButton(ns('go'), 'Plot', width = "100%", class = "btn-primary")
              )
            ),
            tabPanel("Column Mapping",
              div(class = "mzx-section mzx-cmap",
                uiOutput(ns("col_map_ui"))
              )
            )
          )
        ),

        # --- Shown on "Homologue series" ---
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Homologue_series'", ns("result_sections")),
          div(class = "mzx-section mzx-homol",
            tags$h4("Homologue search"),
            checkboxInput(ns("run_homol"), "Detect homologues in filtered data", FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("run_homol")),
              textInput(ns("homol_unit"), "Repeating unit(s) — comma-separated (e.g. CH2, CF2, C2H4O)", value = "CH2"),
              numericInput(ns("homol_ppm"), "m/z tolerance (ppm)", value = 5, min = 1, step = 1),
              uiOutput(ns("ccs_block")),
              numericInput(ns("homol_minlen"), "Minimum series length", value = 4, min = 3, step = 1),
              numericInput(ns("homol_rttol"), "Per-step RT tolerance (same units as Rt); 0 = off", value = 0, min = 0, step = 0.1),
              selectInput(ns("homol_rttrend"), "RT / CCS / ion mobility trend",
                          choices  = c("increasing", "decreasing", "any"), selected = "increasing"),
              checkboxInput(ns("homol_allow_gaps"), "Allow single gaps (k = 2)", FALSE),
              numericInput(ns("homol_R2"), "Minimum spline R² (0 = off)", value = 0.98, min = 0, max = 1, step = 0.01),
              actionButton(ns("go_homol"), "Calculate homologues", width = "100%", class = "btn-secondary"),
              fluidRow(style = "margin-top:6px;",
                column(6, actionButton(ns("cancel_homol"), "Cancel",
                                       class = "btn-outline-warning btn-sm",
                                       width = "100%")),
                column(6, tags$button("Reload session",
                                       id = ns("reload_session_md"),
                                       class = "btn btn-outline-danger btn-sm",
                                       style = "width:100%;",
                                       onclick = "window.location.reload(true); return false;",
                                       title = "Force-reload if a run is stuck. Works even while R is busy."))
              ),
              tags$small(style = "color:#777;",
                         "Cancel halts before the next processing phase; the graph search itself cannot be interrupted mid-run.To fully interupt use Reload session")
            )
          )
        ),

        # --- Shown on "MD / m/z differences" ---
        conditionalPanel(
          condition = sprintf("input['%s'] == 'MD_mz_differences'", ns("result_sections")),
          div(class = "mzx-section mzx-mdiff",
            tags$h4("MD / m/z differences"),
            checkboxInput(ns("enable_md_diff"), "Calculate Mass Defect Differences", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("enable_md_diff")),
              textInput(ns("md_values"), "Mass Defect Differences, mDa (comma-separated)",
                        value = "-5.1, -15.7, 32.1",
                        placeholder = "e.g., -5.1, -15.7, -23.5, 32.1, -43.2, 68.2"),
              textInput(ns("mz_values"), "m/z Differences, Da (comma-separated)",
                        value = "15.9949, -2.0157, 176.0321",
                        placeholder = "e.g., 15.9949, -2.0157, -15.0235, 176.0321, 79.9568, 305.0682"),
              numericInput(ns("MZtolerance"), "m/z difference tolerance (Da)", value = 0.5, min = 0, step = 0.1),
              actionButton(ns("MDcalculate"), "Calculate MD/mz Differences", width = "100%", class = "btn-warning")
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
        tabsetPanel(
          id = ns("result_sections"),
          type = "tabs",
          tabPanel(
            "Plot & Table", value = "Plot_Table",
            div(class = "mzx-pane mzx-plot",
              tags$h4("Interactive Plots to explore MD data"),
              uiOutput(ns("plotctr")),
              fluidRow(
                column(width = 6, plotly::plotlyOutput(ns("DTPlot1"))),
                column(width = 6, plotly::plotlyOutput(ns("DTPlot2")))
              ),
              DT::DTOutput(ns("table_selected")),
              fluidRow(style = "margin-top:6px;",
                column(4, downloadButton(ns("x3"), "Export Data")),
                column(4, actionButton(ns("reset_sel_md"), "Reset selection", class = "btn-outline-secondary"))
              )
            )
          ),
          tabPanel(
            "Homologue series", value = "Homologue_series",
            div(class = "mzx-pane mzx-homol",
              tags$h4("Homologue series"),
              uiOutput(ns("intensity_ctr")),
              plotly::plotlyOutput(ns("barplot")),
              fluidRow(style = "margin-top:6px;",
                column(4, actionButton(ns("clear_homol_sel"), "Clear series selection", class = "btn-outline-secondary"))
              ),
              tags$br(),
              DT::DTOutput(ns("homol_table"))
            )
          ),
          tabPanel(
            "MD / m/z differences", value = "MD_mz_differences",
            div(class = "mzx-pane mzx-mdiff",
              tags$h4("MD / m/z difference results"),
              tags$small(style = "color:#555;",
                         "Table is filtered by the network-plot selection. Use 'Reset selection' to clear."),
              tags$br(),
              checkboxInput(ns("only_matches"),
                            "Show only features with a match (and their matched partners)",
                            value = FALSE),
              tags$br(),
              DT::DTOutput(ns("feature_table")),
              tags$br(),
              fluidRow(
                column(9, plotly::plotlyOutput(ns("md_network_plot"), height = "500px")),
                column(3, actionButton(ns("reset_sel_mdnet"), "Reset selection",
                                       class = "btn-outline-secondary",
                                       style = "margin-top:10px;"))
              )
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

#' Mass Defect Analysis Module Server
#' @param id module id
mass_defect_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    vals <- reactiveValues(
      d = NULL, m = NULL, summ = NULL,
      series_keys = NULL, series_sel = NULL, series_cols = NULL
    )

    # --- Filter helper ---
    # Restrict a data.frame to rows where the chosen column has data:
    # not NA, and (for numeric columns) strictly greater than zero. This
    # is the semantics for "feature was detected in this sample". If
    # `col` is empty / "(none)" / not present, the df is returned as-is.
    .apply_col_filter <- function(df, col) {
      if (is.null(df) || !nrow(df)) return(df)
      if (is.null(col) || !length(col) || col %in% c("", "(none)") ||
          !col %in% names(df)) return(df)
      v <- df[[col]]
      keep <- !is.na(v)
      if (is.numeric(v)) keep <- keep & v > 0
      df[keep, , drop = FALSE]
    }
    
    # --- Shared Data Helpers ---
    # Split raw file IO from formula-dependent MDH computation so that typing
    # in cus1/cus2/rounding does not re-read the CSV.
    #
    # `raw_input` reads the CSV once. Column mapping (user-selectable) is then
    # applied in `raw_file()` so users can point their columns at the internal
    # targets (mz, rt, intensity, id, ccs).
    raw_input <- reactive({
      req(input$file1)
      withProgress(message = "Loading feature list...", value = 0, {
        df <- read_feature_file(input$file1$datapath,
                                orig_name = input$file1$name)
        validate(need(!is.null(df) && ncol(df) > 0,
                      "Could not read the file. Please provide a CSV or TSV text file."))
        df
      })
    })

    # --- Upload-status badge under the fileInput ---
    output$file1_status <- renderUI({
      if (is.null(input$file1)) return(NULL)
      df <- tryCatch(raw_input(), error = function(e) e)
      if (inherits(df, "error"))
        return(div(style = "margin:-8px 0 10px 2px;color:#c62828;font-size:12px;",
                   tags$span(style="font-weight:600;", "\u2716 "),
                   paste("File error:", conditionMessage(df))))
      if (is.null(df))
        return(div(style = "margin:-8px 0 10px 2px;color:#6a6a6a;font-size:12px;",
                   tags$span(style="font-weight:600;", "\u23F3 "),
                   "Reading file..."))
      div(style = "margin:-8px 0 10px 2px;color:#2e7d32;font-size:12px;",
          tags$span(style="font-weight:600;", "\u2714 "),
          sprintf("Loaded: %s  (%s rows, %s columns)",
                  input$file1$name,
                  format(nrow(df), big.mark = " "),
                  format(ncol(df), big.mark = " ")))
    })

    # Render the column-mapping UI as soon as a file is loaded
    output$col_map_ui <- renderUI({
      if (is.null(raw_input())) {
        return(tags$div(style = "color:#777;",
                        tags$em("Upload a feature file first, then map its columns here.")))
      }
      nms <- names(raw_input())
      defaults <- guess_default_mapping(nms)
      choices <- c("(none)", nms)
      tagList(
        tags$h4("Column mapping"),
        tags$small(style = "color:#555;",
                   "Map your file's columns to the required fields. Defaults are guessed from column names."),
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

    # (plot_tab_subtabs_ui is now static UI in mass_defect_ui — no renderUI here.)

    # Show/hide the ISF navbar tab based on the checkbox.
    # Only hides the nav <li> (Bootstrap manages tab-pane naturally).
    # Switches to MD tab first if the ISF tab is currently active.
    observeEvent(input$show_isf_tab, {
      if (isTRUE(input$show_isf_tab)) {
        shinyjs::runjs("$('a[data-value=\"In-Source Fragmentation\"]').closest('li').show();")
      } else {
        shinyjs::runjs("
          var $li = $('a[data-value=\"In-Source Fragmentation\"]').closest('li');
          if ($('div.tab-pane.active[data-value=\"In-Source Fragmentation\"]').length) {
            $('a[data-value=\"Mass Defect Analysis\"]').tab('show');
          }
          $li.hide();
        ")
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    # Resolve the current column mapping. If the user has explicitly set a
    # value in the Column Mapping pill, use it; otherwise fall back to the
    # auto-guessed default based on column names. This lets Process work
    # immediately after upload, without forcing the user to open the
    # Column Mapping pill first.
    resolved_mapping <- reactive({
      req(raw_input())
      nms <- names(raw_input())
      g   <- guess_default_mapping(nms)
      pick <- function(user_val, guess_val) {
        if (!is.null(user_val) && nzchar(user_val) && user_val != "(none)" &&
            user_val %in% nms) return(user_val)
        if (!is.null(guess_val) && !is.na(guess_val) && guess_val %in% nms) return(guess_val)
        NA_character_
      }
      list(
        mz        = pick(input$map_mz,        g$mz),
        rt        = pick(input$map_rt,        g$rt),
        intensity = pick(input$map_intensity, g$intensity),
        id        = pick(input$map_id,        g$id),
        ccs       = pick(input$map_ccs,       g$ccs)
      )
    })

    raw_file <- reactive({
      df <- data.table::copy(raw_input())
      m  <- resolved_mapping()

      # Fail loudly if the required columns can't be resolved even by guessing.
      missing_req <- c(
        if (is.na(m$mz))        "m/z"       else NULL,
        if (is.na(m$rt))        "RT"        else NULL,
        if (is.na(m$intensity)) "intensity" else NULL
      )
      validate(need(length(missing_req) == 0,
                    paste0("Could not auto-detect these required columns: ",
                           paste(missing_req, collapse = ", "),
                           ". Please set them in the Column Mapping pill.")))

      df <- apply_column_mapping(df, m)

      # Coerce to numeric where required
      for (col in intersect(c("mz", "rt", "intensity", "ccs"), names(df))) {
        suppressWarnings(df[, (col) := as.numeric(get(col))])
      }

      if (!"id" %in% names(df)) df$id <- seq_len(nrow(df))

      df$RMD_ppm <- round((round(df$mz) - df$mz) / df$mz * 1e6)
      df$OMD_mDa <- round((round(df$mz) - df$mz) * 1e3)
      df
    })
    
    MD_data_raw <- eventReactive(input$go_process, {
      df <- raw_file()
      withProgress(message = "Applying MD formulas...", value = 0.3, {
        mdh1 <- enviGCMS::getmdh(df$mz, cus = input$cus1, method = input$rounding)
        mdh2 <- enviGCMS::getmdh(df$mz, cus = input$cus2, method = input$rounding)

        mdh  <- cbind(mdh1[, -1, drop=FALSE], mdh2[, -1, drop=FALSE])
        colnames(mdh) <- c(paste0("Formula1_", colnames(mdh1))[-1],
                          paste0("Formula2_", colnames(mdh2))[-1])
        showNotification("Mass defect formulas processed.", type = "message")
        cbind(df, mdh)
      })
    })
    
    has_ccs <- reactive({ "ccs" %in% names(MD_data_raw()) })
    
    # --- Dynamic UI ---
    output$ccs_block <- renderUI({
      if (!has_ccs()) {
        tagList(tags$h5("CCS / ion mobility rules"),
                tags$small(style = "color:#777;",
                           "No CCS or ion mobility column mapped."))
      } else {
        tagList(
          checkboxInput(ns("use_ccs_toggle"),
                        "Enable CCS / ion mobility-based homologue rules",
                        value = FALSE),
          numericInput(ns("homol_ccstol"),
                       "CCS / ion mobility tolerance",
                       value = 0, min = 0, step = 0.1)
        )
      }
    })
    
    output$slide1 <- renderUI({ df <- MD_data_raw(); sliderInput(ns("slide1"), "Intensity range", min = min(df$intensity, na.rm=T), max = max(df$intensity, na.rm=T), value = range(df$intensity, na.rm=T)) })
    output$slide2 <- renderUI({ df <- MD_data_raw(); sliderInput(ns("slide2"), "m/z range", min = min(df$mz, na.rm=T), max = max(df$mz, na.rm=T), value = range(df$mz, na.rm=T)) })
    output$slide3 <- renderUI({ df <- MD_data_raw(); sliderInput(ns("slide3"), "RT range", min = min(df$rt, na.rm=T), max = max(df$rt, na.rm=T), value = range(df$rt, na.rm=T)) })
    
    output$plotctr <- renderUI({
      df <- MD_data_raw()
      tagList(
        fluidRow(
          column(6, selectInput(ns('filter_col'),
                                'Filter by sample (select sample column). Filter applies to network plot as well.',
                                choices = c("(none)", names(df)), selected = "(none)"))
        ),
        fluidRow(
          column(3, selectInput(ns('xvar1'), 'X variable Plot 1', choices = names(df), selected = "rt")),
          column(3, selectInput(ns('yvar1'), 'Y variable Plot 1', choices = names(df), selected = "mz")),
          column(3, selectInput(ns('xvar2'), 'X variable Plot 2', choices = names(df), selected = "RMD_ppm")),
          column(3, selectInput(ns('yvar2'), 'Y variable Plot 2', choices = names(df), selected = "mz"))
        )
      )
    })
    
    output$intensity_ctr <- renderUI({
      df <- MD_data_raw()
      cols <- names(select(df, where(is.numeric)))
      selectInput(ns('selectintensity'), 'Variable for intensity/size', choices = cols, selected = if("intensity" %in% cols) "intensity" else cols[1])
    })
    
    # --- Processing Action ---
    observeEvent(input$go, {
      req(MD_data_raw())
      withProgress(message = "Processing Mass Defect Analysis...", value = 0, {
        m <- MD_data_raw()
        m <- m[m$intensity >= input$slide1[1] & m$intensity <= input$slide1[2] &
               m$mz >= input$slide2[1] & m$mz <= input$slide2[2] &
               m$rt >= input$slide3[1] & m$rt <= input$slide3[2], ]

        m$series_id <- NA_integer_
        m$series_unit <- NA_character_
        m$.key <- sprintf("id%05d", seq_len(nrow(m)))
        d <- crosstalk::SharedData$new(m, key = ~.key, group = ns("md"))

        vals$m <- m
        vals$d <- d
        vals$series_keys <- NULL; vals$series_sel <- NULL; vals$series_cols <- NULL

        output$DTPlot1 <- plotly::renderPlotly({
          req(vals$m)
          m_f <- .apply_col_filter(vals$m, input$filter_col)
          if (!nrow(m_f)) return(plot_ly() %>%
                                    layout(title = "No features with data in chosen column"))
          d_f <- crosstalk::SharedData$new(as.data.frame(m_f),
                                           key = ~.key, group = ns("md"))
          # Sizing settings are captured at the moment of the Plot click
          # (via isolate) so that toggling the "Show intensity as size"
          # checkbox afterwards does NOT re-render the plots; the user has
          # to press Plot again for the change to take effect.
          ins_flag <- isolate(input$ins)
          i_col    <- isolate(input$selectintensity)
          if (is.null(i_col) || !nzchar(i_col) || !(i_col %in% names(m_f))) {
            i_col <- if ("intensity" %in% names(m_f)) "intensity" else NULL
          }
          make_mdplot(d_f, input$xvar1, input$yvar1, input$xvar1, input$yvar1,
                      ins_flag, i_col,
                      vals$series_sel, vals$series_cols, input$show_leg, ns("plot1"))
        })
        output$DTPlot2 <- plotly::renderPlotly({
          req(vals$m)
          m_f <- .apply_col_filter(vals$m, input$filter_col)
          if (!nrow(m_f)) return(plot_ly() %>%
                                    layout(title = "No features with data in chosen column"))
          d_f <- crosstalk::SharedData$new(as.data.frame(m_f),
                                           key = ~.key, group = ns("md"))
          ins_flag <- isolate(input$ins)
          i_col    <- isolate(input$selectintensity)
          if (is.null(i_col) || !nzchar(i_col) || !(i_col %in% names(m_f))) {
            i_col <- if ("intensity" %in% names(m_f)) "intensity" else NULL
          }
          make_mdplot(d_f, input$xvar2, input$yvar2, input$xvar2, input$yvar2,
                      ins_flag, i_col,
                      vals$series_sel, vals$series_cols, input$show_leg, ns("plot2"))
        })
      })
    })

    # --- Cancel flag for homologue search. R is single-threaded so the
    # find_homologues() call itself cannot be interrupted; the flag is
    # checked BEFORE we hand off to it.
    cancel_homol <- reactiveVal(FALSE)
    observeEvent(input$cancel_homol, {
      cancel_homol(TRUE)
      showNotification("Cancel requested — will stop before the next phase.",
                       type = "warning", duration = 3,
                       id = ns("nn_cancel_homol"))
    })
    observeEvent(input$reload_session_md, { session$reload() })

    # --- Homologue Search Action (independent button) ---
    observeEvent(input$go_homol, {
      req(vals$m)
      if (!isTRUE(input$run_homol)) {
        showNotification("Enable 'Detect homologues in filtered data' first.", type = "warning")
        return()
      }
      cancel_homol(FALSE)

      withProgress(message = "Homologue search", value = 0, {
        # Save the full data before find_homologues (which re-orders rows).
        # Match on the stable 'id' column to map results back, exactly like
        # the reference working implementation.
        m_full <- as.data.frame(vals$m)
        m_clean <- m_full
        m_clean$.key <- NULL
        if ("series_id"   %in% names(m_clean)) m_clean$series_id   <- NULL
        if ("series_unit" %in% names(m_clean)) m_clean$series_unit <- NULL

        incProgress(0.05, detail = sprintf("Preparing %d features...", nrow(m_full)))

        # Parse one or more repeating units (comma-separated).
        raw_unit <- input$homol_unit
        if (is.null(raw_unit)) raw_unit <- ""
        units <- trimws(unlist(strsplit(as.character(raw_unit), ",", fixed = TRUE)))
        units <- units[nzchar(units)]
        if (!length(units)) {
          showNotification("Please enter at least one repeating-unit formula.", type = "warning")
          return()
        }

        incProgress(0.05, detail = sprintf("Searching %d repeating unit(s): %s",
                                           length(units), paste(units, collapse = ", ")))

        # Initialise result columns as NA.
        m_full$series_id   <- NA_integer_
        m_full$series_unit <- NA_character_
        sid_offset <- 0L

        for (i in seq_along(units)) {
          u <- units[i]
          if (isTRUE(cancel_homol())) {
            showNotification(sprintf("Cancelled after unit '%s'.", units[max(i - 1L, 1L)]),
                             type = "warning", duration = 3, id = ns("nn_cancel_homol"))
            break
          }
          incProgress(0.5 / length(units),
                      detail = sprintf("Unit %d/%d: %s", i, length(units), u))

          res <- tryCatch(
            find_homologues(df = m_clean, unit = u,
                            ppm = input$homol_ppm,
                            rttol = input$homol_rttol,
                            allow_gaps = input$homol_allow_gaps,
                            min_length = input$homol_minlen,
                            rt_trend = input$homol_rttrend,
                            R2_min = input$homol_R2,
                            ccs_mode = if (isTRUE(input$use_ccs_toggle)) "ccs" else "rt",
                            ccs_tol  = if (isTRUE(input$use_ccs_toggle)) input$homol_ccstol else 0),
            error = function(e) {
              showNotification(sprintf("Unit '%s' failed: %s", u, conditionMessage(e)),
                               type = "error", duration = 8)
              NULL
            }
          )
          if (is.null(res) || !"id" %in% names(res) || !"series_id" %in% names(res)) next

          # Use match on stable 'id' — this is the robust pattern from the
          # reference implementation. Works regardless of row re-ordering
          # inside find_homologues.
          idx <- match(m_full$id, res$id)
          sid <- res$series_id[idx]   # NA for rows not in res
          hit <- which(!is.na(sid) & is.na(m_full$series_id))
          if (!length(hit)) next

          m_full$series_id[hit]   <- as.integer(sid[hit]) + sid_offset
          m_full$series_unit[hit] <- u
          sid_offset <- max(m_full$series_id, na.rm = TRUE)
        }

        incProgress(0.2, detail = "Finalising...")

        vals$m <- data.table::as.data.table(m_full)
        vals$d <- crosstalk::SharedData$new(as.data.frame(vals$m),
                                            key = ~.key, group = ns("md"))
        vals$series_keys <- NULL; vals$series_sel <- NULL; vals$series_cols <- NULL

        n_series <- length(unique(stats::na.omit(vals$m$series_id)))
        n_units_hit <- if ("series_unit" %in% names(vals$m)) length(unique(stats::na.omit(vals$m$series_unit))) else 0L
        showNotification(sprintf("Homologue search done. %d series found across %d unit(s).",
                                 n_series, n_units_hit),
                         type = "message")
      })
    })
    
    # --- Selection & Tables ---
    # Reactive-value backed selection so it can be programmatically reset
    sel_keys_rv <- reactiveVal(character(0))
    sel_gen_md  <- reactiveVal(0L)  # bumped on reset; plots depend on this
    # Cache of the exact data.frame currently rendered in `feature_table`
    # (including `.key`), so the row-selection observer can translate row
    # indices back into shared keys. Populated inside renderDT via isolate().
    feature_table_view <- reactiveVal(NULL)
    # Cache of the MD network plot's data + the trace index of its
    # (initially empty) highlight overlay. Populated at the end of the
    # renderPlotly reactive so that a plotlyProxy-based observer can
    # update the highlight in-place without triggering a full re-render.
    md_plot_state <- reactiveVal(NULL)
    # Tracks which UI element caused the most recent selection change.
    # Used by the sync observers to skip pushing the selection back to
    # its own source (which would cause a redundant round-trip and lag).
    sel_source <- reactiveVal("init")

    # Track selections from each linked plot INDEPENDENTLY. plotly::event_data()
    # is sticky per source, so a shared observe() would union stale keys from
    # plots the user hasn't touched. Each observer here fully REPLACES the
    # current selection with only the keys of the plot the user just acted on.
    .apply_sel <- function(ed) {
      sel_source("plot")
      sel_keys_rv(if (!is.null(ed) && length(ed$key)) unique(as.character(ed$key))
                  else character(0))
    }
    observeEvent(plotly::event_data("plotly_selected", source = ns("plot1")),
                 .apply_sel(plotly::event_data("plotly_selected", source = ns("plot1"))),
                 ignoreNULL = FALSE, ignoreInit = TRUE)
    observeEvent(plotly::event_data("plotly_selected", source = ns("plot2")),
                 .apply_sel(plotly::event_data("plotly_selected", source = ns("plot2"))),
                 ignoreNULL = FALSE, ignoreInit = TRUE)
    observeEvent(plotly::event_data("plotly_selected", source = ns("mdnet")),
                 .apply_sel(plotly::event_data("plotly_selected", source = ns("mdnet"))),
                 ignoreNULL = FALSE, ignoreInit = TRUE)

    observeEvent(input$reset_sel_md, {
      sel_source("reset")
      sel_keys_rv(character(0))
      if (!is.null(vals$m)) {
        vals$d <- crosstalk::SharedData$new(as.data.frame(vals$m),
                                            key = ~.key, group = ns("md"))
      }
      sel_gen_md(sel_gen_md() + 1L)
      vals$series_keys <- NULL; vals$series_sel <- NULL; vals$series_cols <- NULL
      showNotification("Selection reset.", type = "message",
                       duration = 2, id = ns("nn_reset_md"))
    })

    # Same reset behavior wired to the button under the MD network plot
    observeEvent(input$reset_sel_mdnet, {
      sel_source("reset")
      sel_keys_rv(character(0))
      if (!is.null(vals$m)) {
        vals$d <- crosstalk::SharedData$new(as.data.frame(vals$m),
                                            key = ~.key, group = ns("md"))
      }
      sel_gen_md(sel_gen_md() + 1L)
      vals$series_keys <- NULL; vals$series_sel <- NULL; vals$series_cols <- NULL
      showNotification("Selection reset.", type = "message",
                       duration = 2, id = ns("nn_reset_md"))
    })

    selected_keys <- reactive({
      sel_gen_md()
      if (!is.null(vals$series_keys)) return(vals$series_keys)
      sel_keys_rv()
    })
    
    output$table_selected <- DT::renderDT({
      req(vals$m); sel_keys <- selected_keys()
      # Coerce to data.frame first so downstream subsetting is not affected by
      # data.table semantics (which reject `[, cols]` with a character vector
      # and ignore `drop = FALSE`).
      df <- as.data.frame(vals$m)
      if (length(sel_keys)) df <- df[df$.key %in% sel_keys, , drop = FALSE]

      # Reorder columns: identity first, then MD-derived columns,
      # then homologue/CCS-related, then everything else. Drop the
      # internal crosstalk key from the visible table.
      nms <- setdiff(names(df), ".key")
      identity_cols <- intersect(c("id", "mz", "rt", "intensity"), nms)
      md_cols       <- intersect(c("OMD_mDa", "RMD_ppm"), nms)
      formula_cols  <- grep("^Formula[0-9]+_", nms, value = TRUE)
      series_cols_v <- intersect(c("series_id", "series_unit", "ccs"), nms)
      leading       <- unique(c(identity_cols, md_cols, formula_cols, series_cols_v))
      trailing      <- setdiff(nms, leading)
      df <- df[, c(leading, trailing), drop = FALSE]

      DT::datatable(df, editable = TRUE, rownames = FALSE, filter = "top",
                    options = list(scrollX = TRUE, pageLength = 15))
    })
    
    output$barplot <- plotly::renderPlotly({
      req(vals$m); sel_keys <- selected_keys()
      df <- if (length(sel_keys)) vals$m[vals$m$.key %in% sel_keys, ] else return(NULL)
      selvar <- input$selectintensity
      rel_y <- df[[selvar]] / max(df[[selvar]], na.rm = TRUE) * 100
      plotly::plot_ly(df, x = ~mz, y = rel_y, type = "bar",
                      marker = list(color = "#0072B2",
                                    line = list(color = "#003c62", width = 0.4)),
                      hovertemplate = paste0("m/z=%{x:.4f}<br>Rel. ", selvar,
                                             "=%{y:.1f}%<extra></extra>")) %>%
        mzx_style(xtitle = "m/z",
                  ytitle = paste("Relative", selvar, "(%)"),
                  legend = FALSE)
    })
    
    output$homol_table <- DT::renderDT({
      req(vals$m)
      # Empty placeholder table before / without a homologue run so the
      # user sees the column structure and knows where results will
      # appear once they click "Calculate homologues".
      empty <- data.frame(series_id = integer(0), unit = character(0), n = integer(0),
                          mz_min = numeric(0), rt_min = numeric(0),
                          int_sum = numeric(0))
      has_sid <- "series_id" %in% names(vals$m)
      if (!has_sid) {
        return(DT::datatable(
          empty, rownames = FALSE, selection = "multiple",
          caption = "No homologue search has been run yet. Enable 'Detect homologues in filtered data' in the sidebar and click 'Calculate homologues'.",
          options = list(scrollX = TRUE, dom = "t")))
      }
      ms <- vals$m[!is.na(vals$m$series_id), ]
      if (!nrow(ms)) {
        return(DT::datatable(
          empty, rownames = FALSE, selection = "multiple",
          caption = "Homologue search ran but no series were found with the current settings. Try loosening ppm / RT tolerance or reducing minimum series length.",
          options = list(scrollX = TRUE, dom = "t")))
      }
      summ <- ms %>% group_by(series_id) %>% summarize(
        unit = if ("series_unit" %in% names(ms)) dplyr::first(stats::na.omit(series_unit)) else NA_character_,
        n = n(), mz_min = min(mz), rt_min = min(rt), int_sum = sum(intensity),
        .groups = "drop"
      )
      summ <- summ[, c("series_id", "unit", "n", "mz_min", "rt_min", "int_sum"), drop = FALSE]
      vals$summ <- summ
      DT::datatable(summ, rownames = FALSE, selection = "multiple", options = list(scrollX = TRUE))
    })
    
    observeEvent(input$homol_table_rows_selected, {
      req(vals$summ); ids <- vals$summ$series_id[input$homol_table_rows_selected]
      if (!length(ids)) {
        vals$series_keys <- NULL; vals$series_sel <- NULL; vals$series_cols <- NULL
        if (!is.null(vals$d)) vals$d$selection(NULL)
        return()
      }
      keys <- vals$m$.key[!is.na(vals$m$series_id) & vals$m$series_id %in% ids]
      cols <- series_palette(length(ids)); names(cols) <- as.character(ids)
      vals$series_keys <- keys; vals$series_sel <- ids; vals$series_cols <- cols
      # NOTE: intentionally do NOT call vals$d$selection(keys) — the crosstalk
      # highlight recolors every selected point to a single accent color and
      # would mask the per-series colors drawn as overlay traces in make_mdplot.
      # The selected-data table + barplot are already driven by selected_keys()
      # which short-circuits to vals$series_keys.
      if (!is.null(vals$d)) vals$d$selection(NULL)
      showNotification(
        sprintf("Highlighting %d serie%s (%d features).",
                length(ids), if (length(ids) == 1) "" else "s", length(keys)),
        type = "message", duration = 2, id = ns("nn_homol_pick")
      )
    }, ignoreInit = TRUE)

    observeEvent(input$clear_homol_sel, {
      if (!is.null(vals$d)) vals$d$selection(NULL)
      vals$series_keys <- NULL; vals$series_sel <- NULL; vals$series_cols <- NULL
      # Also deselect the picked rows in the homologue series DataTable so
      # the UI reflects the cleared state.
      DT::selectRows(DT::dataTableProxy("homol_table", session = session), NULL)
      showNotification("Cleared series highlight.", type = "message",
                       duration = 2, id = ns("nn_homol_clear"))
    })
    
    output$x3 <- downloadHandler(filename = "MD_export.csv", content = function(f) { 
      sel <- selected_keys(); out <- if(length(sel)) vals$m[vals$m$.key %in% sel, ] else vals$m
      data.table::fwrite(out, f) 
    })
  
    
    
    ##########################################################
    # Replace the MD_data_annotated reactive with this:
    MD_data_annotated <- eventReactive(input$MDcalculate, {
            req(MD_data_raw())
            df <- as.data.frame(MD_data_raw())
            
            md_diffs <- suppressWarnings(as.numeric(unlist(strsplit(gsub(" ", "", input$md_values), ","))))
            mz_diffs <- suppressWarnings(as.numeric(unlist(strsplit(gsub(" ", "", input$mz_values), ","))))
            
            if (anyNA(md_diffs) || anyNA(mz_diffs)) {
                    showNotification("Error parsing MD/mz differences. Use comma-separated numbers.", type = "error")
                    return(df)
            }
            if (length(md_diffs) != length(mz_diffs)) {
                    showNotification("Number of MD values must match number of m/z values!", type = "error")
                    return(df)
            }
            if (!"OMD_mDa" %in% names(df) || !"mz" %in% names(df)) {
                    showNotification("Required columns (OMD_mDa, mz) not found in data", type = "error")
                    return(df)
            }
            
            n <- nrow(df)
            da_tol <- input$MZtolerance
            md_tol <- 0.1
            ids <- as.character(df$id)

            # Build a compact keyed data.table once. All match-searches
            # below use non-equi joins into this table — O(N · k) time
            # and O(N) memory, instead of the previous O(N^2) `outer()`
            # matrix which OOMs above ~15-20k features.
            DT <- data.table::data.table(
              row_idx = seq_len(n),
              row_id  = ids,
              omd     = df$OMD_mDa,
              mz      = df$mz
            )
            data.table::setkey(DT, omd, mz)

            withProgress(message = "Calculating MD + m/z differences...", value = 0, {
                    for (idx in seq_along(md_diffs)) {
                            target_md <- md_diffs[idx]
                            target_mz <- mz_diffs[idx]

                            # Shifted-interval query table: each "from"
                            # row asks "which rows fall within the
                            # target shift + tolerance box?".
                            A <- DT[, .(from_idx = row_idx,
                                        omd_lo = omd + target_md - md_tol,
                                        omd_hi = omd + target_md + md_tol,
                                        mz_lo  = mz  + target_mz - da_tol,
                                        mz_hi  = mz  + target_mz + da_tol)]

                            hits <- DT[A,
                                       on = .(omd >= omd_lo, omd <= omd_hi,
                                              mz  >= mz_lo,  mz  <= mz_hi),
                                       nomatch = 0, allow.cartesian = TRUE,
                                       .(from_idx = i.from_idx,
                                         to_idx   = row_idx,
                                         to_id    = row_id)]
                            hits <- hits[from_idx != to_idx]

                            col_name <- paste0("MD=", target_md,
                                               "_mz=", round(target_mz, 4),
                                               " -> id")
                            out_vec <- rep("", n)
                            if (nrow(hits)) {
                                    agg <- hits[, .(str = paste(to_id, collapse = ", ")),
                                                keyby = from_idx]
                                    out_vec[agg$from_idx] <- agg$str
                            }
                            df[[col_name]] <- out_vec

                            incProgress(1 / length(md_diffs),
                                        detail = paste("Pair", idx, "of", length(md_diffs)))
                    }
            })
            
            match_cols <- grep(" -> id$", names(df), value = TRUE)
            if (length(match_cols) > 0) {
                    total_matches <- sum(rowSums(df[, match_cols, drop = FALSE] != "") > 0)
                    showNotification(paste("Found", total_matches, "features with MD+m/z matches!"), 
                                     type = "message")
            }
            
            df
    })
    
    
    # --- MD network plot (linked via crosstalk to the two scatter plots) ---
    output$md_network_plot <- renderPlotly({
      sel_gen_md()  # re-render on selection reset
      req(MD_data_annotated(), vals$m)
      tryCatch({
      df_full <- as.data.frame(MD_data_annotated())

      # Keep only rows currently plotted, and bring in the shared .key
      key_map <- as.data.frame(vals$m)[, c("id", ".key")]
      df <- merge(df_full, key_map, by = "id", all = FALSE)
      if (!nrow(df)) return(plot_ly() %>% layout(title = "No overlapping rows to display"))

      # Sample-column filter (shared with the two interactive plots).
      df <- .apply_col_filter(df, input$filter_col)
      if (!nrow(df)) return(plot_ly() %>%
                              layout(title = "No features with data in chosen column"))

      diff_cols <- grep(" -> id$", names(df), value = TRUE)

      # "Only matches" filter mirrors the feature_table checkbox: keep
      # rows with at least one non-empty match column, plus the features
      # they matched to. This also reduces the point count in the plot.
      if (isTRUE(input$only_matches) && length(diff_cols)) {
        has_match_pre <- rowSums(df[, diff_cols, drop = FALSE] != "") > 0
        matched_ids <- suppressWarnings(as.numeric(unlist(strsplit(
          unlist(df[has_match_pre, diff_cols, drop = FALSE], use.names = FALSE),
          ",\\s*"))))
        matched_ids <- matched_ids[!is.na(matched_ids)]
        keep_row <- has_match_pre |
          (if ("id" %in% names(df)) df$id %in% matched_ids else FALSE)
        df <- df[keep_row, , drop = FALSE]
        if (!nrow(df))
          return(plot_ly() %>% layout(title = "No features with matches"))
      }

      # Empty state: no differences calculated yet -> still allow linked selection
      if (length(diff_cols) == 0) {
        sd_all <- crosstalk::SharedData$new(df, key = ~.key, group = ns("md"))
        p_empty <- plot_ly(source = ns("mdnet")) %>%
          add_markers(data = sd_all, x = ~OMD_mDa, y = ~mz,
                      marker = list(color = "rgba(0, 114, 178, 0.55)", size = 7,
                                    line = list(color = "rgba(0,0,0,0.35)", width = 0.4)),
                      text = ~paste0("id=", id,
                                     "<br>m/z=", round(mz, 4),
                                     "<br>OMD_mDa=", round(OMD_mDa, 3)),
                      hoverinfo = "text", name = "features") %>%
          # Permanent (initially empty) highlight trace, updated via proxy
          add_markers(x = numeric(0), y = numeric(0),
                      name = "Table selection",
                      marker = list(color = "#F0E442", size = 12, opacity = 0.95,
                                    line = list(color = "#8a7a00", width = 1.2)),
                      hoverinfo = "text", text = character(0),
                      inherit = FALSE, showlegend = TRUE) %>%
          mzx_style(xtitle = "OMD (mDa)", ytitle = "m/z",
                    title = "No MD + m/z differences calculated yet") %>%
          plotly::highlight(on = "plotly_selected", off = "plotly_deselect",
                            color = I(mzx_highlight_color), opacityDim = 0.25)
        # Highlight trace is the 2nd trace (0-based index 1) in this branch.
        md_plot_state(list(df = df, hi_idx = 1L))
        return(p_empty)
      }

      # Build edge segments
      lx <- numeric(0); ly <- numeric(0)
      for (col in diff_cols) {
        nonempty <- which(df[[col]] != "")
        if (!length(nonempty)) next
        matched_lists <- strsplit(df[[col]][nonempty], ", ", fixed = TRUE)
        lens <- lengths(matched_lists)
        from_idx <- rep(nonempty, lens)
        to_ids   <- suppressWarnings(as.numeric(unlist(matched_lists, use.names = FALSE)))
        to_idx   <- match(to_ids, df$id)
        ok <- !is.na(to_idx)
        if (!any(ok)) next
        from_idx <- from_idx[ok]; to_idx <- to_idx[ok]
        lx <- c(lx, as.vector(rbind(df$OMD_mDa[from_idx], df$OMD_mDa[to_idx], NA_real_)))
        ly <- c(ly, as.vector(rbind(df$mz[from_idx],      df$mz[to_idx],      NA_real_)))
      }

      has_match     <- rowSums(df[, diff_cols, drop = FALSE] != "") > 0
      df_with_match <- df[has_match,  , drop = FALSE]
      df_no_match   <- df[!has_match, , drop = FALSE]

      sd_no  <- crosstalk::SharedData$new(df_no_match,   key = ~.key, group = ns("md"))
      sd_yes <- crosstalk::SharedData$new(df_with_match, key = ~.key, group = ns("md"))

      p <- plot_ly(source = ns("mdnet")) %>%
        add_trace(type = "scatter", mode = "lines",
                  x = lx, y = ly,
                  line = list(color = "rgba(120,120,120,0.35)", width = 1),
                  showlegend = FALSE, hoverinfo = "skip") %>%
        add_markers(data = sd_no,  x = ~OMD_mDa, y = ~mz, name = "No match",
                    text = ~paste0("id=", id,
                                   "<br>m/z=", round(mz, 4),
                                   "<br>OMD_mDa=", round(OMD_mDa, 3)),
                    hoverinfo = "text",
                    marker = list(color = "rgba(86, 180, 233, 0.55)", size = 7,
                                  line = list(color = "rgba(0,0,0,0.35)", width = 0.4))) %>%
        add_markers(data = sd_yes, x = ~OMD_mDa, y = ~mz, name = "Has match",
                    text = ~paste0("id=", id,
                                   "<br>m/z=", round(mz, 4),
                                   "<br>OMD_mDa=", round(OMD_mDa, 3)),
                    hoverinfo = "text",
                    marker = list(color = "#D55E00", size = 9, opacity = 0.85,
                                  line = list(color = "#7a2f00", width = 0.5))) %>%
        # Permanent (initially empty) highlight trace. It stays at the
        # SAME trace index for the lifetime of this render, so the
        # plotlyProxy-based observer below can update just its x/y/text
        # with a "restyle" call — much faster than re-rendering.
        add_markers(x = numeric(0), y = numeric(0),
                    name = "Table selection",
                    marker = list(color = "#F0E442", size = 12, opacity = 0.95,
                                  line = list(color = "#8a7a00", width = 1.2)),
                    hoverinfo = "text", text = character(0),
                    inherit = FALSE, showlegend = TRUE) %>%
        mzx_style(xtitle = "OMD (mDa)", ytitle = "m/z",
                  title = "MD + m/z difference network") %>%
        plotly::highlight(on = "plotly_selected", off = "plotly_deselect",
                          color = I(mzx_highlight_color), opacityDim = 0.25)

      # Highlight overlay is the 4th trace in this branch:
      # 0 = edges (lines), 1 = No match, 2 = Has match, 3 = highlight.
      md_plot_state(list(df = df, hi_idx = 3L))
      p
    }, error = function(e) {
      showNotification(paste("MD network plot error:", conditionMessage(e)),
                       type = "error", duration = 15,
                       id = ns("nn_mdnet_err"))
      plot_ly() %>% layout(title = paste("MD network plot error:",
                                         conditionMessage(e)))
    })
    })
    
    output$feature_table <- renderDT({
            df <- if (is.null(input$MDcalculate) || input$MDcalculate == 0) {
                    req(MD_data_raw())
                    as.data.frame(tibble::as_tibble(MD_data_raw()))
            } else {
                    req(MD_data_annotated())
                    as.data.frame(tibble::as_tibble(MD_data_annotated()))
            }
            df <- as.data.frame(df)

            # Bring the shared .key from vals$m (added at processing time)
            # so selections can be translated between this table and the
            # linked plots in either direction.
            if (!is.null(vals$m) && "id" %in% names(df)) {
                    key_map <- as.data.frame(vals$m)[, c("id", ".key"), drop = FALSE]
                    df <- merge(df, key_map, by = "id", all.x = TRUE, sort = FALSE)
            }

            # NOTE: we deliberately do NOT filter this table by
            # selected_keys(). Filtering here caused a feedback loop with
            # `input$feature_table_rows_selected` (the DT re-render drops
            # DOM row selection, which the observer then interprets as
            # "user deselected everything"). Instead, plot-driven
            # selections are reflected via a dataTableProxy call to
            # DT::selectRows below.

            # "Only matches" filter: keep rows that have at least one
            # non-empty match column, plus the features they matched to
            # (so the user sees both sides of every pair).
            if (isTRUE(input$only_matches)) {
                    diff_cols_all <- grep(" -> id$", names(df), value = TRUE)
                    if (length(diff_cols_all)) {
                            has_match <- rowSums(df[, diff_cols_all, drop = FALSE] != "") > 0
                            matched_ids <- suppressWarnings(as.numeric(unlist(strsplit(
                              unlist(df[has_match, diff_cols_all, drop = FALSE],
                                     use.names = FALSE),
                              ",\\s*"))))
                            matched_ids <- matched_ids[!is.na(matched_ids)]
                            keep_row <- has_match |
                              (if ("id" %in% names(df)) df$id %in% matched_ids else FALSE)
                            df <- df[keep_row, , drop = FALSE]
                    }
            }

            nms <- setdiff(names(df), ".key")
            identity_cols <- intersect(c("id", "mz", "rt", "intensity"), nms)
            md_cols       <- intersect(c("OMD_mDa", "RMD_ppm"), nms)
            formula_cols  <- grep("^Formula[0-9]+_", nms, value = TRUE)
            diff_cols     <- grep(" -> id$", nms, value = TRUE)
            leading       <- unique(c(identity_cols, md_cols, formula_cols))
            other_cols    <- setdiff(nms, c(leading, diff_cols))

            display_cols <- c(leading, diff_cols, other_cols)

            # Cache the rendered frame (including `.key`) so the row-selection
            # observer below can look up which shared keys the user picked.
            isolate(feature_table_view(df))

            datatable(df[, display_cols, drop = FALSE],
                      options = list(pageLength = 10, scrollX = TRUE),
                      rownames = FALSE,
                      selection = "multiple") %>%
                    formatStyle(
                            columns = diff_cols,
                            backgroundColor = styleEqual("", "white", default = "#e8f5e9")
                    )
    })

    # Table -> plots: push feature_table row selections into the shared
    # selection state. Guarded with setequal() to prevent an infinite update
    # loop when the table re-renders after sel_keys_rv changes.
    observeEvent(input$feature_table_rows_selected, {
      view <- feature_table_view()
      if (is.null(view) || !".key" %in% names(view)) return()
      idx  <- input$feature_table_rows_selected
      keys <- if (length(idx)) unique(as.character(view$.key[idx])) else character(0)
      keys <- keys[!is.na(keys)]
      if (!setequal(keys, sel_keys_rv())) {
        sel_source("table")
        sel_keys_rv(keys)
        # Note: we deliberately do NOT bump sel_gen_md() here. The
        # highlight overlay is updated in-place via a plotlyProxy
        # observer below (see md_plot_state), so a full renderPlotly
        # is not needed and would cause lag on large data.
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Plots -> table: whenever the shared selection changes from an
    # external source (lasso on a linked plot, reset button, etc.),
    # highlight the matching rows in the feature_table via a DT proxy.
    # The setequal() guard in the table->plots observer prevents the
    # resulting `rows_selected` event from bouncing back and clearing
    # the state.
    observeEvent(sel_keys_rv(), {
      # Skip the DT round-trip when the selection came from the DT
      # itself. Without this, every row click triggers a redundant
      # DT::selectRows message to the browser, which re-emits
      # rows_selected and piles up lag on rapid multi-row selection.
      if (identical(sel_source(), "table")) return()
      view <- feature_table_view()
      proxy <- DT::dataTableProxy("feature_table", session = session)
      if (is.null(view) || !".key" %in% names(view)) {
        DT::selectRows(proxy, NULL)
        return()
      }
      keys <- sel_keys_rv()
      rows <- if (length(keys)) which(view$.key %in% keys) else integer(0)
      DT::selectRows(proxy, rows)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Fast selection-highlight update for the MD network plot. Uses
    # plotlyProxy to restyle ONLY the permanent highlight trace
    # (md_plot_state()$hi_idx). This avoids a full plot re-render on
    # every DT row click, which was the source of the lag on large data.
    observeEvent(sel_keys_rv(), {
      state <- md_plot_state()
      if (is.null(state) || is.null(state$df)) return()
      keys <- sel_keys_rv()
      df   <- state$df
      sel_df <- if (length(keys)) df[df$.key %in% keys, , drop = FALSE]
                else df[integer(0), , drop = FALSE]
      hi_text <- if (nrow(sel_df))
        paste0("id=", sel_df$id,
               "<br>m/z=", round(sel_df$mz, 4),
               "<br>OMD_mDa=", round(sel_df$OMD_mDa, 3))
      else character(0)
      plotly::plotlyProxyInvoke(
        plotly::plotlyProxy("md_network_plot", session),
        "restyle",
        list(x = list(sel_df$OMD_mDa),
             y = list(sel_df$mz),
             text = list(hi_text)),
        list(state$hi_idx)
      )
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    
    
    ##########################################################                
    
    # --- Sample comparison ---
    # Build the per-sample column selectors (Sample 1, Sample 2, ...) dynamically
    output$sample_slots_ui <- renderUI({
      req(raw_input(), input$n_samples)
      nms <- names(raw_input())
      mapped <- unique(c(input$map_mz, input$map_rt, input$map_intensity,
                         input$map_id, input$map_ccs))
      mapped <- mapped[nzchar(mapped) & mapped != "(none)"]
      numeric_cols <- nms[vapply(nms,
                                 function(cn) is.numeric(raw_input()[[cn]]),
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

    # Named vector: names = "Sample 1", "Sample 2", ...; values = chosen column names
    sample_map <- reactive({
      req(input$n_samples)
      n <- max(2, as.integer(input$n_samples))
      vals <- vapply(seq_len(n), function(i) {
        v <- input[[paste0("sample_col_", i)]]
        if (is.null(v)) NA_character_ else as.character(v)
      }, character(1))
      names(vals) <- paste0("Sample ", seq_len(n))
      vals[!is.na(vals) & vals != "(none)" & nzchar(vals)]
    })

    output$sample_ref_ui <- renderUI({
      req(length(sample_map()) > 0)
      selectInput(ns("sample_ref"), "Reference sample",
                  choices = names(sample_map()),
                  selected = names(sample_map())[1])
    })

    # Only rebuild the settings when the "Plot sample comparison" button is
    # pressed. Selection changes automatically refresh the plot.
    sample_settings <- eventReactive(input$go_sample_comp, {
      req(sample_map())
      showNotification("Building sample comparison...", type = "message",
                       duration = 2, id = ns("nn_sample_md"))
      list(sm = sample_map(),
           plot_type = input$sample_plot_type,
           ref = input$sample_ref)
    }, ignoreNULL = FALSE)

    output$sample_x_var_ui <- renderUI({
      # Prefer the mapped MD data; fall back to raw so the selector is
      # available even before Process is clicked.
      src <- if (!is.null(vals$m)) vals$m else raw_input()
      req(src)
      cols <- setdiff(names(src), ".key")
      sel  <- if ("mz" %in% cols) "mz"
              else if ("id" %in% cols) "id"
              else cols[1]
      selectInput(ns("sample_x_var"), "X variable",
                  choices = cols, selected = sel)
    })

    output$sample_comp_plot <- plotly::renderPlotly({
      req(input$enable_sample_comp, vals$m)
      s <- sample_settings()
      req(s, s$sm)
      keys <- selected_keys()
      df <- if (length(keys)) vals$m[vals$m$.key %in% keys, , drop = FALSE] else vals$m
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

    ##########################################################                
    
    })
}

# Helper for plotting within module
make_mdplot <- function(shared_data, xvar, yvar, xlabel, ylabel, intensity_enabled, intensity_col, series_sel, series_cols, show_legend, source_id) {
  m <- shared_data$data(withSelection = FALSE)
  # Robust intensity->size mapping. Guards:
  #  - column missing / non-numeric  -> constant size
  #  - all-NA or zero-range          -> constant size
  #  - individual NA/Inf values      -> replaced with median size
  # sizemode="diameter" + explicit sizemin so small points remain visible in scattergl.
  size_const <- 7
  point_sizes <- rep(size_const, nrow(m))
  use_size <- FALSE
  if (isTRUE(intensity_enabled) && !is.null(intensity_col) &&
      length(intensity_col) == 1 && intensity_col %in% names(m)) {
    v <- suppressWarnings(as.numeric(m[[intensity_col]]))
    finite_v <- v[is.finite(v)]
    if (length(finite_v) && diff(range(finite_v)) > 0) {
      rng <- range(finite_v)
      # Scale to a visible range (5..22 px diameter)
      sz <- 5 + 17 * (v - rng[1]) / diff(rng)
      med <- stats::median(sz, na.rm = TRUE)
      sz[!is.finite(sz)] <- med
      point_sizes <- sz
      use_size <- TRUE
    }
  }

  base_col <- if (!is.null(series_sel)) "rgba(60,60,60,0.18)" else "rgba(0, 114, 178, 0.75)"  # Okabe-Ito blue
  marker_spec <- list(size = point_sizes, color = base_col,
                      line = list(color = "rgba(0,0,0,0.35)", width = 0.4))
  if (use_size) {
    marker_spec$sizemode <- "diameter"
    marker_spec$sizemin  <- 3
  }
  fig <- plotly::plot_ly(shared_data,
                         x = as.formula(paste0("~", xvar)),
                         y = as.formula(paste0("~", yvar)),
                         key = ~.key, source = source_id,
                         type = "scattergl", mode = "markers",
                         marker = marker_spec,
                         name = "All features", showlegend = show_legend,
                         hovertemplate = paste0("<b>id</b>=%{customdata}<br>",
                                                xlabel, "=%{x:.4f}<br>",
                                                ylabel, "=%{y:.4f}<extra></extra>"),
                         customdata = ~id)

  if (!is.null(series_sel)) {
    for (sid in series_sel) {
      df_l <- m[!is.na(m$series_id) & m$series_id == sid, ]; df_l <- df_l[order(df_l$mz), ]
      fig <- fig %>% plotly::add_trace(
        data = df_l, x = df_l[[xvar]], y = df_l[[yvar]],
        type = "scatter", mode = "lines+markers",
        line   = list(color = series_cols[as.character(sid)], width = 2.5),
        marker = list(color = series_cols[as.character(sid)], size = 8,
                      line = list(color = "#222222", width = 0.6)),
        name = paste0("Series ", sid))
    }
  }
  fig %>%
    mzx_style(xtitle = xlabel, ytitle = ylabel, legend = show_legend) %>%
    plotly::highlight(on = "plotly_selected", off = "plotly_deselect",
                      color = I(mzx_highlight_color), opacityDim = 0.25)
}


