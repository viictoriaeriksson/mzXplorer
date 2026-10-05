#' Mass Defect Analysis Module UI
#' @param id module id
mass_defect_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        checkboxInput(ns("show_isf_tab"), "Show ISF analysis tab", FALSE),
        fileInput(ns('file1'), 'Choose Feature File (.csv / .xlsx / .xls)',
                  accept = c('.csv', '.xlsx', '.xls',
                             'text/csv', 'text/comma-separated-values,text/plain',
                             'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                             'application/vnd.ms-excel')),
        uiOutput(ns("file1_status")),

        # --- Shown on "Mass Defect" — 
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
                actionButton(ns('go'), 'Plot', width = "100%", class = "btn-primary"),
                tags$br(), tags$br(),
                tags$button("Reload session",
                            id = ns("reload_session_md"),
                            class = "btn btn-outline-danger btn-sm",
                            style = "width:100%;",
                            onclick = "window.location.reload(true); return false;",
                            title = "Force-reload the app if something is stuck. Discards all state."),
                tags$small(style = "color:#777;",
                           "Force-reloads the app if a run is stuck or unresponsive. All state (uploads, results, selections) will be discarded.")
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
              column(12, actionButton(ns("cancel_homol"), "Cancel",
                                     class = "btn-outline-warning btn-sm",
                                     width = "100%"))
            ),
            tags$small(style = "color:#777;",
                       "Cancel halts before the next processing phase.")
          )
        ),

        # --- Shown on "Molecular Composition" ---
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Molecular_Composition'", ns("result_sections")),
          div(class = "mzx-section mzx-molcomp",
            tags$h4("Molecular composition"),
            tags$small(style = "color:#555;",
                       "Calculate DBE, and H/C and O/C ratios, if chemical formulas are present in feature file, for DBE and Van Kreveln plots."),
            tags$br(), tags$br(),
            selectInput(ns("mc_ratio"), "Van Krevelen y-axis ratio",
                        choices  = c("H/C", "F/C", "Br/C", "Cl/C"),
                        selected = "H/C"),
            actionButton(ns("plot_dbe_vk"),
                         "Calculate DBE and Van Krevelen",
                         width = "100%",
                         style = "background-color:#b8377a; border-color:#b8377a; color:#fff; font-weight:600;"),
            tags$br(), tags$br(),
            uiOutput(ns("slide_nc")),
            uiOutput(ns("slide_dbe")),
            uiOutput(ns("slide_hc")),
            uiOutput(ns("slide_oc")),
            uiOutput(ns("mc_plot_btn"))
          )
        ),

        # --- Shown on "MD / m/z differences" ---
        conditionalPanel(
          condition = sprintf("input['%s'] == 'MD_mz_differences'", ns("result_sections")),
          div(class = "mzx-section mzx-mdiff",
            tags$h4("MD / m/z differences"),
            textInput(ns("md_values"), "Mass Defect Differences, mDa (integers, comma-separated)",
                      value = "-5, -16, 32",
                      placeholder = "e.g., -5, -16, -24, 32, -43, 68"),
            textInput(ns("mz_values"), "m/z Differences, Da (comma-separated)",
                      value = "15.9949, -2.0157, 176.0321",
                      placeholder = "e.g., 15.9949, -2.0157, -15.0235, 176.0321, 79.9568, 305.0682"),
            numericInput(ns("MZtolerance"), "m/z difference tolerance (Da)", value = 0.01, min = 0, step = 0.01),
            actionButton(ns("MDcalculate"), "Calculate MD/mz Differences", width = "100%", class = "btn-warning")
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
        tabsetPanel(
          id = ns("result_sections"),
          type = "tabs",
          tabPanel(
            "Mass Defect", value = "Plot_Table",
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
            "Molecular Composition", value = "Molecular_Composition",
            div(class = "mzx-pane mzx-molcomp",
              tags$h4("Molecular composition: DBE and Van Krevelen"),
              tags$small(style = "color:#555;",
                         ""),
              tags$br(), tags$br(),
              uiOutput(ns("mc_filter_ui")),
              fluidRow(
                column(6, plotly::plotlyOutput(ns("dbe_plot"), height = "450px")),
                column(6, plotly::plotlyOutput(ns("vk_plot"),  height = "450px"))
              )
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
    # not NA, and (for numeric columns) strictly greater than zero. 
    .apply_col_filter <- function(df, col) {
      if (is.null(df) || !nrow(df)) return(df)
      if (is.null(col) || !length(col) || col %in% c("", "(none)") ||
          !col %in% names(df)) return(df)
      v <- df[[col]]
      keep <- !is.na(v)
      if (is.numeric(v)) keep <- keep & v > 0
      df[keep, , drop = FALSE]
    }
    

    # `raw_input` reads the CSV once. Column mapping (user-selectable) is then
    # applied in `raw_file()` so users can point their columns at the internal
    # targets (mz, rt, intensity, id, ccs).
    raw_input <- reactive({
      req(input$file1)
      withProgress(message = "Loading feature list...", value = 0, {
        df <- read_feature_file(input$file1$datapath,
                                orig_name = input$file1$name)
        validate(need(!is.null(df) && ncol(df) > 0,
                      "Could not read the file. Please provide a CSV or XLSX text file."))
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
                    selected = if (!is.na(defaults$ccs))       defaults$ccs       else "(none)"),
        selectInput(ns("map_formulas"),  "Chemical formula column (optional)", choices = choices,
                    selected = if (!is.na(defaults$formulas))  defaults$formulas  else "(none)")
      )
    })


    # Show/hide the ISF navbar tab based on the checkbox.
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
    # auto-guessed default based on column names. 
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
        ccs       = pick(input$map_ccs,       g$ccs),
        formulas  = pick(input$map_formulas,  g$formulas)
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

      df$RMD_ppm <- round((df$mz - round(df$mz)) / df$mz * 1e6)
      #   - the OMD column shown in every table (no client-side rounding)
      #   - the integer-mDa matching in the MD/m/z difference algorithm
      raw_omd <- (df$mz - round(df$mz)) * 1e3
      df$OMD_mDa <- as.integer(sign(raw_omd) * floor(abs(raw_omd) + 0.5))
      df
    })
    
    MD_data_raw <- eventReactive(input$go_process, {
      df <- raw_file()
      withProgress(message = "Applying MD formulas...", value = 0.3, {
        mdh1 <- getmdh(df$mz, cus = input$cus1, method = input$rounding)
        mdh2 <- getmdh(df$mz, cus = input$cus2, method = input$rounding)

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
                           "No CCS or ion mobility column mapped; homologue detection will use RT only."))
      } else {
        tagList(
          checkboxInput(ns("use_ccs_toggle"),
                        "Enable CCS / ion mobility drift time homologue rules",
                        value = FALSE),
          uiOutput(ns("ccs_mode_ui")),
          uiOutput(ns("ccs_tol_ui"))
        )
      }
    })

    output$ccs_mode_ui <- renderUI({
      req(has_ccs(), isTRUE(input$use_ccs_toggle))
      radioButtons(ns("ccs_mode"),
                   "Use which dimension for homologue trend?",
                   choices = c("RT only"                       = "rt",
                               "CCS / ion mobility only"       = "ccs",
                               "Both RT + CCS / ion mobility"  = "both"),
                   selected = "ccs")
    })

    output$ccs_tol_ui <- renderUI({
      req(has_ccs(), isTRUE(input$use_ccs_toggle))
      numericInput(ns("homol_ccstol"),
                   "Per-step CCS / ion mobility tolerance (same units as the mapped column); 0 = off",
                   value = 0, min = 0, step = 0.1)
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
          # Read (NOT isolate) the current lasso selection so the plot
          # redraws its overlay whenever the selection changes. This makes
          # a new lasso in a different sample REPLACE the previous overlay.
          sel_snapshot <- sel_keys_rv()
          ins_flag <- isolate(input$ins)
          i_col    <- isolate(input$selectintensity)
          if (is.null(i_col) || !nzchar(i_col) || !(i_col %in% names(m_f))) {
            i_col <- if ("intensity" %in% names(m_f)) "intensity" else NULL
          }
          make_mdplot(d_f, input$xvar1, input$yvar1, input$xvar1, input$yvar1,
                      ins_flag, i_col,
                      vals$series_sel, vals$series_cols, input$show_leg, ns("plot1"),
                      sel_keys = sel_snapshot)
        })
        output$DTPlot2 <- plotly::renderPlotly({
          req(vals$m)
          m_f <- .apply_col_filter(vals$m, input$filter_col)
          if (!nrow(m_f)) return(plot_ly() %>%
                                    layout(title = "No features with data in chosen column"))
          d_f <- crosstalk::SharedData$new(as.data.frame(m_f),
                                           key = ~.key, group = ns("md"))
          sel_snapshot <- sel_keys_rv()
          ins_flag <- isolate(input$ins)
          i_col    <- isolate(input$selectintensity)
          if (is.null(i_col) || !nzchar(i_col) || !(i_col %in% names(m_f))) {
            i_col <- if ("intensity" %in% names(m_f)) "intensity" else NULL
          }
          make_mdplot(d_f, input$xvar2, input$yvar2, input$xvar2, input$yvar2,
                      ins_flag, i_col,
                      vals$series_sel, vals$series_cols, input$show_leg, ns("plot2"),
                      sel_keys = sel_snapshot)
        })
      })
    })

    # --- Cancel flag for homologue search. 
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
      cancel_homol(FALSE)

      withProgress(message = "Homologue search", value = 0, {
        # Save the full data before find_homologues (which re-orders rows).
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

        # Ffall back to RT-only whenever CCS is unavailable or the toggle is off.
        ccs_mode_sel <- if (!isTRUE(has_ccs()) || !isTRUE(input$use_ccs_toggle)) {
          "rt"
        } else if (is.null(input$ccs_mode)) {
          "rt"
        } else {
          input$ccs_mode
        }
        ccs_tol_sel <- if (!isTRUE(has_ccs()) || !isTRUE(input$use_ccs_toggle)) {
          0
        } else {
          v <- input$homol_ccstol
          if (is.null(v) || is.na(v)) 0 else v
        }

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
                            ccs_mode = ccs_mode_sel,
                            ccs_tol  = ccs_tol_sel),
            error = function(e) {
              showNotification(sprintf("Unit '%s' failed: %s", u, conditionMessage(e)),
                               type = "error", duration = 8)
              NULL
            }
          )
          if (is.null(res) || !"id" %in% names(res) || !"series_id" %in% names(res)) next

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
    # Reactive-value backed selection so it can be reset
    sel_keys_rv <- reactiveVal(character(0))
    sel_gen_md  <- reactiveVal(0L)  
    feature_table_view <- reactiveVal(NULL)
    md_plot_state <- reactiveVal(NULL)
    sel_source <- reactiveVal("init")

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
      sel_source("plot")
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

    observeEvent(input$reset_sel_mdnet, {
      sel_source("plot")
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
    
    table_selected_view <- reactiveVal(NULL)

    md_plot_sel_snapshot <- reactiveVal(list(keys = character(0), src = "init"))
    observeEvent(sel_keys_rv(), {
      if (!identical(sel_source(), "table")) {
        md_plot_sel_snapshot(list(keys = sel_keys_rv(), src = sel_source()))
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    
    ft_plot_sel_snapshot <- reactiveVal(list(keys = character(0), src = "init"))
    observeEvent(sel_keys_rv(), {
      if (!identical(sel_source(), "table")) {
        ft_plot_sel_snapshot(list(keys = sel_keys_rv(), src = sel_source()))
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    output$table_selected <- DT::renderDT({
      req(vals$m)
      snap <- md_plot_sel_snapshot()
      sel_keys <- snap$keys
      src      <- snap$src
      df_all <- as.data.frame(vals$m)


      if (!is.null(vals$series_keys) && length(vals$series_keys)) {
        df <- df_all[df_all$.key %in% vals$series_keys, , drop = FALSE]
      } else if (length(sel_keys) && identical(src, "plot")) {
        df <- df_all[df_all$.key %in% sel_keys, , drop = FALSE]
      } else {
        df <- df_all
      }

      # Reorder columns: identity first, then MD-derived columns,
      # then homologue/CCS-related, then everything else. 
      for (sc in c("series_id", "series_unit")) {
        if (sc %in% names(df) && all(is.na(df[[sc]]))) df[[sc]] <- NULL
      }
      nms <- setdiff(names(df), ".key")
      identity_cols <- intersect(c("id", "mz", "rt", "intensity"), nms)
      md_cols       <- intersect(c("OMD_mDa", "RMD_ppm"), nms)
      formula_cols  <- grep("^Formula[0-9]+_", nms, value = TRUE)
      series_cols_v <- intersect(c("series_id", "series_unit", "ccs"), nms)
      leading       <- unique(c(identity_cols, md_cols, formula_cols, series_cols_v))
      trailing      <- setdiff(nms, leading)
      df <- df[, c(".key", leading, trailing), drop = FALSE]

      table_selected_view(df)

      pre_sel <- if (length(sel_keys))
        which(as.character(df$.key) %in% sel_keys)
      else
        integer(0)

      key_idx <- match(".key", names(df))
      dt <- DT::datatable(df,
                          editable = TRUE, rownames = FALSE, filter = "top",
                          selection = list(mode = "multiple",
                                           selected = pre_sel),
                          options = list(scrollX = TRUE,
                                         pageLength = 15,
                                         deferRender = TRUE,
                                         processing  = TRUE,
                                         columnDefs = list(list(visible = FALSE,
                                                                targets = key_idx - 1L))))
      dt
    })

    observeEvent(input$table_selected_rows_selected, {
      view <- table_selected_view()
      if (is.null(view) || !nrow(view)) return()
      rows <- input$table_selected_rows_selected
      new_keys <- if (length(rows)) unique(as.character(view$.key[rows]))
                  else character(0)
      if (!setequal(new_keys, sel_keys_rv())) {
        sel_source("table")
        sel_keys_rv(new_keys)
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    output$barplot <- plotly::renderPlotly({
      req(vals$m); sel_keys <- selected_keys()
      if (!length(sel_keys)) return(NULL)
      df <- vals$m[vals$m$.key %in% sel_keys, ]
      if (!nrow(df)) return(NULL)
      
      selvar <- input$selectintensity
      if (is.null(selvar) || !nzchar(selvar) || !(selvar %in% names(df)) ||
          !is.numeric(df[[selvar]])) {
        selvar <- if ("intensity" %in% names(df) && is.numeric(df[["intensity"]])) {
          "intensity"
        } else {
          num_nm <- names(df)[vapply(df, is.numeric, logical(1))]
          num_nm <- setdiff(num_nm, c("id", "series_id"))
          if (length(num_nm)) num_nm[1] else NULL
        }
      }
      if (is.null(selvar)) return(NULL)
      yv  <- suppressWarnings(as.numeric(df[[selvar]]))
      mx  <- suppressWarnings(max(yv, na.rm = TRUE))
      if (!is.finite(mx) || mx <= 0) return(NULL)
      rel_y <- yv / mx * 100
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
      empty <- data.frame(series_id = integer(0), unit = character(0), n = integer(0),
                          mz_min = numeric(0), mz_max = numeric(0),
                          rt_min = numeric(0), rt_max = numeric(0),
                          int_sum = numeric(0),
                          ccs_min = numeric(0), ccs_max = numeric(0),
                          ccs_range = numeric(0))
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
      has_ccs <- "ccs" %in% names(ms) && any(is.finite(ms$ccs))
      summ <- ms %>% group_by(series_id) %>% summarize(
        unit = if ("series_unit" %in% names(ms)) dplyr::first(stats::na.omit(series_unit)) else NA_character_,
        n = n(),
        mz_min  = min(mz, na.rm = TRUE), mz_max  = max(mz, na.rm = TRUE),
        rt_min  = min(rt, na.rm = TRUE), rt_max  = max(rt, na.rm = TRUE),
        int_sum = sum(intensity, na.rm = TRUE),
        ccs_min = if (has_ccs) suppressWarnings(min(ccs, na.rm = TRUE)) else NA_real_,
        ccs_max = if (has_ccs) suppressWarnings(max(ccs, na.rm = TRUE)) else NA_real_,
        .groups = "drop"
      )
      summ$ccs_range <- round(summ$ccs_max - summ$ccs_min, 1)
      base_cols <- c("series_id", "unit", "n",
                     "mz_min", "mz_max", "rt_min", "rt_max", "int_sum")
      if (has_ccs) {
        summ <- summ[, c(base_cols, "ccs_min", "ccs_max", "ccs_range"), drop = FALSE]
      } else {
        summ <- summ[, base_cols, drop = FALSE]
      }
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

      DT::selectRows(DT::dataTableProxy("homol_table", session = session), NULL)
      showNotification("Cleared series highlight.", type = "message",
                       duration = 2, id = ns("nn_homol_clear"))
    })
    
    output$x3 <- downloadHandler(filename = "MD_export.csv", content = function(f) { 
      sel <- selected_keys(); out <- if(length(sel)) vals$m[vals$m$.key %in% sel, ] else vals$m
      data.table::fwrite(out, f) 
    })
  
    
    
    ##########################################################
    MD_data_annotated <- eventReactive(input$MDcalculate, {
            req(MD_data_raw())
            df <- as.data.frame(MD_data_raw())
            
            md_diffs <- suppressWarnings(as.numeric(unlist(strsplit(gsub(" ", "", input$md_values), ","))))
            mz_diffs <- suppressWarnings(as.numeric(unlist(strsplit(gsub(" ", "", input$mz_values), ","))))

            if (anyNA(md_diffs) || anyNA(mz_diffs)) {
                    showNotification("Error parsing MD/mz differences. Use comma-separated numbers.", type = "error")
                    return(df)
            }
            md_diffs <- round(md_diffs)
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
            ids    <- as.character(df$id)
            omd_int <- as.integer(df$OMD_mDa)
            mz_val  <- as.numeric(df$mz)
            by_omd <- split(seq_len(n), omd_int)

            withProgress(message = "Calculating MD + m/z differences...", value = 0, {
                    for (idx in seq_along(md_diffs)) {
                            target_md <- as.integer(md_diffs[idx])
                            target_mz <- as.numeric(mz_diffs[idx])

                            out_vec <- character(n)
                            for (i in seq_len(n)) {
                                    # Only features whose integer OMD is
                                    # EXACTLY omd_int[i] + target_md are considered.
                                    key <- as.character(omd_int[i] + target_md)
                                    cand <- by_omd[[key]]
                                    if (is.null(cand)) next
                                    cand <- cand[cand != i]
                                    if (!length(cand)) next
                                    dmz  <- mz_val[cand] - mz_val[i]
                                    keep <- abs(dmz - target_mz) <= da_tol
                                    if (any(keep)) {
                                            out_vec[i] <- paste(ids[cand[keep]],
                                                                collapse = ", ")
                                    }
                            }

                            col_name <- paste0("MD=", target_md,
                                               "_mz=", round(target_mz, 4),
                                               " -> id")
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
      sel_keys <- sel_keys_rv()
      has_sel  <- length(sel_keys) > 0L
      dim_op   <- if (has_sel) 0.3 else 1
      withProgress(message = "Rendering MD network...", value = 0, {
      df_full <- as.data.frame(MD_data_annotated())

      key_map <- as.data.frame(vals$m)[, c("id", ".key")]
      df <- merge(df_full, key_map, by = "id", all = FALSE)
      if (!nrow(df)) return(plot_ly() %>% layout(title = "No overlapping rows to display"))

      df <- .apply_col_filter(df, input$filter_col)
      if (!nrow(df)) return(plot_ly() %>%
                              layout(title = "No features with data in chosen column"))

      diff_cols <- grep(" -> id$", names(df), value = TRUE)

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

      if (length(diff_cols) == 0) {
        is_sel_all <- as.character(df$.key) %in% as.character(sel_keys)
        un_df <- df[!is_sel_all, , drop = FALSE]
        se_df <- df[ is_sel_all, , drop = FALSE]
        sd_un <- crosstalk::SharedData$new(un_df, key = ~.key, group = ns("md"))
        p_empty <- plot_ly(source = ns("mdnet")) %>%
          add_markers(data = sd_un, x = ~OMD_mDa, y = ~mz,
                      marker = list(color = "rgba(0, 114, 178, 0.55)", size = 7,
                                    opacity = dim_op,
                                    line = list(color = "rgba(0,0,0,0.35)", width = 0.4)),
                      hovertemplate = "id=%{customdata}<br>OMD=%{x}<br>m/z=%{y:.4f}<extra></extra>",
                      customdata = ~id, name = "features") %>%
          add_markers(x = se_df$OMD_mDa, y = se_df$mz,
                      name = "Selected",
                      marker = list(color = mzx_highlight_color, size = 8, opacity = 0.95,
                                    line = list(color = "#8a7a00", width = 1.2)),
                      hovertemplate = "id=%{customdata}<br>OMD=%{x}<br>m/z=%{y:.4f}<extra></extra>",
                      customdata = if (nrow(se_df)) se_df$id else integer(0),
                      inherit = FALSE, showlegend = has_sel) %>%
          mzx_style(xtitle = "OMD (mDa)", ytitle = "m/z",
                    title = "No MD + m/z differences calculated yet") %>%
          plotly::layout(dragmode = "select")
        return(p_empty)
      }

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

      skey    <- as.character(sel_keys)
      no_sel  <- as.character(df_no_match$.key)   %in% skey
      yes_sel <- as.character(df_with_match$.key) %in% skey
      un_no   <- df_no_match  [!no_sel , , drop = FALSE]
      un_yes  <- df_with_match[!yes_sel, , drop = FALSE]
      se_df   <- rbind(df_no_match  [ no_sel , , drop = FALSE],
                       df_with_match[ yes_sel, , drop = FALSE])

      sd_no  <- crosstalk::SharedData$new(un_no,  key = ~.key, group = ns("md"))
      sd_yes <- crosstalk::SharedData$new(un_yes, key = ~.key, group = ns("md"))

      p <- plot_ly(source = ns("mdnet")) %>%
        add_trace(type = "scattergl", mode = "lines",
                  x = lx, y = ly,
                  line = list(color = "rgba(120,120,120,0.35)", width = 1),
                  showlegend = FALSE, hoverinfo = "skip") %>%
        add_markers(data = sd_no,  x = ~OMD_mDa, y = ~mz, name = "No match",
                    type = "scattergl",
                    marker = list(color = "rgba(86, 180, 233, 0.55)", size = 7,
                                  opacity = dim_op,
                                  line = list(color = "rgba(0,0,0,0.35)", width = 0.4)),
                    hovertemplate = "id=%{customdata}<br>OMD=%{x}<br>m/z=%{y:.4f}<extra></extra>",
                    customdata = ~id) %>%
        add_markers(data = sd_yes, x = ~OMD_mDa, y = ~mz, name = "Has match",
                    type = "scattergl",
                    marker = list(color = "rgba(213, 94, 0, 0.85)", size = 7,
                                  opacity = dim_op,
                                  line = list(color = "#7a2f00", width = 0.5)),
                    hovertemplate = "id=%{customdata}<br>OMD=%{x}<br>m/z=%{y:.4f}<extra></extra>",
                    customdata = ~id) %>%
        add_markers(x = se_df$OMD_mDa, y = se_df$mz,
                    name = "Selected",
                    type = "scatter", mode = "markers",
                    marker = list(color = mzx_highlight_color, size = 8, opacity = 0.95,
                                  line = list(color = "#8a7a00", width = 1.2)),
                    hovertemplate = "id=%{customdata}<br>OMD=%{x}<br>m/z=%{y:.4f}<extra></extra>",
                    customdata = if (nrow(se_df)) se_df$id else integer(0),
                    inherit = FALSE, showlegend = has_sel) %>%
        mzx_style(xtitle = "OMD (mDa)", ytitle = "m/z",
                  title = "MD + m/z difference network") %>%
        plotly::layout(dragmode = "select")
      p
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

            if (!is.null(vals$m) && "id" %in% names(df)) {
                    key_map <- as.data.frame(vals$m)[, c("id", ".key"), drop = FALSE]
                    df <- merge(df, key_map, by = "id", all.x = TRUE, sort = FALSE)
            }

            ft_snap <- ft_plot_sel_snapshot()
            sel_now <- ft_snap$keys
            plot_driven <- length(sel_now) > 0 && identical(ft_snap$src, "plot")
            if (plot_driven && ".key" %in% names(df)) {
              df <- df[df$.key %in% sel_now, , drop = FALSE]
            }

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

            isolate(feature_table_view(df))

            sel_arg <- if (plot_driven)
              list(mode = "multiple", selected = seq_len(nrow(df)))
            else "multiple"

            dt <- datatable(df[, display_cols, drop = FALSE],
                      options = list(pageLength = 10, scrollX = TRUE),
                      rownames = FALSE,
                      selection = sel_arg) %>%
                    formatStyle(
                            columns = diff_cols,
                            backgroundColor = styleEqual("", "white", default = "#e8f5e9")
                    )
            dt
    })

    observeEvent(input$feature_table_rows_selected, {
      view <- feature_table_view()
      if (is.null(view) || !".key" %in% names(view)) return()
      idx  <- input$feature_table_rows_selected
      keys <- if (length(idx)) unique(as.character(view$.key[idx])) else character(0)
      keys <- keys[!is.na(keys)]
      if (identical(sel_source(), "plot") && !length(keys)) return()
      if (!setequal(keys, sel_keys_rv())) {
        sel_source("table")
        sel_keys_rv(keys)
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    observeEvent(sel_keys_rv(), {
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


    ##########################################################                
    
    # --- Sample comparison ---
    # Build the per-sample column selectors (Sample 1, Sample 2, ...) dynamically
    output$sample_slots_ui <- renderUI({
      req(raw_input(), input$n_samples)
      nms <- names(raw_input())
      mapped <- unique(c(input$map_mz, input$map_rt, input$map_intensity,
                         input$map_id, input$map_ccs, input$map_formulas))
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

    sample_settings <- eventReactive(input$go_sample_comp, {
      req(sample_map())
      showNotification("Building sample comparison...", type = "message",
                       duration = 2, id = ns("nn_sample_md"))
      list(sm = sample_map(),
           plot_type = input$sample_plot_type,
           ref = input$sample_ref)
    }, ignoreNULL = FALSE)

    output$sample_x_var_ui <- renderUI({
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
      req(vals$m)
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

    # ---- Molecular Composition tab (DBE + Van Krevelen) ----
    molcomp_ready <- reactiveVal(FALSE)

    observeEvent(input$plot_dbe_vk, {
      if (is.null(vals$m)) {
        showNotification("Press 'Plot' on the Mass Defect subtab first, then compute molecular composition.",
                         type = "warning"); return()
      }
      m <- vals$m
      if (!"formulas" %in% names(m)) {
        showNotification("No Chemical formula column mapped. Map one in Column Mapping (Mass Defect tab), then press Process + Plot.",
                         type = "error"); return()
      }
      comp <- compute_molcomp(as.character(m$formulas))
      m$DBE      <- comp$DBE
      m$HC_ratio <- comp$HC_ratio
      m$OC_ratio <- comp$OC_ratio
      m$n_C      <- comp$C
      rsel <- if (is.null(input$mc_ratio)) "H/C" else input$mc_ratio
      num_den <- strsplit(rsel, "/", fixed = TRUE)[[1]]
      num <- comp[[num_den[1]]]
      den <- comp[[num_den[2]]]
      m$VK_ratio <- ifelse(is.finite(den) & den > 0, num / den, NA_real_)
      vals$mc_ratio_label <- rsel
      vals$m <- m
      molcomp_ready(TRUE)
      showNotification(sprintf("Molecular composition computed for %d features (Van Krevelen y-axis: %s).",
                               sum(!is.na(comp$DBE)), rsel), type = "message")
    })

    observeEvent(input$go_process, { molcomp_ready(FALSE); mc_plot_data(NULL) }, ignoreInit = TRUE)

    output$mc_filter_ui <- renderUI({
      req(molcomp_ready(), vals$m)
      fluidRow(
        column(6, selectInput(ns("mc_filter_col"),
                              "Filter by sample (select sample column). Filter applies to both plots.",
                              choices = c("(none)", names(vals$m)), selected = "(none)"))
      )
    })
    output$slide_nc <- renderUI({
      req(molcomp_ready(), vals$m); v <- vals$m$n_C
      v <- v[is.finite(v)]; if (!length(v)) return(NULL)
      sliderInput(ns("mc_nc"), "Number of Carbons range",
                  min = min(v), max = max(v), value = range(v), step = 1)
    })
    output$slide_dbe <- renderUI({
      req(molcomp_ready(), vals$m); v <- vals$m$DBE
      v <- v[is.finite(v)]; if (!length(v)) return(NULL)
      sliderInput(ns("mc_dbe"), "DBE range",
                  min = floor(min(v)), max = ceiling(max(v)),
                  value = c(floor(min(v)), ceiling(max(v))), step = 0.5)
    })
    output$slide_hc <- renderUI({
      req(molcomp_ready(), vals$m); v <- vals$m$VK_ratio
      v <- v[is.finite(v)]; if (!length(v)) return(NULL)
      lbl <- paste0(vals$mc_ratio_label, " ratio range")
      hi  <- ceiling(max(v) * 10) / 10
      if (!is.finite(hi) || hi <= 0) hi <- 1
      sliderInput(ns("mc_hc"), lbl, min = 0, max = hi,
                  value = c(0, hi), step = 0.05)
    })
    output$slide_oc <- renderUI({
      req(molcomp_ready(), vals$m); v <- vals$m$OC_ratio
      v <- v[is.finite(v)]; if (!length(v)) return(NULL)
      sliderInput(ns("mc_oc"), "O/C ratio range",
                  min = 0, max = ceiling(max(v) * 10) / 10,
                  value = c(0, ceiling(max(v) * 10) / 10), step = 0.05)
    })

    output$mc_plot_btn <- renderUI({
      req(molcomp_ready())
      actionButton(ns("mc_plot"), "Plot", width = "100%",
                   style = "background-color:#b8377a; border-color:#b8377a; color:#fff; font-weight:600;")
    })

    mc_plot_data <- reactiveVal(NULL)

    observeEvent(input$mc_plot, {
      req(molcomp_ready(), vals$m)
      d <- as.data.frame(.apply_col_filter(vals$m, input$mc_filter_col))
      d <- d[!is.na(d$DBE) & !is.na(d$VK_ratio) & !is.na(d$OC_ratio) &
             is.finite(d$n_C), , drop = FALSE]
      if (!is.null(input$mc_nc))
        d <- d[d$n_C >= input$mc_nc[1] & d$n_C <= input$mc_nc[2], ]
      if (!is.null(input$mc_dbe))
        d <- d[d$DBE >= input$mc_dbe[1] & d$DBE <= input$mc_dbe[2], ]
      if (!is.null(input$mc_hc))
        d <- d[d$VK_ratio >= input$mc_hc[1] & d$VK_ratio <= input$mc_hc[2], ]
      if (!is.null(input$mc_oc))
        d <- d[d$OC_ratio >= input$mc_oc[1] & d$OC_ratio <= input$mc_oc[2], ]
      mc_plot_data(d)
    })

    output$dbe_plot <- plotly::renderPlotly({
      d <- mc_plot_data()
      req(!is.null(d))
      if (!nrow(d)) return(plotly::plot_ly() %>%
                             plotly::layout(title = "No features in current ranges."))
      sd <- crosstalk::SharedData$new(d, key = ~.key, group = ns("md"))
      make_mdplot(sd, "n_C", "DBE",
                  "Number of Carbons", "DBE",
                  intensity_enabled = FALSE, intensity_col = NULL,
                  series_sel = NULL, series_cols = NULL,
                  show_legend = isTRUE(input$show_leg),
                  source_id = ns("dbe"),
                  sel_keys = sel_keys_rv()) %>%
        plotly::layout(title = list(text = "DBE"))
    })

    output$vk_plot <- plotly::renderPlotly({
      d <- mc_plot_data()
      req(!is.null(d))
      if (!nrow(d)) return(plotly::plot_ly() %>%
                             plotly::layout(title = "No features in current ranges."))
      sd <- crosstalk::SharedData$new(d, key = ~.key, group = ns("md"))
      ylab <- isolate(vals$mc_ratio_label)
      if (is.null(ylab)) ylab <- "H/C"
      make_mdplot(sd, "OC_ratio", "VK_ratio",
                  "O / C", ylab,
                  intensity_enabled = FALSE, intensity_col = NULL,
                  series_sel = NULL, series_cols = NULL,
                  show_legend = isTRUE(input$show_leg),
                  source_id = ns("vk"),
                  sel_keys = sel_keys_rv()) %>%
        plotly::layout(title = list(text = "Van Krevelen"))
    })

    observeEvent(plotly::event_data("plotly_selected", source = ns("dbe")),
                 .apply_sel(plotly::event_data("plotly_selected", source = ns("dbe"))),
                 ignoreNULL = FALSE, ignoreInit = TRUE)
    observeEvent(plotly::event_data("plotly_selected", source = ns("vk")),
                 .apply_sel(plotly::event_data("plotly_selected", source = ns("vk"))),
                 ignoreNULL = FALSE, ignoreInit = TRUE)

    })
}


make_mdplot <- function(shared_data, xvar, yvar, xlabel, ylabel, intensity_enabled, intensity_col, series_sel, series_cols, show_legend, source_id, sel_keys = NULL) {
  m <- shared_data$data(withSelection = FALSE)
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

  if (!is.null(sel_keys) && length(sel_keys) && ".key" %in% names(m)) {
    is_sel <- as.character(m$.key) %in% as.character(sel_keys)
  } else {
    is_sel <- rep(FALSE, nrow(m))
  }
  
  has_selection <- any(is_sel)
  base_col_eff <- if (has_selection) {
    if (!is.null(series_sel)) "rgba(60,60,60,0.10)" else "rgba(0, 114, 178, 0.20)"
  } else base_col
  base_line_col_eff <- if (has_selection) "rgba(0,0,0,0.15)" else "rgba(0,0,0,0.35)"
  df_unsel <- m[!is_sel, , drop = FALSE]
  df_sel   <- m[ is_sel, , drop = FALSE]
  sizes_unsel <- point_sizes[!is_sel]
  sizes_sel   <- point_sizes[ is_sel]

  marker_unsel <- list(size = sizes_unsel, color = base_col_eff,
                       line = list(color = base_line_col_eff, width = 0.4))
  marker_sel   <- list(size = sizes_sel,   color = mzx_highlight_color,
                       line = list(color = "#8a7a00", width = 1.2))
  if (use_size) {
    marker_unsel$sizemode <- "diameter"; marker_unsel$sizemin <- 3
    marker_sel$sizemode   <- "diameter"; marker_sel$sizemin   <- 3
  }

  fig <- plotly::plot_ly(source = source_id)
  if (nrow(df_unsel)) {
    sd_unsel <- crosstalk::SharedData$new(df_unsel, key = ~.key,
                                          group = shared_data$groupName())
    fig <- fig %>% plotly::add_markers(
      data = sd_unsel,
      type = "scattergl",
      x = as.formula(paste0("~", xvar)),
      y = as.formula(paste0("~", yvar)),
      key = ~.key,
      marker = marker_unsel, name = "All features", showlegend = show_legend,
      hovertemplate = paste0("<b>id</b>=%{customdata}<br>",
                             xlabel, "=%{x:.4f}<br>",
                             ylabel, "=%{y:.4f}<extra></extra>"),
      customdata = ~id)
  }
  if (nrow(df_sel)) {
    fig <- fig %>% plotly::add_markers(
      data = df_sel,
      type = "scattergl",
      x = df_sel[[xvar]], y = df_sel[[yvar]],
      marker = marker_sel, name = "Selected", showlegend = show_legend,
      hovertemplate = paste0("<b>id</b>=%{customdata}<br>",
                             xlabel, "=%{x:.4f}<br>",
                             ylabel, "=%{y:.4f}<extra></extra>"),
      customdata = df_sel$id, inherit = FALSE)
  }

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
    plotly::layout(dragmode = "select")
}


