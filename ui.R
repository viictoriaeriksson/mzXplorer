# =========================================================
# mzXplorer: UI entry point
# =========================================================

# Section palette (kept in one place so sidebar panels and main-panel
# backgrounds stay in sync). Each entry: (background, border/accent).
.mzx_section_css <- HTML("
  /* Common section wrapper */
  .mzx-section {
    padding: 12px 14px;
    margin: 10px 0 18px 0;
    border-radius: 6px;
    border-left: 4px solid #999;
  }
  .mzx-section > h4, .mzx-section > h5 { margin-top: 0; }

  /* Column mapping (neutral gray to blend with app chrome) */
  .mzx-cmap    { background-color: #f0f0f4; border-left-color: #5a5a70; }
  /* Mass Defect formulas / main plots (blue) */
  .mzx-plot    { background-color: #eef7ff; border-left-color: #1f77b4; }
  /* Homologue search (purple) */
  .mzx-homol   { background-color: #f5efff; border-left-color: #7b3fbf; }
  /* MD / m/z differences (orange) */
  .mzx-mdiff   { background-color: #fff5e6; border-left-color: #e07b00; }
  /* ISF (teal) */
  .mzx-isf     { background-color: #e7f6f5; border-left-color: #2c8a86; }
  /* Sample comparison (green) */
  .mzx-sample  { background-color: #eafbe7; border-left-color: #2e8b57; }
  /* Export / full table (soft yellow) */
  .mzx-export  { background-color: #fffbe6; border-left-color: #c9a227; }

  /* --- Tab-pane variants (mainPanel subtabs) ---
     Mirrors the .mzx-section palette for tab content panes.
     The tab-pane wrapper gets the background + left border accent.    */
  .mzx-pane {
    padding: 12px 14px;
    border-radius: 6px;
    border-left: 4px solid #999;
  }
  .mzx-pane > h4, .mzx-pane > h5 { margin-top: 0; }

  /* Color variants (identical palette to .mzx-section) */
  .mzx-pane.mzx-plot    { background-color: #eef7ff; border-left-color: #1f77b4; }
  .mzx-pane.mzx-homol   { background-color: #f5efff; border-left-color: #7b3fbf; }
  .mzx-pane.mzx-mdiff   { background-color: #fff5e6; border-left-color: #e07b00; }
  .mzx-pane.mzx-isf     { background-color: #e7f6f5; border-left-color: #2c8a86; }
  .mzx-pane.mzx-sample  { background-color: #eafbe7; border-left-color: #2e8b57; }
  .mzx-pane.mzx-export  { background-color: #fffbe6; border-left-color: #c9a227; }

  /* DataTables & plotly overrides inside tab panes */
  .mzx-pane .dataTables_wrapper { background: transparent; }
  .mzx-pane table.dataTable,
  .mzx-pane table.dataTable tbody,
  .mzx-pane table.dataTable tbody tr,
  .mzx-pane table.dataTable tbody td,
  .mzx-pane table.dataTable thead th { background-color: #ffffff !important; }
  .mzx-pane .plotly.html-widget { background: #ffffff; border-radius: 4px; }

  /* Subtle tab styling so tab headers don't clash with the content */
  .nav-tabs .nav-link { color: #555; }
  .nav-tabs .nav-link.active { font-weight: 600; }

  /* Make DataTables & plotly widgets breathe inside a section */
  .mzx-section .dataTables_wrapper { background: transparent; }
  .mzx-section table.dataTable,
  .mzx-section table.dataTable tbody,
  .mzx-section table.dataTable tbody tr,
  .mzx-section table.dataTable tbody td,
  .mzx-section table.dataTable thead th { background-color: #ffffff !important; }
  .mzx-section .plotly.html-widget { background: #ffffff; border-radius: 4px; }

  /* Selectize: keep the widget layout untouched (selectize.js is picky
     about container geometry). Only relax the SELECTED chip so long
     sample-column names wrap onto a second line instead of getting
     ellipsis'd; dropdown options stay single-line + ellipsis.        */
  .selectize-control { max-width: 100%; }
  .selectize-input .item {
    max-width: 100%;
    white-space: normal;
    word-break: break-word;
  }
  .selectize-dropdown, .selectize-dropdown-content { max-width: 100%; }
  .selectize-dropdown-content .option {
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  /* Navbar tint to match section accent (subtle) */
  .navbar { border-bottom: 3px solid #1f77b4; }
")

# Lightweight tooltip helper: when the user hovers a selectize dropdown
# option, copy its text into the `title` attribute so the full name
# shows as a native tooltip. Uses event delegation on a single mouseover
# listener — no MutationObserver, no repeated document-wide scans.
mzx_selectize_titles_js <- tags$script(HTML("
$(document).on('mouseover', '.selectize-dropdown-content .option', function(){
  if (!this.hasAttribute('title')) this.setAttribute('title', this.textContent);
});
"))

ui <- navbarPage(
  title = "mzXplorer: High-Resolution Mass Spectrometry Analysis",
  theme = bslib::bs_theme(
    version   = 5,
    bootswatch = "sandstone",
    primary   = "#1f77b4",   # matches plot/formula section
    secondary = "#7b3fbf",   # matches homologue section
    success   = "#2e8b57",   # matches column mapping
    warning   = "#e07b00",   # matches MD/mz differences
    info      = "#2c8a86",   # matches ISF
    "font-scale" = 0.95
  ),
  header = tagList(
    tags$head(tags$style(.mzx_section_css), mzx_selectize_titles_js),
    shinyjs::useShinyjs()
  ),

  # Tab 1: Mass Defect Analysis
  tabPanel(
    "Mass Defect Analysis",
    mass_defect_ui("md_analysis")
  ),

  # Tab 2: In-Source Fragmentation
  tabPanel(
    "In-Source Fragmentation",
    isf_ui("isf_discovery")
  ),

  # Tab 3: Instructions (Optional)
  tabPanel(
    "Instructions",
    fluidPage(
            shiny::br(),
            # MathJax config must be injected BEFORE MathJax.js loads,
            # so that $...$ / $$...$$ are accepted as math delimiters.
            tags$head(
              tags$script(type = "text/x-mathjax-config", HTML(
                "MathJax.Hub.Config({
                   tex2jax: {
                     inlineMath:  [['$','$'], ['\\\\(','\\\\)']],
                     displayMath: [['$$','$$'], ['\\\\[','\\\\]']],
                     processEscapes: true
                   },
                   TeX: { extensions: ['AMSmath.js','AMSsymbols.js'] }
                 });"
              ))
            ),
            shiny::withMathJax(),
            shiny::includeMarkdown("./instructions.md")
    )
  )
)
