# =========================================================
# mzXplorer: Server entry point
# =========================================================

server <- function(input, output, session) {
  
  # Initialize Mass Defect Module
  mass_defect_server("md_analysis")
  
  # Initialize ISF Module
  isf_server("isf_discovery")
  
}
