# =========================================================
# mzXplorer: Global configuration
# =========================================================

# 1. Libraries
library(shiny)
library(shinyjqui)
library(shinythemes)
library(DT)
library(dplyr)
library(plotly)
library(crosstalk)
library(enviPat)
library(vroom)
library(igraph)
library(bslib)
library(data.table)
library(shinyjs)
library(markdown)
if (requireNamespace("readxl", quietly = TRUE)) {
  suppressPackageStartupMessages(library(readxl))
}


# 2. Options
# Allow uploading large MS2 files (MGF / MSP can easily exceed 200 MB).  
# The setting is per-session; users can override via the MZX_MAX_UPLOAD_GB
# environment variable if they need even bigger files.
.max_gb <- suppressWarnings(as.numeric(Sys.getenv("MZX_MAX_UPLOAD_GB", "4")))
if (!is.finite(.max_gb) || .max_gb <= 0) .max_gb <- 4
options(shiny.maxRequestSize = .max_gb * 1024^3)

# 3. Source all files in R/ directory
r_files <- list.files("R", full.names = TRUE, pattern = "\\.[Rr]$")
sapply(r_files, source)

# 4. Shared data
data("isotopes", package = "enviPat")

