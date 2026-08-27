# ---------------------------------------------------------------------------
# I/O + column mapping + shared plotly theme for mzXplorer
# ---------------------------------------------------------------------------

#' Robustly read a feature file. Accepts CSV, TSV, TXT, and Excel
#' workbooks (.xlsx / .xls). For delimited text it auto-detects the
#' separator and cycles through common encodings so Windows-1252 /
#' Latin-1 exports load cleanly. For Excel files the first non-empty
#' sheet is used.
#'
#' Does NOT rename or validate columns — the column-mapping UI handles
#' that.
#'
#' @param path path to CSV/TSV/TXT/XLSX/XLS
#' @return data.table with the file's original column names
#' @export
#' @param orig_name optional original filename (used to detect the
#'   extension; Shiny's `datapath` does not preserve it).
read_feature_file <- function(path, orig_name = NULL) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)

  ext <- tolower(tools::file_ext(if (!is.null(orig_name) && nzchar(orig_name)) orig_name else path))

  # ---- Excel branch -------------------------------------------------------
  if (ext %in% c("xlsx", "xls")) {
    # readxl needs a proper file extension because Shiny's temp datapath
    # has none. Stage a copy with the correct suffix if required.
    xl_path <- path
    if (tolower(tools::file_ext(path)) != ext) {
      xl_path <- tempfile(fileext = paste0(".", ext))
      file.copy(path, xl_path, overwrite = TRUE)
    }

    # ---- Fast path: openxlsx2 (xlsx only) -------------------------------
    # openxlsx2 reads the raw sharedStrings + sheet XML with a compiled C++
    # backend and *no* per-column type guessing pass, which for wide numeric
    # feature tables is typically 3-10x faster than readxl::read_excel.
    # We fall back to readxl automatically for .xls (openxlsx2 is xlsx-only)
    # or if openxlsx2 is not installed.
    df <- NULL
    if (ext == "xlsx" && requireNamespace("openxlsx2", quietly = TRUE)) {
      sheet_names <- tryCatch(openxlsx2::wb_load(xl_path)$sheet_names,
                              error = function(e) character(0))
      if (!length(sheet_names)) {
        sheet_names <- tryCatch(openxlsx2::wb_to_df(xl_path, sheet = 1, rows = 1) |>
                                  attr("sheet"), error = function(e) NULL) %||% "Sheet1"
      }
      for (sh in sheet_names) {
        cand <- tryCatch(
          openxlsx2::wb_to_df(xl_path, sheet = sh,
                              col_names = TRUE, na.strings = c("", "NA", "N/A"),
                              show_formula = FALSE),
          error = function(e) NULL
        )
        if (!is.null(cand) && ncol(cand) > 0 && nrow(cand) > 0) {
          df <- cand; break
        }
      }
    }

    # ---- Fallback: readxl ----------------------------------------------
    if (is.null(df)) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Reading Excel files requires the 'readxl' or 'openxlsx2' package. Install one with install.packages('readxl') or install.packages('openxlsx2').",
             call. = FALSE)
      }
      sheets <- tryCatch(readxl::excel_sheets(xl_path),
                         error = function(e) character(0))
      if (!length(sheets)) {
        stop(sprintf("Could not read Excel file '%s' (no sheets found).",
                     basename(path)), call. = FALSE)
      }
      last_err <- NULL
      for (sh in sheets) {
        cand <- tryCatch(
          # guess_max = 1000 (readxl default) is much faster than 10000
          # and rarely misses type inference on well-formed feature tables.
          # The app coerces mapped numeric columns explicitly downstream.
          readxl::read_excel(xl_path, sheet = sh, guess_max = 1000,
                             .name_repair = "minimal", progress = FALSE),
          error = function(e) { last_err <<- e; NULL }
        )
        if (!is.null(cand) && ncol(cand) > 0 && nrow(cand) > 0) {
          df <- cand; break
        }
      }
      if (is.null(df)) {
        stop(sprintf("Could not read Excel file '%s'. Original error: %s",
                     basename(path),
                     if (!is.null(last_err)) conditionMessage(last_err) else "no non-empty sheets"),
             call. = FALSE)
      }
    }

    data.table::setDT(df)
    nm <- names(df)
    nm <- gsub("^\ufeff", "", nm)
    nm <- trimws(nm)
    nm[is.na(nm) | nm == ""] <- paste0("V", which(is.na(nm) | nm == ""))
    names(df) <- make.unique(nm, sep = "_")
    return(df)
  }

  # ---- Delimited text branch ---------------------------------------------
  # Fast path: try data.table::fread directly on the file. fread is
  # memory-mapped and multithreaded, so on well-formed UTF-8 CSVs it is
  # dramatically faster than reading the whole file into a character
  # string first. Only fall back to the byte-level decoder if fread errs
  # or the result contains invalid UTF-8 (typical for Windows-1252
  # Excel exports).
  .sanitize_names <- function(df) {
    nm <- names(df)
    nm <- gsub("^\ufeff", "", nm)
    nm <- trimws(nm)
    nm[is.na(nm) | nm == ""] <- paste0("V", which(is.na(nm) | nm == ""))
    names(df) <- make.unique(nm, sep = "_")
    df
  }

  fast_df <- tryCatch(
    data.table::fread(path, sep = "auto", header = TRUE,
                      showProgress = FALSE, data.table = TRUE,
                      fill = TRUE, blank.lines.skip = TRUE,
                      encoding = "UTF-8"),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (!is.null(fast_df) && ncol(fast_df) > 0 && nrow(fast_df) > 0) {
    # Verify column names are valid UTF-8. If not, fall through to the
    # bytes decoder.
    ok_names <- all(vapply(names(fast_df),
                           function(x) validUTF8(x), logical(1)))
    ok_char  <- TRUE
    # Sample up to 5 character columns for encoding validity
    char_cols <- names(fast_df)[vapply(fast_df, is.character, logical(1))]
    if (length(char_cols)) {
      probe <- utils::head(char_cols, 5L)
      ok_char <- all(vapply(probe,
                            function(cn) all(validUTF8(fast_df[[cn]])),
                            logical(1)))
    }
    if (ok_names && ok_char) return(.sanitize_names(fast_df))
  }

  # Slow / robust path: read raw bytes and try several encodings. This
  # handles Excel-exported CSVs on Windows (Windows-1252 / Latin-1) that
  # would otherwise throw "input string N is invalid".
  fsize <- file.info(path)$size
  raw <- tryCatch(readBin(path, what = "raw", n = fsize),
                  error = function(e) NULL)
  if (is.null(raw) || !length(raw)) {
    stop(sprintf("Could not read file '%s' (empty or unreadable).",
                 basename(path)), call. = FALSE)
  }
  # Strip UTF-8 BOM if present
  if (length(raw) >= 3 &&
      raw[1] == as.raw(0xEF) && raw[2] == as.raw(0xBB) && raw[3] == as.raw(0xBF)) {
    raw <- raw[-(1:3)]
  }

  # Convert bytes -> UTF-8. Try encodings in order; accept the first one
  # that produces a fully valid UTF-8 string (validUTF8() returns TRUE).
  txt <- NULL
  bytes_string <- rawToChar(raw)   # marked as native by R; we ignore its encoding
  Encoding(bytes_string) <- "bytes"
  for (enc in c("UTF-8", "Windows-1252", "Latin-1", "ISO-8859-1")) {
    cand <- tryCatch(iconv(bytes_string, from = enc, to = "UTF-8",
                           sub = "?", mark = TRUE),
                     error = function(e) NA_character_,
                     warning = function(w) NA_character_)
    if (!is.na(cand) && nzchar(cand) && validUTF8(cand)) {
      txt <- cand; break
    }
  }
  # Last-resort fallback: force Latin-1 with byte substitution — never fails
  if (is.null(txt)) {
    txt <- iconv(bytes_string, from = "Latin-1", to = "UTF-8", sub = "?")
    Encoding(txt) <- "UTF-8"
  }

  # Detect delimiter from the first non-empty line
  first_line <- strsplit(txt, "\r?\n", perl = TRUE)[[1]]
  first_line <- first_line[nzchar(first_line)][1]
  if (is.na(first_line)) first_line <- ""
  seps <- c(",", "\t", ";", "|")
  counts <- vapply(seps, function(s)
    lengths(regmatches(first_line, gregexpr(s, first_line, fixed = TRUE))),
    integer(1))
  sep <- seps[which.max(counts)]
  if (max(counts) == 0) sep <- ","

  # Parse the (now UTF-8) text with fread
  df <- tryCatch(
    data.table::fread(text = txt, sep = sep, header = TRUE,
                      showProgress = FALSE, data.table = TRUE,
                      fill = TRUE, blank.lines.skip = TRUE,
                      encoding = "UTF-8"),
    error = function(e) NULL
  )

  if (is.null(df) || ncol(df) == 0) {
    stop(sprintf(
      "Could not parse '%s' as a delimited text file (tried separators: %s).",
      basename(path), paste(seps, collapse = " ")
    ), call. = FALSE)
  }

  .sanitize_names(df)
}

#' Guess which of a file's columns map to internal targets
#' (mz, rt, intensity, id, ccs). Returns a named list; entries are NA
#' when no confident match is found.
#' @param col_names character vector of the file's column names
#' @export
guess_default_mapping <- function(col_names) {
  low <- tolower(trimws(col_names))
  patterns <- list(
    mz        = c("mz", "m/z", "mass", "mass-to-charge", "mass.to.charge",
                  "precursor_mz", "precursormz", "average mz", "average.mz"),
    rt        = c("rt", "retention", "retention.time", "retention time",
                  "retention-time", "rtinseconds", "rt.min.", "rt.sec.",
                  "average rt(min)", "average.rt.min.", "rt_min", "rt (min)"),
    intensity = c("intensity", "area", "height", "abundance", "peak area",
                  "peak.area", "peak_height", "peak height", "dummy"),
    id        = c("id", "feature_id", "feature.id", "featureid",
                  "rowid", "row id", "entry", "row.id"),
    ccs       = c("ccs", "collisional_cross_section",
                  "collisional.cross.section", "ccs (Å²)", "ccs.a2",
                  # ion mobility descriptors — treated equivalently downstream
                  "ion_mobility", "ion mobility", "ion.mobility",
                  "mobility", "drift_time", "drift time", "drift.time",
                  "dt", "1/k0", "1/k_0", "k0", "k_0", "im")
  )
  out <- list(mz = NA_character_, rt = NA_character_,
              intensity = NA_character_, id = NA_character_,
              ccs = NA_character_)
  for (tgt in names(patterns)) {
    hit <- which(low %in% tolower(patterns[[tgt]]))
    if (length(hit)) out[[tgt]] <- col_names[hit[1]]
  }
  out
}

#' Apply a user-provided column mapping to a data.table.
#' Only renames columns; extra columns are preserved. "(none)" / NA entries
#' are skipped. Ensures a 1-based `id` column if none was supplied.
#' @param df data.table or data.frame
#' @param mapping named list with entries mz, rt, intensity, id, ccs
#' @export
apply_column_mapping <- function(df, mapping) {
  if (!data.table::is.data.table(df)) data.table::setDT(df)
  for (tgt in names(mapping)) {
    src <- mapping[[tgt]]
    if (is.null(src) || is.na(src) || !nzchar(src) || identical(src, "(none)")) next
    if (!src %in% names(df)) next
    if (identical(src, tgt)) next
    # If a column already exists with the target name (e.g. an unrelated
    # column that happens to be called "id"), rename it out of the way.
    if (tgt %in% names(df)) {
      data.table::setnames(df, tgt, paste0(tgt, "_orig"))
    }
    data.table::setnames(df, src, tgt)
  }
  if (!"id" %in% names(df)) df[, id := .I]
  df
}

# ---------------------------------------------------------------------------
# Legacy loader (kept for backwards compatibility)
# ---------------------------------------------------------------------------

#' Legacy CSV loader with automatic column guessing.
#' New code should use `read_feature_file()` + the mapping UI instead.
#' @export
load_feature_csv <- function(path) {
  if (is.null(path)) return(NULL)
  df <- read_feature_file(path)
  guess <- guess_default_mapping(names(df))
  df <- apply_column_mapping(df, guess)
  missing <- setdiff(c("mz", "rt", "intensity"), names(df))
  if (length(missing)) {
    stop(paste("Required columns missing:", paste(missing, collapse = ", ")))
  }
  df
}

# ---------------------------------------------------------------------------
# Shared plotly theme (scientific style)
# ---------------------------------------------------------------------------

# Okabe-Ito color-blind-safe palette
mzx_palette <- c(
  "#0072B2", "#E69F00", "#009E73", "#CC79A7",
  "#56B4E9", "#D55E00", "#F0E442", "#000000"
)

# Highlight color used by crosstalk selections across all plots
mzx_highlight_color <- "#E69F00"

# Common axis style (mirrored inward ticks, light grid)
mzx_axis <- function(title = "") {
  list(
    title      = list(text = title, font = list(family = "Helvetica", size = 14)),
    tickfont   = list(family = "Helvetica", size = 12),
    showgrid   = TRUE,
    gridcolor  = "#ececec",
    zeroline   = FALSE,
    showline   = TRUE,
    linecolor  = "#333333",
    mirror     = TRUE,
    ticks      = "inside",
    tickcolor  = "#333333"
  )
}

#' Apply the shared mzXplorer plotly theme to a figure.
#' @export
mzx_style <- function(fig, xtitle = "", ytitle = "",
                      title = NULL, legend = TRUE) {
  fig %>%
    plotly::layout(
      font       = list(family = "Helvetica", size = 12, color = "#222"),
      title      = if (!is.null(title))
        list(text = title, font = list(family = "Helvetica", size = 15))
      else NULL,
      xaxis      = mzx_axis(xtitle),
      yaxis      = mzx_axis(ytitle),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      showlegend    = legend,
      legend        = list(bgcolor = "rgba(255,255,255,0.6)",
                           bordercolor = "#cccccc", borderwidth = 1),
      hoverlabel    = list(bgcolor = "white",
                           bordercolor = "#333",
                           font = list(family = "Helvetica", size = 12))
    ) %>%
    plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("sendDataToCloud", "toggleSpikelines",
                                 "hoverCompareCartesian", "hoverClosestCartesian",
                                 "autoScale2d"),
      toImageButtonOptions = list(format = "png", filename = "mzXplorer_plot",
                                  scale = 3)
    )
}

# ---------------------------------------------------------------------------
# Sample-comparison plot
# ---------------------------------------------------------------------------

#' Build a plotly figure comparing intensities of selected features across
#' samples. `sample_map` is a **named character vector**: values are the
#' intensity-column names in `df`, names are display labels.
#'
#' @param df         data.frame or data.table of selected features
#' @param sample_map named character vector (label -> column name)
#' @param plot_type  "Grouped bars" or "Lines+markers"
#' @param x_var      column name in `df` to use for the x-axis label
#'                   (default "mz"). Falls back gracefully to "id" then
#'                   to a 1..N index if the column is missing.
#' @param ref_label  legacy arg, ignored
#' @export
make_sample_comp_plot <- function(df, sample_map, plot_type = "Grouped bars",
                                  x_var = "mz", ref_label = NULL) {
  df <- as.data.frame(df)
  if (!nrow(df) || is.null(sample_map) || !length(sample_map)) {
    return(plotly::plot_ly() %>%
             plotly::layout(title = "No features to compare"))
  }

  # Cap at 40 features to keep the plot readable
  capped <- FALSE
  if (nrow(df) > 40) { df <- df[seq_len(40), , drop = FALSE]; capped <- TRUE }

  # ------------------------------------------------------------------
  # Build the x-axis label from `x_var`. Numeric columns get formatted
  # (m/z -> 4 dp, RT -> 2 dp, others -> 3 dp). Fallback chain:
  #   requested column -> id -> 1..N
  # ------------------------------------------------------------------
  x_col <- if (!is.null(x_var) && length(x_var) == 1 && x_var %in% names(df)) x_var
           else if ("id" %in% names(df)) "id"
           else NA_character_

  # Sort features ascending by the chosen x-axis value when it is numeric
  # (e.g. m/z, RT, id). This makes the sample-comparison bars/lines read
  # left-to-right in a natural order regardless of the row order of the
  # incoming selection. Non-numeric x columns fall back to as-is order.
  if (!is.na(x_col)) {
    xv <- suppressWarnings(as.numeric(df[[x_col]]))
    if (any(!is.na(xv))) {
      df <- df[order(xv, na.last = TRUE), , drop = FALSE]
    }
  }

  if (is.na(x_col)) {
    feat_label <- as.character(seq_len(nrow(df)))
    x_title    <- "Feature (index)"
  } else {
    v <- df[[x_col]]
    x_title <- switch(x_col,
                      "mz"  = "m/z",
                      "rt"  = "Retention time",
                      "ccs" = "CCS / ion mobility",
                      "id"  = "Feature id",
                      x_col)
    if (is.numeric(v)) {
      fmt <- switch(x_col, "mz" = "%.4f", "rt" = "%.2f", "%.3f")
      feat_label <- sprintf(fmt, v)
    } else {
      feat_label <- as.character(v)
    }
  }

  # ------------------------------------------------------------------
  # Enrich the x-axis tick labels with `id` and `m/z` when available,
  # so that every plotted / selected feature is unambiguously
  # identifiable without hovering. The chosen x_var stays first (so
  # the axis is still sorted / titled by it), followed by whichever of
  # id / mz isn't already the primary key.
  # ------------------------------------------------------------------
  has_id <- "id" %in% names(df)
  has_mz <- "mz" %in% names(df)
  extras <- character(nrow(df))
  add_part <- function(vec, prefix, fmt) {
    ok <- !is.na(vec)
    out <- rep("", length(vec))
    out[ok] <- sprintf(paste0(prefix, fmt), vec[ok])
    out
  }
  parts <- list()
  if (!identical(x_col, "id") && has_id) {
    parts[[length(parts) + 1L]] <- add_part(df$id, "id ", "%s")
  }
  if (!identical(x_col, "mz") && has_mz) {
    parts[[length(parts) + 1L]] <- add_part(suppressWarnings(as.numeric(df$mz)),
                                            "m/z ", "%.4f")
  }
  if (length(parts)) {
    joined <- do.call(paste, c(parts, sep = " | "))
    # Drop empty extras cleanly
    joined <- sub("^\\s*\\|\\s*", "", joined)
    joined <- sub("\\s*\\|\\s*$", "", joined)
    nonempty <- nzchar(joined)
    feat_label[nonempty] <- paste0(feat_label[nonempty], " (",
                                   joined[nonempty], ")")
  }

  # Guarantee unique x-axis categories so plotly doesn't collapse bars
  # with identical numeric labels.
  if (anyDuplicated(feat_label)) {
    feat_label <- make.unique(feat_label, sep = " #")
  }

  samples <- as.character(sample_map)
  labels  <- if (!is.null(names(sample_map))) names(sample_map) else samples

  # Do NOT drop samples whose column is missing from `df`. Keep them
  # with a full-NA column so the user still sees the slot in the legend
  # and can tell which selection is missing data. Track missing ones
  # to surface a warning on the plot.
  missing_cols <- samples[!samples %in% names(df)]
  if (!length(samples)) {
    return(plotly::plot_ly() %>%
             plotly::layout(title = "No matching sample columns"))
  }

  # Truncate long legend/trace labels so they don't overflow the plotting
  # area. The full name is kept in `full_labels` for the hover tooltip.
  full_labels <- labels
  short_labels <- ifelse(nchar(labels) > 22,
                         paste0(substr(labels, 1, 20), "\u2026"),
                         labels)

  # Long-format matrix (features x samples). Missing sample columns are
  # filled with NA so the trace still exists (empty bars/points) and the
  # sample appears in the legend.
  mat <- as.data.frame(lapply(samples, function(cn) {
    if (cn %in% names(df)) suppressWarnings(as.numeric(df[[cn]]))
    else rep(NA_real_, nrow(df))
  }))
  names(mat) <- short_labels

  fig <- plotly::plot_ly()
  pal <- mzx_palette
  n   <- length(labels)

  # Rich per-point hover text: id / m/z / RT — independent of x_var.
  hover_id <- if ("id" %in% names(df)) as.character(df$id) else rep("NA", nrow(df))
  hover_mz <- if ("mz" %in% names(df)) sprintf("%.4f", as.numeric(df$mz)) else rep("NA", nrow(df))
  hover_rt <- if ("rt" %in% names(df)) sprintf("%.2f", as.numeric(df$rt)) else rep("NA", nrow(df))
  point_txt <- sprintf("id: %s<br>m/z: %s<br>RT: %s", hover_id, hover_mz, hover_rt)

  if (identical(plot_type, "Lines+markers") ||
      identical(plot_type, "line") ||
      identical(plot_type, "lines")) {
    for (i in seq_len(n)) {
      yvec <- mat[[i]]
      keep <- !is.na(yvec)
      if (!any(keep)) next
      xi   <- feat_label[keep]
      yi   <- yvec[keep]
      # Bake the full hover string into `text` and use hoverinfo="text".
      # Some plotly.js builds don't interpolate `%{text}` inside a
      # hovertemplate for line/marker traces, so we sidestep the
      # placeholder entirely.
      hi <- paste0("<b>", htmltools::htmlEscape(full_labels[i]), "</b><br>",
                   point_txt[keep], "<br>",
                   "Intensity: ", format(yi, big.mark = ",", scientific = FALSE))
      fig <- plotly::add_trace(
        fig,
        x = xi, y = yi,
        name = short_labels[i], type = "scatter", mode = "lines+markers",
        legendgroup = short_labels[i],
        text = hi,
        hoverinfo = "text",
        line   = list(color = pal[((i - 1) %% length(pal)) + 1], width = 2),
        marker = list(size = 8,
                      color = pal[((i - 1) %% length(pal)) + 1],
                      line = list(color = "white", width = 1))
      )
    }
    xaxis_layout <- list(type = "category")
  } else {
    # Grouped bars with MANUAL x-positioning. Using barmode="group" on a
    # categorical x leaves a reserved empty slot for every trace that has
    # no data at a given x — that's the source of the "phantom bar" gap
    # (e.g. sample 2 missing at point 1 shows as space between sample 1
    # and sample 3). Here we place each bar at an explicit numeric x so
    # only samples with data at a feature take up space, and inter-
    # feature spacing is controlled by group_gap.
    bar_w     <- 0.9
    group_gap <- 0.6
    n_feat    <- nrow(df)
    sample_mat <- as.matrix(mat)
    if (!is.numeric(sample_mat)) storage.mode(sample_mat) <- "double"

    # Per-feature list of sample indices with data
    feat_samples <- lapply(seq_len(n_feat), function(fi) {
      which(!is.na(sample_mat[fi, ]))
    })

    # Cumulative x-positions of every (feature, sample) bar + centers
    # of each feature group (used for the tick labels).
    x_positions   <- vector("list", n_feat)
    group_centers <- numeric(n_feat)
    cur <- 0
    for (fi in seq_len(n_feat)) {
      s_ok <- feat_samples[[fi]]
      if (!length(s_ok)) {
        group_centers[fi] <- cur + bar_w / 2
        cur <- cur + bar_w + group_gap
        next
      }
      positions <- cur + (seq_along(s_ok) - 0.5) * bar_w
      names(positions) <- as.character(s_ok)
      x_positions[[fi]]   <- positions
      group_centers[fi]   <- mean(range(positions))
      cur <- max(positions) + bar_w / 2 + group_gap
    }

    for (i in seq_len(n)) {
      key <- as.character(i)
      xs <- numeric(0); ys <- numeric(0); pt <- character(0)
      for (fi in seq_len(n_feat)) {
        xps <- x_positions[[fi]]
        if (!length(xps) || !key %in% names(xps)) next
        xs <- c(xs, xps[[key]])
        ys <- c(ys, sample_mat[fi, i])
        pt <- c(pt, point_txt[fi])
      }
      if (!length(xs)) next
      hi <- paste0("<b>", htmltools::htmlEscape(full_labels[i]), "</b><br>",
                   pt, "<br>",
                   "Intensity: ", format(ys, big.mark = ",", scientific = FALSE))
      fig <- plotly::add_trace(
        fig,
        x = xs, y = ys,
        width = bar_w,
        name = short_labels[i], type = "bar",
        legendgroup = short_labels[i],
        hovertext = hi,
        hoverinfo = "text",
        textposition = "none",
        marker = list(color = pal[((i - 1) %% length(pal)) + 1],
                      line = list(color = "#222", width = 0.4))
      )
    }

    fig <- plotly::layout(fig, barmode = "overlay")

    # Numeric x-axis with custom tick labels at each feature-group center.
    xaxis_layout <- list(
      type      = "linear",
      tickmode  = "array",
      tickvals  = group_centers,
      ticktext  = feat_label,
      tickangle = -30,
      zeroline  = FALSE
    )
  }

  ttl <- if (capped) "Sample comparison (showing first 40 features)" else NULL
  fig <- mzx_style(fig, xtitle = x_title, ytitle = "Intensity",
                   title = ttl, legend = TRUE) %>%
    plotly::layout(
      xaxis  = xaxis_layout,
      legend = list(bgcolor = "rgba(255,255,255,0.6)",
                    bordercolor = "#cccccc", borderwidth = 1,
                    font = list(size = 11),
                    # Top-half of the plot: samples legend
                    y = 1, yanchor = "top",
                    x = 1.02, xanchor = "left"),
      # Wide right margin to fit the sample legend AND the "Features
      # shown" info panel that sits underneath it. Without this the
      # panel gets clipped at the figure's right edge.
      margin = list(r = 340)
    )

  # --- "Features shown" panel (a second legend-like info block below
  # the samples legend). One line per plotted feature listing id, m/z,
  # RT (whichever are available). Hover on the actual bars/markers
  # still shows the same info per point.
  has_rt <- "rt" %in% names(df)
  feat_lines <- vapply(seq_len(nrow(df)), function(k) {
    parts <- character(0)
    if (has_id) parts <- c(parts, paste0("id ", df$id[k]))
    if (has_mz) parts <- c(parts, sprintf("m/z %.4f",
                                          suppressWarnings(as.numeric(df$mz[k]))))
    if (has_rt) parts <- c(parts, sprintf("RT %.2f",
                                          suppressWarnings(as.numeric(df$rt[k]))))
    paste(parts, collapse = "  \u00B7  ")
  }, character(1))
  # Very long feature lists get scrollbar-less; cap displayed lines and
  # note the overflow in the last row.
  max_lines <- 30L
  if (length(feat_lines) > max_lines) {
    overflow  <- length(feat_lines) - max_lines
    feat_lines <- c(feat_lines[seq_len(max_lines)],
                    sprintf("<i>… %d more not shown</i>", overflow))
  }
  feat_panel <- list(
    xref = "paper", yref = "paper",
    x = 1.02, y = 0.45,
    xanchor = "left", yanchor = "top",
    showarrow = FALSE, align = "left",
    font = list(size = 10, family = "monospace"),
    bgcolor = "rgba(255,255,255,0.85)",
    bordercolor = "#cccccc", borderwidth = 1,
    text = paste0("<b>Features shown</b><br>",
                  paste(feat_lines, collapse = "<br>"))
  )

  ann_list <- list(feat_panel)

  # Warn on the plot if any selected sample columns aren't present in
  # the current data (usually because the feature-table view has been
  # filtered/reshaped and dropped those columns).
  if (length(missing_cols)) {
    ann_list <- c(ann_list, list(list(
      xref = "paper", yref = "paper", x = 0, y = 1.06,
      xanchor = "left", yanchor = "bottom", showarrow = FALSE,
      font = list(size = 11, color = "#a15c00"),
      text = sprintf("Missing columns (shown empty): %s",
                     paste(missing_cols, collapse = ", "))
    )))
  }
  fig <- plotly::layout(fig, annotations = ann_list)
  fig
}
