#' Fast MGF/MSP fragment-file parsers.
#'
#' The previous implementations walked every input line through a chain
#' of `grepl` tests inside an R `for` loop and rebuilt peak matrices with
#' `do.call(rbind, lapply(...))`. That is O(n * n_patterns) and
#' allocates one list element per peak. For large MS2 exports (tens of
#' thousands of spectra) this is the dominant load-time cost.
#'
#' The parsers below:
#'   * read the whole file once with `data.table::fread(sep="\n")` which
#'     is much faster than base `readLines` on large files;
#'   * split the line vector into spectrum blocks using vectorised
#'     `which()` on the BEGIN/END markers;
#'   * pull metadata via a single `regexpr("=", block, fixed=TRUE)` call
#'     per block (no per-key regex);
#'   * parse peak blocks in one shot with `fread(text=…)`.

# ---- helpers ---------------------------------------------------------------

.read_lines_fast <- function(path) {
  # fread with sep = "\n" returns a data.table with one column of lines;
  # it uses memory-mapping and is 3-5x faster than readLines() for big
  # text files. Falls back to readLines on any error.
  out <- tryCatch(
    data.table::fread(path, sep = "\n", header = FALSE, quote = "",
                      strip.white = FALSE, blank.lines.skip = FALSE,
                      showProgress = FALSE, data.table = FALSE,
                      encoding = "UTF-8"),
    error = function(e) NULL, warning = function(w) NULL
  )
  if (!is.null(out) && ncol(out) >= 1) return(out[[1]])
  readLines(path, warn = FALSE)
}

.parse_peaks_block <- function(peak_lines) {
  if (!length(peak_lines)) return(list(mz = numeric(), int = numeric()))
  # Vectorised: split every "mz  int" line at once. Handles space, tab
  # and multiple whitespace. Only the first two tokens are used.
  parts <- strsplit(peak_lines, "[ \t,;]+", perl = TRUE)
  mz  <- suppressWarnings(as.numeric(vapply(parts, `[`, character(1), 1L)))
  int <- suppressWarnings(as.numeric(vapply(parts, `[`, character(1), 2L)))
  keep <- !is.na(mz) & !is.na(int)
  list(mz = mz[keep], int = int[keep])
}

# Vectorised peak parser for MANY blocks in a single call. Given the
# full lines vector and a peak-line index vector plus each peak-line's
# spectrum id (integer group), returns a list of `list(mz, int)`, one
# per spectrum id in `1:n_spectra`. This is orders of magnitude faster
# than calling `.parse_peaks_block()` once per spectrum.
.parse_all_peaks <- function(all_lines, peak_idx, spec_of_line, n_spectra) {
  out <- rep(list(list(mz = numeric(), int = numeric())), n_spectra)
  if (!length(peak_idx)) return(out)
  pl <- all_lines[peak_idx]
  # data.table::tstrsplit is a single C-level call that returns already
  # split columns — much faster than base strsplit + vapply for millions
  # of peak lines.
  tk <- data.table::tstrsplit(pl, "[ \t,;]+", perl = TRUE, fixed = FALSE,
                              keep = 1:2, fill = NA_character_)
  mz  <- suppressWarnings(as.numeric(tk[[1]]))
  int <- suppressWarnings(as.numeric(tk[[2]]))
  keep <- !is.na(mz) & !is.na(int)
  if (!any(keep)) return(out)
  mz  <- mz[keep]; int <- int[keep]
  grp <- spec_of_line[keep]
  # data.table split by integer key is very fast.
  ord <- order(grp)
  grp <- grp[ord]; mz <- mz[ord]; int <- int[ord]
  # Find run-length boundaries in the sorted group vector.
  runs <- rle(grp)
  ends <- cumsum(runs$lengths)
  starts <- c(1L, head(ends, -1L) + 1L)
  for (k in seq_along(runs$values)) {
    j <- runs$values[k]
    rng <- starts[k]:ends[k]
    out[[j]] <- list(mz = mz[rng], int = int[rng])
  }
  out
}

# Given begin/end line-index vectors of contiguous spectrum blocks,
# return an integer vector of length n_lines telling which spectrum
# each line belongs to (NA outside any block). Fully vectorised.
.build_spec_of_line <- function(begin_idx, end_idx, n_lines) {
  spec_of_line <- rep.int(NA_integer_, n_lines)
  valid <- end_idx >= begin_idx
  if (!any(valid)) return(spec_of_line)
  lo   <- begin_idx[valid]
  hi   <- end_idx[valid]
  ids  <- which(valid)
  reps <- hi - lo + 1L
  # Expand [lo, hi] pairs into a single index vector without a loop.
  # sequence(reps, from = lo) returns lo[1], lo[1]+1, ..., lo[1]+reps[1]-1,
  # lo[2], lo[2]+1, ...
  line_idx <- sequence(reps, from = lo)
  spec_ids <- rep.int(ids, reps)
  spec_of_line[line_idx] <- spec_ids
  spec_of_line
}

# ---- MGF -------------------------------------------------------------------

#' Parse MGF file content
#' @param lines character vector
#' @return list of spectra
parse_mgf <- function(lines) {
  # trimws is vectorised — call it once, not per line.
  lines <- trimws(lines)
  if (!length(lines)) return(list())

  begin_idx <- which(lines == "BEGIN IONS")
  end_idx   <- which(lines == "END IONS")
  if (!length(begin_idx)) return(list())

  m <- min(length(begin_idx), length(end_idx))
  begin_idx <- begin_idx[seq_len(m)]
  end_idx   <- end_idx[seq_len(m)]

  # Pre-classify meta-vs-peak lines once. A meta line contains "=" and
  # starts with a letter/underscore. A peak line starts with a digit.
  starts_digit <- grepl("^[0-9]", lines)
  has_eq       <- grepl("=", lines, fixed = TRUE)

  # ---- Vectorised peak parsing for the WHOLE file --------------------
  # Build the per-line "which spectrum am I in" vector once (fully
  # vectorised), then let `.parse_all_peaks` split all peak lines in a
  # single pass. This replaces the O(n_spectra) fread(text=) calls the
  # old code did.
  spec_of_line <- .build_spec_of_line(begin_idx + 1L, end_idx - 1L,
                                      length(lines))
  peak_mask  <- starts_digit & !is.na(spec_of_line)
  peak_idx   <- which(peak_mask)
  peaks_all  <- .parse_all_peaks(lines, peak_idx,
                                 spec_of_line[peak_idx], m)

  # ---- Metadata (still per-spectrum but very cheap: just substr) ----
  lapply(seq_len(m), function(i) {
    lo <- begin_idx[i] + 1L
    hi <- end_idx[i]   - 1L
    res <- list(peaks_mz = peaks_all[[i]]$mz,
                peaks_int = peaks_all[[i]]$int,
                mslevel = 2)
    if (hi < lo) return(res)
    idx  <- lo:hi
    meta_lines <- lines[idx][has_eq[idx] & !starts_digit[idx]]
    if (!length(meta_lines)) return(res)
    eq_pos <- regexpr("=", meta_lines, fixed = TRUE)
    keys   <- toupper(substr(meta_lines, 1L, eq_pos - 1L))
    vals   <- substr(meta_lines, eq_pos + 1L, nchar(meta_lines))
    for (k in seq_along(keys)) {
      v <- vals[k]
      switch(keys[k],
        "FEATURE_ID"  = { res$feature_id   <- suppressWarnings(as.numeric(v)) },
        "MSLEVEL"     = { res$mslevel      <- suppressWarnings(as.numeric(v)) },
        "RTINSECONDS" = { res$rt           <- suppressWarnings(as.numeric(v)) / 60 },
        "PEPMASS"     = {
          res$precursor_mz <- suppressWarnings(
            as.numeric(strsplit(v, "\\s+", perl = TRUE)[[1]][1]))
        }
      )
    }
    res
  })
}

# ---- MSP -------------------------------------------------------------------

#' Parse MSP file content
#' @param lines character vector
#' @return list of spectra
parse_msp <- function(lines) {
  # Drop blank lines up front.
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(list())

  # Entry boundaries: "NAME:" or "BEGIN IONS". Vectorised regex.
  starts <- grep("^(NAME:|BEGIN IONS|TITLE=)", lines, ignore.case = TRUE)
  if (!length(starts)) return(list())
  n_spec <- length(starts)

  ends <- c(starts[-1] - 1L, length(lines))

  # Pre-classify: peak lines start with a digit and do NOT contain ":".
  starts_digit <- grepl("^[0-9]", lines)
  has_colon    <- grepl(":", lines, fixed = TRUE)
  is_peak      <- starts_digit & !has_colon

  # Build spec_of_line once (all lines in [starts[i]..ends[i]] -> i).
  spec_of_line <- .build_spec_of_line(starts, ends, length(lines))
  peak_mask <- is_peak & !is.na(spec_of_line)
  peak_idx  <- which(peak_mask)
  peaks_all <- .parse_all_peaks(lines, peak_idx,
                                spec_of_line[peak_idx], n_spec)

  # Key patterns — single combined regex per key
  meta_keys <- list(
    feature_id   = "^(FEATURE_ID)[:=]\\s*",
    precursor_mz = "^(PRECURSORMZ|PRECURSOR M/Z|PRECURSOR MZ|PEPMASS|PrecursorMZ)[:=]\\s*",
    rt           = "^(RETENTIONINDEX|RTINSECONDS|RT|RETENTIONTIME)[:=]\\s*"
  )

  lapply(seq_len(n_spec), function(i) {
    idx   <- starts[i]:ends[i]
    meta_lines <- lines[idx][!is_peak[idx]]

    res <- list(peaks_mz  = peaks_all[[i]]$mz,
                peaks_int = peaks_all[[i]]$int,
                mslevel   = 2)

    for (nm in names(meta_keys)) {
      hit <- grep(meta_keys[[nm]], meta_lines, ignore.case = TRUE)
      if (length(hit)) {
        val <- sub(meta_keys[[nm]], "", meta_lines[hit[1]], ignore.case = TRUE)
        val <- trimws(val)
        if (nm == "precursor_mz")
          val <- strsplit(val, "\\s+", perl = TRUE)[[1]][1]
        res[[nm]] <- suppressWarnings(as.numeric(val))
      }
    }
    res
  })
}

# ---- Unified reader --------------------------------------------------------

#' Unified reader for fragment files
#' @param path path to file
#' @return list of spectra
read_fragment_file <- function(path) {
  if (is.null(path) || !file.exists(path)) return(list())
  ext <- tolower(tools::file_ext(path))
  lines <- .read_lines_fast(path)

  if (ext == "mgf") return(parse_mgf(lines))
  if (ext == "msp") return(parse_msp(lines))

  # Unknown extension: sniff for MGF markers, else assume MSP.
  # Only check the first few thousand lines — enough to decide.
  probe <- utils::head(lines, 2000L)
  if (any(grepl("BEGIN IONS", probe, ignore.case = TRUE))) return(parse_mgf(lines))
  parse_msp(lines)
}
