#' Annotate In-Source Fragmentation (ISF).
#'
#' For every MS1 feature that lies within `ppm_tol` of a fragment peak
#' AND within `rt_tol` of the precursor RT, an annotation is added.
#' `ISF_dppm` and `ISF_drt` are comma-separated strings — one value per
#' matched precursor, in the same order as the IDs in `ISF_annotation`.
#'
#' Δppm is signed: (feature_mz − fragment_mz) / fragment_mz × 1e6
#' ΔRT  is signed: (feature_rt − precursor_rt)
#'
#' @return data.frame with ISF_annotation, ISF_dppm, ISF_drt
#' @export
annotate_isf <- function(ms1, spectra, ppm_tol, rt_tol, progress = NULL) {
  # `progress` is an optional callback: function(phase, i, n, detail).
  # It lets a Shiny caller drive `incProgress()` with per-chunk detail.
  # Silent no-op when NULL so the function is usable outside Shiny.
  .prog <- if (is.function(progress)) progress else function(...) invisible(NULL)

  .empty_isf <- function(df) {
    df$ISF_annotation <- "not ISF"
    df$ISF_dppm       <- NA_character_
    df$ISF_drt        <- NA_character_
    df
  }

  if (length(spectra) == 0) return(.empty_isf(as.data.frame(ms1)))

  ppm_factor <- ppm_tol * 1e-6
  .prog("build_peaks", 0L, length(spectra),
        sprintf("Building peak table from %d spectra...", length(spectra)))

  all_peaks <- data.table::rbindlist(lapply(spectra, function(s) {
    if (is.null(s$peaks_mz) || length(s$peaks_mz) == 0) return(NULL)
    rt_val <- if (is.null(s$rt))            NA_real_ else s$rt
    if (is.na(rt_val)) return(NULL)
    pid    <- if (is.null(s$feature_id))    NA_real_ else s$feature_id
    pmz    <- if (is.null(s$precursor_mz))  NA_real_ else s$precursor_mz
    fm     <- s$peaks_mz
    data.table::data.table(
      precursor_id = pid,
      precursor_mz = pmz,
      precursor_rt = rt_val,
      frag_mz      = fm,
      rt_low       = rt_val - rt_tol,
      rt_high      = rt_val + rt_tol,
      mz_low       = fm * (1 - ppm_factor),
      mz_high      = fm * (1 + ppm_factor)
    )
  }), use.names = TRUE, fill = TRUE)

  if (!nrow(all_peaks)) return(.empty_isf(as.data.frame(ms1)))

  dt_ms1  <- data.table::as.data.table(ms1)
  mz_lo   <- min(all_peaks$mz_low,  na.rm = TRUE)
  mz_hi   <- max(all_peaks$mz_high, na.rm = TRUE)
  rt_lo   <- min(all_peaks$rt_low,  na.rm = TRUE)
  rt_hi   <- max(all_peaks$rt_high, na.rm = TRUE)
  dt_scan <- dt_ms1[mz >= mz_lo & mz <= mz_hi & rt >= rt_lo & rt <= rt_hi,
                    .(id, mz, rt)]

  if (!nrow(dt_scan)) return(.empty_isf(as.data.frame(dt_ms1)))

  data.table::setorder(dt_scan, rt, mz)
  data.table::setkey(dt_scan, rt, mz)

  # ---- CHUNKED non-equi join by RT ---------------------------------
  # A single 4-condition non-equi join over the full peak set can
  # produce enormous intermediates (peak_memory ∝ number of matching
  # rows across ALL peaks). Splitting the peak table into RT chunks
  # keeps each intermediate small, allows progress reporting, and lets
  # the caller cancel between chunks. Speed is comparable or better
  # for large inputs because each chunk's dt_scan pre-filter is tighter.
  data.table::setorder(all_peaks, precursor_rt)
  n_peaks   <- nrow(all_peaks)
  # ~30k peaks per chunk is a good sweet spot on typical hardware.
  chunk_sz  <- 30000L
  n_chunks  <- max(1L, as.integer(ceiling(n_peaks / chunk_sz)))
  .prog("match", 0L, n_chunks,
        sprintf("Matching %s features against %s fragment peaks in %d chunks...",
                format(nrow(dt_scan), big.mark = " "),
                format(n_peaks,        big.mark = " "),
                n_chunks))

  match_cols <- c("id", "feat_mz", "feat_rt",
                  "precursor_id", "precursor_mz", "precursor_rt", "frag_mz")
  matches_list <- vector("list", n_chunks)

  for (ci in seq_len(n_chunks)) {
    lo <- (ci - 1L) * chunk_sz + 1L
    hi <- min(ci * chunk_sz, n_peaks)
    peaks_i <- all_peaks[lo:hi]

    rt_lo_i <- min(peaks_i$rt_low,  na.rm = TRUE)
    rt_hi_i <- max(peaks_i$rt_high, na.rm = TRUE)
    mz_lo_i <- min(peaks_i$mz_low,  na.rm = TRUE)
    mz_hi_i <- max(peaks_i$mz_high, na.rm = TRUE)
    # Restrict dt_scan to this chunk's bounding box — usually 5-20x
    # smaller than the global scan set, which is the real speed win.
    scan_i <- dt_scan[rt >= rt_lo_i & rt <= rt_hi_i &
                      mz >= mz_lo_i & mz <= mz_hi_i]

    if (!nrow(scan_i)) {
      .prog("match", ci, n_chunks,
            sprintf("Chunk %d/%d: 0 matches (empty scan window)", ci, n_chunks))
      next
    }

    m_i <- scan_i[peaks_i,
                  on = .(rt >= rt_low, rt <= rt_high,
                         mz >= mz_low, mz <= mz_high),
                  nomatch = 0,
                  allow.cartesian = TRUE,
                  .(id, feat_mz = x.mz, feat_rt = x.rt,
                    precursor_id, precursor_mz, precursor_rt, frag_mz)]
    if (nrow(m_i)) matches_list[[ci]] <- m_i
    .prog("match", ci, n_chunks,
          sprintf("Chunk %d/%d done - %s partial matches",
                  ci, n_chunks, format(nrow(m_i), big.mark = " ")))
  }

  matches <- data.table::rbindlist(matches_list, use.names = TRUE, fill = TRUE)
  if (!nrow(matches)) return(.empty_isf(as.data.frame(dt_ms1)))

  .prog("filter", 0L, 1L,
        sprintf("Filtering %s raw matches...",
                format(nrow(matches), big.mark = " ")))
  # Discard self-hits and any match where the "fragment" is heavier than
  # its own precursor — both are meaningless as ISF.
  matches <- matches[(is.na(precursor_id) | id != precursor_id) &
                     (is.na(precursor_mz) | feat_mz <= precursor_mz)]

  if (!nrow(matches)) return(.empty_isf(as.data.frame(dt_ms1)))

  .prog("aggregate", 0L, 1L,
        sprintf("Aggregating %s matches per feature...",
                format(nrow(matches), big.mark = " ")))

  matches[, `:=`(
    dppm = (feat_mz - frag_mz) / frag_mz * 1e6,
    drt  = feat_rt - precursor_rt
  )]
  matches[, pkey := data.table::fifelse(is.na(precursor_id),
                                        -1, as.numeric(precursor_id))]
  best_idx <- matches[, .I[which.min(abs(dppm))], by = .(id, pkey)]$V1
  per_pair <- matches[best_idx, .(id, pkey, precursor_id, dppm, drt)]

  data.table::setorder(per_pair, id, pkey)
  per_pair[, `:=`(
    p_label  = data.table::fifelse(is.na(precursor_id),
                                   "Unknown", as.character(precursor_id)),
    dppm_fmt = sprintf("%.2f", dppm),
    drt_fmt  = sprintf("%.3f", drt)
  )]

  agg <- per_pair[, .(
    ids      = paste(p_label,  collapse = ", "),
    dppm_str = paste(dppm_fmt, collapse = ", "),
    drt_str  = paste(drt_fmt,  collapse = ", ")
  ), by = id]

  ms1_out <- data.table::copy(dt_ms1)
  ms1_out[, ISF_annotation := "not ISF"]
  ms1_out[, ISF_dppm       := NA_character_]
  ms1_out[, ISF_drt        := NA_character_]

  ms1_out[agg, `:=`(
    ISF_annotation = paste0("ISF of ID ", ids),
    ISF_dppm       = dppm_str,
    ISF_drt        = drt_str
  ), on = "id"]

  return(as.data.frame(ms1_out))
}

#' Helper for series palette
#' @param n number of series
#' @export
series_palette <- function(n) {
  base <- c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
    "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
    "#bcbd22", "#17becf", "#a6cee3", "#fb9a99",
    "#33a02c", "#b15928", "#cab2d6", "#fdbf6f"
  )
  rep(base, length.out = n)
}


# ---------------------------------------------------------
# .mass_cache + getmass(): compute exact mass from a chemical formula
# (using enviPat::isopattern, with caching)
# ---------------------------------------------------------
.mass_cache <- new.env(parent = emptyenv())

getmass <- function(data) {
    if (exists(data, envir = .mass_cache, inherits = FALSE)) {
        return(get(data, envir = .mass_cache, inherits = FALSE))
    }
    if (grepl("-", data)) {
        name <- unlist(strsplit(data, "-"))
        iso1 <- as.double(enviPat::isopattern(chemforms = name[1],
            isotopes = isotopes)[[1]][[1, 1]])
        iso2 <- as.double(enviPat::isopattern(chemforms = name[2],
            isotopes = isotopes)[[1]][[1, 1]])
        cus <- iso1 - iso2
    }
    else if (grepl("/", data)) {
        name <- unlist(strsplit(data, "/"))
        frac <- as.double(name[2])
        iso <- as.double(enviPat::isopattern(chemforms = name[1],
            isotopes = isotopes)[[1]][[1, 1]])
        cus <- iso/frac
    }
    else {
        cus <- as.double(enviPat::isopattern(chemforms = data,
            isotopes = isotopes)[[1]][[1, 1]])
    }
    assign(data, cus, envir = .mass_cache)
    return(cus)
}

# ---------------------------------------------------------
# get_ls_unit(): compute repeating-unit mass from formula
# ---------------------------------------------------------
get_ls_unit <- function(unit, ppm = 5, abs_floor = 0.006) {
        mass_val <- tryCatch({
                as.numeric(getmass(unit))[1]
        }, error = function(e) NA_real_)
        
        if (!is.finite(mass_val) || is.na(mass_val)) {
                warning(sprintf("Could not compute mass for repeating unit '%s'.", unit))
                return(list(unit = unit, mass = NA_real_, mz_lower = NA_real_, mz_upper = NA_real_))
        }
        
        tol_ppm <- mass_val * ppm * 1e-6
        band    <- max(tol_ppm, abs_floor)
        
        list(
                unit     = unit,
                mass     = mass_val,         # dm0
                mz_lower = mass_val - band,  # dm_min
                mz_upper = mass_val + band   # dm_max
        )
}

# ---------------------------------------------------------
# build_edges(): m/z + RT/CCS-based edges depending on mode
# ---------------------------------------------------------
build_edges <- function(mz,
                        ls_unit,
                        rt,
                        rttol,
                        ccs      = NULL,
                        ccs_mode = "rt",   # "rt", "ccs", "both"
                        ccs_tol  = 0,
                        trend    = NULL,   # "increasing", "decreasing", "any"/NULL
                        allow_gaps = FALSE) {
        dm0 <- ls_unit$mass
        if (!is.finite(dm0))
                return(data.frame(from = integer(0), to = integer(0)))
        
        dm_min <- ls_unit$mz_lower
        dm_max <- ls_unit$mz_upper
        
        has_rt  <- !is.null(rt)
        has_ccs <- !is.null(ccs)
        
        # Decide which dimensions to enforce
        use_rt  <- has_rt  && ccs_mode %in% c("rt", "both")
        use_ccs <- has_ccs && ccs_mode %in% c("ccs", "both")
        
        # If neither is usable, fall back to RT-only if available, otherwise CCS-only
        if (!use_rt && !use_ccs) {
                if (has_rt)  use_rt  <- TRUE
                if (!has_rt && has_ccs) use_ccs <- TRUE
        }
        
        k_values <- if (allow_gaps) c(1L, 2L) else 1L
        
        out_from <- integer()
        out_to   <- integer()
        
        i_vec <- seq_along(mz)
        
        for (k in k_values) {
                lower_dm <- k * dm_min
                upper_dm <- k * dm_max
                
                mz_low  <- mz + lower_dm
                mz_high <- mz + upper_dm
                
                L <- findInterval(mz_low,  mz)
                R <- findInterval(mz_high, mz)
                
                L_i <- L[i_vec]
                R_i <- R[i_vec]
                
                keep <- R_i > L_i
                if (!any(keep)) next
                
                i_use <- i_vec[keep]
                L_use <- L_i[keep]
                R_use <- R_i[keep]
                
                len_each    <- R_use - L_use
                total_edges <- sum(len_each)
                if (total_edges == 0) next
                
                from_vec <- rep.int(i_use, len_each)
                to_vec   <- unlist(Map(function(a, b) seq.int(a + 1L, b), L_use, R_use),
                                   use.names = FALSE)
                
                # --- RT constraints (tolerance + trend) ---
                if (use_rt) {
                        # tolerance
                        if (rttol > 0) {
                                d_rt   <- rt[to_vec] - rt[from_vec]
                                good_t <- abs(d_rt) <= rttol
                                from_vec <- from_vec[good_t]
                                to_vec   <- to_vec[good_t]
                        }
                        
                        # trend
                        if (!is.null(trend) && trend != "any") {
                                d_rt <- rt[to_vec] - rt[from_vec]
                                good_trend <- if (trend == "increasing") {
                                        d_rt >= -1e-12
                                } else if (trend == "decreasing") {
                                        d_rt <=  1e-12
                                } else {
                                        rep(TRUE, length(d_rt))
                                }
                                from_vec <- from_vec[good_trend]
                                to_vec   <- to_vec[good_trend]
                        }
                }
                
                # --- CCS constraints (tolerance + trend) ---
                if (use_ccs) {
                        d_ccs <- ccs[to_vec] - ccs[from_vec]
                        
                        # trend
                        if (!is.null(trend) && trend != "any") {
                                good_trend <- if (trend == "increasing") {
                                        d_ccs >= -1e-12
                                } else if (trend == "decreasing") {
                                        d_ccs <=  1e-12
                                } else {
                                        rep(TRUE, length(d_ccs))
                                }
                        } else {
                                good_trend <- rep(TRUE, length(d_ccs))
                        }
                        
                        # tolerance
                        if (ccs_tol > 0) {
                                good_tol <- abs(d_ccs) <= ccs_tol
                        } else {
                                good_tol <- rep(TRUE, length(d_ccs))
                        }
                        
                        good_ccs <- good_trend & good_tol
                        from_vec <- from_vec[good_ccs]
                        to_vec   <- to_vec[good_ccs]
                }
                
                if (!length(from_vec)) next
                
                out_from <- c(out_from, from_vec)
                out_to   <- c(out_to,   to_vec)
        }
        
        data.frame(from = out_from, to = out_to)
}

# ---------------------------------------------------------
# build_graph(): keep vertex names as ORIGINAL ROW INDICES
# ---------------------------------------------------------
build_graph <- function(edges, peaks2keep) {
        
        valid_idx <- which(peaks2keep)
        if (length(valid_idx) == 0) {
                return(igraph::make_empty_graph(n = 0, directed = TRUE))
        }
        
        if (nrow(edges) == 0) {
                return(
                        igraph::make_empty_graph(
                                n = length(valid_idx),
                                directed = TRUE
                        ) %>%
                                igraph::set_vertex_attr("name", value = as.character(valid_idx))
                )
        }
        
        keep  <- peaks2keep[edges$from] & peaks2keep[edges$to]
        edges <- edges[keep, , drop = FALSE]
        
        if (nrow(edges) == 0) {
                return(
                        igraph::make_empty_graph(
                                n = length(valid_idx),
                                directed = TRUE
                        ) %>%
                                igraph::set_vertex_attr("name", value = as.character(valid_idx))
                )
        }
        
        g <- igraph::graph_from_data_frame(
                edges[, c("from", "to")],
                directed = TRUE,
                vertices = data.frame(name = as.character(valid_idx))
        )
        
        igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
}

# ---------------------------------------------------------
# Optional spline-based RT smoothness filter (R² threshold)
# ---------------------------------------------------------
apply_shiny_splines <- function(df, R2_min = 0.98, spar = 0.45) {
        if (!"series_id" %in% names(df)) return(df)
        sids <- unique(na.omit(df$series_id))
        if (!length(sids)) return(df)
        
        drop_ids <- integer()
        for (sid in sids) {
                idx <- which(df$series_id == sid)
                if (length(idx) < 3) next
                fit <- try(
                        suppressWarnings(stats::smooth.spline(df$mz[idx], df$rt[idx], spar = spar)),
                        silent = TRUE
                )
                if (!inherits(fit, "try-error")) {
                        yhat <- stats::predict(fit, df$mz[idx])$y
                        R2   <- suppressWarnings(stats::cor(df$rt[idx], yhat)^2)
                        if (!is.finite(R2) || R2 < R2_min) drop_ids <- c(drop_ids, sid)
                }
        }
        if (length(drop_ids)) df$series_id[df$series_id %in% drop_ids] <- NA_integer_
        df
}

# ---------------------------------------------------------
# STRICT RT TREND FILTER (post-hoc, RT only)
# CCS constraints now enforced in edge construction.
# ---------------------------------------------------------
strict_rt_filter <- function(df, trend = NULL,
                             ccs_mode = "rt",
                             ccs_tol  = 0) {
        
        if (is.null(trend) || trend == "any")
                return(df)
        
        if (!"series_id.index" %in% names(df)) {
                warning("strict_rt_filter: 'series_id.index' column missing; skipping strict trend filtering.")
                return(df)
        }
        
        has_rt <- "rt" %in% names(df)
        if (!has_rt) return(df)
        
        sids <- unique(na.omit(df$series_id))
        if (!length(sids)) return(df)
        
        for (sid in sids) {
                idx  <- which(df$series_id == sid)
                comp <- df[idx, ]
                comp <- comp[order(comp$mz), ]
                
                d_rt <- diff(comp$rt)
                bad_rt <- if (trend == "increasing") {
                        which(d_rt < 0)
                } else if (trend == "decreasing") {
                        which(d_rt > 0)
                } else integer(0)
                
                if (length(bad_rt)) {
                        bad_pos  <- bad_rt + 1L
                        drop_idx <- comp$series_id.index[bad_pos]
                        df$series_id[df$series_id.index %in% drop_idx] <- NA_integer_
                }
        }
        
        df
}

###########  Built-in Homologue series finder  ###########
find_homologues <- function(
                df,
                unit       = "CH2",
                ppm        = 5,
                rttol      = 0,
                allow_gaps = FALSE,
                min_length = 3,
                rt_trend   = NULL,
                R2_min     = 0.98,
                verbose    = FALSE,
                ccs_mode   = "rt",   # "rt", "ccs", or "both"
                ccs_tol    = 0       # per-step CCS tolerance
) {
        # Map rt_trend -> internal 'trend'
        trend <- if (!is.null(rt_trend) && rt_trend != "none") rt_trend else NULL
        
        if (verbose) message("Sorting by m/z ...")
        df <- df[order(df$mz), ]
        rownames(df) <- NULL
        
        df_orig <- df  # includes 'id' if present
        
        ls_unit   <- get_ls_unit(unit, ppm = ppm)
        mz_sorted <- df$mz
        
        peaks2keep <- rep(TRUE, length(mz_sorted))
        
        if (verbose) message("Building edges ...")
        edges <- build_edges(
                mz        = mz_sorted,
                ls_unit   = ls_unit,
                rt        = df$rt,
                rttol     = rttol,
                ccs       = if ("ccs" %in% names(df)) df$ccs else NULL,
                ccs_mode  = ccs_mode,
                ccs_tol   = ccs_tol,
                trend     = trend,
                allow_gaps = allow_gaps
        )
        
        if (verbose) {
                message(sprintf(
                        "Peaks: %d | Candidate edges: %d",
                        length(mz_sorted), nrow(edges)
                ))
                message(sprintf(
                        "Unit: %s | dm0=%.6f | dm_min=%.6f | dm_max=%.6f",
                        unit, ls_unit$mass, ls_unit$mz_lower, ls_unit$mz_upper
                ))
        }
        
        if (verbose) message("Constructing graph ...")
        graph <- build_graph(
                edges     = edges,
                peaks2keep = peaks2keep
        )
        
        if (verbose) message("Extracting series ...")
        subgraphs <- igraph::decompose(graph)
        
        nodes_with_series <- do.call(
                rbind,
                lapply(seq_along(subgraphs), function(i) {
                        if (igraph::vcount(subgraphs[[i]]) == 0) return(NULL)
                        data.frame(
                                index     = as.numeric(igraph::V(subgraphs[[i]])$name),
                                series_id = i
                        )
                })
        )
        
        if (is.null(nodes_with_series) || nrow(nodes_with_series) == 0) {
                out <- df_orig
                out$series_id <- NA_integer_
                if (verbose) message("No components detected; returning NA series_id for all peaks.")
                return(out)
        }
        
        df$series_id <- nodes_with_series$series_id[
                match(seq_len(nrow(df)), as.numeric(nodes_with_series$index))
        ]
        df$series_id.index <- seq_len(nrow(df))
        
        # Apply strict RT trend filter (RT only; CCS already used in edges)
        df <- strict_rt_filter(
                df,
                trend     = trend,
                ccs_mode  = ccs_mode,
                ccs_tol   = ccs_tol
        )
        
        # Keep only sufficiently long series
        counts   <- table(df$series_id)
        good_ids <- names(counts[counts >= min_length & names(counts) != "NA"])
        df <- df[df$series_id %in% good_ids, , drop = FALSE]
        
        if (nrow(df) == 0) {
                if (verbose) message("No series found after min_length filtering")
                out <- df_orig
                out$series_id <- NA_integer_
                return(out)
        }
        
        # Optional RT spline smoothness filter
        if (!is.null(trend) && !is.null(R2_min) && R2_min > 0) {
                df <- apply_shiny_splines(df, R2_min = R2_min)
        }
        
        # Normalize series IDs
        df$series_id <- as.numeric(factor(df$series_id))
        df <- df[!is.na(df$series_id), , drop = FALSE]
        df$series_id <- as.numeric(factor(df$series_id))
        
        df
}