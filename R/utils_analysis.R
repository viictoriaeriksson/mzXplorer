#' Annotate In-Source Fragmentation (ISF).
#'
#' For every MS1 feature that lies within `da_tol` Da of a fragment peak
#' AND within `rt_tol` of the precursor RT, an annotation is added.
#' `ISF_dDa` and `ISF_drt` are comma-separated strings — one value per
#' matched precursor, in the same order as the IDs in `ISF_annotation`.
#'
#' ΔDa is signed: feature_mz − fragment_mz  (Da)
#' ΔRT is signed: feature_rt − precursor_rt
#'
#' @return data.frame with ISF_annotation, ISF_dDa, ISF_drt
#' @export
annotate_isf <- function(ms1, spectra, da_tol, rt_tol, progress = NULL) {

  .prog <- if (is.function(progress)) progress else function(...) invisible(NULL)

  .empty_isf <- function(df) {
    df$ISF_annotation <- "not ISF"
    df$ISF_dDa        <- NA_character_
    df$ISF_drt        <- NA_character_
    df
  }

  if (length(spectra) == 0) return(.empty_isf(as.data.frame(ms1)))

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
      mz_low       = fm - da_tol,
      mz_high      = fm + da_tol
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


  data.table::setorder(all_peaks, precursor_rt)
  n_peaks   <- nrow(all_peaks)
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
  # its own precursor 
  matches <- matches[(is.na(precursor_id) | id != precursor_id) &
                     (is.na(precursor_mz) | feat_mz <= precursor_mz)]

  if (!nrow(matches)) return(.empty_isf(as.data.frame(dt_ms1)))

  ms1_rt_map <- dt_ms1[, .(precursor_id = id, ms1_prec_rt = rt)]
  matches <- merge(matches, ms1_rt_map, by = "precursor_id",
                   all.x = TRUE, sort = FALSE)
  matches <- matches[!is.na(ms1_prec_rt)]
  if (!nrow(matches)) return(.empty_isf(as.data.frame(dt_ms1)))
  matches[, precursor_rt := ms1_prec_rt][, ms1_prec_rt := NULL]
  matches <- matches[abs(feat_rt - precursor_rt) <= rt_tol]

  if (!nrow(matches)) return(.empty_isf(as.data.frame(dt_ms1)))

  .prog("aggregate", 0L, 1L,
        sprintf("Aggregating %s matches per feature...",
                format(nrow(matches), big.mark = " ")))

  matches[, `:=`(
    dDa = feat_mz - frag_mz,
    drt = feat_rt - precursor_rt
  )]
  matches[, pkey := data.table::fifelse(is.na(precursor_id),
                                        -1, as.numeric(precursor_id))]
  best_idx <- matches[, .I[which.min(abs(dDa))], by = .(id, pkey)]$V1
  per_pair <- matches[best_idx, .(id, pkey, precursor_id, dDa, drt)]

  data.table::setorder(per_pair, id, pkey)
  per_pair[, `:=`(
    p_label  = data.table::fifelse(is.na(precursor_id),
                                   "Unknown", as.character(precursor_id)),
    dDa_fmt  = sprintf("%.4f", dDa),
    drt_fmt  = sprintf("%.3f", drt)
  )]

  agg <- per_pair[, .(
    ids     = paste(p_label, collapse = ", "),
    dDa_str = paste(dDa_fmt, collapse = ", "),
    drt_str = paste(drt_fmt, collapse = ", ")
  ), by = id]

  ms1_out <- data.table::copy(dt_ms1)
  ms1_out[, ISF_annotation := "not ISF"]
  ms1_out[, ISF_dDa        := NA_character_]
  ms1_out[, ISF_drt        := NA_character_]

  ms1_out[agg, `:=`(
    ISF_annotation = paste0("ISF of ID ", ids),
    ISF_dDa        = dDa_str,
    ISF_drt        = drt_str
  ), on = "id"]

  return(as.data.frame(ms1_out))
}

#' Parse a mixed preset+custom neutral-loss / adduct input.
#'
#' Preset entries look like `"H2O:18.0106"` (label:mass). Custom string
#' looks like `"Label1,mass1; Label2,mass2"`. Returns a named numeric
#' vector (names = labels, values = masses in Da). Malformed entries
#' are silently skipped.
#' @export
parse_mass_input <- function(preset_vals, custom_str = "") {
  out <- c()
  for (v in preset_vals) {
    p <- strsplit(v, ":", fixed = TRUE)[[1]]
    if (length(p) == 2) {
      m <- suppressWarnings(as.numeric(p[2]))
      if (is.finite(m)) out[[p[1]]] <- m
    }
  }
  if (isTRUE(nzchar(custom_str))) {
    for (entry in strsplit(custom_str, ";", fixed = TRUE)[[1]]) {
      entry <- trimws(entry)
      if (!nzchar(entry)) next
      p <- strsplit(entry, ",", fixed = TRUE)[[1]]
      if (length(p) == 2) {
        nm <- trimws(p[1]); m <- suppressWarnings(as.numeric(trimws(p[2])))
        if (nzchar(nm) && is.finite(m)) out[[nm]] <- m
      }
    }
  }
  out
}

#' Add neutral-loss + adduct annotations to an ISF-annotated feature
#' table.
#'
#' Given the output of `annotate_isf()` (columns id, mz, rt, intensity,
#' ISF_annotation, ISF_dppm, ISF_drt), adds:
#'   * ISF_ratio         - intensity_ISF / intensity_precursor, per
#'                         precursor ID in ISF_annotation, comma-separated,
#'                         1 decimal.
#'   * ISF_NL_annotation - "ISF of ID X, Y" via neutral-loss matching.
#'   * ISF_NL_type       - comma-separated NL labels (same order).
#'   * ISF_NL_ratio      - comma-separated ratios (same order).
#'   * Adduct_annotation - "adduct of ID X, Y" if this feature's m/z
#'                         differs from another same-RT feature by any
#'                         pairwise difference between two selected
#'                         adducts. Overrides ISF tags for that feature.
#'   * Adduct_type       - comma-separated adduct-pair labels.
#' @export
annotate_extra <- function(df, nl_list = c(), adduct_list = c(),
                           da_tol = 0.01, rt_tol = 0.05) {
  n <- nrow(df)
  df$ISF_ratio         <- NA_character_
  df$ISF_NL_annotation <- "not ISF"
  df$ISF_NL_type       <- NA_character_
  df$ISF_NL_ratio      <- NA_character_
  df$ISF_NL_dDa        <- NA_character_
  df$ISF_NL_drt        <- NA_character_
  df$Adduct_annotation <- "no adduct"
  df$Adduct_type       <- NA_character_
  if (!n) return(df)
  need <- c("id", "mz", "rt", "intensity")
  if (!all(need %in% names(df))) return(df)

  dt <- data.table::as.data.table(df[, need])

  # ---- Fragment-based ratios (from existing ISF_annotation) ----------
  if ("ISF_annotation" %in% names(df)) {
    frag_rows <- which(!is.na(df$ISF_annotation) & df$ISF_annotation != "not ISF")
    if (length(frag_rows)) {
      id_int <- setNames(as.numeric(df$intensity), as.character(df$id))
      ids_list <- strsplit(sub("^ISF of ID\\s*", "", df$ISF_annotation[frag_rows]),
                           "\\s*,\\s*")
      df$ISF_ratio[frag_rows] <- vapply(seq_along(frag_rows), function(k) {
        ids <- suppressWarnings(as.numeric(ids_list[[k]]))
        p_int <- id_int[as.character(ids)]
        r <- as.numeric(df$intensity[frag_rows[k]]) / as.numeric(p_int)
        paste(ifelse(is.finite(r), sprintf("%.1f", r), ""), collapse = ", ")
      }, character(1))
    }
  }

  .match_shift <- function(X) {
    q <- dt[, .(j_id = id, j_mz = mz, j_rt = rt, j_int = intensity,
                rt_lo = rt - rt_tol, rt_hi = rt + rt_tol,
                mz_target = mz + X)]
    q[, `:=`(mz_lo = mz_target - da_tol,
             mz_hi = mz_target + da_tol)]
    p <- dt[q,
            on = .(rt >= rt_lo, rt <= rt_hi, mz >= mz_lo, mz <= mz_hi),
            nomatch = 0, allow.cartesian = TRUE,
            .(j_id, j_mz, j_rt, j_int,
              i_id = id, i_mz = x.mz, i_rt = x.rt, i_int = intensity)]
    p[j_id != i_id]
  }

  # ---- Neutral-loss ISF matching ------------------------------------
  if (length(nl_list)) {
    nl_hits <- data.table::rbindlist(lapply(names(nl_list), function(nm) {
      X <- as.numeric(nl_list[[nm]])
      if (!is.finite(X) || X <= 0) return(NULL)
      p <- .match_shift(X)
      if (!nrow(p)) return(NULL)
      p[, nl_label := nm][, nl_mass := X][]
    }), use.names = TRUE, fill = TRUE)
    if (nrow(nl_hits)) {
      # ΔDa signed: (mz_i - mz_j) - X — how far the observed mass gap is
      # from the target neutral-loss mass.
      nl_hits[, dDa   := abs((i_mz - j_mz) - nl_mass)]
      nl_hits[, drt   := j_rt - i_rt]
      nl_hits[, ratio := as.numeric(j_int) / as.numeric(i_int)]
      # Sort so the best (smallest |dDa|) precursor match is listed first
      # within each fragment.
      nl_hits[, .abs_dDa := abs(dDa)]
      data.table::setorder(nl_hits, j_id, .abs_dDa)
      nl_hits[, .abs_dDa := NULL]
      agg <- nl_hits[, .(
        ids    = paste(i_id, collapse = ", "),
        types  = paste(nl_label, collapse = ", "),
        ratios = paste(ifelse(is.finite(ratio),
                              sprintf("%.1f", ratio), ""),
                       collapse = ", "),
        dDas   = paste(ifelse(is.finite(dDa),
                              sprintf("%.4f", dDa), ""),
                       collapse = ", "),
        drts   = paste(ifelse(is.finite(drt),
                              sprintf("%.3f", drt), ""),
                       collapse = ", ")
      ), by = j_id]
      m <- match(agg$j_id, df$id)
      df$ISF_NL_annotation[m] <- paste0("ISF of ID ", agg$ids)
      df$ISF_NL_type[m]       <- agg$types
      df$ISF_NL_ratio[m]      <- agg$ratios
      df$ISF_NL_dDa[m]        <- agg$dDas
      df$ISF_NL_drt[m]        <- agg$drts
    }
  }

  # ---- Adduct matching (overrides ISF) ------------------------------
  # For each pair of user-supplied adducts (a, b) with masses (Ma, Mb),
  # any two features whose m/z differ by |Mb - Ma| (within da_tol) and
  # which co-elute (within rt_tol) are flagged as adduct partners of
  # the same neutral molecule M. Both partners are annotated (the
  # lighter one is labelled as adduct_a, the heavier as adduct_b) and
  # any ISF / NL flag on either feature is cleared.
  if (length(adduct_list) >= 2) {
    ad_nm <- names(adduct_list)
    ad_v  <- as.numeric(adduct_list)
    # Iterate ordered pairs a<b so each unsigned diff is visited once.
    diffs <- list()
    for (a in seq_along(ad_nm)) for (b in seq_along(ad_nm)) {
      if (a >= b) next
      D <- ad_v[b] - ad_v[a]
      if (!is.finite(D) || abs(D) < 1e-6) next
      # heavier - lighter; swap labels if needed so lo_lab is lighter
      if (D > 0) {
        diffs[[length(diffs) + 1L]] <- list(
          D = D, lo_lab = ad_nm[a], hi_lab = ad_nm[b])
      } else {
        diffs[[length(diffs) + 1L]] <- list(
          D = -D, lo_lab = ad_nm[b], hi_lab = ad_nm[a])
      }
    }
    ad_hits <- data.table::rbindlist(lapply(diffs, function(dd) {
      p <- .match_shift(dd$D)
      if (!nrow(p)) return(NULL)
      # j is the lighter partner (base), i is heavier (base + D).
      p[, `:=`(lo_lab = dd$lo_lab, hi_lab = dd$hi_lab)][]
    }), use.names = TRUE, fill = TRUE)
    if (nrow(ad_hits)) {
      # Aggregate from lighter partner's perspective (j -> i list).
      agg_lo <- ad_hits[, .(
        ids   = paste(i_id, collapse = ", "),
        types = paste(lo_lab, collapse = ", ")
      ), by = j_id]
      # And from the heavier partner's perspective (i -> j list).
      agg_hi <- ad_hits[, .(
        ids   = paste(j_id, collapse = ", "),
        types = paste(hi_lab, collapse = ", ")
      ), by = i_id]

      m_lo <- match(agg_lo$j_id, df$id)
      df$Adduct_annotation[m_lo] <- paste0("adduct of ID ", agg_lo$ids)
      df$Adduct_type[m_lo]       <- agg_lo$types

      m_hi <- match(agg_hi$i_id, df$id)
      df$Adduct_annotation[m_hi] <- paste0("adduct of ID ", agg_hi$ids)
      df$Adduct_type[m_hi]       <- agg_hi$types

      # Adduct overrides ISF for BOTH partners.
      m_all <- unique(c(m_lo, m_hi))
      df$ISF_annotation[m_all]    <- "not ISF"
      df$ISF_dDa[m_all]           <- NA_character_
      df$ISF_drt[m_all]           <- NA_character_
      df$ISF_ratio[m_all]         <- NA_character_
      df$ISF_NL_annotation[m_all] <- "not ISF"
      df$ISF_NL_type[m_all]       <- NA_character_
      df$ISF_NL_ratio[m_all]      <- NA_character_
      df$ISF_NL_dDa[m_all]        <- NA_character_
      df$ISF_NL_drt[m_all]        <- NA_character_
    }
  }

  df
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
                
                .ok <- function(x) !is.na(x) & x

                # --- RT constraints (tolerance + trend) ---
                if (use_rt) {
                        # tolerance
                        if (rttol > 0) {
                                d_rt   <- rt[to_vec] - rt[from_vec]
                                good_t <- .ok(abs(d_rt) <= rttol)
                                from_vec <- from_vec[good_t]
                                to_vec   <- to_vec[good_t]
                        }
                        
                        # trend
                        if (!is.null(trend) && trend != "any") {
                                d_rt <- rt[to_vec] - rt[from_vec]
                                good_trend <- if (trend == "increasing") {
                                        .ok(d_rt >= -1e-12)
                                } else if (trend == "decreasing") {
                                        .ok(d_rt <=  1e-12)
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
                                        .ok(d_ccs >= -1e-12)
                                } else if (trend == "decreasing") {
                                        .ok(d_ccs <=  1e-12)
                                } else {
                                        rep(TRUE, length(d_ccs))
                                }
                        } else {
                                good_trend <- rep(TRUE, length(d_ccs))
                        }
                        
                        # tolerance
                        if (ccs_tol > 0) {
                                good_tol <- .ok(abs(d_ccs) <= ccs_tol)
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
# Optional spline-based smoothness filter (R² threshold)
# ---------------------------------------------------------
apply_shiny_splines <- function(df, R2_min = 0.98, spar = 0.45,
                                ccs_mode = "rt") {
        if (!"series_id" %in% names(df)) return(df)
        sids <- unique(na.omit(df$series_id))
        if (!length(sids)) return(df)

        dims <- switch(ccs_mode,
                       rt   = "rt",
                       ccs  = "ccs",
                       both = c("rt", "ccs"),
                       "rt")
        dims <- dims[dims %in% names(df)]
        if (!length(dims)) return(df)

        .r2 <- function(x, y) {
                ok <- is.finite(x) & is.finite(y)
                if (sum(ok) < 3) return(NA_real_)
                fit <- try(suppressWarnings(
                        stats::smooth.spline(x[ok], y[ok], spar = spar)),
                        silent = TRUE)
                if (inherits(fit, "try-error")) return(NA_real_)
                yhat <- stats::predict(fit, x[ok])$y
                suppressWarnings(stats::cor(y[ok], yhat)^2)
        }

        drop_ids <- integer()
        for (sid in sids) {
                idx <- which(df$series_id == sid)
                if (length(idx) < 3) next
                for (d in dims) {
                        R2 <- .r2(df$mz[idx], as.numeric(df[[d]][idx]))
                        # A dimension that cannot be fitted is not evidence
                        # against the series; only a computed, too-low R2 is.
                        if (is.finite(R2) && R2 < R2_min) {
                                drop_ids <- c(drop_ids, sid)
                                break
                        }
                }
        }
        if (length(drop_ids)) df$series_id[df$series_id %in% drop_ids] <- NA_integer_
        df
}

# ---------------------------------------------------------
# STRICT TREND FILTER (post-hoc)
# ---------------------------------------------------------
#   ccs_mode = "rt"    -> RT monotonicity only   (CCS ignored)
#   ccs_mode = "ccs"   -> CCS monotonicity only  (RT ignored; RT may be absent)
#   ccs_mode = "both"  -> RT and CCS must both be monotonic

strict_rt_filter <- function(df, trend = NULL,
                             ccs_mode = "rt",
                             ccs_tol  = 0) {
        
        if (is.null(trend) || trend == "any")
                return(df)
        
        if (!"series_id.index" %in% names(df)) {
                warning("strict_rt_filter: 'series_id.index' column missing; skipping strict trend filtering.")
                return(df)
        }
        
        dims <- switch(ccs_mode,
                       rt   = "rt",
                       ccs  = "ccs",
                       both = c("rt", "ccs"),
                       "rt")
        dims <- dims[dims %in% names(df)]
        if (!length(dims)) return(df)
        
        sids <- unique(na.omit(df$series_id))
        if (!length(sids)) return(df)
        
        for (sid in sids) {
                idx  <- which(df$series_id == sid)
                comp <- df[idx, , drop = FALSE]
                comp <- comp[order(comp$mz), , drop = FALSE]
                n    <- nrow(comp)
                if (n < 2L) next
                
                vals <- lapply(dims, function(d) as.numeric(comp[[d]]))
                names(vals) <- dims
                
                keep <- rep(TRUE, n)
                last <- stats::setNames(rep(NA_real_, length(dims)), dims)
                
                for (i in seq_len(n)) {
                        bad <- FALSE
                        for (d in dims) {
                                vi <- vals[[d]][i]
                                if (!is.finite(vi) || !is.finite(last[[d]])) next
                                ok <- if (trend == "increasing") {
                                        vi >= last[[d]] - 1e-12
                                } else if (trend == "decreasing") {
                                        vi <= last[[d]] + 1e-12
                                } else TRUE
                                if (!ok) { bad <- TRUE; break }
                        }
                        if (bad) {
                                keep[i] <- FALSE
                        } else {
                                for (d in dims) {
                                        vi <- vals[[d]][i]
                                        if (is.finite(vi)) last[[d]] <- vi
                                }
                        }
                }
                
                if (any(!keep)) {
                        drop_idx <- comp$series_id.index[!keep]
                        df$series_id[df$series_id.index %in% drop_idx] <- NA_integer_
                }
        }
        
        df
}




########### MD calculation function #############

# Nominal-mass operator. `how` is one of "round", "floor", "ceiling".
.md_nom <- function(x, how) {
        switch(how,
               round   = round(x),
               floor   = floor(x),
               ceiling = ceiling(x),
               round(x))
}

# Mass defect = value - nominal(value).
.md_defect <- function(x, how) round(x - .md_nom(x, how), digits = 7)

# getmdh(): first-order mass defect uses the user-selected rounding mode;
# second- and third-order defects always use ceiling
getmdh <- function(mz, cus = c("CH2,H2"), method = "round") {
        temp <- trimws(if (grepl(',', cus)) unlist(strsplit(cus, ',')) else cus)
        cus  <- vapply(temp, getmass, numeric(1), USE.NAMES = FALSE)

        n <- length(cus)
        if (n > 3) {
                message("Sorry, only three MD base units are allowed!")
                cus <- cus[1:3]
                n   <- 3
        }

        # Scaling factor built from the first base unit.
        scale1 <- .md_nom(cus[1], method) / cus[1]

        omd <- mz * scale1
        MD1 <- .md_defect(omd, method)
        if (n == 1) return(cbind.data.frame(mz, MD1))

        sumd <- cus[2] * scale1
        md2  <- .md_defect(sumd, method)

        # Second order onwards: ceiling regardless of the selected mode.
        smd <- MD1 / md2
        MD2 <- .md_defect(smd, "ceiling")
        if (n == 2) return(cbind.data.frame(mz, MD1, MD2))

        tumd  <- cus[3] * scale1
        md3   <- .md_defect(tumd, method)
        tsmd  <- md3 / md2
        md3_2 <- .md_defect(tsmd, "ceiling")
        tmd   <- MD2 / md3_2
        MD3   <- .md_defect(tmd, "ceiling")
        cbind.data.frame(mz, MD1, MD2, MD3)
}


# ---------------------------------------------------------
# extract_ladders(): one ladder per series, by path extraction
# ---------------------------------------------------------

extract_ladders <- function(df, edges, unit_mass, min_length = 3,
                            trend = NULL, ccs_mode = "rt",
                            md_vals = NULL, method = "round") {
        if (!"series_id" %in% names(df)) return(df)
        n <- nrow(df)
        if (!is.finite(unit_mass) || unit_mass <= 0 || n == 0L) return(df)

        nom    <- .md_nom(unit_mass, method)
        md_all <- if (!is.null(md_vals) && length(md_vals) == n) as.numeric(md_vals)
                  else .md_defect(df$mz * nom / unit_mass, method)

        dims <- switch(ccs_mode,
                       ccs  = intersect("ccs", names(df)),
                       both = intersect(c("rt", "ccs"), names(df)),
                       intersect("rt", names(df)))
        dmat <- if (length(dims))
                matrix(unlist(lapply(dims, function(d) as.numeric(df[[d]]))),
                       nrow = n) else NULL
        use_trend <- !is.null(trend) && trend != "any" && !is.null(dmat)

        ef <- as.integer(edges$from)
        et <- as.integer(edges$to)
        new_id <- rep(NA_integer_, n)
        alt    <- rep(NA_integer_, n)
        next_id <- 0L

        for (sid in unique(na.omit(df$series_id))) {
                idx <- which(df$series_id == sid)
                m   <- length(idx)
                if (m < min_length) next

                # Local unit-step edges for this component, forward in m/z.
                pf <- match(ef, idx); pt <- match(et, idx)
                ok <- !is.na(pf) & !is.na(pt) & pt > pf
                fa <- pf[ok]; ta <- pt[ok]
                if (use_trend && length(fa)) {
                        good <- rep(TRUE, length(fa))
                        for (dm in seq_len(ncol(dmat))) {
                                dl <- dmat[idx, dm]
                                d  <- dl[ta] - dl[fa]
                                g  <- if (trend == "increasing") d >= -1e-12 else d <= 1e-12
                                good <- good & !is.na(g) & g
                        }
                        fa <- fa[good]; ta <- ta[good]
                }
                inc <- lapply(seq_len(m), function(i) fa[ta == i])

                md_l <- md_all[idx]
                mz_l <- df$mz[idx]

                remaining <- rep(TRUE, m)
                chains <- list()

                repeat {
                        if (sum(remaining) < min_length) break
                        score <- rep(-Inf, m)
                        prev  <- rep(NA_integer_, m)

                        for (i in which(remaining)) {
                                src <- inc[[i]]
                                src <- src[remaining[src] & is.finite(score[src])]
                                if (length(src)) {
                                        step <- 10 - 1000 * abs(md_l[i] - md_l[src])
                                        step[!is.finite(step)] <- -Inf
                                        k <- which.max(score[src] + step)
                                        if (is.finite(step[k])) {
                                                score[i] <- score[src[k]] + step[k]
                                                prev[i]  <- src[k]
                                                next
                                        }
                                }
                                score[i] <- 10
                        }
                        best <- which.max(score)
                        if (!is.finite(score[best])) break
                        path <- integer(0); i <- best
                        while (!is.na(i)) { path <- c(i, path); i <- prev[i] }
                        if (length(path) < min_length) break

                        next_id <- next_id + 1L
                        new_id[idx[path]] <- next_id
                        remaining[path] <- FALSE
                        chains[[length(chains) + 1L]] <- list(
                                id    = next_id,
                                base  = min(mz_l[path]),
                                rungs = round((mz_l[path] - min(mz_l[path])) / unit_mass),
                                md    = stats::median(md_l[path], na.rm = TRUE))
                }

                un <- which(is.na(new_id[idx]))
                for (j in un) {
                        cands <- Filter(function(ch) {
                                round((mz_l[j] - ch$base) / unit_mass) %in% ch$rungs
                        }, chains)
                        if (!length(cands)) next
                        dd <- vapply(cands, function(ch) abs(md_l[j] - ch$md), numeric(1))
                        dd[!is.finite(dd)] <- Inf
                        alt[idx[j]] <- cands[[which.min(dd)]]$id
                }
        }

        df$series_id  <- new_id
        df$series_alt <- alt
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
                ccs_tol    = 0,      # per-step CCS tolerance
                md_col     = NULL,   # existing MD column for this same unit
                method     = "round" # rounding mode used for that MD column
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
                out$series_id  <- NA_integer_
                out$series_alt <- NA_integer_
                if (verbose) message("No components detected; returning NA series_id for all peaks.")
                return(out)
        }
        
        df$series_id <- nodes_with_series$series_id[
                match(seq_len(nrow(df)), as.numeric(nodes_with_series$index))
        ]
        df$series_id.index <- seq_len(nrow(df))
        df$series_alt <- NA_integer_
        
        df <- extract_ladders(df, edges = edges, unit_mass = ls_unit$mass,
                              min_length = min_length, trend = trend,
                              ccs_mode = ccs_mode,
                              md_vals = if (!is.null(md_col) && md_col %in% names(df))
                                              df[[md_col]] else NULL,
                              method = method)
        
        # Apply strict trend filter on the selected dimension(s)
        df <- strict_rt_filter(
                df,
                trend     = trend,
                ccs_mode  = ccs_mode,
                ccs_tol   = ccs_tol
        )
        
        # Keep only sufficiently long series
        counts   <- table(df$series_id)
        good_ids <- as.numeric(names(counts[counts >= min_length & names(counts) != "NA"]))
        drop_sid <- !is.na(df$series_id) & !(df$series_id %in% good_ids)
        df$series_id[drop_sid] <- NA_integer_
        # An alternate of a series that no longer exists is not an alternate.
        df$series_alt[!is.na(df$series_alt) & !(df$series_alt %in% good_ids)] <- NA_integer_
        
        if (!any(!is.na(df$series_id))) {
                if (verbose) message("No series found after min_length filtering")
                out <- df_orig
                out$series_id  <- NA_integer_
                out$series_alt <- NA_integer_
                return(out)
        }
        
        # Optional spline smoothness filter
        if (!is.null(trend) && !is.null(R2_min) && R2_min > 0) {
                df <- apply_shiny_splines(df, R2_min = R2_min, ccs_mode = ccs_mode)
                still <- unique(na.omit(df$series_id))
                df$series_alt[!is.na(df$series_alt) & !(df$series_alt %in% still)] <- NA_integer_
        }
        
        # Normalise series IDs, keeping series_alt consistent with them.
        keep_ids <- sort(unique(na.omit(df$series_id)))
        if (length(keep_ids)) {
                remap <- stats::setNames(seq_along(keep_ids), as.character(keep_ids))
                df$series_id  <- unname(remap[as.character(df$series_id)])
                df$series_alt <- unname(remap[as.character(df$series_alt)])
        } else {
                df$series_id  <- NA_integer_
                df$series_alt <- NA_integer_
        }
        
        df
}
