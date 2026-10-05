# Molecular composition: element counts, DBE, Van Krevelen ratios.

#' Parse a chemical formula string into element counts.
#' Handles multi-letter symbols (Cl, Br).
#' @keywords internal
parse_one_formula <- function(fs, elements = c("C","H","N","O","F","Cl","Br","I","S","P")) {
  out <- setNames(rep(0L, length(elements)), elements)
  if (is.na(fs) || !nzchar(fs)) return(out)
  s <- gsub("\\s+", "", as.character(fs))
  m <- regmatches(s, regexpr("[A-Za-z0-9()]+", s))
  if (!length(m) || !nzchar(m)) return(out)
  tokens <- regmatches(m, gregexpr("[A-Z][a-z]?\\d*", m))[[1]]
  for (tok in tokens) {
    sym <- sub("\\d+$", "", tok)
    num <- sub("^[A-Za-z]+",  "", tok)
    n   <- if (nzchar(num)) suppressWarnings(as.integer(num)) else 1L
    if (is.na(n)) n <- 1L
    if (sym %in% elements) out[[sym]] <- out[[sym]] + n
  }
  out
}

#' Element counts, DBE and Van Krevelen ratios for a vector of formulas.
#' DBE = C - H/2 + N/2 - X/2 + 1, where X = F + Cl + Br + I.
#' @param formulas character vector of formula strings
#' @export
compute_molcomp <- function(formulas) {
  elems <- c("C","H","N","O","F","Cl","Br","I","S","P")
  mat <- t(vapply(formulas, parse_one_formula, integer(length(elems)),
                  elements = elems))
  df  <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df) <- elems
  df$halogens <- df$F + df$Cl + df$Br + df$I
  df$DBE <- df$C - df$H/2 + df$N/2 - df$halogens/2 + 1
  df$HC_ratio <- ifelse(df$C > 0, df$H / df$C, NA_real_)
  df$OC_ratio <- ifelse(df$C > 0, df$O / df$C, NA_real_)
  df
}
