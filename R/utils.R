# ══════════════════════════════════════════════════════════════════
# 内部工具
# ══════════════════════════════════════════════════════════════════

#' Thousands separator
#' @noRd
fmt_n <- function(x) formatC(x, format = "d", big.mark = ",")


#' Package version (falls back to "dev" when sourced without installing)
#' @noRd
.pkg_version <- function() {
  tryCatch(as.character(utils::packageVersion("ukbcareer")),
           error = function(e) "dev")
}


#' Read a wide table (path or an existing data.frame)
#' @noRd
.read_wide <- function(path) {
  if (inherits(path, "data.frame")) return(data.table::as.data.table(path))
  if (!is.character(path) || length(path) != 1L) {
    stop("`path` must be either a csv path or a data.frame")
  }
  if (!file.exists(path)) stop("File not found: ", path)
  data.table::fread(path, showProgress = FALSE)
}


#' Per-person questionnaire year (UKB field 22500)
#'
#' Used both to fill ongoing records and as the third term of `career_end`.
#' In the reference cohort coverage is 100% (2015 for 98.3%, 2016 for 1.7%).
#' @noRd
.entry_year <- function(entry_path, d, fill_year, verbose = TRUE) {
  ids <- as.character(d$eid)
  src <- "fallback_constant"
  y <- rep(NA_real_, length(ids))

  pick <- function(tb) {
    cc <- .slot_cols(names(tb), FIELDS$entry_date)
    if (!length(cc)) {
      cc <- intersect(c(FIELDS$entry_date, "entry_date", "p22500"), names(tb))
    }
    if (!length(cc)) return(NULL)
    v <- tb[[cc[1L]]]
    # 可能是日期字符串，也可能已经是年份
    yr <- suppressWarnings(as.numeric(v))
    if (all(is.na(yr)) || any(stats::na.omit(yr) > 3000)) {
      yr <- as.numeric(format(as.Date(as.character(v)), "%Y"))
    }
    stats::setNames(yr, as.character(tb$eid))
  }

  got <- NULL
  if (!is.null(entry_path)) {
    tb <- .read_wide(entry_path)
    got <- pick(tb)
    if (is.null(got)) {
      warning("`entry_path` does not contain ", FIELDS$entry_date,
              " -- falling back to the constant ", fill_year, call. = FALSE)
    } else {
      src <- "field_22500"
    }
  }
  if (is.null(got)) {
    got <- pick(d)                       # 也许就在同一张表里
    if (!is.null(got)) src <- "field_22500"
  }
  if (!is.null(got)) y <- got[ids]

  n_miss <- sum(is.na(y))
  if (n_miss) {
    if (verbose && src == "field_22500") {
      message(sprintf("  [warn] %s people have no entry date -> using %d",
                      fmt_n(n_miss), fill_year))
    }
    if (src != "field_22500") {
      warning("No work-history questionnaire year (field 22500, which lives in ",
              "**Category 123**, not 130).\n",
              "  Falling back to the constant ", fill_year, ". Cost: ongoing ",
              "records gain about 2 years each = 5.7% of all person-years,\n",
              "  and the observed-retirement rate drops from 56.8% to 27.7%. ",
              "**These are no longer the published definitions**; ukb_qc() ",
              "will flag it.", call. = FALSE)
    }
    y[is.na(y)] <- fill_year
  }
  names(y) <- ids
  attr(y, "source") <- src
  y
}


#' Look up a column in the main table or the covariate table
#'
#' **The covariate table wins.** The same person's year of birth can differ
#' between two UK Biobank files (we measured 14 of 1,998 people differing by
#' up to 5 years). Baseline data is the authoritative source and is what the
#' upstream pipeline uses; preferring the main table would shift the whole age
#' axis -- and `career_end`, `seq_len` and every age-related measure with it --
#' **without raising an error**. When both are present they are compared and
#' any disagreement is reported.
#' @noRd
.lookup_col <- function(d, cov, field, aliases, what) {
  pick <- function(tb) {
    cc <- .slot_cols(names(tb), field)
    if (!length(cc)) cc <- intersect(aliases, names(tb))
    if (length(cc)) cc[1L] else NULL
  }
  c_cov <- if (is.null(cov)) NULL else pick(cov)
  c_main <- pick(d)
  v_cov <- if (is.null(c_cov)) NULL else cov[[c_cov]][match(d$eid, cov$eid)]
  v_main <- if (is.null(c_main)) NULL else d[[c_main]]

  if (!is.null(v_cov) && !is.null(v_main)) {
    dif <- which(!is.na(v_cov) & !is.na(v_main) &
                   as.numeric(v_cov) != as.numeric(v_main))
    if (length(dif)) {
      warning(sprintf(paste0("%s disagrees between the two tables for %d ",
                             "people (main `%s` vs covariate `%s`).\n  ",
                             "**The covariate table is used** -- it is the ",
                             "authoritative baseline source and matches the ",
                             "upstream pipeline. The main-table column may ",
                             "come from a different dispensal."),
                      what, length(dif), c_main, c_cov), call. = FALSE)
    }
  }
  if (!is.null(v_cov)) {
    if (anyNA(v_cov)) {
      n_na <- sum(is.na(v_cov))
      if (!is.null(v_main)) {
        v_cov[is.na(v_cov)] <- as.numeric(v_main)[is.na(v_cov)]
        warning(sprintf(paste0("%d people have no %s in the covariate table ",
                               "-> filled from the main table"), n_na, what),
                call. = FALSE)
      } else {
        warning(sprintf("%d people have no %s", n_na, what), call. = FALSE)
      }
    }
    return(v_cov)
  }
  if (!is.null(v_main)) return(v_main)
  stop(what, " not found. It is absent from the main table and ",
       if (is.null(cov)) {
         paste0("no `covariate_path` was given -- a real Category 130 export ",
                "usually does not carry this column, so please supply one")
       } else {
         "also absent from the `covariate_path` table"
       })
}


#' Resolve the sex column
#'
#' **Modelling by sex is the design, not a post-hoc stratification**: the two
#' representations come from two independently trained encoders, and the CAMSIS
#' scale is itself sex-specific. So sex cannot be missing.
#' @noRd
.resolve_sex <- function(d, cov = NULL, sex = NULL) {
  if (!is.null(sex) && length(sex) == nrow(d)) return(.norm_sex(sex))
  if (!is.null(sex) && length(sex) == 1L) {
    cc <- intersect(sex, names(d))
    if (length(cc)) return(.norm_sex(d[[cc[1L]]]))
    if (!is.null(cov) && sex %in% names(cov)) {
      return(.norm_sex(cov[[sex]][match(d$eid, cov$eid)]))
    }
    stop("The sex column you named (`", sex, "`) was not found")
  }
  v <- tryCatch(
    .lookup_col(d, cov, FIELDS$sex,
                c("Sex", "sex", "sex_label", "p31", "f.31.0.0"),
                "Sex (field 31)"),
    error = function(e) {
      stop("Sex column not found. Modelling by sex is this package's design ",
           "(the two representations come from independently trained encoders, ",
           "and the CAMSIS scale is sex-specific).\n",
           "  Sex is field 31, which is baseline data -- **a real Category 130 ",
           "export usually does not carry it**.\n",
           "  Pass `covariate_path=` pointing at a table that has it, or ",
           "`sex=` with a vector aligned to the rows.", call. = FALSE)
    })
  .norm_sex(v)
}


#' Normalise sex coding to "Female" / "Male"
#' @noRd
.norm_sex <- function(v) {
  if (is.numeric(v)) {
    # UKB coding 9：0 = Female，1 = Male
    out <- ifelse(v == 0, "Female", ifelse(v == 1, "Male", NA_character_))
  } else {
    s <- tolower(trimws(as.character(v)))
    out <- ifelse(s %in% c("female", "f", "0", "women", "woman"), "Female",
                  ifelse(s %in% c("male", "m", "1", "men", "man"), "Male",
                         NA_character_))
  }
  if (anyNA(out)) {
    stop(sum(is.na(out)), " people have an unparseable sex value ",
         "(0/1 and Female/Male/F/M are recognised)")
  }
  factor(out, levels = SEXES)
}


#' Is a path pure ASCII?
#'
#' libtorch resolves paths through the system ANSI encoding, so non-ASCII
#' characters (a Chinese user name, say) make `jit_load()` report
#' "Parent directory does not exist". The export side works around this with
#' an in-memory buffer, but **the load side cannot** -- the bundle has to sit
#' on an ASCII path.
#' @noRd
.is_ascii_path <- function(p) {
  !grepl("[^\x01-\x7F]", p, useBytes = TRUE)
}


#' Rank transform, breaking ties at random with a fixed seed
#'
#' Ties are broken because `rank()`'s average ranks pile up at the cut points.
#' Breaking them means tied cases go arbitrarily to either side, which is
#' unavoidable -- so **the tied fraction is reported** as an attribute.
#' @noRd
# 上游的教训：真实困惑度 IQR 只有 0.0008，float32 在 1.0 附近间距约 1.2e-7，
# 44,768 人挤在约 6,700 个取值上 → qcut(..., 10) 会静默给出少于 10 组。
.urank <- function(x, seed = 42L) {
  n <- length(x)
  set.seed(seed)
  r <- order(order(x, stats::runif(n)))
  attr(r, "frac_tied") <- 1 - length(unique(stats::na.omit(x))) / sum(!is.na(x))
  r
}
