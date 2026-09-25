# ══════════════════════════════════════════════════════════════════
# 派生变量词典 —— 用户查"这一列是什么、怎么用"的唯一入口
# ══════════════════════════════════════════════════════════════════
# 改了任何派生列的口径，这里要同步改；test-dictionary.R 会检查
# ukb_career() 的每一列都在词典里有条目（反之亦然）。

## 10 个暴露的可读名称（问卷原文的简写）
AGENT_LABELS <- c(
  noisy = "very noisy workplace", cold = "very cold workplace",
  hot = "very hot workplace", dusty = "very dusty workplace",
  fumes = "chemical or other fumes",
  cigarette = "other people's cigarette smoke",
  asbestos = "materials containing asbestos",
  paints = "paints, thinners or glues", pesticides = "pesticides",
  diesel = "diesel exhaust"
)

## 词典正文。列：variable / what（ukb_career 的 what 参数）/ group / needs /
## description（是什么）/ interpretation（怎么用、别怎么用）
.dictionary_rows <- function() {
  csv <- "your CSVs"
  d <- function(variable, what, group, needs, description, interpretation) {
    list(variable = variable, what = what, group = group, needs = needs,
         description = description, interpretation = interpretation)
  }
  rows <- list(
    ## ── core：生涯时点 ──────────────────────────────────────
    d("eid", "core", "Identifiers", csv, "UK Biobank participant ID.",
      "Merge key for outcomes and covariates."),
    d("sex", "core", "Identifiers", csv, "Sex (Female / Male), from field 31.",
      "Career types and mobility are derived separately by sex."),
    d("year_birth", "core", "Identifiers", csv, "Year of birth (field 34).",
      "Ages in this package are calendar year minus year of birth."),
    d("entry_year", "core", "Identifiers", csv,
      "Year the occupational history questionnaire was completed (field 22500).",
      "Histories are observed up to this year; later work is unknown."),
    d("career_end", "core", "Career timing", csv,
      paste("Age at which the working career ends: the earliest of a definite",
            "retirement, the year after the last job, and the age at the",
            "questionnaire (capped at 75)."),
      paste("The right edge of every other summary. It is an exit *event* only",
            "when career_end_source is \"retire\"; otherwise it is where",
            "observation stops. Never use it without career_end_source.")),
    d("career_end_source", "core", "Career timing", csv,
      "How career_end was determined: \"retire\", \"last_job_plus1\" or \"cap\".",
      paste("retire = retired (an event); cap = still working at the",
            "questionnaire (censored); last_job_plus1 = stopped working with no",
            "retirement record (unrecorded retirement, long unemployment or an",
            "incomplete report). Stratify or adjust for it; in time-to-retirement",
            "analyses treat \"cap\" as censored.")),
    d("retire_age", "core", "Career timing", csv,
      "Age at the first definite retirement record (a retirement gap with no work after it).",
      "NA if no definite retirement was reported. Can be later than career_end."),
    d("observed_retirement", "core", "Career timing", csv,
      "1 = a definite retirement was reported, 0 = not.",
      paste("Not the same as career_end_source == \"retire\": someone can report",
            "retirement years after their last job, so their career_end comes",
            "from the last job.")),
    d("work_years", "exposure", "Career timing", csv,
      "Number of years in paid work (at least one job) from age 16 to career_end.",
      "Denominator of every exposure share; the most direct 'career length'."),
    d("max_parallel", "core", "Career timing", csv,
      "Largest number of jobs held in any one year.",
      "2 or more means overlapping jobs or a same-year job change."),

    ## ── core：记录质量 ──────────────────────────────────────
    d("n_obs_years", "core", "Record quality", csv,
      "Years between age 16 and career_end covered by a job or gap record.",
      "Years with neither are blank, not 'not working'."),
    d("seq_len", "core", "Record quality", csv,
      "Years from the first to the last recorded age.", "Span of the record."),
    d("first_year", "core", "Record quality", csv,
      "First age with a job or gap record (an age, despite the name).",
      "Late values mean the early career was not reported."),
    d("last_year", "core", "Record quality", csv,
      "Last age with a job or gap record (an age).", "Normally equals career_end."),
    d("coverage", "core", "Record quality", csv,
      "n_obs_years / (career_end - 16 + 1): share of the working-age window that is documented.",
      "Values well below 1 mean large unreported stretches."),
    d("flag_incomplete", "core", "Record quality", csv,
      "1 if coverage < 0.5.",
      "Use for a sensitivity analysis that excludes these people."),

    ## ── exposure：10 个暴露（模板行，expand = TRUE 展开成 40 行） ──
    d("yrs_exp_<agent>", "exposure", "Workplace exposures", csv,
      "Working years with <agent> reported sometimes or often.",
      "Cumulative dose in years. 'Do not know' years count as 0 here."),
    d("yrs_high_<agent>", "exposure", "Workplace exposures", csv,
      "Working years with <agent> reported often.",
      "Cumulative dose at the highest level; the most specific of the four."),
    d("peak_<agent>", "exposure", "Workplace exposures", csv,
      "Highest level ever reported for <agent>: 0 never, 1 sometimes, 2 often.",
      "NA (not 0) when every answer was 'do not know'; 'never' and 'unknown' stay distinct."),
    d("mean_<agent>", "exposure", "Workplace exposures", csv,
      "Average level of <agent> over working years with a known answer (0 to 2).",
      "Intensity independent of career length."),
    d("exposure_breadth", "exposure", "Workplace exposures", csv,
      "Number of the 10 agents ever reported (sometimes or often), 0 to 10.",
      "A simple index of how hazardous the working life was overall."),
    d("high_exposure_years", "exposure", "Workplace exposures", csv,
      "Sum over the 10 agents of yrs_high_<agent> (agent-years at 'often').",
      "Total burden; can exceed work_years because agents co-occur."),
    d("longest_agent_years", "exposure", "Workplace exposures", csv,
      "Largest yrs_high_<agent> across the 10 agents.",
      "Longest sustained heavy exposure to any single agent."),

    ## ── exposure：工时与轮班 ────────────────────────────────
    d("long_hours_years", "exposure", "Working hours and shifts", csv,
      "Working years at 48 or more hours per week (main job of the year).",
      "48 h is the EU Working Time Directive limit."),
    d("long_hours_frac", "exposure", "Working hours and shifts", csv,
      "long_hours_years / work_years.", "Share of working life on long hours."),
    d("shift_years", "exposure", "Working hours and shifts", csv,
      "Working years with any shift work.", "Includes night shifts."),
    d("night_shift_years", "exposure", "Working hours and shifts", csv,
      "Working years with night shifts.", "The circadian-disruption measure."),
    d("shift_frac", "exposure", "Working hours and shifts", csv,
      "shift_years / work_years.", "Share of working life on shifts."),

    ## ── camsis：职业地位 ────────────────────────────────────
    d("camsis_mean", "camsis", "Occupational status (CAMSIS)", "a CAMSIS table",
      "Average CAMSIS score over working years.",
      "Higher = more advantaged occupational position. Lifetime status level."),
    d("camsis_first", "camsis", "Occupational status (CAMSIS)", "a CAMSIS table",
      "CAMSIS score of the first working year.", "Status at labour-market entry."),
    d("camsis_last", "camsis", "Occupational status (CAMSIS)", "a CAMSIS table",
      "CAMSIS score of the last working year.", "Status at the end of the career."),
    d("camsis_change", "camsis", "Occupational status (CAMSIS)", "a CAMSIS table",
      "camsis_last - camsis_first.", "Net intragenerational mobility."),
    d("camsis_slope", "camsis", "Occupational status (CAMSIS)", "a CAMSIS table",
      "Linear slope of CAMSIS on age (points per year).",
      "NA with fewer than 5 scored years or a span under 3 years (too short to fit)."),
    d("camsis_pattern", "camsis", "Occupational status (CAMSIS)", "a CAMSIS table",
      "\"rising\" / \"flat\" / \"declining\" (|slope| > 0.05 points per year).",
      "Easy-to-report trajectory category; NA when the slope is NA."),
    d("zero_slope", "camsis", "Occupational status (CAMSIS)", "a CAMSIS table",
      "1 if the CAMSIS score never changed (slope exactly 0).",
      "These people have a true flat trajectory, not a noisy estimate near 0."),
    d("camsis_n_years", "camsis", "Occupational status (CAMSIS)", "a CAMSIS table",
      "Number of working years with a CAMSIS score.",
      "Occupations missing from the CAMSIS table are not scored."),

    ## ── movement：职业流动 ──────────────────────────────────
    d("n_moves", "movement", "Occupational mobility", "the model bundle",
      "Number of changes of occupation code between working years.",
      "The backbone mobility measure. 0 for about a third of people (one code for life)."),
    d("mean_jump", "movement", "Occupational mobility", "the model bundle",
      paste("Average distance of a move in the occupation embedding (about 19 =",
            "within the same major group, about 37 = to a different one)."),
      "How far a person typically moves, independent of how often. NA if n_moves = 0 (undefined, not zero)."),
    d("max_single_jump", "movement", "Occupational mobility", "the model bundle",
      "Largest single move.", "NA if n_moves = 0."),
    d("net_displacement", "movement", "Occupational mobility", "the model bundle",
      "Distance between the first and the last occupation.",
      "How far the career ended from where it started. NA if n_moves = 0."),
    d("first_move_age", "movement", "Occupational mobility", "the model bundle",
      "Age at the first change of occupation.", "NA if n_moves = 0."),
    d("first_move_rel", "movement", "Occupational mobility", "the model bundle",
      "Years from the first working year to the first change.",
      "Preferred over first_move_age: less confounded by birth cohort."),
    d("last_move_age", "movement", "Occupational mobility", "the model bundle",
      "Age at the last change of occupation.", "NA if n_moves = 0."),

    ## ── type / latent ────────────────────────────────────────
    d("type", "type", "Career type", "the model bundle + torch + igraph",
      paste("Career type: clusters of similar whole careers found in *your*",
            "sample (Leiden, same settings as the paper), numbered 0, 1, ... per sex."),
      paste("Describe each type (plot_type_mix(), plot_chronogram()) before using",
            "it. Your types are not the paper's T0-T11 and the partition is only",
            "moderately stable; check the seed ARI and prefer the continuous",
            "measures above for primary analyses.")),
    d("z001 ... z192", "latent", "Career representation", "the model bundle + torch",
      "The 192-dimensional representation of the whole career (with include_latent = TRUE).",
      paste("For similarity, clustering or as a high-dimensional adjustment set.",
            "Individual dimensions have no meaning of their own."))
  )
  data.table::rbindlist(rows)
}


#' What each derived variable means, and how to use it
#'
#' A dictionary of every column that [ukb_career()] can return. For each
#' variable it gives a short description, what it needs, and advice on how to
#' interpret and use it.
#'
#' Look up one variable with `ukb_dictionary("career_end")`. Agent-specific
#' names such as `"yrs_high_asbestos"` are also recognised. List a whole group
#' with `ukb_dictionary(what = "exposure")`.
#'
#' The ten workplace exposures (`<agent>`) are `noisy`, `cold`, `hot`,
#' `dusty`, `fumes` (chemical or other fumes), `cigarette` (other people's
#' smoke), `asbestos`, `paints` (paints, thinners or glues), `pesticides` and
#' `diesel` (diesel exhaust), each answered per job as rarely/never, sometimes
#' or often (UK Biobank fields 22606-22615).
#'
#' @param variable Optional variable name(s) to look up.
#' @param what Optional variable group(s), as in the `what` argument of
#'   [ukb_career()]: `"core"`, `"exposure"`, `"camsis"`, `"movement"`,
#'   `"type"`, `"latent"`.
#' @param expand If `TRUE`, write out the four per-agent rows for each of the
#'   ten agents (40 rows) instead of one `<agent>` row each.
#' @return A `data.table` with columns `variable`, `what`, `group`, `needs`,
#'   `description` and `interpretation`. Printing it shows a readable summary.
#' @examples
#' ukb_dictionary()
#' ukb_dictionary("career_end_source")
#' ukb_dictionary(what = "exposure")
#' # as a plain table, e.g. to export for a data-analysis plan
#' as.data.frame(ukb_dictionary(expand = TRUE))[1:3, 1:4]
#' @export
ukb_dictionary <- function(variable = NULL, what = NULL, expand = FALSE) {
  ## 参数名与列名同名（what / variable）：进 d[...] 前先换成局部名，
  ## 否则 data.table 会把它们解析成列、筛选静默失效
  sel_what <- what
  sel_var <- variable
  d <- .dictionary_rows()
  if (expand || !is.null(sel_var)) d <- .expand_agents(d)
  if (!is.null(sel_what)) {
    bad <- setdiff(sel_what, unique(d$what))
    if (length(bad)) {
      stop("Unknown `what`: ", paste(bad, collapse = ", "), ". Choose from ",
           paste(unique(d$what), collapse = " / "))
    }
    d <- d[d$what %in% sel_what]
  }
  if (!is.null(sel_var)) {
    is_z <- grepl("^z[0-9]{3}$", sel_var)
    hit <- d$variable %in% sel_var | (any(is_z) & d$what == "latent")
    miss <- setdiff(sel_var[!is_z], d$variable)
    if (length(miss)) {
      warning("Not a ukbcareer variable: ", paste(miss, collapse = ", "),
              call. = FALSE)
    }
    d <- d[hit]
  }
  data.table::setattr(d, "class", c("ukbcareer_dictionary", class(d)))
  d[]
}


#' `<agent>` 模板行 → 10 个 agent 各一行
#' @noRd
.expand_agents <- function(d) {
  tpl <- grepl("<agent>", d$variable, fixed = TRUE)
  if (!any(tpl)) return(d)
  ## 原位展开，保持词典的行序
  data.table::rbindlist(lapply(seq_len(nrow(d)), function(i) {
    r <- d[i]
    if (!tpl[i]) return(r)
    data.table::rbindlist(lapply(names(AGENT_LABELS), function(a) {
      s <- data.table::copy(r)
      s[, variable := sub("<agent>", a, variable, fixed = TRUE)]
      s[, description := sub("<agent>", AGENT_LABELS[[a]], description,
                             fixed = TRUE)]
      s
    }))
  }))
}


#' @export
print.ukbcareer_dictionary <- function(x, ...) {
  w <- getOption("width", 80L)
  if (!nrow(x)) {
    cat("<no matching variables>\n")
    return(invisible(x))
  }
  if (nrow(x) <= 3L) {
    ## 单个变量：全文
    for (i in seq_len(nrow(x))) {
      cat(x$variable[i], "\n", sep = "")
      cat("  group: ", x$group[i], "  |  ukb_career(what = \"", x$what[i],
          "\")  |  needs ", x$needs[i], "\n", sep = "")
      cat(paste0("  ", strwrap(x$description[i], w - 4L), collapse = "\n"),
          "\n", sep = "")
      cat("  Use: ", paste(strwrap(x$interpretation[i], w - 9L),
                           collapse = "\n       "), "\n", sep = "")
      if (i < nrow(x)) cat("\n")
    }
    return(invisible(x))
  }
  cat(sprintf("<ukbcareer dictionary> %d variables", nrow(x)),
      "-- details: ukb_dictionary(\"<name>\")\n")
  for (g in unique(x$group)) {
    s <- x[x$group == g]
    cat("\n", g, "  [needs ", s$needs[1L], "]\n", sep = "")
    nw <- max(nchar(s$variable))
    for (i in seq_len(nrow(s))) {
      txt <- strtrim(s$description[i], max(w - nw - 6L, 20L))
      if (nchar(txt) < nchar(s$description[i])) {
        txt <- paste0(substr(txt, 1L, nchar(txt) - 3L), "...")
      }
      cat(sprintf("  %-*s  %s\n", nw, s$variable[i], txt))
    }
  }
  invisible(x)
}
