# ══════════════════════════════════════════════════════════════════
# 样本级可视化（用户自己的队列）
# ══════════════════════════════════════════════════════════════════

#' Plot state shares by age, and their difference from the reference cohort
#'
#' Draws a chronogram (stacked share of each labour-market state at each age)
#' and, if a bundle is supplied, a second panel showing the percentage-point
#' difference from the reference cohort at the same age.
#'
#' **Read the two panels together.** Full-time work accounts for the vast
#' majority of working person-years, so on the absolute-share panel the
#' differences between types or groups are squeezed into a thin band at the
#' top and are almost invisible. The difference panel is where the
#' information is.
#'
#' @param x Output of [ukb_worklife] or [ukb_career].
#' @param by Grouping column (a column of `cohort`, e.g. `"sex"` or `"type"`).
#'   `NULL` means no grouping.
#' @param bundle Output of [ukb_bundle]. If supplied, the difference from the
#'   reference cohort is drawn as the second panel.
#' @param which `"both"`, `"absolute"` or `"difference"`.
#' @return A ggplot object (a patchwork composite when `which = "both"`).
#' @examples
#' wl <- ukb_worklife(ukb_synth(n = 200), verbose = FALSE)
#' plot_chronogram(wl, by = "sex", which = "absolute")
#' @export
plot_chronogram <- function(x, by = NULL, bundle = NULL,
                            which = c("both", "absolute", "difference")) {
  .need_ggplot()
  which <- match.arg(which)
  wl <- .as_worklife(x)
  a <- data.table::copy(wl$annual)
  a[, state := .derive_state(a)]
  a[, state_name := factor(STATE_NAMES[state], levels = STATE_NAMES)]
  if (!is.null(by)) {
    key <- .grp_col(wl, x, by)
    a[key, grp := i.grp, on = "eid"]
    a <- a[!is.na(grp)]
  } else {
    a[, grp := "all"]
  }
  d <- a[, .(n = .N), by = .(grp, age, state_name)]
  ## 补齐 (grp, age, state) 的 0 格：geom_area 在某龄缺一个状态时会在相邻年龄间
  ## 插值，堆叠总和因此冲出 100%
  full <- unique(d[, .(grp, age)])[, .(state_name = factor(STATE_NAMES, levels = STATE_NAMES)),
                                   by = .(grp, age)]
  d <- d[full, on = c("grp", "age", "state_name")]
  d[is.na(n), n := 0L]
  tot <- d[, .(tot = sum(n)), by = .(grp, age)]
  d <- merge(d, tot, by = c("grp", "age"))
  d[, frac := n / tot]
  ## 小样本抑制：某个 (grp, age) 的总人数太少时整列不画
  small <- tot[tot < MIN_CELL_PLOT]
  d <- d[!small, on = c("grp", "age")]
  sup <- nrow(small)

  p_abs <- ggplot2::ggplot(d, ggplot2::aes(age, frac, fill = state_name)) +
    ggplot2::geom_area(position = "stack") +
    ggplot2::scale_fill_manual(values = STATE_COLOURS, drop = FALSE,
                               name = "state") +
    ggplot2::scale_y_continuous(labels = function(v) paste0(100 * v, "%"),
                               expand = ggplot2::expansion(0)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(0)) +
    ggplot2::labs(x = "age", y = "share of those under observation",
                  title = "State composition (absolute shares)",
                  subtitle = "Full-time dominates, so between-group differences are invisible here") +
    ukb_theme(grid = FALSE)
  if (!is.null(by)) p_abs <- p_abs + ggplot2::facet_wrap(~ grp)

  if (which == "absolute" || is.null(bundle)) {
    if (which != "absolute") {
      message("No bundle given -> absolute shares only. The difference panel needs ",
              "the reference cohort baseline (in the bundle tables/P1)")
    }
    return(p_abs + ggplot2::labs(caption = .cap(suppressed = sup)))
  }

  ## 差值图：与参考队列同龄基线的百分点差
  ref <- .ref_chronogram(bundle)
  if (is.null(ref)) {
    message("The bundle has no P1_chronogram -> absolute shares only")
    return(p_abs)
  }
  dd <- merge(d, ref, by = c("age", "state_name"), all.x = TRUE)
  dd <- dd[!is.na(ref_frac)]
  dd[, diff := frac - ref_frac]
  p_dif <- ggplot2::ggplot(dd, ggplot2::aes(age, diff, colour = state_name)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::scale_colour_manual(values = STATE_COLOURS, drop = FALSE,
                                 name = "state") +
    ggplot2::scale_y_continuous(
      labels = function(v) sprintf("%+.0f", 100 * v)) +
    ggplot2::labs(x = "age", y = "difference vs reference (pp)",
                  title = "Difference from the reference cohort at the same age",
                  subtitle = "This is where between-group and between-study differences live",
                  caption = .cap(
                    "The reference baseline comes from the bundle P1_chronogram (our 117,590 people).",
                    "Deviation is not necessarily an error; it may be a real difference in sample selection.",
                    suppressed = sup)) +
    ukb_theme()
  if (!is.null(by)) p_dif <- p_dif + ggplot2::facet_wrap(~ grp)
  if (which == "difference") return(p_dif)
  .stack2(p_abs, p_dif)
}


#' Plot the number of occupation changes and the ages at first and last change
#'
#' Shows `n_moves` as a ring chart (one ring per sex) and the distributions of
#' `first_move_age` / `last_move_age` as density curves (movers only).
#'
#' **The 0 bin is drawn in grey, not in the colour scale.** It is the set of
#' people who reported a single occupation code for life, for whom distance
#' measures are undefined -- not people who "moved very little". Colouring it
#' with the lightest shade of an ordinal scale would wrongly present it as one
#' end of a continuum.
#'
#' @param x An object that has been through [ukb_movement].
#' @param cap Upper bin for `n_moves` (values at or above are pooled). The
#'   default of 5 reflects that 8+ changes make up only 0.5% of the reference
#'   cohort (1.8 degrees of the ring -- a single line).
#' @return A patchwork composite.
#' @export
plot_moves <- function(x, cap = 5L) {
  .need_ggplot()
  wl <- .as_worklife(x)
  if (is.null(wl$movement)) stop("Run ukb_movement() first")
  mv <- data.table::copy(wl$movement)
  mv[, k := pmin(n_moves, cap)]
  d <- mv[, .(n = .N), by = .(sex, k)]
  d[, frac := n / sum(n), by = sex]
  d[, lab := data.table::fifelse(k == cap, paste0(cap, "+"), as.character(k))]
  d[, lab := factor(lab, levels = c(as.character(0:(cap - 1L)),
                                    paste0(cap, "+")))]
  ## 0 档灰色 + 其余 Purples 浅→深
  pal <- c("0" = UKB_NA,
           stats::setNames(grDevices::colorRampPalette(
             c("#efedf5", "#3f007d"))(cap), levels(d$lab)[-1L]))
  p1 <- ggplot2::ggplot(d, ggplot2::aes(x = sex, y = frac, fill = lab)) +
    ggplot2::geom_col(width = 0.9, colour = "white", linewidth = 0.2) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::scale_fill_manual(values = pal, name = "occupation-code changes") +
    ggplot2::labs(x = NULL, y = NULL, title = "Distribution of occupation-code changes",
                  subtitle = "The 0 bin (grey) = one code for life, so distance measures are undefined") +
    ukb_theme(grid = FALSE) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank())

  lg <- data.table::melt(
    mv[n_moves > 0L, .(sex, first_move_age, last_move_age)],
    id.vars = "sex", variable.name = "what", value.name = "age")
  lg <- lg[!is.na(age)]
  lg[, what := factor(what, levels = c("first_move_age", "last_move_age"),
                      labels = c("first change", "last change"))]
  p2 <- ggplot2::ggplot(lg, ggplot2::aes(age, colour = sex, linetype = what)) +
    ggplot2::geom_density(linewidth = 0.6) +
    ggplot2::scale_colour_manual(values = UKB_SEX, name = NULL) +
    ggplot2::scale_linetype_manual(values = c(1, 2), name = NULL) +
    ggplot2::labs(x = "age", y = "density", title = "Age at first and last change",
                  subtitle = "Movers only",
                  caption = .cap(paste0(
                    "first/last_move_age are the ONLY mobility measures not based on distance, ",
                    "and are therefore almost unrelated to the number of changes (rho ~ 0)."))) +
    ukb_theme()
  .stack2(p1, p2)
}


#' Plot occupational exposure profiles: overall load and agent-specific residuals
#'
#' Left panel: the overall exposure load of each group (lollipop chart). Right
#' panel: a heatmap of each agent's residual **after removing the overall
#' level**. The two steps answer two different questions -- "which groups are
#' more exposed overall?" and "what is a given group unusually exposed to?".
#' In a single heatmap the first would swamp the second.
#'
#' By default the plot uses `mean_*` (duration-normalised) rather than
#' `peak_*`. A peak is a maximum, so **people with more working years have
#' more chances to hit a high value** -- that is an opportunity bias, not
#' exposure intensity.
#'
#' @param x Output of [ukb_career] (must include `exposure`).
#' @param by Grouping column.
#' @param metric `"mean"` (default, duration-normalised) or `"peak"`
#'   (reproduces the older definition).
#' @return A patchwork composite.
#' @export
plot_exposure <- function(x, by = "sex", metric = c("mean", "peak")) {
  .need_ggplot()
  metric <- match.arg(metric)
  d <- .as_result(x)
  pre <- paste0(metric, "_")
  cols <- intersect(paste0(pre, EXPOSURES), names(d))
  if (!length(cols)) {
    stop("No ", pre, "* columns found -- run ukb_career(what = \"exposure\") first")
  }
  key <- .grp_col(.as_worklife(x), x, by)
  dd <- merge(d[, c("eid", cols), with = FALSE], key, by = "eid")
  lg <- data.table::melt(dd, id.vars = c("eid", "grp"),
                         variable.name = "agent", value.name = "v")
  lg[, agent := sub(paste0("^", pre), "", agent)]
  lg <- lg[!is.na(v)]
  g <- lg[, .(n = .N, m = mean(v)), by = .(grp, agent)]
  g <- .suppress_small(g, "n")
  sup <- attr(g, "suppressed") %||% 0L

  ## 左：总体负荷
  tot <- g[, .(load = mean(m), n = sum(n)), by = grp]
  p1 <- ggplot2::ggplot(tot, ggplot2::aes(x = load,
                                          y = stats::reorder(grp, load))) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = load, yend = grp),
                          colour = "grey70", linewidth = 0.4) +
    ggplot2::geom_point(size = 2.4, colour = "#b2182b") +
    ggplot2::labs(x = sprintf("mean intensity across 10 agents (%s)",
                              if (metric == "mean") "duration-normalised" else "peak"),
                  y = NULL, title = "Overall exposure load") +
    ukb_theme()

  ## 右：去掉总体水平后的残差
  g[tot, load := i.load, on = "grp"]
  g[, agent_mean := mean(m), by = agent]
  g[, resid := m - load - agent_mean + mean(g$m)]
  lim <- max(abs(g$resid), na.rm = TRUE)
  p2 <- ggplot2::ggplot(g, ggplot2::aes(agent, grp, fill = resid)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7",
                                 high = "#b2182b", midpoint = 0,
                                 limits = c(-lim, lim), name = "residual") +
    ggplot2::labs(x = NULL, y = NULL, title = "Agent-specific residuals",
                  subtitle = "Net of group-level load and agent-level prevalence",
                  caption = .cap(
                    if (metric == "mean")
                      "Uses mean_* (duration-normalised). peak_* takes a max, so more working years means more chances to hit a high value -- an opportunity bias."
                    else
                      "Uses peak_* (legacy definition): OPPORTUNITY BIAS -- more working years means more chances to hit a high value.",
                    suppressed = sup)) +
    ukb_theme(grid = FALSE) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  .stack2(p1, p2, widths = c(1, 1.6), horizontal = TRUE)
}


#' Plot the career-type composition of your sample next to the reference cohort
#'
#' Bar chart of the share of each career type, by sex. If a bundle is
#' supplied, the reference cohort's composition is drawn alongside.
#'
#' **Your types are not the reference types.** Each set comes from running
#' Leiden on a different sample, so neither the numbering nor the meaning of
#' a type carries over. The two are shown side by side only to compare how
#' fine-grained the partitions are, not to compare type by type. See
#' [ukb_typology].
#'
#' @param x An object that has been through [ukb_typology].
#' @param bundle Output of [ukb_bundle]. If supplied, the reference cohort's
#'   composition is drawn as well.
#' @return A ggplot object.
#' @export
plot_type_mix <- function(x, bundle = NULL) {
  .need_ggplot()
  wl <- .as_worklife(x)
  if (is.null(wl$types)) stop("Run ukb_typology() first")
  d <- wl$types[, .(n = .N), by = .(sex, type)]
  d[, frac := n / sum(n), by = sex]
  d[, src := "your sample"]
  if (!is.null(bundle) && !is.null(bundle$reference_qc)) {
    rr <- list()
    for (s in names(bundle$reference_qc$by_sex)) {
      td <- bundle$reference_qc$by_sex[[s]]$type_distribution
      if (is.null(td)) next
      rr[[s]] <- data.table::data.table(
        sex = factor(s, levels = levels(d$sex)),
        type = as.integer(names(td)),
        frac = unlist(lapply(td, function(v) if (is.null(v)) NA_real_ else v)),
        n = NA_integer_, src = "reference cohort")
    }
    if (length(rr)) d <- data.table::rbindlist(c(list(d), rr), use.names = TRUE,
                                               fill = TRUE)
  }
  ggplot2::ggplot(d, ggplot2::aes(factor(type), frac, fill = src)) +
    ggplot2::geom_col(position = "dodge", width = 0.75) +
    ggplot2::facet_wrap(~ sex, scales = "free_x") +
    ggplot2::scale_fill_manual(values = c("your sample" = "#1d91c0",
                                          "reference cohort" = "grey70"), name = NULL) +
    ggplot2::scale_y_continuous(labels = function(v) paste0(100 * v, "%")) +
    ggplot2::labs(x = "type", y = "share of sex",
                  title = "Type composition",
                  caption = .cap(
                    paste0("YOUR TYPES ARE NOT THE REFERENCE TYPES. Each comes from Leiden re-run on a different sample; ",
                           "neither the numbering nor the meaning carries over --"),
                    "they are drawn side by side only to compare the granularity of the partition, not type by type.",
                    "See ?ukb_typology")) +
    ukb_theme()
}


#' Plot quality-control checks as deviations from the reference cohort
#'
#' Runs [ukb_qc] and plots, for every check that has a reference value
#' (`kind = "cohort"`), the relative deviation of your sample from the
#' reference cohort, coloured by status. A `warn` is not necessarily an error
#' -- it may be a real difference between samples, but you should be able to
#' explain it. A `fail` must be resolved: it means your definitions differ
#' from the paper's and the results are not comparable.
#'
#' @param x Output of [ukb_worklife] or [ukb_career].
#' @param bundle Output of [ukb_bundle]; needed to supply the reference values.
#' @return A ggplot object.
#' @export
plot_qc <- function(x, bundle = NULL) {
  .need_ggplot()
  q <- ukb_qc(x, bundle, quiet = TRUE)
  q <- q[!is.na(value_num) & !is.na(reference) & kind == "cohort"]
  if (!nrow(q)) stop("Nothing to plot -- a bundle is needed to supply reference values")
  q[, rel := (value_num - reference) / pmax(abs(reference), 1e-9)]
  q[, status := factor(status, levels = c("ok", "warn", "fail", "info"))]
  ggplot2::ggplot(q, ggplot2::aes(x = rel, y = stats::reorder(check, rel),
                                  colour = status)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = rel, yend = check),
                          linewidth = 0.4) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_colour_manual(
      values = c(ok = "#1b7837", warn = "#e6ab02", fail = "#b2182b",
                 info = "grey60"), name = NULL, drop = FALSE) +
    ggplot2::scale_x_continuous(labels = function(v) sprintf("%+.0f%%", 100 * v)) +
    ggplot2::labs(x = "relative deviation from reference", y = NULL,
                  title = "Definition QC: deviation from the reference cohort",
                  caption = .cap(
                    "Only checks with a reference value are shown (kind = cohort).",
                    "A warn is not necessarily an error; it may be a real sample difference, but it must be explainable.",
                    "A fail must be resolved: it means the definitions differ from the paper and results are not comparable.")) +
    ukb_theme()
}


# ── 内部工具 ──────────────────────────────────────────────────────

#' 取分组列（可以来自 cohort、types 或结果宽表）
#' @noRd
.grp_col <- function(wl, x, by) {
  if (length(by) != 1L) stop("`by` must be a single column name")
  src <- NULL
  if (by %in% names(wl$cohort)) {
    src <- wl$cohort[, c("eid", by), with = FALSE]
  } else if (!is.null(wl$types) && by %in% names(wl$types)) {
    src <- wl$types[, c("eid", by), with = FALSE]
  } else if (inherits(x, "data.frame") && by %in% names(x)) {
    src <- data.table::as.data.table(x)[, c("eid", by), with = FALSE]
  }
  if (is.null(src)) stop("Grouping column `", by, "` not found")
  data.table::setnames(src, by, "grp")
  src[, grp := as.factor(grp)]
  src[]
}


#' 结果宽表
#' @noRd
.as_result <- function(x) {
  if (inherits(x, "data.frame")) return(data.table::as.data.table(x))
  wl <- .as_worklife(x)
  d <- data.table::copy(wl$cohort)
  if (!is.null(wl$exposure)) d <- merge(d, wl$exposure, by = "eid", all.x = TRUE)
  d
}


#' 参考队列的 chronogram 基线
#'
#' `P1_chronogram_{sex}.csv` 是**长表**：`type, age, state, frac, cohort_frac,
#' diff`。`cohort_frac` 已经是全队列的同龄基线（不分型），所以直接取它、
#' 按 (age, state) 去重即可，**不要对 `frac` 按型求平均** —— 那会按型的个数
#' 而不是按人数加权。
#'
#' 注意它有一层 `ended`（职业生涯已结束），而本包用 NA 表示那件事，
#' 所以这一层要排除，否则差值图上会多出一条无对应的线。
#' @noRd
.ref_chronogram <- function(bundle) {
  fs <- Sys.glob(file.path(bundle$path, "tables", "P1_chronogram_*.csv"))
  if (!length(fs)) return(NULL)
  out <- list()
  for (f in fs) {
    d <- data.table::fread(f, showProgress = FALSE)
    data.table::setnames(d, tolower(names(d)))
    if (!all(c("age", "state", "cohort_frac") %in% names(d))) next
    out[[f]] <- unique(d[, .(age, state = as.character(state),
                             ref_frac = cohort_frac)],
                       by = c("age", "state"))
  }
  if (!length(out)) return(NULL)
  r <- data.table::rbindlist(out)
  # 两性各一份 → 按 (age, state) 取平均（两性人数相近，这里只作视觉基线）
  r <- r[, .(ref_frac = mean(ref_frac, na.rm = TRUE)), by = .(age, state)]
  r <- r[state %in% STATE_NAMES]          # 排除 `ended`（本包用 NA 表示）
  r[, state_name := factor(state, levels = STATE_NAMES)]
  r[, state := NULL]
  r[]
}


#' 两图上下（或左右）拼
#' @noRd
.stack2 <- function(p1, p2, widths = NULL, horizontal = FALSE) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    message("Install patchwork to combine the panels: ",
            "install.packages(\"patchwork\"). Returning the first panel only.")
    return(p1)
  }
  if (horizontal) {
    patchwork::wrap_plots(p1, p2, nrow = 1L, widths = widths)
  } else {
    patchwork::wrap_plots(p1, p2, ncol = 1L, heights = widths)
  }
}
