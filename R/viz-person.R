# ══════════════════════════════════════════════════════════════════
# 个人级可视化（一人一图）；完整的一页报告在 viz-report.R
# ══════════════════════════════════════════════════════════════════

#' Compact career timeline for one person
#'
#' A single strip along age coloured by the annual state (full-time,
#' part-time, unemployed, ...), with the occupation code written where it
#' changes, workplace exposures underneath and a dashed line at `career_end`.
#' For the full one-page picture use [plot_person()].
#'
#' @param x The result of [ukb_worklife()] or [ukb_career()].
#' @param eid The person to draw.
#' @param show_exposure Draw the exposure strips under the timeline.
#' @return A ggplot object.
#' @examples
#' wl <- ukb_worklife(ukb_synth(n = 40), verbose = FALSE)
#' plot_career(wl, eid = wl$cohort$eid[4])
#' @export
plot_career <- function(x, eid, show_exposure = TRUE) {
  .need_ggplot()
  wl <- .as_worklife(x)
  id <- eid
  a <- wl$annual[eid == id]
  if (!nrow(a)) stop("eid ", id, " has no person-years in the panel")
  co <- wl$cohort[eid == id]
  a <- data.table::copy(a)
  a[, state := .derive_state(a)]
  a[, state_name := factor(STATE_NAMES[state], levels = STATE_NAMES)]
  a[, soc_label := data.table::fifelse(in_job == 1L & !is.na(soc4),
                                       as.character(as.integer(soc4)), NA_character_)]
  ## 职业码变化处画分隔并标码
  a[, new_code := is.na(data.table::shift(soc_label)) |
      data.table::shift(soc_label) != soc_label]
  lab <- a[in_job == 1L & new_code == TRUE]

  ## 状态色带画在 y = 0 上（占 [-0.28, 0.28]），码标签在上方，暴露条在下方。
  ## y 轴范围按**实际画了什么**收紧 —— 固定 ylim 会在没有暴露的人身上留下
  ## 四成高度的空白，把色带压成顶部一条窄带。
  p <- ggplot2::ggplot(a, ggplot2::aes(x = age, y = 0, fill = state_name)) +
    ggplot2::geom_tile(width = 1, height = 0.56) +
    ggplot2::scale_fill_manual(values = STATE_COLOURS, drop = FALSE,
                               name = "state") +
    ggplot2::geom_vline(xintercept = co$career_end + 0.5, linetype = "dashed",
                        colour = "grey20", linewidth = 0.4)
  ylim_hi <- 0.34
  if (nrow(lab)) {
    p <- p + ggplot2::geom_text(data = lab,
                                ggplot2::aes(x = age, y = 0.36,
                                             label = soc_label),
                                inherit.aes = FALSE, size = 2, angle = 45,
                                hjust = 0, colour = "grey20")
    ylim_hi <- 0.78
  }
  ylim_lo <- -0.34
  n_agent_shown <- 0L
  if (show_exposure) {
    ec <- intersect(paste0("exp_", EXPOSURES), names(a))
    if (length(ec)) {
      lg <- data.table::melt(a[, c("age", ec), with = FALSE], id.vars = "age",
                            variable.name = "agent", value.name = "level")
      lg[, agent := sub("^exp_", "", agent)]
      lg <- lg[!is.na(level) & level > 0]
      if (nrow(lg)) {
        # **只为实际出现的 agent 留行**：按 EXPOSURES 的全局位置排行时，
        # 一个只有 diesel 暴露的人会在上方留九行空白，而下界按 10 行算
        # → 整幅图的信息被压到顶部一条窄带里（就是 P1 绝对图那个毛病）。
        shown <- EXPOSURES[EXPOSURES %in% unique(lg$agent)]
        n_agent_shown <- length(shown)
        lg[, yy := -0.42 - 0.13 * (match(agent, shown) - 1L)]
        ylim_lo <- -0.42 - 0.13 * n_agent_shown
        p <- p + ggplot2::geom_tile(
          data = lg, ggplot2::aes(x = age, y = yy, alpha = level),
          fill = "#b2182b", width = 1, height = 0.12, inherit.aes = FALSE) +
          ggplot2::scale_alpha_continuous(range = c(0.45, 1), breaks = c(1, 2),
                                          labels = c("Sometimes", "Often"),
                                          name = "exposure") +
          # agent 名画在**轴外左侧** → 必须 clip = "off" 且左边距留够，
          # 否则标签被面板裁掉，只剩前三个字母（实测 "pla" / "col" / "hot"）
          ggplot2::geom_text(
            data = unique(lg[, .(agent, yy)]),
            ggplot2::aes(x = min(a$age) - 0.8, y = yy, label = agent),
            inherit.aes = FALSE, hjust = 1, size = 1.8, colour = "grey30")
      }
    }
  }
  src <- co$career_end_source
  left_pad <- if (n_agent_shown > 0L) 34 else 5.5
  p + ggplot2::scale_y_continuous(breaks = NULL, name = NULL) +
    ggplot2::coord_cartesian(ylim = c(ylim_lo, ylim_hi), clip = "off") +
    ggplot2::scale_x_continuous(name = "age",
                                expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::labs(
      title = sprintf("eid %s (%s)", id, co$sex),
      subtitle = sprintf(
        "career_end = %.0f (source: %s); %d employed years; %d distinct occupation codes",
        co$career_end, src, sum(a$in_job == 1L),
        data.table::uniqueN(a[in_job == 1L, soc4])),
      caption = .cap(
        "Dashed line = career_end. With source = retire it is a substantive event; with cap / last_job_plus1 it is the edge of the observation window.",
        "Numbers above = 4-digit SOC2000 codes (shown only where the code changes).")) +
    ukb_theme(grid = FALSE) +
    ggplot2::theme(legend.position = "right",
                   plot.margin = ggplot2::margin(5.5, 5.5, 5.5, left_pad))
}


#' Annual state sequences for one or many people
#'
#' One row per person, one column per age, coloured by the nine annual
#' states. Useful for eyeballing a sample or a career type.
#'
#' @param x The result of [ukb_worklife()] or [ukb_career()], or a state
#'   matrix from [ukb_states()].
#' @param eid People to draw. `NULL` (default) draws everyone.
#' @param max_rows Maximum number of rows. Larger samples are thinned at
#'   equal intervals after sorting (not by taking the first rows, which would
#'   favour low eids).
#' @param sort_by Row order: `"length"` (years observed), `"first_state"` or
#'   `"none"`.
#' @details Rows are drawn as a raster image, so plots of thousands of people
#'   stay small and open quickly.
#' @return A ggplot object.
#' @examples
#' wl <- ukb_worklife(ukb_synth(n = 60), verbose = FALSE)
#' plot_states(wl)
#' @export
plot_states <- function(x, eid = NULL, max_rows = 500L, sort_by = "length") {
  .need_ggplot()
  st <- if (inherits(x, "ukbcareer_states")) x else ukb_states(.as_worklife(x))
  ids <- attr(st, "eid")
  if (!is.null(eid)) {
    keep <- match(eid, ids)
    if (anyNA(keep)) stop("These eids have no state sequence: ",
                          paste(eid[is.na(keep)], collapse = ", "))
    st <- st[keep, , drop = FALSE]
    ids <- ids[keep]
  }
  n_obs <- rowSums(!is.na(st))
  ord <- switch(sort_by,
                length = order(n_obs, decreasing = TRUE),
                first_state = order(apply(st, 1L, function(v) {
                  w <- which(!is.na(v))
                  if (length(w)) v[w[1L]] else 99L
                }), n_obs),
                seq_len(nrow(st)))
  st <- st[ord, , drop = FALSE]
  ids <- ids[ord]
  if (nrow(st) > max_rows) {
    # **等距抽样**：排序后等距取，首行与末行都保留，不偏向任何一端
    sel <- round(seq(1L, nrow(st), length.out = max_rows))
    st <- st[sel, , drop = FALSE]
    ids <- ids[sel]
  }
  ages <- as.integer(colnames(st))
  d <- data.table::data.table(
    row = rep(seq_len(nrow(st)), times = ncol(st)),
    age = rep(ages, each = nrow(st)),
    state = as.vector(st))
  d <- d[!is.na(state)]
  d[, state_name := factor(STATE_NAMES[state], levels = STATE_NAMES)]
  ggplot2::ggplot(d, ggplot2::aes(x = age, y = row, fill = state_name)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual(values = STATE_COLOURS, drop = FALSE,
                               name = "state") +
    ggplot2::scale_y_reverse(name = sprintf("%s people", fmt_n(nrow(st))),
                             breaks = NULL) +
    ggplot2::scale_x_continuous(name = "age",
                                expand = ggplot2::expansion(0)) +
    ggplot2::labs(
      title = "Nine-state annual sequences",
      caption = .cap(paste0("Blank = career already ended (past career_end): ",
                            "that is information, not missingness."),
                     if (nrow(st) < length(ids) || nrow(st) == max_rows)
                       "Rows were sorted and then sampled at equal intervals." else NULL)) +
    ukb_theme(grid = FALSE)
}


#' Occupational status (CAMSIS) trajectory for one person
#'
#' The person's own CAMSIS score by age, over the sample mean among those
#' employed at each age.
#'
#' The grey background curve is a *cross-sectional* profile (the mean among
#' people still working at that age), not anyone's longitudinal path. Because
#' lower-status workers tend to leave work earlier, it can rise with age even
#' if nobody is promoted.
#'
#' @param x A work-life object that has been through [ukb_camsis()].
#' @param eid The person to draw.
#' @return A ggplot object.
#' @export
plot_camsis_track <- function(x, eid) {
  .need_ggplot()
  wl <- .as_worklife(x)
  if (is.null(wl$camsis_by_age)) stop("Run ukb_camsis() first")
  id <- eid
  me <- wl$camsis_by_age[eid == id]
  if (!nrow(me)) stop("eid ", id, " has no CAMSIS record (possibly no employed years at all)")
  bg <- wl$camsis_by_age[, .(n = .N, mean_camsis = mean(camsis)), by = age]
  bg <- .suppress_small(bg, "n")
  sup <- attr(bg, "suppressed") %||% 0L
  ggplot2::ggplot() +
    ggplot2::geom_line(data = bg, ggplot2::aes(age, mean_camsis),
                       colour = "grey65", linewidth = 0.8) +
    ggplot2::geom_line(data = me, ggplot2::aes(age, camsis),
                       colour = UKB_SEX[[as.character(
                         wl$cohort[eid == id, sex])]], linewidth = 0.9) +
    ggplot2::geom_point(data = me, ggplot2::aes(age, camsis),
                        colour = UKB_SEX[[as.character(
                          wl$cohort[eid == id, sex])]], size = 1.2) +
    ggplot2::labs(
      x = "age", y = "CAMSIS",
      title = sprintf("Occupational status trajectory, eid %s", id),
      subtitle = sprintf("Thick line = mean among all employed at that age (%s)",
                         wl$cohort[eid == id, sex]),
      caption = .cap(
        paste0("The background curve is a CROSS-SECTIONAL profile (mean among those still working at that age), ",
               "not a longitudinal path."),
        paste0("Earlier exit by lower-status workers lifts it even when NOBODY is promoted."),
        paste0("CAMSIS has no native SOC2000 version; scores are approximated from SOC90."),
        suppressed = sup)) +
    ukb_theme()
}


#' @noRd
.as_worklife <- function(x) {
  if (inherits(x, "ukbcareer_worklife")) return(x)
  w <- attr(x, "worklife")
  if (inherits(w, "ukbcareer_worklife")) return(w)
  stop("Expected the result of ukb_worklife() or ukb_career()")
}
