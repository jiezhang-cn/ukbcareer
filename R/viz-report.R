# ══════════════════════════════════════════════════════════════════
# 个人工作史全景报告（一人一页）
# ══════════════════════════════════════════════════════════════════
# 版式照论文 Fig 1a（codes/py/fig_concept.py 的 panel_a）：每人年一格，逐行是
# 编码器看到的各个通道。通道值按 .tokenize() 的同一套规则算，但**不需要 bundle**：
# dt 与 SOC 层级都只靠年度面板与码的位数截断就能得到。
#
# 画法上的三个约定（与 fig_concept 一致）：
#   · 不在职的年在 SOC 与暴露行上画**斜纹**而不是留白或填 0 —— 那里的值是
#     "未定义"，不是"零"（这正是包里暴露汇总只在在职年上累加的原因）；
#   · career_end 之后是 PAD（浅灰底），模型不看、汇总不算；
#   · 全部填色走 identity 标度，图例单独画成一个面板 —— 一张 ggplot 只能有一个
#     fill 标度，而这里有状态、职业大类、YlGnBu 三套色。

## RColorBrewer 的 Purples / Greys（9 级）。不引 RColorBrewer 依赖，直接写死。
PURPLES <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba",
             "#6a51a3", "#54278f", "#3f007d")
GREYS <- c("#ffffff", "#f0f0f0", "#d9d9d9", "#bdbdbd", "#969696", "#737373",
           "#525252", "#252525", "#000000")
REPORT_RED <- "#b2182b"
REPORT_PAD <- "#f0f0f0"
REPORT_INK <- "#222222"
REPORT_HATCH <- "#9a9a9a"
COL_TRANSITION <- "#d95f02"
COL_PARALLEL <- "#7570b3"
COL_DT <- "#1a7f37"

## SOC2000 各层的类数（不含 PAD）。论文图上的 82 / 26 / 10 是词表大小，多出的 1 是 PAD。
SOC_SIZES <- c(soc_l4 = 353L, soc_l3 = 81L, soc_l2 = 25L, soc_l1 = 9L)

INTENS_LABELS <- c(EXPOSURES, "hours / 48", "shift level / 2",
                   "don't-know share")


#' Whole-career report for one person
#'
#' Draws one person's entire work history on a single page, one column per
#' year of age, in the layout of Figure 1a of the accompanying paper. It is the
#' quickest way to check what the package made of a person's raw UK Biobank
#' records -- and to show collaborators what the derived variables mean.
#'
#' From top to bottom:
#'
#' * **Summary** (title block): the person-level derived variables -- when the
#'   career ended and why, working years, number of occupations and changes,
#'   exposure breadth, shift and long-hours share, plus CAMSIS, mobility and
#'   career type when those have been computed.
#' * **state**: the annual state (full-time, part-time, unemployed, home and
#'   family, education, marginal work, health, retired, other, unknown).
#' * **SOC 4-digit to 1-digit**: the occupation code at the four levels of the
#'   SOC2000 hierarchy. Consecutive years in the same code merge into one
#'   labelled block, so a move within the same minor group (say 4123 to 4122)
#'   shows as two blocks on the 4-digit row but one block on the rows above.
#' * **gap code**: the UK Biobank gap code (103 education, 105 home/family,
#'   106 illness or disability, 108 retirement, ...) in years not in work.
#' * **age**: the age axis the model uses (one learned position per year).
#' * **dt**: tenure in the current occupation code, `min(tenure, 3) / 3`. A
#'   break does not reset it (returning to the same job continues the spell); a
#'   new code does.
#' * **jobs**: number of jobs held that year, with markers for a
#'   *transition* year (one job ends as another starts) and a *parallel* year
#'   (two jobs held at the same time).
#' * **exposure intensity**: the ten workplace exposures (never / sometimes /
#'   often), weekly hours / 48, shift level / 2 (none / shifts / including
#'   nights) and the share of exposures answered "do not know" (a `?` marks
#'   which ones). Every row is drawn on its own 0-to-maximum scale, so the
#'   darkest colour means "often", 48+ hours or night shifts respectively.
#'
#' Hatched cells are years not in work, where occupation and exposures are
#' *undefined* rather than zero. The red dashed line is `career_end`; the grey
#' area after it (`PAD`) is not used by the model or by any summary. Blank
#' columns are years with no job or gap record at all.
#'
#' @param x The result of [ukb_worklife()] or [ukb_career()]. Results of
#'   [ukb_camsis()], [ukb_movement()] and [ukb_typology()] are picked up and
#'   shown in the summary when present.
#' @param eid The person to draw.
#' @param age_range Ages to show, `c(from, to)`. Default: 16 to 65, extended
#'   to the career end when it is later.
#' @param show_tokens Draw the model-specific markers (`[CLS_W]`, `PAD`).
#'   Set `FALSE` for a plainer figure.
#' @param base_size Base font size in points.
#' @return A patchwork object (print it, or save it with [ukb_save()]; a good
#'   size is `width = 9, height = 8.5`). Needs ggplot2 and patchwork.
#' @seealso [ukb_report()] to write reports for many people to one PDF;
#'   [ukb_dictionary()] for the definitions of the summary variables.
#' @examples
#' d  <- ukb_synth(n = 60)
#' wl <- ukb_worklife(d, verbose = FALSE)
#' if (requireNamespace("patchwork", quietly = TRUE)) {
#'   p <- plot_person(wl, eid = attr(d, "showcase_eid"))
#'   # ukb_save(p, "person.pdf", width = 9, height = 8.5)
#' }
#' @export
plot_person <- function(x, eid, age_range = NULL, show_tokens = TRUE,
                        base_size = 8) {
  .need_ggplot()
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("plot_person() needs patchwork: install.packages(\"patchwork\")")
  }
  if (length(eid) != 1L) stop("`eid` must be a single person; see ukb_report() for many")
  wl <- .as_worklife(x)
  id <- eid
  co <- wl$cohort[eid == id]
  if (!nrow(co)) stop("eid ", id, " is not in the data")
  a <- data.table::copy(wl$annual[eid == id])
  if (!nrow(a)) stop("eid ", id, " has no person-years (no job or gap record inside the age window)")

  m <- attr(wl, "ukbcareer")
  ce <- co$career_end
  lo <- if (is.null(age_range)) m$age_min %||% DEFAULTS$age_min else age_range[1L]
  hi <- if (is.null(age_range)) {
    max(65, min(ce + 3, m$age_max %||% DEFAULTS$age_max))
  } else age_range[2L]
  a <- a[age >= lo & age <= hi]
  if (!nrow(a)) stop("eid ", id, " has no person-years between ages ", lo, " and ", hi)

  a[, state := .derive_state(a)]
  a <- .add_tenure_and_hazard(a)
  a[, in_job := as.integer(in_job == 1L)]

  tracks <- .report_tracks(a, lo, hi, ce, show_tokens, base_size)
  expo <- .report_exposure(a, lo, hi, ce, show_tokens, base_size)
  leg <- .report_legend(base_size)
  s <- .person_summary(x, wl, id, a)

  patchwork::wrap_plots(tracks, expo, leg, ncol = 1L,
                        heights = c(attr(tracks, "height"),
                                    attr(expo, "height"), 1.25)) +
    patchwork::plot_annotation(
      title = s$title, subtitle = s$subtitle,
      caption = paste0(
        "Hatched = not in work (occupation and exposures undefined, not zero). ",
        "Blank = no record. Red dashed line = career_end; grey area after it is not analysed.\n",
        "Exposure, hours and shift rows show the main job of each year (longest hours); ",
        "each row is scaled to its maximum (exposures: often; hours: 48 h/week; shift: night shifts). ",
        "? = answered 'do not know'."),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = base_size + 3, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = base_size, colour = "grey20",
                                              lineheight = 1.25,
                                              family = "mono"),
        plot.caption = ggplot2::element_text(size = base_size - 1.5,
                                             colour = "grey35", hjust = 0)))
}


#' Write whole-career reports for several people
#'
#' Calls [plot_person()] for each `eid` and writes the pages to one
#' multi-page PDF (or, for a single person, to a PNG if `file` ends in
#' `.png`). Handy for eyeballing a random sample before analysis.
#'
#' @inheritParams plot_person
#' @param eid One or more eids. Use `sample(wl$cohort$eid, 20)` for a random
#'   check.
#' @param file Output path, `.pdf` (any number of people) or `.png` (one
#'   person).
#' @param width,height Page size in inches.
#' @param ... Passed on to [plot_person()].
#' @return The file path, invisibly.
#' @examples
#' \dontrun{
#' wl <- ukb_worklife(ukb_synth(n = 60), verbose = FALSE)
#' ukb_report(wl, eid = wl$cohort$eid[1:5], file = "career_reports.pdf")
#' }
#' @export
ukb_report <- function(x, eid, file, width = 9, height = 8.5, ...) {
  .need_ggplot()
  ext <- tolower(tools::file_ext(file))
  if (ext == "png") {
    if (length(eid) != 1L) stop("A PNG holds one person; use a .pdf file for several")
    return(ukb_save(plot_person(x, eid, ...), file, width = width,
                    height = height))
  }
  if (ext != "pdf") stop("`file` must end in .pdf or .png")
  dev <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
  dev(file, width = width, height = height, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  skipped <- character(0)
  for (id in eid) {
    p <- tryCatch(plot_person(x, id, ...), error = function(e) {
      skipped <<- c(skipped, sprintf("%s (%s)", id, conditionMessage(e)))
      NULL
    })
    if (!is.null(p)) print(p)
  }
  if (length(skipped)) {
    warning(sprintf("%d of %d eids skipped: ", length(skipped), length(eid)),
            paste(utils::head(skipped, 5L), collapse = "; "), call. = FALSE)
  }
  invisible(file)
}


# ── 各面板 ─────────────────────────────────────────────────────────

#' 上半部分：state / SOC 四层 / gap / age / dt / jobs
#' @noRd
.report_tracks <- function(a, lo, hi, ce, show_tokens, base_size) {
  rows <- .report_rows(
    key = c("state", "soc_l4", "soc_l3", "soc_l2", "soc_l1", "gap", "age",
            "dt", "jobs"),
    h = c(0.62, 0.62, 0.42, 0.42, 0.42, 0.42, 0.42, 0.80, 0.80),
    gap_before = c(0, 0.10, 0, 0, 0, 0.14, 0, 0.14, 0),
    label = c("state (10)", sprintf("SOC 4-digit (%d)", SOC_SIZES[["soc_l4"]]),
              sprintf("SOC 3-digit (%d)", SOC_SIZES[["soc_l3"]]),
              sprintf("SOC 2-digit (%d)", SOC_SIZES[["soc_l2"]]),
              sprintf("SOC 1-digit (%d)", SOC_SIZES[["soc_l1"]]),
              "gap code", "age", "dt = min(tenure, 3) / 3",
              "jobs + flags"))
  R <- function(k) rows[rows$track == k]
  rect <- list()
  txt <- list()
  hat <- list()
  fs <- base_size * 0.72 / ggplot2::.pt

  ## state
  r <- R("state")
  rect$state <- data.table::data.table(
    xmin = a$age, xmax = a$age + 1, ymin = r$y0, ymax = r$y1,
    fill = unname(STATE_COLOURS[a$state]), colour = "white")

  ## SOC 四层：连续同码合并成块
  job <- a[in_job == 1L & !is.na(soc4)]
  l1 <- job$soc4 %/% 1000
  for (lv in c("soc_l4", "soc_l3", "soc_l2", "soc_l1")) {
    r <- R(lv)
    b <- .runs(a$age, data.table::fifelse(a$in_job == 1L,
                                          .soc_level(as.numeric(a$soc4), lv),
                                          NA_real_))
    if (nrow(b)) {
      major <- .soc_level(a$soc4[match(b$a0, a$age)], "soc_l1")
      fill <- .ramp(PURPLES, 0.18 + 0.09 * major)
      rect[[lv]] <- data.table::data.table(xmin = b$a0, xmax = b$a1 + 1,
                                           ymin = r$y0, ymax = r$y1,
                                           fill = fill, colour = "white")
      txt[[lv]] <- data.table::data.table(
        x = (b$a0 + b$a1 + 1) / 2, y = (r$y0 + r$y1) / 2,
        label = as.character(b$val), colour = .txt_on(fill),
        size = fs * data.table::fifelse(b$a1 > b$a0 | lv == "soc_l1", 1, 0.8))
    }
    off <- a[in_job == 0L]
    if (nrow(off)) hat[[lv]] <- .hatch(off$age, r$y0, 1, r$h)
  }

  ## gap 码：只在不在职的年有值
  r <- R("gap")
  b <- .runs(a$age, data.table::fifelse(a$in_job == 0L,
                                        as.numeric(a$gap_code), NA_real_))
  if (nrow(b)) {
    st <- a$state[match(b$a0, a$age)]
    fill <- .blend(unname(STATE_COLOURS[st]), 0.85)
    rect$gap <- data.table::data.table(xmin = b$a0, xmax = b$a1 + 1,
                                       ymin = r$y0, ymax = r$y1, fill = fill,
                                       colour = "white")
    txt$gap <- data.table::data.table(
      x = (b$a0 + b$a1 + 1) / 2, y = (r$y0 + r$y1) / 2,
      label = as.character(b$val), colour = .txt_on(fill),
      size = fs * data.table::fifelse(b$a1 > b$a0, 1, 0.75))
  }
  on <- a[in_job == 1L]
  if (nrow(on)) hat$gap <- .hatch(on$age, r$y0, 1, r$h)

  ## age：灰阶，深浅 = 在 [lo, career_end] 里走了多远
  r <- R("age")
  fr <- (a$age - lo) / max(ce - lo, 1)
  rect$age <- data.table::data.table(xmin = a$age, xmax = a$age + 1,
                                     ymin = r$y0, ymax = r$y1,
                                     fill = .ramp(GREYS, 0.12 + 0.5 * pmin(fr, 1)),
                                     colour = "white")

  ## dt 与 jobs 的外框
  for (k in c("dt", "jobs")) {
    r <- R(k)
    rect[[paste0(k, "_box")]] <- data.table::data.table(
      xmin = lo, xmax = ce + 1, ymin = r$y0, ymax = r$y1, fill = "white",
      colour = "grey70")
  }
  ## dt 阶梯线（按连续年份分段，空白年断开）
  r <- R("dt")
  a[, run := cumsum(c(TRUE, diff(age) != 1))]
  dt_line <- a[, .(x = c(rbind(age, age + 1)),
                   y = rep(r$y1 - 0.03 - dt * (r$h - 0.06), each = 2L)),
               by = run]
  ## jobs：柱高 = 当年工作数，封顶按 max(2, 实际最大值)
  r <- R("jobs")
  nj <- a$n_jobs_year
  nj[is.na(nj)] <- 0
  top <- max(2, nj)
  jb <- a[nj > 0]
  if (nrow(jb)) {
    fr <- nj[nj > 0] / top
    ## 柱子只占下方 70%，上方留给 transition / parallel 标记
    rect$jobs <- data.table::data.table(
      xmin = jb$age + 0.18, xmax = jb$age + 0.82,
      ymin = r$y1 - 0.04 - fr * 0.70 * (r$h - 0.08), ymax = r$y1 - 0.04,
      fill = "#bdbdbd", colour = NA)
  }
  flags <- rbind(
    a[is_transition == 1L, .(x = age + 0.5, kind = "transition")],
    a[is_parallel == 1L, .(x = age + 0.5, kind = "parallel")])
  flags[, `:=`(y = r$y0 + 0.13,
               col = data.table::fifelse(kind == "transition", COL_TRANSITION,
                                         COL_PARALLEL))]

  ## [CLS_W]：位置 0，只是提示"序列级摘要从这里读出"
  y_bottom <- max(rows$y1)
  if (show_tokens) {
    top_r <- R("state")
    bot_r <- R("soc_l1")
    rect$cls <- data.table::data.table(xmin = lo - 1.5, xmax = lo - 0.2,
                                       ymin = top_r$y0, ymax = bot_r$y1,
                                       fill = "#303030", colour = NA)
  }
  rect <- data.table::rbindlist(rect, use.names = TRUE, fill = TRUE)
  txt <- data.table::rbindlist(txt, use.names = TRUE, fill = TRUE)
  hat <- data.table::rbindlist(hat, use.names = TRUE, fill = TRUE)

  p <- .report_base(lo, hi, ce, y_bottom, show_tokens) +
    ggplot2::geom_rect(data = rect, ggplot2::aes(xmin = xmin, xmax = xmax,
                                                 ymin = ymin, ymax = ymax,
                                                 fill = fill, colour = colour),
                       linewidth = 0.25)
  if (nrow(hat)) {
    p <- p + ggplot2::geom_segment(data = hat,
                                   ggplot2::aes(x = x, y = y, xend = xend,
                                                yend = yend),
                                   colour = REPORT_HATCH, linewidth = 0.2)
  }
  p <- p + ggplot2::geom_path(data = dt_line,
                              ggplot2::aes(x = x, y = y, group = run),
                              colour = COL_DT, linewidth = 0.55,
                              linejoin = "mitre")
  if (nrow(txt)) {
    p <- p + ggplot2::geom_text(data = txt, ggplot2::aes(x = x, y = y,
                                                         label = label,
                                                         colour = colour,
                                                         size = size))
  }
  if (nrow(flags)) {
    p <- p + ggplot2::geom_point(
      data = flags, ggplot2::aes(x = x, y = y, shape = kind, fill = col,
                                 colour = col),
      size = 1.6, stroke = 0.2) +
      ggplot2::scale_shape_manual(values = c(transition = 25, parallel = 22),
                                  guide = "none")
  }
  if (show_tokens) {
    top_r <- R("state")
    bot_r <- R("soc_l1")
    p <- p + ggplot2::annotate("text", x = lo - 0.85,
                               y = (top_r$y0 + bot_r$y1) / 2, label = "[CLS_W]",
                               angle = 90, colour = "white", size = fs)
    if (hi - ce >= 2) {
      soc_r <- R("soc_l4")
      p <- p + ggplot2::annotate("text", x = (ce + 1 + hi + 1) / 2,
                                 y = (soc_r$y0 + soc_r$y1) / 2, label = "PAD",
                                 colour = "grey55", size = fs * 1.1)
    }
  }
  p <- p +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_size_identity() +
    .report_y(rows, y_bottom)
  p <- .report_theme(p, base_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank())
  attr(p, "height") <- y_bottom + 0.1
  p
}


#' 下半部分：13 行暴露强度热图
#' @noRd
.report_exposure <- function(a, lo, hi, ce, show_tokens, base_size) {
  h <- 0.40
  rows <- .report_rows(key = INTENS_LABELS, h = rep(h, 13L),
                       gap_before = rep(0, 13L), label = INTENS_LABELS)
  y_bottom <- max(rows$y1)
  on <- a[in_job == 1L]
  off <- a[in_job == 0L]
  fs <- base_size * 0.72 / ggplot2::.pt

  cells <- list()
  dk_marks <- list()
  if (nrow(on)) {
    dk <- numeric(nrow(on))
    for (j in seq_along(EXPOSURES)) {
      cc <- paste0("exp_", EXPOSURES[j])
      v <- if (cc %in% names(on)) as.numeric(on[[cc]]) else rep(NA_real_, nrow(on))
      dk <- dk + is.na(v)
      if (any(is.na(v))) {
        dk_marks[[j]] <- data.table::data.table(x = on$age[is.na(v)] + 0.5,
                                                y = rows$y0[j] + h / 2)
      }
      v[is.na(v)] <- 0
      ## 每行按自身上限归一到 0–1（agent 除以 2）：否则 shift = 2（夜班）与
      ## 工时 48h 会被画成与 agent "sometimes" 同一个颜色
      cells[[j]] <- data.table::data.table(age = on$age, j = j, v = v / 2)
    }
    sl <- as.numeric(on$shift_level)
    sl[is.na(sl)] <- 0
    cells[[11L]] <- data.table::data.table(age = on$age, j = 11L,
                                           v = .norm_hours(on$hours))
    cells[[12L]] <- data.table::data.table(age = on$age, j = 12L, v = sl / 2)
    cells[[13L]] <- data.table::data.table(age = on$age, j = 13L,
                                           v = dk / N_EXPOSURE)
  }
  cells <- data.table::rbindlist(cells)
  dk_marks <- data.table::rbindlist(dk_marks)
  if (nrow(cells)) {
    cells[, `:=`(xmin = age, xmax = age + 1, ymin = rows$y0[j],
                 ymax = rows$y1[j])]
  }
  hat <- if (nrow(off)) {
    data.table::rbindlist(lapply(seq_len(13L), function(j) {
      .hatch(off$age, rows$y0[j], 1, h)
    }))
  } else NULL

  p <- .report_base(lo, hi, ce, y_bottom, show_tokens)
  if (nrow(cells)) {
    p <- p + ggplot2::geom_rect(
      data = cells, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin,
                                 ymax = ymax, fill = v),
      colour = "white", linewidth = 0.2)
  }
  if (!is.null(hat)) {
    p <- p + ggplot2::geom_segment(data = hat,
                                   ggplot2::aes(x = x, y = y, xend = xend,
                                                yend = yend),
                                   colour = REPORT_HATCH, linewidth = 0.2)
  }
  if (nrow(dk_marks)) {
    p <- p + ggplot2::geom_text(data = dk_marks, ggplot2::aes(x = x, y = y),
                                label = "?", colour = "grey35",
                                size = fs * 0.95)
  }
  p <- p + ggplot2::annotate("text", x = ce + 1.35, y = y_bottom * 0.5,
                             label = "career end", angle = 90, hjust = 0.5,
                             vjust = 1, colour = REPORT_RED, size = fs)
  p <- p +
    ggplot2::scale_fill_gradientn(
      colours = UKB_SEQ, limits = c(0, 1), oob = .squish,
      breaks = c(0, 0.5, 1),
      labels = c("never / none", "sometimes", "often / max"),
      name = "share of
row maximum",
      guide = ggplot2::guide_colourbar(barheight = ggplot2::unit(2.4, "cm"),
                                       barwidth = ggplot2::unit(0.25, "cm"))) +
    .report_y(rows, y_bottom, title = "exposure intensity (13)")
  p <- .report_theme(p, base_size) +
    ggplot2::theme(legend.position = "right",
                   axis.title.y = ggplot2::element_text(
                     size = base_size, face = "bold", angle = 90))
  attr(p, "height") <- y_bottom + 0.1
  p
}


#' 图例面板：状态字母表 + 标记含义
#' @noRd
.report_legend <- function(base_size) {
  fs <- base_size * 0.78 / ggplot2::.pt
  k <- seq_along(STATE_NAMES) - 1L
  st <- data.table::data.table(
    x = (k %% 5L) * 2.2, y = (k %/% 5L) * 0.55,
    fill = unname(STATE_COLOURS), label = sprintf("%d %s", k + 1L, STATE_NAMES))
  y3 <- 1.25
  sw <- data.table::data.table(x = c(0, 2.2, 4.4, 6.6), y = y3)
  ## 第三行的标签要短：列宽 2.2 只够约 20 个字符
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = st, ggplot2::aes(xmin = x, xmax = x + 0.28,
                                               ymin = y - 0.16, ymax = y + 0.16,
                                               fill = fill)) +
    ggplot2::geom_text(data = st, ggplot2::aes(x = x + 0.38, y = y, label = label),
                       hjust = 0, size = fs) +
    ## 第三行：斜纹 / transition / parallel / career_end
    ggplot2::annotate("rect", xmin = 0, xmax = 0.28, ymin = y3 - 0.16,
                      ymax = y3 + 0.16, fill = "white", colour = "grey70",
                      linewidth = 0.2) +
    ggplot2::geom_segment(data = .hatch(0, y3 - 0.16, 0.28, 0.32),
                          ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                          colour = REPORT_HATCH, linewidth = 0.2) +
    ggplot2::annotate("point", x = 2.34, y = y3, shape = 25, size = 1.8,
                      fill = COL_TRANSITION, colour = COL_TRANSITION) +
    ggplot2::annotate("point", x = 4.54, y = y3, shape = 22, size = 1.8,
                      fill = COL_PARALLEL, colour = COL_PARALLEL) +
    ggplot2::annotate("segment", x = 6.74, xend = 6.74, y = y3 - 0.2,
                      yend = y3 + 0.2, colour = REPORT_RED, linetype = "22",
                      linewidth = 0.5) +
    ggplot2::annotate("text", x = sw$x + 0.38, y = y3, hjust = 0, size = fs,
                      label = c("not in work", "transition year",
                                "parallel jobs", "career end")) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_cartesian(xlim = c(-0.1, 10.9), clip = "off") +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(plot.margin = ggplot2::margin(4, 5.5, 0, 5.5))
  p
}


# ── 共用骨架 ───────────────────────────────────────────────────────

#' PAD 底色 + career_end 虚线 + x 范围
#' @noRd
.report_base <- function(lo, hi, ce, y_bottom, show_tokens) {
  x_lo <- if (show_tokens) lo - 1.7 else lo - 0.1
  br <- unique(c(lo, seq(ceiling((lo + 1) / 5) * 5, hi, by = 5)))
  p <- ggplot2::ggplot()
  if (hi > ce) {
    p <- p + ggplot2::annotate("rect", xmin = ce + 1, xmax = hi + 1,
                               ymin = -0.08, ymax = y_bottom + 0.08,
                               fill = REPORT_PAD, colour = NA)
  }
  p + ggplot2::annotate("segment", x = ce + 1, xend = ce + 1, y = -0.08,
                        yend = y_bottom + 0.08, colour = REPORT_RED,
                        linetype = "22", linewidth = 0.5) +
    ## 刻度放在格子中央：第 a 岁那一格是 [a, a + 1)
    ggplot2::scale_x_continuous(name = "age (years)", breaks = br + 0.5,
                                labels = br, expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = c(x_lo, hi + 1.1), clip = "off")
}


#' 行布局：每行的 y0 / y1 / 中心（y 向下增长，配 scale_y_reverse）
#' @noRd
.report_rows <- function(key, h, gap_before, label) {
  y0 <- cumsum(c(0, utils::head(h, -1L))) + cumsum(gap_before)
  data.table::data.table(track = key, h = h, y0 = y0, y1 = y0 + h,
                         mid = y0 + h / 2, label = label)
}


#' @noRd
.report_y <- function(rows, y_bottom, title = NULL) {
  ggplot2::scale_y_reverse(name = title, breaks = rows$mid, labels = rows$label,
                           limits = c(y_bottom + 0.1, -0.1), expand = c(0, 0))
}


#' @noRd
.report_theme <- function(p, base_size) {
  p + ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = base_size - 1,
                                          colour = "grey20", hjust = 1),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = base_size - 0.5),
      axis.title.x = ggplot2::element_text(size = base_size),
      legend.position = "none",
      plot.margin = ggplot2::margin(2, 5.5, 2, 5.5))
}


# ── 小工具 ─────────────────────────────────────────────────────────

#' 连续年份上的同值段 → (a0, a1, val)。NA 不成段，空白年断开。
#' @noRd
.runs <- function(age, val) {
  n <- length(age)
  if (!n) return(data.table::data.table(a0 = numeric(0), a1 = numeric(0),
                                        val = numeric(0)))
  o <- order(age)
  age <- age[o]
  val <- val[o]
  brk <- c(TRUE, diff(age) != 1 | val[-1L] != val[-n] |
             is.na(val[-1L]) != is.na(val[-n]))
  brk[is.na(brk)] <- TRUE
  d <- data.table::data.table(age = age, val = val, id = cumsum(brk))[!is.na(val)]
  if (!nrow(d)) return(d[, .(a0 = age, a1 = age, val = val)])
  d[, .(a0 = min(age), a1 = max(age), val = val[1L]), by = id][, id := NULL][]
}


#' 斜纹：每格三条 "/"，在格内裁好（y 轴反向，所以 v 取 1 − v）
#' @noRd
.hatch <- function(x0, y0, w, h, ts = c(-0.5, 0, 0.5)) {
  n <- length(x0)
  if (!n) return(data.table::data.table(x = numeric(0), y = numeric(0),
                                        xend = numeric(0), yend = numeric(0)))
  y0 <- rep_len(y0, n)
  w <- rep_len(w, n)
  h <- rep_len(h, n)
  i <- rep(seq_len(n), each = length(ts))
  t <- rep(ts, times = n)
  u0 <- pmax(t, 0)
  v0 <- pmax(-t, 0)
  u1 <- pmin(1, 1 + t)
  v1 <- pmin(1, 1 - t)
  data.table::data.table(x = x0[i] + u0 * w[i], y = y0[i] + (1 - v0) * h[i],
                         xend = x0[i] + u1 * w[i], yend = y0[i] + (1 - v1) * h[i])
}


#' @noRd
.ramp <- function(pal, p) {
  m <- grDevices::colorRamp(pal)(pmin(pmax(p, 0), 1))
  grDevices::rgb(m[, 1L], m[, 2L], m[, 3L], maxColorValue = 255)
}


#' 与白色按 alpha 混合（等价于 alpha 叠在白底上，但不产生半透明层）
#' @noRd
.blend <- function(col, alpha) {
  m <- grDevices::col2rgb(col)
  m <- alpha * m + (1 - alpha) * 255
  grDevices::rgb(m[1L, ], m[2L, ], m[3L, ], maxColorValue = 255)
}


#' 按**实测相对亮度**决定字色（浅色块上白字看不见）
#' @noRd
.txt_on <- function(col) {
  m <- grDevices::col2rgb(col) / 255
  lum <- 0.2126 * m[1L, ] + 0.7152 * m[2L, ] + 0.0722 * m[3L, ]
  ifelse(lum < 0.55, "white", REPORT_INK)
}


#' scales::squish 的替身（不为一个函数引 scales 依赖）
#' @noRd
.squish <- function(x, range = c(0, 1), only.finite = TRUE) {
  x[x < range[1L]] <- range[1L]
  x[x > range[2L]] <- range[2L]
  x
}


#' 标题块：一人一行的派生变量
#'
#' 有 ukb_career() 的结果就直接取那一行；只有 worklife 时现算同样的量
#' （`.derive_exposure_cols()` 与 ukb_career() 共用，口径不会分叉）。
#' @noRd
.person_summary <- function(x, wl, id, a) {
  co <- wl$cohort[eid == id]
  row <- if (inherits(x, "ukbcareer_result")) {
    data.table::as.data.table(x)[eid == id]
  } else {
    r <- merge(co, wl$exposure, by = "eid", all.x = TRUE)
    .derive_exposure_cols(r)
  }
  for (tb in c("camsis", "movement", "types")) {
    t <- wl[[tb]]
    if (!is.null(t) && nrow(t[eid == id])) {
      add <- setdiff(names(t), names(row))
      if (length(add)) row <- cbind(row, t[eid == id, add, with = FALSE])
    }
  }
  g <- function(k) if (k %in% names(row)) row[[k]][1L] else NA
  job <- a[in_job == 1L & !is.na(soc4)][order(age)]
  n_codes <- data.table::uniqueN(job$soc4)
  n_change <- if (nrow(job) > 1L) sum(diff(job$soc4) != 0) else 0L
  pct <- function(v) if (is.na(v)) "NA" else sprintf("%.0f%%", 100 * v)
  src <- switch(as.character(co$career_end_source),
                retire = "retired",
                last_job_plus1 = "no retirement record; year after last job",
                cap = "still working at the questionnaire (censored)",
                as.character(co$career_end_source))
  line1 <- sprintf(
    "Career   ends at %s (%s); observed retirement: %s; %s working years; coverage %s%s",
    co$career_end, src, if (isTRUE(co$observed_retirement == 1L)) "yes" else "no",
    g("work_years") %||% NA, pct(co$coverage),
    if (isTRUE(co$flag_incomplete == 1L)) "  [flag_incomplete]" else "")
  line2 <- sprintf(
    "Jobs     %d occupation code%s, %d change%s; at most %s job%s at a time; shift work %s, long hours (>= 48 h) %s of working years",
    n_codes, if (n_codes == 1L) "" else "s", n_change,
    if (n_change == 1L) "" else "s", co$max_parallel,
    if (isTRUE(co$max_parallel == 1L)) "" else "s",
    pct(g("shift_frac")), pct(g("long_hours_frac")))
  line3 <- sprintf(
    "Exposure %s of 10 agents ever; %s agent-years at 'often'; longest single agent at 'often' %s years",
    g("exposure_breadth"), g("high_exposure_years"), g("longest_agent_years"))
  extra <- character(0)
  if (!is.na(g("camsis_mean"))) {
    extra <- c(extra, sprintf("CAMSIS mean %.1f, %s -> %s (%s)", g("camsis_mean"),
                              format(round(g("camsis_first"), 1)),
                              format(round(g("camsis_last"), 1)),
                              g("camsis_pattern")))
  }
  if (!is.na(g("n_moves"))) {
    extra <- c(extra, sprintf("mobility %s moves, mean jump %s", g("n_moves"),
                              if (is.na(g("mean_jump"))) "NA (no move)" else
                                sprintf("%.1f", g("mean_jump"))))
  }
  if (!is.na(g("type"))) {
    extra <- c(extra, sprintf("career type %s (from your sample)", g("type")))
  }
  lines <- c(line1, line2, line3)
  if (length(extra)) lines <- c(lines, paste("Other   ", paste(extra, collapse = "; ")))
  list(title = sprintf("eid %s  |  %s, born %s", id, co$sex, co$year_birth),
       subtitle = paste(lines, collapse = "\n"))
}
