# ══════════════════════════════════════════════════════════════════
# 参考级可视化（只用 bundle，不需要用户数据）
# ══════════════════════════════════════════════════════════════════

#' Map of the 353 occupation codes in the embedding space
#'
#' Plots the 2-D map of occupation codes from the model bundle. Each point is
#' one 4-digit SOC2000 code (**not one person**); point size is the number of
#' person-years with that code, and colour is the SOC major group (L1). The
#' coordinates are taken directly from the bundle, so they are in the same
#' coordinate system as the paper's figure and can be compared point by point.
#' No individual-level data are needed.
#'
#' The 9 major groups use a Dark2-style palette that **deliberately avoids
#' blue and red** (reserved for sex). Major-group labels sit at the **top
#' edge** of each cluster rather than its centre, where they would overlap
#' the labels of the codes with the most person-years.
#'
#' Keep in mind that the embedding is partly learned from which codes occur
#' next to each other in careers, so "similar jobs sit close together" is
#' partly true by construction.
#'
#' @param bundle Output of [ukb_bundle].
#' @param sex Sex. **The two sexes have unrelated coordinate systems** (the
#'   encoder and the projection are fitted separately for each sex), so
#'   positions cannot be compared across sexes.
#' @param label_top Number of codes with the most person-years to label.
#' @return A ggplot object.
#' @examples
#' \dontrun{
#' plot_soc_map(b, "Female")
#' }
#' @export
plot_soc_map <- function(bundle, sex = "Female", label_top = 12L) {
  .need_ggplot()
  E <- .bundle_embed(bundle, sex)
  if (!all(c("x", "y") %in% names(E))) {
    stop("soc_embed_", sex, ".parquet in the bundle has no 2D coordinates")
  }
  E <- E[!is.na(x) & !is.na(y)]
  E[, l1 := factor(soc_l1)]
  py <- if ("person_years" %in% names(E)) E$person_years else rep(1, nrow(E))
  # **人年被小格抑制（n < 10）的码必须仍然画出来。** 把它们的 NA 留给
  # scale_size_area 会让 ggplot 静默丢掉整行 —— 于是地图上少了 22 个码，
  # 而标题仍说"353 个"。给它们最小尺寸并在图注里标出个数。
  n_sup <- sum(is.na(py))
  py[is.na(py)] <- min(py, na.rm = TRUE)
  E[, pyv := py]
  lab <- E[order(-pyv)][seq_len(min(label_top, nrow(E)))]
  ## 大类名标在团的上沿
  hull <- E[, .(x = stats::median(x), y = max(y) + 0.3), by = l1]
  ggplot2::ggplot(E, ggplot2::aes(x, y, colour = l1)) +
    ggplot2::geom_point(ggplot2::aes(size = pyv), alpha = 0.75) +
    ggplot2::scale_size_area(max_size = 5, name = "person-years") +
    ggplot2::scale_colour_manual(values = UKB_L1, name = "SOC major group") +
    ggplot2::geom_text(data = hull, ggplot2::aes(label = paste0("L", l1)),
                       size = 2.6, fontface = "bold", show.legend = FALSE) +
    ggplot2::geom_text(data = lab, ggplot2::aes(label = soc), size = 1.9,
                       colour = "grey20", vjust = -0.9, show.legend = FALSE) +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = sprintf("Embedding map of %d occupation codes (%s)", nrow(E), sex),
      caption = .cap(
        "One point is ONE OCCUPATION CODE, not one person (all person-years with that code fall on one point).",
        paste0("The two sexes have unrelated coordinate systems (encoder and UMAP are both fitted per sex): do not compare positions across panels."),
        paste0("The embedding is partly learned from transition co-occurrence, so 'similar jobs sit close' is partly tautological."),
        if (n_sup > 0L) sprintf(
          "%d codes had person-years suppressed (n < 10); they are still drawn, at minimum point size.",
          n_sup) else NULL)) +
    ukb_theme(grid = FALSE) +
    ggplot2::theme(axis.text = ggplot2::element_blank())
}


#' Plot the baseline ladder for sequence predictability
#'
#' Compares the model's per-token cross-entropy on annual occupation
#' sequences with a ladder of simple baselines (uniform, marginal
#' distribution, "copy the previous year", "copy both neighbouring years"),
#' using the values stored in the bundle's `reliability.json`.
#'
#' **The point of this plot is to state honestly that the model is only 0.7%
#' ahead.** The encoder is bidirectional (no causal mask): when it predicts a
#' masked year j, it **sees both year j-1 and year j+1**. Per-token
#' cross-entropy therefore measures interpolation, not forecasting. The right
#' comparator is not the marginal distribution (a straw man) but the
#' non-learning rule "copy both neighbouring years" -- and the model beats it
#' by only about 0.03 nats. The subtitle expresses this gap as a share of the
#' model's total gain over the marginal distribution.
#'
#' In practice, report "annual occupation sequences are almost fully
#' determined by the adjacent years", not "the model learned career
#' predictability".
#'
#' @param bundle Output of [ukb_bundle].
#' @return A ggplot object.
#' @export
plot_baselines <- function(bundle) {
  .need_ggplot()
  b <- bundle$reliability$baselines
  if (!length(b)) stop("The bundle reliability.json has no baseline ladder")
  ## **图面文字一律英文**（见 ?ukb_theme 的绘图纪律）：这些图是要进论文的，
  ## 而 cairo_pdf 在多数系统上没有嵌入中文字形 —— 上游 Python 侧同样纪律。
  keys <- c(uniform = "uniform, log(353)", marginal = "marginal H(X)",
            persistence = "copy previous year",
            interpolation = "copy both neighbours", model_ce = "model")
  rows <- list()
  for (s in names(b)) {
    for (k in names(keys)) {
      v <- b[[s]][[k]]
      if (is.null(v) || !is.numeric(v)) next
      # 列名**不能叫 `key`** —— 那是 `data.table()` 的保留参数（设置主键），
      # 传字符串进去会被当成列名去查找，报 "some columns are not in the
      # data.table: [uniform]"。
      rows[[length(rows) + 1L]] <- data.table::data.table(
        sex = s, what = keys[[k]], metric = k, ce = as.numeric(v))
    }
  }
  if (!length(rows)) stop("No usable values in the baseline ladder")
  d <- data.table::rbindlist(rows)
  d[, what := factor(what, levels = rev(unname(keys)))]
  d[, ppl := exp(ce)]
  ## 分母必须是**模型相对边际分布的全部增益**，不是双向填空基线自己的高度。
  ## 用后者会得到 57%（"相对基线又降了 57%"），读起来像"模型大幅优于基线" ——
  ## 正好与这张图的意图相反。前者给 0.7%：在模型声称的全部本事里，
  ## 超出一条不学习的规则的部分只占 0.7%。
  gap <- d[metric %in% c("marginal", "interpolation", "model_ce"),
           .(g = ce[metric == "interpolation"] - ce[metric == "model_ce"],
             share = (ce[metric == "interpolation"] - ce[metric == "model_ce"]) /
               (ce[metric == "marginal"] - ce[metric == "model_ce"])),
           by = sex]
  # 一行放不下两性（实测在 8 英寸宽下右端被截断）→ 每性别一行
  sub <- paste(sprintf("%s: +%.3f nats beyond 'copy both neighbours' = %.1f%% of its total gain over the marginal",
                       gap$sex, gap$g, 100 * gap$share), collapse = "\n")
  ggplot2::ggplot(d, ggplot2::aes(ce, what, fill = sex)) +
    ggplot2::geom_col(position = "dodge", width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f (ppl %.2f)", ce, ppl)),
                       position = ggplot2::position_dodge(width = 0.7),
                       hjust = -0.08, size = 2.1) +
    ggplot2::scale_fill_manual(values = UKB_SEX, name = NULL) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.28))) +
    ggplot2::labs(
      x = "per-token cross-entropy (nats; lower = more predictable)", y = NULL,
      title = "Predictability of annual occupation sequences: baseline ladder",
      subtitle = sub,
      caption = .cap(
        "The encoder is BIDIRECTIONAL (no causal mask): this measures interpolation, not forecasting.",
        "The right comparator is therefore 'copy both neighbours' -- a rule that does not learn -- not the marginal.",
        "=> Report 'annual occupation sequences are almost fully determined by the adjacent years',",
        "   NOT 'the model learned career predictability'.",
        "This is also why per-person perplexity is not returned (see ?ukb_movement, ?ukbcareer).")) +
    ukb_theme()
}


#' Plot how concentrated the pairwise distances between occupation codes are
#'
#' Histogram of the distances between every pair of occupation codes in the
#' 192-dimensional embedding, split into pairs within the same SOC major
#' group and pairs across major groups.
#'
#' This is the **key fact behind every mobility measure** returned by
#' [ukb_movement]. Distances are nearly three-valued: 0 (same code), about
#' 19 (within the same L1 major group) and about 37 (anywhere else), with
#' about 37 by far the most common. As a consequence, path length, detour
#' ratio and straightness are just the number of changes in disguise, which
#' is why the package does not return them.
#'
#' @param bundle Output of [ukb_bundle] (needs the SOC embedding,
#'   `soc_embed_*.parquet`).
#' @param sex Sex.
#' @return A ggplot object.
#' @export
plot_distance_concentration <- function(bundle, sex = "Female") {
  .need_ggplot()
  E <- .bundle_embed(bundle, sex)
  ec <- grep("^e[0-9]{3}$", names(E), value = TRUE)
  M <- as.matrix(E[, ..ec])
  l1 <- E$soc_l1
  dm <- as.matrix(stats::dist(M))
  iu <- which(upper.tri(dm))
  same_l1 <- outer(l1, l1, "==")[iu]
  d <- data.table::data.table(dist = dm[iu],
                              grp = ifelse(same_l1, "within same major group", "across major groups"))
  cv <- stats::sd(d$dist) / mean(d$dist)
  ggplot2::ggplot(d, ggplot2::aes(dist, fill = grp)) +
    ggplot2::geom_histogram(bins = 80, position = "identity", alpha = 0.7) +
    ggplot2::scale_fill_manual(values = c("within same major group" = "#41b6c4",
                                          "across major groups" = "#253494"), name = NULL) +
    ggplot2::geom_vline(xintercept = stats::median(d$dist), linetype = "dashed",
                        colour = "grey20", linewidth = 0.4) +
    ggplot2::labs(
      x = "pairwise distance between occupation codes (192-d embedding)", y = "code pairs",
      title = sprintf("Pairwise distances between occupation codes are highly concentrated (%s)", sex),
      subtitle = sprintf("median %.1f, CV %.3f, %.1f%% within [30, 45]",
                         stats::median(d$dist), cv,
                         100 * mean(d$dist >= 30 & d$dist <= 45)),
      caption = .cap(
        paste0("Distances are near-TERNARY: 0 (same code) / ~19 (within major group) / ~37 (elsewhere), ",
               "and ~37 dominates."),
        paste0("Consequence: path_length ~ 37k, detour_ratio ~ k, straightness ~ 1/k ",
               "-- all three are the number of changes in disguise, so this package does not return them."),
        paste0("The retained mean_jump and net_displacement have R^2 vs k <= 0.02 ",
               "and are genuinely independent. See ?ukb_movement"))) +
    ukb_theme()
}


#' Plot career-type profiles of the reference cohort
#'
#' Heatmap (z-scored across types) read directly from the bundle's `tables/`
#' folder: P2 characteristic occupations, P3 exposure enrichment, P4 CAMSIS
#' profile and P5 career breaks. These are profiles of the **reference
#' cohort**, meant to help you understand what its 12 (women) / 9 (men)
#' types are. They are not profiles of your sample.
#'
#' Profile variables are for description and naming only; they were never
#' used to choose the number of types or the clustering resolution
#' (otherwise the profiles would be tautological).
#'
#' @param bundle Output of [ukb_bundle].
#' @param sex Sex.
#' @param panel Which profile to draw: `"camsis"`, `"exposure"`, `"soc"` or
#'   `"gaps"`.
#' @return A ggplot object.
#' @export
plot_type_profiles <- function(bundle, sex = "Female",
                               panel = c("camsis", "exposure", "soc", "gaps")) {
  .need_ggplot()
  panel <- match.arg(panel)
  f <- file.path(bundle$path, "tables",
                 sprintf("%s_%s.csv", switch(panel,
                                             camsis = "P4_camsis",
                                             exposure = "P3_exposure",
                                             soc = "P2_soc_mix",
                                             gaps = "P5_gaps"), sex))
  if (!file.exists(f)) stop("The bundle has no ", basename(f))
  d <- data.table::fread(f, showProgress = FALSE)
  nm <- bundle$type_names[[sex]]$names
  tn <- if (is.null(nm)) NULL else unlist(nm)
  tcol <- intersect(c("type", "Type"), names(d))[1L]
  if (is.na(tcol)) stop(basename(f), " has no type column")
  d[, tlab := {
    k <- as.character(get(tcol))
    if (is.null(tn)) paste0("T", k) else {
      s <- tn[k]
      paste0("T", k, " ", substr(ifelse(is.na(s), "", s), 1L, 46L))
    }
  }]
  num <- names(d)[vapply(d, is.numeric, logical(1))]
  num <- setdiff(num, c(tcol, "n"))
  if (!length(num)) stop(basename(f), " has no numeric column to plot")
  keep <- utils::head(num, 8L)
  lg <- data.table::melt(d[, c("tlab", keep), with = FALSE], id.vars = "tlab",
                         variable.name = "v", value.name = "val")
  lg <- lg[is.finite(val)]
  lg[, z := {
    s <- stats::sd(val)
    if (is.na(s) || s == 0) 0 else (val - mean(val)) / s
  }, by = v]
  lim <- max(abs(lg$z), na.rm = TRUE)
  ggplot2::ggplot(lg, ggplot2::aes(v, tlab, fill = z)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7",
                                 high = "#b2182b", midpoint = 0,
                                 limits = c(-lim, lim), name = "z (across types)") +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = sprintf("Reference-cohort type profile: %s (%s)", panel, sex),
      caption = .cap(
        paste0("This profiles the REFERENCE COHORT (our 117,590 people), not your sample."),
        paste0("Type names are generated from P2 lift + P3 enrichment + P4 CAMSIS + P5 breaks, outcome-blind."),
        paste0("Profile axes are for description and naming ONLY, never for choosing K or resolution --",
               " selecting a partition by them would make the profile tautological."))) +
    ukb_theme(grid = FALSE) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   axis.text.y = ggplot2::element_text(size = 5.5))
}
