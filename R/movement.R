# ══════════════════════════════════════════════════════════════════
# Tier 1：职业移动几何
# ══════════════════════════════════════════════════════════════════
# 对应 codes/py/05_derived.py 的 transitions / seq_movement。
# 只需要 bundle 里的 353 × 192 嵌入矩阵，**不需要 encoder 权重**。
#
# ── 为什么这里只有 6 个量，而不是上游算过的 10 个 ────────────────────
# 353 个码的两两距离**高度集中**：变异系数只有 0.10，94.8% 的码对落在 [30, 45]，
# 中位 37.6。距离实际上是三态的：0（同码）/ ~19（同 L1 大类内）/ ~37（换到别处）。
# 后果是一串"看起来像几何量"的东西其实只是转换次数 k 的伪装：
#
#   path_length  = k × 单跳 ≈ 37k        → R²(k) 0.90/0.92，弃用
#   net_displace ≈ 37（**与 k 无关**）   → 保留（它是真正独立的信息）
#   detour_ratio = path/net ≈ k          → R²(k) 0.93/0.94，**它就是 k 的另一个写法**
#   straightness = net/path ≈ 1/k        → R²(k) 0.81/0.84，是 detour 的倒数不是新信息
#
# 所以**不要把 detour_ratio 说成"绕路程度"**。留下的六个量里，`mean_jump` 与
# `net_displacement` 对 k 的 R² 都 ≤ 0.02、残差 IQR 比 0.97–1.00，是真正独立的；
# 又因为有恒等式 `path = k × mean_jump`，"扣掉 k 之后的 path"就是 `mean_jump` ——
# 留 mean_jump 丢 path，信息不减。`first_move_*` / `last_move_age` 是唯一
# 不基于距离的量，因此与 k 几乎无关（rho ≈ 0）。

#' 实际发生的职业转换
#'
#' 只算**在职年之间**的相邻不同码转换。中间隔着 gap 年的也算一次转换
#' （那正是"换了工作"），但要求两端都在职。与 token 侧同一个右截断。
#' @noRd
.transitions <- function(annual, level = DEFAULTS$level) {
  a <- annual[in_job == 1L & !is.na(soc4), .(eid, age, soc4)]
  a[, code := .soc_level(soc4, level)]
  data.table::setorder(a, eid, age)
  a[, prev := data.table::shift(code), by = eid]
  a[!is.na(prev) & prev != code, .(eid, age, frm = prev, to = code)]
}


#' Occupational mobility measures from the SOC embedding
#'
#' Summarises each person's sequence of occupation changes as a path through
#' the 192-dimensional embedding space of the 353 occupation codes, and
#' returns six per-person measures alongside the number of changes. This needs
#' only the SOC embedding in the model bundle (`soc_embed_*.parquet`), **not**
#' the encoder weights or the torch package.
#'
#' A change is counted between consecutive **working** years with different
#' codes. A change across an intervening gap also counts (that is exactly
#' "changing job"), but both ends must be working years. Transitions involving
#' codes outside the embedding vocabulary are dropped and reported.
#'
#' **For people with `n_moves == 0`, all distance measures are `NA`, not 0.**
#' In the reference cohort 36.8% of people reported a single occupation code
#' for life. Their "zero distance" does not mean "moved little" -- it is
#' **undefined**. Mixing the two kinds of zero in one column is the same
#' mistake as treating missing exposure as zero exposure. Handle them
#' separately downstream; that is why `n_moves` is always returned as the
#' backbone variable.
#'
#' @section Why only these measures:
#' Pairwise distances between the 353 codes are **highly concentrated**: the
#' coefficient of variation is only 0.10, 94.8% of code pairs lie in
#' \[30, 45\], and the median is 37.6. In practice distances take three
#' values: 0 (same code), about 19 (within the same L1 major group) and about
#' 37 (anywhere else). As a result, several quantities that look geometric are
#' just the number of changes k in disguise:
#' * path length = k x single jump, about 37k (R^2 with k 0.90/0.92 for
#'   women/men) -- not returned;
#' * detour ratio = path / net, about k (R^2 0.93/0.94) -- **it is simply
#'   another way of writing k**, so do not describe it as "how roundabout" a
#'   career was; not returned;
#' * straightness = net / path, about 1/k (R^2 0.81/0.84) -- the reciprocal of
#'   the detour ratio, no new information; not returned.
#'
#' Of the measures kept, `mean_jump` and `net_displacement` have R^2 with k of
#' at most 0.02 and are genuinely independent of it. Because
#' `path = k x mean_jump`, "path length net of k" is just `mean_jump`, so
#' nothing is lost by dropping path length. `first_move_age`,
#' `first_move_rel` and `last_move_age` are the only measures not based on
#' distance and are almost unrelated to k (rho about 0). See
#' [ukb_distance_concentration] and [plot_distance_concentration].
#'
#' @param x Output of [ukb_worklife].
#' @param bundle Output of [ukb_bundle]; it must contain the SOC embedding
#'   (`soc_embed_*.parquet`).
#' @param level SOC level; default `"soc_l4"` (as in the paper).
#' @param verbose If `TRUE`, print the share of people who never changed code
#'   and a few medians.
#' @return `x` with an added element `movement` (one row per person):
#'   `n_moves` (backbone), `mean_jump`, `net_displacement`,
#'   `max_single_jump`, `first_move_age`, `first_move_rel` (years since the
#'   person's first working year; less confounded by birth cohort than
#'   absolute age) and `last_move_age`, plus `sex`.
#' @examples
#' \dontrun{
#' wl <- ukb_movement(wl, bundle = b)
#' wl$movement[1:5]
#' }
#' @export
ukb_movement <- function(x, bundle, level = DEFAULTS$level, verbose = TRUE) {
  stopifnot(inherits(x, "ukbcareer_worklife"),
            inherits(bundle, "ukbcareer_bundle"))
  if (!isTRUE(bundle$tiers[["tier1"]])) {
    stop("Mobility measures unavailable: the bundle lacks soc_embed_*.parquet ",
         "(the SOC embedding)")
  }
  out <- list()
  for (sx in levels(x$cohort$sex)) {
    ids <- x$cohort$eid[x$cohort$sex == sx]
    if (!length(ids)) next
    E <- .bundle_embed(bundle, sx)
    ec <- grep("^e[0-9]{3}$", names(E), value = TRUE)
    M <- as.matrix(E[, ..ec])
    pos <- stats::setNames(seq_len(nrow(E)), as.character(E$soc))

    tr <- .transitions(x$annual[eid %in% ids], level)
    # 词表外的码（UNK）无嵌入向量 → 该次转换无法度量，如实丢掉并报数
    n_all <- nrow(tr)
    tr <- tr[as.character(frm) %in% names(pos) & as.character(to) %in% names(pos)]
    if (verbose && n_all > nrow(tr)) {
      message(sprintf(paste0("  [%s] %s/%s transitions involve codes outside the vocabulary ",
                             "(UNK) -> those transitions are excluded from the geometry"),
                      sx, fmt_n(n_all - nrow(tr)), fmt_n(n_all)))
    }
    first_job <- x$annual[eid %in% ids & in_job == 1L,
                          .(first_job_age = min(age)), by = eid]

    if (nrow(tr)) {
      i <- pos[as.character(tr$frm)]
      j <- pos[as.character(tr$to)]
      tr[, d := sqrt(rowSums((M[i, , drop = FALSE] - M[j, , drop = FALSE])^2))]
      data.table::setorder(tr, eid, age)
      g <- tr[, {
        fi <- pos[as.character(frm[1L])]
        tj <- pos[as.character(to[.N])]
        .(n_moves = .N,
          mean_jump = mean(d),
          max_single_jump = max(d),
          # 首末直线距离。**与 k 几乎无关** —— 距离近乎相等时，末码只是"另外
          # 一个码"，所以它约等于单次转换距离，不随转换次数增长。
          net_displacement = sqrt(sum((M[fi, ] - M[tj, ])^2)),
          first_move_age = min(age), last_move_age = max(age))
      }, by = eid]
    } else {
      g <- data.table::data.table(eid = numeric(0))
    }
    o <- data.table::data.table(eid = ids)
    o <- merge(o, g, by = "eid", all.x = TRUE)
    o <- merge(o, first_job, by = "eid", all.x = TRUE)
    o[is.na(n_moves), n_moves := 0L]
    # **距离类量在无变动者身上是 NA 不是 0**
    for (cc in c("mean_jump", "max_single_jump", "net_displacement",
                 "first_move_age", "last_move_age")) {
      o[n_moves == 0L, (cc) := NA_real_]
    }
    # first_move_rel：距**该人首个在职年**的相对年数。比绝对年龄更少地混入
    # 出生队列效应（两者残差相关 0.88，扣掉 k 后仍需二选一 —— 这里两个都给）
    o[, first_move_rel := first_move_age - first_job_age]
    o[, sex := factor(sx, levels = levels(x$cohort$sex))]
    out[[sx]] <- o
  }
  mv <- data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  if (verbose) {
    n0 <- sum(mv$n_moves == 0L)
    message(sprintf(paste0("  %s/%s (%.1f%%) never changed occupation code -> distance ",
                           "measures are recorded as MISSING, not 0"),
                    fmt_n(n0), fmt_n(nrow(mv)), 100 * n0 / nrow(mv)))
    m <- mv[n_moves > 0L]
    if (nrow(m)) {
      message(sprintf(paste0("  movers (n=%s): median jump %.2f, median net displacement %.2f, ",
                             "median largest single jump %.2f"),
                      fmt_n(nrow(m)), stats::median(m$mean_jump),
                      stats::median(m$net_displacement),
                      stats::median(m$max_single_jump)))
    }
  }
  x$movement <- mv[]
  x
}


#' Summarise how concentrated the distances between occupation codes are
#'
#' Computes all pairwise distances between occupation codes in the SOC
#' embedding and prints their median, coefficient of variation, share within
#' \[30, 45\] and quantiles. This is the **key fact behind every mobility
#' measure** from [ukb_movement], and you should report it in any paper that
#' uses them: distances are nearly three-valued (0 / about 19 within a major
#' group / about 37 elsewhere), so any wording about how "roundabout" a career
#' was does not hold.
#'
#' @param bundle Output of [ukb_bundle] (needs the SOC embedding,
#'   `soc_embed_*.parquet`).
#' @param sex Sex.
#' @return Invisibly, a list with `n_codes`, `median`, `cv`, `frac_30_45` and
#'   `q` (quantiles). A summary is printed as a side effect.
#' @export
ukb_distance_concentration <- function(bundle, sex = "Female") {
  E <- .bundle_embed(bundle, sex)
  ec <- grep("^e[0-9]{3}$", names(E), value = TRUE)
  M <- as.matrix(E[, ..ec])
  d <- as.vector(stats::dist(M))
  out <- list(n_codes = nrow(E), median = stats::median(d),
              cv = stats::sd(d) / mean(d),
              frac_30_45 = mean(d >= 30 & d <= 45),
              q = stats::quantile(d, c(.01, .05, .25, .5, .75, .95, .99)))
  cat(sprintf("%s: %d codes, %s code pairs\n", sex, out$n_codes,
              fmt_n(length(d))))
  cat(sprintf("  median %.2f, CV %.3f, %.1f%% within [30,45]\n",
              out$median, out$cv, 100 * out$frac_30_45))
  cat("  quantiles: ", paste(sprintf("%s=%.1f", names(out$q), out$q),
                        collapse = "  "), "\n", sep = "")
  cat("  -> near-ternary: 0 (same code) / ~19 (within major group) /",
      "~37 (elsewhere)\n")
  cat("     path_length / detour_ratio / straightness are therefore the\n")
  cat("     number of changes in disguise; not returned. See ?ukb_movement\n")
  invisible(out)
}
