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


#' 职业移动几何
#'
#' 在 353 个职业码的 192 维嵌入空间里，把每个人走过的轨迹算成六个量。
#'
#' **`n_moves == 0` 的人，距离类量一律记 `NA` 而不是 0。** 参考队列 36.8% 的人
#' 一生只报一个职业码，他们的路程"恒为 0"不是"变动小"而是**没有定义** ——
#' 把两种 0 混在一列里正是"缺失当零暴露"那类错误。下游必须分开处理，
#' 所以本函数同时返回 `n_moves` 作为骨架变量。
#'
#' @param x [ukb_worklife] 的返回值。
#' @param bundle [ukb_bundle] 的返回值（需要 Tier 1）。
#' @param level SOC 层级，默认 `"soc_l4"`（与论文一致）。
#' @param verbose 打印无变动者比例与几个中位数。
#' @return 在 `x` 上加一项 `movement`（一人一行）：
#'   `n_moves`（骨架）、`mean_jump`、`net_displacement`、`max_single_jump`、
#'   `first_move_age`、`first_move_rel`（距首个在职年的相对年数）、`last_move_age`。
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
    stop("The bundle has no soc_embed_*.parquet -> Tier 1 unavailable")
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


#' 码对距离的集中度
#'
#' 报告 Tier 1 全部衍生量的**承重事实**。写文章时必须报这个数：
#' 距离近乎三态（0 / ~19 同大类内 / ~37 换到别处），一切"绕路程度"的措辞都不成立。
#'
#' @param bundle [ukb_bundle] 的返回值。
#' @param sex 性别。
#' @return 一个含 `median` / `cv` / `frac_30_45` / `q` 的列表（不可见地打印）。
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
