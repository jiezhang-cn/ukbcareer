# ══════════════════════════════════════════════════════════════════
# Tier 0：CAMSIS 职业地位（用户自备量表）
# ══════════════════════════════════════════════════════════════════
# 对应 codes/py/01_worklife.py 的 build_camsis / build_camsis_by_age。
#
# **CAMSIS 表不随本包分发**（用户 2026-09-21 拍定）：它是外部资源，许可另计。
# 用 `ukb_camsis(wl, path)` 自备。可从 CAMSIS 项目站点取 SOC2000 版。
#
# **承重限制，必须进方法学部分**：CAMSIS 官方未建原生 SOC2000 版本，现有分数由
# SOC90 经 Occupational Information Unit 索引近似映射（"Britain 2001
# approximations" v0.1）。在型画像里它是脚注；在桑基图里它是**纵轴** ——
# "职业流动绝大多数是横向的"这个结论直接依赖这个映射。

#' 附上 CAMSIS 职业地位
#'
#' **CAMSIS 不进 Transformer 的输入**（它是 SOC 码的确定性函数，同码同分，
#' 进输入等于把职业码换一种写法喂两遍）。它的价值恰在于**独立于表征** ——
#' "latent space 是否恢复了职业地位梯度"只有在它没进输入时才是有意义的验证。
#'
#' **必须按性别选列**（`mcamsis` / `fcamsis`）：性别特异性已内建于量表。
#' 后果是**两性的纵轴不是同一把尺子** —— 可比"横向流动占多少"，
#' 不可比"某个高度对应什么职业"。
#'
#' 只在 `career_end` 之前的**在职**人年上计算（与 token 同窗口）。
#'
#' @param x [ukb_worklife] 的返回值。
#' @param path CAMSIS 表的 csv 路径，或一个 data.frame。需要三列：
#'   `soc2000`（4 位码）、`mcamsis`、`fcamsis`。列名大小写不敏感。
#' @param slope_flat 判"平"的斜率门槛（CAMSIS 点/年），默认 0.05。
#' @param verbose 打印覆盖率与轨迹型构成。
#' @return 在 `x` 上加两项：`camsis`（逐人汇总）与 `camsis_by_age`（逐人年长表，
#'   画年龄轨迹要用它，逐人汇总量给不了）。
#' @examples
#' \dontrun{
#' wl <- ukb_worklife("ukb_cat130.csv", entry_path = "ukb_cat123.csv")
#' wl <- ukb_camsis(wl, "camsis_soc2000.csv")
#' }
#' @export
ukb_camsis <- function(x, path, slope_flat = 0.05, verbose = TRUE) {
  stopifnot(inherits(x, "ukbcareer_worklife"))
  cm <- .read_wide(path)
  data.table::setnames(cm, tolower(names(cm)))
  need <- c("soc2000", "mcamsis", "fcamsis")
  miss <- setdiff(need, names(cm))
  if (length(miss)) {
    stop("CAMSIS table is missing column(s): ", paste(miss, collapse = ", "),
         ". It needs soc2000 / mcamsis / fcamsis")
  }
  cm <- cm[, .(soc4 = as.numeric(soc2000), mcamsis = as.numeric(mcamsis),
               fcamsis = as.numeric(fcamsis))]
  cm <- unique(cm, by = "soc4")
  if (verbose) {
    message(sprintf(paste0("  CAMSIS table: %s rows / %s unique 4-digit SOC2000 codes -- ",
                           "353 is this table's NATIVE granularity, matching the model's soc_level"),
                    fmt_n(nrow(cm)), fmt_n(data.table::uniqueN(cm$soc4))))
  }

  a <- x$annual[in_job == 1L, .(eid, age, soc4)]
  a[x$cohort, sex := i.sex, on = "eid"]
  a <- merge(a, cm, by = "soc4", all.x = TRUE)
  a[, camsis := data.table::fifelse(as.character(sex) == "Female",
                                    fcamsis, mcamsis)]
  cover <- mean(!is.na(a$camsis))
  if (verbose) {
    message(sprintf("  CAMSIS covers %.4f of employed person-years%s", cover,
                    if (cover > 0.95) "" else "  [warn] low -- check your SOC coding version"))
  }
  a <- a[!is.na(camsis)]
  data.table::setorder(a, eid, age)

  # 逐人斜率：**点数不足或年龄跨度太小时给 NA**，不要硬拟合。
  # 上游的教训：`polyfit` 在**常数 y** 上会返回 3.2e-16 这样的浮点噪声，
  # 于是"逐人斜率的中位数"落在 0 这个原子上，把所有型都标成 flat。
  # 终生只报一个职业码的人（CAMSIS 从未变过）正是这种情形，占比不小。
  sl <- a[, {
    if (.N < 5L || diff(range(age)) < 3) {
      .(camsis_slope = NA_real_, zero_slope = NA_integer_)
    } else if (stats::var(camsis) == 0) {
      # 显式标出"终生单码" —— 它的斜率是真 0 而非拟合噪声
      .(camsis_slope = 0, zero_slope = 1L)
    } else {
      .(camsis_slope = unname(stats::coef(stats::lm(camsis ~ age))[2L]),
        zero_slope = 0L)
    }
  }, by = eid]
  agg <- a[, .(camsis_mean = mean(camsis),
               camsis_first = camsis[1L],
               camsis_last = camsis[.N],
               camsis_n_years = .N), by = eid]
  out <- merge(agg, sl, by = "eid", all = TRUE)
  out[, camsis_change := camsis_last - camsis_first]
  out[, camsis_pattern := data.table::fifelse(
    is.na(camsis_slope), NA_character_,
    data.table::fifelse(camsis_slope > slope_flat, "rising",
                        data.table::fifelse(camsis_slope < -slope_flat,
                                            "declining", "flat")))]
  if (verbose) {
    tb <- out[!is.na(camsis_pattern), .N, by = camsis_pattern]
    message("  status trajectory patterns: ",
            paste(sprintf("%s %s", tb$camsis_pattern, fmt_n(tb$N)),
                  collapse = " / "))
    nz <- sum(out$zero_slope == 1L, na.rm = TRUE)
    if (nz) {
      message(sprintf(paste0("  of which %s reported a single occupation code for life ",
                             "(CAMSIS never changed) -- their flat slope is a true ",
                             "zero, not fitting noise"), fmt_n(nz)))
    }
  }

  x$camsis <- out[]
  x$camsis_by_age <- a[, .(eid, age, soc4, camsis)]
  at <- attr(x, "ukbcareer")
  at$camsis_coverage <- cover
  at$camsis_caveat <- paste0(
    "CAMSIS has no native SOC2000 version; scores are approximated from SOC90 ",
    "via Occupational Information Unit indices (Britain 2001 approximations ",
    "v0.1). The scale is sex-specific, so the two sexes are not on the same ruler.")
  attr(x, "ukbcareer") <- at
  x
}
