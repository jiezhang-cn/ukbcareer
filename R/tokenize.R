# ══════════════════════════════════════════════════════════════════
# Tier 2：年度面板 → 12 通道定长张量
# ══════════════════════════════════════════════════════════════════
# 对应 codes/py/02_tokenize.py 的 pack_annual / add_tenure_and_hazard。
# 每人年一个 token，**位置编码用年龄而不是序号** —— 重要的是"几岁经历"，
# 这也是变长序列可行的前提。第 0 位是 [CLS]。

HOURS_CAP <- 100      # 每周工时的生理上限
TENURE_CAP <- 3       # dt 的截顶


#' 每周工时 → 归一化到长工时阈值（48h），**先截尾**
#'
#' 参考队列的 22605 里有 48 条 168 h/wk —— 一周的全部小时数，物理上不可能。
#' 不截尾则 intens 通道最大值达 3.5，比其他通道（0–1）大三倍多，嵌入层会被
#' 这几十条脏数据主导。截到 100 后最大 2.08，仍保留"极端长工时"的信息。
#' @noRd
.norm_hours <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  v[is.na(v)] <- 0
  pmin(v, HOURS_CAP) / 48
}


#' 加 `dt`（归一化 SOC 任期）与 `soc_change_next`（T3 的 hazard 标签）
#'
#' `dt = min(当前 SOC spell 任期, 3) / 3`。**截顶不可省**：原始任期与年龄的相关是
#' +0.633，cap 到 3 之后降到 +0.316（log1p 0.606 / cap15 0.575 / cap10 0.521）。
#' 保留了"刚换过 vs 已稳定"，丢掉了与年龄同构的"稳定了多少年"。
#'
#' **gap 年不打断 spell**：gap 年的 `soc4` 是 NA，直接比较会把每个 gap 年都判成
#' 新 spell。所以先在人内向前填充 —— 语义上"中断一年再回到同一职业"不该重置任期。
#'
#' 两个量都必须在"保留最近 room 年"之前算：① 任期要从真实的职业生涯起点数起；
#' ② `soc_change_next` 需要下一年的 soc，而截断之后末尾那年的"下一年"已被移出。
#' @noRd
.add_tenure_and_hazard <- function(a, cap = TENURE_CAP) {
  data.table::setorder(a, eid, age)
  a[, soc_f := .fill_down(as.numeric(soc4)), by = eid]
  a[, new_spell := is.na(data.table::shift(soc_f)) |
      data.table::shift(soc_f) != soc_f, by = eid]
  a[, new_spell := data.table::fifelse(is.na(new_spell), TRUE, new_spell)]
  a[, spell_id := cumsum(new_spell)]
  a[, tenure := seq_len(.N), by = spell_id]
  a[, dt := pmin(as.numeric(tenure), cap) / cap]
  a[, nxt := data.table::shift(soc_f, type = "lead"), by = eid]
  a[, soc_change_next := as.numeric(!is.na(nxt) & !is.na(soc_f) & nxt != soc_f)]
  a[, c("new_spell", "spell_id", "tenure", "nxt") := NULL]
  a[]
}


#' 人内向前填充（LOCF）
#' @noRd
.fill_down <- function(x) {
  nn <- which(!is.na(x))
  if (!length(nn)) return(x)
  # 每个位置取其前面最近的一个非 NA
  idx <- cumsum(!is.na(x))
  out <- rep(NA_real_, length(x))
  ok <- idx > 0
  out[ok] <- x[nn][idx[ok]]
  out
}


#' 从 bundle 的 vocab 重建层级映射
#'
#' `emb(soc4) = E4[id4] + E3[id3] + E2[id2] + E1[id1]`。稀疏 4 位码的嵌入由其上层
#' 分量（成千上万人年共同估计）收缩 → 不退化成噪声，而退化成"所属职业小类的平均"。
#' **这是 353 类可行的技术前提。**
#'
#' 映射由**冻结词表**按位数截断算出，不需要外部编码表。
#' @noRd
.hier_maps <- function(vocab) {
  level <- vocab$level %||% DEFAULTS$level
  sv <- unlist(vocab$soc_vocab)
  codes <- as.numeric(names(sv))
  tids <- as.integer(sv)
  base_div <- SOC_DIVISOR[[level]]
  n_tok <- max(tids) + 1L
  order_lv <- c("soc_l1", "soc_l2", "soc_l3", "soc_l4")
  out <- list()
  for (lv in order_lv[SOC_DIVISOR[order_lv] > base_div]) {
    div <- SOC_DIVISOR[[lv]] %/% base_div
    grp <- codes %/% div
    uk <- sort(unique(grp))
    remap <- stats::setNames(seq_along(uk), as.character(uk))  # 0 留给 PAD
    arr <- integer(n_tok)                                      # 默认 0 = PAD
    arr[tids + 1L] <- remap[as.character(grp)]
    out[[lv]] <- list(map = arr, n = length(uk) + 1L)
  }
  out
}


#' 年度面板 → 12 通道张量
#'
#' 输出是一个 list，每个通道一个矩阵（n × max_len），`intens` 是 n × max_len × 13
#' 的数组。这个结构直接喂给 [ukb_encode]。
#'
#' @param x [ukb_worklife] 的返回值。
#' @param bundle [ukb_bundle] 的返回值（要用它的**冻结词表**）。
#' @param sex 只 token 化某个性别。分性别独立建模是硬约束。
#' @param max_len 序列上限。参考队列实测 max 正好 60，顶到 64 即口径不一致。
#' @param verbose 打印序列长度分布、dt 统计、DK 指示率。
#' @return `ukbcareer_tokens` 对象。
#' @noRd
.tokenize <- function(x, bundle, sex, max_len = DEFAULTS$max_len,
                      verbose = TRUE) {
  vocab <- bundle$vocab
  level <- vocab$level %||% DEFAULTS$level
  sv <- unlist(vocab$soc_vocab)
  gv <- unlist(vocab$gap_vocab)
  hier <- .hier_maps(vocab)

  ids <- x$cohort$eid[x$cohort$sex == sex]
  a <- data.table::copy(x$annual[eid %in% ids])
  if (!nrow(a)) stop("No person-years for sex ", sex)
  a[, code := .soc_level(as.numeric(soc4), level)]
  a <- .add_tenure_and_hazard(a)

  uniq <- sort(unique(a$eid))
  n <- length(uniq)
  L <- as.integer(max_len)
  room <- L - 1L

  data.table::setorder(a, eid, age)
  a[, rank0 := seq_len(.N) - 1L, by = eid]
  a[, cnt := .N, by = eid]
  if (verbose) {
    cn <- a[, .(cnt = cnt[1L]), by = eid]$cnt
    n_long <- sum(cn > room)
    message(sprintf("  [%s] sequence length: median %.0f, max %d%s", sex,
                    stats::median(cn), max(cn),
                    if (n_long) sprintf("; %d exceed the limit -> keeping their most recent %d years",
                                        n_long, room) else "; none exceed the limit"))
    if (max(cn) > room) {
      message("    [warn] someone hit max_len -- the reference cohort maximum is ",
              "exactly 60, so hitting the limit means the definitions differ ",
              "from the paper (ukb_qc() reports this)")
    }
  }
  # 保留**最近**的 room 年：截断只影响 16 岁附近的早年，那时多数人还在上学
  a <- a[rank0 >= pmax(cnt - room, 0L)]
  a[, pos := rank0 - pmax(cnt - room, 0L) + 1L]     # 第 0 位留给 [CLS]
  a[, row := match(eid, uniq)]

  mk <- function(v = 0) matrix(v, nrow = n, ncol = L)
  tok <- list(
    soc = matrix(PAD, n, L), gap = matrix(0L, n, L), age = matrix(0L, n, L),
    dt = mk(), n_jobs = mk(), is_transition = mk(), is_parallel = mk(),
    soc_l1 = matrix(0L, n, L), soc_l2 = matrix(0L, n, L),
    soc_l3 = matrix(0L, n, L),
    pad = matrix(TRUE, n, L),
    soc_change_next = mk()
  )
  tok$soc[, 1L] <- CLS
  tok$pad[, 1L] <- FALSE
  ij <- cbind(a$row, a$pos + 1L)                    # R 的列从 1 开始

  ## SOC：词表外 → UNK
  tid <- sv[as.character(a$code)]
  tid[is.na(tid)] <- UNK
  n_unk <- sum(tid == UNK & a$in_job == 1L)
  # **不在职的年（gap）不带职业身份** —— 否则"做什么"与"是否在工作"纠缠
  soc_tok <- data.table::fifelse(a$in_job == 1L, as.integer(tid), PAD)
  tok$soc[ij] <- soc_tok
  for (lv in names(hier)) {
    m <- hier[[lv]]$map
    tok[[lv]][ij] <- m[pmin(pmax(soc_tok, 0L), length(m) - 1L) + 1L]
  }
  ## gap 码 → gap 词表 id（0 = 无 gap）
  g <- gv[as.character(as.numeric(a$gap_code))]
  g[is.na(g)] <- 0L
  tok$gap[ij] <- as.integer(g)
  tok$age[ij] <- pmin(pmax(as.integer(a$age), 0L), 100L)
  tok$dt[ij] <- a$dt
  nj <- a$n_jobs_year
  nj[is.na(nj)] <- 0
  tok$n_jobs[ij] <- nj
  for (k in c("is_transition", "is_parallel")) {
    v <- if (k %in% names(a)) as.numeric(a[[k]]) else rep(0, nrow(a))
    v[is.na(v)] <- 0
    tok[[k]][ij] <- v
  }
  tok$soc_change_next[ij] <- a$soc_change_next
  tok$pad[ij] <- FALSE

  ## intens：10 暴露 + 工时 + 轮班 + DK 指示
  it <- matrix(0, nrow = nrow(a), ncol = N_INTENS)
  dk <- numeric(nrow(a))
  for (i in seq_along(EXPOSURES)) {
    cc <- paste0("exp_", EXPOSURES[i])
    v <- if (cc %in% names(a)) as.numeric(a[[cc]]) else rep(NA_real_, nrow(a))
    dk <- dk + as.numeric(is.na(v))
    v[is.na(v)] <- 0
    it[, i] <- v
  }
  it[, IDX_HOURS + 1L] <- .norm_hours(a$hours)
  sl <- as.numeric(a$shift_level)
  sl[is.na(sl)] <- 0
  it[, IDX_SHIFT + 1L] <- sl / 2
  # **DK 缺失指示不可省**：该人年答"不知道"的 agent 数 / 10。不加它则 -121 被
  # 填成 0，与"从未暴露"不可区分 —— 而参考队列 16.5% 的在职人年至少有一个 DK。
  it[, IDX_DK + 1L] <- dk / N_EXPOSURE
  it[a$in_job != 1L, ] <- 0          # gap 年的暴露本是 NA，不是 0

  arr <- array(0, dim = c(n, L, N_INTENS))
  for (k in seq_len(N_INTENS)) {
    slice <- matrix(0, n, L)
    slice[ij] <- it[, k]
    arr[, , k] <- slice
  }
  tok$intens <- arr

  if (verbose) {
    message(sprintf("    dt (normalised SOC tenure, cap=%d): mean %.3f, sd %.3f",
                    TENURE_CAP, mean(a$dt), stats::sd(a$dt)))
    inj <- a$in_job == 1L
    message(sprintf("    \"do not know\" indicator: %.1f%% of employed person-years have at least one",
                    100 * mean(it[inj, IDX_DK + 1L] > 0)))
    if (n_unk) {
      message(sprintf("    [warn] %s employed person-years have a SOC code outside the frozen vocabulary -> UNK (%.3f%%)",
                      fmt_n(n_unk), 100 * n_unk / sum(inj)))
    }
  }
  structure(tok, class = c("ukbcareer_tokens", "list"),
            eid = uniq, sex = sex, max_len = L, level = level,
            n_unk_in_job = n_unk, n_person_years = nrow(a))
}


#' @export
print.ukbcareer_tokens <- function(x, ...) {
  cat(sprintf("<ukbcareer_tokens> %s: %s people x %d positions x 12 channels\n",
              attr(x, "sex"), fmt_n(length(attr(x, "eid"))), attr(x, "max_len")))
  cat(sprintf("  %s token positions used (with [CLS]); %s UNK person-years\n",
              fmt_n(sum(!x$pad)), fmt_n(attr(x, "n_unk_in_job"))))
  invisible(x)
}
