# ══════════════════════════════════════════════════════════════════
# Tier 2：职业生涯类型学（在用户样本上重跑 Leiden）
# ══════════════════════════════════════════════════════════════════
# 对应 codes/py/04_typology.py 的 knn_graph / leiden / merge_small。
#
# ── 这里是"同方法学"而不是"同分区"，这个区别很重要 ─────────────────────
# Leiden 是**转导式**的，没有 `predict`。所以本包不把你的样本分配到我们已发表的
# K=12(女)/9(男) 个型上，而是用**同一套口径**在你自己的样本上重跑一遍聚类。
#
# 后果，必须在写文章时说明：
#   · 你得到的型**不是**我们的 T0–T11。编号与语义都不通，K 也很可能不同。
#   · 跨研究可比的是**方法与表征**（同一个冻结编码器、同一套聚类参数），
#     不是**分区**。不要把你的 "T3" 与论文的 "T3" 对照解读。
#   · 好处是没有分配误差，也不必分发簇中心。
#
# 另外必须知道分区本身有多稳：参考队列上 S1 bootstrap ARI 女 0.515（低于预注册的
# 0.60 参考线）、男 0.618，最大簇女性占 48.1%，某个型的 SOC 纯度高达 0.995
# （即它实质上就是一个职业码）。这是**方法的属性**，对你的样本同样适用。
# `ukb_bundle_info()` 会把这些数字打出来。

#' 无权 kNN 图
#'
#' **必须在原始 192 维 z 上建图，不能在 UMAP 坐标上。** 合成验证（3 个良好分离的
#' 192 维高斯球）：192 维图 ARI 1.000、UMAP 的加权 fuzzy 图 1.000，而
#' **2 维 UMAP 坐标上重建的图只有 0.259–0.432**（把 3 个簇切成 9–15 个）。
#' UMAP 坐标不是分析空间。
#' @noRd
.knn_graph <- function(z, k = 30L) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("igraph is required: install.packages(\"igraph\")")
  }
  nn <- .knn_index(z, k)
  n <- nrow(z)
  el <- cbind(rep(seq_len(n), each = k), as.vector(t(nn)))
  g <- igraph::graph_from_edgelist(el, directed = FALSE)
  # simplify：kNN 是有向的，i→j 与 j→i 同时存在时会成重边。
  # 上游 Python 侧 `g.simplify()` 做的是同一件事 —— **图必须是无权的**。
  igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
}


#' k 近邻索引
#'
#' 有 RANN / FNN 时用它们（KD 树）；否则回落到分块暴力。
#' 分块是必需的：5 万人的完整距离矩阵是 20 GB。
#' @noRd
.knn_index <- function(z, k = 30L, block = 1000L) {
  n <- nrow(z)
  if (k >= n) stop("k (", k, ") must be smaller than the sample size (", n, ")")
  if (requireNamespace("RANN", quietly = TRUE)) {
    return(RANN::nn2(z, k = k + 1L)$nn.idx[, -1L, drop = FALSE])
  }
  if (requireNamespace("FNN", quietly = TRUE)) {
    return(FNN::get.knn(z, k = k)$nn.index)
  }
  if (n > 20000L) {
    message("  [tip] installing RANN or FNN speeds up the k-NN search a lot: ",
            "install.packages(\"RANN\")")
  }
  sq <- rowSums(z^2)
  out <- matrix(NA_integer_, n, k)
  for (i in seq(1L, n, by = block)) {
    j <- min(i + block - 1L, n)
    # ||a-b||² = |a|² + |b|² - 2a·b —— 走 BLAS 的 crossprod
    d2 <- outer(sq[i:j], sq, "+") - 2 * (z[i:j, , drop = FALSE] %*% t(z))
    d2[cbind(seq_len(j - i + 1L), i:j)] <- Inf     # 排除自己
    for (r in seq_len(j - i + 1L)) {
      out[i + r - 1L, ] <- order(d2[r, ])[seq_len(k)]
    }
  }
  out
}


#' 把过小的簇合并入最近簇
#'
#' 下界是 `max(min_frac × n, min_n)`，**两个条件都要满足**。人数是实质约束：
#' `min_frac = 0.005` 在 65,641 人里只有 328 人，低于型画像可用的下限
#' （每型要 medoid、要 CAMSIS 置信区间）。Leiden 在 kNN 图上**总会**产生
#' 几百人的微小社群（参考队列最小簇占 0.1–0.5%），这是算法的固有行为而非数据问题。
#' @noRd
.merge_small <- function(lab, z, min_frac = 0.005, min_n = 500L) {
  lab <- as.integer(lab)
  n <- length(lab)
  floor_n <- max(min_frac * n, min_n)
  repeat {
    tb <- sort(table(lab), decreasing = TRUE)
    small <- names(tb)[tb < floor_n]
    if (!length(small) || length(tb) <= 2L) break
    s <- as.integer(small[length(small)])          # 从最小的开始
    big <- as.integer(names(tb)[tb >= floor_n])
    big <- setdiff(big, s)
    # **没有任何簇达到下界时，这个函数是空操作而不是"并成两个"。** 小样本上
    # 每个簇都小于 500 → big 为空 → 立刻退出，一个都不合并。Python 侧同样如此。
    # 后果是用户会静默拿到一堆低于画像可用下界的型，min_cluster_n 形同不存在 ——
    # 所以 ukb_typology() 必须在**跑完之后**检查最小簇并明确警告。
    if (!length(big)) break
    cs <- colMeans(z[lab == s, , drop = FALSE])
    dd <- vapply(big, function(cc) {
      sqrt(sum((colMeans(z[lab == cc, , drop = FALSE]) - cs)^2))
    }, numeric(1))
    lab[lab == s] <- big[which.min(dd)]
  }
  out <- as.integer(factor(lab, levels = sort(unique(lab)))) - 1L  # 重编号 0..K-1
  attr(out, "floor_n") <- floor_n
  out
}


#' Career typology: re-run Leiden clustering on your own sample
#'
#' Clusters people into career types using the same method as the paper,
#' applied to your sample: Leiden with `resolution = 0.2` (fixed in advance,
#' no search), on an unweighted 30-nearest-neighbour graph built from the raw
#' 192-dimensional representations, with a minimum cluster size of
#' `max(0.005 * n, 500)`. By default these settings are read from the
#' bundle's `MANIFEST.json` rather than hard-coded in R. Requires the output
#' of [ukb_encode] (which needs the model bundle's encoder and the torch
#' package) and the igraph package.
#'
#' Each sex is clustered separately: the two sexes' representations come
#' from two independently trained encoders, so clustering them in one graph
#' would be meaningless.
#'
#' @section Same method, not the same partition:
#' Leiden is transductive -- it has no `predict()`. So this function does
#' **not** assign your participants to the published 12 (women) / 9 (men)
#' types. It re-runs the clustering on your sample with the same settings.
#' When you write up, make clear that:
#' * Your types are **not** the paper's T0-T11. Neither the numbering nor the
#'   meaning carries over, and K will probably differ.
#' * What is comparable across studies is the **method and the
#'   representation** (the same frozen encoder, the same clustering settings),
#'   not the partition. Do not interpret your "T3" against the paper's "T3".
#' * The upside: there is no assignment error, and no cluster centres need to
#'   be distributed.
#'
#' You should also know how stable the partition itself is. In the reference
#' cohort the bootstrap ARI was 0.515 for women (below the pre-registered
#' 0.60 reference line) and 0.618 for men; the largest cluster held 48.1% of
#' women; and one type had an SOC purity of 0.995 (it is essentially a single
#' occupation code). These are properties of the method and apply to your
#' sample too. [ukb_bundle_info] prints these numbers.
#'
#' @param x An object that has been through [ukb_encode].
#' @param bundle Output of [ukb_bundle].
#' @param resolution,knn_k,min_cluster_frac,min_cluster_n Clustering settings.
#'   `NULL` (recommended) reads them from the bundle.
#' @param seed Random seed. Leiden is stochastic, and **seeds in R's igraph
#'   and Python's leidenalg are not equivalent**: the same data will not give
#'   bit-identical labels in the two languages (though K and the overall
#'   structure should agree).
#' @param n_seeds Number of different seeds to run in order to **measure how
#'   sensitive the partition is to the seed** (default 5). The result is stored
#'   in `attr(x$types, "diagnostics")[[sex]]$seed_ari` (median pairwise ARI).
#'   Set to 1 to skip (faster, but you lose this number). **This number
#'   matters**: in a sample of about 1,000 people, changing only the seed
#'   brought the ARI down to 0.28-0.73 -- the data did not change, yet the
#'   partition did. Check it before entering `type` into a regression as if it
#'   were a fixed grouping.
#' @param verbose If `TRUE`, print K, cluster sizes, dominance diagnostics and
#'   seed sensitivity.
#' @return `x` with an added element `types` (columns `eid`, `sex`, `type`).
#'   `attr(x$types, "diagnostics")` holds, for each sex, K, the largest
#'   cluster's share, the size-entropy ratio, seed sensitivity, and two
#'   **dominance** diagnostics: whether a cluster is just a restatement of an
#'   occupation code (`max_cluster_soc_purity`, `nmi_mode_soc`) and whether
#'   the typology is a function of `career_end` (`nmi_career_end_decile`).
#'   Warnings are issued if seed sensitivity is high (median ARI < 0.80) or if
#'   any type falls below the minimum cluster size.
#' @examples
#' \dontrun{
#' wl <- ukb_typology(wl, bundle = b)
#' table(wl$types$type, wl$types$sex)
#' }
#' @export
ukb_typology <- function(x, bundle, resolution = NULL, knn_k = NULL,
                         min_cluster_frac = NULL, min_cluster_n = NULL,
                         seed = NULL, n_seeds = 5L, verbose = TRUE) {
  stopifnot(inherits(x, "ukbcareer_worklife"))
  if (is.null(x$latent)) stop("Run ukb_encode() first")
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("igraph is required: install.packages(\"igraph\")")
  }
  cc <- if (inherits(bundle, "ukbcareer_bundle")) bundle$cluster else CLUSTER_DEFAULTS
  res <- resolution %||% cc$resolution
  k <- as.integer(knn_k %||% cc$knn_k)
  mf <- min_cluster_frac %||% cc$min_cluster_frac
  mn <- as.integer(min_cluster_n %||% cc$min_cluster_n)
  sd_ <- as.integer(seed %||% cc$seed %||% 42L)

  zc <- grep("^z[0-9]{3}$", names(x$latent), value = TRUE)
  out <- list()
  diag <- list()
  for (sx in levels(x$latent$sex)) {
    sub <- x$latent[sex == sx]
    if (!nrow(sub)) next
    z <- as.matrix(sub[, ..zc])
    g <- .knn_graph(z, k)
    one <- function(sd_i) {
      set.seed(sd_i)
      # objective_function = "modularity" 对应 leidenalg 的
      # RBConfigurationVertexPartition（带 resolution 的模块度），与上游同一目标。
      # igraph 2.1.0 起 `resolution_parameter` 改名 `resolution` —— 两边都支持。
      lei_args <- list(g, objective_function = "modularity", n_iterations = 2L)
      nm <- if ("resolution" %in% names(formals(igraph::cluster_leiden))) {
        "resolution"
      } else {
        "resolution_parameter"
      }
      lei_args[[nm]] <- res
      p <- do.call(igraph::cluster_leiden, lei_args)
      .merge_small(as.integer(igraph::membership(p)), z, mf, mn)
    }
    lab <- one(sd_)
    raw_k <- length(unique(lab))
    floor_n <- attr(lab, "floor_n")

    ## 种子敏感性：**数据完全不变、只换随机种子**，分区会变多少？
    ## 这比 bootstrap 更直接地回答"这个分区有多确定"。实测在约 1,000 人的样本上
    ## ARI 能掉到 0.28 —— 那时 `type` 不该当确定分组用。
    seed_ari <- NA_real_
    if (n_seeds > 1L) {
      labs <- c(list(lab), lapply(seq_len(n_seeds - 1L),
                                  function(i) one(sd_ + i * 1000L)))
      pr <- utils::combn(length(labs), 2L)
      seed_ari <- stats::median(apply(pr, 2L, function(ij) {
        .ari(labs[[ij[1L]]], labs[[ij[2L]]])
      }))
    }
    tb <- sort(table(lab), decreasing = TRUE)
    fr <- as.numeric(tb) / length(lab)
    d <- list(K = length(tb), K_raw = raw_k, seed_ari = seed_ari,
              n_seeds = as.integer(n_seeds),
              sizes = as.integer(tb), min_n = min(as.integer(tb)),
              max_frac = max(fr), min_frac = min(fr),
              entropy_ratio = if (length(tb) < 2L) 0 else
                -sum(fr * log(fr)) / log(length(tb)),
              resolution = res, knn_k = k, seed = sd_,
              floor_n = floor_n, n = nrow(z),
              below_floor = sum(as.integer(tb) < floor_n))
    ## 支配度诊断：**只报告，不阻断**
    d <- c(d, .dominance(lab, sub$eid, x, verbose, sx))
    diag[[sx]] <- d
    out[[sx]] <- data.table::data.table(eid = sub$eid, sex = sub$sex,
                                        type = lab)
    if (verbose) {
      message(sprintf("  [%s] K=%d (before merging %d), sizes %s", sx, d$K, d$K_raw,
                      paste(d$sizes, collapse = "/")))
      message(sprintf("       largest cluster %.1f%%, size-entropy ratio %.3f",
                      100 * d$max_frac,
                      d$entropy_ratio))
      if (d$max_frac > 0.60) {
        message("       [warn] the largest cluster exceeds 60% -- this is not ",
                "\"K types\" but \"one body plus a few satellites\". The body is ",
                "probably continuous; word your conclusions accordingly.")
      }
      if (!is.na(d$seed_ari)) {
        message(sprintf(paste0("       seed sensitivity: over %d seeds, median ",
                             "pairwise ARI %.3f"), d$n_seeds, d$seed_ari))
      }
    }
    ## 种子敏感性低 = **数据完全不变、分区却变了**。这时 `type` 不是一个确定的
    ## 分组变量，把它当金标准丢进回归会把分区噪声当成真信号。
    if (!is.na(d$seed_ari) && d$seed_ari < 0.80) {
      warning(sprintf(paste0(
        "%s: changing only the random seed gives a median pairwise ARI of ",
        "%.3f across %d seeds.\n",
        "  That means **the data did not change and the partition did** -- ",
        "`type` is not a determinate grouping variable in this sample.\n",
        "  Suggested: (1) treat it as one variant in a sensitivity analysis ",
        "rather than the main exposure, or (2) use only the continuous ",
        "work-history and mobility measures (ukb_career(), ukb_movement()), ",
        "which do not have this problem.\n",
        "  For reference, the bootstrap ARI on our own cohort is 0.515 (female) ",
        "/ 0.618 (male); see ukb_bundle_info()."),
        sx, d$seed_ari, d$n_seeds), call. = FALSE)
    }
    ## **必须在跑完之后检查**：merge_small 在"没有任何簇达到下界"时是空操作，
    ## 不会报错也不会合并。不检查的话用户静默拿到一堆过小的型。
    if (d$below_floor > 0L) {
      warning(sprintf(paste0(
        "%s: %d of %d types fall below the floor of %s (smallest %s, sample ",
        "%s people).\n",
        "  merge_small can only fold small clusters into clusters that reach ",
        "the floor; when none does, it is a no-op -- so these types were not ",
        "merged at all.\n",
        "  Types below %s people cannot support a profile (each needs a medoid ",
        "and CAMSIS confidence intervals). With a sample this size, consider ",
        "skipping the typology and using the continuous work-history and ",
        "mobility measures (ukb_career(), ukb_movement()) instead."),
        sx, d$below_floor, d$K, fmt_n(floor_n), fmt_n(d$min_n),
        fmt_n(d$n), fmt_n(floor_n)), call. = FALSE)
    }
  }
  ty <- data.table::rbindlist(out, use.names = TRUE)
  data.table::setattr(ty, "diagnostics", diag)
  data.table::setattr(ty, "caveat", paste0(
    "These types come from re-running Leiden on YOUR sample; they are not ",
    "the paper's T0-T11. Neither the numbering nor the meaning carries over, ",
    "so do not match individual types across studies."))
  x$types <- ty
  x
}


#' 簇层面的支配度诊断
#'
#' `ukb_encode` 的诊断是**个体层面**的（表征有效秩），抓不到两个簇层面的退化：
#'
#' 1. **簇 = 职业码？** 若某簇 90% 是同一个 SOC 码，它是职业标签而非"生涯类型"。
#'    参考队列 36.8% 的人终生只报一个码，所以这个退化很容易发生 —— 而且它会让
#'    ASW **上升**（单码孤岛分离度极好），科学价值反而下降。
#'    **单码主导本身不是错误** —— 护士/教师/技工的生涯本就同质，那是表征按职业内容
#'    组织的正常表现；这里报的是程度，供命名时判断某簇是否等价于单一职业码。
#' 2. **簇 = career_end 的函数？** 这比个体层面的 R² 更直接：它问的是
#'    "分型本身是不是这个变量的重新表述"。
#'
#' 两个都是 outcome-blind（只用输入侧变量）。
#' @noRd
.dominance <- function(lab, eids, x, verbose, sx) {
  out <- list()
  ## 1. 每簇的众数 SOC 占比
  a <- x$annual[eid %in% eids & in_job == 1L, .(eid, soc4)]
  if (nrow(a)) {
    md <- a[, .(mode_soc = as.numeric(names(sort(table(soc4),
                                                 decreasing = TRUE))[1L])),
            by = eid]
    md <- md[match(eids, eid)]
    pur <- vapply(split(md$mode_soc, lab), function(v) {
      v <- v[!is.na(v)]
      if (!length(v)) return(NA_real_)
      max(table(v)) / length(v)
    }, numeric(1))
    out$max_cluster_soc_purity <- max(pur, na.rm = TRUE)
    out$nmi_mode_soc <- .nmi(lab, md$mode_soc)
    if (verbose && out$max_cluster_soc_purity > 0.70) {
      message(sprintf(paste0("       [note] one type has %.1f%% of its members ",
                             "sharing the same modal occupation code -- it ",
                             "looks more like an occupation label than a ",
                             "career type"),
                      100 * out$max_cluster_soc_purity))
    }
  }
  ## 2. 与 career_end 十分位的互信息
  ce <- x$cohort[match(eids, eid), career_end]
  if (sum(!is.na(ce)) > 10L) {
    dec <- .safe_decile(ce)
    out$nmi_career_end_decile <- .nmi(lab, dec)
    if (verbose && !is.na(out$nmi_career_end_decile) &&
        out$nmi_career_end_decile > 0.25) {
      message(sprintf(paste0("       [warn] NMI with career_end is %.3f -- the ",
                             "typology may be a restatement of career length"),
                      out$nmi_career_end_decile))
    }
  }
  out
}


#' 调整兰德指数
#'
#' 两个分区的一致性，已对随机一致作调整（随机分区期望 0，完全相同 1）。
#' 用它而不是"标签相同的比例"：后者在标签任意重编号下没有意义。
#' @noRd
.ari <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) < 4L) return(NA_real_)
  t <- table(a[ok], b[ok])
  n <- sum(t)
  ci <- function(x) sum(choose(x, 2))
  idx <- ci(as.vector(t))
  ea <- ci(rowSums(t)) * ci(colSums(t)) / choose(n, 2)
  mx <- (ci(rowSums(t)) + ci(colSums(t))) / 2
  if (mx == ea) return(NA_real_)          # K=1 时 ARI 无定义（**不要返回 1.0**）
  (idx - ea) / (mx - ea)
}


#' 归一化互信息
#' @noRd
.nmi <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) < 10L) return(NA_real_)
  a <- a[ok]
  b <- b[ok]
  p <- table(a, b) / sum(ok)
  pa <- rowSums(p)
  pb <- colSums(p)
  ha <- -sum(pa[pa > 0] * log(pa[pa > 0]))
  hb <- -sum(pb[pb > 0] * log(pb[pb > 0]))
  hab <- -sum(p[p > 0] * log(p[p > 0]))
  if (ha <= 0 || hb <= 0) return(0)
  # MI = H(A) + H(B) − H(A,B)。**符号容易写反** —— 写反时 NMI 恒为负，于是
  # "分型只是生涯长度的重新表述"这个判据永远不会触发（一个静默失效的门槛）。
  max(0, (ha + hb - hab)) / sqrt(ha * hb)
}


#' 十分位（取值集中时自动减少组数，不报错）
#' @noRd
.safe_decile <- function(v) {
  br <- unique(stats::quantile(v, seq(0, 1, 0.1), na.rm = TRUE))
  if (length(br) < 3L) return(rep(1L, length(v)))
  as.integer(cut(v, br, include.lowest = TRUE))
}
