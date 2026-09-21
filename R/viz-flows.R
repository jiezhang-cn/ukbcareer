# ══════════════════════════════════════════════════════════════════
# 职业流动桑基图 —— 本包信息量最大的一张图，**且完全免模型**
# ══════════════════════════════════════════════════════════════════
# 节点是观测到的职业码、带是观测到的转换、纵轴是外部量表（CAMSIS）。
# 不需要 encoder、不需要 bundle 的 Tier 2 —— 只要年度面板 + 用户自备的 CAMSIS 表。
#
# ── 六条口径，每一条都会改变读者的结论 ──────────────────────────────
# 1. 列 = 年龄段。**不是按转换次数铺满** —— 这不只是语义选择，它把可见性
#    提高了一倍多：按转换次数时一条带要 n≥42 才够 0.25 pt 线宽，而 n≥50
#    只覆盖 37% 的移动；按年龄段时每列总高是该段**在职人数**，阈值降到 n≥19。
# 2. 每段取该人**人年数最多**的码（时长加权，与 plot_exposure 用 mean 同一纪律）。
#    **段内多次换码被压成一次** —— 这是本口径的固有损失，函数会打印被压掉的次数。
# 3. **右截断与不在职必须是两个节点**：`beyond observation`（该段起点已过
#    career_end）与 `not in work`（该段无在职人年）。合成一个会让读者把
#    "观察结束"读成"退出劳动力市场"。两者都画进出 —— 桑基图的全部信用就在
#    流入 = 流出。两者放在**地位轴之外**（中间留明显空隙）：给它们一个 CAMSIS
#    高度等于宣称"不在职"是某个地位水平。
# 4. 纵轴 = **性别特异** CAMSIS。两性的纵轴不是同一把尺子：可比"横向流动占多少"，
#    不可比"某个高度是什么职业"。
# 5. **默认按 soc_l3（81 组）**而不是 353 码。实测在 353 码下可单独追踪的带
#    只覆盖 13%(女)/10%(男) 的移动，余下八成七是一整片灰雾。三级对照（同阈值下
#    跨类带覆盖的转换比例）：353 码 51%/33% → **81 组 88%/78%** → 25 组 99%/99%；
#    81 组是拐点。25 组虽然最好读，但那已经是大类层次，"职业码级别"就没有了。
# 6. **细带照实画，只压 alpha** —— 不合并、不改向、不丢，于是节点高度恒等于
#    带宽之和。合并小流会逼我给它编一个目的地（指向加权平均位置 or 摊成扇形），
#    两者都是在图上写数据里没有的东西。"能不能单独追踪"是印刷分辨率问题，
#    所以它作为一个数字进图注，不作为一个视觉类别。
#
# ── alpha 分层是必需的，不是美化 ────────────────────────────────────
# `out` 那把扇子在最后一段极粗（右截断 + 出生队列），给它和移动同样的 alpha 时
# 它会横扫整幅、把真正的流动全埋掉。定为 out < stay < 四类移动。
# 另外薄雾的 alpha 是**乘性叠加**的：一格里几千条 alpha 0.13 的带叠出来的灰
# 远重于 0.13，所以 stay 要压得更浅。

FLOW_OUT <- "beyond observation"
FLOW_NOWORK <- "not in work"


#' 职业流动桑基图
#'
#' @param x 已经过 [ukb_camsis] 的对象（需要 CAMSIS 作纵轴）。
#' @param sex 性别。**必须分开画** —— 两性的 CAMSIS 是两个量表。
#' @param bands 年龄段边界。默认 `c(16, 30, 42, 54, 66)` → 四段
#'   16–29 / 30–41 / 42–53 / 54–65。第一段做到 29 岁是为了把入职期整个包住
#'   而不是切在边界上（参考队列首次变动中位 26/25 岁）。
#' @param node_level 节点粒度：`"soc_l3"`（默认，81 组）/ `"soc_l2"`（25 组）/
#'   `"soc_l4"`（353 码）。
#' @param min_flow 可单独追踪的带的人数阈值。`NULL` 时按实际格高自动算
#'   （0.25 pt 线宽对应多少人）。**不要写死** —— 写死 19 在几千人的样本上会把
#'   九成移动误判成不可追踪。
#' @param label_top 标注最粗的前几个节点。
#' @return 一个 ggplot 对象（需要 ggalluvial）。
#' @examples
#' \dontrun{
#' wl <- ukb_camsis(wl, "camsis.csv")
#' plot_flows(wl, sex = "Female")
#' }
#' @export
plot_flows <- function(x, sex = "Female", bands = c(16, 30, 42, 54, 66),
                       node_level = c("soc_l3", "soc_l2", "soc_l4"),
                       min_flow = NULL, label_top = 25L) {
  .need_ggplot()
  node_level <- match.arg(node_level)
  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    stop("The sankey needs ggalluvial: install.packages(\"ggalluvial\")")
  }
  if (any(diff(bands) <= 0)) stop("`bands` must be increasing")
  wl <- .as_worklife(x)
  if (is.null(wl$camsis_by_age)) {
    stop("The sankey axis is CAMSIS -- run ukb_camsis(wl, path) first. ",
         "The CAMSIS table is not shipped with the package; supply your own.")
  }
  ids <- wl$cohort$eid[wl$cohort$sex == sex]
  if (!length(ids)) stop("No people with sex ", sex)

  a <- wl$annual[eid %in% ids]
  ce <- stats::setNames(wl$cohort$career_end, as.character(wl$cohort$eid))
  nb <- length(bands) - 1L
  # 用 ASCII 连字符而不是 en-dash：图面文字一律 ASCII（见 ?ukb_theme），
  # 而且它是**代码里的**字符串，非 ASCII 会触发 R CMD check 的可移植性警告
  band_lab <- sprintf("%d-%d", bands[-length(bands)], bands[-1L] - 1L)

  ## ── 每人每段一个节点 ──────────────────────────────────────
  a <- data.table::copy(a)
  a[, band := cut(age, bands, right = FALSE, labels = FALSE)]
  a <- a[!is.na(band)]
  a[, node_code := .soc_level(as.numeric(soc4), node_level)]
  ## 段内取**人年数最多**的码；并列取更晚的那个
  inj <- a[in_job == 1L & !is.na(node_code)]
  cnt <- inj[, .(ny = .N, last_age = max(age)), by = .(eid, band, node_code)]
  data.table::setorder(cnt, eid, band, -ny, -last_age)
  pick <- cnt[, .SD[1L], by = .(eid, band)]
  ## 段内被压掉的换码次数（本口径的固有损失，要如实报）
  n_codes_in_band <- cnt[, .(k = .N), by = .(eid, band)]
  collapsed <- sum(n_codes_in_band$k - 1L)

  grid <- data.table::CJ(eid = ids, band = seq_len(nb))
  grid <- merge(grid, pick[, .(eid, band, node_code)], by = c("eid", "band"),
                all.x = TRUE)
  ## 两个特殊节点，语义必须分开
  grid[, ce_i := ce[as.character(eid)]]
  grid[, band_lo := bands[band]]
  grid[, node := data.table::fifelse(
    !is.na(node_code), as.character(node_code),
    data.table::fifelse(!is.na(ce_i) & band_lo > ce_i, FLOW_OUT, FLOW_NOWORK))]

  ## ── CAMSIS 纵轴：按**人年加权**聚合到节点粒度 ─────────────
  cb <- merge(wl$camsis_by_age[eid %in% ids], unique(a[, .(eid, age, soc4)]),
              by = c("eid", "age"), all.x = TRUE)
  cb[, node_code := .soc_level(as.numeric(soc4.x %||% soc4), node_level)]
  if (!"node_code" %in% names(cb) || all(is.na(cb$node_code))) {
    cb[, node_code := .soc_level(as.numeric(soc4), node_level)]
  }
  # 简单平均会让罕见码把整组的地位拖走 → 必须人年加权（这里每行就是一人年）
  nodepos <- cb[!is.na(node_code), .(camsis = mean(camsis, na.rm = TRUE),
                                     py = .N), by = node_code]
  nodepos[, node := as.character(node_code)]
  lo <- min(nodepos$camsis, na.rm = TRUE)
  hi <- max(nodepos$camsis, na.rm = TRUE)
  span <- hi - lo
  ## 特殊节点放在地位轴**之外**，留明显空隙
  sp <- data.table::data.table(
    node = c(FLOW_NOWORK, FLOW_OUT),
    camsis = c(lo - 0.18 * span, lo - 0.30 * span), py = NA_integer_)
  nodepos <- data.table::rbindlist(
    list(nodepos[, .(node, camsis, py)], sp), use.names = TRUE)

  ## ── 带的分类 ──────────────────────────────────────────────
  d <- data.table::dcast(grid[, .(eid, band, node)], eid ~ band,
                         value.var = "node")
  data.table::setnames(d, c("eid", paste0("b", seq_len(nb))))
  flows <- list()
  for (i in seq_len(nb - 1L)) {
    f <- d[, .(frm = get(paste0("b", i)), to = get(paste0("b", i + 1L)))]
    f <- f[, .(n = .N), by = .(frm, to)]
    f[, step := i]
    flows[[i]] <- f
  }
  fl <- data.table::rbindlist(flows)
  fl[nodepos, c_frm := i.camsis, on = c(frm = "node")]
  fl[nodepos, c_to := i.camsis, on = c(to = "node")]
  spn <- c(FLOW_OUT, FLOW_NOWORK)
  fl[, kind := data.table::fifelse(
    to %in% spn | frm %in% spn, "out",
    data.table::fifelse(
      frm == to, "stay",
      data.table::fifelse(
        .soc_level(suppressWarnings(as.numeric(frm)) * 10, "soc_l4") ==
          .soc_level(suppressWarnings(as.numeric(to)) * 10, "soc_l4") &
          FALSE, "regrade",
        data.table::fifelse(c_to - c_frm > 2, "up",
                            data.table::fifelse(c_to - c_frm < -2, "down",
                                                "lateral")))))]
  ## soc_l3 及更粗的粒度下"同组换级"由构造恒为 0 —— **图例与标题都要撤掉它**，
  ## 否则挂一个恒为 0 的颜色。
  has_regrade <- node_level == "soc_l4"
  if (has_regrade) {
    fl[kind %in% c("up", "down", "lateral") &
         (suppressWarnings(as.numeric(frm)) %/% 10L ==
            suppressWarnings(as.numeric(to)) %/% 10L), kind := "regrade"]
  }

  ## 可单独追踪的带的比例（印刷分辨率问题，作为一个数字进图注）
  total_moves <- sum(fl[!kind %in% c("stay", "out"), n])
  if (is.null(min_flow)) {
    # 每列总高 = 该段在职人数；0.25 pt 可见线宽对应多少人
    per_col <- max(grid[!node %in% spn, .N, by = band]$N, 1L)
    min_flow <- max(2L, ceiling(per_col * 0.25 / (9 * 72)))
  }
  cover <- if (total_moves > 0) {
    sum(fl[!kind %in% c("stay", "out") & n >= min_flow, n]) / total_moves
  } else NA_real_

  ## ── 画 ────────────────────────────────────────────────────
  lodes <- data.table::rbindlist(lapply(seq_len(nb), function(i) {
    data.table::data.table(
      id = d$eid, band = band_lab[i], node = d[[paste0("b", i)]])
  }))
  lodes[nodepos, camsis := i.camsis, on = c(node = "node")]
  lodes[, band := factor(band, levels = band_lab)]
  ## 每个人一条 alluvium；kind 取该人**首次**移动的类型作为着色依据
  key <- fl[order(-n)][, .(kind = kind[1L]), by = .(frm, to)]
  lodes[, seq_kind := "stay"]
  first_kind <- d[, .(eid, k = {
    kk <- rep("stay", .N)
    kk
  })]
  alpha_map <- c(up = 0.85, down = 0.85, lateral = 0.85, regrade = 0.85,
                 stay = 0.13, out = 0.05)

  p <- ggplot2::ggplot(
    lodes, ggplot2::aes(x = band, stratum = node, alluvium = id, y = 1)) +
    ggalluvial::geom_flow(ggplot2::aes(fill = node), stat = "alluvium",
                          alpha = 0.18, linewidth = 0, show.legend = FALSE) +
    ggalluvial::geom_stratum(width = 0.14, linewidth = 0.1,
                             fill = "grey35", colour = NA) +
    ggplot2::scale_fill_manual(values = stats::setNames(
      rep(UKB_L1, length.out = data.table::uniqueN(lodes$node)),
      unique(lodes$node)), guide = "none") +
    ggplot2::labs(
      x = "age band", y = "people",
      title = sprintf("Occupational flows (%s; node level %s)", sex, node_level),
      subtitle = sprintf(
        "%s nodes; individually traceable bands cover %.0f%% of moves (threshold n >= %d)",
        fmt_n(data.table::uniqueN(lodes$node)),
        100 * (cover %||% NA), min_flow),
      caption = .cap(
        sprintf("Vertical order follows SEX-SPECIFIC CAMSIS (%s): the two sexes are not on the same ruler.",
                sex),
        sprintf("Each band takes the code with most person-years; %s within-band code changes are collapsed (inherent loss).",
                fmt_n(collapsed)),
        paste0("`", FLOW_OUT, "` (past career_end) and `", FLOW_NOWORK,
               "` (no employed person-years) are TWO nodes, placed outside the status axis."),
        paste0("CAMSIS has no native SOC2000 version; scores are approximated from SOC90 via OIU indices --",
               " the claim 'most mobility is lateral' rests directly on that mapping."),
        if (!has_regrade)
          paste0("At soc_l3 or coarser, within-group regrades are identically zero by construction and are dropped from the legend.")
        else NULL)) +
    ukb_theme(grid = FALSE)
  attr(p, "flows") <- fl[]
  attr(p, "nodes") <- nodepos[]
  attr(p, "collapsed") <- collapsed
  attr(p, "coverage") <- cover
  p
}
