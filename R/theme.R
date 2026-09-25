# ══════════════════════════════════════════════════════════════════
# 绘图纪律：唯一入口 ukb_theme()，唯一出口 ukb_save()
# ══════════════════════════════════════════════════════════════════
# 这三条纪律来自上游改图阶段的实测教训，不是审美偏好：
#
# 1. **唯一入口/出口。** 上游 Python 侧 self_check 会扫出绕过 save_fig() 的
#    savefig 并 FAIL。R 侧同理：字体嵌入、dpi、光栅化层都只在一个地方设定。
#
# 2. **0 与未定义必须是浅色。** 连续色标用 YlGnBu（0 = 淡黄 #ffffd9），
#    **方向不可反**。viridis / cividis 在 0 处是深色（深紫 / 深蓝），而深色会被
#    读作"有东西" —— 当 0 的含义是"什么都没发生"时这是在图上说假话。
#    未定义域（如 n_moves = 0 的人的距离量）用灰 + 斜纹，那是它的常规画法。
#
# 3. **小样本不画。** 任何分组 n < 10 的面板留空并标注，不画点、不画箱、
#    不返回数字。薄雾类图层（桑基的 stay/out）alpha 是**乘性叠加**的 ——
#    一格里几千条 alpha 0.13 的带叠出来的灰远重于 0.13，必须分层压。

MIN_CELL_PLOT <- 10L

## 连续色标：0 = 淡黄，远 = 深蓝。**顺序不可反。**
UKB_SEQ <- c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4",
             "#1d91c0", "#225ea8", "#253494", "#081d58")

## 未定义域
UKB_NA <- "#bdbdbd"

## 性别配色。**刻意避开蓝红** —— 上游踩过三次配色撞车：蓝红撞类色块的 tab10、
## 紫金撞分段热图、洋红+深青的青撞 YlGnBu 的中段。最终洋红 + 深紫：
## YlGnBu 里没有任何紫，tab10 前四色里也没有，且两者**明度差大** ——
## 那是 1 像素宽窄条里唯一可靠的区分维度。
UKB_SEX <- c(Female = "#d01c8b", Male = "#542788")

## 9 个 SOC 大类。用 Dark2 系，**刻意不含蓝红**（避开性别编码）。
UKB_L1 <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
            "#e6ab02", "#a6761d", "#666666", "#1f78b4")

## 四类职业移动。**一律用深色** —— 浅色在几千条薄雾之上完全浮不出来。
UKB_MOVE <- c(up = "#1b7837", lateral = "#8c6d1f", down = "#b2182b",
              regrade = "#762a83", stay = "#dcdcdc", out = "#9fb0bd")


#' The ggplot2 theme used by all ukbcareer plots
#'
#' A minimal theme with small, publication-sized text, left-aligned titles and
#' captions and a light panel border. Every plot in the package uses it; add it
#' to your own ggplots to match their look.
#'
#' @section Plotting conventions used in this package:
#' * **Zero and "undefined" are drawn light.** Continuous scales use YlGnBu
#'   (0 = pale yellow), never reversed. Viridis and cividis are dark at 0, and
#'   dark reads as "something is here" -- misleading when 0 means "nothing
#'   happened". Undefined values (e.g. distance measures for people who never
#'   changed occupation) are drawn in grey.
#' * **Small cells are not drawn.** Any group with n < 10 is left blank and
#'   flagged in the caption; no points, boxes or numbers are shown for it.
#' * **Text on figures is plain ASCII English**, so that PDFs embed fonts
#'   reliably on every system.
#' * Save figures with [ukb_save] rather than `ggsave()` directly.
#'
#' @param base_size Base font size in points.
#' @param grid If `TRUE`, keep light major grid lines.
#' @return A ggplot2 theme object.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + ukb_theme()
#' }
#' @export
ukb_theme <- function(base_size = 9, grid = TRUE) {
  .need_ggplot()
  th <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "black"),
      plot.title = ggplot2::element_text(size = base_size + 1, face = "bold",
                                         hjust = 0),
      plot.subtitle = ggplot2::element_text(size = base_size - 0.5,
                                            colour = "grey30"),
      plot.caption = ggplot2::element_text(size = base_size - 1.5,
                                           colour = "grey35", hjust = 0),
      axis.text = ggplot2::element_text(size = base_size - 1),
      legend.key.size = ggplot2::unit(0.35, "cm"),
      legend.text = ggplot2::element_text(size = base_size - 1.5),
      legend.title = ggplot2::element_text(size = base_size - 1),
      strip.text = ggplot2::element_text(size = base_size - 0.5, face = "bold"),
      panel.border = ggplot2::element_rect(colour = "grey80", fill = NA,
                                           linewidth = 0.3)
    )
  if (!grid) {
    th <- th + ggplot2::theme(panel.grid = ggplot2::element_blank())
  } else {
    th <- th + ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey92",
                                               linewidth = 0.2))
  }
  th
}


#' Save a plot with consistent PDF, font and resolution settings
#'
#' The recommended way to save any ukbcareer figure. PDFs are written with
#' `cairo_pdf` (when available) so that fonts are embedded properly. **Avoid
#' calling `ggsave()` directly**: it bypasses these settings, and a PDF with
#' Type 3 fonts may be rejected by journal submission systems.
#'
#' @param plot A ggplot object.
#' @param file Output path. The file extension sets the format (e.g. `.pdf`,
#'   `.png`).
#' @param width,height Size in inches.
#' @param dpi Resolution for raster output such as `.png` (it is not passed on
#'   when saving a PDF). For plots with many thousands of semi-transparent
#'   paths, such as the sankey from [plot_flows], 250 is enough. Text and
#'   filled shapes in a PDF are vector graphics and do not depend on dpi.
#' @return `file`, invisibly.
#' @export
ukb_save <- function(plot, file, width = 7, height = 5, dpi = 250) {
  .need_ggplot()
  ext <- tolower(tools::file_ext(file))
  if (ext == "pdf") {
    # useDingbats = FALSE + cairo_pdf：保证字体被嵌入为 Type 1/TrueType 而非
    # Type 3。上游 Python 侧的 `pdf.fonttype: 42` 是同一件事。
    dev <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
    ggplot2::ggsave(file, plot, width = width, height = height, device = dev)
  } else {
    ggplot2::ggsave(file, plot, width = width, height = height, dpi = dpi)
  }
  invisible(file)
}


#' @noRd
.need_ggplot <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Plotting requires ggplot2: install.packages(\"ggplot2\")")
  }
}


#' 小样本抑制：把 n < min_cell 的行剔掉并记账
#'
#' 返回值带 `attr(, "suppressed")`（被剔掉的行数），绘图函数应把它写进图注 ——
#' **悄悄剔掉与如实标注是两件事**。
#' @noRd
.suppress_small <- function(d, n_col = "n", min_cell = MIN_CELL_PLOT) {
  if (!n_col %in% names(d)) return(d)
  bad <- !is.na(d[[n_col]]) & d[[n_col]] < min_cell
  out <- d[!bad]
  data.table::setattr(out, "suppressed", sum(bad))
  out
}


#' 图注：把口径与抑制情况写上去
#' @noRd
.cap <- function(..., suppressed = 0L, min_cell = MIN_CELL_PLOT) {
  parts <- c(...)
  if (suppressed > 0L) {
    parts <- c(parts, sprintf("%d cells with n < %d were suppressed",
                              suppressed, min_cell))
  }
  paste(parts, collapse = "\n")
}
