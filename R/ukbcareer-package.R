#' @keywords internal
#' @aliases ukbcareer-package
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table := .N .SD data.table setDT setnames fifelse rbindlist
#' @importFrom stats median quantile sd
#' @importFrom utils head tail
## usethis namespace: end
NULL

# ══════════════════════════════════════════════════════════════════
# 特殊 token —— 必须与 codes/py/02_tokenize.py:45 的定义一致（0..3 保留）
# ══════════════════════════════════════════════════════════════════
# 改这里等于改口径：任何改动都必须同步 Python 侧并重跑黄金测试（PLAN §9）。
PAD <- 0L
CLS <- 1L
MASK <- 2L
UNK <- 3L
N_SPECIAL <- 4L

## intens 的 13 维布局：10 个暴露 + 工时 + 轮班 + DK 指示。
## 索引是 **0-based**（与 Python 一致）—— 在 R 里用时记得 +1。
N_EXPOSURE <- 10L
N_INTENS <- 13L
IDX_HOURS <- 10L
IDX_SHIFT <- 11L
IDX_DK <- 12L

## SOC 层级的整除因子。**键是字符串**（Python 侧的 config `model.soc_level`
## 就是字符串 "soc_l4"）—— 用整数键曾在 fig_f8e_clustered.py 埋过一个静默 bug：
## `SOC_DIVISOR.get("soc_l4", 1)` 落到默认值 1，而 soc_l4 的除数本来就是 1，
## 所以错了也看不出来，换成 soc_l3 才会突然按 L4 取码。
SOC_DIVISOR <- c(soc_l1 = 1000L, soc_l2 = 100L, soc_l3 = 10L, soc_l4 = 1L)

# ══════════════════════════════════════════════════════════════════
# 口径默认值 —— 与 codes/config.yaml v5.0 同源
# ══════════════════════════════════════════════════════════════════
# bundle 的 MANIFEST.json 里带一份权威值，`ukb_bundle()` 载入时会比对，
# 不一致即 warn —— 避免用户悄悄换了口径还以为结果与论文可比。
DEFAULTS <- list(
  max_len = 64L,          # 参考队列实测 seq_len max 正好 60，顶到 64 即口径不一致
  age_min = 16L,
  age_max = 75L,          # **绑定约束不是惰性参数**：参考队列 6.7% 的人被封顶
  min_work_age = 14L,
  tenure_cap = 3,         # dt = min(SOC 任期, 3)/3。截顶不可省：raw tenure 与年龄
                          # 的相关是 +0.633，cap3 降到 +0.316
  long_hours_threshold = 48,
  hours_ft_threshold = 30,
  level = "soc_l4",       # 353 位。CAMSIS 的原生粒度也正是这 353 个码
  d_model = 192L,
  n_states = 9L,
  fill_year = 2015L       # **只是缺失时的回落值**，参考队列 22500 覆盖 100%
)

## 聚类口径（PLAN §4 Tier 2）。优先从 bundle 的 MANIFEST 读，这里只是回落值。
CLUSTER_DEFAULTS <- list(
  resolution = 0.2,       # 预先固定，无搜索
  knn_k = 30L,
  min_cluster_frac = 0.005,
  min_cluster_n = 500L,   # 两个条件都要满足：0.5% 在 65,641 人里只有 328 人，
                          # 低于画像可用下限
  seed = 42L
)

#' 包内使用的性别标签
#' @noRd
SEXES <- c("Female", "Male")

# ══════════════════════════════════════════════════════════════════
# UKB 特殊编码 —— **负数码是实义等级，不是缺失**
# ══════════════════════════════════════════════════════════════════
# 这是本包处理的 UKB 数据里最容易致命的陷阱：按常规"负数 = 缺失"清洗
# 会直接毁掉全部暴露与工时变量。构念效度已在上游验证（dusty 各级对应
# "该工作期间有呼吸问题"的比例 2.8% / 5.9% / 12.5% 单调）。
CODINGS <- list(
  ## 22606–22615（coding 493）：0 = Rarely/never < -131 = Sometimes < -141 = Often
  exposure_ordinal = c("0" = 0L, "-131" = 1L, "-141" = 2L),
  exposure_dk = -121L,    # Do not know → NA，但另有 DK 指示通道记录它
  ## 22604（coding 494）→ 每周小时数中点
  hours_midpoint = c("-1520" = 17.5, "-2030" = 25.0, "-3040" = 35.0,
                     "4000" = 48.0),
  shift_notdone = 9L,     # 22630/40/50：0 = 部分期间，1 = 整个期间，9 = 未做此班型
  ongoing = -313L,        # 22603 / gap 结束年 = "Ongoing when data entered"
  gap_retirement = 108L,
  gap_health = 106L,      # 因病/残疾无法工作 —— 健康选择偏倚的第一指标
  gap_codes = c(101L, 102L, 103L, 105L, 106L, 107L, 108L, -717L, -818L, -121L)
)

## gap 码 → 9 状态（+ unknown = 10）。**唯一定义在此。**
## 上游曾有三份硬编码拷贝，改字母表要三处同步，漏一处会静默标错标签而不报错。
GAP_TO_STATE <- c("107" = 3L, "105" = 4L, "103" = 5L, "101" = 6L, "106" = 7L,
                  "108" = 8L, "102" = 9L, "-717" = 9L, "-818" = 10L,
                  "-121" = 10L)

STATE_NAMES <- c("full_time", "part_time", "unemployed", "home_family",
                 "education", "marginal_work", "health", "retired", "other",
                 "unknown")

## 状态配色。**逐色取自 codes/config.yaml 的 profiles.states**（tab10），与论文
## Fig 1a 同一套 —— 上一版在这里写了另一套色，注释却声称同源，图与论文对不上。
STATE_COLOURS <- c(
  full_time = "#1f77b4", part_time = "#aec7e8", unemployed = "#d62728",
  home_family = "#ff7f0e", education = "#2ca02c", marginal_work = "#17becf",
  health = "#8c564b", retired = "#9467bd", other = "#bcbd22",
  unknown = "#c7c7c7"
)

## 10 个暴露 agent，顺序即 intens 的前 10 维 —— **顺序是契约**，不可重排。
EXPOSURES <- c("noisy", "cold", "hot", "dusty", "fumes", "cigarette",
               "asbestos", "paints", "pesticides", "diesel")

# ══════════════════════════════════════════════════════════════════
# data.table 的 NSE 列名
# ══════════════════════════════════════════════════════════════════
# `dt[, col := ...]` 里的 `col` 在 R CMD check 看来是未绑定的全局变量。
# 这是 data.table 的已知代价，声明一下即可 —— **它不是真的缺失引用**。
# 这份清单由 check 日志生成，改动列名后要重新跑一次 check 并更新。
utils::globalVariables(c(
  # `.` 是 data.table 的 `.()` 简写。它不能走 @importFrom（roxygen 会过滤掉），
  # 按 data.table 官方 FAQ 的做法声明在这里。
  ".",
  "..ec", "..sc", "..zc", "N", "a0", "a1", "after", "age", "agent",
  "agent_mean", "band", "band_lo", "c_frm", "c_to", "camsis",
  "camsis_change", "camsis_first", "camsis_last", "camsis_pattern",
  "camsis_slope", "cap_i", "career_end", "career_end_source", "ce", "ce_i",
  "check", "cnt", "code", "cohort_frac", "coverage", "d", "dist", "dt",
  "eid", "end", "exposure_breadth", "fcamsis", "first_job_age",
  "first_move_age", "first_move_rel", "flag_incomplete", "frac", "frm",
  "gap_code", "grp", "has_int", "high_exposure_years", "hours", "i.camsis",
  "i.grp", "i.load", "i.max_parallel", "i.sex", "id", "in_gap", "in_job",
  "interior", "is_parallel", "is_ret", "is_transition", "k", "kind", "l1",
  "lab", "last_age", "last_job_plus1", "last_move_age", "level",
  "long_hours_frac", "long_hours_years", "longest_agent_years", "m",
  "max_parallel", "mcamsis", "mean_camsis", "metric", "n", "n_jobs_year",
  "n_moves", "n_obs_years", "new_code", "new_spell", "node", "node_code",
  "nxt", "ny", "observed_retirement", "pct", "pos", "ppl", "prev", "pri",
  "py", "pyv", "rank0", "ref_frac", "reference", "rel", "resid", "retire",
  "retire_age", "rev_job", "row_id", "seq_kind", "sex", "shift_frac",
  "shift_level", "shift_years", "soc", "soc2000", "soc4", "soc4.x",
  "soc_change_next", "soc_f", "soc_l1", "soc_label", "spell_id", "src",
  "start", "state", "state_name", "status", "step", "tenure", "tlab", "to",
  "type", "v", "val", "value_num", "what", "work_years", "x", "y", "yy",
  "z",
  # viz-report.R（ggplot2 aes 里的列名）
  "xmin", "xmax", "ymin", "ymax", "fill", "colour", "label", "size", "xend",
  "yend", "run", "col", "j", "major", "row", "lo", "hi", "group", "needs",
  "variable", "description", "interpretation"
))
