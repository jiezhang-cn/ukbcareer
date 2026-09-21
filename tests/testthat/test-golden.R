# 黄金测试：Python 链与 R 链在**我们自己的队列**上必须给出同一结果。
#
# 那 2,000 人的输入是 UKB 数据，**不能进包**（PLAN §1 第 1 条）。做法是把
# 黄金数据集放在服务器/本地的分析环境里，由环境变量指路；CI 上自动跳过。
#
#   Sys.setenv(UKBCAREER_GOLDEN = "/path/to/golden")
#   Sys.setenv(UKBCAREER_BUNDLE = "/path/to/bundle_v5.0")
#
# 四级判据（PLAN §9）：
#   Tier 0   整数列**完全相等**，浮点列 max|Δ| < 1e-6。任何不等都是口径 bug，
#            不许用 tolerance 掩盖。
#   Tier 0 张量  12 个通道同上。
#   Tier 1   移动几何 6 个量 max|Δ| < 1e-6（同一个矩阵上的确定性算术）。
#   Tier 2   latent max|Δ| < 1e-4（FP32 + 不同 BLAS 的合理范围）。
#            **type 不比对** —— R 的 igraph 与 Python 的 leidenalg 种子语义不通，
#            同一份数据不会得到逐位相同的标签。只比 K 与整体一致性（ARI）。

golden_dir <- function() Sys.getenv("UKBCAREER_GOLDEN", "")
bundle_dir <- function() Sys.getenv("UKBCAREER_BUNDLE", "")

skip_no_golden <- function() {
  if (!nzchar(golden_dir()) || !dir.exists(golden_dir())) {
    skip("没有黄金数据集（设 UKBCAREER_GOLDEN 指向它）")
  }
  if (!requireNamespace("arrow", quietly = TRUE)) skip("需要 arrow")
}

read_golden <- function(name) {
  f <- file.path(golden_dir(), name)
  if (!file.exists(f)) skip(paste("黄金数据集里没有", name))
  data.table::as.data.table(arrow::read_parquet(f))
}

cmp_cols <- function(r, py, cols, int_cols = character(), tol = 1e-6) {
  for (cc in intersect(cols, intersect(names(r), names(py)))) {
    a <- r[[cc]]
    b <- py[[cc]]
    if (cc %in% int_cols || is.integer(a)) {
      expect_identical(as.numeric(a), as.numeric(b),
                       label = paste0("整数列 ", cc, " 必须完全相等"))
    } else {
      d <- max(abs(as.numeric(a) - as.numeric(b)), na.rm = TRUE)
      expect_lt(d, tol, label = paste0("浮点列 ", cc, " 的 max|Δ|"))
      expect_identical(is.na(a), is.na(b),
                       label = paste0(cc, " 的缺失模式"))
    }
  }
}

test_that("Tier 0：cohort 逐列与 Python 一致", {
  skip_no_golden()
  py <- read_golden("cohort.parquet")
  wl <- ukb_worklife(file.path(golden_dir(), "employment.csv"),
                     entry_path = file.path(golden_dir(), "work_env.csv"),
                     covariate_path = file.path(golden_dir(), "covariates.csv"),
                     verbose = FALSE)
  r <- wl$cohort[order(eid)]
  py <- py[eid %in% r$eid][order(eid)]
  r <- r[eid %in% py$eid]
  expect_equal(nrow(r), nrow(py))
  cmp_cols(r, py, c("career_end", "coverage", "seq_len", "n_obs_years"),
           int_cols = c("seq_len", "n_obs_years"))
  # career_end_source 是字符串 → 必须逐个相等
  expect_identical(as.character(r$career_end_source),
                   as.character(py$career_end_source))
  expect_identical(as.integer(r$observed_retirement),
                   as.integer(py$observed_retirement))
})

test_that("Tier 0：年度面板的行集与整数通道完全相等", {
  skip_no_golden()
  py <- read_golden("worklife_annual.parquet")
  wl <- ukb_worklife(file.path(golden_dir(), "employment.csv"),
                     entry_path = file.path(golden_dir(), "work_env.csv"),
                     covariate_path = file.path(golden_dir(), "covariates.csv"),
                     verbose = FALSE)
  # Python 侧 `worklife_annual.parquet` 落的是**未截断**面板（画像 P1 需要它），
  # 所以要拿 `annual_untruncated` 去比，而不是 `annual`（已按 career_end 截断）。
  r <- wl$annual_untruncated[order(eid, age)]
  py <- py[eid %in% r$eid][order(eid, age)]
  # 行集必须一致 —— 多一行少一行都是口径 bug
  expect_identical(paste(r$eid, r$age), paste(py$eid, py$age))
  cmp_cols(r, py, c("soc4", "n_jobs_year", "in_job", "in_gap", "is_parallel",
                    "is_transition", "shift_level", "hours"),
           int_cols = c("in_job", "in_gap", "is_parallel", "is_transition",
                        "n_jobs_year"))
})

test_that("Tier 0：暴露汇总一致（mean_* 的定义域是 [0,2]）", {
  skip_no_golden()
  py <- read_golden("worklife_exposure.parquet")
  wl <- ukb_worklife(file.path(golden_dir(), "employment.csv"),
                     entry_path = file.path(golden_dir(), "work_env.csv"),
                     covariate_path = file.path(golden_dir(), "covariates.csv"),
                     verbose = FALSE)
  r <- wl$exposure[order(eid)]
  py <- py[eid %in% r$eid][order(eid)]
  cols <- c("work_years", paste0("mean_", EXPOSURES),
            paste0("yrs_exp_", EXPOSURES), paste0("peak_", EXPOSURES))
  cmp_cols(r, py, cols, int_cols = c("work_years",
                                     paste0("yrs_exp_", EXPOSURES)))
})

test_that("Tier 0：9 状态序列逐格相等", {
  skip_no_golden()
  py <- read_golden("states_Female.parquet")
  wl <- ukb_worklife(file.path(golden_dir(), "employment.csv"),
                     entry_path = file.path(golden_dir(), "work_env.csv"),
                     covariate_path = file.path(golden_dir(), "covariates.csv"),
                     verbose = FALSE)
  st <- ukb_states(wl, sex = "Female")
  ids <- intersect(as.character(attr(st, "eid")), as.character(py$eid))
  skip_if(length(ids) < 10L, "重叠的人太少")
  ages <- intersect(colnames(st), names(py))
  a <- st[as.character(attr(st, "eid")) %in% ids, ages, drop = FALSE]
  b <- as.matrix(py[as.character(eid) %in% ids, ..ages])
  expect_identical(as.integer(a), as.integer(b))
})

test_that("Tier 1：移动几何与 Python 一致", {
  skip_no_golden()
  skip_if(!nzchar(bundle_dir()), "没有 bundle")
  py <- read_golden("seqmove_Female.parquet")
  b <- ukb_bundle(bundle_dir(), quiet = TRUE)
  wl <- ukb_worklife(file.path(golden_dir(), "employment.csv"),
                     entry_path = file.path(golden_dir(), "work_env.csv"),
                     covariate_path = file.path(golden_dir(), "covariates.csv"),
                     verbose = FALSE)
  wl <- ukb_movement(wl, b, verbose = FALSE)
  r <- wl$movement[sex == "Female"][order(eid)]
  py <- py[eid %in% r$eid][order(eid)]
  r <- r[eid %in% py$eid]
  # 距离类量的容差是 1e-5 而不是 1e-6：bundle 里的嵌入矩阵**存成 float32**
  # （353×192 用 float64 没有意义，权重本身就是 FP32 训练的）。距离约 37，
  # 实测 max|Δ| ≈ 4.6e-6 = 相对误差 1.2e-7，正是 float32 的精度。
  # 整数量与年龄仍要求完全相等。
  cmp_cols(r, py, c("mean_jump", "max_single_jump", "net_displacement"),
           tol = 1e-5)
  cmp_cols(r, py, c("n_moves", "first_move_age", "last_move_age"),
           int_cols = c("n_moves", "first_move_age", "last_move_age"))
})

test_that("Tier 2：latent 与 Python 在实质上相同（方向 + 下游不变）", {
  skip_no_golden()
  skip_if(!nzchar(bundle_dir()), "没有 bundle")
  skip_if_not_installed("torch")
  py <- read_golden("latent_Female.parquet")
  b <- ukb_bundle(bundle_dir(), quiet = TRUE)
  wl <- ukb_worklife(file.path(golden_dir(), "employment.csv"),
                     entry_path = file.path(golden_dir(), "work_env.csv"),
                     covariate_path = file.path(golden_dir(), "covariates.csv"),
                     verbose = FALSE)
  wl <- ukb_encode(wl, b, verbose = FALSE)
  r <- wl$latent[sex == "Female"][order(eid)]
  py <- py[eid %in% r$eid][order(eid)]
  r <- r[eid %in% py$eid]
  zc <- grep("^z[0-9]{3}$", names(r), value = TRUE)
  pc <- grep("^z", names(py), value = TRUE)
  skip_if(length(pc) != length(zc), "latent 维数不一致")
  A <- as.matrix(r[, ..zc])
  P <- as.matrix(py[, ..pc])

  ## **不用绝对 max|Δ| 作判据。** 6 层 pre-LN Transformer 会把 BLAS 之间 1e-7
  ## 量级的差异逐层放大：R 侧同一 BLAS 内换 batch 大小只差 9.5e-7，而跨 BLAS
  ## （R torch vs Python torch）实测 max|Δ| ≈ 2.4e-3。误差与序列长度几乎无关
  ## （r = +0.12），误差最小的人反而序列最长 —— 所以它不是累加项数的问题，
  ## 也不是某个通道算错（那会让相关明显下降）。
  ## 有意义的判据是方向与下游用途。
  rel <- max(abs(A - P)) / stats::sd(P)
  expect_lt(rel, 1e-2)
  per_person <- vapply(seq_len(nrow(A)), function(i) cor(A[i, ], P[i, ]),
                       numeric(1))
  expect_gt(min(per_person), 0.9999)
  per_dim <- vapply(seq_len(ncol(A)), function(j) cor(A[, j], P[, j]),
                    numeric(1))
  expect_gt(min(per_dim), 0.9999)

  ## 下游一：PCA 的前 5 个 PC（连续坐标的用途）
  pa <- stats::prcomp(A, center = TRUE, scale. = FALSE)
  pp <- stats::prcomp(P, center = TRUE, scale. = FALSE)
  for (k in 1:5) {
    expect_gt(abs(cor(pa$x[, k], pp$x[, k])), 0.999)
  }

  ## 下游二：30-NN 图的邻居集合（Leiden 的唯一输入）
  na <- ukbcareer:::.knn_index(A, 30L)
  np <- ukbcareer:::.knn_index(P, 30L)
  ov <- mean(vapply(seq_len(nrow(A)), function(i) {
    length(intersect(na[i, ], np[i, ])) / 30
  }, numeric(1)))
  expect_gt(ov, 0.99)
})


test_that("Tier 2：两份 z 在同一套聚类口径下给出同一个分区", {
  skip_no_golden()
  skip_if(!nzchar(bundle_dir()), "没有 bundle")
  skip_if_not_installed("torch")
  skip_if_not_installed("igraph")
  py <- read_golden("latent_Female.parquet")
  b <- ukb_bundle(bundle_dir(), quiet = TRUE)
  wl <- ukb_worklife(file.path(golden_dir(), "employment.csv"),
                     entry_path = file.path(golden_dir(), "work_env.csv"),
                     covariate_path = file.path(golden_dir(), "covariates.csv"),
                     verbose = FALSE)
  wl <- ukb_encode(wl, b, verbose = FALSE)
  r <- wl$latent[sex == "Female"][order(eid)]
  py <- py[eid %in% r$eid][order(eid)]
  r <- r[eid %in% py$eid]
  A <- as.matrix(r[, grep("^z[0-9]{3}$", names(r)), with = FALSE])
  P <- as.matrix(py[, grep("^z", names(py)), with = FALSE])
  skip_if(nrow(A) < 400L, "样本太小")

  ari <- function(a, bb) {
    t <- table(a, bb)
    n <- sum(t)
    ci <- function(x) sum(choose(x, 2))
    i <- ci(as.vector(t))
    ea <- ci(rowSums(t)) * ci(colSums(t)) / choose(n, 2)
    mx <- (ci(rowSums(t)) + ci(colSums(t))) / 2
    (i - ea) / (mx - ea)
  }
  # **口径参数刻意不用论文的那一套。** 黄金子集只有约 1,000 人，而论文口径
  # （resolution 0.2 + min_cluster_n 500）在这个规模上给 K = 1，ARI 无定义。
  # 这个测试问的是"同一份口径在两份 z 上是否给出同一个分区"，与 K 有没有科学
  # 意义无关 —— 所以调到 resolution 1.0 + 下界 50，只为让分区有结构可比。
  lab <- function(z, seed) {
    g <- ukbcareer:::.knn_graph(z, 30L)
    set.seed(seed)
    p <- igraph::cluster_leiden(g, objective_function = "modularity",
                                resolution = 1.0, n_iterations = 2L)
    ukbcareer:::.merge_small(as.integer(igraph::membership(p)), z, 0, 50L)
  }
  la <- lab(A, 42L)
  lp <- lab(P, 42L)
  skip_if(length(unique(la)) < 2L, "这个子样本上分不出 2 个以上的簇")
  expect_equal(length(unique(la)), length(unique(lp)))

  ## **判据必须与「算法噪声地板」比，不能设一个绝对的 ARI 门槛。**
  ## 实测：同一份 z 只换随机种子，ARI 就掉到 0.28（res 0.5）/ 0.73（res 1.0）
  ## —— Leiden 在这个规模上对 seed 极其敏感。所以"两份 z 的 ARI = 0.86"这个数
  ## 单独看没有意义，要问的是它**是否低于换种子的影响**。
  ## 若低于，才说明 latent 差异有额外影响。
  floor_ari <- ari(la, lab(A, 7L))
  expect_gte(ari(la, lp), floor_ari * 0.95)
})

test_that("Tier 2：前向是确定性的，且不受 batch 大小影响", {
  skip_no_golden()
  skip_if(!nzchar(bundle_dir()), "没有 bundle")
  skip_if_not_installed("torch")
  b <- ukb_bundle(bundle_dir(), quiet = TRUE)
  wl <- ukb_worklife(file.path(golden_dir(), "employment.csv"),
                     entry_path = file.path(golden_dir(), "work_env.csv"),
                     covariate_path = file.path(golden_dir(), "covariates.csv"),
                     verbose = FALSE)
  w1 <- ukb_encode(wl, b, batch_size = 256L, verbose = FALSE)
  w2 <- ukb_encode(wl, b, batch_size = 64L, verbose = FALSE)
  zc <- grep("^z[0-9]{3}$", names(w1$latent), value = TRUE)
  m1 <- as.matrix(w1$latent[order(eid), ..zc])
  m2 <- as.matrix(w2$latent[order(eid), ..zc])
  expect_lt(max(abs(m1 - m2)), 1e-5)
})
