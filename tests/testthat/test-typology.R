# 类型学的内部函数 —— 用有已知答案的构造验证，不依赖 bundle

test_that("NMI 的四个已知答案", {
  set.seed(1)
  a <- sample(0:3, 3000L, TRUE)
  # 相同分区 → 1
  expect_equal(ukbcareer:::.nmi(a, a), 1, tolerance = 1e-9)
  # 独立分区 → ~0（小样本有正偏）
  b <- sample(0:9, 3000L, TRUE)
  expect_lt(ukbcareer:::.nmi(a, b), 0.05)
  # 确定性粗化 → 严格介于 0 与 1 之间
  v <- ukbcareer:::.nmi(a, a %/% 2L)
  expect_gt(v, 0.4)
  expect_lt(v, 1)
  # **绝不为负** —— 符号写反时它恒为负，于是"分型只是生涯长度的重新表述"
  # 这条判据永远不触发（一个静默失效的门槛）
  vals <- replicate(100L, ukbcareer:::.nmi(sample(0:4, 400L, TRUE),
                                           sample(0:9, 400L, TRUE)))
  expect_true(all(vals >= 0))
})

test_that("merge_small 把小簇并入最近的达标簇", {
  set.seed(2)
  # 三团：600 / 600 / 20。下界 100 → 小团应被并入**质心更近**的那个
  z <- rbind(matrix(rnorm(600 * 3, 0), ncol = 3),
             matrix(rnorm(600 * 3, 10), ncol = 3),
             matrix(rnorm(20 * 3, 0.5), ncol = 3))
  lab <- c(rep(0L, 600), rep(1L, 600), rep(2L, 20))
  out <- ukbcareer:::.merge_small(lab, z, min_frac = 0, min_n = 100L)
  expect_equal(length(unique(out)), 2L)
  # 小团（均值 0.5）离第 0 团（均值 0）更近
  expect_equal(length(unique(out[1201:1220])), 1L)
  expect_equal(unique(out[1201:1220]), unique(out[1:600]))
})

test_that("merge_small 在没有任何簇达标时是空操作（不是并成两个）", {
  set.seed(3)
  z <- matrix(rnorm(300 * 3), ncol = 3)
  lab <- rep(0:2, each = 100L)
  out <- ukbcareer:::.merge_small(lab, z, min_frac = 0, min_n = 500L)
  # 这是本函数一个容易误解的行为：每个簇都 < 下界时 big 为空、立刻退出，
  # 一个都不合并。ukb_typology() 因此必须在**跑完之后**检查最小簇并警告。
  expect_equal(length(unique(out)), 3L)
  expect_equal(attr(out, "floor_n"), 500)
})

test_that("merge_small 重编号为 0..K-1 且连续", {
  set.seed(4)
  z <- matrix(rnorm(400 * 2), ncol = 2)
  lab <- rep(c(5L, 9L, 13L, 20L), each = 100L)
  out <- ukbcareer:::.merge_small(lab, z, min_frac = 0, min_n = 1L)
  expect_equal(sort(unique(out)), 0:3)
})

test_that("kNN 索引不含自己，且与暴力解一致", {
  set.seed(5)
  z <- matrix(rnorm(200 * 4), ncol = 4)
  nn <- ukbcareer:::.knn_index(z, k = 5L)
  expect_equal(dim(nn), c(200L, 5L))
  expect_false(any(nn == seq_len(200L)))   # 不含自己
  # 与手算的第 1 行比对
  d <- sqrt(colSums((t(z) - z[1L, ])^2))
  d[1L] <- Inf
  expect_setequal(nn[1L, ], order(d)[1:5])
})

test_that("safe_decile 在取值集中时不报错", {
  expect_equal(length(unique(ukbcareer:::.safe_decile(rep(5, 100L)))), 1L)
  expect_gt(length(unique(ukbcareer:::.safe_decile(rnorm(1000L)))), 5L)
})

test_that("聚类口径的默认值与论文一致", {
  expect_equal(CLUSTER_DEFAULTS$resolution, 0.2)
  expect_equal(CLUSTER_DEFAULTS$knn_k, 30L)
  expect_equal(CLUSTER_DEFAULTS$min_cluster_n, 500L)
  expect_equal(CLUSTER_DEFAULTS$min_cluster_frac, 0.005)
})

test_that("ARI 的已知答案，且 K=1 时不返回 1.0", {
  a <- rep(0:3, each = 50L)
  expect_equal(ukbcareer:::.ari(a, a), 1, tolerance = 1e-9)
  # 标签重编号不该改变 ARI
  expect_equal(ukbcareer:::.ari(a, (a + 2L) %% 4L), 1, tolerance = 1e-9)
  set.seed(9)
  rnd <- ukbcareer:::.ari(a, sample(0:3, 200L, TRUE))
  expect_lt(abs(rnd), 0.1)
  # **K=1 时 ARI 无定义** —— 上游 ari_null 在这里返回 1.0 是个已知缺陷，
  # 它让"零基线 ARI = 1.0"这句假话进了图。这里必须返回 NA。
  expect_true(is.na(ukbcareer:::.ari(rep(0L, 100L), rep(0L, 100L))))
})
