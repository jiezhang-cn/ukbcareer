# ══════════════════════════════════════════════════════════════════
# 一站式封装
# ══════════════════════════════════════════════════════════════════

#' 一次拿到全部工作史派生变量
#'
#' 把 Tier 0 → Tier 1 →（可选）Tier 2 串起来，返回**一人一行**的宽表。
#'
#' **默认只跑 Tier 0 + Tier 1**（用户 2026-09-21 拍定）：快、不需要模型权重、
#' 没有分配误差。Tier 2（表征与类型学）用 `what = c("latent", "type")` 显式打开。
#'
#' @param path Category 130 的 csv 路径，或已读入的表，或一个已有的
#'   `ukbcareer_worklife` 对象（那就跳过 ETL）。
#' @param bundle [ukb_bundle] 的返回值。`NULL` 时只能跑最基础的 Tier 0。
#' @param entry_path Category 123 的 csv（含 field 22500）。见 [ukb_worklife]。
#' @param camsis_path CAMSIS 表的路径。**不随包分发**，需自备。
#' @param what 要哪几组。`"core"` = 口径与工作史计数，`"exposure"` = 暴露汇总，
#'   `"camsis"` = 职业地位，`"movement"` = 移动几何（Tier 1），
#'   `"latent"` = 192 维表征（Tier 2），`"type"` = 类型学（Tier 2，含 latent）。
#' @param include_latent 是否把 192 列 `z*` 放进返回的宽表。默认 `FALSE`
#'   （它们会让表宽到不好用；需要时从 `attr(out, "worklife")$latent` 取）。
#' @param verbose 打印每一步的关键口径数字。
#' @return 一个 `data.table`，一人一行。`attr(, "worklife")` 是完整的中间产物，
#'   `attr(, "ukbcareer")` 是口径元数据。
#' @examples
#' out <- ukb_career(ukb_synth(n = 60), verbose = FALSE)
#' names(out)
#' @export
ukb_career <- function(path, bundle = NULL, entry_path = NULL,
                       camsis_path = NULL,
                       what = c("core", "exposure", "movement"),
                       include_latent = FALSE, verbose = TRUE) {
  all_what <- c("core", "exposure", "camsis", "movement", "latent", "type")
  bad <- setdiff(what, all_what)
  if (length(bad)) {
    stop("Unknown `what`: ", paste(bad, collapse = ", "),
         ". Choose from ", paste(all_what, collapse = " / "))
  }
  if ("type" %in% what) what <- union(what, "latent")
  if (!is.null(camsis_path)) what <- union(what, "camsis")

  wl <- if (inherits(path, "ukbcareer_worklife")) {
    path
  } else {
    if (verbose) message("-- Tier 0: ETL --")
    ukb_worklife(path, entry_path = entry_path, verbose = verbose)
  }

  if ("camsis" %in% what) {
    if (is.null(camsis_path) && is.null(wl$camsis)) {
      warning("`camsis` was requested but no `camsis_path` given (the CAMSIS ",
              "table is not shipped with the package) -> skipped", call. = FALSE)
      what <- setdiff(what, "camsis")
    } else if (is.null(wl$camsis)) {
      if (verbose) message("-- Tier 0: CAMSIS --")
      wl <- ukb_camsis(wl, camsis_path, verbose = verbose)
    }
  }
  need_bundle <- intersect(what, c("movement", "latent", "type"))
  if (length(need_bundle) && is.null(bundle)) {
    warning(paste(need_bundle, collapse = "/"),
            " was requested but no `bundle` given -> skipped (Tier 0 unaffected)",
            call. = FALSE)
    what <- setdiff(what, need_bundle)
  }
  if ("movement" %in% what) {
    if (verbose) message("-- Tier 1: mobility geometry --")
    wl <- ukb_movement(wl, bundle, verbose = verbose)
  }
  if ("latent" %in% what) {
    if (verbose) message("-- Tier 2: representation --")
    wl <- ukb_encode(wl, bundle, verbose = verbose)
  }
  if ("type" %in% what) {
    if (verbose) message("-- Tier 2: typology (Leiden re-run on your sample) --")
    wl <- ukb_typology(wl, bundle, verbose = verbose)
  }

  ## ── 组装宽表 ──────────────────────────────────────────────
  out <- data.table::copy(wl$cohort)
  if ("exposure" %in% what && !is.null(wl$exposure)) {
    out <- merge(out, wl$exposure, by = "eid", all.x = TRUE)
    # exposure_breadth：接触过几种有害因素。**这是第七个量的定稿选择**
    # （用户 2026-09-21 拍定，取代 intens_mse）—— 理由有三：
    #   ① 它是 config `primary_axes` 里预注册的画像轴；
    #   ② intens_mse 是七个量里唯一**必须有模型才能算**的，别人复现不了；
    #   ③ 选它之后整个第七格从 Tier 2 降到 Tier 0，不依赖任何权重分发许可。
    yc <- paste0("yrs_exp_", EXPOSURES)
    yc <- intersect(yc, names(out))
    if (length(yc)) {
      out[, exposure_breadth := rowSums(.SD > 0, na.rm = TRUE), .SDcols = yc]
    }
    hc <- intersect(paste0("yrs_high_", EXPOSURES), names(out))
    if (length(hc)) {
      out[, high_exposure_years := rowSums(.SD, na.rm = TRUE), .SDcols = hc]
      out[, longest_agent_years := do.call(pmax, c(.SD, na.rm = TRUE)),
          .SDcols = hc]
    }
    if (all(c("shift_years", "work_years") %in% names(out))) {
      out[, shift_frac := data.table::fifelse(work_years > 0,
                                              shift_years / work_years, NA_real_)]
    }
    if (all(c("long_hours_years", "work_years") %in% names(out))) {
      out[, long_hours_frac := data.table::fifelse(
        work_years > 0, long_hours_years / work_years, NA_real_)]
    }
  }
  if ("camsis" %in% what && !is.null(wl$camsis)) {
    out <- merge(out, wl$camsis, by = "eid", all.x = TRUE)
  }
  if ("movement" %in% what && !is.null(wl$movement)) {
    mv <- data.table::copy(wl$movement)
    mv[, sex := NULL]
    out <- merge(out, mv, by = "eid", all.x = TRUE)
  }
  if ("type" %in% what && !is.null(wl$types)) {
    ty <- data.table::copy(wl$types)
    ty[, sex := NULL]
    out <- merge(out, ty, by = "eid", all.x = TRUE)
  }
  if (include_latent && !is.null(wl$latent)) {
    lt <- data.table::copy(wl$latent)
    lt[, sex := NULL]
    out <- merge(out, lt, by = "eid", all.x = TRUE)
  }

  data.table::setattr(out, "worklife", wl)
  data.table::setattr(out, "ukbcareer",
                      c(attr(wl, "ukbcareer"), list(what = what)))
  data.table::setattr(out, "class", c("ukbcareer_result", class(out)))
  if (verbose) {
    message(sprintf("-- done: %s people x %d columns (%s) --",
                    fmt_n(nrow(out)), ncol(out), paste(what, collapse = " + ")))
  }
  out[]
}


#' @export
print.ukbcareer_result <- function(x, ...) {
  m <- attr(x, "ukbcareer")
  cat("<ukbcareer_result>", fmt_n(nrow(x)), "people x", ncol(x), "columns\n")
  cat("  computed: ", paste(m$what, collapse = " + "), "\n", sep = "")
  if (!is.null(m$bundle_version)) {
    cat("  bundle: ", m$bundle_version, "  model_sha256: ",
        substr(unlist(m$model_sha256)[1L], 1L, 12L), "...\n", sep = "")
  }
  if ("type" %in% m$what) {
    cat("  Note: `type` comes from re-running Leiden on YOUR sample, not\n")
    cat("        the paper's T0-T11. Neither numbering nor meaning carries\n")
    cat("        over. See ?ukb_typology\n")
  }
  data.table::setattr(x, "class", c("data.table", "data.frame"))
  print(utils::head(x, 5L))
  data.table::setattr(x, "class", c("ukbcareer_result", "data.table", "data.frame"))
  invisible(x)
}
