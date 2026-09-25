# ══════════════════════════════════════════════════════════════════
# 一站式封装
# ══════════════════════════════════════════════════════════════════

#' All derived work-history variables in one table
#'
#' The main entry point. Reads your UK Biobank export (or takes the result of
#' [ukb_worklife()]), derives the person-level variables, and returns **one
#' row per person**, ready to merge with outcomes or covariates by `eid`.
#'
#' The variable groups, chosen with `what`:
#'
#' | `what` | What you get | Needs |
#' |---|---|---|
#' | `"core"` | career end and how it was determined, working years, record coverage, quality flags | your CSVs |
#' | `"exposure"` | years / peak / mean of 10 workplace exposures, shift work, long hours, exposure breadth | your CSVs |
#' | `"camsis"` | occupational status (CAMSIS) level and trajectory | a CAMSIS table (`camsis_path`) |
#' | `"movement"` | occupational mobility: number and size of moves between occupations | the model bundle |
#' | `"latent"` | the 192-dimensional career representation | the model bundle + torch |
#' | `"type"` | career type (clusters re-derived on your sample) | the model bundle + torch + igraph |
#'
#' Every column is described in [ukb_dictionary()], with advice on how to use
#' it. With the default `what`, `movement` is simply skipped when no bundle is
#' given, so `ukb_career(path)` works out of the box on your CSVs alone.
#'
#' @param path Path to your Category 130 CSV, a data frame already read in, or
#'   the result of [ukb_worklife()] (then the data are not processed again).
#' @param bundle The model bundle from [ukb_bundle()]. Only needed for
#'   `movement`, `latent` and `type`.
#' @param entry_path CSV with field 22500 (Category 123). See [ukb_worklife()].
#' @param covariate_path CSV with sex (field 31) and year of birth (field 34),
#'   if they are not in the main file. See [ukb_worklife()].
#' @param camsis_path A CAMSIS table (not shipped with the package; see
#'   [ukb_camsis()]).
#' @param what Which variable groups to compute; see the table above.
#' @param include_latent Add the 192 `z*` columns to the returned table.
#'   Default `FALSE` -- they make the table unwieldy; they are always
#'   available as `attr(out, "worklife")$latent`.
#' @param verbose Print progress and the key numbers of each step.
#' @return A `data.table` with one row per person. `attr(out, "worklife")`
#'   holds the full intermediate results (annual panel, job and gap tables),
#'   which [plot_person()] and the other plots use.
#' @examples
#' out <- ukb_career(ukb_synth(n = 60), verbose = FALSE)
#' out[1:3, c("eid", "career_end", "career_end_source", "work_years",
#'            "exposure_breadth")]
#' @export
ukb_career <- function(path, bundle = NULL, entry_path = NULL,
                       covariate_path = NULL, camsis_path = NULL,
                       what = c("core", "exposure", "movement"),
                       include_latent = FALSE, verbose = TRUE) {
  what_given <- !missing(what)
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
    if (verbose) message("-- Reading and cleaning work histories --")
    ukb_worklife(path, entry_path = entry_path,
                 covariate_path = covariate_path, verbose = verbose)
  }

  if ("camsis" %in% what) {
    if (is.null(camsis_path) && is.null(wl$camsis)) {
      warning("`camsis` was requested but no `camsis_path` given (the CAMSIS ",
              "table is not shipped with the package) -> skipped", call. = FALSE)
      what <- setdiff(what, "camsis")
    } else if (is.null(wl$camsis)) {
      if (verbose) message("-- Occupational status (CAMSIS) --")
      wl <- ukb_camsis(wl, camsis_path, verbose = verbose)
    }
  }
  need_bundle <- intersect(what, c("movement", "latent", "type"))
  if (length(need_bundle) && is.null(bundle)) {
    ## 默认 what 含 movement：没给 bundle 时跳过它是预期行为，不该发 warning 吓人
    if (!identical(need_bundle, "movement") || what_given) {
      warning(paste(need_bundle, collapse = "/"),
              " needs the model bundle (`bundle = ukb_bundle(...)`) -> skipped; ",
              "everything else is unaffected", call. = FALSE)
    } else if (verbose) {
      message("(mobility measures skipped: they need the model bundle)")
    }
    what <- setdiff(what, need_bundle)
  }
  if ("movement" %in% what) {
    if (verbose) message("-- Occupational mobility --")
    wl <- ukb_movement(wl, bundle, verbose = verbose)
  }
  if ("latent" %in% what) {
    if (verbose) message("-- Career representation (encoder) --")
    wl <- ukb_encode(wl, bundle, verbose = verbose)
  }
  if ("type" %in% what) {
    if (verbose) message("-- Career types (Leiden re-run on your sample) --")
    wl <- ukb_typology(wl, bundle, verbose = verbose)
  }

  ## ── 组装宽表 ──────────────────────────────────────────────
  out <- data.table::copy(wl$cohort)
  if ("exposure" %in% what && !is.null(wl$exposure)) {
    out <- merge(out, wl$exposure, by = "eid", all.x = TRUE)
    out <- .derive_exposure_cols(out)
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


#' 暴露汇总的五个派生列（ukb_career 与 plot_person 共用，口径只在这里定义）
#'
#' exposure_breadth：接触过几种有害因素。**这是第七个量的定稿选择**
#' （用户 2026-09-21 拍定，取代 intens_mse）—— 理由有三：
#'   ① 它是 config `primary_axes` 里预注册的画像轴；
#'   ② intens_mse 是七个量里唯一**必须有模型才能算**的，别人复现不了；
#'   ③ 选它之后第七格只靠 CSV 就能算，不依赖任何权重分发许可。
#' @noRd
.derive_exposure_cols <- function(out) {
  out <- data.table::as.data.table(out)
  yc <- intersect(paste0("yrs_exp_", EXPOSURES), names(out))
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
  out
}


#' @export
print.ukbcareer_result <- function(x, ...) {
  m <- attr(x, "ukbcareer")
  ## res[, .N, by = ...] 之类的派生表会继承本类却丢掉 "ukbcareer" 属性，
  ## 它们不是"一人一行"的结果：按普通 data.table 打印（行子集保留属性，照常打印）
  if (is.null(m$what) || !"eid" %in% names(x)) {
    cls <- class(x)
    data.table::setattr(x, "class", c("data.table", "data.frame"))
    on.exit(data.table::setattr(x, "class", cls))
    print(x, ...)
    return(invisible(x))
  }
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
  ## 按词典分组计列数 —— 64+ 列全部打印出来对新用户是一堵墙
  dd <- .expand_agents(.dictionary_rows())
  grp <- dd$group[match(names(x), dd$variable)]
  grp[grepl("^z[0-9]{3}$", names(x))] <- "Career representation"
  grp[is.na(grp)] <- "Other"
  tab <- table(factor(grp, levels = unique(c(dd$group, "Other"))))
  tab <- tab[tab > 0L]
  cat("  ", paste(strwrap(paste(sprintf("%s (%d)", names(tab), tab),
                               collapse = ", "),
                         getOption("width", 80L) - 12L,
                         prefix = "           ", initial = "columns: "),
                 collapse = "\n"), "\n", sep = "")
  cat("  What each column means: ukb_dictionary()\n\n")
  keep <- intersect(c("eid", "sex", "career_end", "career_end_source",
                      "work_years", "coverage", "exposure_breadth",
                      "shift_frac", "camsis_mean", "n_moves", "type"),
                    names(x))
  n <- min(5L, nrow(x))
  h <- data.table::setDT(lapply(stats::setNames(keep, keep),
                                function(k) x[[k]][seq_len(n)]))
  print(h)
  if (nrow(x) > n || ncol(x) > length(keep)) {
    cat(sprintf("# %d of %s rows, %d of %d columns shown; View(x) shows all\n",
                n, fmt_n(nrow(x)), length(keep), ncol(x)))
  }
  invisible(x)
}
