# ukbcareer

Lifetime work-history attributes for UK Biobank, on the definitions of a
published pipeline, so that estimates are comparable across studies.

**This package produces exposures, not results.** No health outcomes are
involved anywhere in it.

## What you get

The package is built in tiers. Each one works on its own; when an upper tier is
unavailable, everything below it is unaffected.

| Tier | Needs | Gives |
| --- | --- | --- |
| **0** | nothing but your CSVs | event tables, annual panel, nine-state sequences, exposure summaries, CAMSIS status, and all model-free plots |
| **1** | a 0.5 MB code-embedding matrix from the bundle | occupational mobility geometry |
| **2** | the frozen encoder from the bundle + R `torch` | the 192-d representation and a career typology |

```r
library(ukbcareer)

# Tier 0 only -- no bundle, no model weights
wl <- ukb_worklife("ukb_cat130.csv",
                   entry_path     = "ukb_cat123.csv",   # field 22500
                   covariate_path = "ukb_baseline.csv") # fields 31, 34
wl  <- ukb_camsis(wl, "camsis_soc2000.csv")             # you supply this
out <- ukb_career(wl, verbose = FALSE)

ukb_qc(out)                 # compare your definitions against ours
plot_states(wl)             # nine-state sequences
plot_flows(wl, "Female")    # occupational flows on a CAMSIS axis

# Tier 1 + 2
b   <- ukb_bundle("~/ukbcareer_bundle_v5.0")
out <- ukb_career(wl, bundle = b,
                 what = c("core", "exposure", "camsis", "movement",
                          "latent", "type"))
```

## The data you need

From your own UK Biobank application. Category 130 in full, plus four fields
that are **not** in it:

| Field | Where | Why it cannot be skipped |
| --- | --- | --- |
| **22500** When occupational data entered | **Category 123** | `career_end` takes the per-person questionnaire year. Substituting a constant adds ~2 years to every ongoing record (5.7% of all person-years) and drops the observed-retirement rate from 56.8% to 27.7%. Then it is no longer our definition. |
| **31** Sex | baseline | Representations are trained separately per sex, and the CAMSIS scale is itself sex-specific. |
| **34** Year of birth | baseline | The anchor of the age axis. If your Category 130 export also carries a birth year, the package prefers the baseline one and reports any disagreement — we measured 14 people in 1,998 whose two values differ by up to 5 years. |
| **22617** Job code - historical | Category 130 | The 4-digit SOC2000 code (353 values) that the vocabulary is built on. Not 22601, which is the 8-digit OSCAR code. |

Three structural traps, all measured on the full cohort:

- Short arrays are **sparse arrays that preserve the job index** — slot *k* is
  job *k*. Agreement with that reading is 1.00000; with the "compressed
  subsequence" reading, 0.84285. Do not re-map.
- **Negative exposure codes are ordinal levels, not missing values**:
  `0 < -131 < -141`, and `-121` is "do not know". Cleaning on "negative means
  missing" destroys every exposure variable. `ukb_qc()` checks for this.
- `22604` (hours band) and `22605` (exact hours) are **either/or**, not a
  subset relation. Both are needed.

CAMSIS is not distributed with the package; supply it via `ukb_camsis(path)`.

## Three things to read before using the typology

1. **Your types are not the paper's types.** Leiden is transductive, so the
   package re-runs the published recipe on *your* sample rather than assigning
   you to our 12 (female) / 9 (male) clusters. What is comparable across
   studies is the method and the representation, not the partition. Do not
   match your "T3" against the paper's "T3".
2. **Check the seed sensitivity.** `ukb_typology()` re-runs with several seeds
   and reports the median pairwise ARI. On a ~1,000-person sample we measured
   0.28–0.73 — the data did not change and the partition did. On our own cohort
   the bootstrap ARI is 0.515 (female) and 0.618 (male); the female value is
   below the pre-registered reference line of 0.60. `ukb_bundle_info()` prints
   all of this.
3. **Mobility geometry is smaller than it looks.** Pairwise distances between
   the 353 occupation codes are highly concentrated (CV 0.10; 95% within
   [30, 45]). Consequently `path_length ≈ 37k`, `detour_ratio ≈ k` and
   `straightness ≈ 1/k` are all the number of changes in disguise, so the
   package does not return them. What it returns — `mean_jump`,
   `net_displacement`, `max_single_jump`, `first_move_*`, `last_move_age` —
   has R² against *k* of at most 0.15. See `plot_distance_concentration()`.

Per-person perplexity is also not returned. The encoder is bidirectional, so
per-token loss measures interpolation, not forecasting: against a rule that
merely copies the two adjacent years, the model gains 0.034 nats — 0.7% of its
total gain over the marginal. Per-person values have an IQR of 0.0008 and half
of everyone sits on the floor at 1.0. See `plot_baselines()`.

## Installation

```r
# install.packages("remotes")
remotes::install_github("jiezhang-cn/ukbcareer")
```

Tier 0 needs only `data.table` and `jsonlite`. Tier 1 adds `arrow`; Tier 2 adds
`torch` and `igraph`; plots need `ggplot2` (plus `ggalluvial` for the sankey and
`patchwork` for composites). `RANN` or `FNN` will speed up the k-NN graph
considerably on large samples.

The bundle is distributed under controlled access, not bundled with the
package: it is a derivative of UK Biobank data. It contains no individual-level
data — only code-level embeddings, frozen weights and distributional summaries,
with cells below n = 10 suppressed. **Keep it on an ASCII-only path**: libtorch
resolves paths through the system ANSI encoding, and non-ASCII characters make
`jit_load()` fail.

See [BUNDLE-ACCESS.md](BUNDLE-ACCESS.md) for what is in the bundle, what is
deliberately left out, and how to request it. You need a UK Biobank application
of your own; the bundle is a derivative work, not a substitute for access.

## Citing

The package accompanies:

> Zhang J, et al. Lifetime occupational histories associated divergent pathways
> to biological ageing and health outcomes. (in preparation)

`citation("ukbcareer")` gives the current entry.

## Provenance

Definitions follow `worklife_typology_plan` v5.0. The R implementation is
verified against the Python pipeline on a held-back golden dataset: the annual
panel matches row-for-row and column-for-column (88,625 rows), `career_end` and
its source label match for 100% of people, and the 192-d representation agrees
to a per-person correlation above 0.9999 with identical 30-NN neighbourhoods.
See `tests/testthat/test-golden.R`.
