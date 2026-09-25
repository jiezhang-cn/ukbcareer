# Getting the `ukbcareer` bundle

The bundle holds the pretrained model -- the occupation embedding and the
career encoder trained on the reference UK Biobank cohort. It is **not
shipped with the package and is not downloaded automatically**, because it is a
derivative of UK Biobank data and its distribution is governed by the UK Biobank
Material Transfer Agreement.

**Most users do not need it.** Career timing, record quality, workplace
exposures, working hours and shifts, CAMSIS status, the nine-state sequences
and the one-page person report (`plot_person()`) all come from your own CSV
files. The bundle is needed only for occupational mobility
(`what = "movement"`), the 192-d career representation and career types.

## What is in it

`bundle_v5.0`, 27 MB, 34 files.

| File | Needed for | Contents |
| --- | --- | --- |
| `vocab.json` | everything below | the frozen vocabulary: 353 SOC2000 codes, 10 gap codes, special tokens |
| `reference_qc.json` | `ukb_qc()` | reference-cohort distributions that `ukb_qc()` compares against |
| `reliability.json` | `ukb_bundle_info()` | S1 / ASW / largest cluster / SOC purity, and the predictability baseline ladder |
| `tables/` | reference comparisons | reference type profiles (P1-P7, T1-T4) and the BC audit |
| `type_names_{sex}.json` | reference comparisons | frozen names of the **reference** types, for orientation |
| `soc_embed_{sex}.parquet` | occupational mobility | 353 x 192 occupation-code embedding plus 2-D map coordinates |
| `encoder_{sex}.ts` | career representation, career types | the frozen encoder as TorchScript (about 3M parameters) |
| `MANIFEST.json` | — | sha256 of every file, plus the definitions the bundle was built under |

## What is *not* in it

No individual-level data of any kind. Specifically absent:

- the 117,590 x 192 person-level representations,
- per-person token tensors,
- the `eid` of cluster medoids,
- the golden-test inputs used to verify the R implementation.

Every aggregate is suppressed below n = 10, and distributions are reported at
p5/p95 rather than at the extreme percentiles.

Two residual disclosure risks are worth stating plainly:

1. **Membership inference.** Per-token loss is the training loss, so low loss
   correlates with being inside the training set. Your sample and our cohort
   come from the same UK Biobank and may overlap substantially. This is one
   reason the package does not return per-person perplexity.
2. **Model memorisation.** The encoder weights are an aggregate, but no
   published method can guarantee that a 3M-parameter model memorises nothing
   about rare sequences. The 353-code vocabulary is public (ONS SOC2000, OGL
   v3), which bounds what the embedding layer alone can reveal.

## Eligibility

You need a current UK Biobank application of your own. The bundle is a
derivative work, not a substitute for access: it is useless without your own
Category 130 extract, and we cannot grant you rights to UK Biobank data.

## How to request it

Email **changchieh1998@163.com** with the subject line
`ukbcareer bundle request`, including:

1. your UK Biobank application number and the name of the principal
   investigator;
2. a one-paragraph description of what you intend to derive;
3. confirmation that you will not redistribute the bundle, and that any derived
   variables will be returned to UK Biobank as your application requires.

You will get back a download URL and the expected `MANIFEST.json` sha256. Please
allow a few working days.

Requests are logged (application number, date, bundle version) so that a
correction can be circulated if a defect is found in a released bundle.

## After you download it

```r
library(ukbcareer)

# Verification is on by default. Do not turn it off: it is where the
# "frozen representation" discipline lands on the R side.
b <- ukb_bundle("~/ukbcareer_bundle_v5.0")

ukb_bundle_info(b)   # read this before putting `type` into a model
```

`ukb_bundle()` checks every file against `MANIFEST.json` and **stops** on any
mismatch. A modified or partially downloaded bundle is refused, because the
frozen representation is the only thing that makes estimates comparable across
studies.

**Keep the bundle on an ASCII-only path.** libtorch resolves paths through the
system ANSI encoding, so non-ASCII characters (a non-Latin user name, for
instance) make `jit_load()` fail with "Parent directory does not exist". The
package works around this by copying the encoder to an ASCII cache directory and
telling you it did; you can choose the location with
`options(ukbcareer.ascii_cache = "C:/somewhere_ascii")`.

## Versioning

The bundle version tracks the analysis pipeline, not the package version.
`bundle_v5.0` corresponds to `worklife_typology_plan` v5.0. `ukb_bundle()`
compares the bundle's definitions against the package's defaults and warns on
any mismatch: a warning there means the package and bundle versions do not agree
and your results may not be comparable with the published ones.

Older bundles are kept available. Results produced with one bundle version
should not be pooled with results from another without checking
`MANIFEST.json`'s `model_sha256`.
