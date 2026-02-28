# Derived datasets (anonymized, analysis-ready)

This folder contains analysis-ready country/region–year panel datasets used to reproduce the main models.

## Files (one per pathogen–drug combination)
- `3GCREC_merged_anonymized.csv` — 3GCR *E. coli*
- `3GCRKP_merged_anonymized.csv` — 3GCR *K. pneumoniae*
- `CREC_merged_anonymized.csv` — CR *E. coli*
- `CRKP_merged_anonymized.csv` — CR *K. pneumoniae*
- `CRAB_merged_anonymized.csv` — CR *A. baumannii*
- `CRPA_merged_anonymized.csv` — CR *P. aeruginosa*

## Key columns
Common fields include: `NAME` (anonymized country/region identifier), `year`, `TMP` (annual mean temperature), `R` (resistance proportion), and isolate counts (`Tested.isolates`, `isolates`) when available. Variable definitions are provided in `data/metadata/data_dictionary.csv`.

## Notes
- These are derived/anonymized datasets prepared for reproducibility; `NAME` does not expose country names.
- Upstream third-party source datasets are not re-hosted if redistribution is restricted; see `data/README.md` for retrieval/processing guidance.

## Reproduce
From repository root:
```bash
Rscript scripts/00_master_pipeline.R
