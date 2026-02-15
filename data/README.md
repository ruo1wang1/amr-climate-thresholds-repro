# Data

## What is shared in this repository
- `data/derived/`: analysis-ready country/region–year panel dataset(s) used for modelling (only if redistribution is permitted).
- `data/metadata/`: data dictionary and variable definitions.

## Third-party source data
Some upstream datasets are subject to third-party licensing and are not re-hosted here.
We therefore provide (i) exact source links and (ii) scripted retrieval/processing instructions to reproduce all analytic variables.

## Expected local layout (if you download restricted raw data)
- Place any restricted raw files under `data/raw/` (not tracked by git).
- Then run the processing scripts under `scripts/` to regenerate `data/derived/`.
