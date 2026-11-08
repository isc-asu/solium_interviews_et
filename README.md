# Pigs in Ethiopia: interviews, literature review and FAOSTAT

Run the analyses from the repository root. The notebooks use relative paths;
inputs are included under `data/` and figures are generated in `figures/`.

## Analyses

- `interview_code_march4.Rmd`: current farm and abattoir interview analysis.
  `interview_code_github.R` contains the same R code, extracted from this notebook.
- `pigs_literature_review_revised.Rmd`: reviewed publication dataset, locality map,
  and publication-level summaries. The publication input retains its historical
  `publications_92_...` filename but currently contains 94 records.
- `github_pig_data.Rmd`: FAOSTAT livestock and meat-production analysis;
  see [FAOSTAT_README.md](FAOSTAT_README.md) for source details.

## Run

Install the packages once:

```r
install.packages(c("dplyr", "tidyr", "stringr", "tibble", "purrr", "readr",
                   "ggplot2", "sf", "cowplot", "ggrepel", "patchwork",
                   "scales", "knitr", "rmarkdown"))
```

Open a notebook in RStudio and run its chunks from top to bottom, or render it
from an R session whose working directory is the repository root:

```r
rmarkdown::render("interview_code_march4.Rmd")
rmarkdown::render("pigs_literature_review_revised.Rmd")
rmarkdown::render("github_pig_data.Rmd")
```

Rendering requires Pandoc, included with RStudio. The `sf` package requires
GDAL, GEOS and PROJ when building from source. To run the interview analysis
without rendering HTML, use `source("interview_code_github.R")`.

## Data and outputs

The interview CSVs use participant/site identifiers and retain the original
raw answers. The notebook cleans separate copies and excludes `farm_19`,
which has no substantive survey answers: the analyzed sample is 23 farm
respondents at 15 sites and 12 abattoir respondents at four sites.
The root-level interview CSV copies are retained for existing links;
the current analysis reads the matching copies under `data/`.

The reviewed publication, farm-report, locality and source-list CSVs retain
their source/evidence columns. The 544-record source list documents the
search pool, not the final included sample. Approximate map locations are
locality/district representatives, not exact interview farm coordinates.

`data/boundaries/eth_admin1.*` includes the complete shapefile used by the
locality analysis. These are the existing OCHA administrative boundaries
used by the project and are not assumed to reflect current boundaries.
Keep all five shapefile components together.

Rebuilt summary CSVs are included under `data/results/`. Running the notebooks
regenerates these tables and figures from the included inputs.
