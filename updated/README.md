# Updated analyses — use this folder

This is the current, reproducible version of the project. Start here.
The original GitHub files are preserved separately in `../archive/`.

## Choose an analysis

| Notebook | Purpose | Inputs |
| --- | --- | --- |
| `interviews.Rmd` | Farm and abattoir interviews | `data/farmer_data_github.csv`, `data/abattoir_data_github.csv` |
| `literature_review.Rmd` | Publication summaries and locality map | Reviewed publication, farm-report, locality and boundary files in `data/` |
| `faostat.Rmd` | Livestock stocks and meat production | `data/FAOSTAT_data_en_11-7-2026.csv` |

Each analysis has one notebook. There is no duplicate R-script version.
The interview questionnaire is in `docs/interview_questions.pdf`.

## Run

Download or clone the repository. Open an R session with **this `updated/`
folder as its working directory**. In RStudio, open a notebook here and run
its chunks from top to bottom, or use Knit.

Install the required packages once:

```r
install.packages(c("dplyr", "tidyr", "stringr", "tibble", "purrr", "readr",
                   "ggplot2", "sf", "cowplot", "ggrepel", "patchwork",
                   "scales", "knitr", "rmarkdown"))
```

To render from an R session already in `updated/`:

```r
rmarkdown::render("interviews.Rmd")
rmarkdown::render("literature_review.Rmd")
rmarkdown::render("faostat.Rmd")
```

HTML rendering requires Pandoc, included with RStudio. The `sf` package needs
GDAL, GEOS and PROJ when built from source.

## Folder contents

- `data/`: source CSVs and the complete five-file map shapefile. No duplicate
  source CSVs are stored beside the notebooks.
- `results/`: rebuilt summary CSVs, organized by analysis. The notebooks
  regenerate them; they are outputs, not source inputs.
- `docs/`: the interview questionnaire.
- `figures/`: created when the notebooks run; generated figures are not
  committed in this version.

## Data notes

The raw interview CSVs preserve all records. Cleaning excludes `farm_19`,
which has no substantive survey answers: the analyzed sample is 23 farm
respondents at 15 sites and 12 abattoir respondents at four sites. The raw
interview CSVs are unchanged from the archived version; the analysis is updated.

`publications_reviewed.csv` contains 94 records. It was previously named
`publications_92_with_integrity_and_visibility.csv`; its contents are unchanged
by this reorganization. `locality_coordinates.csv` was previously named
`pig_study_locs - final_feb16.csv`. The 544-record source list documents the
search pool, not the final included sample. Source/evidence columns are retained.

Map locations are approximate locality/district representatives, not exact
interview farm coordinates. The existing OCHA administrative boundaries are
retained as used by the project and are not assumed to reflect current boundaries.

The [FAOSTAT Crops and livestock products](https://www.fao.org/faostat/en/#data/QCL)
download is dated 7 November 2026 and covers modern Ethiopia, 1993–2024.
Original flags are preserved: estimates and imputations are not treated as
independent census observations. Meat-production shares cover the six categories
listed in `faostat.Rmd`.
