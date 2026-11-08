# Ethiopia FAOSTAT analysis

`github_pig_data.Rmd` analyzes Ethiopia livestock stocks, meat production,
and slaughter counts using `data/FAOSTAT_data_en_11-7-2026.csv`.
The original FAOSTAT download and its flags are preserved.

Source: [FAOSTAT Crops and livestock products](https://www.fao.org/faostat/en/#data/QCL), downloaded 7 November 2026.

## Run

Download or clone the repository, then open an R session in its root folder.
Install the required packages once:

```r
install.packages(c("dplyr", "ggplot2", "tidyr", "ggrepel", "patchwork",
                   "scales", "knitr", "rmarkdown"))
```

Open `github_pig_data.Rmd` in RStudio and run its chunks, or render it with:

```r
rmarkdown::render("github_pig_data.Rmd")
```

Rendering requires Pandoc (included with RStudio). Figure files are written
to `figures/`. FAO estimates and imputations retain their source flags;
the meat-production shares refer only to the six categories in the analysis.
