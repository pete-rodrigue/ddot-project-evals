# DDOT Project Evaluations

Analysis of Washington DC Department of Transportation (DDOT) bike infrastructure and bus priority projects, focusing on safety outcomes and micromobility behavior.

Pete Rodrigue and Spencer Ainsworth

---

## Overview

This project evaluates the impact of DDOT's protected and buffered bike lane installations on traffic safety and cycling activity in Washington, DC. The analysis:

1. Replicates DDOT's own before-after evaluations of completed projects
2. Scales those evaluations to additional projects using DC crash data
3. Examines behavioral data from street observations, Capital Bikeshare (CaBi), and micromobility trip records

The main output is a rendered HTML report (`index.html`) containing maps, time series charts, and statistical summaries.

---

## Key Findings

- Protected and buffered bike lanes showed substantial safety improvements: crashes (vehicle, pedestrian, and overall) decreased significantly across most projects.
- Absolute bike crash counts sometimes increased, likely driven by elevated cycling volumes rather than increased risk per trip.
- Permutation tests (500 iterations) confirm these differences are statistically significant.
- Evidence bike ride volume being the driver of increased crashes: Micromobility trips increased more on streets with new bike infrastructure compared to other streets in DC.

---

## Data Sources

| Dataset | Source | Format |
|---|---|---|
| DC crash incidents | [DC Open Data](https://opendata.dc.gov) | CSV |
| Protected/buffered bike lane geometries | DC Open Data (hand-labeled installation dates via satellite imagery) | GeoJSON |
| Roadway subblock segments | DC Open Data | GeoJSON |
| Capital Bikeshare trip data | CaBi public data | CSV |
| Micromobility trip data | RideReport.com | GeoJSON |
| DDOT evaluation results | Google Sheets | CSV |

> **Note on crash data quality:** DC crash location accuracy improved significantly starting in 2016. Analyses are restricted to projects installed between 2017 and 2024. COVID years (2020–2023) are excluded to omit confounding travel pattern changes.

---

## Methodology

- **Spatial buffering:** Crashes are matched to street projects using a 4-meter buffer around each project corridor.
- **Time-series analysis:** Crash counts are smoothed with rolling averages and decomposed to identify trends.
- **Difference-in-differences:** Streets on which bike lanes were installed are compared against other streets to isolate the effect of installation.
- **Permutation testing:** 500 permutation iterations assess whether observed differences exceed what would be expected by chance.

---

## Repository Structure

```
ddot-project-evals/
├── analysis.Rmd
├── load_data.R
├── clean_cabi_data.R
├── clean_mm_trip_data.R
├── make_plots.R
├── index.html
├── data/
│   ├── Crashes_in_DC.csv
│   ├── Protected-Buffered-Bike-Lanes.geojson
│   ├── Roadway_SubBlock.geojson
│   └── cabi_both_years_clean.csv
├── .gitignore
├── .gitattributes
└── LICENSE
```

### Script descriptions

**`analysis.Rmd`** — The main analysis document. Knits to `index.html`. Loads all data, runs the full before-after evaluation pipeline (spatial buffering, difference-in-differences, permutation testing), and assembles all results, maps, and charts into the final report.

**`load_data.R`** — Reads the four core datasets into memory: DC crash incidents (`Crashes_in_DC.csv`), protected/buffered bike lane geometries (`Protected-Buffered-Bike-Lanes.geojson`), DC roadway subblock segments (`Roadway_SubBlock.geojson`), and the cleaned CaBi trip counts (`cabi_both_years_clean.csv`). Filters subblocks to exclude service roads, alleys, and walkways.

**`clean_cabi_data.R`** — Downloads monthly Capital Bikeshare trip ZIP files from the CaBi S3 bucket for 2018 and 2025, handles schema differences between years, aggregates trip counts per end station, joins the two years, and manually fills in coordinates for a handful of retired stations. Outputs `cabi_both_years_clean.csv`.

**`clean_mm_trip_data.R`** — Spatially matches RideReport micromobility trip records (`DC_RideReport_MM_Trips.geojson`) to roadway subblock segments using nearest-neighbor matching (≤15–20 m). Outputs `trips_subblocks_matched.csv` for use in the main analysis. Includes a `RUN` flag to prevent accidental re-execution.

**`make_plots.R`** — Defines all visualization functions used in `analysis.Rmd`: interactive Leaflet maps, four-panel crash time series charts (Overall / Vehicle / Bike / Pedestrian), pre/post density plots, empirical CDF plots, and summary statistics tables.

---

## Reproducing the Analysis

### Requirements

- R (≥ 4.0)
- R packages including `tidyverse`, `sf`, `leaflet`, `knitr`, and `rmarkdown`

### Steps

```r
# 1. Place raw data files in the data/ directory (see Data Sources above). Contact me if you want the raw files.

# 2. Load all datasets
source("load_data.R")

# 3. Render the full analysis
rmarkdown::render("analysis.Rmd")
```

The rendered report will be saved as `analysis.html`.

---

## License

MIT © 2026 Pete Rodrigue
