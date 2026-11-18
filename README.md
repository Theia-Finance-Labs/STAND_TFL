# STAND – IPR policy pathway app

STAND is a Shiny app for exploring the Inevitable Policy Response (IPR) Forecast Policy Scenario.
For each region and policy you can move the forecast policy date, see where the IPR forecast and the
expert survey place that policy, check how the underlying pathway (e.g. clean power share, BEV sales)
shifts, and get the resulting change in cumulative emissions. The pathways can be exported as CSV, including
technology-level capacity, new deployment and capex.

## Run it

1. Open `STAND.Rproj` in RStudio (or set the working directory to this folder).
2. Install the packages once:
   ```r
   install.packages(c("shiny", "shinyBS", "shinyjs", "dplyr", "plotly", "shinyWidgets",
                      "tidyr", "shinydashboard", "purrr", "stringr"))
   ```
3. Put the input data in `data/` (not included in this repository, see `data/README.md`).
4. Start the app:
   ```r
   shiny::runApp()
   ```

All paths are relative to the repository root, so no `setwd()` is needed.

## Structure

```
app.R                      UI and server
R/
  initialisation.R         loads the data, sets thresholds, pathway and recalibration functions
  create_plot_data.R       builds time series from the logistic pathway parameters
  impact_calculations.R    emissions impact of a changed policy date
  humps_bumps.R            disruption pathways (Advanced Analysis tab)
  _disable_autoload.R      stops Shiny auto-sourcing R/; app.R sources the files in order
data/                      input data goes here - not included (see data/README.md)
```

## Licence

The code is licensed under GPL-3.0, see `LICENSE`. The licence covers the code only; no input
data is distributed with this repository.
