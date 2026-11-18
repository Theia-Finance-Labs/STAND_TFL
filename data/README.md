# Data

This repository contains the STAND code only. The input data is **not** included and is not
covered by the repository licence. To run the app, place the files below in this folder
(`data/`) with the names and columns shown. `app.R` checks for them at start-up and lists any
that are missing. Everything in `data/` except this README and `region_name_translations.csv`
is excluded from git by `.gitignore`.

## Included

| File | Content |
|---|---|
| `region_name_translations.csv` | ISO3 code → region name (`Full_Name`, `Abbreviation`) |

## Required, not included

| File | Loaded in | Content | Columns used |
|---|---|---|---|
| `STAND_model_parameters.csv` | `R/initialisation.R` | Fitted logistic pathway parameters per variable and region | `Unique_ID, Variable_ID, Region, Scenario, Phase, K, V0, r, t0, R_squared, Start_year, End_year, t0_perc, t0_category, t0_perc_simple, t0_category_low, t0_category_high, t0_perc_low, t0_perc_high, t0_simple, t0_low, t0_high, r_simple_cat, r_simple, Units` |
| `Energy_Policies_Params.csv` | `R/initialisation.R` | Forecast policy dates and texts per region and policy | `Energy_Region, Policy, Forecast_Definition, Forecast_Year, Forecast_Text, Definition_Text, Policy_Text, Energy_Region_Full` |
| `Policy_Value_Bridge.csv` | `R/initialisation.R` | Maps each policy to its pathway variable, threshold, units and sector | `Policy, Policy_Type, Policy_Order, Variable_ID, Policy_Threshold, Threshold_Criteria, Units, Metric2, Units2, Metric3, Units3, Metric4, Units4` |
| `SEAO_Power_Data.csv` | `R/initialisation.R` | Country power generation shares, to downscale South East Asia & Oceania | `Country, Sub_variable_class_1, Value, Variable` |
| `SEAO_Emissions_Share.csv` | `R/initialisation.R` | Country CO2 shares, to downscale South East Asia & Oceania | `Country, Region, Region_CO2_Perc` |
| `power_cost_retirement_rates.csv` | `R/initialisation.R` | Power capex per MW and retirement rates (used by Export Pathways) | `Sub_variable_class_2, Cost_per_MW_2022, Cost_per_MW_2030, Cost_per_MW_2050, Retirement_Rate` |
| `transport_costs.csv` | `R/initialisation.R` | Vehicle costs (used by Export Pathways) | `Unique ID, Variable ID, Scenario, Sub_variable_class_2, Sub_Sector, Cost_per_Vehicle_2022, Cost_per_Vehicle_2030, Cost_per_Vehicle_2050` |
| `global_emissions.csv` | `R/impact_calculations.R` | Global emissions by year | `Year, Global_Emissions` |
| `survey/expert_survey_baseline.csv` | `app.R`, `R/initialisation.R` | Survey responses shown on the Policy Analysis tab, one row per response | `Region, Policy, Response, SubGroup` |

Save CSVs as UTF-8 **without** a byte-order mark (BOM); otherwise the first column name is not
recognised on macOS/Linux.
