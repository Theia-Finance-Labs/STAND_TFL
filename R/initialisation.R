# Initialize environment for the Stand Shiny App.

# Read in pre-processed forecast parameters file
model_parameters  <- read.csv(file.path(data_dir, "STAND_model_parameters.csv"), check.names = FALSE)

# Cost and retirement assumptions (used by the "Export Pathways" download)
power_cost_retirement  <- read.csv(file.path(data_dir, "power_cost_retirement_rates.csv"), check.names = FALSE)
transport_costs  <- read.csv(file.path(data_dir, "transport_costs.csv"), check.names = FALSE)
# 

model_parameters <- model_parameters %>%
  mutate( #r = r_simple,
    t0 = t0_simple,
    t0_perc = t0_perc_simple) %>%
  rename("t0_perc_Category" = "t0_category", "r_Category" =  "r_simple_cat")

# correct the units..
# Update Units to "%" where Variable_ID contains "percentage", case-insensitive
model_parameters$Units[grepl("percentage", model_parameters$Variable_ID, ignore.case = TRUE)] <- "%"


# Read in Policy dataframe
Policies <- read.csv(file.path(data_dir, "Energy_Policies_Params.csv"), check.names = FALSE) %>%
  rename("Region" = "Energy_Region")
Policy_Bridge <- read.csv(file.path(data_dir, "Policy_Value_Bridge.csv"), check.names = FALSE) %>%
  select(c("Policy", "Variable_ID", "Policy_Threshold", "Threshold_Criteria", "Units", "Policy_Type", "Policy_Order", "Metric2", "Units2", "Metric3", "Units3", "Metric4", "Units4"))
Policies <- Policies %>%
  left_join(Policy_Bridge, by = c("Policy"))%>%  filter(!Policy %in% c("Carbon pricing"))
Policies$Your_Response = NA

# Correct incorrect label for South Africa
Policies$Region <- gsub("^ZAF$", "SAF", Policies$Region)

# Ensure Forecast_Year is numeric
Policies$Forecast_Year <- as.numeric(Policies$Forecast_Year)
rm(Policy_Bridge)

#Filter the available policies to the ones where there are models for, and have thresholds
Policies <- Policies %>%
  filter(Variable_ID %in% model_parameters$Variable_ID & !is.na(Policy_Threshold))


# Add Thailand to params & Policies, inherit SEAO params, downscale in app through downscale_plot_data
Thailand_proxy <- Policies %>%
  filter(Region == "SEAO") %>%
  mutate(Region = "Thailand",
         Energy_Region_Full = "Thailand")

Policies <- rbind(Policies, Thailand_proxy)

# Add lines for the supplemental questions, on BEV, clean power and for CO2.

BEV_proxy <- Policies %>%
  filter(Policy == "Phase out of CO2 light duty vehicles") %>%
  mutate(Policy = "BEV Sales 2030",
         Variable_ID = "New deployment_Percent_BEV_Transport_LDV")

Clean_proxy <- Policies %>%
  filter(Policy == "Clean power") %>%
  mutate(Policy = "Clean Power 2030")

CO2_proxy <- Policies %>%
  filter(Policy == "Net zero target") %>%
  mutate(Policy = "80% Net Zero CO2", Variable_ID = "Net CO2 emissions_CO2_Percent",  Units = "%")

Policies <- rbind(Policies, BEV_proxy, Clean_proxy, CO2_proxy)

rm(BEV_proxy,Clean_proxy,CO2_proxy)

# Translate abbreviated regions to full names
# Read in region name translation file
region_names<- read.csv(file.path(data_dir, "region_name_translations.csv"), check.names = FALSE) 
                       
# # Replace Region in Policies
# Policies <- Policies %>%
#   left_join(region_names, by = c("Region" = "Abbreviation")) %>%
#   mutate(Region = Full_Name) %>%
#   select(-Full_Name)


# Replace Region in Policies
Policies <- Policies %>%
    mutate(Region = Energy_Region_Full) %>%
  filter(Region != "World")

Policies$Region <- iconv(Policies$Region, from = "UTF-8", to = "ASCII//TRANSLIT")

 
# Replace Region in model_parameters
model_parameters <- model_parameters %>%
  left_join(region_names, by = c("Region" = "Abbreviation")) %>%
  mutate(Region = Full_Name) %>%
  select(-Full_Name)

Thailand_params_proxy <- model_parameters %>%
  filter(Region == "South East Asia and Oceania") %>%
  mutate(Region = "Thailand")

model_parameters <- rbind(model_parameters, Thailand_params_proxy)

rm(Thailand_params_proxy)
rm(Thailand_proxy)

# Remove declining BEV  models
model_parameters <- model_parameters[!(model_parameters$Variable_ID == "New deployment_Percent_BEV_Transport_LDV" & 
                                         model_parameters$Phase == "Decline"), ]

# Force carry value K
model_parameters$K[model_parameters$Variable_ID == "New deployment_Percent_BEV_Transport_Cars" & 
                     model_parameters$K >= .90 ] <- 1

# #Add row for 80% net co2

NETCO2proxy <- model_parameters %>%
  filter(Variable_ID == "GHG emissions_CO2_Total") %>%
  mutate(Variable_ID = "Net CO2 emissions_CO2_Percent")


update_model_parameters_to_percent <- function(params) {
  # Make a copy to avoid modifying the original data frame
  params_updated <- params
  
  # Check if any V0 are zero (to avoid division by zero)
  if(any(params$V0 == 0, na.rm = TRUE)) {
    warning("Some V0 values are zero. Division by zero may occur for these rows.")
  }
  
  # Re-scale K and V0:
  # New K is 100 * (K / V0) so that when f(t_min) = V0 is re-scaled to 100,
  # the asymptotic value becomes 100 * (K/V0)
  params_updated$K <- (params$K / params$V0)
  
  # Set V0 to 100 (i.e., initial value is now 100%)
  params_updated$V0 <- 1
  
  # Update the Units column to indicate percentages
  params_updated$Units <- "%"
  
  return(params_updated)
}

NETCO2proxy <- update_model_parameters_to_percent(NETCO2proxy)

model_parameters <- rbind(model_parameters, NETCO2proxy)

rm(NETCO2proxy)


# Read in pre-processed forecast parameters file
all_survey_data <- read.csv(survey_file, check.names = FALSE) %>%
  filter(!is.na(Response))

# all_survey_data <- all_survey_data %>%
#   mutate(SubGroup = recode(SubGroup, "NY Pre-Workshop" = "Previous Workshops"))

# Ensure SubGroup is a factor
all_survey_data$SubGroup <- as.factor(all_survey_data$SubGroup)


# Replace Region in all_survey_data
all_survey_data <- all_survey_data %>%
  left_join(region_names, by = c("Region" = "Abbreviation")) %>%
  mutate(Region = if_else(nchar(Region) == 3, Full_Name, Region)) %>%
  select(-Full_Name)

# List of policies
policy_list <- c("Clean power",
                 "Deforestation",
                 "Industry fuel combustion",
                 "Industry process emissions",
                 "Land protection & restoration",
                 "Nature incentives",
                 "Phase out of CO2 heavy duty vehicles",
                 "Phase out of CO2 light duty vehicles",
                 "Phase out of all unabated coal",
                 "Clean Power 2030",
                 "BEV Sales 2030",
                 "80% Net Zero CO2"
                 )  #"Phase out of new coal"


# Define Category thresholds and names for rate
Constant <- 0.01
Slow <- 0.05
Medium <- 0.10
Fast <- 0.15
Hyper <- 0.25
Supersonic <- 0.35

r_thresholds <- c(Constant = Constant, Slow = Slow, Medium = Medium, Fast = Fast, Hyper = Hyper, Supersonic = Supersonic)
# Convert to a two-column dataframe
r_thresholds_df <- data.frame(r_Category = names(r_thresholds), Threshold = unname(r_thresholds))
r_categories <- names(r_thresholds)  # Extracting names from the thresholds vector

# Define Category thresholds and names for t0_perc
Underway <- -0.5
Immediate <- 0
Near_term <- 0.25
Midway <- 0.5
Late <- 0.75
Post_2050 <- 1.00
t0_perc_thresholds <- c(Underway = Underway, Immediate = Immediate, Near_term = Near_term, Midway = Midway, Late = Late, Post_2050 = Post_2050)

# Convert to a two-column dataframe
t0_perc_thresholds_df <- data.frame(t0_perc_Category = names(t0_perc_thresholds), Threshold = unname(t0_perc_thresholds))

# Extract categories and thresholds
t0_categories <- t0_perc_thresholds_df$t0_perc_Category
t0_thresholds <- t0_perc_thresholds_df$Threshold

####### Define helper functions for find nearest_model

# Define the modified logistic growth model for searching for closest Model, with V0 enforced at t=0
modified_logistic_model_search <- function(t, K, V0, r, t0, start_year) {
  P0 = K - (K - V0) * (1 + exp(-r * (t0 - start_year)))
  return(K + (P0 - K) / (1 + exp(r * (t - t0))))
}

# Define the function to find the appropriate r value
model_difference <- function(r, user_forecast_year, K, V0, t0, start_year, threshold) {
  value <- modified_logistic_model_search(user_forecast_year, K, V0, r, t0, start_year)
  return(value - threshold)
}

# Function to classify new r value
assign_nearest_r_category_and_threshold <- function(r, thresholds, categories) {
  # Handling the special case for values between -0.05 and +0.05
  if (r >= -0.05 && r <= 0.05) {
    return(list(category = "Constant", threshold = 0))
  }

  # Calculate distances to thresholds
  distances <- sapply(thresholds, function(threshold) abs(r - threshold))

  category_index <- which.min(distances)
  nearest_category <- categories[category_index]
  nearest_threshold <- thresholds[category_index]

  # Adjust threshold sign based on the sign of r
  nearest_threshold <- if (r < 0) -nearest_threshold else nearest_threshold

  list(category = nearest_category, threshold = nearest_threshold)
}

# Function to classify t0 percent value
assign_t0_category <- function(t0_perc, thresholds, categories) {
  # Check for exact match in thresholds
  if (t0_perc %in% thresholds) {
    category_index <- which(thresholds == t0_perc)
    nearest_category <- categories[category_index]
    return(nearest_category)
  } else {
    return(NA)  # Return NA if no exact match is found
  }
}

# Function to classify t0 percent value
assign_t0_perc_category_open <- function(t0_perc, thresholds, categories) {

    # Calculate distances to thresholds
    distances <- sapply(thresholds, function(threshold) abs(t0_perc - threshold))

    category_index <- which.min(distances)
    nearest_category <- categories[category_index]
    nearest_threshold <- thresholds[category_index]

    # Adjust threshold sign based on the sign of r
    nearest_threshold <- if (t0_perc < 0) -nearest_threshold else nearest_threshold

    list(category = nearest_category, threshold = nearest_threshold)

  }


# Define functions for calculations
calculate_mean_year <- function(df) {
  weighted_sum <- sum(df$Year * df$Responses, na.rm = TRUE)
  total_responses <- sum(df$Responses, na.rm = TRUE)
  mean_year <- weighted_sum / total_responses
  return(mean_year)
}

calculate_distance_from_mean <- function(mean_year, response_year) {
  distance <- abs(response_year - mean_year)
  return(distance)
}

calculate_percentile <- function(df, response_year) {
  sorted_years <- sort(unique(df$Year))
  total_responses <- sum(df$Responses, na.rm = TRUE)

  if (response_year <= min(sorted_years)) {
    return(0)
  }
  if (response_year >= max(sorted_years)) {
    return(100)
  }

  cumulative_responses <- df %>%
    group_by(Year) %>%
    summarise(Responses = sum(Responses, na.rm = TRUE), .groups = 'drop') %>%
    arrange(Year) %>%
    mutate(Cumulative = cumsum(Responses))

  response_percentile <- approx(x = cumulative_responses$Year, y = cumulative_responses$Cumulative, xout = response_year)$y / total_responses * 100
  return(response_percentile)
}


# # # #Test varaibles
# variable_id = "GHG emissions_CO2_Total"
# region = "Western EU"
# user_forecast_year = 2057
# app_units = "NA"
# Value_Driver_Params = model_parameters
# #
#  test <- create_pathways(variable_id, region, user_forecast_year, Value_Driver_Params, app_units)

# Define function to create pathways by stretching the dataset
create_pathways <- function(variable_id, region, user_forecast_year, Value_Driver_Params, app_units) {


  cat("Scaling", variable_id, "(", app_units,")", " in ", region, " for ", user_forecast_year, "\n")

  # Error checks
  if (is.na(variable_id) || variable_id == "") {
    warning("Variable_ID is missing - skipping calculations.")
    return(NULL)
  }

  if (is.na(user_forecast_year) || user_forecast_year == "") {
    warning("Variable_ID is missing - skipping calculations.")
    return(NULL)
  }

  # Retrieve params for IPR data
  FPS_params <- Value_Driver_Params  %>%
    filter(Variable_ID == variable_id, Region == region)


  Units = FPS_params$Units

  if (is.null(FPS_params) || nrow(FPS_params) == 0) {
    warning("No value driver for that region/variable - skipping calculations.")
    return(NULL)
  }

  # Version if you take the original value drivers, but it will only go to 2050.
  # ipr_data_subset <-  Value_Drivers %>%
  #   filter(Variable_ID == variable_id , Region == region, !Year %in% c(2020,2021) ) %>%
  #   select(Variable_ID, Region, Year, Value)

  Policies_sub <- Policies %>%
    filter(Variable_ID == variable_id, Region == region)

  # if units = mt for power, then get power sector emissions and scale that to the forecast data. assume same power demand, get existing power capacity and assume fixed emission intensity of non-clean techs to get tech

  # Overwrite the params with different units
  if(variable_id == "Electricity generation_Clean Percentage_Power" & app_units == "Mt") {
  cat(" Creating emisssions data for Clean Power")

    FPS_params <- Value_Driver_Params  %>%
      filter(Variable_ID == "GHG emissions_CO2_Power" , Region == region)


    if (is.null(FPS_params) || nrow(FPS_params) == 0) {
      warning("No value driver for that region/variable - skipping calculations.")
      return(NULL)
    }
  }

  # Overwrite the params with different units
  if(variable_id == "Electricity generation_Clean Percentage_Power" & app_units == "TWh") {

    FPS_params <- Value_Driver_Params  %>%
      filter(Variable_ID == "Electricity generation_Clean_Power" , Region == region)

    if (is.null(FPS_params) || nrow(FPS_params) == 0) {
      warning("No value driver for that region/variable - skipping calculations.")
      return(NULL)
    }
  }

  # Overwrite the params wth different units
  if(variable_id == "Electricity generation_Clean Percentage_Power" & app_units == "GW") {

    cat("Creating capacity data for Clean Power")

    FPS_params <- Value_Driver_Params  %>%
      filter(Variable_ID == "Capacity_Electricity generation_Clean_Power" , Region == region)

    if (is.null(FPS_params) || nrow(FPS_params) == 0) {
      warning("No value driver for that region/variable - skipping calculations.")
      return(NULL)
    }
  }

  # Retrieve IPR Forecast year
  # For clean power GHG emissions_CO2_Power & LDVs we need slightly different rules, as we scale the emissions pathway not the primary value driver. This is for when the function is called form the emissions calculations
  if(variable_id == "GHG emissions_CO2_Power") {
    threshold_criteria <- "<="
    IPR_Year <- Policies %>%
      filter(Variable_ID == "Electricity generation_Clean Percentage_Power", Region == region) %>%
      pull(Forecast_Year) %>%
      first()
  } else {
    if(variable_id == "GHG emissions_CO2_Transport_Cars") {
      threshold_criteria <- "<="
      IPR_Year <- Policies %>%
        filter(Variable_ID == "New deployment_Percent_ICE_Transport_Cars", Region == region) %>%
        pull(Forecast_Year) %>%
        first()
    } else {

      IPR_Year <- Policies_sub$Forecast_Year[1]
      threshold_criteria <- Policies_sub$Threshold_Criteria[1]

      }
  }

  # Round user forecast year in case an average result is sent from group data
  user_forecast_year <- round(user_forecast_year)

  year_change <- user_forecast_year - IPR_Year


  # Get FPS time series data
  ipr_pathway <- create_plot_data(FPS_params) %>%
    rename(Value = Fitted_model) %>%
    mutate(Region = region,
           Variable_ID = variable_id)

  min_year <- min(ipr_pathway$Year, na.rm = TRUE)
  max_year <- max(ipr_pathway$Year, na.rm = TRUE)

  if (max_year == min_year) {
    warning("max_year equals min_year; cannot compute stretch")
    return(NULL)
  }

  iterations <- 0
  max_iterations <- 100  # Set a reasonable maximum number of iterations
  condition_met <- FALSE

  while (!condition_met && iterations < max_iterations) {

    # Apply the stretch!
    ipr_pathway$Stretched_Year <- ipr_pathway$Year +
      (ipr_pathway$Year - min_year) * (year_change / (max_year - min_year))

    result_columns <- c('Stretched_Year', 'Value', 'Variable_ID', 'Region')
    transformed_subset <- ipr_pathway[, result_columns, drop = FALSE]
    colnames(transformed_subset)[colnames(transformed_subset) == 'Stretched_Year'] <- 'Year'

    transformed_subset <- interpolate_values(transformed_subset)

    # Exit if unable to create pathways
    if (is.null(transformed_subset)) {
      warning("Unable to create pathways")
      break
    }

    # Find User_Value from interpolated_subset
    final_result_value_row <- transformed_subset %>%
      filter(Year == user_forecast_year)

    user_value <- if (nrow(final_result_value_row) > 0) final_result_value_row$Value else NA

    # Find FPS value at IPR policy year
    ipr_value_row <- ipr_pathway %>%
      filter(Year == IPR_Year)

    ipr_value <- if (nrow(ipr_value_row) > 0) ipr_value_row$Value else NA

    # Check the condition based on Threshold_Criteria
    if (!is.na(user_value) && !is.na(ipr_value)) {
      if (threshold_criteria == "<=") {
        condition_met <- (user_value <= ipr_value)
      } else if (threshold_criteria == ">=") {
        condition_met <- (user_value >= ipr_value)
      } else {
        warning(paste("Unknown Threshold_Criteria:", threshold_criteria))
        break
      }
    } else {
      condition_met <- FALSE
    }

    if (condition_met) {
      final_result_df <- transformed_subset
      break
    } else {
      # Modify year_change based on Threshold_Criteria
      if (threshold_criteria == "<=") {
        if (year_change < 0) {
          year_change <- year_change - 1
        } else if (year_change > 0) {
          year_change <- year_change + 1
        } else {
          year_change <- 1  # Start increasing if zero
        }
      } else if (threshold_criteria == ">=") {
        if (year_change < 0) {
          year_change <- year_change - 1  # More negative
        } else if (year_change > 0) {
          year_change <- year_change + 1  # More positive
        } else {
          year_change <- -1  # Start decreasing if zero
        }
      }
    }

    iterations <- iterations + 1
    if (iterations >= max_iterations) {
      warning("Maximum iterations reached for adjusting year_change")
      break
    }

  }

  if (condition_met) {

    final_result_df$Units = FPS_params$Units[1]

    return(final_result_df)
  } else {
    warning("Condition not met within maximum iterations")
    return(NULL)
  }
}


# Define the interpolation function used in pathways function
interpolate_values <- function(data) {

  year_range <- 2022:2100
  interpolated <- approx(x = data$Year, y = data$Value, xout = year_range, rule = 2)

  interpolated_df <- data.frame(
    Year = interpolated$x,
    Value = interpolated$y,
    stringsAsFactors = FALSE
  )

  other_columns <- setdiff(names(data), c('Year', 'Value'))
  for (col in other_columns) {
    interpolated_df[[col]] <- unique(data[[col]])
  }

  return(interpolated_df)
}


create_tech_pathways <- function(new_pathways) {

  if (!exists("model_parameters", envir = .GlobalEnv)) {
    stop("The dataframe 'model_parameters' does not exist in the global environment.")
  }

  model_parameters <- get("model_parameters", envir = .GlobalEnv)

  power_ids <- Policies %>%
    select(Policy_Type, Variable_ID, Units) %>%
    filter(Policy_Type == "Power") %>%
      distinct()

  # power_ids <- c(
  #   "Electricity generation_Clean Percentage_Power",
  #   "Electricity generation_Coal Percentage_Power")

  ldv_ids <- c(
    "New deployment_Percent_ICE_Transport_Cars")

  #initialise results dataframes
  all_power_tech_scaled = data.frame()
  all_ldv_tech_scaled = data.frame()

  #Assess if power policies have been modified and create power pathways
  if(any(new_pathways$Variable_ID %in% power_ids$Variable_ID)) {

  power_techs_list <- c(
    "Capacity_Electricity generation_Biomass CCS Percentage_Power",
    "Capacity_Electricity generation_Biomass Percentage_Power",
    "Capacity_Electricity generation_Coal CCS Percentage_Power",
    "Capacity_Electricity generation_Coal Percentage_Power",
    "Capacity_Electricity generation_Hydro Percentage_Power",
    "Capacity_Electricity generation_Hydrogen Percentage_Power",
    "Capacity_Electricity generation_Natural gas CCS Percentage_Power",
    "Capacity_Electricity generation_Natural gas Percentage_Power",
    "Capacity_Electricity generation_Nuclear Percentage_Power",
    "Capacity_Electricity generation_Offshore wind Percentage_Power",
    "Capacity_Electricity generation_Oil Percentage_Power",
    "Capacity_Electricity generation_Onshore wind Percentage_Power",
    "Capacity_Electricity generation_Solar Percentage_Power",
    "Electricity generation_Biomass CCS Percentage_Power",
    "Electricity generation_Biomass Percentage_Power",
    "Electricity generation_Coal CCS Percentage_Power",
    "Electricity generation_Coal Percentage_Power",
    "Electricity generation_Hydro Percentage_Power",
    "Electricity generation_Hydrogen Percentage_Power",
    "Electricity generation_Natural gas CCS Percentage_Power",
    "Electricity generation_Natural gas Percentage_Power",
    "Electricity generation_Nuclear Percentage_Power",
    "Electricity generation_Offshore wind Percentage_Power",
    "Electricity generation_Oil Percentage_Power",
    "Electricity generation_Onshore wind Percentage_Power",
    "Electricity generation_Solar Percentage_Power"
  )

  # Filter new_pathways to include only relevant Variable_IDs and get unique regions
  # filtered_new_pathways <- new_pathways %>%
  #   filter(Variable_ID %in% power_ids)

  filtered_new_pathways <- new_pathways %>%
    semi_join(power_ids, by = c("Variable_ID", "Units"))

  regions_to_process <- unique(filtered_new_pathways$Region)

  # Create all combinations of Region and Variable_ID in power_techs_list
  combinations <- expand.grid(
    Region = regions_to_process,
    Variable_ID = power_techs_list,
    stringsAsFactors = FALSE
  )

  plot_data_list <- list()

  # Iterate over each combination of Region and Variable_ID
  for (i in seq_len(nrow(combinations))) {
    current_region <- combinations$Region[i]
    current_variable_id <- combinations$Variable_ID[i]

    # Filter model_parameters by Region and Variable_ID
    technology_params <- model_parameters %>%
      filter(
        Variable_ID == current_variable_id,
        Region == current_region
      )

    if (nrow(technology_params) == 0) {
      next  # Skip if no data
    }

    # Call create_plot_data with the filtered technology_params
    plot_data <- create_plot_data(technology_params)

    # Add Variable_ID and Region columns to the result
    plot_data$Variable_ID <- current_variable_id
    plot_data$Region <- current_region

    plot_data_list[[length(plot_data_list) + 1]] <- plot_data
  }

  # Combine all plot data into a single dataframe
  technology_pathways_OG <- bind_rows(plot_data_list)

  # normalize the technology shares for production and capacity as proportion of clean power and non-clean power, then scale by the change

  # Filter out the rows where Variable_ID contains "Coal", "Oil", or "Gas" but not "CCS", ignoring case
  dirty_technology_pathways_filtered <- technology_pathways_OG %>%
    filter(grepl("Coal|Oil|Gas", Variable_ID, ignore.case = TRUE) &
             !grepl("CCS", Variable_ID, ignore.case = TRUE))

  # Group by Year, Region, and create two groups based on whether Variable_ID contains "Capacity_Electricity generation"
  dirty_technology_pathways_scaled <- dirty_technology_pathways_filtered %>%
    group_by(Year, Region, contains_capacity = grepl("Capacity_Electricity generation", Variable_ID, ignore.case = TRUE)) %>%
    mutate(Total_Fitted = sum(Fitted_model),
           Scaled_Value = Fitted_model / Total_Fitted) %>%
    ungroup() #%>%
    # select(-contains_capacity)

  # Now create the clean power dataset
  clean_technology_pathways_filtered <- technology_pathways_OG %>%
    filter(!Variable_ID %in% dirty_technology_pathways_filtered$Variable_ID)

  # Group by Year, Region, and create two groups based on whether Variable_ID contains "Capacity_Electricity generation"
  clean_technology_pathways_scaled <- clean_technology_pathways_filtered %>%
    group_by(Year, Region, contains_capacity = grepl("Capacity_Electricity generation", Variable_ID, ignore.case = TRUE)) %>%
    mutate(Total_Fitted = sum(Fitted_model),
           Scaled_Value = Fitted_model / Total_Fitted) %>%
    ungroup() #%>%
    # select(-contains_capacity)


  # Now scale by new pathways
  # extract the two different policy pathways
  cleangen_perc_pathways <- filtered_new_pathways %>%
    filter(Variable_ID  == "Electricity generation_Clean Percentage_Power") %>%
    rename("Clean_Perc" = "Value") %>%
    distinct() %>%
    select(-Variable_ID)

  coalgen_perc_pathways <- filtered_new_pathways %>%
    filter(Variable_ID  == "Electricity generation_Coal Percentage_Power") %>%
    rename("Coal_Perc" = "Value") %>%
    distinct() %>%
    select(-Variable_ID)

  dirty_technology_pathways_scaled <- dirty_technology_pathways_scaled %>%
    filter(contains_capacity == FALSE) %>%
    left_join(cleangen_perc_pathways, by = c("Region", "Year")) %>%
    mutate(Value = Scaled_Value * (1-Clean_Perc)) %>%
    select(Year, Value, Variable_ID, Region)

  clean_technology_pathways_scaled <- clean_technology_pathways_scaled %>%
    filter(contains_capacity == FALSE) %>%
    left_join(cleangen_perc_pathways, by = c("Region", "Year")) %>%
    mutate(Value = Scaled_Value * (Clean_Perc)) %>%
    select(Year, Value, Variable_ID, Region)

  # Now you have tech shares of generation, you need absolute generation, and absolute capacity
  # multiply tech share by absolute generation

  # Calculate original generation pathway, requires to iterate over each Region

  gen_plot_data_list <- list()

  for (region in regions_to_process) {

    # Filter model_parameters by Region and Variable_ID
    generation_params <- model_parameters %>%
      filter(
        Variable_ID == "Electricity generation_Total_Power",
        Region == region
      )

    if (nrow(generation_params) == 0) {
      next  # Skip if no data
    }

    # Call create_plot_data with the filtered technology_params
    gen_plot_data <- create_plot_data(generation_params)

    # Add Variable_ID and Region columns to the result
    gen_plot_data$Variable_ID <- "Electricity generation_Total_Power"
    gen_plot_data$Region <- region

    gen_plot_data_list[[length(gen_plot_data_list) + 1]] <- gen_plot_data
  }

  # Combine all plot data into a single dataframe
  sec_generation_pathways_OG <- bind_rows(gen_plot_data_list) %>%
    rename("Total_Sector_Gen" = "Fitted_model") %>%
    select(-Variable_ID) %>%
    distinct(Year, Region, Total_Sector_Gen)

  dirty_technology_pathways_scaled <- dirty_technology_pathways_scaled %>%
    left_join(sec_generation_pathways_OG, by = c("Region", "Year")) %>%
    mutate(Generation = Value * Total_Sector_Gen)

  clean_technology_pathways_scaled <- clean_technology_pathways_scaled %>%
    left_join(sec_generation_pathways_OG, by = c("Region", "Year")) %>%
    mutate(Generation = Value * Total_Sector_Gen)

  all_technology_pathways_scaled <- rbind(dirty_technology_pathways_scaled, clean_technology_pathways_scaled)

  # Now capacity
  # Calculate existing capacity factors

  cap_plot_data_list <- list()

  for (region in regions_to_process) {

    # Filter model_parameters by Region and Variable_ID
    capacity_params <- model_parameters %>%
      filter(
        Variable_ID == "Capacity_Electricity generation_Total_Power",
        Region == region
      )

    if (nrow(capacity_params) == 0) {
      next  # Skip if no data
    }

    # Call create_plot_data with the filtered technology_params
    cap_plot_data <- create_plot_data(capacity_params)

    # Add Variable_ID and Region columns to the result
    cap_plot_data$Variable_ID <- "Capacity_Electricity generation_Total_Power"
    cap_plot_data$Region <- region

    cap_plot_data_list[[length(cap_plot_data_list) + 1]] <- cap_plot_data
  }

  # Combine all plot data into a single dataframe
  capacity_pathways_OG <- bind_rows(cap_plot_data_list) %>%
    rename("Total_Sector_Cap" = "Fitted_model") %>%
    select(-Variable_ID) %>%
    distinct(Year, Region, Total_Sector_Cap)

  perc_capacity_pathways_OG <- technology_pathways_OG  %>%
    filter(grepl("Capacity", Variable_ID, ignore.case = TRUE))

  capacity_pathways_OG <- capacity_pathways_OG %>%
    left_join(perc_capacity_pathways_OG, by =c("Year", "Region")) %>%
    mutate(tech_cap = Fitted_model*Total_Sector_Cap) %>%
    mutate(Technology = sub(".*generation_(.*?) Percentage_Power.*", "\\1", Variable_ID)) %>%
    select(-Variable_ID)


  perc_gen_pathways_OG <- technology_pathways_OG  %>%
    filter(grepl("generation", Variable_ID, ignore.case = TRUE) &
      !grepl("Capacity", Variable_ID, ignore.case = TRUE))

  generation_pathways_OG <- sec_generation_pathways_OG %>%
    left_join(perc_gen_pathways_OG, by =c("Year", "Region")) %>%
    mutate(tech_gen = Fitted_model*Total_Sector_Gen) %>%
    select(Year, Region, Variable_ID, tech_gen) %>%
    mutate(Technology = sub(".*generation_(.*?) Percentage_Power.*", "\\1", Variable_ID))


  cap_factor_pathways_OG <- capacity_pathways_OG  %>%
    left_join(generation_pathways_OG, by = c("Year", "Region", "Technology")) %>%
    mutate(capacity_factor_TWh_GW = tech_gen/tech_cap) %>%
    select(Year, Region, Variable_ID, capacity_factor_TWh_GW, Technology)

  #calculate new capacity from new gnerationusing orginal capacity factors
  all_technology_pathways_scaled <- all_technology_pathways_scaled %>%
    left_join(cap_factor_pathways_OG, by = c("Year", "Region", "Variable_ID")) %>%
    mutate(Capacity = Generation / capacity_factor_TWh_GW)

  all_tech_gen_scaled <- all_technology_pathways_scaled %>%
    mutate(Variable_ID = paste0("Electricity generation_", Technology,"_Power"), Units = "TWh") %>%
    select(Year, Generation, Variable_ID, Region, Units) %>%
    rename(Value = Generation)

  all_tech_cap_scaled <- all_technology_pathways_scaled %>%
    mutate(Variable_ID = paste0("Capacity_Electricity generation_", Technology,"_Power"), Units = "GW") %>%
    select(Year, Capacity, Variable_ID, Region, Units) %>%
    rename(Value = Capacity)

  all_power_tech_scaled <- rbind(all_tech_cap_scaled, all_tech_gen_scaled)

  }

  if(any(new_pathways$Variable_ID %in% ldv_ids)) {

          # Filter new_pathways to include only relevant Variable_IDs and get unique regions
          filtered_new_pathways <- new_pathways %>%
            filter(Variable_ID %in% ldv_ids) %>%
            rename(ICE_Deploy_Perc = Value) %>%
            distinct() %>%
            mutate(Technology = str_extract(Variable_ID, "ICE|PHEV|BEV|H2")) %>%
            select(-Units, -Variable_ID)


          regions_to_process <- unique(filtered_new_pathways$Region)


          # ldv_techs_list <- c(
          #   "New deployment_Vehicles_ICE_Transport_Cars",
          #   "New deployment_Vehicles_ICE_Transport_Vans",
          #   "New deployment_Vehicles_ICE_Transport_Trucks",
          #   "New deployment_Vehicles_PHEV_Transport_Cars",
          #   "New deployment_Vehicles_PHEV_Transport_Vans",
          #   "New deployment_Vehicles_BEV_Transport_Cars",
          #   "New deployment_Vehicles_BEV_Transport_Vans",
          #   "New deployment_Vehicles_BEV_Transport_Trucks",
          #   "New deployment_Vehicles_H2_Transport_Cars",
          #   "New deployment_Vehicles_H2_Transport_Vans",
          #   "New deployment_Vehicles_H2_Transport_Trucks"
          # )

          ldv_techs_list <- c(
            "New deployment_Vehicles_ICE_Transport_Cars",
            "New deployment_Vehicles_PHEV_Transport_Cars",
            "New deployment_Vehicles_BEV_Transport_Cars",
            "New deployment_Vehicles_H2_Transport_Cars")

          # Create all combinations of Region and Variable_ID in power_techs_list
          combinations <- expand.grid(
            Region = regions_to_process,
            Variable_ID = ldv_techs_list,
            stringsAsFactors = FALSE
          )

          plot_data_list <- list()

          # Iterate over each combination of Region and Variable_ID
          for (i in seq_len(nrow(combinations))) {
            current_region <- combinations$Region[i]
            current_variable_id <- combinations$Variable_ID[i]

            # Filter model_parameters by Region and Variable_ID
            technology_params <- model_parameters %>%
              filter(
                Variable_ID == current_variable_id,
                Region == current_region
              )

            if (nrow(technology_params) == 0) {
              next  # Skip if no data
            }

            # Call create_plot_data with the filtered technology_params
            plot_data <- create_plot_data(technology_params)

            # Add Variable_ID and Region columns to the result
            plot_data$Variable_ID <- current_variable_id
            plot_data$Region <- current_region

            plot_data_list[[length(plot_data_list) + 1]] <- plot_data
          }

          # Combine all plot data into a single dataframe
          transp_technology_pathways_OG <- bind_rows(plot_data_list)

          # Calculate the total volume of cars deployed, the technology share for each technology, and then each non-ICE technology as a proportion of all non-ICE
          deployment_total <- transp_technology_pathways_OG %>%
            filter(grepl("ICE|PHEV|BEV|H2", Variable_ID, ignore.case = TRUE)) %>%
            group_by(Year, Region) %>%
            mutate(
              Total_Deployment = sum(Fitted_model, na.rm = TRUE),
              Total_techshare = Fitted_model / Total_Deployment  # Calculate total tech share here
            ) %>%
            ungroup() %>%
            group_by(Year, Region) %>%
            mutate(
              NonICE = grepl("PHEV|BEV|H2", Variable_ID, ignore.case = TRUE),
              NonICE_Deployment = sum(Fitted_model[NonICE], na.rm = TRUE),  # Sum Non-ICE deployment
              NonICE_techshare = ifelse(NonICE, Fitted_model / NonICE_Deployment, NA)  # Calculate non-ICE tech share
            ) %>%
            ungroup() %>%
            mutate(Technology = str_extract(Variable_ID, "ICE|PHEV|BEV|H2"))

          # Calculate the new volume of ICE based on the revise pathway * total orginal volume

          ICE_deployment <- deployment_total %>%
            filter(grepl("ICE", Variable_ID, ignore.case = TRUE)) %>%
            left_join(filtered_new_pathways, by = c("Year", "Region", "Technology")) %>%
          mutate(Revised_Deployment  = ICE_Deploy_Perc * Total_Deployment,
                 Revised_NonICE_Deployment = Total_Deployment - Revised_Deployment)

          # Calculate the remaining volume and then multiply that by each clean technology as a proportion of all clean.
          Revised_NonICE_Deployment <- ICE_deployment %>%
            select(Year, Region, Revised_NonICE_Deployment)

          Non_ICE_deployment <- deployment_total %>%
            filter(!grepl("ICE", Variable_ID, ignore.case = TRUE)) %>%
            left_join(Revised_NonICE_Deployment, by = c("Year", "Region")) %>%
            mutate(Revised_Deployment  = NonICE_techshare * Revised_NonICE_Deployment) %>%
            select(Year, Region, Variable_ID, Revised_Deployment) %>%
            rename(Value = Revised_Deployment)


          ICE_deployment <- ICE_deployment %>%
            select(Year, Region, Variable_ID, Revised_Deployment) %>%
            rename(Value = Revised_Deployment)

          # Combine that into one dataframe called all_ldv_tech_scaled, include Units.
          all_ldv_tech_scaled <- rbind(ICE_deployment, Non_ICE_deployment) %>%
            mutate(Units = "Vehicles (thousands)")
  }


  # Combined all scaled pathways for each sector that was modified
  all_tech_scaled <- rbind(all_power_tech_scaled, all_ldv_tech_scaled)



  return(all_tech_scaled)
}



# Translate the users streched data into parameters.
paramaterise_pathway <- function(data_scenario_long, variableName) {

  # Initialize dataframe to store model parameters
  new_model_parameters <- data.frame()

  # # Dynamically find the start and end years based on column names
  # Pivot the data to long format
  data_scenario_long

  # Skip if all values are NA
  if(all(is.na(data_scenario_long$Value) | data_scenario_long$Value == 0)) return(NULL)

  # Calculate the earliest year with a non-zero, non-NA value and trim trailing zeros and NAs
  data_scenario_long <- data_scenario_long %>%
    filter(!is.na(Value) & Value != 0) %>%
    mutate(cumulative = rev(cumsum(rev(Value != 0 & !is.na(Value))))) %>%
    filter(cumulative > 0) %>%
    mutate(Year = as.numeric(Year))

  # Get the start year from the cleaned data
  start_year <- min(data_scenario_long$Year)

  # Optionally filter the data from this start year onwards
  valid_data <- data_scenario_long %>%
    filter(Year >= start_year)

  if(nrow(valid_data)< 4) next  # skip if there isn't many values

  # Set the results
  years <- valid_data$Year
  values <- valid_data$Value

  # This is a work around to get around cases where there is a plateau in a scenario that impacts the model creation logic. It adjust the value by an extremely small amount.
  # Adjust values slightly if they are the same as the previous value, skipping NA values
  for(i in 2:length(values)) {
    if(!is.na(values[i]) && !is.na(values[i + 1])) {
      if(values[i] == 0 || abs(values[i + 1] - values[i]) < 1e-05) {
        values[i] <- values[i] - 0.0001
      }
    }
  }

  # Segment the forecast
  segment_data(values, years) # growth_values growth_years etc.

  # Define V0 for both growth and decline phases
  V0_growth <- growth_values[1]
  V0_decline <- decline_values[1]

  # Define the objective function for fitting
  objective_function <- function(params, t, y) {
    K <- params[1]
    V0 <- params[2]
    r <- params[3]
    t0 <- params[4]
    y_hat <- modified_logistic_model_V0(t, K, V0, r, t0)
    return(y - y_hat)
  }

  # Set constraints for K based on variableName containing "percentage"
  if (grepl("percentage", variableName, ignore.case = TRUE)) {
    lower_bound_growth_K <- 0
    upper_bound_growth_K <- 1
    lower_bound_decline_K <- 0
    upper_bound_decline_K <- 1
  } else {
    lower_bound_growth_K <- 0
    upper_bound_growth_K <- Inf
    lower_bound_decline_K <- -Inf
    upper_bound_decline_K <- 0
  }

  # Fit the growth phase
  fit_growth <- tryCatch({
    nls.lm(par = c(max(growth_values), V0_growth, 0.1, median(growth_years)),
           fn = objective_function, t = growth_years, y = growth_values,
           lower = c(lower_bound_growth_K, min(growth_values), 0, min(growth_years)),
           upper = c(upper_bound_growth_K, max(growth_values), Inf, max(growth_years) + 50),
           control = nls.lm.control(maxiter = 1000))
  }, error = function(e) NULL)

  # Fit the decline phase ensuring K is negative
  fit_decline <- tryCatch({
    nls.lm(par = c(-1000, V0_decline, 0.1, median(decline_years)),
           fn = objective_function, t = decline_years, y = decline_values,
           lower = c(lower_bound_decline_K, min(decline_values), -1, min(decline_years)),
           upper = c(upper_bound_decline_K, max(decline_values), 1, max(decline_years) + 50),
           control = nls.lm.control(maxiter = 1000))
  }, error = function(e) NULL)

  # Store model parameters
  # Calculate metrics and store parameters directly
  if (!is.null(fit_growth)) {
    growth_params <- coef(fit_growth)
    new_model_parameters <- rbind(new_model_parameters, data.frame(
      Phase = "Growth",
      K = growth_params[1], V0 = V0_growth, r = growth_params[3], t0 = growth_params[4],
      R_squared = 1 - sum((growth_values - modified_logistic_model_V0(growth_years, growth_params[1], V0_growth, growth_params[3], growth_params[4]))^2) / sum((growth_values - mean(growth_values))^2),
      Start_year = min(growth_years), End_year = max(growth_years),
      t0_perc = (growth_params[4] - min(growth_years)) / (max(growth_years) - min(growth_years))
    ))
  }

  if (!is.null(fit_decline)) {
    decline_params <- coef(fit_decline)
    new_model_parameters <- rbind(new_model_parameters, data.frame(
      Phase = "Decline",
      K = decline_params[1], V0 = V0_decline, r = decline_params[3], t0 = decline_params[4],
      R_squared = 1 - sum((decline_values - modified_logistic_model_V0(decline_years, decline_params[1], V0_decline, decline_params[3], decline_params[4]))^2) / sum((decline_values - mean(decline_values))^2),
      Start_year = min(decline_years), End_year = max(decline_years),
      t0_perc = (decline_params[4] - min(decline_years)) / (max(decline_years) - min(decline_years))
    ))


  }

  new_model_parameters <- new_model_parameters %>%
  filter(!is.nan(new_model_parameters$R_squared))
  
  # Instead of just creating the dataframes, return them as a list at the end
  return(new_model_parameters)

}


# Define the function
segment_data <- function(values, years, duration = 3) {
  # Initialize default settings
  growth_values <<- numeric(0)
  growth_years <<- numeric(0)
  decline_values <<- numeric(0)
  decline_years <<- numeric(0)
  transition_year <<- numeric(0)
  trend_first <<- "none"
  end_year <<- NA

  # Determine initial trend
  initial_diff <- diff(values[1:(duration + 1)])  # Ensure this covers the correct number of elements
  if (all(initial_diff >= 0)) {
    trend_first <<- "growth"
  } else if (all(initial_diff < 0)) {  # Changed from 'else' to 'else if'
    trend_first <<- "decline"
  } else {
    trend_first <<- "growth"  # It's good to handle cases where neither condition is met
  }

  # Find transition point
  diffs <- diff(values)
  transition_point <- NULL

  #   if (length(diffs) < 2 * duration) {
  #     # Handle the case where there isn't enough data
  #     transition_point <- length(values)

  # Remove trailing NAs from diffs
  non_na_diffs <- na.omit(diffs)
  if (length(non_na_diffs) < length(diffs) && all(is.na(diffs[(length(non_na_diffs) + 1):length(diffs)]))) {
    diffs <- non_na_diffs
  }

  # Check for sufficient data length
  if (length(diffs) < 2 * duration) {
    # Handle the case where there isn't enough data
    transition_point <- length(values) # Set to NA or another indicator if insufficient data
  } else {
    # Loop through diffs with enough data to check conditions
    for (i in 1:(length(diffs) - 2 * duration)) {
      # Extract segments of differences
      current_diffs = diffs[i:(i + duration - 1)]
      next_diffs = diffs[(i + duration):(i + 2 * duration - 1)]

      # Check for the first condition of transition from growth to decline
      if (length(current_diffs) == duration && length(next_diffs) == duration) {  # Ensure segments are complete
        if (all(current_diffs > 0) && any(next_diffs < 0)) {
          transition_point <- i + which(next_diffs < 0)[1] + duration - 1
          break
        } else if (all(current_diffs < 0) && any(next_diffs > 0)) {
          transition_point <- i + which(next_diffs > 0)[1] + duration - 1
          break
        }
      }
    }
  }

  # Find end year
  # Assuming a simple approach where we look for the first zero or minimal change as the stopping condition
  # Initialize the variables
  end_index <- length(values) # Default to the last index if no specific stopping condition is found

  # Loop through the values to find the first zero or minimal change as the stopping condition
  for(i in 1:(length(values) - 1)) {
    if(values[i] == 0 || abs(values[i+1] - values[i]) < 1e-5) { # Change the threshold as needed
      end_index <- i
      break
    }
  }

  # Get the corresponding year using end_index
  end_year <<- years[end_index]

  if (!is.null(transition_point)) {
    # Determine growth and decline phases
    if (trend_first == "growth") {
      growth_values <<- values[1:transition_point]  # Adjusted to end at the transition point
      growth_years <<- years[1:transition_point]  # Adjusted accordingly
      decline_values <<- values[(transition_point + 1):end_index]  # Starts right after transition
      decline_years <<- years[(transition_point + 1):end_index]
    } else {
      decline_values <<- values[1:transition_point]  # Similarly adjusted
      decline_years <<- years[1:transition_point]
      growth_values <<- values[(transition_point + 1):end_index]
      growth_years <<- years[(transition_point + 1):end_index]
    }
    transition_year <<- years[transition_point]

  } else {
    # No transition point found, assume entire series is either growth or decline
    if (trend_first == "growth") {
      growth_values <<- values[1:end_index]
      growth_years <<- years[1:end_index]
    } else {
      decline_values <<- values[1:end_index]
      decline_years <<- years[1:end_index]
    }
    transition_year <<- end_year # transistion year is the last year of the series if there is no transition within the data series
  }

}

# Define the modified logistic growth model with V0 enforced at t=0
modified_logistic_model_V0 <- function(t, K, V0, r, t0) {
  P0 = K - (K - V0) * (1 + exp(-r * (t0 - min(t))))
  return(K + (P0 - K) / (1 + exp(r * (t - t0))))
}


# Function to adjust r so that the model equals user_threshold_value at user_forecast_year
adjust_r_for_threshold <- function(existing_model_result, user_forecast_year, user_threshold_value, tol = 1e-8) {
  # Extract parameters from the existing model result
  K         <- existing_model_result$K
  V0        <- existing_model_result$V0
  t0        <- existing_model_result$t0
  r_initial <- existing_model_result$r

  # Error function to minimize the difference between model value at user_forecast_year and user_threshold_value
  error_function <- function(r) {
    model_value_at_forecast <- modified_logistic_model_V0(user_forecast_year, K, V0, r, t0)
    return(abs(model_value_at_forecast - user_threshold_value))  # We want to minimize this difference
  }

  # Optimize r using the error function
  r_optimized <- optimize(error_function, interval = c(r_initial * 0.25, r_initial * 1.25), tol = tol)$minimum

  return(r_optimized)
}

# Function to adjust t0 so that the model equals user_threshold_value at user_forecast_year
adjust_t_for_threshold <- function(existing_model_result, user_forecast_year, user_threshold_value, tol = 1e-6) {
  # Extract parameters from the existing model result
  K          <- existing_model_result$K
  V0         <- existing_model_result$V0
  r          <- existing_model_result$r
  t0_initial <- existing_model_result$t0

  # Error function to minimize the difference between model value at user_forecast_year and user_threshold_value
  error_function <- function(t0) {
    model_value_at_forecast <- modified_logistic_model_V0(user_forecast_year, K, V0, r, t0)
    return(abs(model_value_at_forecast - user_threshold_value))  # We want to minimize this difference
  }

  # Optimize t0 using the error function
  t0_optimized <- optimize(error_function, interval = c(2020, 2100), tol = tol)$minimum

  return(t0_optimized)
}



recalibrate_parameters_on_t <- function(existing_model_result, user_pathway, revisedPolicyDate) {
  
  # Extract parameters from the existing model result
  K <- existing_model_result$K
  V0 <- existing_model_result$V0
  t0 <- existing_model_result$t0  # This has been incremented already
  r_initial <- existing_model_result$r  # Initial r
  
  # Define the modified logistic function with min_t as parameter
  modified_logistic_model_V0 <- function(t, K, V0, r, t0, min_t) {
    P0 <- K - (K - V0) * (1 + exp(-r * (t0 - min_t)))
    return(K + (P0 - K) / (1 + exp(r * (t - t0))))
  }
  
  # Get the time vector and original model values
  t_values <- user_pathway$Year
  min_t <- min(t_values)
  original_t0 <- existing_model_result$t0 - 1  # Before incrementing t0
  original_values <- modified_logistic_model_V0(t_values, K, V0, r_initial, original_t0, min_t)
  
  # Get the user_threshold_value
  user_threshold_value <- user_pathway$Value[user_pathway$Year == revisedPolicyDate]
  
  # Calculate the acceptable tolerance (5% of the user_threshold_value)
  tolerance <- 0.05 * user_threshold_value
  
  # Define the function whose root we want to find
  root_function <- function(r_new) {
    model_value_at_revised_date <- modified_logistic_model_V0(revisedPolicyDate, K, V0, r_new, t0, min_t)
    return(model_value_at_revised_date - user_threshold_value)
  }
  
  # Use uniroot to find the r_new that makes the model value equal to user_threshold_value
  lower_bound <- r_initial * 0.5
  upper_bound <- r_initial * 1.5
  
  # Check if the root exists within the bounds
  f_lower <- root_function(lower_bound)
  f_upper <- root_function(upper_bound)
  

  if (f_lower * f_upper > 0) {
    warning("Cannot find a suitable 'r' within the bounds that satisfies the constraint.")
    r_new <- r_initial  # Fallback to initial r
  } else {
    root_result <- uniroot(
      f = root_function,
      lower = lower_bound,
      upper = upper_bound,
      tol = .Machine$double.eps^0.5
    )
    r_new <- root_result$root
  }
  
  # Generate the adjusted pathway over the required time span
  adj_stretched_values <- modified_logistic_model_V0(t_values, K, V0, r_new, t0, min_t)
  
  adj_stretched_pathway <- data.frame(
    Year = t_values,
    Value = adj_stretched_values
  )
  
  # Update the model result with the new r
  updated_model_result <- existing_model_result
  updated_model_result$r <- r_new
  
  # Calculate the new model value at revisedPolicyDate
  new_value_at_revised_date <- modified_logistic_model_V0(revisedPolicyDate, K, V0, r_new, t0, min_t)
  
  # Calculate the percentage difference
  percent_difference <- abs(new_value_at_revised_date - user_threshold_value) / user_threshold_value * 100
  
  # Verify if the new value is within 5%
  if (percent_difference > 5) {
    warning("Adjusted model value at revisedPolicyDate is not within 5% of user_threshold_value.")
  }
  
  # Print verification information
  cat("Original r:", r_initial, "\n")
  cat("Adjusted r:", r_new, "\n")
  cat("Percentage Difference in at revised policy year:", percent_difference, "%\n")
  
  # Return the recalibrated results along with the verification data
  return(list(
    adj_stretched_pathway = adj_stretched_pathway,
    updated_model_result = updated_model_result,
    percent_difference = percent_difference,
    new_value_at_revised_date = new_value_at_revised_date,
    user_threshold_value = user_threshold_value
  ))
}


recalibrate_parameters_on_r <- function(existing_model_result, user_pathway, revisedPolicyDate) {
  
  # Extract parameters from the existing model result
  K <- existing_model_result$K
  V0 <- existing_model_result$V0
  r <- existing_model_result$r  # Updated r after increment
  t0_initial <- existing_model_result$t0  # Initial t0
  min_t <- min(user_pathway$Year, na.rm = TRUE)  # Minimum time from user pathway
 
  cat("Debug - Input Parameters:\n")
  print(existing_model_result)
  # cat("K =", K, ", V0 =", V0, ", r =", r, ", t0_initial =", t0_initial, "\n")
  
  
  # Get the user_threshold_value dynamically from the user_pathway at the revisedPolicyDate
  user_threshold_value <- user_pathway$Value[user_pathway$Year == revisedPolicyDate]
  
  # Define the logistic function
  modified_logistic_model_V0 <- function(t, K, V0, r, t0, min_t) {
    P0 <- K - (K - V0) * (1 + exp(-r * (t0 - min_t)))
    return(K + (P0 - K) / (1 + exp(r * (t - t0))))
  }
  
  # Objective function to minimize (adjusting t0)
  adjust_t0_for_point <- function(t0_adjusted) {
    value <- modified_logistic_model_V0(revisedPolicyDate, K, V0, r, t0_adjusted, min_t)
    return((value - user_threshold_value)^2)
  }
  
  # Dynamic search space for t0
  t0_search_range <- c(t0_initial - 10, t0_initial + 10)
  
  # Optimize t0
  result <- optimize(
    f = adjust_t0_for_point,
    interval = t0_search_range
  )
  
  # Get the optimized t0
  t0_optimized <- result$minimum
  
  # Generate the adjusted pathway using updated r and optimized t0
  t_values <- user_pathway$Year
  adj_stretched_values <- modified_logistic_model_V0(t_values, K, V0, r, t0_optimized, min_t)
  adj_stretched_pathway <- data.frame(
    Year = t_values,
    Value = adj_stretched_values
  )
  
  # Update the model result with the new parameters
  updated_model_result <- existing_model_result
  updated_model_result$t0 <- t0_optimized
  updated_model_result$r <- r
  
  # Calculate the new model value at revisedPolicyDate
  new_value_at_revised_date <- modified_logistic_model_V0(revisedPolicyDate, K, V0, r, t0_optimized, min_t)
  
  # Calculate the percentage difference
  percent_difference <- abs(new_value_at_revised_date - user_threshold_value) / abs(user_threshold_value) * 100
  
  # Verify if the new value is within 5%
  if (percent_difference > 5) {
    warning("Adjusted model value at revisedPolicyDate is not within 5% of user_threshold_value.")
  }
  
  # Print verification information
  
  cat("Original t0:", t0_initial, "\n")
  cat("Adjusted t0:", t0_optimized, "\n")
  cat("Percentage Difference in at revised policy year:", percent_difference, "%\n")
  
  # Return the adjusted pathway and updated model parameters
  return(list(
    adj_stretched_pathway = adj_stretched_pathway,
    updated_model_result = updated_model_result,
    percent_difference = percent_difference,
    new_value_at_revised_date = new_value_at_revised_date,
    user_threshold_value = user_threshold_value
  ))
}




downscale_plot_data <- function(userDataForPlot, variable_id, region, units) {
  
  scaling_factor <- 1  # Initialize scaling_factor
  
  if(units != "%") {
    
    SEAO_regions <- c(
      "Democratic People's Republic of Korea", "Brunei Darussalam", "Cambodia",
      "Lao People's Democratic Republic", "Malaysia", "Myanmar", "New Zealand",
      "Philippines", "Singapore", "Chinese Taipei", "Thailand", "Viet Nam"
    )
    
    if(region %in% SEAO_regions && variable_id == "Electricity generation_Clean Percentage_Power") {
      
      cat("Region is in SEAO \n")
      # Read in country level data
      SEAO_Generation_Share <- read.csv(file.path(data_dir, "SEAO_Power_Data.csv"), check.names = FALSE)
      
      scaling_factor <- SEAO_Generation_Share %>%
        filter(
          Country == region,
          Sub_variable_class_1 == "Clean Power",
          case_when(
            units == "MW" ~ Variable == "Capacity",
            units == "TWh" ~ Variable == "Generation",
            TRUE ~ TRUE # Default to no additional filter if `units` is not MW or TWh
          )
        ) %>%
        pull(Value) %>%
        .[1]  # Get the first value
      
      cat("\nClean power scaling factor is: \n")
      print(scaling_factor)
      
    } else if (region %in% SEAO_regions && variable_id == "GHG emissions_CO2_Total") {
      
      # Read in country level data
      SEAO_Emissions_Share <- read.csv(file.path(data_dir, "SEAO_Emissions_Share.csv"), check.names = FALSE)
      
      # Extract the first value from the 'Region_CO2_Perc' column for the specified region
      scaling_factor <- SEAO_Emissions_Share %>%
        filter(Country == region) %>%
        pull(Region_CO2_Perc) %>%
        .[1]  # Get the first value
      
      cat("\nEmissions scaling factor is: \n")
      print(scaling_factor)
      
    } else {
      cat("\nNo Scaling\n")
    }
  }
  
  downscaled_userDataForPlot <- userDataForPlot %>%
    mutate(Value = Value * scaling_factor)
  
  return(downscaled_userDataForPlot)
}

