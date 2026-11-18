# Calculates the impact in terms of emission change from the user adjusted policy dates

# # # # # # # # # example variable and region
# varId = "Electricity generation_Clean Percentage_Power" # Electricity generation_Coal Percentage_Power Capacity_Electricity generation_Clean Percentage_Power  GHG emissions_CO2_Total
# region = "China"
# user_forecast_year = 2065
# threshold = 0.97
# threshold_criteria = ">="
# IPR_Forecast_Year = 2050
# 
# user_pathways <-  create_pathways(varId, region, user_forecast_year, model_parameters, "NA")


# if(varId == "GHG emissions_CO2_Total" ) {
# user_params <- find_nearest_NZ_model(user_forecast_year, varId, region, threshold, threshold_criteria) %>%
#   rename("Variable_ID" = "Variable")
# } else {
# user_params <- find_nearest_model_rate(user_forecast_year, varId, region, threshold, threshold_criteria) %>%
#   rename("Variable_ID" = "Variable")
# }


## Start of function


calculate_impact  <- function(user_forecast_year, varId, region, user_pathways) {

  All_Data <- data.frame()

  # Initialize final results dataframe
  Impact_Summary <- data.frame(
    Total_Emissions_Change = numeric(0),
    Emissions_Change_Perc_Sector = numeric(0),
    Emissions_Change_Perc_Region = numeric(0),
    Emissions_Change_Perc_Global = numeric(0),
    IPR_Policy_Year = numeric(0),
    Policy_Year = numeric(0),
    Policy_Year_Change = numeric(0),
    User_Max_CARG = numeric(0),
    FPS_Max_CARG = numeric(0)
  )

  # Attempt to get time series user data
  # User_data <- try(
  #   create_pathways(varId, region, user_forecast_year, model_parameters, "NA"),
  #   silent = TRUE
  # )

  User_data <- user_pathways

  # Get time series FPS Data
  Policy_data <- Policies %>%
    filter(Variable_ID == varId & Region == region)

  IPR_Forecast_Year <- Policy_data$Forecast_Year
  FPS_data <- try(
    create_pathways(varId, region, IPR_Forecast_Year,  model_parameters, "NA"),
    silent = TRUE
  )

  # Record threshold data
  threshold <- Policy_data$Policy_Threshold[1]
  threshold_criteria <- Policy_data$Threshold_Criteria[1]

  # Calculate the difference in policy year
  Policy_Year_Change <- user_forecast_year - IPR_Forecast_Year

  # # Define a function to calculate and return the maximum annual CAGR
  max_cagr <- function(V) {
    if (is.null(V) || length(V) < 2) {
      return(NA)
    }
    annual_CAGR <- numeric(length(V) - 1)
    for (i in 2:length(V)) {
      if (V[i - 1] == 0) {
        annual_CAGR[i - 1] <- NA
      } else {
        annual_CAGR[i - 1] <- (V[i] / V[i - 1]) - 1
      }
    }
    if (all(is.na(annual_CAGR))) {
      return(NA)
    } else {
      return(round(max(annual_CAGR, na.rm = TRUE), 3))
    }
  }

  # Calculate maximum CAGR
  if (nrow(User_data) > 1) {
    user_max_CARG <- max_cagr(User_data$Value)
  } else {
    user_max_CARG <- NA
  }

  if (nrow(FPS_data) > 1) {
    FPS_max_CARG <- max_cagr(FPS_data$Value)
  } else {
    FPS_max_CARG <- NA
  }


# Get emissions data relevant geography, first get parameters, then create data series

  # Fetch params
  if(varId == "GHG emissions_CO2_Total" ) {
    Region_Emissions <- FPS_data %>%
      rename("Region_Emissions" =  "Value")

  } else { #need the GHG as a time series that goes out till 2100, thus we take the parametrised version and plot out to 2100
    region_emission_params <- model_parameters %>%
      filter(Variable_ID == "GHG emissions_CO2_Total" & Region == region)
    Region_Emissions <- create_plot_data(region_emission_params) %>%
      rename("Region_Emissions" =  "Fitted_model")
  }

 #Using the emissions from FPS... for energy only
  Global_Emissions <- read.csv(file.path(data_dir, "global_emissions.csv"), check.names = FALSE)


  #### Net-Zero target

  if(varId == "GHG emissions_CO2_Total") {

    User_data <- User_data %>%
      rename("User_Sector_Emissions" =  "Value")

    All_Data <- User_data %>%
      left_join(Region_Emissions, by = c("Year")) %>%
      left_join(Global_Emissions, by = c("Year")) %>%
      mutate(Emissions_Change = User_Sector_Emissions - Region_Emissions,
                                 Sector_Emissions = NA)
        }

#### Phase out of all unabated coal

    if(varId == "Electricity generation_Coal Percentage_Power") {

      # Get power sector emissions
      sector_emission_params <- model_parameters %>%
      filter(Variable_ID == "GHG emissions_CO2_Power" & Region == region)

       Sector_Emissions <- create_plot_data(sector_emission_params) %>%
       rename("Sector_Emissions" =  "Fitted_model")

      # Get data from fps and user, this is coal power percentage in this case
      FPS_data <- FPS_data %>%
          rename("Coal_Perc_FPS" =  "Value")

      User_data <- User_data %>%
        rename("Coal_Perc_User" =  "Value")

      ## Read in region grid intensities

      Non_coal_params <- model_parameters %>%
         filter(Variable_ID == "Electricity generation_Non Coal Grid Intensity_Power" & Region == region)

      Non_coal_intensity <- create_plot_data(Non_coal_params) %>%
        rename("Non_Coal_Grid_Intensity" =  "Fitted_model")

          # Combine data and calculate impact on total emissions
        All_Data <- FPS_data %>%
          left_join(User_data, by = "Year") %>%
          left_join(Non_coal_intensity, by = "Year") %>%
          left_join(Sector_Emissions, by = "Year") %>%
          left_join(Region_Emissions, by = "Year") %>%
          left_join(Global_Emissions, by = "Year") %>%
          mutate(Coal_Intensity = 935,
                 Coal_Intensity_FPS = Coal_Intensity *(Coal_Perc_FPS),
                 Non_Coal_Intensity_FPS = Non_Coal_Grid_Intensity *(1-Coal_Perc_FPS),
                 Total_Intensity_FPS = Coal_Intensity_FPS + Non_Coal_Intensity_FPS,
                 Coal_Intensity_User = Coal_Intensity *(Coal_Perc_User),
                 Non_Coal_Intensity_User = Non_Coal_Grid_Intensity *(1-Coal_Perc_User),
                 Total_Intensity_User = Coal_Intensity_User + Non_Coal_Intensity_User,
                 Intensity_Change_Perc = (Total_Intensity_User-Total_Intensity_FPS)/Total_Intensity_FPS,
                 Emissions_Change = Intensity_Change_Perc*Sector_Emissions)
    }


#### Clean power
    if(varId == "Electricity generation_Clean Percentage_Power") {

      # Get power sector emissions and create series out to 2100

       sector_emission_params <- model_parameters %>%
          filter(Variable_ID == "GHG emissions_CO2_Power" & Region == region)

        Sector_Emissions <- create_plot_data(sector_emission_params) %>%
          rename("Sector_Emissions" =  "Fitted_model")

        # Now modify that series assuming that 97% assume non-clean is zero emissions.

        user_sector_Emissions <- create_pathways("GHG emissions_CO2_Power", region, user_forecast_year, model_parameters, "NA") %>%
          rename("User_Sector_Emissions" =  "Value")

        All_Data <- user_sector_Emissions %>%
          left_join(Sector_Emissions, by = "Year") %>%
          left_join(Region_Emissions, by = c("Year")) %>%
          left_join(Global_Emissions, by = c("Year")) %>%
          mutate(Emissions_Change = User_Sector_Emissions - Sector_Emissions)
      }


##### Transport
### Cars

if(varId == "New deployment_Percent_ICE_Transport_Cars") {

  # Get Car's sector emissions and create series out to 2100

  # sector_emission_params <- model_parameters %>%
  #   filter(Variable_ID == "GHG emissions_CO2_Transport_Cars" & Region == region)
  #
  # Sector_Emissions <- create_plot_data(sector_emission_params) %>%
  #   rename("Sector_Emissions" =  "Fitted_model")

  # Now modify that series assuming that 97% assume non-clean is zero emissions.

  Sector_Emissions  <- create_pathways("GHG emissions_CO2_Transport_Cars", region, IPR_Forecast_Year, model_parameters, "NA")%>%
    rename("Sector_Emissions" =  "Value")

  user_sector_Emissions <- create_pathways("GHG emissions_CO2_Transport_Cars", region, user_forecast_year, model_parameters, "NA")%>%
    rename("User_Sector_Emissions" =  "Value")

  All_Data <- user_sector_Emissions %>%
    left_join(Sector_Emissions, by = "Year") %>%
    left_join(Region_Emissions, by = c("Year")) %>%
    left_join(Global_Emissions, by = c("Year")) %>%
    mutate(Emissions_Change = User_Sector_Emissions - Sector_Emissions)
}


# Calculate cumulative impact for which VarId/Policy was run

  #Filter the data to only sum the impact until the latest date IPR policy or user policy date:
  All_Data_2100 <- All_Data

  # filter by max polciy date to avoid weird stuff from post 2050 curves
  # All_Data <- All_Data %>%
  #   filter(Year <= max(c(IPR_Forecast_Year, user_params$Policy_year), na.rm = TRUE))

  # Safely handle Total_Emissions_Change
  Total_Emissions_Change <- ifelse(is.null(All_Data$Emissions_Change) || length(All_Data$Emissions_Change) == 0, NA, sum(All_Data$Emissions_Change, na.rm = TRUE))
  
  # Calculate the Emissions_Change_Perc_Sector and handle NA values
  Emissions_Change_Perc_Sector <- if (sum(All_Data$Sector_Emissions, na.rm = TRUE) == 0 || is.na(sum(All_Data$Sector_Emissions))) {
    NA
  } else {
    round(sum(All_Data$Emissions_Change, na.rm = TRUE) / sum(All_Data$Sector_Emissions, na.rm = TRUE), 3)
  }

  # Check for zero or missing values in Region_Emissions and Global_Emissions
  Emissions_Change_Perc_Region <- if (is.null(All_Data$Region_Emissions) || length(All_Data$Region_Emissions) == 0 || sum(All_Data$Region_Emissions, na.rm = TRUE) == 0) {
    NA
  } else {
    round(Total_Emissions_Change / abs(sum(All_Data$Region_Emissions, na.rm = TRUE)), 3)
  }

  Emissions_Change_Perc_Global <- if (is.null(All_Data$Global_Emissions) || length(All_Data$Global_Emissions) == 0 || sum(All_Data$Global_Emissions, na.rm = TRUE) == 0) {
    NA
  } else {
    round(sum(All_Data$Emissions_Change, na.rm = TRUE) / sum(All_Data$Global_Emissions, na.rm = TRUE), 3)
  }

  # Handle Max_CARG values
  user_max_CARG <- ifelse(exists("user_max_CARG") && !is.null(user_max_CARG), user_max_CARG, NA)
  FPS_max_CARG <- ifelse(exists("FPS_max_CARG") && !is.null(FPS_max_CARG), FPS_max_CARG, NA)



  # Construct the Impact_Summary safely
  Impact_Summary <- data.frame(
    Total_Emissions_Change = round(Total_Emissions_Change, 3),
    Emissions_Change_Perc_Sector = Emissions_Change_Perc_Sector,
    Emissions_Change_Perc_Region = Emissions_Change_Perc_Region,
    Emissions_Change_Perc_Global = Emissions_Change_Perc_Global,
    Policy_Year = user_forecast_year,
    IPR_Policy_Year = IPR_Forecast_Year,
    Policy_Year_Change = Policy_Year_Change,
    User_Max_CARG = user_max_CARG,
    FPS_Max_CARG = FPS_max_CARG
  )

return(Impact_Summary)

}





