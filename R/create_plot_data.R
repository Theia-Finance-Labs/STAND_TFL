## Code to create data series fit for plotting. Apply log growth model to parameters to generate the time series data
## testing code is written below the function definitions

# Define the modified logistic growth model with V0 enforced at t=0
modified_logistic_model_V0 <- function(t, K, V0, r, t0) {
  P0 = K - (K - V0) * (1 + exp(-r * (t0 - min(t))))
  return(K + (P0 - K) / (1 + exp(r * (t - t0))))
}

# Define function to plot the annual values using the growth model
create_plot_data <- function(params) {
   
  # # # # # # # # # # manual test code
  #  params <- model_parameters %>%
  #    filter(Variable_ID == "New deployment_Vehicles_ICE_Transport_Cars", Region == "AUS")  #  #GHG emissions_CO2_Total #Capacity_Electricity generation_Clean Percentage_Power
  # # # # # # # #
 
      #params <- params[order(params$End_year), ]
  
  # Check if params is a valid data frame and has End_year column
  if (!is.null(params) && nrow(params) > 0 && "End_year" %in% names(params)) {
    
    # Check if End_year is a valid vector
    if (!is.null(params$End_year) && any(!is.na(params$End_year))) {
      params <- params[order(params$End_year, na.last = TRUE), ]  # Handle NA if necessary
    } else {
      stop("Error: `End_year` column is NULL or contains only NA values.")
    }
    
  } else {
    stop("Error: `params` is NULL, empty, or does not contain `End_year` column.")
  }
  
      first_phase_params <- params[1, ]
      year_vector <- 2022:2100
      
      # Generate predictions for the first phase for all years
      first_phase_predictions <- modified_logistic_model_V0(year_vector, first_phase_params$K, first_phase_params$V0, first_phase_params$r, first_phase_params$t0)
      combined_predictions_df <- data.frame(Year = year_vector, Fitted_model = first_phase_predictions)
      
      # Values cannot be negative for some techs. If they are, set the carry value K to zero.
      if(any(grepl("New deployment|Capacity", params$Variable_ID, ignore.case = TRUE)) & any(combined_predictions_df$Fitted_model < 0)) {
        first_phase_predictions <- modified_logistic_model_V0(year_vector, 0, first_phase_params$V0, first_phase_params$r, first_phase_params$t0)
        combined_predictions_df <- data.frame(Year = year_vector, Fitted_model = first_phase_predictions)
      }
      
      if (nrow(params) > 1) {
        second_phase_params <- params[2, ]
        # Generate predictions for the second phase if parameters exist
        second_phase_year_vector <- second_phase_params$Start_year:2100
        second_phase_predictions <- modified_logistic_model_V0(second_phase_year_vector, second_phase_params$K, second_phase_params$V0, second_phase_params$r, second_phase_params$t0)
        
        # Values cannot be negative for some techs. If they are, set the carry value K to zero.
        if(any(grepl("New deployment|Capacity", params$Variable_ID, ignore.case = TRUE)) & any(combined_predictions_df$Fitted_model < 0)) {
          second_phase_year_vector <- second_phase_params$Start_year:2100
          second_phase_predictions <- modified_logistic_model_V0(second_phase_year_vector, 0, second_phase_params$V0, second_phase_params$r, second_phase_params$t0)
        }
        
        
        # Add the second phase predictions to the combined dataframe
        combined_predictions_df <- merge(combined_predictions_df, data.frame(Year = second_phase_year_vector, Second_model = second_phase_predictions), by = "Year", all = TRUE)
        
        # Apply smoothing for the transition
        transition_start <- first_phase_params$End_year - 1
        transition_end <- first_phase_params$End_year + 1
        
        combined_predictions_df$Weight <- ifelse(combined_predictions_df$Year >= transition_start & combined_predictions_df$Year <= transition_end, 
                                                 (combined_predictions_df$Year - transition_start) / (transition_end - transition_start), 
                                                 ifelse(combined_predictions_df$Year < transition_start, 0, 1))
        
        combined_predictions_df$Fitted_model <- ifelse(is.na(combined_predictions_df$Second_model), 
                                                       combined_predictions_df$Fitted_model, 
                                                       (1 - combined_predictions_df$Weight) * combined_predictions_df$Fitted_model + combined_predictions_df$Weight * combined_predictions_df$Second_model)
        
        # Remove the temporary Second_model and Weight columns
        combined_predictions_df <- combined_predictions_df[, c("Year", "Fitted_model")]
      }
  
   
    # If net-zero emissions, complete predictions, else # Ensure values are non-negative and complete the predictions
        if(!is.null(params) && "Variable_ID" %in% colnames(params) && nrow(params) > 0) {
           if(!is.null(params$Variable_ID[1]) && params$Variable_ID[1] == "GHG emissions_CO2_Total") {
    combined_predictions_df <- combined_predictions_df %>%
      complete(Year = year_vector) %>%
      fill(Fitted_model, .direction = "down")
        }
        } else {
          combined_predictions_df <- combined_predictions_df %>%
            mutate(Fitted_model = pmax(Fitted_model, 0)) %>%
            complete(Year = year_vector) %>%
            fill(Fitted_model, .direction = "down")
        }
    
   
    return(combined_predictions_df)
 
}


  
  
  # # Sort params by End_year to ensure the correct order of growth and decline phases
  # params <- params[order(params$End_year), ]
  # 
  # # Extract parameters for both phases
  # first_phase_params <- params[1, ]
  # second_phase_params <- params[2, ]
  # 
  # # Example year vector (update according to your specific years)
  # year_vector <- 2022:2050  # Define min_year and max_year
  # 
  # #print(paste(first_phase_params$K, first_phase_params$V0, first_phase_params$r, first_phase_params$t0))
  # 
  # # Generate predictions for the first phase up to its End_year
  # # first_phase_years <- year_vector[year_vector <= first_phase_params$End_year] # this is redundant now
  # first_phase_predictions <- modified_logistic_model_V0(year_vector, first_phase_params$K, first_phase_params$V0, first_phase_params$r, first_phase_params$t0) # generate model for all years first, then trim later if there is a second phase
  # 
  # # place them in a dataframe
  # first_phase_df <- data.frame(Year = year_vector, Fitted_model = first_phase_predictions) # generate model for all years first, then trim later if there is a second phase
  # 
  # # Generate predictions for the second phase starting from its beginning until its End_year
  # second_phase_years <- year_vector[year_vector <= 2050 & year_vector > first_phase_params$End_year]
  # #second_phase_years <- year_vector[year_vector <= second_phase_params$End_year & year_vector > first_phase_params$End_year] #this version stops it and keeps it constant if there is a second asymptote
  # 
  # # Check if a second phases exists, and then proceed, otherwise skip
  # if (length(second_phase_years) != 0 & !all(is.na(second_phase_years))) {
  #   second_phase_predictions <- modified_logistic_model_V0(second_phase_years, second_phase_params$K, second_phase_params$V0, second_phase_params$r, second_phase_params$t0)
  # 
  #   # Convert predictions and years into dataframes
  #   second_phase_df <- data.frame(Year = second_phase_years, Fitted_model = second_phase_predictions)
  #   # Combine the two dataframes
  #   first_phase_df <- first_phase_df %>%
  #     filter(Year <= first_phase_params$End_year)
  #   combined_predictions_df <- rbind(first_phase_df, second_phase_df)
  # 
  # } else {
  #   combined_predictions_df <- first_phase_df
  # }
  # 
  # # Assuming you want to order the combined dataframe by Year
  # combined_predictions_df <- combined_predictions_df %>% arrange(Year)
  # 
  # # Clean results if values go negative
  # combined_predictions_df <- combined_predictions_df %>%
  #   mutate(across(c(Fitted_model), ~ pmax(., 0)))
  # 
  # #Insure that the data goes out until 2050.
  # combined_predictions_df <- combined_predictions_df %>%
  #   complete(Year = year_vector) %>%
  #   fill(Fitted_model, .direction = "down")
# 
#   return(combined_predictions_df) # Return the prepared data
# }
# 



# 
## Test function
# # Read data for initial code testing
# all_params <- read.csv(file.path(data_dir, "STAND_model_parameters.csv"))
# 
# variables <- unique(as.character(data$Variable_ID))
# unique_regions <- unique(All_model_parameters$Region)
# all_forecasts <- data.frame()
# 
# for(variableName in variables) {
# 
#   tech_model_params <- all_params %>%
#     filter(Variable_ID == variableName)
# 
#   for(region in unique_regions) {
# 
#     region_scenarios <- unique(tech_model_params$Scenario[tech_model_params$Region == region])
# 
#     for(scenario in region_scenarios) {
#       params <- subset(tech_model_params, Region == region & Scenario == scenario)
# 
#       params$t0 <- params$t0_simple
#       params$t0_perc <- params$t0_perc_simple
# 
#       forecast <- create_plot_data(params) %>%
#         mutate(Region = region,
#                Variable_ID = variableName)
# 
#       all_forecasts <- rbind(all_forecasts,forecast)
# 
#     }
#   }
# }
# 
# 
# #plot results
# transport_variables <- as.data.frame(variables) %>%
#   filter(grepl("ICE", variables) & (grepl("Cars", variables) | grepl("Trucks", variables))) %>%
#   filter(!grepl("Retirements", variables))
# 
# key_variables<- as.data.frame(variables) %>%
#   filter(!grepl("Transport", variables) & (!grepl("Total_Emissions_Industry", variables))) %>%
#   rbind(transport_variables)
# 
# plot_data <- all_forecasts %>%
#   filter(Variable_ID %in% key_variables$variables) %>%
#   rename("Value" = "Fitted_model") %>%
#   mutate(Model = "Fitted_Data")
# 
# 
# #retrieve original data for validation
# original_data <- read.csv(file.path(data_dir, "Processed_Value_Drivers.csv"), check.names = FALSE) %>%
#   select(-'2020', -'2021') %>%
#   filter(Variable_ID %in% key_variables$variables) %>%
#   pivot_longer(cols = `2022`:`2050`, names_to = "Year", values_to = "Value",
#                values_transform = list(Value = as.numeric)) %>%
#   mutate(Year = as.numeric(Year), Model = "Original_Data") %>%
#   select(c("Region", "Variable_ID", "Year", "Value", "Model", "Units"))
# 
# ## Add Units to parameter file
# Units <- original_data %>%
#   select(Variable_ID, Units) %>%
#   unique()
# 
# plot_data <- plot_data %>%
#   left_join(Units, by = c("Variable_ID"))
# 
# # Inspect which variables where not run
# # Find elements in 'data_variables' not present in 'variables'
# missing_in_variables <- setdiff(original_data$Variable_ID, plot_data$Variable_ID)
# # Print the missing elements
# print(missing_in_variables)
# 
# plot_data <- rbind(plot_data, original_data)
# 
# 
# # check fits
# 
# data_wide <- plot_data %>%
#   pivot_wider(names_from = Model, values_from = Value, names_prefix = "Value_")
# 
# Fit <- data_wide %>%
#   group_by(Variable_ID, Region) %>%
#   summarize(
#     R_squared = {
#       safe_model <- tryCatch({
#         model <- lm(Value_Original_Data ~ Value_Fitted_Data, data = cur_data())
#         summary(model)$r.squared
#       }, error = function(e) NA)  # Return NA in case of an error
#       safe_model
#     },
#     Net_Difference = sum(abs(Value_Original_Data - Value_Fitted_Data), na.rm = TRUE),
#     .groups = 'drop'
#   )
# 
# Bad_Fit <- Fit %>%
#   filter(R_squared < 0.9)
# 
# Decent_Models = paste(round((1-nrow(Bad_Fit)/nrow(Fit))*100,2),"%")
# Decent_Models
# 
# 
# # Call the plot:
# 
# # Initialize an empty list to store plots
# plots <- list()
# 
# #testplot variables
# variableName  <- "New deployment_Electricity generation_Clean_Power"
# region = "CHN"
# 
# 
# for(variableName in variables) {
# 
#   plot_data_tech <- plot_data[plot_data$Variable_ID == variableName, ]
# 
#   for(region in unique_regions) {
# 
#     plot_data_region <- plot_data_tech[plot_data_tech$Region== region, ]
# 
#     # Skip plot if no values
#     if(nrow(plot_data_region) == 0) next
# 
#     # Generate the plot
#     p <- tryCatch({
#       ggplot(plot_data_region, aes(x = Year)) +
#         geom_point(data = plot_data_region %>% filter(Model == "Original_Data"),
#                    aes(y = Value, color = "Original_Data"),
#                    shape = 16, size = 3, alpha = 0.5) +
#         geom_line(data = plot_data_region %>% filter(Model == "Fitted_Data"),
#                   aes(y = Value, color = "Fitted_Data"),
#                   linetype = "solid", size = 1) +
#         scale_color_manual(values = c("Original_Data" = "#3498db", "Fitted_Data" = "#e67e22")) +
#         scale_x_continuous(limits = c(2020, 2050), breaks = seq(2020, 2050, by = 10)) +
#         labs(title = paste("Region: ", unique(plot_data_region$Region)),
#              subtitle = unique(plot_data_region$Variable_ID),
#              y = unique(plot_data_region$Units),  # Set y-axis label dynamically
#              color = "Model",
#              linetype = "Model Type") +
#         theme_minimal() +
#         theme(plot.title = element_text(size = 14, face = "bold"),
#               plot.subtitle = element_text(size = 12),
#               legend.position = "bottom",
#               legend.title = element_blank())
#       }, error = function(e) {
#       cat("Error in plotting for region:", region, "\n")
#       return(NULL)  # Returning NULL or a similar approach to handle the error
#     })
# 
#     # Print the plot
#     print(p)
# 
#     plots[[length(plots) + 1]] <- p
#   }
# }
# 
# plots
# 
