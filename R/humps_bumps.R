create_bumps_data <- function(params, disrupt_type, disrupt_start, disrupt_duration, custom_rate, threshold, threshold_criteria) {
  # Set end dates for disruption and blend periods
  blend_end <- params$End_year
  disrupt_end <- disrupt_start + disrupt_duration
  
  # Generate Original and adjusted curves
  series_data <- create_plot_data(params)
  
  if (is.null(series_data) || nrow(series_data) == 0) {
    stop("series_data is NULL or empty.")
  }
  
  V1 <- series_data$Fitted_model
  t <- series_data$Year
  
  # Ensure there are no missing values in t and V1
  if (any(is.na(t)) || any(is.na(V1))) {
    stop("t or V1 contains missing values.")
  }
  
  # Calculate annual CAGR for each year
  annual_CAGR <- numeric(length(V1))
  for (i in 2:length(V1)) {
    annual_CAGR[i] <- (V1[i] / V1[i - 1]) - 1
  }
  
  # Adjust CAGR values based on disrupt_type
  modified_CAGR <- annual_CAGR  # Copy annual CAGR to modified_CAGR
  
  if (disrupt_type == "constant") {
    modified_CAGR[t >= disrupt_start & t <= disrupt_end] <- modified_CAGR[which(t == disrupt_start)]
  } else if (disrupt_type == "custom") {
    modified_CAGR[t >= disrupt_start & t <= disrupt_end] <- custom_rate
  } else if (disrupt_type == "constant_volume") {
    modified_CAGR[t >= disrupt_start & t <= disrupt_end] <- 0  # No growth/decline
  }
  
  # Calculate function values using modified CAGR
  Modified_V1 <- numeric(length(t))
  Modified_V1[1] <- V1[1]  # Initial value
  
  for (i in 2:length(t)) {
    Modified_V1[i] <- Modified_V1[i-1] * (1 + modified_CAGR[i])
  }
  
  # Create the blended curve
  Blend_V1 <- numeric(length(t))
  Blend_V1[t <= disrupt_end] <- Modified_V1[t <= disrupt_end]
  
  # Sigmoid blending function
  sigmoid_blend <- function(x, start, end) {
    k <- 10 / (end - start)  # Control the steepness of the transition
    return(1 / (1 + exp(-k * (x - (start + (end - start) / 2)))))
  }
  
  # Calculate blend ratios using sigmoid function
  blend_range <- t[t > disrupt_end & t <= blend_end]
  blend_ratio <- sigmoid_blend(blend_range, disrupt_end, blend_end)
  
  # Apply sigmoid blending
  Blend_V1[t > disrupt_end & t <= blend_end] <- (1 - blend_ratio) * Modified_V1[t > disrupt_end & t <= blend_end] + 
    blend_ratio * V1[t > disrupt_end & t <= blend_end]
  
  # After blending period, follow the Original_V1
  Blend_V1[t > blend_end] <- V1[t > blend_end]
  
  # Create dataframe with t, Original_V1, Original_CAGR, Modified_CAGR, Modified_V1, and Blend_V1
  df <- data.frame(t = t, Original_V1 = V1, Original_CAGR = annual_CAGR, Modified_CAGR = modified_CAGR, Modified_V1 = Modified_V1, Blend_V1 = Blend_V1)
  
  # Recalculate Blend_CAGR
  recalculate_blend_cagr <- function(Blend_V1) {
    Blend_CAGR <- numeric(length(Blend_V1))
    for (i in 2:length(Blend_V1)) {
      Blend_CAGR[i] <- (Blend_V1[i] / Blend_V1[i - 1]) - 1
    }
    return(Blend_CAGR)
  }
  
  Blend_CAGR <- recalculate_blend_cagr(Blend_V1)
  df$Blend_CAGR <- Blend_CAGR
  
  # Adjust the blend_ratio to meet threshold criteria
  if (threshold_criteria == "<=") {
    for (i in which(t > disrupt_end & t <= blend_end)) {
      while (!is.na(Blend_V1[i]) && Blend_V1[i] > Blend_V1[i - 1] && blend_ratio[which(blend_range == t[i])] > 0) {
        blend_idx <- which(blend_range == t[i])
        blend_ratio[blend_idx] <- blend_ratio[blend_idx] - 0.01
        Blend_V1[i] <- (1 - blend_ratio[blend_idx]) * Modified_V1[i] + blend_ratio[blend_idx] * V1[i]
        Blend_CAGR <- recalculate_blend_cagr(Blend_V1)
        df$Blend_CAGR <- Blend_CAGR
        if (Blend_V1[i] <= Blend_V1[i - 1]) break
      }
    }
  } else if (threshold_criteria == ">=") {
    for (i in which(t > disrupt_end & t <= blend_end)) {
      while (!is.na(Blend_V1[i]) && Blend_V1[i] < Blend_V1[i - 1] && blend_ratio[which(blend_range == t[i])] < 1) {
        blend_idx <- which(blend_range == t[i])
        blend_ratio[blend_idx] <- blend_ratio[blend_idx] + 0.01
        Blend_V1[i] <- (1 - blend_ratio[blend_idx]) * Modified_V1[i] + blend_ratio[blend_idx] * V1[i]
        Blend_CAGR <- recalculate_blend_cagr(Blend_V1)
        df$Blend_CAGR <- Blend_CAGR
        if (Blend_V1[i] >= Blend_V1[i - 1]) break
      }
    }
  }
  
  # Recalculate Blend_CAGR one final time after all adjustments
  Blend_CAGR <- recalculate_blend_cagr(Blend_V1)
  df$Blend_CAGR <- Blend_CAGR
  
  # Rename column for consistency
  df <- df %>% rename(Year = t)
  
  return(df)
}

