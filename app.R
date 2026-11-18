# STAND - IPR policy pathway app
# Run from the repository root with:  shiny::runApp()

# Load necessary libraries
library(shiny)
library(shinyBS)  # For collapse panels
library(shinyjs)  # For disabling sliders
library(dplyr)    # For data manipulation
library(plotly)
library(shinyWidgets)
library(tidyr)
library(shinydashboard)
library(purrr)
library(stringr)

# Paths are relative to the repository root
data_dir    <- "data"
survey_file <- file.path(data_dir, "survey", "expert_survey_baseline.csv")

# The input data is not part of this repository (see data/README.md).
# Stop early with a clear message if any required file is missing.
required_files <- c(
  file.path(data_dir, c("STAND_model_parameters.csv", "Energy_Policies_Params.csv",
                        "Policy_Value_Bridge.csv", "region_name_translations.csv",
                        "SEAO_Power_Data.csv", "SEAO_Emissions_Share.csv",
                        "power_cost_retirement_rates.csv", "transport_costs.csv",
                        "global_emissions.csv")),
  survey_file
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("STAND input data missing - see data/README.md for what is needed:\n  ",
       paste(missing_files, collapse = "\n  "), call. = FALSE)
}

source(file.path("R", "initialisation.R"))       # read in data, set some global variables
source(file.path("R", "create_plot_data.R"))     # creates time series data based on the pathway parameters
source(file.path("R", "impact_calculations.R"))  # calculates the emissions change
source(file.path("R", "humps_bumps.R"))          # creates the disruption ("humps & bumps") pathways


## Initialization code
# Survey data shown on the Policy Analysis tab
all_survey_data <- read.csv(survey_file, check.names = FALSE)


Policies <- Policies %>%
  filter(Policy != "Phase out of new coal")


### Shiny Code

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  tags$head(
    # Existing CSS styles
    tags$style(HTML("
      @font-face {
        font-family: 'Arial';
        font-weight: normal;
        font-style: normal;
      }
      body, .shiny-input-container, .shiny-output-container, .shiny-text-output, .shiny-table-output, .irs-bar, .irs-handle, .irs-bar-edge, .irs-single, .irs-grid-pol {
        font-family: 'Arial', sans-serif !important;
      }
      /* Common style for labels */
      .custom-label {
        font-size: 14px;
        color: #000000;
        margin-bottom: 5px;
        font-weight: normal;
      }
      .custom-label.bold {
        font-weight: bold;
      }
      .custom-label.slider-label:hover {
        font-weight: bold;
        color: #D14956;
      }
      /* Custom CSS for sliders */
      .custom-slider .irs-bar {
        background-color: #cccdcf !important;
        border: 2px solid #a8a8a8 !important;
      }
      .persistent-hover .irs-bar {
        background-color: #595959 !important;
        border: 2px solid #7F7F7F !important;
      }
      .custom-slider .irs-min {
        color: #ffffff !important;
        background-color: #ffffff !important;
      }
      .custom-slider .irs-max {
        color: #ffffff !important;
        background-color: #ffffff !important;
      }
      .persistent-hover .irs-min {
        color: #a9aaab !important;
        background-color: #ffffff !important;
      }
      .persistent-hover .irs-max {
        color: #a9aaab !important;
        background-color: #ffffff !important;
      }
      .custom-slider .irs-handle {
        background-color: #D9D9D9 !important;
        border-color: #a9aaab !important;
        border-width: 2px !important;
      }
      .persistent-hover .irs-handle {
        background-color: #D14956 !important;
        border-color: #D9D9D9 !important;
        border-width: 2px !important;
      }
      .custom-slider .irs-single, .custom-slider .irs-grid-pol {
        background-color: #D9D9D9!important;
        border-color: #D9D9D9 !important;
      }
      .persistent-hover .irs-single, .persistent-hover .irs-grid-pol {
        background-color: #000000 !important;
        border-color: #000000 !important;
      }
      /* Hide grid lines and ticks */
      .custom-slider .irs-grid, .custom-slider .irs-grid-pol, .custom-slider .irs-grid-text {
        display: none !important;
      }
      /* Custom CSS for panel headings */
      .panel-title {
        font-family: 'Univers Condensed Light', sans-serif !important;
        font-size: 16px; /* Adjust font size */
        color: #ffffff; /* Adjust color */
        font-weight: bold; /* Make font bold */
      }
      .panel-heading {
        background-color: white !important;
        color: white !important;
        border: 1px solid #e1e1e1 !important; /* Adjust border */
      }
      .sidebar-panel-custom {
        border: 1px solid #e1e1e1 !important;
        width: 300px !important; /* Set fixed width */
        min-width: 300px !important; /* Ensure minimum width */
        max-width: 300px !important; /* Ensure maximum width */
      }
      .panel-heading {
        background-color: white !important;
        color: black !important;
        border: 1px solid #e1e1e1 !important; /* Adjust border */
        border-bottom: none !important; /* Remove bottom border */
      }
      .panel-title a {
        color: black !important;
      }
      .panel-body {
        border-top: 1px solid #e1e1e1 !important; /* Add top border to the body */
      }
      .panel.panel-primary {
        border-color: #e1e1e1 !important; /* Set the border color of the collapsible panels */
      }
      .panel.panel-primary > .panel-heading {
        background-color: #f5f5f5 !important; /* Adjust header background color */
        border-color: #e1e1e1 !important; /* Adjust header border color */
      }
      .panel.panel-primary > .panel-heading + .panel-collapse > .panel-body {
        border-top-color: #e1e1e1 !important; /* Adjust the color of the top border of the panel body */
      }
      .toggle-on {
        color: black !important; /* Change text color when toggle is on */
        background-color: #f5f5f5 !important; /* Custom 'on' color */
      }
      .toggle-off {
        color: black !important; /* Change text color when toggle is off */
        background-color: #f5f5f5 !important; /* Custom 'off' color */
      }

      /* New CSS for rotated dropdown next to policyPathwayPlot */
      .plot-container {
        position: relative;
        width: fit-content;
        margin-left: auto;
        margin-right: auto;
      }
      .y-axis-dropdown {
        position: absolute;
        left: -10px; /* Adjust this value to position the dropdown horizontally */
        top: 50%;
        transform: translateY(-50%) rotate(-90deg);
        transform-origin: left center;
        cursor: pointer; /* Change cursor to pointer on hover */
      }
      /* Define the rotated state when expanded */
      .y-axis-dropdown.expanded {
        transform: translateY(-50%) rotate(0deg);
      }
      /* Adjust the width of the dropdown to accommodate variable names */
      .y-axis-dropdown .selectize-control {
        width: 150px !important;
      }
    ")),
    # Existing JavaScript
    tags$script(HTML("
      $(document).ready(function() {
        // JavaScript to add persistent hover effect and send message to Shiny
        $(document).on('mouseenter', '.custom-slider, .custom-label.slider-label', function() {
          $('.custom-slider').removeClass('persistent-hover');
          $('.custom-label.slider-label').removeClass('bold');  // Ensure all labels are reset to normal weight

          var sliderContainer = $(this).hasClass('custom-slider') ? $(this) : $(this).closest('.custom-slider');
          sliderContainer.addClass('persistent-hover');
          sliderContainer.find('.custom-label.slider-label').addClass('bold');  // Add bold class to the label

          var sliderId = sliderContainer.find('input').attr('id');
          var sliderLabel = sliderContainer.find('.custom-label.slider-label').text();  // Get the label text
          Shiny.setInputValue('hovered_slider', { id: sliderId, label: sliderLabel });  // Send both id and label
        });
      });

      // JavaScript to handle click events for the rotated dropdown
      $(document).on('click', '.y-axis-dropdown', function(e) {
        e.stopPropagation();  // Prevent the click event from propagating to the document
        $(this).toggleClass('expanded');
      });
      // Close the dropdown when clicking outside
      $(document).on('click', function(e) {
        if(!$(e.target).closest('.y-axis-dropdown').length) {
          $('.y-axis-dropdown').removeClass('expanded');
        }
      });

      // JavaScript to handle click event for transitioning tabs
      $(document).on('click', '#go_to_policy_analysis', function() {
        $('a[data-value=\"policy_analysis\"]').click();
      });

      // JavaScript to handle click event for transitioning tabs
      $(document).on('click', '#go_to_impact_analysis', function() {
        $('a[data-value=\"policy_impact_analysis\"]').click();
      });
      
      // JavaScript to handle click event for transitioning tabs
      $(document).on('click', '#go_to_advanced_analysis', function() {
        $('a[data-value=\"advanced_analysis\"]').click();
      });
      
      
    ")),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-toggle/2.2.2/css/bootstrap-toggle.min.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-toggle/2.2.2/js/bootstrap-toggle.min.js"),
    # Include Font Awesome for icons
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
  ),
  titlePanel("STAND"),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel-custom",
      style = "display: flex; flex-direction: column; height: 100%;",
      p(class = "custom-label bold", "Region:"),
      selectInput(
        inputId = "regionInput",
        NULL,
        choices = unique(Policies$Region),
        selected = unique(Policies$Region)[1]  # Set a default value here
      ),
      uiOutput("netzeroSliderUI"),
      uiOutput("slidersUI"),
      
     
      # Spacer to push the button to the bottom
      div(style = "flex-grow: 1;"),
      
      # Add padding above the Export Pathways button
      div(
        style = "display:flex; justify-content: center; padding-top: 20px;",  # Adjust the padding as needed
        downloadButton("exportPathways", "Export Pathways", class = "btn-primary")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",  
        tabPanel(
          "Policy Analysis",
          value = "policy_analysis",  
         htmlOutput("maintabTitle"),
         textOutput("inputPolicy"), 
          plotlyOutput(outputId = "responsePlot", height = "500px"),
           div(
            style = "text-align: right; margin-top: 10px;",
            actionLink(
              "go_to_impact_analysis",
              HTML("Impact Analysis&nbsp;<i class='fa fa-arrow-right'></i>"),
              style = "font-size: 16px;"
            )
          ),
          plotlyOutput("percentilePlot", height = "500px")
        ),
        
        tabPanel(
          "Policy Impact Analysis",
          value = "policy_impact_analysis",  
          htmlOutput("policyPathwayText"),
          
          # Plot container with rotated dropdown
          div(
            class = "plot-container",
            plotlyOutput("policyPathwayPlot", height = "500px"),
            div(
              class = "y-axis-dropdown",
              selectInput("yAxisUnits", NULL, choices = NULL, selected = NULL)
            )
          ),
          
          # Custom CSS for styling
          tags$style(HTML("
            .plot-container { margin-bottom: 10px; }
            .policy-output-row { margin-top: 0px; margin-bottom: 10px; }

            /* Align buttons horizontally */
            .control-row {
              display: flex;
              align-items: flex-start;
            }
            .control-item {
              flex: 1;
              text-align: center;
              display: flex;
              flex-direction: column;
              align-items: center;
            }
            .control-label {
              height: 20px;
              margin-bottom: 5px;
            }
            /* Custom Reset button styling */
            .reset-button {
              background-color: #D9D9D9;
              color: white;
              border-color: #D9D9D9;
            }
            .reset-button:hover {
              background-color: #e74c3c;
              border-color: #e74c3c;
            }
          ")),
          
          # Output Section with Corrected Column Widths
          fluidRow(
            class = "policy-output-row",
            column(3, uiOutput("YearChange1")),
            column(3, uiOutput("totalEmissionsChange1")),
            column(3, uiOutput("emissionsChangePercRegion1"))
            #column(3, uiOutput("MaxAnnualCARG1"))
          ),
          fluidRow(
            class = "policy-output-row",
            column(12, uiOutput("policyDetails"))
          ),
          
          # Advanced Adjustment Panel with Horizontally Aligned Buttons
          bsCollapse(id = "advanced-adjustment",
                     bsCollapsePanel(
                       title = "Advanced Adjustment",
                       tags$div(
                         class = "custom-card",
                         fluidRow(
                           class = "control-row",
                           # column(3,
                           #        div(class = "control-item",
                           #            div(class = "control-label", "Stylized Response"),
                           #            div(class = "control-buttons",
                           #                radioGroupButtons(
                           #                  inputId = "methodToggle",
                           #                  label = NULL,
                           #                  choices = c("Slow", "Fast"),
                           #                  selected = "Slow",
                           #                  direction = "vertical",
                           #                  individual = TRUE,
                           #                  size = "sm",
                           #                  status = "primary"
                           #                )
                           #            )
                           #        )
                           # ),
                           column(3,
                                  div(class = "control-item",
                                      div(class = "control-label", "Timing"),
                                      div(class = "control-buttons",
                                          actionButton("t0_minus", label = "-", class = "btn-small btn-primary"),
                                          actionButton("t0_plus", label = "+", class = "btn-small btn-primary")
                                      ),
                                      div(class = "control-label", tags$strong(uiOutput("manualAdjustedInflectionText")))
                                  )
                           ),
                           column(3,
                                  div(class = "control-item",
                                      div(class = "control-label", "Abruptness"),
                                      div(class = "control-buttons",
                                          actionButton("r_minus", label = "-", class = "btn-small btn-primary"),
                                          actionButton("r_plus", label = "+", class = "btn-small btn-primary")
                                      ),
                                      div(class = "control-label", tags$strong(uiOutput("manualAdjustedRateText")))
                                  )
                           ),
                           # Reset button with custom class
                           column(3,
                                  div(class = "control-item",
                                      div(class = "control-label", ""),  # Empty label for alignment
                                      div(class = "control-buttons",
                                          actionButton(
                                            inputId = "resetButton",
                                            label = "Reset",
                                            class = "btn reset-button"  # Apply the custom class here
                                          )
                                      )
                                  )
                           )
                         )
                       )
                     )
          ),
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-top: 10px;",
            actionLink(
              "go_to_policy_analysis",
              HTML("<i class='fa fa-arrow-left'></i> Policy Analysis"),
              style = "font-size: 16px;"
            ),
            actionLink(
              "go_to_advanced_analysis",
              HTML("Advanced Analysis <i class='fa fa-arrow-right'></i>"),
              style = "font-size: 16px;"
            )
          )
        ),
        
        tabPanel("Advanced Analysis",
                 value = "advanced_analysis",
                 htmlOutput("advancedTabTitle"),
                 tags$div(),
                 fluidRow(
                   column(6,
                          selectInput(
                            "disruptType",
                            "Disruption Type",
                            choices = c(
                              "Constant" = "constant",
                              "Custom" = "custom",
                              "Constant Volume" = "constant_volume"
                            ),
                            selected = "constant"
                          ),                          conditionalPanel(
                            condition = "input.disruptType == 'custom'",
                            numericInput("customRate", "Custom Rate", value = -0.2)
                          )
                   ),
                   column(6,
                          div(class = "slider-container super-advanced-slider",
                              sliderInput("disruptRange", "Disruption Start and Duration (Years)",
                                          min = 2025, max = 2075, value = c(2026, 2030), sep = "", step = 1)
                          )
                   )
                 ),
                 fluidRow(
                   plotlyOutput("combinedPlotOutput", height = "800px")
                 ),
                 div(
                   style = "text-align: left; margin-top: -100px;",
                   actionLink(
                     "go_to_impact_analysis",
                     HTML("<i class='fa fa-arrow-left'></i> Policy Pathway Analysis&nbsp;"),
                     style = "font-size: 16px;"
                   )
                 )
        )
      )
    )
  )
)


server <- function(input, output, session) {

  current_policy_reactive <- reactiveVal(NULL)
  policy_threshold_reactive <- reactiveVal(NULL)
  threshold_criteria_reactive <- reactiveVal(NULL)
  threshold_Units_reactive <- reactiveVal(NULL)
  selected_variable_id_reactive <- reactiveVal(NULL)
  original_params_reactive <- reactiveVal(NULL)
  user_model_reactive <- reactiveVal(NULL)
  user_pathway_reactive <- reactiveVal(NULL)
  current_t0_perc_index <- reactiveVal(0)
  current_r_index <- reactiveVal(0)
  adjusted_model_reactive <- reactiveVal(NULL)
  adjusted_pathway_reactive <- reactiveVal(NULL)
  revisedPolicyDate <- reactiveVal(NULL)
  iprPolicyDate_reactive <- reactiveVal(NULL)
  current_slider_id <- reactiveVal(NULL)
  UnitOptions <- reactiveVal(NULL)
  adjustments_made <- reactiveVal(FALSE)


  # Render the NetZero slider separately
  output$netzeroSliderUI <- renderUI({
    req(input$regionInput)

    selected_set <- isolate(input$regionInput)
    netzero_row <- Policies[Policies$Region == selected_set & Policies$Policy == "Net zero target", ]

    if (nrow(netzero_row) == 0 || is.na(netzero_row$Forecast_Year) || netzero_row$Forecast_Year < 2020 || netzero_row$Forecast_Year > 2100) {
      netzero_value <- NA  # Set to NA if not available or out of bounds
    } else {
      netzero_value <- netzero_row$Forecast_Year[1]
    }

    div(class = "custom-slider",
        div(class = "custom-label slider-label", "Net zero target"),  # Add label div with slider-label class
        sliderInput("netzeroSlider", NULL,
                    min = 2020, max = 2100, value = ifelse(is.na(netzero_value), 2020, netzero_value),
                    animate = FALSE,
                    pre = "", post = "", sep = "") %>%
          { if (is.na(netzero_value)) shinyjs::disabled(.) else . }  # Disable the slider if required
    )
  })

  # Generate the sector sliders UI dynamically based on region selection
  output$slidersUI <- renderUI({
    req(input$regionInput)

    tryCatch({
      selected_set <- input$regionInput
      sliders <- Policies[Policies$Region == selected_set & Policies$Policy != "Net zero target", ]

      # Add policies not present in sliders data
      missing_policies <- setdiff(policy_list, sliders$Policy)
      if (length(missing_policies) > 0) {
        missing_df <- data.frame(Region = selected_set, Policy = missing_policies, Forecast_Year = NA,
                                 Policy_Order = NA, Variable_ID = NA, Policy_Threshold = NA,
                                 Threshold_Criteria = NA, Units = NA, Policy_Type = NA, stringsAsFactors = FALSE)
        sliders <- bind_rows(sliders, missing_df)
      }

      # Sort sliders by Policy_Order
      sliders <- sliders[order(sliders$Policy_Order, sliders$Policy), ]

      # Generate slider inputs dynamically
      slider_inputs <- lapply(1:nrow(sliders), function(i) {
        forecast_year <- sliders$Forecast_Year[i]
        disabled <- is.na(forecast_year) || !is.numeric(forecast_year) || forecast_year < 2020 || forecast_year > 2100
        div(class = "custom-slider",
            div(class = "custom-label slider-label", sliders$Policy[i]),  # Add label div with slider-label class
            sliderInput(inputId = paste0("slider", i),
                        label = NULL,  # Remove default label
                        min = 2020, max = 2100, value = ifelse(disabled, 2020, forecast_year),
                        animate = FALSE,
                        pre = "", post = "", sep = "") %>%
              { if (disabled) shinyjs::disabled(.) else . }  # Disable the slider if required
        )
      })

      # Filter out NA or "Unknown" Policy_Type
      valid_sliders <- sliders[!is.na(sliders$Policy_Type) & sliders$Policy_Type != "Unknown", ]

      # Create collapsible panels for each sector
      sector_panels <- lapply(unique(valid_sliders$Policy_Type), function(sector) {
        sector_sliders <- slider_inputs[valid_sliders$Policy_Type == sector]
        if (length(sector_sliders) > 0) {
          bsCollapsePanel(sector, do.call(tagList, sector_sliders), style = "primary")
        } else {
          NULL
        }
      })

      # Return the UI elements
      tagList(
        p(class = "custom-label bold", "Sector Policies:"),  # Make "Sector Policies:" label bold without hover effect
        do.call(bsCollapse, sector_panels)
      )

    }, error = function(e) {
      # Log the error and provide a fallback UI or message
      print(paste("Error in slidersUI:", e$message))
      tagList(
        p("An error occurred while generating the sliders. Please try again.")
      )
    })
  })



  # Update the current slider id whenever hovered_slider changes
  observe({
    req(input$hovered_slider)
    current_slider_id(input$hovered_slider$id)
  })

  # Reactive expression to subset the Policies dataframe for plotting in the overview tab
  Overview_data <- reactive({
    req(input$hovered_slider, input$regionInput, revisedPolicyDate())

    hover_info <- input$hovered_slider
    policySelected <- hover_info$label
    regionSelected <- input$regionInput

    # Retrieve the Policy_Type based on the hovered_slider policy
    policy_type <- Policies %>%
      filter(Policy == policySelected) %>%
      select(Policy_Type) %>%
      pull() %>%
      first()

    # Subset the Policies dataframe by the Policy_Type and Region, and sort by Policy_Order
    Policies %>%
      filter((Policy_Type == policy_type | Policy_Type == "Economy"), Region == regionSelected) %>%
      arrange(Policy_Order)

  })

  # Reactive expression for Policies_sub
  Policies_sub <- reactive({
    req(Overview_data())  # Ensure Overview_data is available
    Overview_data() %>% arrange(Policy_Order)
  })

  # Display hover information with label instead of ID
  output$inputPolicy <- renderText({
    hover_info <- input$hovered_slider
    if (is.null(hover_info)) {
      return("Select a policy on the left")
    } else {
    }
  })
  
  # Create a reactive value with a default fallback
  methodToggleValue <- reactive({
    if (is.null(input$methodToggle)) {
      "Slow"  # Default value when input$methodToggle is missing
    } else {
      input$methodToggle
    }
  })
  
  #Create title for plot
  output$maintabTitle <- renderUI({
    req(input$regionInput)
    req(current_policy_reactive())
    
    region <- input$regionInput
    policy_full <- current_policy_reactive()
    policy <- gsub("^([^_]*)_.*$", "\\1", policy_full)
    
    HTML(paste0("<h4><b>Survey Distribution for ", policy, " in ", region, "</b></h4>"))
  })
  
   # Reactive expression to filter data based on inputs
  filtered_survey_data <- reactive({
    
    hover_info <- input$hovered_slider
    policySelected <- hover_info$label
    regionSelected <- input$regionInput
    
    data <- all_survey_data %>%
      filter(
        Policy == policySelected,
        Region == regionSelected
      )
    

    # Standardize SubGroup names
    data$SubGroup <- trimws(data$SubGroup)
    data
    
  })
  
  
  # Define custom colors and alpha for each subgroup
  subgroup_colors <- list(
    "Expert Group (2023)" = list(
      fill = "rgba(31, 120, 180, 0.5)",  # RGBA color with alpha for fill
      line = "rgba(31, 120, 180, 1.0)"   # Opaque line color
    ),
    "Expert Group (2024)" = list(
      fill = "rgba(218, 166, 0, 0.5)",  # RGBA color with alpha for fill
      line = "rgba(218, 166, 0, 1.0)"   # Opaque line color
    ),
    "Previous Workshops" = list(
      fill = "rgba(128, 128, 128, 0.5)",  # Grey fill with 50% transparency
      line = "rgba(128, 128, 128, 1.0)"   # Solid grey line
    ),
    "IPR Forecast" = list(
      line = "rgba(0, 0, 0, 1.0)"    # Line color for IPR Forecast
    )
  )
  
  # Generate the distribution plot
  observeEvent(list(Overview_data(), input[[current_slider_id()]], input$regionInput, revisedPolicyDate()), {
  
    output$responsePlot <- renderPlotly({
      data <- filtered_survey_data()
      hover_info <- input$hovered_slider
      policySelected <- hover_info$label
      regionSelected <- input$regionInput
      
     # Check if there's data to plot
      if (nrow(data) == 0) {
        plot_ly() %>%
          layout(
            annotations = list(
              x = 0.5, y = 0.5, text = "No data available for selected Policy and Region",
              showarrow = FALSE, font = list(size = 16)
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      } else {
        # Standardize SubGroup names in data
        data$SubGroup <- trimws(data$SubGroup)
        
        # Calculate density for each subgroup, excluding "IPR Forecast"
        density_data <- data %>%
          filter(SubGroup != "IPR Forecast") %>%
          group_by(SubGroup) %>%
          do(density = density(.$Response, na.rm = TRUE)) %>%
          ungroup() %>%
          mutate(
            x = map(density, ~ .$x),
            y = map(density, ~ .$y)
          ) %>%
          select(SubGroup, x, y) %>%
          unnest(cols = c(x, y))
        
        # Calculate mean for each subgroup (excluding "IPR Forecast")
        means <- data %>%
          filter(SubGroup != "IPR Forecast") %>%
          group_by(SubGroup) %>%
          summarise(mean_response = mean(Response, na.rm = TRUE), .groups = "drop")
        
        # Calculate counts for each subgroup (excluding "IPR Forecast")
        counts <- data %>%
          filter(SubGroup != "IPR Forecast") %>%
          group_by(SubGroup) %>%
          summarise(count = n(), .groups = "drop")
        
        # Standardize SubGroup names in means and counts
        means$SubGroup <- trimws(means$SubGroup)
        counts$SubGroup <- trimws(counts$SubGroup)
        
        # Extract the unique "IPR Forecast" value, if available
        ipr_forecast_value <- data %>%
          filter(SubGroup == "IPR Forecast") %>%
          pull(Response) %>%
          unique()
        
        # Start the plot
        p <- plot_ly()
        
        # Add density lines for each subgroup with custom colors and alpha
        for (subgroup in unique(density_data$SubGroup)) {
          fill_color <- subgroup_colors[[subgroup]]$fill
          line_color <- subgroup_colors[[subgroup]]$line
          
          # Check if colors are defined for the subgroup
          if (is.null(fill_color) || is.null(line_color)) {
            fill_color <- "rgba(128, 128, 128, 0.5)"  # Default gray fill
            line_color <- "rgba(128, 128, 128, 1.0)"  # Default gray line
            warning(paste("Colors not defined for SubGroup:", subgroup))
          }
          
          # Get the count for this subgroup
          subgroup_count <- counts$count[counts$SubGroup == subgroup]
          
          # Modify the legend name to include the count
          legend_name <- paste0(subgroup, " (n=", subgroup_count, ")")
          
          # Set default visibility for "Previous Workshops" to hidden
          default_visibility <- ifelse(subgroup %in% c("Previous Workshops"), "legendonly", TRUE)
          
          
          p <- p %>%
            add_trace(
              data = density_data %>% filter(SubGroup == subgroup),
              x = ~x,
              y = ~y,
              type = "scatter",
              mode = "lines",
              name = legend_name,
              fill = "tozeroy",
              fillcolor = fill_color,
              line = list(color = line_color),
              showlegend = TRUE,
              visible = default_visibility  # Apply default visibility
            )
        }
        
        # Add mean lines for each subgroup with custom line colors
        for (i in 1:nrow(means)) {
          subgroup <- means$SubGroup[i]
          mean_x <- means$mean_response[i]
          line_color <- subgroup_colors[[subgroup]]$line
          
          # Check if line color is defined for the subgroup
          if (is.null(line_color)) {
            line_color <- "rgba(128, 128, 128, 1.0)"  # Default gray line
            warning(paste("Line color not defined for SubGroup:", subgroup))
          }
          
          # Set default visibility for "Previous Workshops" to hidden
          default_visibility <- ifelse(subgroup %in% c("Previous Workshops"), "legendonly", TRUE)
          
          p <- p %>%
            add_trace(
              x = c(mean_x, mean_x),
              y = c(0, max(density_data$y, na.rm = TRUE)),
              mode = "lines",
              line = list(dash = "dash", color = line_color),
              showlegend = TRUE,
              visible = default_visibility,  # Apply default visibility
              name = paste("Mean:", subgroup)
            )
        }
        
        # Add the IPR Forecast line if available
        if (length(ipr_forecast_value) == 1) {
          p <- p %>%
            add_trace(
              x = c(ipr_forecast_value, ipr_forecast_value),
              y = c(0, max(density_data$y, na.rm = TRUE)),
              mode = "lines",
              line = list(dash = "dot", color = subgroup_colors[["IPR Forecast"]]$line),
              showlegend = TRUE,
              name = "IPR Forecast"
            )
        }
        
        # Add the user-selected year marker with a circle and outline
        user_selected_year <- revisedPolicyDate()
        
        mid_y <- max(density_data$y, na.rm = TRUE) / 2
        
        p <- p %>%
          add_trace(
            x = user_selected_year,
            y = mid_y,
            mode = "markers",
            type = "scatter",
            marker = list(
              symbol = "circle",
              size = 20,
              color = "#D14956",
              line = list(
                color = "#D9D9D9",
                width = 2
              )
            ),
            showlegend = TRUE,
            name = paste("Your Response:", user_selected_year)
          )
        
        # Add the vertical line at user_selected_year
        vertical_line <- list(
          type = "line",
          x0 = user_selected_year,
          x1 = user_selected_year,
          y0 = 0,
          y1 = max(density_data$y, na.rm = TRUE),
          line = list(
            color = "rgba(209,73,86,0.3)",  # Faint red color with transparency
            width = 2,
            dash = "solid"
          )
        )
        
        # Add annotation to the "Your Response" marker without arrow and to the right
        p <- p %>%
          add_annotations(
            x = user_selected_year,
            y = mid_y,
            text = "Your Response",
            showarrow = FALSE,
            xanchor = "left",
            xshift = 10,  # Adjust this value to position the text to the right
            font = list(color = "#D14956", size = 12),
            align = "left"
          )
        
        # Customize layout and include the vertical line
        p <- p %>%
          layout(
            title = NULL,
            xaxis = list(title = "Response Year"),
            yaxis = list(title = "Density of Responses"),
            barmode = "overlay",
            shapes = list(vertical_line)
          )
        
        p
      }
    })
    
  })
  
  
  

# Store date changes in global dataframe when sliders are adjusted and update reactive values
observe({
  req(input$hovered_slider)  # Ensure that there is a hovered slider
  hover_info <- input$hovered_slider
  policySelected <- hover_info$label
  regionSelected <- input$regionInput

  # Find the slider ID and the current value
  slider_id <- hover_info$id
  value <- input[[slider_id]]

  # Debugging statement
  cat("\n", policySelected, "in", regionSelected, "currently at:", value, "\n")

  # Update current policy reactive value
  current_policy_reactive(policySelected)

  filtered_data <- Policies %>%
    filter(Policy == policySelected, Region == regionSelected) %>%
    select(Policy, Definition_Text, Forecast_Year, Policy_Threshold, Threshold_Criteria, Units, Variable_ID, Metric2, Units2, Metric3, Units3) %>%
    unique()

  if (nrow(filtered_data) == 0) {
    policy_threshold <- NA
    threshold_criteria <- NA
    threshold_units <- NA
    selected_variable_id <- NA
  } else {
    policy_threshold <- filtered_data$Policy_Threshold[1]
    threshold_criteria <- filtered_data$Threshold_Criteria[1]
    threshold_units <- filtered_data$Units[1]
    selected_variable_id <- filtered_data$Variable_ID[1]
    ipr_policy_date <- filtered_data$Forecast_Year[1]
  }

  # Save the model parameters for the selected policiy
  original_params <- model_parameters %>%
    filter(Variable_ID == selected_variable_id, Region == regionSelected)

  # Update reactive values only if they have changed
  isolate({
    policy_threshold_reactive(policy_threshold)
    threshold_criteria_reactive(threshold_criteria)
    threshold_Units_reactive(threshold_units)
    selected_variable_id_reactive(selected_variable_id)
    iprPolicyDate_reactive(ipr_policy_date)
    original_params_reactive(original_params)
      })

  # update the revisedPolicyDate with the value from the slider
  revisedPolicyDate(value)

  # Reset reactive values
  user_model_reactive(NULL)
  current_t0_perc_index(0)
  current_r_index(0)
  adjusted_model_reactive(NULL)
  adjusted_pathway_reactive(NULL)
  adjustments_made(FALSE)
  
  # Ensure the column Your_Response exists in Policies dataframe
  if (!"Your_Response" %in% colnames(Policies)) {
    Policies$Your_Response <- NA
  }

  # Update the Policies dataframe with the new slider value
  Policies <<- Policies %>%
    mutate(Your_Response = ifelse(Policy == policySelected & Region == regionSelected, value, Your_Response))

  # Save to the global environment
  assign("Policies", Policies, envir = .GlobalEnv)
})


  # Calculate new pathways based on users change of policy data

  observeEvent({
    revisedPolicyDate()
    methodToggleValue()
  }, {

    cat("\nobserveEvent triggered for revisedPolicyDate with value: ", revisedPolicyDate(), "\n")

    # Check if selected_variable_id_reactive is not NULL
    if (!is.null(selected_variable_id_reactive())) {
      threshold <- as.numeric(policy_threshold_reactive())
      threshold_criteria <- threshold_criteria_reactive()

      cat("Selected Variable ID: ", selected_variable_id_reactive(), "\n")
      cat("Policy Threshold: ", threshold, "\n")
      cat("Threshold Criteria: ", threshold_criteria, "\n")

      # Ensure revisedPolicyDate is not NULL or NA
      if (!is.null(revisedPolicyDate()) && !is.na(revisedPolicyDate())) {

        if(methodToggleValue() == "Fast") {

          # Choose which function to use to calculate nearest model.
          if(current_policy_reactive() == "Net zero target") {
            user_model_reactive(find_nearest_NZ_model(revisedPolicyDate(), selected_variable_id_reactive(), input$regionInput, threshold, threshold_criteria))
              # for other polices use the other function
               } else {
            user_model_reactive(find_nearest_model_rate(revisedPolicyDate(), selected_variable_id_reactive(), input$regionInput, threshold, threshold_criteria))
               }

          # Plot data based on model params and store in reactive container
          userDataForPlot <- create_plot_data(user_model_reactive()) %>%
            rename(Value = Fitted_model) %>%
            mutate(Units = original_params_reactive()$Units[1])
          
          userDataForPlot <- downscale_plot_data(userDataForPlot, selected_variable_id_reactive(), input$regionInput, input$yAxisUnits)
          
          user_pathway_reactive(userDataForPlot)

        } else { # Use the stretch pathway method and update reactive container as well as compute paramters for the streched pathways and store them
          streched_pathway <- create_pathways(selected_variable_id_reactive(), input$regionInput, revisedPolicyDate(), model_parameters, "NA")
          
          userDataForPlot <- downscale_plot_data(streched_pathway, selected_variable_id_reactive(), input$regionInput, input$yAxisUnits)
          
          user_pathway_reactive(userDataForPlot)

          user_model_reactive(paramaterise_pathway(streched_pathway, selected_variable_id_reactive()))
          # cat("The parameters for the stretched pathway are: \n")
          # print(user_model_reactive())

        }

        # Also copy the model details to the adjusted model container so that it's ready to be used if the user chose to adjust the parameters
        adjusted_model_reactive(user_model_reactive())

      }
        else {
        cat("Invalid revised policy date for nearest model creation\n")
      }
    } else {
      cat("No variable selected for nearest model creation\n")
    }

  })

  # Create title for Pathway page
  output$policyPathwayText <- renderUI({
    # Use req() to ensure inputs are available
    req(input$regionInput)
    req(current_policy_reactive())
    
    region <- input$regionInput
    policy_full <- current_policy_reactive()
    policy <- gsub("^([^_]*)_.*$", "\\1", policy_full)
    
    HTML(paste0("<h4><b>Pathway for ", policy, " in ", region, "</b></h4>"))
  })


  # Set UnitOptions dataframe
  observe({
    req(input$hovered_slider)  # Ensure that there is a hovered slider
    hover_info <- input$hovered_slider
    policySelected <- hover_info$label
    regionSelected <- input$regionInput

    updatedUnitOptions <- Policies %>%
      filter(Policy == policySelected, Region == regionSelected) %>%
      select(Variable_ID, Units, Metric2, Units2, Metric3, Units3, Metric4, Units4) %>%
      unique() %>%
      bind_rows(
        select(., Metric = Variable_ID, Unit = Units),
        select(., Metric = Metric2, Unit = Units2),
        select(., Metric = Metric3, Unit = Units3),
        select(., Metric = Metric4, Unit = Units4)
      ) %>%
      filter(!is.na(Metric) & !is.na(Unit)) %>%
      select(Metric, Unit)

    UnitOptions(updatedUnitOptions)  # Update the reactiveVal
  })

  # Generate the dropdown menu for the units selection for the pathway plot dynamically
  observe({
    updateSelectInput(
      session,
      "yAxisUnits",
      choices = UnitOptions()$Unit,
      selected = UnitOptions()$Unit[1]  # Or any default value you prefer
    )
  })


  # Update user plot data based on unit selection
  observe({
    req(input$hovered_slider,input$yAxisUnits, methodToggleValue(), user_model_reactive())

    userDataForPlot = user_pathway_reactive()

          # Recreate data if the units are changed
      if (input$yAxisUnits != userDataForPlot$Units[1]) {

        cat("Units changed sending", input$yAxisUnits, "to scaling function\n")
        userDataForPlot <- create_pathways(selected_variable_id_reactive(), input$regionInput, revisedPolicyDate(), model_parameters, input$yAxisUnits)
        cat("Returned scaled values in", userDataForPlot$Units[1], "\n")
        userDataForPlot <- downscale_plot_data(userDataForPlot, selected_variable_id_reactive(), input$regionInput, input$yAxisUnits)
        user_pathway_reactive(userDataForPlot)
      }

  })


  # Define a reactive function to create the Pathway plot
  createPlot <- reactive({
    req(selected_variable_id_reactive(), input$regionInput, user_pathway_reactive(), methodToggleValue(), input$yAxisUnits)

    # Set the data point for the forecast year marker within the plot
    forecast_year <- iprPolicyDate_reactive()
    
    # Get parameters for plotting original FPS data
    tryCatch({
      params <- original_params_reactive()
      # Check if params is empty or if End_year is missing
      if (nrow(params) == 0 || all(is.na(params$End_year))) {
        print("No parameters found for the selected Variable_ID and Region, or End_year contains only NA values.")
        return(NULL)
      }

      # If different units have been selected then create the data from the select units rather than the default units
      if (input$yAxisUnits == params$Units[1]) {
      originalDataForPlot <- create_plot_data(params) %>%
        rename(Value = Fitted_model)
      originalDataForPlot$Units <- params$Units[1]
      originalDataForPlot <- downscale_plot_data(originalDataForPlot, selected_variable_id_reactive(), input$regionInput, input$yAxisUnits)
      
      }
      else {
        originalDataForPlot <- create_pathways(selected_variable_id_reactive(), input$regionInput, forecast_year , model_parameters, input$yAxisUnits)
        originalDataForPlot <- downscale_plot_data(originalDataForPlot, selected_variable_id_reactive(), input$regionInput, input$yAxisUnits)
              }

   
      if (any(originalDataForPlot$Year == forecast_year)) {
        y_value_at_forecast <- originalDataForPlot$Value[originalDataForPlot$Year == forecast_year]
      } else {
        y_value_at_forecast <- NA
      }

    
      # Check if all values in the first 10 entries are greater than 1, then decide decimal places
      yaxis_tickformat <- if (all(head(originalDataForPlot$Value, 10) > 1)) {
        ".0f"  # No decimal places
      } else {
        ".2f"  # Keep 2 decimal places
      }

      # Create the initial plot
      plot <- plot_ly(data = originalDataForPlot, x = ~Year, y = ~Value, type = 'scatter', mode = 'lines',
                      line = list(color = '#007CB0'), name = 'Original Policy Pathway',
                      height = 500, showlegend = FALSE)

      # Add IPR policy year line and marker
      plot <- plot %>%
        add_trace(
          x = c(forecast_year, forecast_year),
          y = c(0, y_value_at_forecast),
          type = 'scatter',
          line = list(color = '#9FCDE1', dash = 'dot'),
          name = 'Policy Date',
          hoverinfo = 'text',
          text = rep(sprintf("Policy date: %d", forecast_year)),
          showlegend = FALSE
        ) %>%
        add_trace(
          x = forecast_year,
          y = y_value_at_forecast,
          type = 'scatter',
          mode = 'markers',
          marker = list(size = 10, color = 'white', symbol = 'diamond', line = list(color = 'black', width = 2)),
          name = 'IPR Policy Date',
          showlegend = TRUE
        )

      # Handle different y-axis units
      if (input$yAxisUnits == "%") {
        plot <- plot %>%
          layout(
            yaxis = list(
              tickformat = ".0%",
              range = c(0, 1),
              linecolor = '#292929',
              linewidth = 1,
              title = ""
              # Do not add ticksuffix here
            )
          )
      } else {
        plot <- plot %>%
          layout(
            yaxis = list(
              tickformat = yaxis_tickformat,
              linecolor = '#292929',
              linewidth = 1,
              title = "",
              ticksuffix = paste0(" ", input$yAxisUnits)  # Adds a space before the units
            )
          )
      }
      
      # Add traces for the User's policy (nearest model result), if available

      # Create series for user data

      if (!is.null(user_pathway_reactive()) && !is.null(revisedPolicyDate())) {

        # If the user has made advanced adjustments, then plot the defualt pathway, otherwise plot the adjusted pathway
        if(!adjustments_made()){
          userDataForPlot <- user_pathway_reactive()
        } else {
          userDataForPlot <- adjusted_pathway_reactive()
        }

      new_forecast_year <- as.numeric(revisedPolicyDate())

        # Render User's pathway
        plot <- plot %>%
          add_trace(data = userDataForPlot, x = ~Year, y = ~Value, type = 'scatter', mode = 'lines',
                    line = list(color = '#D14956'), name = 'Adjusted Policy Pathway')

        if (any(as.numeric(userDataForPlot$Year) == new_forecast_year)) {
            y_value_at_new_forecast <- userDataForPlot$Value[userDataForPlot$Year == new_forecast_year]
        } else {
          cat("No Y value for marker!")
          y_value_at_new_forecast <- NA
        }

        # Add user's policy year line and marker together
        plot <- plot %>%
          add_trace(
            x = new_forecast_year,
            y = y_value_at_new_forecast,
            type = 'scatter',
            mode = 'markers',
            line = list(color = '#D14956', dash = 'dot'),
            marker = list(size = 20, color = '#D14956', line = list(color = '#D9D9D9', width = 2)),
            name = 'Your Response',
            hoverinfo = 'text',
            text = sprintf("Revised Policy date: %d", new_forecast_year),
            showlegend = TRUE
          )

      # Add threshold horizontal line
      plot <- plot %>%
        add_trace(
          x = range(originalDataForPlot$Year),
          y = c(policy_threshold_reactive(), policy_threshold_reactive()),
          type = 'scatter',
          mode = 'lines',
          line = list(color = '#292929', dash = 'dot', size = 1),
          name = 'Threshold Line',
          hoverinfo = 'text',
          text = paste("Policy threshold", policy_threshold_reactive()),
          showlegend = FALSE
        )

      plot <- plot %>% layout(
        margin = list(l = 40, r = 25, b = 60, t = 50, pad = 1),
        yaxis = list(linecolor = '#292929', linewidth = 1, zeroline = FALSE),
        xaxis = list(linecolor = '#292929', linewidth = 1)
      )
  }
      return(plot)

    }, error = function(e) {
      # Handle the error by logging and showing a message in the UI
      print(paste("Error creating the plot:", e$message))
      return(NULL)  # Return NULL to avoid crashing the app
    })
  })

  # Render the pathway plot
  output$policyPathwayPlot <- renderPlotly({
    createPlot()
  })

  
  
  

  # Initialize reactive values if not already done
  current_t0_perc_index <- reactiveVal(0)
  current_r_index <- reactiveVal(0)


  observeEvent(input$t0_plus, {
    updated_model_result <- adjusted_model_reactive()
    updated_model_result$t0 <- updated_model_result$t0 + 1

    recalibration_result <- recalibrate_parameters_on_t(
      existing_model_result = updated_model_result,
      user_pathway = user_pathway_reactive(),
      revisedPolicyDate = revisedPolicyDate()
    )

    adjusted_pathway_reactive(recalibration_result$adj_stretched_pathway)
    adjusted_model_reactive(recalibration_result$updated_model_result)
    adjustments_made(TRUE)
  })

  observeEvent(input$t0_minus, {
    updated_model_result <- adjusted_model_reactive()
    updated_model_result$t0 <- updated_model_result$t0 - 1

    recalibration_result <- recalibrate_parameters_on_t(
      existing_model_result = updated_model_result,
      user_pathway = user_pathway_reactive(),
      revisedPolicyDate = revisedPolicyDate()
    )

    adjusted_pathway_reactive(recalibration_result$adj_stretched_pathway)
    adjusted_model_reactive(recalibration_result$updated_model_result)
    adjustments_made(TRUE)
  })


  observeEvent(input$r_plus, {
    updated_model_result <- adjusted_model_reactive()
    updated_model_result$r <- updated_model_result$r + 0.01  # Increment r
    
    recalibration_result <- recalibrate_parameters_on_r(
      existing_model_result = updated_model_result,
      user_pathway = user_pathway_reactive(),
      revisedPolicyDate = revisedPolicyDate()
    )
    
    adjusted_pathway_reactive(recalibration_result$adj_stretched_pathway)
    adjusted_model_reactive(recalibration_result$updated_model_result)
    adjustments_made(TRUE)
  })
  
  
  observeEvent(input$r_minus, {
    updated_model_result <- adjusted_model_reactive()
    updated_model_result$r <- updated_model_result$r - 0.01  # Decrease r by 0.01
    
    recalibration_result <- recalibrate_parameters_on_r(
      existing_model_result = updated_model_result,
      user_pathway = user_pathway_reactive(),
      revisedPolicyDate = revisedPolicyDate()
    )
    
    adjusted_pathway_reactive(recalibration_result$adj_stretched_pathway)
    adjusted_model_reactive(recalibration_result$updated_model_result)
    adjustments_made(TRUE)
  })
  


  observeEvent(input$resetButton, {
    # Reset inputs to their default values
    adjusted_pathway_reactive(user_pathway_reactive)
    adjusted_model_reactive(user_model_reactive)
    # update reactive value to track if adjustments have been made
    adjustments_made(FALSE)
    # Reset other inputs as needed
  })



  ### Impact Results -  
  results <- reactiveValues(impactSummary = NULL)

   calculateImpact <- reactive({
    req(selected_variable_id_reactive(), input$regionInput, user_model_reactive())

    tryCatch({
      varId <- selected_variable_id_reactive()[1]
      Region <- input$regionInput
      user_params <- user_model_reactive()  # Get the latest model results
      IPR_policy_date <- iprPolicyDate_reactive()
      user_forecast_year <- revisedPolicyDate()
      
      if(adjustments_made()){
        
        user_pathways <- adjusted_pathway_reactive()
        
      } else {
      user_pathways <- user_pathway_reactive()
      }

     # Call the calculate_impact function and store the result

      results$impactSummary <- calculate_impact(user_forecast_year, varId, Region, user_pathways)

      # cat("The variables sent to the impact assessment are:\n", user_forecast_year, "-" , varId,  "-", Region, "\n" )
      # print(user_params[1,])

      # cat("The parameters sent for impact assessment are (first row):\n")
      # print(user_params[1,])

      return(results$impactSummary)
    }, error = function(e) {
      # Handle the error: log it and return NULL to prevent crashing the app
      print(paste("Error in calculateImpact:", e$message))
      return(NULL)
    })
  })

  # # The observe block with error handling to run calculateImpact
   observe({
    req(selected_variable_id_reactive(), input$regionInput, user_model_reactive())

    tryCatch({
      calculateImpact()  # Call the reactive function
    }, error = function(e) {
      # Log the error if the observe encounters any issue
      print(paste("Error in observe block for calculateImpact:", e$message))
    })
  })


  # Render ValueBoxes with reactive values using renderUI

  renderValueBoxes <- function(suffix) {
    output[[paste0("YearChange", suffix)]] <- renderUI({
      impactSummary <- calculateImpact()
      req(impactSummary)

      formatted_value <- as.numeric(impactSummary$Policy_Year_Change)

      value_with_unit <- paste0(formatted_value, " year")

      title <- ifelse(formatted_value < 0, "Policy Acceleration",
                      ifelse(formatted_value > 0, "Policy Delay", "Policy Date Constant"))

      valueBox(
        value_with_unit,
        title,
        width = 12
      )
    })

    output[[paste0("totalEmissionsChange", suffix)]] <- renderUI({
      impactSummary <- calculateImpact()
      req(impactSummary)

      formatted_value <- format(signif(impactSummary$Total_Emissions_Change/1000, digits = 2), big.mark = ",")
      if (impactSummary$Total_Emissions_Change > 0) {
        formatted_value <- paste0("+", formatted_value)
      }

      value_with_unit <- paste0(formatted_value, " Gt")

      valueBox(
        value_with_unit,
        "Accumulative Emissions Impact",
        width = 12
      )
    })

    # output[[paste0("MaxAnnualCARG", suffix)]] <- renderUI({
    #   impactSummary <- calculateImpact()
    #   req(impactSummary)
    # 
    # 
    #   formatted_value <- scales::percent(impactSummary$User_Max_CARG)
    #   if (impactSummary$User_Max_CARG > 0) {
    #     formatted_value <- paste0("+", formatted_value)
    #   }
    # 
    # 
    #   valueBox(
    #     formatted_value,
    #     "Max Annual CAGR",
    #     width = 12
    #   )
    # })

    output[[paste0("emissionsChangePercRegion", suffix)]] <- renderUI({
      impactSummary <- calculateImpact()
      req(impactSummary)
      
      # Extract the emissions change value
      emissions_change <- impactSummary$Emissions_Change_Perc_Region
      
      # Initialize variables for formatted value and message
      formatted_value <- "N/A"
      message <- "No data available"
     
      # Check if emissions_change is numeric and not NA
      if (is.numeric(emissions_change) && !is.na(emissions_change)) {
        # Format the emissions change as a percentage
        formatted_value <- scales::percent(emissions_change, accuracy = 0.1)
        
        # Determine the message and color based on the emissions_change value
        if (emissions_change > 0) {
          message <- "Emissions Increase"
          formatted_value <- paste0("+", formatted_value)
              } else if (emissions_change < 0) {
          message <- "Emissions Decrease"
          
        } else {
          message <- "No Impact on Emissions"
         
        }
      }
      
      # Create the valueBox with dynamic content and styling
      valueBox(
        formatted_value,
        message,
        width = 12
      )
    })
    
  }

  renderValueBoxes("1")
  renderValueBoxes("2")

  
  #Create title for plot
  output$advancedTabTitle <- renderUI({
    req(input$regionInput)
    req(current_policy_reactive())
    
    region <- input$regionInput
    policy_full <- current_policy_reactive()
    policy <- gsub("^([^_]*)_.*$", "\\1", policy_full)
    
    HTML(paste0("<h4><b>Policy Distruption for ", policy, " in ", region, "</b></h4>"))
  })
  
  
  # Create humps & bumps
  observe({
    req(input$regionInput, selected_variable_id_reactive(), input$disruptType, threshold_criteria_reactive(), threshold_Units_reactive(), policy_threshold_reactive())  # Ensure these inputs are present

      params <- model_parameters %>%
        filter(Variable_ID == selected_variable_id_reactive(), Region == input$regionInput)

      # Check if params is empty or if End_year is missing
      if (nrow(params) == 0 || all(is.na(params$End_year))) {
        print("No parameters found for the selected Variable_ID and Region, or End_year contains only NA values.")
        return(NULL)
      }

    disrupt_range <- input$disruptRange
    disrupt_start <- disrupt_range[1]
    disrupt_duration <- disrupt_range[2] - disrupt_range[1]

    disrupt_type <- input$disruptType
    #custom_rate <- ifelse(disrupt_type == "custom", input$customRate, 0.02)
    custom_rate <- switch(
      disrupt_type,
      "custom" = input$customRate,
      "constant_volume" = 0,
      "constant" = 0.02,
      0.02  # Default value if none match
    )
    
    print(disrupt_type)
    print(custom_rate)
    
    # Ensure params has data and is valid
    req(params)
    req(nrow(params) > 0)

    # Generate series_data and ensure it's valid
    series_data <- create_plot_data(params)
    req(series_data)
    req(nrow(series_data) > 0)
    req(!any(is.na(series_data$Year)), !any(is.na(series_data$Fitted_model)))

    bumps_plot_data <- create_bumps_data(params, disrupt_type, disrupt_start, disrupt_duration, custom_rate, policy_threshold_reactive(), threshold_criteria_reactive())

    # Ensure bumps_plot_data is valid
    req(bumps_plot_data)
    req(nrow(bumps_plot_data) > 0)

    axis_title <- gsub("^([^_]*)_.*$", "\\1", selected_variable_id_reactive())
    axis_title <- paste(threshold_Units_reactive(), " of ", axis_title)

    plot_title_p1 <- gsub("^([^_]*)_.*$", "\\1", current_policy_reactive())
    plot_title_p1 <- paste(plot_title_p1, "in", input$regionInput)

    plot_title_p2 <- ifelse(threshold_criteria_reactive() == ">=", "Change in required annual growth rate", "Change in required decarbonization rate")

    output$combinedPlotOutput <- renderPlotly({
      # Plot Original, Modified, and Blended Values
      p1 <- plot_ly(bumps_plot_data, x = ~Year) %>%
        add_lines(y = ~Original_V1, name = 'Non Disrupted', line = list(color = '#007CB0')) %>%
      #  add_lines(y = ~Modified_V1, name = 'Misses Policy', line = list(color = '#0096FF', dash = 'dash'), visible = 'legendonly') %>%
        add_lines(y = ~Blend_V1, name = 'Disrupted', line = list(color = '#D14956', dash = 'dot')) %>%
        layout(
          yaxis = list(title = axis_title, titlefont = list(size = 12), tickfont = list(size = 12),linecolor = '#292929', linewidth = 1),
          xaxis = list(title = "Time", titlefont = list(size = 12), tickfont = list(size = 12),linecolor = '#292929', linewidth = 1),
          legend = list(orientation = 'h', y = -0.2, x = 0.5, xanchor = 'center', font = list(size = 12)),
          margin = list(l = 70, r = 30, t = 60, b = 80),
          height = 300,  # Fixed height for the plot
          plot_bgcolor = "#F8F8F8",
          paper_bgcolor = "#F8F8F8",
          shapes = list(
            list(
              type = "rect",
              x0 = disrupt_start, x1 = disrupt_start + disrupt_duration,
              y0 = 0, y1 = 1,
              xref = "x", yref = "paper",
              fillcolor = "rgba(221, 229, 237, 0.75)",
              line = list(width = 0),
              layer = "below"
            )
          ),
          annotations = list(
            list(
              x = -0.1,
              y = 1.05,
              xref = "paper",
              yref = "paper",
              text = paste("<b>", plot_title_p1, "</b>"),
              showarrow = FALSE,
              font = list(
                size = 12,
                family = "Arial Nova Condensed, Univers Condensed, Arial, sans-serif",  # List of fonts to use
                color = '#292929' # Font color
              ),
              xanchor = "left",
              yanchor = "bottom"
            ),
            list(
              x = disrupt_start + disrupt_duration / 2,
              y = 1,
              xref = "x",
              yref = "paper",
              text = paste("Disruption", disrupt_duration, "years"),
              showarrow = FALSE,
              align = "center",
              bgcolor = "rgba(0, 0, 0, 0)",  # Transparent background
              bordercolor = "rgba(0, 0, 0, 0)",  # Transparent border
              borderwidth = 0
            )
          )
        )

      # Plot Original, Modified, and Blended CAGR
      p2 <- plot_ly(bumps_plot_data, x = ~Year) %>%
        add_bars(y = ~Original_CAGR, name = 'Non Disrupted', marker = list(color = '#007CB0')) %>%
       # add_bars(y = ~Modified_CAGR, name = 'Misses Policy', marker = list(color = '#0096FF'), visible = 'legendonly') %>%
        add_bars(y = ~Blend_CAGR, name = 'Disrupted', marker = list(color = '#D14956')) %>%
        layout(
          yaxis = list(title = "CAGR (%)", titlefont = list(size = 12), tickfont = list(size = 12), tickformat = ".0%", linecolor = '#292929', linewidth = 1),
          xaxis = list(title = "Time", titlefont = list(size = 12), tickfont = list(size = 12), linecolor = '#292929', linewidth = 1),
          legend = list(orientation = 'h', y = -0.2, x = 0.5, xanchor = 'center', font = list(size = 12)),
          barmode = 'group',
          margin = list(l = 70, r = 30, t = 60, b = 100),
          height = 300,  # Fixed height for the plot
          plot_bgcolor = "#F8F8F8",
          paper_bgcolor = "#F8F8F8",
          shapes = list(
            list(
              type = "rect",
              x0 = disrupt_start, x1 = disrupt_start + disrupt_duration,
              y0 = 0, y1 = 1,
              xref = "x", yref = "paper",
              fillcolor = "rgba(221, 229, 237, 0.75)",
              line = list(width = 0),
              layer = "below"
            )
          ),
          annotations = list(
            list(
              x = -0.1,
              y = 1,
              xref = "paper",
              yref = "paper",
              text =  paste("<b>", plot_title_p2, "</b>"),
              showarrow = FALSE,
              font = list(
                size = 12,
                family = "Arial Nova Condensed, Univers Condensed, Arial, sans-serif",  # List of fonts to use
                color = '#292929' # Font color
              ),
              xanchor = "left",
              yanchor = "bottom"
            )
          )
        )

      # convert axis to % if required
      if (threshold_Units_reactive() == "%") {
        p1 <- p1 %>%
          layout(
            yaxis = list(
              tickformat = ".0%",
              range = c(0, 1)
            )
          )
      }

      # Combine plots into a panel
      subplot(p1, p2, nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE) %>%
        layout(
          margin = list(t = 60, l = 70, r = 30, b = 120),
          height = 700,  # Fixed height for the entire subplot
          plot_bgcolor = "#F8F8F8",
          paper_bgcolor = "#F8F8F8",
          hovermode = "x unified"  # Enable compare data on hover
        )
    })
  })

  # Define the download handler for exporting pathways
  output$exportPathways <- downloadHandler(
    filename = function() {
      paste0("IPR_STAND_Pathways-", Sys.Date(), ".csv")  # Filename with date
    },
    content = function(file) {
      # **Step 1: Retrieve and Validate Inputs**
      new_forecasts <- Policies %>%
        filter(!is.na(Your_Response)) %>%

        # Pivot longer on all columns containing "Units"
        pivot_longer(
          cols = contains("Units"),
          names_to = "Units_Type",
          values_to = "Units"
        ) %>%

        # Select relevant columns
        select(Region, Policy, Variable_ID, Policy_Threshold, Threshold_Criteria, Units, Your_Response)

      # **Step 2: Apply create_pathways to Each Row and Collect Results**
        new_pathways_list <- new_forecasts %>%
        mutate(
          # Create a list-column by applying create_pathways to each row
          originalDataForPlot = pmap(
            list(Variable_ID, Region, Your_Response, Units),
            ~ create_pathways(..1, ..2, ..3, model_parameters, ..4)
          )
        ) %>%

        # Extract the list of dataframes
        pull(originalDataForPlot)

      # **Step 3: Bind All Dataframes into a Single Dataframe**
        new_pathways <- bind_rows(new_pathways_list) %>%
          distinct()


      # ** Step 4. Convert policy-level pathways to technology-level pathways
        tech_pathways <- create_tech_pathways(new_pathways)


        ###############################################################################
        # 1) Define interpolation function
        ###############################################################################
        interpolate_cost <- function(year, cost_2022, cost_2030, cost_2050) {
          
          # If any cost is missing, return NA
          if (is.na(cost_2022) || is.na(cost_2030) || is.na(cost_2050)) {
            return(NA_real_)
          }
          
          # Determine which cost to use or how to interpolate
          if (year <= 2022) {
            cost_2022
          } else if (year >= 2050) {
            cost_2050
          } else if (year <= 2030) {
            # Linear interpolation between 2022 and 2030
            cost_2022 + (year - 2022) * (cost_2030 - cost_2022) / (2030 - 2022)
          } else {
            # Linear interpolation between 2030 and 2050
            cost_2030 + (year - 2030) * (cost_2050 - cost_2030) / (2050 - 2030)
          }
        }
        
        ###############################################################################
        # 2) Filter tech pathways for electricity generation capacity
        ###############################################################################
        tech_pathways_filtered <- tech_pathways %>%
          filter(str_detect(Variable_ID, "Capacity_Electricity generation")) %>%
          mutate(
            # Extract the string between the 2nd and 3rd underscores
            # e.g., "Capacity_Electricity generation_Coal_Power" -> "Coal"
            Sub_variable_class_2 = sapply(
              strsplit(Variable_ID, "_"),
              function(x) x[3]
            )
          )
        
        if(nrow(tech_pathways_filtered) != 0) { 
        
        ###############################################################################
        # 3) Join with retirement rates and cost data
        ###############################################################################
        tech_pathways_joined <- tech_pathways_filtered %>%
          left_join(
            power_cost_retirement %>%
              select(
                Sub_variable_class_2,
                Retirement_Rate, 
                Cost_per_MW_2022,
                Cost_per_MW_2030,
                Cost_per_MW_2050
              ),
            by = "Sub_variable_class_2"
          )
        
        ###############################################################################
        # 4) Compute Net Additions
        ###############################################################################
        tech_pathways_net_additions <- tech_pathways_joined %>%
          group_by(Region, Units, Variable_ID) %>%
          arrange(Year) %>%
          mutate(
            # difference from previous year
            Value_net = Value - lag(Value),
            
            # replace negative or NA with 0
            Value_net = ifelse(is.na(Value_net) | Value_net < 0, 0, Value_net),
            
            # create a new Variable_ID
            Variable_ID_net = str_replace(Variable_ID, "Capacity", "Net Additions")
          ) %>%
          ungroup() %>%
          select(
            Year,
            Value = Value_net,
            Variable_ID = Variable_ID_net,
            Region,
            Units
          )
        
        ###############################################################################
        # 5) Compute Retirements
        ###############################################################################
        tech_pathways_retirements <- tech_pathways_joined %>%
          mutate(
            # multiply the Capacity by Retirement_Rate
            Value_retirement = Value * Retirement_Rate,
            
            # create a new Variable_ID
            Variable_ID_ret = str_replace(Variable_ID, "Capacity", "Retirements")
          ) %>%
          select(
            Year,
            Value = Value_retirement,
            Variable_ID = Variable_ID_ret,
            Region,
            Units
          )
        
        ###############################################################################
        # 6) Create New Additions by summing Net Additions & Retirements
        ###############################################################################
        tech_pathways_new_additions <- bind_rows(
          tech_pathways_net_additions,  # "Net Additions" rows
          tech_pathways_retirements     # "Retirements" rows
        ) %>%
          mutate(
            # Convert the "Net Additions" or "Retirements" back to "Capacity" so these can be grouped
            Variable_ID_common = str_replace(Variable_ID, "Net Additions|Retirements", "Capacity")
          ) %>%
          group_by(Year, Region, Units, Variable_ID_common) %>%
          summarize(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
          mutate(
            # Now change "Capacity" -> "New deployment"
            Variable_ID = str_replace(Variable_ID_common, "Capacity", "New deployment")
          ) %>%
          select(
            Year,
            Value,
            Variable_ID,
            Region,
            Units
          )
        
        ###############################################################################
        # 7) Create Capex data for power capacity
        ###############################################################################
        tech_pathways_capex <- tech_pathways_joined %>%
          mutate(
            # interpolate the cost for each row
            Cost_interpolated = mapply(
              interpolate_cost,
              year       = Year,
              cost_2022  = Cost_per_MW_2022,
              cost_2030  = Cost_per_MW_2030,
              cost_2050  = Cost_per_MW_2050
            ),
            # multiply the Capacity (in MW) by cost (million $/MW) * 1000 for total $ (million)
            Value_capex = Value * Cost_interpolated * 1000,
            
            # replace "Capacity" with "Capex" in the Variable_ID
            Variable_ID_capex = str_replace(Variable_ID, "Capacity", "Capex"),
            
            # set Units to "USD (million)"
            Units = "USD (million)"
          ) %>%
          select(
            Year,
            Value = Value_capex,
            Variable_ID = Variable_ID_capex,
            Region,
            Units
          )
        }
        
        ###############################################################################
        # 8) Process auto capex data
        ###############################################################################
        auto_tech_pathways_clean <- tech_pathways %>%
          separate(
            col   = Variable_ID,
            into  = c("temp1", "temp2", "Sub_variable_class_2", "temp3", "Sub_Sector"),
            sep   = "_",
            remove = FALSE,
            extra  = "merge",  # merges any extra pieces into the last column
            fill   = "right"   # if there are fewer pieces, fill with NA
          ) %>%
          filter(temp2 == "Vehicles") %>%
          select(
            Year,
            Region,
            Variable_ID,
            Value,
            Units,
            Sub_variable_class_2,
            Sub_Sector
          ) %>%
          # Safely convert to character in case these columns are lists
          mutate(
            Sub_variable_class_2 = as.character(Sub_variable_class_2),
            Sub_Sector           = as.character(Sub_Sector)
          )
        
        if(nrow(auto_tech_pathways_clean != 0)) {
          
        # Join with transport_costs
          auto_tech_pathways_clean <- auto_tech_pathways_clean %>%
          left_join(transport_costs, by = c("Sub_variable_class_2", "Sub_Sector"))
        
        # Interpolate row-wise using mapply() and create final capex output
        auto_capex <- auto_tech_pathways_clean %>%
          mutate(
            Cost_interpolated = mapply(
              interpolate_cost,
              year       = Year,
              cost_2022  = Cost_per_Vehicle_2022,
              cost_2030  = Cost_per_Vehicle_2030,
              cost_2050  = Cost_per_Vehicle_2050
            ),
            # Multiply Value by the interpolated cost
            Value_capex = Value * Cost_interpolated,
            
            # Replace "New deployment" with "Capex" in the Variable_ID
            Variable_ID_capex = str_replace(Variable_ID, "New deployment", "Capex"),
            
            # Update units
            Units = "USD (million)"
          ) %>%
          select(
            Year,
            Value = Value_capex,
            Variable_ID = Variable_ID_capex,
            Region,
            Units
          )
        }
        
        
        ###############################################################################
        # 9) Combine results for saving
        ###############################################################################
        if (exists("tech_pathways_new_additions")) {
          # If 'tech_pathways_new_additions' is available, add it along with the capex data
          new_pathways <- rbind(
            new_pathways,
            tech_pathways,
            tech_pathways_new_additions,
            tech_pathways_capex
          )
        } else {
          new_pathways <- rbind(new_pathways, tech_pathways)
        }
        
        if (exists("auto_capex")) {
          new_pathways <- rbind(new_pathways, auto_capex)
        }
        
        
          
      # Convert to wide format to reduce file size
       new_pathways_wide <- new_pathways %>%
         pivot_wider(
           names_from = Year,
           values_from = Value
         )



      # **Step 5: Write the Dataframe to the Specified File**
      write.csv(new_pathways_wide, file, row.names = FALSE)
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
