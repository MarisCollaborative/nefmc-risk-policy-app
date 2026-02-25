
# Server process for Risk Policy Shiny App ####

## Environment set-up #### ============================================================
library(shiny)
library(tidyverse)
library(here)
library(gt)
library(DT)
library(surveydown)
library(shinyjs)
source(here("helpers.R")) # read in helper functions
# connect to the database for fetching data
db <- surveydown::sd_db_connect()

# Server #### =========================================================================
server <- function(input, output, session) {

## Server Parameters #### =============================================================
# create reactive element for selected year 
year <- reactive(input$year)
  
# create reative element for selected FMP 
fmp <- reactive(input$fmp)

# when a user selects an fmp
observeEvent(input$fmp, {
  # Filter choices for the stocks 
  choices_to_show <- nefishr::nefmc_species |>
    dplyr::filter(FMP == fmp()) |>
    dplyr::pull(STOCK_NAME) |>
    unique() |> 
    sort()
  # update the stock selection drop down with only the choices for a given fmp
  updateSelectInput(session, "stock", choices = c("Select a stock...", choices_to_show))
})

# create reactive element for selected stock
stock <- reactive(input$stock)

## Fetch data #### ====================================================================
## get risk policy data from the database
# a reactive object containing results from the matrix survey
info <- sd_get_data(db,
                      table = "rp-matrix-tbl",
                      refresh_interval = 30) 

# a static object containing results from the weightings survey
weights <- sd_get_data(db, table = "rp-weights") |> 
    clean_weights() # uses helper function to tidy the data and columns 
  
# a static object containing results from the scoring survey
scores <- sd_get_data(db, table = "rp-scores") |> 
    clean_scores() |> # uses helper function to tidy the data and columns 
    filter(!factor %in% str_subset(factor, "rationale")) # removes observations containing rationale for a given score

# create a static data frame containing the scores and weights for each factor
z_data <- left_join(scores, weights, by = c("report_year", "factor"))

# create a reactive value for later manipulation and restoration
zdata_rv <- reactiveValues(original = z_data, 
                           updated = z_data)
                      
  
## Page 1: Matrix Output #### ========================================================
# create reactive for the matrix 
matrix_tbl  <- reactive({

  info() |> 
      clean_matrix() |> # helper function to tidy the data and columns 
      filter(report_year == year() & stock == stock()) |> # filter for user inputs
      select(!c(report_year, stock)) # remove stock and year from table once filtered
  
})

# Render a GT table using the reactive info object containing the answers from the matrix survey
output$matrix <- render_gt({ 

     matrix_tbl() |>
      gt(rowname_col = "value", 
         groupname_col = "factor", # group rows based on the factor column
         row_group_as_column = TRUE) |> 
      text_case_match(
        NA ~ "Not provided", # where there is an NA replace with "Not provided"
        .locations = cells_body(answer) # in the answer column
      ) |>       
      tab_header(title = str_c(year(), "Risk Policy Matrix for", stock(), sep = " ")) |> # create a table header using the user inputs
      opt_align_table_header(align = "left") 
  
})


## Page 2: Scores, Weights, Plots #### ================================================
### Initial Reactives ####==============================================================
# create reactive objects based on slider inputs for each factors; stores the value from the slider 
biomass <- reactive(input$changeBiomass)
recruitment <- reactive(input$changeRecruitment)
climate <- reactive(input$changeClimate)
commercial <- reactive(input$changeCommercial)
recreational <- reactive(input$changeRecreational)
rationale <- reactive(input$rationale)
  
  
### Score Manipulation ####================================================================
# when the "Make Changes" button is pressed, the following operation is performed
observeEvent(input$changeScores, {
  #1. Create an object from the Updated Reactive Value
  updated <- zdata_rv$updated |> 
    filter(report_year == year(), stock == stock()) # filter for the user inputs for year and stock 
      
  #2. Update the each score cell based on the user input slider values above
  ### Biomass score
  updated[updated$factor=="biomass", "score"] <- updated[updated$factor=="biomass", "score"] + biomass()
  ### Recruitment score
  updated[updated$factor=="recruitment", "score"] <- updated[updated$factor=="recruitment", "score"] + recruitment()
  ### Climate score
  updated[updated$factor=="climate", "score"] <- updated[updated$factor=="climate", "score"] + climate()
  ### Commercial Fishery score
  updated[updated$factor=="commercial", "score"] <- updated[updated$factor=="commercial", "score"] + commercial()
  ### Recreational Fishery score
  updated[updated$factor=="recreational", "score"] <- updated[updated$factor=="recreational", "score"] + recreational()
    
  #3. Rescale the scores based on the updated values from #2 
  updated$scaled_score <- scale_val(updated$score, 4) # helper function
    
  #4. Overwrite the "Updated Reactive value" with the manipulated data
  zdata_rv$updated <- updated
})  
  
# when the "Reset Scores" button is pressed, the following operation is performed
observeEvent(input$resetScores, {
    
    #1. Overwrite the "Updated Reactive value" with the "Original Reactive value"
    zdata_rv$updated <- zdata_rv$original |> 
      filter(report_year == year(), stock == stock()) # filtered by user inputs for year and stock
  
    #2. Reset the sliders to 0 
    shinyjs::reset("changeBiomass")
    shinyjs::reset("changeRecruitment")
    shinyjs::reset("changeClimate")
    shinyjs::reset("changeCommercial")
    shinyjs::reset("changeRecreational")
  
})

  
### Final Reactives #### ================================================================
# Using the "Updated Reactive Value" (regardless of it's state), create a reactive object
zscore_vals <- reactive({
  zdata_rv$updated |> 
    filter(report_year == year(), stock == stock()) |> # filtered by user inputs for year and stock, and
    summarise(zscore = calc_zscore(scaled_score, normalized_weight), # calculate the zscore using a helper function, and
              rec_prob = calc_recprob(zscore)) # the recommended probablity using a helper function

})

# Using the "Updated Reactive Value" (regardless of it's state), create a data reactive that can be used in the shiny output and report
final_scores <- reactive({ 
  zdata_rv$updated |> 
    filter(report_year == year(), stock == stock()) |> # filtered by user inputs for year and stock
    select(!c(scaled_score, normalized_weight)) |> 
    gt()
})
  
#  Using the zscore_vals reactive object, pull out the zscore value and save in its own reactive for the app and report
zscore <- reactive({
  zscore_vals()$zscore
})

#  Using the zscore_vals reactive object, pull out the recommended probability value and save in its own reactive for the app and report
RecProb <- reactive({
    str_c( # creating a string that includes: 
      round( # the rounded product of 
        zscore_vals()$rec_prob*100, # the rec_prob value multiplied by 100
        1 # to the nearest tenth,
      ),
      "%", # and a percent sign,  
      sep = "") # without any separating space or punctuation
})

#  Using the zscore_vals reactive object, create a reactive plot that plots the score and recommended probability
zplot <- reactive({
  plot_zscore(data = zscore_vals(), # helper function for plotting the z-score function
    xcol = zscore, # and values
    ycol = rec_prob)
})


## Outputs ####
  
# Render the GT table output using the data reactive
output$scores <- render_gt({

   final_scores() 
    
})

# Print the z-score value 
output$zscore <- renderText(

  zscore()

  )

# Print the recommended probability value by
output$RecProb <- renderText(
    
  RecProb()

)

# Plot the z-score and recommended probability values
output$zplot <- renderPlot({

  zplot()
  
})
  
## Report #### =====================================================================
# create a temporary file location
report_path <- tempfile(fileext = ".qmd")

## copy the RMD file in the repo to the temporary file location and overwrite if already existing
file.copy("rp_report_template.Rmd", report_path, overwrite = TRUE)
  
report_name <- reactive({
  stringr::str_replace(stock(), pattern = "[:space:]", replace = "_") |> 
  stringr::str_c("_rp-report_", year(), ".pdf", sep = "")
})

output$report <- downloadHandler(
  filename = function() {
      report_name()
    }, 
  content = function(file) {
    # Use withProgress to show a progress bar
    withProgress(message = "Creating Report: ", value = 0, {

    # Stage 1: Increment progress
    incProgress(0.1, detail = "Collecting inputs...")

    # Generate the Quarto params
    params <- list(year = year(), 
                   stock = stock(), 
                  #  fmp = fmp(),
                   matrix_tbl = matrix_tbl(),
                   scores = final_scores(), 
                   zscore = zscore(), 
                   RecProb = RecProb(), 
                   zplot = zplot(), 
                   rationale = rationale()
                  )
    # debug params
    print("Parameters for RMD render:")
    print(params)

    # debug file path
    # print(paste("Temporary output file path:", temp_output_dir))
    print(paste("file name path:", report_name()))
    print(paste("Final output file path:", file))
    # quarto_file <- normalizePath(here("draft_report.qmd", mustWork = TRUE))
    # Temporarily switch to a temp directory to avoid write permission issues
    # and ensure unique file generation for concurrent users
     incProgress(0.2, detail = "Building...")
        
        tryCatch({
          
          render_report(input = report_path, output = file, params = params)
          
          
          incProgress(0.95, detail = "Downloading report...")
          
          # Copy the generated file to the correct location for download
          # file.copy(report_name(), file, overwrite = TRUE)
          
        }, error = function(e) {
          
          print(paste("Error generating RMD report:", e$message))
          
        })
      }
      )
    }
  )
}

