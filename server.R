
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
# Render a GT table using the reactive info object containing the answers from the matrix survey
output$matrix <- render_gt({ 

    info() |> 
      clean_matrix() |> # helper function to tidy the data and columns 
      filter(report_year == year() & stock == stock()) |> # filter for user inputs
      select(!c(report_year, stock)) |> # remove stock and year from table once filtered
      # replace any underscores in the value and answer columns with spaces
      mutate(value = str_to_title(str_replace_all(value, "_", " ")), 
             answer = str_to_sentence(str_replace_all(answer, "_", " "))) |>
      gt(rowname_col = "value", 
         groupname_col = "factor", # group rows based on the factor column
         row_group_as_column = TRUE) |>
      tab_header(title = str_c(year(), "Risk Policy Matrix for", stock(), sep = " ")) |> # create a table header using the user inputs
      text_case_match(
        NA ~ "Not provided", # where there is an NA replace with "Not provided"
        .locations = cells_body(answer) # in the answer column
      ) 
   
  })


## Page 2: Scores, Weights, Plots #### ================================================
# create reactive objects based on slider inputs for each factors; stores the value from the slider 
biomass <- reactive(input$changeBiomass)
recruitment <- reactive(input$changeRecruitment)
climate <- reactive(input$changeClimate)
commercial <- reactive(input$changeCommercial)
recreational <- reactive(input$changeRecreational)

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
  updated$scaled_score <- scale_score(updated$score) # helper function
    
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

  
### Final outputs #### ================================================================
# Using the "Updated Reactive Value" (regardless of it's state), create a reactive object
zscore_vals <- reactive({
  zdata_rv$updated |> 
    filter(report_year == year(), stock == stock()) |> # filtered by user inputs for year and stock, and
    summarise(zscore = calc_zscore(scaled_score, normalized_weight), # calculate the zscore using a helper function, and
              rec_prob = calc_recprob(zscore)) # the recommended probablity using a helper function

})
  
# Using the "Updated Reactive Value" (regardless of it's state), render the GT table output 
output$scores <- render_gt({

  zdata_rv$updated |> 
    filter(report_year == year(), stock == stock()) |> # filtered by user inputs for year and stock
    gt()
    
})

# Using the zscore_vals reactive object, print the z-score value 
output$zscore <- renderText(

  zscore_vals()$zscore

  )

# Using the zscore_vals reactive object, print the recommended probability value by
output$RecProb <- renderText(
    
    str_c( # creating a string that includes: 
      round( # the rounded product of 
        zscore_vals()$rec_prob*100, # the rec_prob value multiplied by 100
        1 # to the nearest tenth,
      ),
      "%", # and a percent sign,  
      sep = "") # without any separating space or punctuation

)

# Using the zscore_vals reactive object, plot the z-score and recommended probability values
output$zplot <- renderPlot({

 plot_zscore(data = zscore_vals(), # helper function for plotting the z-score function
  xcol = zscore, # and values
  ycol = rec_prob)
  
})
  


}