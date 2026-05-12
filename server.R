
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
# disable report button upon start up
observe({
  shinyjs::disable(id = "report")
})

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

observe({
  if (req(input$stock) != "Select a stock...") {
    enable("report")
  }
})

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
      select(!c(report_year, stock)) |>  # remove stock and year from table once filtered
      arrange(factor) # arrange the table based on the assigned levels of the 'factor' column
})

# Render a GT table using the reactive info object containing the answers from the matrix survey
output$matrix <- render_gt({
  
  # does not show the matrix until the inputs are selected
   if (input$year == "Select a year..." || input$stock == "Select a stock...") {
    return(NULL)
  }

    matrix_tbl() |>
      gt(rowname_col = "value", 
         groupname_col = "factor", # group rows based on the factor column
         row_group_as_column = TRUE) |> 
      text_case_match(
        NA ~ "Not provided", # where there is an NA replace with "Not provided"
        .locations = cells_body(answer) # in the answer column
      ) |> 
      cols_label(
        answer = md("Supporting Information")
      ) |>
      tab_header(title = str_c(year(), "Risk Policy Matrix for", stock(), sep = " ")) |> # create a table header using the user inputs
      opt_align_table_header(align = "left") 
  
})


## Page 2: Scores, Weights, Plots #### ================================================
### Initial Reactives ####==============================================================
# create reactive objects based on slider inputs for each factors; stores the value from the slider 
# biomass <- reactive(input$changeBiomass*2)
# recruitment <- reactive(input$changeRecruitment*2)
# climate <- reactive(input$changeClimate)
# commercial <- reactive(input$changeCommercial)
# recreational <- reactive(input$changeRecreational)
# rationale <- reactive(input$rationale)
  
  
### Score Manipulation ####================================================================
# when the "Make Changes" button is pressed, the following operation is performed
# observeEvent(input$changeScores, {
#   #1. Create an object from the Updated Reactive Value
#   updated <- zdata_rv$updated |> 
#     filter(report_year == year(), stock == stock()) # filter for the user inputs for year and stock 
      
#   #2. Update the each score cell based on the user input slider values above
#   ### Biomass score
#   updated[updated$factor=="biomass", "score"] <- updated[updated$factor=="biomass", "score"] + biomass()
#   ### Recruitment score
#   updated[updated$factor=="recruitment", "score"] <- updated[updated$factor=="recruitment", "score"] + recruitment()
#   ### Climate score
#   updated[updated$factor=="climate", "score"] <- updated[updated$factor=="climate", "score"] + climate()
#   ### Commercial Fishery score
#   updated[updated$factor=="commercial", "score"] <- updated[updated$factor=="commercial", "score"] + commercial()
#   ### Recreational Fishery score
#   updated[updated$factor=="recreational", "score"] <- updated[updated$factor=="recreational", "score"] + recreational()
    
#   #3. Rescale the scores based on the updated values from #2 
#   updated$scaled_score <- scale_val(updated$score, 4) # helper function
    
#   #4. Overwrite the "Updated Reactive value" with the manipulated data
#   zdata_rv$updated <- updated
# })  
  
# when the "Reset Scores" button is pressed, the following operation is performed
# observeEvent(input$resetScores, {
    
#     #1. Overwrite the "Updated Reactive value" with the "Original Reactive value"
#     zdata_rv$updated <- zdata_rv$original |> 
#       filter(report_year == year(), stock == stock()) # filtered by user inputs for year and stock
  
#     #2. Reset the sliders to 0 
#     shinyjs::reset("changeBiomass")
#     shinyjs::reset("changeRecruitment")
#     shinyjs::reset("changeClimate")
#     shinyjs::reset("changeCommercial")
#     shinyjs::reset("changeRecreational")
  
# })

  
### Final Reactives #### ================================================================
original_zvals <- reactive({
  zdata_rv$original |> 
    filter(report_year == year(), stock == stock()) |> # filtered by user inputs for year and stock, and
    summarise(zscore = calc_zscore(scaled_score, normalized_weight), # calculate the zscore using a helper function, and
              # alpha_recprob = alpha_recprob(zscore), 
              RecProb= calcRecProb(zscore))#,  # calculate the recommended probability using the logistic function
    #           perc.diff = percent.diff(alpha_recprob, beta_recprob)) |> 
    # mutate(alpha_recprob = case_when(
    #   alpha_recprob < 0.5 ~ 0.5, 
    #   TRUE ~ alpha_recprob)
    # )
})

# Using the "Updated Reactive Value" (regardless of it's state), create a reactive object
zscore_vals <- reactive({
  zdata_rv$updated |> 
    filter(report_year == year(), stock == stock()) |> # filtered by user inputs for year and stock, and
    summarise(zscore = calc_zscore(scaled_score, normalized_weight), # calculate the zscore using a helper function, and
              # alpha_recprob = alpha_recprob(zscore), 
              RecProb = calcRecProb(zscore))#,  # calculate the recommended probability using the logistic function
              # perc.diff = percent.diff(alpha_recprob, beta_recprob)) #|> # the recommended probablity using a helper function
    # mutate(alpha_recprob = case_when(
    #   alpha_recprob < 0.5 ~ 0.5, 
    #   TRUE ~ alpha_recprob)
    # )
  # map(zdata_rv, 
  #   ~filter(., report_year == year(), stock == stock()) |> # filtered by user inputs for year and stock, and
  #   summarise(zscore = calc_zscore(scaled_score, normalized_weight), # calculate the zscore using a helper function, and
  #             alpha_recprob = alpha_recprob(zscore), 
  #             beta_recprob = beta_recprob(zscore), 
  #             perc.diff = percent.diff(alpha_recprob, beta_recprob), # the recommended probablity using a helper function
  #             .by = c("report_year", "stock"))
  # )

})

# Using the "Updated Reactive Value" (regardless of it's state), create a data reactive that can be used in the shiny output and report
final_scores <- reactive({ 
  zdata_rv$updated |> 
    filter(report_year == year(), stock == stock()) |> # filtered by user inputs for year and stock
    select(!c(scaled_score, normalized_weight)) |> 
    gt() |> 
    text_transform(str_to_title, locations = cells_body(columns = factor)) |> 
    cols_label(
      avg_weight = "Average Weight"
    ) |> 
    cols_label_with(
    fn = function(x) {
      janitor::make_clean_names(x, case = "title") |>
        stringr::str_replace_all("_", " ") |>
        md()
    }
  ) |> 
    cols_align(align = "left", 
               columns = c(report_year, stock, factor)) |> 
    cols_align(align = "right", 
               columns = c(score, avg_weight))
})
  
#  Using the zscore_vals reactive object, pull out the zscore value and save in its own reactive for the app and report
zscore <- reactive({
  zscore_vals()$zscore
  # zscore_vals()$updated$zscore
})

#  Using the zscore_vals reactive object, pull out the recommended probability value and save in its own reactive for the app and report
# alpha_prob <- reactive({
#     str_c( # creating a string that includes: 
#       round( # the rounded product of 
#         zscore_vals()$alpha_recprob*100, # the rec_prob value multiplied by 100
#         #  zscore_vals()$updated$alpha_recprob*100, 
#         1 # to the nearest tenth,
#       ),
#       "%", # and a percent sign,  
#       sep = "") # without any separating space or punctuation
# })
  
RecProb <- reactive({
    str_c( # creating a string that includes: 
      round( # the rounded product of 
        zscore_vals()$RecProb*100, # the rec_prob value multiplied by 100
        #  zscore_vals()$updated$beta_recprob*100, 
        1 # to the nearest tenth,
      ),
      "%", # and a percent sign,  
      sep = "") # without any separating space or punctuation
})

# prob_diff <- reactive({
#     str_c( # creating a string that includes: 
#       round( # the rounded product of 
#         zscore_vals()$perc.diff*100, # 
#         #  zscore_vals()$updated$perc.diff*100, 
#         1 # to the nearest tenth,
#       ),
#       "%", # and a percent sign,  
#       sep = "") # without any separating space or punctuation
# })

#  Using the zscore_vals reactive object, create a reactive plot that plots the score and recommended probability
# alpha_plot <- reactive({

#   plot_alpha(data = original_zvals(), # helper function for plotting the z-score function
#     # data = zscore_vals()$updated,
#     xcol = zscore, # and values
#     ycol = alpha_recprob, 
#     color = "gray") + 
#     ggplot2::geom_point(data = zscore_vals(), 
#   aes(x = zscore, y = alpha_recprob), color = "#3e9eb6", size = 4) 
# })

# ab_plot <- reactive({
#   plot_abprob(data = zscore_vals(), # helper function for plotting the z-score function
#     # data = zscore_vals()$updated,
#     z = zscore, # and values
#     alpha = alpha_recprob, 
#     beta = beta_recprob) #+
#     # labs(subtitle = "This plot compares the differences between recommended probabilities that were calculated based on the logistic curve approved in the\nAlpha phase of the Risk Policy, and the logistic cuve that is being considered in the Beta phase of the Risk Policy.") + 
#     # theme(plot.subtitle = element_text(size = 14))
# })
  
RecProb_plot <- reactive({
  plotRecProb(data = zscore_vals(), 
              z = zscore, 
              RecProb = RecProb)
})

## Outputs ####
  
# Render the GT table output using the data reactive
output$scores <- render_gt({
  
  if (input$year == "Select a year..." || input$stock == "Select a stock...") {
    return(NULL)
  }

   final_scores() |> 
    tab_options(table.width = pct(80))
    
})

# Print the z-score value 
output$zscore <- renderText(

  zscore()

  )

# Print the recommended probability value by
# output$AlphaProb <- renderText(
    
#   alpha_prob()

# )

# Plot the z-score and recommended probability values
# output$alpha_plot <- renderPlot({

#   alpha_plot()
  
# })
  
# output$ab_plot <- renderPlot({

#   ab_plot()
  
# })
  
# output$BetaProb <- renderText(
    
#   beta_prob()

# )
  
output$RecProb_plot <- renderPlot(
    
  RecProb_plot()

)
  
output$RecProb <- renderText(
    
  RecProb()

)

# output$PercDiff <- renderText(
    
#   prob_diff()

# )
  
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
                   RecProb = alpha_prob(), 
                   zplot = alpha_plot(), 
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

