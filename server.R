
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

# disable report button upon start up
observe({
  shinyjs::disable(id = "report")
})

## Fetch data #### ====================================================================
## get risk policy data from the database
# a reactive object containing results from the matrix survey
info <- sd_get_data(db,
                      table = "rp_matrix_tbl",
                      refresh_interval = 30) 

# a static object containing results from the weightings survey
weights <- sd_get_data(db, table = "rp-weights") |> 
    clean_weights() # uses helper function to tidy the data and columns 

  
# a static object containing results from the scoring survey
raw_score_data <- sd_get_data(db, table = "rp_scores_dev") 
all_score_info <- raw_score_data |> get_score_info()
scores <- raw_score_data |> 
    clean_scores() # uses helper function to tidy the data and columns 

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
  
# when a user selects a stock 
observeEvent(input$stock, {
  #Filter choices for the dates 
  dates_drafted <- scores |> 
    dplyr::filter(stock == stock()) |> 
    dplyr::pull(draft_date) |> 
    unique() |> 
    sort()

  # update the stock selection drop down with only the choices for a given fmp
  updateSelectInput(session, "draft_date", choices = c("Select a date...", dates_drafted))
})

date_drafted <- reactive(input$draft_date)
    
# only allow the "Generate report button" to be active if a stock has been selected.
observe({
  if (req(input$draft_date) != "Select a date...") {
    enable("report")
  }
})
                      
  
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
# create a static data frame containing the scores and weights for each factor
z_data <- left_join(scores, weights, by = c("report_year", "factor")) 

# create a reactive value for later manipulation and restoration
zdata_rv <- reactiveValues(original = z_data, 
                           updated = z_data)
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
    filter(report_year == year(), stock == stock(), draft_date == date_drafted()) |> # filtered by user inputs for year and stock, and
    mutate(normalized_weight = round(normalize_val(avg_weight), 2)) |>
    summarise(zscore = calc_zscore(score, normalized_weight), # calculate the zscore using a helper function, and
              RecProb= calcRecProb(zscore))  # calculate the recommended probability using the logistic function
})

# Using the "Updated Reactive Value" (regardless of it's state), create a reactive object
zscore_vals <- reactive({
  zdata_rv$updated |> 
    filter(report_year == year(), stock == stock(), draft_date == date_drafted()) |> # filtered by user inputs for year and stock, and
    mutate(normalized_weight = round(normalize_val(avg_weight), 2)) |>
    summarise(zscore = calc_zscore(score, normalized_weight), # calculate the zscore using a helper function, and
              RecProb = calcRecProb(zscore))  # calculate the recommended probability using the logistic function

})

# Using the "Updated Reactive Value" (regardless of it's state), create a data reactive that can be used in the shiny output and report
final_scores <- reactive({ 
  zdata_rv$updated |> 
    filter(report_year == year(), stock == stock(), draft_date == date_drafted()) |> # filtered by user inputs for year and stock
    mutate(normalized_weight = round(normalize_val(avg_weight), 2)) |>
    select(!c(normalized_weight, staff_name)) |> 
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
    cols_align(align = "center", 
               columns = c(draft_date, report_year)) |> 
    cols_align(align = "left", 
               columns = c(stock, factor)) |> 
    cols_align(align = "right", 
               columns = c(score, avg_weight))
})
  
#  Using the zscore_vals reactive object, pull out the zscore value and save in its own reactive for the app and report
zscore <- reactive({
  zscore_vals()$zscore
  # zscore_vals()$updated$zscore
})
  
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
  
TierArea <- reactive({
  if(zscore_vals()$RecProb <= 0.61){ # if the recommended probability value is less than or equal to 0.61
    paste0("High Risk Tolerance") # then this falls into the High Risk Tolerance Zone
  } else if(zscore_vals()$RecProb >= 0.89){ # if the recommended probability value is greater than or equal to 0.89
    paste0("Low Risk Tolerance") # then this falls into the Low Risk Tolerance Zone
  } else {
    paste0("Intermediate Risk Tolerance") # otherwise, all other values are within the Intermediate Risk Tolerance Zone
  }
})

RecProb_plot <- reactive({
  plotRecProb(data = zscore_vals(), 
              z = zscore, 
              RecProb = RecProb, 
              size = 3)
})

## Outputs ####
  
# Render the GT table output using the data reactive
output$scores <- render_gt({
  
  if (input$year == "Select a year..." || input$stock == "Select a stock...") {
    return(NULL)
  }

   final_scores() |> 
    tab_options(table.width = pct(100))
    
})

# Print the z-score value 
output$zscore <- renderText(

  zscore()

  )


output$plot <- renderPlot(
    
  RecProb_plot() + 
    ggplot2::theme(
      legend.position = "right",
      axis.title = element_text(size = rel(1)),
      axis.text = element_text(size = rel(1)),
      legend.text = element_text(size = rel(1))
    ) +
    ggplot2::coord_fixed(ratio = 8)

)
  
output$RecProb <- renderText(
    
  RecProb()

)
  
output$ClassifyZone <- renderText(
  TierArea()
)

  
## Report #### =====================================================================

file_name <- reactive({
  stock_name <- stringr::str_replace_all(stock(), pattern = "[:space:]", replace = "_") 
  
  date <- stringr::str_replace_all(date_drafted(), pattern = "-", replace = "")

  stringr::str_c("DRAFT_", stock_name, "_rp-report_", date, sep = "")
})

output$report <- downloadHandler(
  # 1. Define the filename
  filename = function() {
      str_c(file_name(), "zip", sep=".")
    }, 
  # generate the files and compress them
  content = function(file) {    
    # Use withProgress to show a progress bar
    withProgress(message = "Bundling Report: ", value = 0, {

    # Stage 1: Increment progress
    incProgress(0.1, detail = "Collecting inputs...")
    
    # 1. Create AN ISOLATED TEMP DIRECTORY INSIDE THE HANDLER
    report_path <- tempdir()
    dir.create(report_path, showWarnings = FALSE)

    # 2. DEFINE AND EXECUTE COPIES INSIDE THE HANDLER
    # Define paths for Rmd files
    pdf_src <- normalizePath("rp_report_template.Rmd", mustWork = TRUE)
    ppt_src <- normalizePath("rp_ppt_template.Rmd", mustWork = TRUE)
    ppt_ref_src <- normalizePath("NEFMC_PPT_MASTER_2026.pptx", mustWork = TRUE)  
      
    # Define temporary paths for the two individual files
    tmp_pdf <- file.path(report_path,"rp_report_template.Rmd")
    tmp_ppt <- file.path(report_path, "rp_ppt_template.Rmd")  
    tmp_pptx_ref <-  file.path(report_path, "NEFMC_PPT_MASTER_2026.pptx")
      
    ## copy the RMD files in the repo to the temporary file locations and overwrite if already existing
    file.copy(from = pdf_src, to = tmp_pdf, overwrite = TRUE)
    file.copy(from = ppt_src, to = tmp_ppt, overwrite = TRUE)
    file.copy(from = ppt_ref_src, to = tmp_pptx_ref, overwrite = TRUE)

    # Generate the RMarkdown params
    params <- list(year = year(), 
                   stock = stock(), 
                   draft_date = date_drafted(),
                   fmp = fmp(),
                  # matrix_tbl = matrix_tbl(),
                   weights = weights,
                   all_score_info = all_score_info,
                   scores = final_scores(), 
                   zscore = zscore(), 
                   RecProb = RecProb(), 
                   plot_data = zscore_vals(),
                  #  plot = RecProb_plot(), 
                   tier = TierArea()
                  #  rationale = rationale()
                  )
    # debug params
    print("Parameters for RMD render:")
    print(params)

    # debug file path
    print(paste("file name path:", file_name()))
    print(paste("Final output file path:", file))
    
     incProgress(0.2, detail = "Building...")
        
        tryCatch({

          # Create file names for each document 
          pdf_filename  <- stringr::str_c(file_name(), "pdf", sep=".")
          pptx_filename <- stringr::str_c(file_name(), "pptx", sep=".")
          
          # Render the documents in their respective temporary locations
          render_report(input = tmp_pdf, output = pdf_filename, params = params)
          render_report(input = tmp_ppt, output = pptx_filename, params = params)#,
            #  output_format = rmarkdown::powerpoint_presentation(reference_doc = tmp_pptx_ref)) 
            
          # 3. FIX WORKING DIRECTORY AND ZIP PATHS
          # Temporarily change working directory to report_path for clean zipping paths
          # Store original working directory
          owd <- getwd()

          # Ensure we return to the original working directory even if zipping fails
          on.exit(setwd(owd), add = TRUE)

          # Switch to the build folder so zip doesn't include the absolute path tree
          setwd(report_path)

          docs <- c(pdf_filename, pptx_filename)
          # Create the zip archive directly to the target 'file' location
          zip::zip(zipfile = file, files = docs)
          
          incProgress(0.95, detail = "Downloading report...")
                    
        }, error = function(e) {
          
          print(paste("Error bundling reports:", e$message))
          # Proactively stop execution so Shiny doesn't return an empty/broken file
          stop(e)
        })
    }
      )
  }, 
  contentType = "application/zip"
  )
}

