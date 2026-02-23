# Server ####
library(shiny)
library(tidyverse)
library(here)
library(gt)
library(DT)
library(surveydown)
library(shinyjs)
source(here("helpers.R"))

## connect once configured
db <- surveydown::sd_db_connect()

server <- function(input, output, session) {

### Server Parameters ####
# create reactive element for selected year 
year <- reactive(input$year)
  
# create reative element for selected FMP 
fmp <- reactive(input$fmp)

observeEvent(input$fmp, {
  # Filter choices for the stocks based on the fmp
  choices_to_show <- nefishr::nefmc_species |>
    dplyr::filter(FMP == fmp()) |>
    dplyr::pull(STOCK_NAME) |>
    unique() |> 
    sort()

  updateSelectInput(session, "stock", choices = c("Select a stock...", choices_to_show))
})

# create reactive element for selected stock
stock <- reactive(input$stock)

### Fetch data ####
# get risk policy data from the database
info <- sd_get_data(db,
                      table = "rp-matrix-tbl",
                      refresh_interval = 30) 

scores_static <- sd_get_data(db, table = "rp-scores") 

weights <- #reactive(
  sd_get_data(db, table = "rp-weights") |> 
    clean_weights() #|> 
    #filter(report_year == year())
#)
  
# scores <- reactiveVal({
#   sd_get_data(db, table = "rp-scores") |> 
#     clean_scores() 
# })
scores <- #reactive(
  sd_get_data(db, table = "rp-scores") |> 
    clean_scores() |> 
    filter(!factor %in% str_subset(factor, "rationale")) #|> 
    # left_join(weights(), by = c("report_year", "factor")) 
#)
  
z_data <- left_join(scores, weights, by = c("report_year", "factor"))
  
zdata_rv <- reactiveValues(original = z_data, 
                           updated = z_data)
                      
  
### Page 1: Matrix Output ###
output$matrix <- render_gt({ 

    info() |> 
      clean_matrix() |> 
      filter(report_year == year() & stock == stock()) |>
      select(!c(report_year, stock)) |> 
      mutate(value = str_to_title(str_replace_all(value, "_", " ")), 
             answer = str_to_sentence(str_replace_all(answer, "_", " "))) |>
      gt(rowname_col = "value", 
         groupname_col = "factor", 
         row_group_as_column = TRUE) |>
      tab_header(title = str_c(year(), "Risk Policy Matrix for", stock(), sep = " ")) |> 
      text_case_match(
        NA ~ "Not provided",
        .locations = cells_body(answer)
      ) 
   
  })




### Page 2: Scores, Weights, Plots
scale_score <- function(x){ x/4 }
## Using DT output #### 
# render_dt = function(data, editable = 'cell', server = TRUE, ...) {
#   renderDT(data, selection = 'none', server = server, editable = editable, ...)
# }
# 
  
# output$scores <- renderDT({
#   scores() |> 
#     filter(report_year == year(), stock == stock(), !factor %in% str_subset(factor, "rationale")) |> 
#     left_join(weights(), by = c("report_year", "factor")) |> 
#     datatable(editable = list(target = 'column', disable = list(1:3,4:6)), rownames = F)#editable = "cell") 
  
#   # datatable(z_data$data, editable = "cell", rownames = F)
# })

# # Observe cell edits
#   observeEvent(input$scores_cell_edit, {
#     # Get information about the edit
#     info <- input$scores_cell_edit
#     r <- info$row
#     c <- info$col 
#     value <- info$value
    
#     # Access the current data frame
#     temp_reset <- scores() |> 
#       filter(report_year == year(), stock == stock(), !factor %in% str_subset(factor, "rationale")) |> 
#       left_join(weights(), by = c("report_year", "factor")) 
#     # current_data <- z_data$data
    
#     # Update the edited cell with the new value
#     # Coerce value to the correct type to avoid issues
#     # temp_df[r, c] <- DT::coerceValue(value, temp_df[r, c])
#     temp_df[r, c] <- value
#     # current_data[r,c] <- value
    
#     # 4. Perform calculation and update the dependent column
#     # The 'Value_Squared' column (index 3) is updated based on 'Value' (index 2)
#     # temp_df[r, "scaled_score"] <- scale_score(temp_df[r, "score"])
#     temp_df <-  isolate({
#       temp_df |>
#         mutate(scaled_score = scale_score(score))
#     })
#     # current_data <-  isolate({
#     #   current_data |>
#     #     mutate(scaled_score = scale_score(score))
#     # })
    
#     # 5. Update the reactive data frame with the modified data
#     scores(temp_df)
#     # z_data$data <- current_data
    
#     # Optional: Use dataTableProxy and replaceData for smoother updates
#     # proxy <- dataTableProxy("editable_table")
#     # replaceData(proxy, temp_df, resetPaging = FALSE)
#   })

### ===================================================================== 
# Using GT output; needs some tinkering to not accumulate score changes ####
# eventReactive(input$changeScores, {
  biomass <- reactive(input$changeBiomass)
  recruitment <- reactive(input$changeRecruitment)
  climate <- reactive(input$changeClimate)
  commercial <- reactive(input$changeCommercial)
  recreational <- reactive(input$changeRecreational)
# })

observeEvent(input$changeScores, {
    # factors <- c("biomass", "recruitment", "climate", "commercial", "recreational")
    # # changes <- c(input$changeBiomass, input$changeRecruitment, input$changeClimate, input$changeCommercial, input$changeRecreational)
    # changes <- c(biomass(), recruitment(), climate(), commercial(), recreational())
    
    # changes.df <- data.frame(factor = factors, update = changes)
    updated <- zdata_rv$updated |> 
      filter(report_year == year(), stock == stock())#, !factor %in% str_subset(factor, "rationale")) |> 
      # left_join(weights(), by = c("report_year", "factor"))
    
    # new <- left_join(updated, changes.updated, by = "factor") |> 
    #   mutate(score = score + update, 
    #          scaled_score = score / 4)
    # Update the edited cell
    updated[updated$factor=="biomass", "score"] <- updated[updated$factor=="biomass", "score"] + biomass()
    updated[updated$factor=="recruitment", "score"] <- updated[updated$factor=="recruitment", "score"] + recruitment()
    updated[updated$factor=="climate", "score"] <- updated[updated$factor=="climate", "score"] + climate()
    updated[updated$factor=="commercial", "score"] <- updated[updated$factor=="commercial", "score"] + commercial()
    updated[updated$factor=="recreational", "score"] <- updated[updated$factor=="recreational", "score"] + recreational()
    
    # Perform Calculation: Total = Quantity * Price
    # Note: DT column indices are 1-based in R, 
    # but cell_info uses 1-based row, 1-based col.
    updated$scaled_score <- scale_score(updated$score)
    
    # Update the reactive data
    zdata_rv$updated <- updated
})  
  

observeEvent(input$resetScores, {
  # factors <- c("biomass", "recruitment", "climate", "commercial", "recreational")
  # # changes <- c(input$changeBiomass, input$changeRecruitment, input$changeClimate, input$changeCommercial, input$changeRecreational)
  # changes <- c(biomass(), recruitment(), climate(), commercial(), recreational())
    
  # changes.df <- data.frame(factor = factors, reset = changes)
  # reset.df <- scores() |> 
      # filter(report_year == year(), stock == stock(), !factor %in% str_subset(factor, "rationale")) |> 
      # left_join(weights(), by = c("report_year", "factor"))
    
    # reset <- left_join(reset, changes.reset, by = "factor") |> 
    #   mutate(score = score - reset, 
    #          scaled_score = score / 4)
    # Update the edited cell
    # reset.df[reset.df$factor=="biomass", "score"] <- reset.df[reset.df$factor=="biomass", "score"] + biomass()
    # reset.df[reset.df$factor=="recruitment", "score"] <- reset.df[reset.df$factor=="recruitment", "score"] + recruitment()
    # reset.df[reset.df$factor=="climate", "score"] <- reset.df[reset.df$factor=="climate", "score"] + climate()
    # reset.df[reset.df$factor=="commercial", "score"] <- reset.df[reset.df$factor=="commercial", "score"] + commercial()
    # reset.df[reset.df$factor=="recreational", "score"] <- reset.df[reset.df$factor=="recreational", "score"] + recreational()
    
    # Perform Calculation: Total = Quantity * Price
    # Note: DT column indices are 1-based in R, 
    # but cell_info uses 1-based row, 1-based col.
    # reset.df$scaled_score <- scale_score(reset.df$score)
    
    # Update the reactive data
    zdata_rv$updated <- zdata_rv$original |> 
      filter(report_year == year(), stock == stock())#, !factor %in% str_subset(factor, "rationale")) |> 
      # left_join(weights(), by = c("report_year", "factor"))
  
    # reset slider inputs 
    shinyjs::reset("changeBiomass")
    shinyjs::reset("changeRecruitment")
    shinyjs::reset("changeClimate")
    shinyjs::reset("changeCommercial")
    shinyjs::reset("changeRecreational")
  
})
# ====================================================================== #  

## Some combination of DT and GT? ####
  
  
## Final outputs ####
# z_data <- reactive({
#     scores() |> 
#       filter(report_year == year(), stock == stock(), !factor %in% str_subset(factor, "rationale")) |> 
#       left_join(weights(), by = c("report_year", "factor")) 
# })
  
zscore_vals <- reactive({
  zdata_rv$updated |> 
  # scores() |>
    filter(report_year == year(), stock == stock()) |> #, !factor %in% str_subset(factor, "rationale")) |>
    summarise(zscore = calc_zscore(scaled_score, normalized_weight), 
              rec_prob = calc_recprob(zscore))

})

output$scores <- render_gt({

  zdata_rv$updated |> 
    filter(report_year == year(), stock == stock()) |>#, !factor %in% str_subset(factor, "rationale")) |>
    gt()

    
})




output$zscore <- renderText(
  zscore_vals()$zscore
)

output$RecProb <- renderText(
    
    str_c( # create a string that includes: 
      round( # the rounded product of 
        zscore_vals()$rec_prob*100, # the rec_prob value from the reactive zscores_val multiplied by 100
        1 # to the nearest tenth,
      ),
      "%", # and a percent sign,  
      sep = "") # without any separating space or punctuation

)
  
output$zplot <- renderPlot({

 plot_zscore(data = zscore_vals(), xcol = zscore, ycol = rec_prob)
  
})
  


}