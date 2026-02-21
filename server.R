# Server ####
library(shiny)
library(tidyverse)
library(here)
library(gt)
library(surveydown)
source(here("helpers.R"))

## connect once configured
db <- sd_db_connect()

server <- function(input, output, session) {

### Server Parameters ####
# create reactive element for selected year 
year <- reactive(input$year)
  
# create reative element for selected FMP 
fmp <- reactive(input$fmp)

observeEvent(input$fmp, {
  # Filter choices for the stocks based on the fmp
  choices_to_show <- nefishr::nefmc_species |>
    dplyr::filter(FMP == input$fmp) |>
    dplyr::pull(FMP_NAME) |>
    unique() |> 
    sort()

  updateSelectInput(session, "stock", choices = choices_to_show)
})

# create reactive element for selected stock
stock <- reactive(input$stock)

### Fetch data ####
# get risk policy data from the database
# matrix_data <- data[["rp-matrix"]]
info <- sd_get_data(db,
                      table = "rp-matrix-tbl",
                      refresh_interval = 30) 
scores <- sd_get_data(db,
                      table = "rp-scores",
                      refresh_interval = 30) #|> 
          # clean_scores()
weights <- sd_get_data(db,
                      table = "rp-weights",
                      refresh_interval = 30) #|>
          #  clean_weights()
  
### Page 1: Matrix Output ###
output$matrix <- render_gt(
  {
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

      
  }
  )



### Page 2: Scores, Weights, Plots 
scores_clean <- reactive(
    scores() |> 
    clean_scores() |> 
    filter(report_year == year() & stock == stock()) |> 
    mutate(score = as.integer(score), 
           scaled_score = score/4))

weights_clean <- reactive(
    weights() |> 
    clean_weights() |> 
    mutate(normalized_weight = avg_weight / sum(avg_weight))
)

z_data <- reactive(
    scores_clean() |> 
      filter(!factor %in% str_subset(factor, "rationale")) |> 
      left_join(weights_clean(), by = c("report_year", "factor")) 
)
  
zscore_vals <- reactive(
  
  z_data() |> 
    summarise(zscore = calc_zscore(scaled_score, normalized_weight), 
              rec_prob = calc_recprob(zscore))
  # calc_zscore(z_data())

)

# recprob_val <- reactive(
  
#   calc_recprob(zscore_val())

# )
  
output$scores <- renderTable({

  z_data()
    
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