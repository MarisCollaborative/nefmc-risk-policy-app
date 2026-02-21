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


# matrix_data <- data[["rp-matrix"]]
info <- sd_get_data(db,
                      table = "rp-matrix-tbl",
                      refresh_interval = 30) 

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
}