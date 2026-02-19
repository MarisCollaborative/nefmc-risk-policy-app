# UI ####
library(shiny)
library(tidyverse)
library(here)
library(bslib)
library(nefishr)
library(gt)
source("helpers.R")

## read in data 
# data file path 
data.loc <- here("data")

# 
csv.names <- c("rp-weights", "rp-scores", "rp-matrix")
data <- map(csv.names, ~read.csv(here(data.loc, str_c(., "csv", sep = "."))))
names(data) <- csv.names

year_vals <- unique(data[["rp-scores"]]$report_year) |> sort()
stock_vals <- unique(nefishr::nefmc_species$FMP_NAME) |> sort()
fmp_vals <- unique(nefishr::nefmc_species$FMP) |> sort()

link_shiny <- tags$a(
  shiny::icon("github"), "Shiny",
  href = "https://github.com/MarisCollaborative",
  target = "_blank"
)
# link_nefmc <- tags$a(
#   shiny::icon("r-project"), "Posit",
#   href = "https://posit.co",
#   target = "_blank"
# )


ui <- fluidPage(
  # create a multi-page application 
  page_navbar(title = 'NEFMC Risk Policy Application', # with the following title
              sidebar = sidebar( # and a shared sidebar across pages, that has the following
                # Settings  
                id = "sidebar", 
                width = 350, 
                open = T, # default to open
                # Inputs: 
                ## Year selection 
                selectInput(inputId = "year", 
                      label = "Action Year", 
                      choices = year_vals),

                ## FMP selection
                selectInput('fmp', label = 'Select FMP', choices = fmp_vals),
                
                ## Stock selection
                selectInput('stock', label = 'Select stock', choices = stock_vals),
                
                # Generate Report button
                downloadButton("report", "Generate report")
                ),
              # Page 1 - shows the matrix table based on the sidebar inputs
              nav_panel(title = "Matrix", 
                        gt_output("matrix")
                        ), 
              # Page 2: shows the recommended probability information of the selected stock and contains
              nav_panel(title = "Recommended Probability",
                    # a multi-tab card that  
                    navset_card_tab( 
                        # contains a shared sidebar to change the factors by one level 
                        sidebar = sidebar(id = "changeFactors",
                        sliderInput('changeBiomass', "Biomass", min = -1, max = 1, value = 0, step = 1),
                        sliderInput('changeRecruitment', "Recruitment", min = -1, max = 1, value = 0, step = 1),
                        sliderInput('changeClimate', "Climate Vulnerability", min = -1, max = 1, value = 0, step = 1),
                        sliderInput('changeCommercial', "Commercial Fishery", min = -1, max = 1, value = 0, step = 1),
                        sliderInput('changeRecreational', "Recreational Fishery", min = -1, max = 1, value = 0, step = 1)
                                          ),
                        # shows the table of data with PDT scores and Council weightings
                        nav_panel("Z-score Data", gt_output("scores")), 
                        # plots the calculated Z-score based on the scores and weights
                        nav_panel("Z-score Plot", plotOutput("zplot")),
                                        ),
                  # an text area, if there is a decision to change a score(s) and ultimately the recommended probability 
                  card(textAreaInput("text", "If you are recommending a change to a score, enter your rationale below.", width = '100%')
                      )
                
            )
          )
        )






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


matrix_data <- data[["rp-matrix"]]

output$matrix <- render_gt(
  {
    clean_matrix(matrix_data) |> 
      filter(report_year == year() & stock == str_to_lower(stock())) |>
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

  shinyApp(ui, server)
