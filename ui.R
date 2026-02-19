# UI ####
library(shiny)
library(tidyverse)
library(here)
library(bslib)
library(nefishr)

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

                ## Stock selection
                selectInput('stock', label = 'Select stock', choices = stock_vals),

                ## FMP selection
                selectInput('fmp', label = 'Select FMP', choices = fmp_vals),
                
                # Generate Report button
                downloadButton("report", "Generate report")
                ),
              # Page 1 - shows the matrix table based on the sidebar inputs
              nav_panel(title = "Matrix", 
                        tableOutput("matrix")
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
                        nav_panel("Z-score Data", tableOutput("scores")), 
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

}

shinyApp(ui, server)
