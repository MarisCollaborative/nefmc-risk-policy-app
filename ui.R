# User Interface for Risk Policy Shiny App ####
## Environment set-up
library(shiny)
library(tidyverse)
library(here)
library(bslib)
library(nefishr)
library(gt)
library(surveydown)
library(shinyjs)

# connect to survey database
db <- sd_db_connect()

# fetch the risk policy scores from the database 
data <- sd_get_data(db, "rp-scores")

# identify the unique values of report year that occur in the table
year_vals <- unique(data$report_year) |> sort()

# identify the unique values of Stock Name from the nefmc_species dataframe in the nefishr package
stock_vals <- unique(nefishr::nefmc_species$STOCK_NAME) |> sort()

# identify the unique values of FMP from the nefmc_species dataframe in the nefishr package
fmp_vals <- unique(nefishr::nefmc_species$FMP) |> sort()

# User Interface ####
ui <- fluidPage(
  shinyjs::useShinyjs(),
  # create a multi-page application 
  page_navbar(title = 'NEFMC Risk Policy Application', # with the following title
              # a shared sidebar across pages, that has the following
              sidebar = sidebar( 
                # Settings  
                id = "sidebar", 
                width = 350, 
                open = T, # default to open
                # Inputs: 
                ## Year selection 
                selectInput(inputId = "year", 
                      label = "Action Year", 
                      choices = c("Select a year...", year_vals), # using the unique year values from the scoring table
                      selected = "Select a year..."), # the initial value shown when the user starts the app 

                ## FMP selection
                selectInput(inputId = 'fmp', 
                      label = 'Select FMP', 
                      choices = c("Select an FMP...", fmp_vals), # using the unique fmp values from the nefmc_species table
                      selected = "Select an FMP..."), # the initial value shown when the user starts the app 
                
                ## Stock selection
                selectInput(inputId = 'stock', 
                            label = 'Select stock', 
                            choices = c("Select a stock...", stock_vals), # using the unique stock values from the nefmc_species table
                            selected = "Select a stock..."), # the initial value shown when the user starts the app
                
                # Generate Report button
                downloadButton("report", "Generate report")
                ),
              # Page 1 - shows the matrix table based on the sidebar inputs
              nav_panel(title = "Matrix", 
                        gt_output("matrix")
                        ), 
              # Page 2 - shows the recommended probability information based on user inputs and contains
              nav_panel(title = "Recommended Probability",
                    # a multi-tab card with  
                    navset_card_tab( 
                        # a shared sidebar to change the factors by one level 
                        sidebar = sidebar(id = "changeFactors",
                        sliderInput('changeBiomass', "Biomass", min = -1, max = 1, value = 0, step = 1),
                        sliderInput('changeRecruitment', "Recruitment", min = -1, max = 1, value = 0, step = 1),
                        sliderInput('changeClimate', "Climate Vulnerability", min = -1, max = 1, value = 0, step = 1),
                        sliderInput('changeCommercial', "Commercial Fishery", min = -1, max = 1, value = 0, step = 1),
                        sliderInput('changeRecreational', "Recreational Fishery", min = -1, max = 1, value = 0, step = 1), 
                        # action buttons to manipulate the scores based on user input or restore the scores to their original values
                        actionButton('changeScores', "Make Changes"), 
                        actionButton('resetScores', "Reset Scores")
                                          ),
                        # a table of data with PDT scores and Council weightings, and text containing the calculated z-score and recommended probabilities
                        nav_panel("Z-score Data", 
                                  gt_output("scores"),
                                  verbatimTextOutput('zscore', placeholder = T), 
                                  verbatimTextOutput('RecProb', placeholder = T)), 
                        # a plot of the calculated Z-score and recommended probability based on the scores and weights
                        nav_panel("Z-score Plot", plotOutput("zplot")),
                                        ),
                  # and a text area for user input if there is a decision to change a score(s) and ultimately the recommended probability for a given stock in a given year
                  card(textAreaInput(inputId = "rationale", 
                                     label = "If you are recommending a change to a score, enter your rationale below.", 
                                     width = '100%')#, 
                                     #value = "No changes or additional rationale provided.")
                      )
                
            )
          )
        )
