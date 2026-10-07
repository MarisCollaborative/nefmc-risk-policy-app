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
library(bsicons)

# connect to survey database
db <- sd_db_connect(".env")

# fetch the risk policy scores from the database 
data <- sd_get_data(db, "rp_scores")

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
  page_navbar(title = 'NEFMC Risk Policy Toolkit', # with the following title
              # Side bar - shared across pages, that has the following ===============================================================
              sidebar = sidebar( 
                # Settings  
                id = "sidebar", 
                width = 350, 
                open = T, # default to open
                # Instructions:
                p(em("Use the dropdown menus to select parameters for filtering data throughout the application:")),
                # Inputs: 
                ## Year selection 
                selectInput(inputId = "year", 
                      label = strong("Action Year"), 
                      choices = c("Select a year...", year_vals), # using the unique year values from the scoring table
                      selected = "Select a year..."), # the initial value shown when the user starts the app 

                ## FMP selection
                selectInput(inputId = 'fmp', 
                      label = strong("Select FMP"), 
                      choices = c("Select an FMP...", fmp_vals), # using the unique fmp values from the nefmc_species table
                      selected = "Select an FMP..."), # the initial value shown when the user starts the app 
                
                ## Stock selection
                selectInput(inputId = 'stock', 
                            label = strong("Select stock"), 
                            choices = c("Select a stock...", stock_vals), # using the unique stock values from the nefmc_species table
                            selected = "Select a stock..."), # the initial value shown when the user starts the app
                br(), 
                # Generate Report button
                downloadButton("report", "Generate report")
                  # uiOutput("downloadReport")
                ),

              # Page 1 - shows the matrix table based on the sidebar inputs ===========================================================
              nav_panel(title = "Matrix",
                        # gt_output("matrix")
                        uiOutput("matrix")
                        ), 

              # Page 2 - shows the recommended probability information based on user inputs and contains
              nav_panel(title = "Recommended Probability",
                    
                    ### a multi-tab card with ==========================================================================================
                    navset_card_tab( 
                        
                        ### Tab 1: a table of data with PDT scores and Council weightings, and text containing the calculated z-score and recommended probabilities ======================================================================================
                        nav_panel("Z-score Data", 
                                  p(strong("Z-score Data")),
                                  gt_output("scores"),
                                  p(strong("The Z-score value based on the scores and weights table above:")),
                                  verbatimTextOutput('zscore', placeholder = T)#, 
                                  # p(strong("Z-score Plot")),
                                  # plotOutput("RecProb_plot"),
                                  # p(strong("The recommended probability based on the calculated z-score:")),
                                  # verbatimTextOutput('AlphaProb', placeholder = T)), 
                                  # verbatimTextOutput('BetaProb', placeholder = T),
                        # nav_panel("Alpha Z-score Plot", plotOutput("alpha_plot"),
                                  ),
                                  
                        ### Tab 2: a plot of the calculated Z-score and recommended probability based on the scores and weights =======
                        nav_panel("Z-score Plot",
                                  plotOutput("plot"), 
                                  # p("This plot compares the differences between recommended probabilities that were calculated based on the logistic curve approved in the Alpha phase of the Risk Policy, and the logistic cuve that is being considered in the Beta phase of the Risk Policy."),
                                  p(strong("The recommended probability based on the logisitic curve and the Z-score:")),
                                  verbatimTextOutput('RecProb', placeholder = T),
                                  p(strong("The level of risk tolerance to consider based on the risk zones:")),
                                  verbatimTextOutput('ClassifyZone', placeholder = T)
                                ), 

                        ### Tab 3: information from scoring regarding AP Input for Fishery Factors =======
                        # nav_panel("AP Input on Fishery Characterizations",
                        #           h5(em("Commercial Advisory Panel")),
                        #           textOutput("com_ap"),
                        #           h5(em("Recreational Advisory Panel")),
                        #           textOutput("rec_ap")
                        #         )
                                      )#,
                      ),
             
            ## Page 3 - About the application and how to use it ========================================================================
            nav_panel(title = "About",
              h5(strong("About the Application")), 
              p("This multi-page application was built to support the New England Fishery Management Council's (NEFMC) decision-making when applying its Risk Policy. Data within this application is collected according to the ", 
                a("Risk Policy Concept Document.", #name of the link 
                href = "https://d23h0vhsm26o6d.cloudfront.net/Risk-Policy-Statement-and-Concept-Overview_for-Posting-August-12-2026.pdf", # the url
                target = "_blank")), # opens in a new tab
              h5(em("Features and functionalities:")),
              tags$ul( # create a bulleted list
                tags$li(strong("A top navigation bar:"), " to navigate throughout the application"), 
                tags$li(strong("A user selection menu:"), " a collapsible menu on the left-hand side to filter the data within the application based on a specific Council action year, Fishery Management Plan, and stock. Hide the menu using the carrot in the top right corner of the menu."), 
                tags$li(strong("The Matrix page:"), " displays additional qualitative information and context for the selected stock that was considered during the decision-making process for each of the Risk Policy factors."),
                tags$li(strong("The Recommended Probability page:"), " displays the quantitative outputs of the Risk Policy mechanics. It includes a table of the risk policy factors scored and scaled by the FMP Plan Development Teams, the average weights for each factor approved by the Council; statements about the calculated values of the Z-score and recommended probability; the final Z-score plotted along the recommended probability curve."), #; and additional information regarding the advisory panel's input on the fishery characterizations."), 
                tags$li(strong("A Generate Report button:"), " nested within the user selection menu. Clicking this button will generate and download a report onto your device containing information from the Matrix and Recommended Probability page based on the user's selections in the menu."))
          )
          )
        )
