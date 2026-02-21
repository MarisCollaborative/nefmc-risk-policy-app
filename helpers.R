# Helper functions ####
library(tidyverse)
library(here)
library(nefishr)

## Generate matrix #####
clean_matrix <- function(data){

  matrix <- data |> 
    dplyr::select(!c(starts_with("time"), "session_id", "browser", "ip_address", "current_page", "fish_condition")) |> 
    tidyr::drop_na(report_year) |> 
    dplyr::mutate(across(3:dplyr::last_col(), ~as.character(.))) |>
    tidyr::pivot_longer(cols = 3:dplyr::last_col(),
                        names_to = "value", 
                        values_to = "answer") |> 
    mutate(factor = case_when(
            value %in% c("overfished", "overfishing", "rebuilding_plan", "rebuilding_target", "ssb", "relative_ssb") ~ "Biomass",
            value %in% c("recruit_incl", "recruitment_model", "beg_recruit_yr", "other_recruit_info", "recruit_year_est_1", "recruit_year_est_2", "recruit_year_est_3", "recruit_est_1", "recruit_est_2", "recruit_est_3") ~ "Recruitment",
            value %in% c("climate_vulnerability", "climate_direction", "no_of_prey", "prey_information") ~ "Climate Vulnerability",
            value %in% c("assessment_type", "assessment_model", "retro_pattern", "retro_val", "data_used", "missing_data", "uncertainty_sources", "terminal_assessment_year") ~ "Assessment and Uncertainty",
            value %in% c("other_quota_reliance", "other_fisheries") ~ "Commercial Fishery Characterization",
            value %in% c("OFL", "ABC", "harvest_control_rules", "accountability_measures", "signif_catch_present", "signif_catch_information") ~ "Additional Information",
            TRUE ~ value
          )) |>#, 
          # theme = case_when(
          #   factor %in% c("Biomass", "Recruitment") ~ "Stock Status and Uncertainty",
          #   factor == "Climate Vulnerability" ~ "Climate and Ecosystem",
          #   TRUE ~ "Additional Information",
          # )) |> 
    tidyr::drop_na(factor) 

  return(matrix)
  # res <- data |> 
  #   gt::gt(rowname_col = "factor") |> 
  #   tab_stubhead(label = "Factor") |> 
  #   cols_label(answer = "Supporting Information") |> 
  #   cols_hide(c("report_year", "stock"))
}


# get_matrix_data <- function(data, cols, filter_by){
#   data <- clean_matrix(data) |> 
#   filter({{ cols }} %in% {{ filter_by }}) |> 
#     rename("Supporting Information" = answer, 
#            "Value" = value)
#   return(data)
# }


## Render Report function #####
## create a temporary file location
report_path <- tempfile(fileext = ".Rmd")

## copy the RMD file in the repo to the temporary file location and overwrite if already existing
file.copy("draft_report.Rmd", report_path, overwrite = TRUE)

## create render report function 
render_report <- function(input, output, params) {
  # render the report by rendering the RMD file
  rmarkdown::render(input,
    output_file = output,
    params = params,
    envir = new.env(parent = globalenv())
  )
} 