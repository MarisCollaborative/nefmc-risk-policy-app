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


clean_scores <- function(data){

  scores <- data |> 
    dplyr::select(!c(starts_with("time"), "session_id", "browser", "ip_address", "current_page", "assessment")) |>  
    tidyr::pivot_longer(cols = 3:dplyr::last_col(),
                        names_to = "factor", 
                        values_to = "score")
  
}

clean_weights <- function(data){

  weights <- data |> 
    relocate(report_year, .before = everything()) |> 
    dplyr::select(!c(starts_with("time"), "session_id", "browser", "ip_address", "current_page", "weight_year", "weightings")) |>  
    tidyr::pivot_longer(cols = 2:dplyr::last_col(),
                        names_to = "factor", 
                        values_to = "weight") |> 
    mutate(weight = as.integer(weight), 
           factor = str_extract(factor, pattern = "(?<=[:punct:])[:alpha:]+")) |> # extract words/letters that are preceded by a punctuation
    tidyr::drop_na(weight) |>
    # group_by(report_year, factor) |> 
    summarise(avg_weight = mean(weight, na.rm = T), .by = c(report_year, factor))

}

## Z-Score Calculation ####
calc_zscore <- function(score, weight){
  # data <- data |> 
  #   mutate(product = scaled_score*normalized_weight) 
  # x <- sum(data$product)
  sum({{score}}*{{weight}})
}

calc_recprob <- function(z){
    1/(1+exp(-z))
}

## Plot Z-Score ####
plot_zscore <- function(data, xcol, ycol, ...){
    ggplot() + 
        lims(x = c(-2,4), y = c(0,1))+
        geom_function(fun = calc_recprob, linewidth = 1) + 
        geom_hline(aes(yintercept = 0.5, color = "MSA 50%\nprobability limit"), linetype = 'dashed', linewidth = 1) +
        geom_point(data = data, aes(x = {{xcol}}, y = {{ycol}}, color = "Recommended\nProbability"), size = 4) +
        scale_color_manual(name = "Legend", values = c("MSA 50%\nprobability limit" = "red", "Recommended\nProbability" = "#3e9eb6")) +
        # annotate(geom = 'shadowtext', x = {{x}}, y = {{y}}, label = input$dataset, color = 'blue', size = 6, bg.colour = 'white', vjust = -0.75)+
        # annotate(geom = 'text', x = 3, y = 0.5, label = 'MSA 50%\nprobability limit', color = 'red', size = 4, vjust = -0.4)+
        labs(x = 'Z-Score', y = 'Recommended Probability') +
        theme_bw() +
        theme(axis.title = element_text(size = rel(1.25)),
              axis.text = element_text(size = rel(1.25)), 
              legend.position = "bottom", 
              legend.title = element_text(size = rel(1.25)),
              legend.text = element_text(size = rel(1.25)), 
              legend.key.spacing = unit(0.5, "cm"))
}

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