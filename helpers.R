# Helper functions ####
library(dplyr)
library(stringr)
library(ggplot2)
library(here)
library(nefishr)

## General helpers #### ===============================================================
### Scale a value ####
#' 
#' 
#' 
scale_val <- function(x, y = 4){ x / {{y}} }


### Normalize a value ####
#' 
#' 
#' 
#' 
normalize_val <- function(x){ x / sum(x) }


### Calculate a Z-score ####
#' 
#' 
#' 
calc_zscore <- function(score, weight){ sum({{score}}*{{weight}}) }

### Calculate the recommended probability ###
#' Logistic function that truncates the curve at ymin = 0.5. 
#' 
#' 
# alpha_recprob <- function(z){ 1/(1+exp(-z)) }

### Recommended probability function ####
#' Logistic function that fits the full curve between 0.5 and 1 y-limits
#' 
#' 
#' 
calcRecProb <- function(z){ 0.5 + (0.5/(1+exp(z))) }

### Percent difference ####
percent.diff <- function(x1, x2) {
  (x2 - x1) / abs(x1)
}

### Render Report function #####
#'
#' 
#' 
## create render report function 
render_report <- function(input, output, params) {
  # render the report by rendering the RMD file
  rmarkdown::render(input,
    output_file = output,
    params = params,
    envir = new.env(parent = globalenv())
  )
}


## Specific helpers #### ==============================================================
### Clean matrix ####
#' 
#' 
#' 
clean_matrix <- function(data){

  matrix <- data |> 
    dplyr::select(!c(starts_with("time"), "session_id", "browser", "ip_address", "current_page")) |> 
    tidyr::drop_na(report_year) |> 
    dplyr::mutate(across(3:dplyr::last_col(), ~as.character(.))) |>
    dplyr::relocate("terminal_assessment_year", .after = "assessment_model") |>  
    dplyr::relocate("signif_catch_present", .before = "signif_catch_information") |> 
    tidyr::pivot_longer(cols = 3:dplyr::last_col(),
                        names_to = "value", 
                        values_to = "answer") |> 
    dplyr::mutate(factor = case_when(
            value %in% c("overfished", "overfishing", "rebuilding_plan", "rebuilding_target", "ssb", "relative_ssb") ~ "Biomass",
            value %in% c("recruit_incl", "recruitment_model", "beg_recruit_yr", "other_recruit_info", "recruit_year_est_1", "recruit_year_est_2", "recruit_year_est_3", "recruit_est_1", "recruit_est_2", "recruit_est_3") ~ "Recruitment",
            value %in% c("climate_vulnerability", "climate_direction", "no_of_prey", "prey_information") ~ "Climate Vulnerability",
            value %in% c("assessment_type", "assessment_model", "retro_pattern", "retro_val", "data_used", "missing_data", "uncertainty_sources", "terminal_assessment_year") ~ "Assessment and Uncertainty",
            value %in% c("other_quota_reliance", "other_fisheries") ~ "Commercial Fishery Characterization",
            value %in% c("OFL", "ABC", "harvest_control_rules", "accountability_measures", "signif_catch_present", "signif_catch_information") ~ "Additional Information",
            TRUE ~ value
          )) |>
    dplyr::relocate("factor", .after = "stock") |> 
    tidyr::drop_na(factor) |> 
    # replace any underscores in the value and answer columns with spaces
    dplyr::mutate(value = dplyr::case_when(
                        value == "ssb" ~ "SSB",
                        value == "relative_ssb" ~ "Relative SSB",
                        value == "recruit_incl" ~ "Recruitment is estimated", 
                        value == "beg_recruit_yr" ~ "Initial year of time series",
                        value == "retro_pattern" ~ "Retrospective Pattern", 
                        value == "retro_val" ~ "Retrospective Values",
                        value == "signif_catch_present" ~ "Significant Catch Present", 
                        value == "signif_catch_information" ~ "Significant Catch Information",
                        value %in% stringr::str_subset(value, "[:lower:]") ~ stringr::str_to_title(stringr::str_replace_all(value, "_", " ")),
                        TRUE ~ str_replace_all(value, "_", " ")),
                  answer = str_replace_all(answer, "_", " "))
  
  ### Reorder the factors so assessment and uncertainty is first. 
  #1) create an object containing the factor names in the desired order
  reorder_levels <- c("Assessment and Uncertainty", "Biomass", "Recruitment", "Climate Vulnerability", "Commercial Fishery Characterization", "Additional Information")
 
  #2) use the object to overwrite the default levels of the factor column
  matrix <- matrix |> 
    mutate(factor = factor(factor, levels = reorder_levels))

  return(matrix)

}

### Clean risk policy scores data ####
#' 
#' 
#' 
clean_scores <- function(data){

  scores <- data |> 
    dplyr::select(!c(starts_with("time"), "session_id", "browser", "ip_address", "current_page", "assessment")) |>  
    tidyr::pivot_longer(cols = 3:dplyr::last_col(),
                        names_to = "factor", 
                        values_to = "score") |> 
    dplyr::mutate(score = as.integer(score),
           scaled_score = scale_val(score))
  
  return(scores)
  
}

### Clean risk policy weights data ####
#' 
#' 
#'
clean_weights <- function(data){

  weights <- data |> 
    dplyr::relocate(report_year, .before = dplyr::everything()) |> 
    dplyr::select(!c(starts_with("time"), "session_id", "browser", "ip_address", "current_page", "weight_year", "weightings")) |>  
    tidyr::pivot_longer(cols = 2:dplyr::last_col(),
                        names_to = "factor", 
                        values_to = "weight") |> 
    dplyr::mutate(weight = as.integer(weight), 
           factor = str_extract(factor, pattern = "(?<=[:punct:])[:alpha:]+")) |> # extract words/letters that are preceded by a punctuation
    tidyr::drop_na(weight) |>
    dplyr::summarise(avg_weight = round(
                                        mean(weight, na.rm = T), 
                                        2),
                    .by = c(report_year, factor)) |>
    dplyr::mutate(normalized_weight = round(normalize_val(avg_weight), 2)) 

  return(weights)
}

### Plot Alpha Plot ####
#' Plots the originally proposed logisitc function
#' 
#' 
#' 
# plot_alpha <- function(data, xcol, ycol, color = "#3e9eb6", ...){
#     ggplot2::ggplot() + 
#         ggplot2::lims(x = c(0,4), y = c(0.5,1))+
#         ggplot2::geom_function(fun = alpha_recprob, linewidth = 1) + 
#         ggplot2::geom_hline(aes(yintercept = 0.5, color = "MSA 50%\nprobability limit"), linetype = 'dashed', linewidth = 1) +
#         ggplot2::geom_point(data = {{ data }}, aes(x = {{xcol}}, y = {{ycol}}, color = "Recommended\nProbability"), size = 4) +
#         ggplot2::scale_color_manual(name = "Legend", values = c("MSA 50%\nprobability limit" = "red", "Recommended\nProbability" = {{color}} )) +
#         ggplot2::labs(x = 'Z-Score', y = 'Recommended Probability') +
#         ggplot2::theme_bw() +
#         ggplot2::theme(axis.title = element_text(size = rel(1.25)),
#               axis.text = element_text(size = rel(1.25)), 
#               legend.position = "bottom", 
#               legend.title = element_text(size = rel(1.25)),
#               legend.text = element_text(size = rel(1.25)), 
#               legend.key.spacing = unit(0.5, "cm")) + 
#         ggplot2::coord_fixed(ratio = 4)
# }


### Plot Alpha and Beta functions ####
#' Plots alpha and beta functions on the same pane to show differences in z-scores between approaches
#' 
#' 
# plot_abprob <- function(data, z, alpha = NULL, beta){
#     ggplot2::ggplot() + 
#         ggplot2::lims(x = c(-4,4), y = c(0.5,1))+
#         # ggplot2::geom_function(fun = alpha_recprob, linewidth = 1, lty = 2) + 
#         # ggplot2::geom_point(data = data, aes(x = {{z}}, y = {{alpha}}, color = "Alpha\nRecommended\nProbability"), size = 4) +
#         ggplot2::geom_function(fun = beta_recprob, linewidth = 1, lty = 3) + 
#         ggplot2::geom_hline(aes(yintercept = 0.5, color = "MSA 50%\nprobability limit"), linetype = 'dashed', linewidth = 1) +
#         ggplot2::geom_point(data = data, aes(x = {{z}}, y = {{beta}}, color = "Beta\nRecommended\nProbability"), size = 4) +
#         ggplot2::scale_color_manual(name = "Legend", values = c("MSA 50%\nprobability limit" = "red", "Alpha\nRecommended\nProbability" = "#3e9eb6", "Beta\nRecommended\nProbability" = "red")) +
#         ggplot2::labs(x = 'Z-Score', y = 'Recommended Probability') +
#         ggplot2::theme_bw() +
#         ggplot2::theme(axis.title = element_text(size = rel(1.25)),
#               axis.text = element_text(size = rel(1.25)), 
#               legend.position = "bottom", 
#               legend.title = element_text(size = rel(1.25)),
#               legend.text = element_text(size = rel(1.25)), 
#               legend.key.spacing = unit(0.5, "cm")) + 
#         ggplot2::coord_fixed(ratio = 8)
# }

### Plot the Recommended Probability ####
#' Plots the logistic function 
#' 
#'
plotRecProb <- function(data, z, RecProb){
  
  horizon_zones <- c("High Risk\nTolerance", "Intermediate Risk\nTolerance", "Low Risk\nTolerance")
  
  # zones created horizontally at the inflection points 
  horizontal_inf_pts <- data.frame(
   x = c(-4, -4, -4),
   ymin = c(0.5, 0.61, 0.89),
   ymax = c(0.61, 0.89, 1.00),
   Zones = factor(horizon_zones, levels = horizon_zones),
   w = c(8, 8, 8)
  )
  
  ggplot2::ggplot() +
    ggplot2::lims(x = c(-4,4), y = c(0.5,1))+
    geom_rect(data = horizontal_inf_pts, aes(xmin = x, xmax = x + w, ymin = ymin, ymax = ymax, fill = Zones), alpha = 0.35) +
    ggplot2::geom_function(fun = calcRecProb, linewidth = 1, lty = 3) + 
    ggplot2::geom_hline(aes(yintercept = 0.5, color = "MSA 50%\nprobability limit"), linetype = 'dashed', linewidth = 1) +
    ggplot2::geom_point(data = data, aes(x = {{z}}, y = {{RecProb}}, color = "Recommended\nProbability"), size = 4) +
    ggplot2::scale_color_manual(name = "Legend",
      values = c("MSA 50%\nprobability limit" = "red", 
                 "Recommended\nProbability" = "#1d365e")
    ) +
    ggplot2::scale_fill_manual(
      values = c("Low Risk\nTolerance" = "#CC3300", 
                 "Intermediate Risk\nTolerance" = "#FF9900", 
                 "High Risk\nTolerance" = "#33CC33")
    ) +
    ggplot2::labs(x = 'Z-Score', y = 'Recommended Probability') +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title = element_text(size = rel(1)),
        axis.text = element_text(size = rel(1)), 
        legend.position = "bottom", 
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(1)), 
        legend.key.spacing = unit(0.5, "cm")) + 
    guides(
    color = guide_legend(order = 1), # Hline
    fill = guide_legend(order = 2)   # Rect
    ) +
    ggplot2::coord_fixed(ratio = 8)
}