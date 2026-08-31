# Helper functions ####
library(dplyr)
library(stringr)
library(ggplot2)
library(here)
library(nefishr)

## General helpers #### ===============================================================
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
calc_zscore <- function(score, weight){ sum({{score}}*{{weight}}, na.rm =T) }

### Recommended probability function ####
#' Logistic function that fits the full curve between 0.5 and 1 y-limits
#' 
#' 
#' 
calcRecProb <- function(z){ 0.5 + (0.5/(1+exp(z))) }

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
    dplyr::select(!c(starts_with("time"), "session_id", "browser", "ip_address", "current_page", "name")) |> 
    tidyr::drop_na(report_year) |> 
    # dplyr::mutate(across(3:dplyr::last_col(), ~as.character(.))) |>
    dplyr::relocate("terminal_assessment_year", .after = "assessment_model") |>  
    dplyr::relocate("rec_acl", .before = "rec_fishery") |> 
    tidyr::pivot_longer(cols = 3:dplyr::last_col(),
                        names_to = "value", 
                        values_to = "answer") |> 
    dplyr::mutate(factor = case_when(
            value %in% c("overfished", "overfishing", "rebuilding_plan", "rebuilding_target", "ssb", "relative_ssb") ~ "Biomass",
            value %in% c("recruit_incl", "recruitment_model", "beg_recruit_yr", "other_recruit_info", "recruit_ests") ~ "Recruitment",
            value %in% c("climate_vulnerability", "climate_direction") ~ "Climate Vulnerability",
            value %in% c("no_of_prey", "prey_information") ~ "Fish Condition",
            value %in% c("assessment_type", "assessment_model", "retro_pattern", "retro_val", "data_used", "missing_data", "uncertainty_sources", "terminal_assessment_year") ~ "Assessment and Uncertainty",
            value %in% c("commercial_revenue", "commercial_catch", "commercial_dealers", "commercial_mgmt", "commercial_vessels", "commercial_dealers", "commercial_ports", "other_quota_reliance", "other_fisheries") ~ "Commercial Fishery Characterization",
            value %in% c("rec_acl", "rec_fishery", "recreational_catch", "recreational_trips", "recreational_mgmt", "recreational_ports") ~ "Recreational Fishery Characterization", 
            value == "other_econ_info" ~ "Other Socioeconomic Information",
            value %in% c("fmsy_ref_pt", "ssb_ref_pt", "msy_ref_pt", "OFL", "ABC", "harvest_control_rules", "accountability_measures", "signif_catch_present", "signif_catch_information") ~ "Additional Information",
            TRUE ~ value
          )) |>
    dplyr::relocate("factor", .after = "stock") |> 
    tidyr::drop_na(factor) |> 
    # replace any underscores in the value and answer columns with spaces
    dplyr::mutate(value = dplyr::case_when(
                        value == "ssb" ~ "SSB",
                        value == "relative_ssb" ~ "Relative SSB",
                        value == "recruit_incl" ~ "Recruitment is estimated", 
                        value == "recruit_ests" ~ "Recruitment estimates", 
                        value == "other_recruit_info" ~ "Other Recruitment Information",
                        value == "beg_recruit_yr" ~ "Initial year of time series",
                        value == "retro_pattern" ~ "Retrospective Pattern", 
                        value == "retro_val" ~ "Retrospective Values",
                        value == "commercial_mgmt" ~ "Commercial Management Uncertainty Buffer", 
                        value == "rec_acl" ~ "Recreational Fishery Management",
                        value == "rec_fishery" ~ "Recreational Fishery Activity", 
                        value == "recreational_mgmt" ~ "Recreational Management Uncertainty Buffer", 
                        value == "other_econ_info" ~ "Other Socioeconomic Information", 
                        value == "signif_catch_present" ~ "Significant Catch Present", 
                        value == "signif_catch_information" ~ "Significant Catch Information",
                        value == "fmsy_ref_pt" ~ "FMSY Reference Point", 
                        value == "ssb_ref_pt" ~ "SSB MSY Reference Point",
                        value == "msy_ref_pt" ~ "MSY Reference Point",
                        value %in% stringr::str_subset(value, "[:lower:]") ~ stringr::str_to_title(stringr::str_replace_all(value, "_", " ")),
                        TRUE ~ str_replace_all(value, "_", " ")),
                  answer = str_replace_all(answer, "_", " "), 
                answer = case_when(
                  is.na(answer) ~ paste("Not provided"), 
                  TRUE ~ answer
                ))
  
  ### Reorder the factors so assessment and uncertainty is first. 
  #1) create an object containing the factor names in the desired order
  reorder_levels <- c("Assessment and Uncertainty", "Biomass", "Recruitment", "Climate Vulnerability", "Fish Condition", "Commercial Fishery Characterization", "Recreational Fishery Characterization", "Other Socioeconomic Information", "Additional Information")
 
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
    # select only the columns that did not include the following information
    dplyr::select(!c(starts_with("time"), "session_id", "browser", "ip_address", "current_page", ends_with("rationale"), ends_with("source"), "climate_score_level", starts_with("comm_"), starts_with("rec_"))) |>  
    # make the table longer by taking
    tidyr::pivot_longer(cols = 3:dplyr::last_col(), # all the columns with the scores
                        names_to = "factor", # create a new column named factor from the column names
                        values_to = "score") |> # create a new column named score from the values in the columns
    tidyr::drop_na(any_of(c("report_year", "stock", "score"))) |>
    dplyr::mutate(score = as.integer(score)) #, # make the scores an integer
          #  scaled_score = scale_val(score))
  
  return(scores)
  
}

### Clean risk policy weights data ####
#' 
#' 
#'
clean_weights <- function(data){

  weights <- data |> 
    dplyr::relocate(report_year, .before = dplyr::everything()) |> 
    dplyr::select(!c(starts_with("time"), "session_id", "browser", "ip_address", "current_page", "weight_year", "weightings", "weightings_assessment", "form_id")) |>  
    tidyr::pivot_longer(cols = 2:dplyr::last_col(),
                        names_to = "factor", 
                        values_to = "weight") |> 
    dplyr::mutate(weight = as.integer(weight), 
           factor = str_extract(factor, pattern = "(?<=[:punct:])[:alpha:]+")) |> # extract words/letters that are preceded by a punctuation
    tidyr::drop_na(weight) |>
    dplyr::summarise(avg_weight = round(
                                        mean(weight, na.rm = T), 
                                        2),
                    .by = c(report_year, factor)) 

  return(weights)
}

### Plot the Recommended Probability ####
#' Plots the logistic function 
#' 
#'
plotRecProb <- function(data, z, RecProb, size = 1.5){
  
  horizon_tiers <- c("High Risk\nTolerance", "Intermediate Risk\nTolerance", "Low Risk\nTolerance")
  
  # zones created horizontally at the inflection points 
  horizontal_inf_pts <- data.frame(
   x = c(-4, -4, -4),
   ymin = c(0.5, 0.61, 0.89),
   ymax = c(0.61, 0.89, 1.00),
   Tiers = factor(horizon_tiers, levels = horizon_tiers),
   w = c(8, 8, 8)
  )
  
  ggplot2::ggplot() +
    ggplot2::lims(x = c(-4,4), y = c(0.5,1))+
    geom_rect(data = horizontal_inf_pts, aes(xmin = x, xmax = x + w, ymin = ymin, ymax = ymax, fill = Tiers), alpha = 0.35) +
    ggplot2::geom_function(fun = calcRecProb, linewidth = 1, lty = 3) + 
    ggplot2::geom_hline(aes(yintercept = 0.5, color = "MSA 50%\nprobability limit"), linetype = 'dashed', linewidth = 1) +
    ggplot2::geom_point(data = data, aes(x = {{z}}, y = {{RecProb}}, color = "Recommended\nProbability"), size =  size) +
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
    guides(
    color = guide_legend(order = 1, 
      override.aes = list(size = 2.5),
      theme = theme(#legend.justification = "left", 
      legend.title = element_text(face = "bold"))), # Hline
    fill = guide_legend(order = 2, 
      theme = theme(#legend.justification = "left", 
      legend.title = element_text(face = "italic", size = 8.5), 
      legend.margin = margin(0,0,0,0)))   # Rect
    ) +
    ggplot2::theme( 
        legend.position = "bottom", 
        legend.justification = "left", 
        legend.box = "vertical",
        legend.box.just = "left", 
        # legend.title = element_text(face = "italic"),
        legend.title.position = "top", 
        legend.byrow = TRUE,
        # legend.spacing = unit(0.75, "inches"), 
        legend.key.spacing = unit(0.5, "cm")) 
}
