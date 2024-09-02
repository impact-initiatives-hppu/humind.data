#Step-1: Install and Load the humind package from GitHub

#We need devtools package to be able to install any package from GitHub. 

if (!require("devtools")) {
  install.packages("devtools")
  library(devtools)}

#Then we can install humind v2024.1.1 as the following, if release was not specified, main head version will be installed: 
if (!require("humind")) {
  devtools::install_github("impact-initiatives-hppu/humind@v2024.1.1")
  library(humind)}
  
#Step-2: loading the first columns from the data related to shelter type indicators. 

df <- data.frame(
    snfi_shelter_type = c("hosted", "collective_center", "hosted", "individual_shelter"),
    snfi_shelter_type_individual = c("tent", NA, "house", "unfinished_building")
  )
  
  result <- add_shelter_type_cat(df)

#Step-3: Installing tidyverse or dplyr for mutate function to update the df with new calculations. 
  
  if (!require("tidyverse")) {
    install.packages("tidyverse")
    library(tidyverse)}
  
  df <- df %>% mutate(tibble::tibble(
    snfi_shelter_issue = c("temperature, ventilation", "lack_privacy", "none", "lack_privacy"),
    'snfi_shelter_issue/temperature' = c(1, 0, 0, 0),
    'snfi_shelter_issue/ventilation' = c(1, 0, 0, 0),
    'snfi_shelter_issue/leak' = c(0, 0, 0, 0),
    'snfi_shelter_issue/lack_privacy' = c(0, 1, 0, 1),
    'snfi_shelter_issue/lack_space' = c(0, 0, 0, 0),
    'snfi_shelter_issue/lock' = c(0, 0, 0, 0),
    'snfi_shelter_issue/lack_lighting' = c(0, 0, 0, 0),
    'snfi_shelter_issue/difficulty_move' = c(0, 0, 0, 0),
    'snfi_shelter_issue/dnk' = c(0, 0, 0, 0),
    'snfi_shelter_issue/pnta' = c(0, 0, 0, 0),
    'snfi_shelter_issue/other' = c(0, 0, 0, 0),
    'snfi_shelter_issue/none' = c(0, 0, 1, 0)
  )
  )
  
  result <- result %>% mutate(add_shelter_issue_cat(df))
  

#Step-4: Add "cannot cat" based on the snfi fds indicators 
  
df <- df %>% mutate (data.frame(
  snfi_fds_cooking = c("no_cannot", "yes_no_issues", "no_no_need", "no_cannot"),
  snfi_fds_sleeping = c("no_cannot", "yes_issues", "yes_no_issues", "no_cannot"),
  snfi_fds_storing = c("no_cannot", "yes_no_issues", "yes_issues", "no_cannot"),
  snfi_fds_personal_hygiene = c("yes_issues", "yes_no_issues", "yes_no_issues", "no_cannot"),
  energy_lighting_source = c("none", "candle", "yes_no_issues", "no_cannot")
))

  result <- result %>% mutate (add_fds_cannot_cat(df))
  
 
# Step-5: Add the hlp occupancy cat 
  
df <- df %>% mutate(
    hlp_occupancy = c("no_agreement", "hosted_free", "rented", "hosted_free")
  )
  
  result <- result %>% mutate (add_occupancy_cat(df))


# Final step: calculate the sector comp for snfi 

  results <- add_comp_snfi(result)