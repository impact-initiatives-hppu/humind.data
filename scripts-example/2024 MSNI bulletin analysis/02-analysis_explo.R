library(tidyverse)
library(srvyr)
library(analysistools)

# Read the dataset with indicators and loa
main <- read.csv("outputs/main_with_indicators.csv")
loop <- read.csv("outputs/loop_with_indicators.csv")
# msni_bulletin_loa <- readxl::read_excel("inputs/msni_bulletin_loa.xlsx", sheet = "msni_bulletin_loa")
msni_explo_loa <- readxl::read_excel("inputs/msni_bulletin_loa.xlsx", sheet = "basic")

# Add the grouping variable and weights to the loop
group_vars <- msni_explo_loa$group_var |>
  na.omit() |>
  unique()

loop <- loop |>
  left_join(
    main |> select(uuid, weights, all_of(group_vars)),
    by = "uuid"
  )

# LOA should be divided into analysis:
# - main dataset with weights,
# - loop dataset with weights

## Analysis main - weighted
design_main <- main |>
  as_survey_design(strata = "stratum", weight = weights)

loa_main <- msni_explo_loa |>
  filter(dataset == "main")

results_main_weigthed <- create_analysis(
  design_main,
  loa_main,
  sm_separator = "."
)

saveRDS(results_main_weigthed, "outputs/results_main_weigthed.RDS")

## Loop analysis - weighted
# design_loop <- loop |>
#   as_survey_design(weight = "weight")
#
# loa_loop <- msni_explo_loa |>
#   filter(dataset == "loop")
#
# results_loop_weigthed <- create_analysis(
#   design_loop,
#   loa_loop,
#   sm_separator =  ".")
#
# saveRDS(results_loop_weigthed, "outputs/results_loop_weigthed.RDS")
