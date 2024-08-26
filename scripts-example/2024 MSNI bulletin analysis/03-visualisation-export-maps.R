library(tidyverse)
library(presentresults)
results_table_labeled <- readRDS("outputs/labeled_results_table.RDS")

results_table_filtered <- results_table_labeled |>
  filter(
    analysis_var %in% c(
      "msni_in_need", "comp_health_in_need", "comp_prot_in_need",
      "comp_edu_in_need", "comp_foodsec_in_need", "comp_wash_in_need",
      "comp_snfi_in_need"
    ),
    analysis_var_value == "1",
    group_var == "admin1"
  )

results_table_filtered
results_table_recoded_5_classes <- results_table_filtered %>%
  create_table_for_map(number_classes = 5)
results_table_recoded_5_classes
results_table_recoded_5_classes |> write.csv("outputs/maps/maps_input_5_classes.csv")

results_table_recoded_6_classes <- results_table_filtered %>%
  create_table_for_map(number_classes = 6)
results_table_recoded_6_classes
results_table_recoded_6_classes |> write.csv("outputs/maps/maps_input_6_classes.csv")

# what the function does
results_table_filtered |>
  select(analysis_var, group_var, group_var_value, stat) |>
  mutate(stat_recoded = case_when(
    stat == 0 ~ 1,
    stat <= .25 ~ 2,
    stat <= .50 ~ 3,
    stat <= .75 ~ 4,
    stat <= 1 ~ 5,
    TRUE ~ NA_integer_
  )) |>
  pivot_wider(id_cols = group_var_value, names_from = analysis_var, values_from = stat_recoded)

# Example for categoric variable.
results_table_labeled |>
  filter(
    analysis_var == "wash_drinking_water_quantity",
    analysis_var_value == "always",
    group_var == "admin1"
  ) %>%
  create_table_for_map(number_classes = 5)
