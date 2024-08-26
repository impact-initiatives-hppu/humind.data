library(tidyverse)
library(presentresults)

results_main_weigthed <- readRDS("outputs/results_main_weigthed.RDS")
# results_loop_weigthed <- readRDS("outputs/results_loop_weigthed.RDS")

kobo_survey <- readxl::read_excel("inputs/REACH_2024_MSNA-kobo-tool_draft_v11.xlsx", sheet = "survey")
kobo_choices <- readxl::read_excel("inputs/REACH_2024_MSNA-kobo-tool_draft_v11.xlsx", sheet = "choices")

update_survey <- readxl::read_excel("inputs/update-kobo-tool.xlsx", sheet = "update_survey")
update_choices <- readxl::read_excel("inputs/update-kobo-tool.xlsx", sheet = "update_choices")

updated_survey <- bind_rows(kobo_survey, update_survey)
updated_choices <- bind_rows(kobo_choices, update_choices)


# 1 bind results
results_main_weigthed$results_table$analysis_key %>%
  duplicated() %>%
  sum()
# results_loop_weigthed$results_table$analysis_key %>% duplicated() %>% sum()

results_main_weigthed$results_table$dataset <- "main"
# results_loop_weigthed$results_table$dataset <- "loop"

# binded_results_table <- bind_rows(results_main_weigthed$results_table,
#                                   results_loop_weigthed$results_table)
# 2 add labels
review_kobo_labels_results <- review_kobo_labels(updated_survey,
  updated_choices,
  results_table = results_main_weigthed$results_table
)
review_kobo_labels_results
kobo_choices_fixed <- updated_choices |>
  filter(`label::english` != "Surface water (river, dam, lake, pond, stream, canal, irrigation channel)")
duplicated_listname_label <- review_kobo_labels_results |>
  filter(comments == "Kobo choices sheet has duplicated labels in the same list_name.")

kobo_choices_fixed <- kobo_choices_fixed |>
  group_by(list_name) |>
  mutate(`label::english` = case_when(
    list_name %in% duplicated_listname_label$list_name ~ paste(`label::english`, row_number()),
    TRUE ~ `label::english`
  )) |>
  ungroup()
review_kobo_labels_results <- review_kobo_labels(updated_survey,
  kobo_choices_fixed,
  results_table = results_main_weigthed
)
review_kobo_labels_results

label_dictionary <- create_label_dictionary(updated_survey,
  kobo_choices_fixed,
  results_table = results_main_weigthed$results_table
)

results_table_labeled <- add_label_columns_to_results_table(
  results_main_weigthed$results_table,
  label_dictionary
)

nrow(results_table_labeled) == nrow(results_main_weigthed$results_table)

results_table_labeled <- results_table_labeled |>
  mutate(
    group_var = if_else(is.na(group_var), "Overall", group_var),
    group_var_value = if_else(is.na(group_var_value), "Overall", group_var_value),
    label_group_var = if_else(is.na(label_group_var) | label_group_var == "NA", "Overall", label_group_var),
    label_group_var_value = if_else(is.na(label_group_var_value) | label_group_var_value == "NA", "Overall", label_group_var_value)
  )

results_table_labeled %>% saveRDS("outputs/labeled_results_table.RDS")
