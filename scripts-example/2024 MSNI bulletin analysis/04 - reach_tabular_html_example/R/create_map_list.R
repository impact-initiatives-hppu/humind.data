create_map_nested_list <- function(results_table, grouped_by) {
  theme_list_names <- results_table |>
    group_by(!!sym(grouped_by)) |>
    group_keys()
  
  results_table |>
    group_by(!!sym(grouped_by)) |>
    group_split() |>
    set_names(theme_list_names[[grouped_by]])
}