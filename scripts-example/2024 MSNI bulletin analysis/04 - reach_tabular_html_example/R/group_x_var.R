gt_cols_group_merge <- function(gt_table, 
                                spanner_header, 
                                stat_cols = c("stat_low", "stat_upp"), 
                                pattern = NULL , 
                                rows = everything() ){ 
  for(i in seq_along(spanner_header)){
    column_val <- paste0(spanner_header[i], " %/% stat")
    columns_uncert <-  paste(spanner_header[i], c(stat_cols), sep = " %/% ")
    gt_table <- gt_table %>% 
      cols_merge_uncert(col_val = column_val, 
                        col_uncert =  columns_uncert, 
                        rows = {{rows}}
      )
  }
  return(gt_table)
}

create_gt_table_group_x_var <- function(results_table) {
  short_results_table <- results_table|> 
    select(all_of(c("label_analysis_type",
                    "label_analysis_var",
                    "label_analysis_var_value",
                    "label_group_var",
                    "label_group_var_value",
                    "analysis_type",
                    "stat",
                    "stat_low",
                    "stat_upp",
                    "n",
                    "n_total"))) 
  
  if(all(stringr::str_detect(short_results_table[["analysis_type"]], "prop"))) {
    short_results_table <- short_results_table|> 
      filter(!(is.na(label_analysis_var_value) | label_analysis_var_value == "NA"))
  }   
  
  # unique_label_analysis_var_value <- unique(short_results_table$label_analysis_var_value)
  
  table_gt_group_x_var <-  short_results_table |> 
    pivot_wider(id_cols = all_of(c("label_analysis_type","label_group_var","label_group_var_value","label_analysis_var", "n_total")),
                names_from = label_analysis_var_value,
                values_from = all_of(c("stat", "stat_low", "stat_upp")), 
                names_glue = "{label_analysis_var_value} %/% {.value}", names_vary = "slowest"
    )
return(table_gt_group_x_var)
}

create_gt_group_x_var <- function(table_gt_group_x_var){
  if(all(stringr::str_detect(table_gt_group_x_var[["label_analysis_type"]], "Proportion"))) {
    type_number <- "prop"
  } else if (all(table_gt_group_x_var[["label_analysis_type"]] %in% c("Mean", "Median", "Ratio"))) {
    type_number <- "digits"
  } else {
    type_number <- "unknown"
  }
  table_to_return <- table_gt_group_x_var |> 
    select(-label_analysis_type, - label_analysis_var) |> 
    gt(rowname_col = "label_group_var_value",
       groupname_col ="label_group_var")  |>
    tab_spanner_delim(delim = " %/% ", reverse = T)
  
  unique_label_analysis_var_value <- table_gt_group_x_var%>%
    names() %>% 
    grep(x=.,pattern = " %/% stat", value = T) %>% 
    str_remove_all(" %/% (.*)") %>% 
    unique()
  
  title_header <- unique(table_gt_group_x_var$label_analysis_var)
  
  table_to_return2 <- table_to_return
  table_to_return2 <- gt_cols_group_merge(table_to_return, spanner_header = unique_label_analysis_var_value)
  table_to_return2 <- table_to_return2|> 
    rm_spanners() |> 
    cols_align("right") |> 
    cols_move_to_end(n_total) |> 
    cols_label(n_total = "n") |> 
    sub_missing() |> 
    tab_header(title = title_header)
  
  
  locale <- "en"
  if(type_number == "prop") {
    table_to_return2 <- table_to_return2 |> 
      fmt_percent(columns = -n_total, decimals = 0,
                  locale = locale)
    
  }
  
  if(type_number == "digits") {
    cols_label_digits <- unique(table_gt_group_x_var$label_analysis_type)
    
    table_to_return2 <- table_to_return2 |> 
      fmt_number(columns = -n_total, 
                 decimals = 0,
                 locale = locale) |> 
      cols_label(`NA %/% stat` = cols_label_digits)
      
    
  }
  return(table_to_return2)
}


