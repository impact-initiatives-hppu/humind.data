# Load packages -----------------------------------------------------------
# devtools::install_github("impact-initiatives-hppu/humind.data")
# devtools::install_github("impact-initiatives-hppu/impactR.viz")

require(pacman)
p_load(humind, humind.data, impactR.utils, dplyr)

# Prepare datasets --------------------------------------------------------
loop <- left_joints_dup(list(
  loop = dummy_raw_data$roster,
  edu_ind = dummy_raw_data$edu_ind,
  health_ind = dummy_raw_data$health_ind,
  nut_ind = dummy_raw_data$nut_ind),
  person_id,
  uuid)

main <- dummy_raw_data$main %>% mutate(weight=1)

## add age categories for main and loop dataset ----------------------------
main <- main %>% add_age_cat(age_col = "resp_age",
                             breaks = c(0, 17, 30, 65, 100, 120),
                             int_undefined = c(-999, 999),
                             char_undefined = "undefined",
                             new_colname = "resp_age_cat")
loop <- loop %>% add_age_cat(age_col = "ind_age",
                             breaks = c(0, 17, 30, 65, 100, 120),
                             int_undefined = c(-999, 999),
                             char_undefined = "undefined",
                             new_colname = "ind_age_cat")


## Example function to aggregate age from dataset (if not done within the general analysis)-----

#' @title Aggregate age
#' @description This function aggregates the age categories and calculates the proportion of each category for each group
#' @df The input data frame
#' @var_age_cat The name of the column that contains the information about the age categories
#' @gender_filtered Optionnal argument to filter out gender categories that you don't want to keep
#' @group_var Optional argument with hte name of the column that contains the information about the grouping for facet graph
#' @col_weight Optional argument with the name of the column that contains sampling weights. If NULL, no weighting will be applied. If provided, the output will give both weighted and unweighted proportions and counts
#' 
#' @return A dataframe with the proportion of each age category for each group
aggregate_age <- function(df = main, 
                          var_age_cat = "resp_age_cat",
                          var_gender = "resp_gender",
                          gender_filtered_values = c("other", "pnta", NA),
                          group_var = NULL,
                          col_weight = NULL){
  df_prop <- df %>% 
    filter(!(!!sym(var_gender) %in% gender_filtered_values)) %>%
    count(!!!syms(group_var), !!sym(var_gender), !!sym(var_age_cat)) %>%
    group_by(!!!syms(group_var)) %>% mutate(prop=n/sum(n))
  
  if (!is.null(col_weight)){
    df_prop_wt <- df %>% rename(weight=!!sym(col_weight)) %>%
      filter(!(!!sym(var_gender) %in% gender_filtered_values)) %>%
      count(!!!syms(group_var), !!sym(var_gender), !!sym(var_age_cat), wt = weight, name="n_w") %>%
      group_by(!!!syms(group_var)) %>% mutate(prop_w=n_w/sum(n_w))
    df_prop <- df_prop %>% full_join(df_prop_wt)
  }
  return(df_prop)
}

## using the function to aggregate age from the main and loop datasets:

## ungrouped
analysis_main_age <- main %>% aggregate_age(col_weight = "weight")
analysis_loop_age <- loop %>% left_join(main %>% select(uuid, weight)) %>%
  aggregate_age(var_age_cat = "ind_age_cat", var_gender = "ind_gender", col_weight = "weight")

## grouped by setting
analysis_main_age_grouped <- main %>% aggregate_age(col_weight = "weight", group_var = "setting")
analysis_loop_age_grouped <- loop %>% left_join(main %>% select(uuid, setting, weight)) %>%
  aggregate_age(group_var = "setting", var_age_cat = "ind_age_cat", var_gender = "ind_gender", col_weight = "weight")


## Example functions to plot age pyramid ---------
#' @title Age pyramid plot
#' @description This function creates an age pyramid plot from any dataframe that has a column with age categories and a column with a proportion (weighted or not)
#' @df The input data frame
#' @var_age_cat The name of the column that contains the information about the age categories
#' @col_stat The name of the column that contains proportion of individuals/households falling in each age category
#' @col_n The name of the column that contains the number of individuals/households falling in each age category
#' @value_men Choice value corresponding to men gender
#' @value_women Choice value corresponding to women gender
#' @unit Text to inform the sampling unit (Individual, Household) to be reported in the graph title and captions
#' @save Optional argument to save the plot as a .png file 
#' @group_var Optional argument with hte name of the column that contains the information about the grouping for facet graphs
#' @dir Optional argument with the directory and name of the file to save the plot base data as csv file
#'
#' @return A ggplot2 age pyramid plot 
plot_age_pyramid  <- function(df=age_table, 
                              var_gender="resp_gender",
                              var_age_cat="resp_age_cat",
                              col_stat="prop",
                              col_n_unw="n",
                              unit = "Respondent",
                              group_var=NULL,
                              save=F, 
                              age.labs=NULL,
                              value_men="male",
                              value_women="female",
                              title = NULL,
                              subtitle = NULL,
                              n_col=2,
                              dir = paste0("age_pyramid_ind.csv")){
  
  max <- round((max(df[[col_stat]])+.05)*10)/10
  if (is.null(age.labs)) age.labs <- df %>% mutate(start_age = as.numeric(sub(".*?([0-9]+).*", "\\1", !!sym(var_age_cat)))) %>%
      arrange(start_age) %>% select(-start_age) %>% pull(var_age_cat) %>% unique
  if (is.null(title)) plot_title <- paste0(unit, " age pyramid by gender") else plot_title <- title
  
  plot <- df %>%
    ggplot(aes(x = !!sym(var_age_cat), y = !!sym(col_stat), fill = !!sym(var_gender))) +
    geom_bar(data = df %>% filter(!!sym(var_gender) == value_men), stat = "identity", position = "identity") +
    geom_bar(data = df %>% filter(!!sym(var_gender) == value_women), stat = "identity", position = "identity", aes(y = -!!sym(col_stat))) +
    scale_x_discrete(labels = age.labs) +
    scale_y_continuous(labels = function(x) {scales::percent(abs(x))}, limits = c(-max,max)) +
    scale_fill_manual(values = setNames(c("#EE5859", "#0067A9"), c(value_women, value_men))) +
    labs(title = plot_title, subtitle=subtitle, x = "", y = paste0("% of ", tolower(unit), "s"), fill=paste0(unit, " gender")) +
    theme_minimal() + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5, size=9), legend.position = "bottom") +
    coord_flip()
  
  if (!is.null(col_n_unw)) {
    plot <- plot + labs(caption=paste0("Distribution for a total ", sum(df[[col_n_unw]]), " ", tolower(unit), "s reporting both age and gender."))
  }
  
  if (!is.null(group_var)) {
    plot <- plot + facet_wrap(vars(!!sym(group_var)), ncol = n_col)
  }
  
  if (save) write.csv(df_prop, dir, row.names = F)
  
  return(plot)
}

## using the functions to plot the age pyramid for the main and loop datasets:
p_load(ggplot2)
respondent.age.pyramid <- plot_age_pyramid(df = analysis_main_age)
individual.age.pyramid <- plot_age_pyramid(df = analysis_loop_age, 
                                           var_gender = "ind_gender", 
                                           var_age_cat = "ind_age_cat", 
                                           unit = "Individual")

## grouped plotting
## for respondent
respondent.age.pyramid.setting <- plot_age_pyramid(df = analysis_main_age_grouped, group_var = "setting", n_col = 3)
respondent.age.pyramid.setting

## for individual loop
individual.age.pyramid.setting <- plot_age_pyramid(df = analysis_loop_age_grouped,
                                                   var_gender = "ind_gender", var_age_cat =  "ind_age_cat", 
                                                   unit="Individual", group_var = "setting", n_col = 3)
individual.age.pyramid.setting

### If plotting from output analysis long table, make sure you have put resp_gender as disaggregating variable and resp_age_cat in the loa

# List of grouping variables
group_vars <- list("admin1", "resp_gender")
loop <- df_diff(loop, main, uuid) |>
  left_join(
    main |> select(uuid, weight, !!!unlist(group_vars)),
    by = "uuid"
  )

design_main <- main |> as_survey_design(weight = weight)
survey <- survey_updated |> split_survey(type) |> rename(label = label_english)
choices <- choices_updated |> rename(label = label_english)
loa <- loa |> filter(level == "main", var=="resp_age_cat")

# Run analysis ------------------------------------------------------------
# Main analysis - weighted
if (nrow(loa) > 0) {
  an_main <- impactR.analysis::kobo_analysis_from_dap_group(
    design_main,
    loa,
    survey,
    choices,
    l_group = group_vars,
    choices_sep = "/")
} else {
  an_main <- tibble()
}

### plotting using the output analysis long result table
## Make sure to filter the long result table to display the age/gender cross disaggregated analysis, filtering out unwanted other/pnta gender values
## Make sure to update correclty the var_gender column to group_key_value if you grouped by resp gender and var_value for resp_age_cat
an.plot.age.pyramid.respondent <- an_main %>% 
  filter(group_key=="resp_gender", group_key_value %in% c("female", "male")) %>%
  plot_age_pyramid(var_gender = "group_key_value", var_age_cat = "var_value",
                   value_women = "female", value_men = "male", col_stat = "stat")

### Example function to compare main and loop age/gender distributions:
#' @title Compare proportions of age categories between household level and individual level
#' @description This function compares the proportions of age categories by gender between household respondent and individual level sample
#' @df_resp The input data frame at household level to map age and gender of respondent (or head of household)
#' @col_resp_gender Column name in df_resp containing respondent gender
#' @col_resp_age_cat Column name in df_resp containing respondent age category
#' @df_ind The input data frame at the individual level 
#' @col_ind_gender Column name in df_ind containing individual gender
#' @col_ind_age_cat Column name in df_ind containing individual age category
#' @group_var Column name in df_resp containing the grouping variable. If not null you should also provide group_var_ind
#' @group_var_ind Column name in df_ind containing the grouping variable. If not null you should also provide group_var
#' @value_gender choice vector of gender categories to be kept for the comparison (usually men and women / male and female)
#' @all.age.group If TRUE, the function will return the proportion of any age group present in both sample. If FALSE, it will provide proportion only for age groups present in both household and individual level sample (exluding children)
#' @return A dataframe with the proportion of each age category for each group as well as the difference in proportion between respondent and individual level. A positive difference means
compare_main_loop <- function(
    df_resp=main,
    col_resp_gender="resp_gender",
    col_resp_age_cat="resp_age_cat",
    df_ind=loop,
    col_ind_gender="ind_gender",
    col_ind_age_cat="ind_age_cat",
    col_ind_age="ind_age",
    group_var=NULL, 
    group_var_ind=NULL,
    filter_value_gender=c("male", "female"),
    all.age.group=F
){
  
  ## By default, filter out under 18 individuals to have proportions that are calculated over adults only
  if (!all.age.group) {df_ind <- df_ind %>% filter(!!sym(col_ind_age)>17)}
  
  df_count_resp <- df_resp %>%
    filter(!!sym(col_resp_gender) %in% filter_value_gender) %>%
    count(!!!syms(group_var), !!sym(col_resp_gender), !!sym(col_resp_age_cat), name = "n_resp") %>% 
    group_by(!!!syms(group_var)) %>% mutate(prop_resp=n_resp/sum(n_resp)) %>% 
    rename(gender = !!sym(col_resp_gender), age_cat:=!!sym(col_resp_age_cat))
  
  ## Check that both group var are entered or both are null or throw a message
  if (any(is.null(c(group_var, group_var_ind))) & !all(is.null(c(group_var, group_var_ind)))) {
    stop("You must provide both group_var and group_var_ind or none of them")
  }
  if (!is.null(group_var) & !is.null(group_var_ind)) {
    df_count_resp <- df_resp %>% rename(!!sym(group_var_ind):=!!sym(group_var))
  }
  
  df_count_ind <- df_ind %>%
    filter(!!sym(col_ind_gender) %in% filter_value_gender) %>%
    count(!!!syms(group_var_ind), !!sym(col_ind_gender), !!sym(col_ind_age_cat), name = "n_ind") %>%
    group_by(!!!syms(group_var_ind)) %>% mutate(prop_ind=n_ind/sum(n_ind)) %>% 
    rename(gender = !!sym(col_ind_gender), age_cat:=!!sym(col_ind_age_cat))
  
  ## Join both datasets and compute proportion difference
  df_all <- df_count_resp %>% full_join(df_count_ind) %>% mutate(prop_diff = prop_resp - prop_ind)
  
  return(df_all)
}

compare.sample <- compare_main_loop(df_resp = main,
                                    col_resp_gender = "resp_gender",
                                    col_resp_age_cat = "resp_age_cat",
                                    df_ind = loop,
                                    col_ind_gender = "ind_gender",
                                    col_ind_age_cat = "ind_age_cat",
                                    col_ind_age = "ind_age",
                                    group_var = NULL,
                                    group_var_ind = NULL,
                                    filter_value_gender = c("male", "female"),
                                    all.age.group = F)
view(compare.sample)

#' @title Plot age pyramid for the comparison of age categories between household level and individual level
#' @description This function plots the age pyramid for the comparison of age categories between household level and individual level
#' @df The input data frame containing the proportion of each age category for each group as well as the difference in proportion between respondent and individual level
#' @col_age_cat Column name in df containing the age category
#' @col_prop_diff Column name in df containing the difference in proportion between respondent and individual level
#' @col_gender Column name in df containing gender category
#' @group_var Column name in df containing the grouping variable for faceting the graph. Default value set to NULL.
#' @val_women  Choice value corresponding to women gender category
#' @val_men  Choice value corresponding to men gender category
#' @age.labs  Vector of age categories to be used as labels on the x-axis. Default value set to NULL.
#' 
#' @return A ggplot object representing the age pyramid for the comparison of proportion for each age category between household level and individual level, by gender
plot_age_pyramid_main_loop <- function(df=compare.sample,
                                       col_age_cat="age_cat",
                                       col_prop_diff="prop_diff",
                                       col_gender="gender",
                                       group_var=NULL,
                                       val_women="female",
                                       val_men="male",
                                       age.labs=NULL){
  
  max <- df$prop_diff %>% abs %>% max %>% round(., 1)
  if (is.null(age.labs)) age.labs <- df %>% mutate(start_age = as.numeric(sub(".*?([0-9]+).*", "\\1", !!sym(col_age_cat)))) %>%
      arrange(start_age) %>% select(-start_age) %>% pull(col_age_cat) %>% unique
  
  plot <- df %>%
    ggplot(aes(x=!!sym(col_age_cat), y=!!sym(col_prop_diff), fill=!!sym(col_gender))) +
    geom_bar(data = df, stat = "identity", position = "dodge") +
    scale_x_discrete(labels = age.labs) + scale_y_continuous(labels = scales::percent_format(), limits = c(-max,max)) +
    scale_fill_manual(values = setNames(c("#EE5859", "#0067A9"), c(val_women, val_men))) +
    labs(title = paste0("Difference between proportion of respondents and proportion of individuals\nby age category and gender\n"),
         x = "", y = paste0("\n% difference between respondent and individual\nby age category"), 
         caption=paste0("Distribution for a total ", sum(df$n_resp), " respondents who where surveyed and a total of ", 
                        sum(df$n_ind), " individuals in the surveyed households.\nA positive difference means that respondent are overrepresented compared to the individual sample age/gender breakdown for the age group and gender considered."),
         fill=paste0("Gender")) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5, size=9), legend.position = "bottom") +
    coord_flip() + facet_grid(vars(!!!syms(group_var)))
  return(plot)
}


## Plot Age/gender distribution between main and loop ------
plot.age.pyramid_main.loop <- compare.sample %>% 
  plot_age_pyramid_main_loop(col_age_cat = "age_cat",
                             col_prop_diff = "prop_diff",
                             col_gender = "gender",
                             group_var = NULL,
                             val_women = "female",
                             val_men = "male",
                             age.labs = NULL)
plot.age.pyramid_main.loop
