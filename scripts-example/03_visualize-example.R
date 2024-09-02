source("scripts-example/02-analysis-example.R")


# This is a dev work
# pak::pak("impact-initiatives-hppu/impactR.viz")
library(impactR.viz)

# Visualize Metric 1 - MSNI 3 and above, by gender of the hoh
an_main |>
  filter(
    group_key == "hoh_gender",
    var == "msni_in_need",
    var_value == 1
  ) |>
  mutate(stat = round(stat * 100, 1)) |>
  bar(
    x = "group_key_value",
    y = "stat",
    title = "Metric 1 - % of households in need, by gender of the HoH"
  )

# Visualize Metric 2 - MSNI 4 and above, by age category of the HoH
an_main |>
  filter(
    group_key == "hoh_age_cat",
    var == "msni_in_acute_need",
    var_value == 1
  ) |>
  mutate(stat = round(stat * 100, 1)) |>
  bar(
    x = "group_key_value",
    y = "stat",
    title = "Metric 2 - % of households in acute need, by age category of the HoH"
  )


# Visualize % of households by shelter composite score, by age category of the HoH
an_main |>
  filter(
    group_key == "hoh_age_cat",
    var == "comp_snfi_score"
  ) |>
  mutate(stat = round(stat * 100, 1)) |>
  bar(
    x = "group_key_value",
    y = "stat",
    group = "var_value",
    title = "% of households by shelter composite score, by age category of the HoH",
    palette = "quant_5_red",
    reverse_guide = F
  )

# Now do it by category of the HoH and by shelter composite score
an_main |>
  filter(
    group_key == "hoh_age_cat",
    var == "comp_snfi_score"
  ) |>
  mutate(stat = round(stat * 100, 1)) |>
  bar(
    x = "var_value",
    y = "stat",
    group = "group_key_value",
    title = "% of households by shelter composite score, by age category of the HoH",
    palette = "cat_5_custom_1",
    reverse_guide = F
  )

