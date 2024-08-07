# Load libraries
# pak::pak("gnoblet/impactR.kobo")
library(impactR.kobo)
# pak::pak("impact-initiatives-hppu/humind")
library(impactR.analysis)
library(srvyr)

# First, let's source the compose.R file
# - it loads needed data
# - it composes the needed new column
source("scripts-example/01-compose-example.R")

# Note that there are warnings that are fixed in v2024.1.1 (see https://github.com/impact-initiatives-hppu/humind/tree/dev2024.1.1)

# Loads other needed data -- explicit call
data(loa, package = "humind.data")
data(survey_updated, package = "humind.data")
data(choices_updated, package = "humind.data")

# Analysis groups ---------------------------------------------------------

# List of grouping variables
group_vars <- list("admin1", "hoh_gender", "hoh_age_cat")
# Here you can add in group_vars the variable you want to disaggregate by
# Following your data disagregation plan, see MSNI guidance for more information

# Add this list of variables to loop (including weights, and stratum if relevant), joining by uuid
# and removing columns existing in both
loop <- df_diff(loop, main, uuid) |>
  left_join(
    main |> select(uuid, weight, !!!unlist(group_vars)),
    by = "uuid"
  )


# Prepare design and kobo -------------------------------------------------

# Design main - weighted
design_main <- main |>
  as_survey_design(weight = weight)

# Survey - one column must be named label
# and the type column must be split into type and list_name
survey <- survey_updated |>
  split_survey(type) |>
  rename(label = label_english)

# Choices - one column must be named label
choices <- choices_updated |>
  rename(label = label_english)

# Loa for main only
loa <- loa |>
  filter(dataset == "main")

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


# Count missing values ----------------------------------------------------

# On top of the analysis, required (and good practice to look at missing values).
# A function in impactR.analysis does that.

# With the below we get the
na_n <- count_missing_values(
  df = main,
  vars = colnames(main)
)
tail(na_n, 20)

# For instance, we see that we have 46 missing values for the health score or 19.1%
# Let's look at the health composite score by gender of the hoh
na_n_hoh_gender <- count_missing_values(
  df = main,
  vars = colnames(main),
  group = "hoh_gender"
) |>
  filter(
    hoh_gender %in% c("male", "female", "other"),
    var == "comp_health_score")
na_n_hoh_gender
