library(impactR.utils)
library(dplyr)

loa <- import_full_xlsx("data-raw/loa.xlsx")

survey_update <- loa$update_survey
choices_update <- loa$update_choices

kobo <- import_full_xlsx("data-raw/REACH_2024_MSNA-kobo-tool_draft_v11.xlsx")
survey <- kobo$survey
choices <- kobo$choices

survey_updated <- bind_rows(
  survey,
  survey_update
)

choices_updated <- bind_rows(
  choices,
  choices_update |> mutate(
    across(
      everything(),
      \(x) as.character(x)
    )
  )
)

loa <- loa$loa

usethis::use_data(loa, overwrite = TRUE)
usethis::use_data(survey_updated, overwrite = TRUE)
usethis::use_data(choices_updated, overwrite = TRUE)
