
# Load packages -----------------------------------------------------------

# install.packages("pak")
# pak::pak("gnoblet/impactR.utils")
library(impactR.utils)

# Loading humind loads functions
# pak::pak("impact-initiatives-hppu/humind")
library(humind)

# Loading example data
# pak::pak("impact-initiatives-hppu/humind.data")
library(humind.data)
data(dummy_raw_data, package = "humind.data")

# Needed tidyverse packages
library(dplyr)


# Prepare datasets --------------------------------------------------------

loop <- left_joints_dup(list(
  loop = dummy_raw_data$roster,
  edu_ind = dummy_raw_data$edu_ind,
  health_ind = dummy_raw_data$health_ind,
  nut_ind = dummy_raw_data$nut_ind),
  person_id,
  uuid)

main <- dummy_raw_data$main


# Add indicators ----------------------------------------------------------

#------ Add to loop
loop <- loop |>
  # Demographics
  add_age_cat("ind_age", breaks = c(-1, 17, 59, 120)) |>
  add_age_18_cat("ind_age") |>
  # Education
  add_loop_edu_ind_age_corrected(main = main, month = 7) |>
  add_loop_edu_access_d() |>
  add_loop_edu_barrier_protection_d() |>
  add_loop_edu_disrupted_d() |>
  # WGQ-SS
  add_loop_wgq_ss() |>
  # Health --- example if wgq_dis_3 exists
  add_loop_healthcare_needed_cat(wgq_dis = "wgq_dis_3")

#------ Add loop to main
main <- main |>
  # Education
  add_loop_edu_ind_schooling_age_d_to_main(loop = loop) |>
  add_loop_edu_access_d_to_main(loop = loop) |>
  add_loop_edu_barrier_protection_d_to_main(loop = loop) |>
  add_loop_edu_disrupted_d_to_main(loop = loop) |>
  # WGQ-SS
  add_loop_wgq_ss_to_main(loop = loop) |>
  # Health
  add_loop_healthcare_needed_cat_to_main(
    loop =loop,
    ind_healthcare_needed_no_wgq_dis = "health_ind_healthcare_needed_no_wgq_dis",
    ind_healthcare_needed_yes_unmet_wgq_dis = "health_ind_healthcare_needed_yes_unmet_wgq_dis",
    ind_healthcare_needed_yes_met_wgq_dis = "health_ind_healthcare_needed_yes_met_wgq_dis")

#------- Clean up food security indicators

# HHS calculation should not contain Don't know or Prefer not to answer
main <- mutate(
  main,
  across(
    c("fsl_hhs_nofoodhh", "fsl_hhs_sleephungry", "fsl_hhs_alldaynight"),
    \(x) case_when(
      x %in% c("dnk", "pnta") ~ NA_character_,
      .default = x
    )
  )
)

# Add to main
main <- main |>
  # Demographics
  add_hoh_final() |>
  add_age_cat(age_col = "resp_age", breaks = c(-1, 17, 59, 120)) |>
  add_age_18_cat(age_col = "resp_age") |>
  add_age_cat(age_col = "hoh_age", breaks = c(-1, 17, 59, 120)) |>
  add_age_18_cat(age_col = "hoh_age") |>
  # Protection
  add_child_sep_cat() |>
  # WASH
  # WASH - Sanitation facility
  add_sanitation_facility_cat() |>
  add_sharing_sanitation_facility_cat() |>
  add_sharing_sanitation_facility_n_ind() |>
  add_sanitation_facility_jmp_cat() |>
  # WASH - Water
  add_drinking_water_source_cat() |>
  add_drinking_water_time_cat() |>
  add_drinking_water_time_threshold_cat() |>
  add_drinking_water_quality_jmp_cat() |>
  # WASH - Hygiene
  add_handwashing_facility_cat() |>
  # SNFI
  add_shelter_type_cat() |>
  add_shelter_issue_cat() |>
  add_fds_cannot_cat() |>
  # HLP
  add_occupancy_cat() |>
  # Food security
  add_lcsi() |>
  add_hhs() |>
  add_fcs(cutoffs = "normal") |>
  add_rcsi() |>
  add_fcm_phase() |>
  add_fclcm_phase() |>
  # Cash & markets
  add_income_source_zero_to_sl() |>
  add_income_source_prop() |>
  add_income_source_rank() |>
  # Expenditure 
  add_expenditure_type_zero_freq(
    # Note that in the initial kobo template the utilities column was:
    # cm_expenditure_frequent_utilitues
    # Spelling mistake
    expenditure_freq_types = c("cm_expenditure_frequent_food",
      "cm_expenditure_frequent_rent",
      "cm_expenditure_frequent_water",
      "cm_expenditure_frequent_nfi",
      "cm_expenditure_frequent_utilities",
      "cm_expenditure_frequent_fuel",
      "cm_expenditure_frequent_transportation",
      "cm_expenditure_frequent_communication",
      "cm_expenditure_frequent_other")
  ) |> 
  add_expenditure_type_prop_freq(
    # Note that in the initial kobo template the utilities column was:
    # cm_expenditure_frequent_utilitues
    # Spelling mistake
    "cm_expenditure_frequent_food",
    "cm_expenditure_frequent_rent",
    "cm_expenditure_frequent_water",
    "cm_expenditure_frequent_nfi",
    "cm_expenditure_frequent_utilities",
    "cm_expenditure_frequent_fuel",
    "cm_expenditure_frequent_transportation",
    "cm_expenditure_frequent_communication",
    "cm_expenditure_frequent_other"
  ) |> 
  add_expenditure_type_freq_rank(
    # Note that in the initial kobo template the utilities column was:
    # cm_expenditure_frequent_utilitues
    # Spelling mistake
    expenditure_freq_types = c(
    "cm_expenditure_frequent_food",
      "cm_expenditure_frequent_rent",
      "cm_expenditure_frequent_water",
      "cm_expenditure_frequent_nfi",
      "cm_expenditure_frequent_utilities",
      "cm_expenditure_frequent_fuel",
      "cm_expenditure_frequent_transportation",
      "cm_expenditure_frequent_communication",
      "cm_expenditure_frequent_other")
  ) |> 
  add_expenditure_type_zero_infreq() |> 
  add_expenditure_type_prop_infreq() |> 
  add_expenditure_type_infreq_rank() |>
  # AAP
  add_received_assistance() |>
  add_access_to_phone_best() |>
  add_access_to_phone_coverage()


# Add composites ---------------------------------------------------------

# Add sectoral composites
main <- main |>
  add_comp_foodsec() |>
  add_comp_snfi() |>
  add_comp_prot() |>
  add_comp_health() |>
  add_comp_wash() |>
  add_comp_edu()

# Add MSNI score and the 4 metrics
main <- main |>
  add_msni()


