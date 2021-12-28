correct.zeroes <- function(summary) {
  zeroes <- which(summary$dependent.var.value == 0 & summary$numbers == 1)
  summary$dependent.var.value[zeroes] <- 1
  summary$numbers[zeroes] <- 0
  summary$min[zeroes] <- 0
  summary$max[zeroes] <- 0
  return(summary)
}

temp <-
  indiv_df %>% group_by(X_submission__uuid) %>% summarize(n = n())

df$hh_members_num <-
  temp$n[match(df$X_uuid, temp$X_submission__uuid)]

df$num_people <-
  sum_row(
    df$num_dom_workers,
    df$num_separate_relatives,
    df$num_tenants,
    df$hh_members_num,
    na.rm = T
  )


df$num_people_per_room <- df$num_people / df$num_bedrooms

df$shelter_structure_issues  <- case_when(sum_row(df[, c(
  "shelter_defects.collapse",
  "shelter_defects.damage_roof",
  "shelter_defects.damage_columns",
  "shelter_defects.damage_walls"
)], na.rm = T) >= 1 ~ 1,
T ~ 0)

df$shelter_wash_issues  <- case_when(sum_row(df[, c(
  "shelter_defects.water_pipes_dysfunctional",
  "shelter_defects.sanitation_pipes_dysfunctional",
  "shelter_defects.latrines_unusable",
  "shelter_defects.bathing_facilities_unusable"
)], na.rm = T) >= 1 ~ 1,
T ~ 0)

df$critical_shelter  <- case_when(
  df$shelter_type %in% c(
    "construction_site",
    "agriculture_engine_pump",
    "factory",
    "farm",
    "school",
    "shop",
    "tent",
    "warehouse",
    "workshop"
  ) ~ 1,
  T ~ 0
)

################################### PWD calculcation #################################
indiv_df <- indiv_df %>%
  mutate(
    seeing_cannot_do_at_all = case_when(indiv_df$wgss_seeing == "cannot_do_at_all" ~ 1, TRUE ~ 0),
    seeing_a_lot_of_difficulty = case_when(indiv_df$wgss_seeing == "a_lot_of_difficulty" ~ 1, TRUE ~ 0),
    seeing_some_difficulty = case_when(indiv_df$wgss_seeing == "some_difficulty" ~ 1, TRUE ~ 0),
    seeing_no_difficulty = case_when(indiv_df$wgss_seeing == "no_difficulty" ~ 1, TRUE ~ 0),
    
    hearing_cannot_do_at_all = case_when(indiv_df$wgss_hearing == "cannot_do_at_all" ~ 1, TRUE ~ 0),
    hearing_a_lot_of_difficulty = case_when(indiv_df$wgss_hearing == "a_lot_of_difficulty" ~ 1, TRUE ~ 0),
    hearing_some_difficulty = case_when(indiv_df$wgss_hearing == "some_difficulty" ~ 1, TRUE ~ 0),
    hearing_no_difficulty = case_when(indiv_df$wgss_hearing == "no_difficulty" ~ 1, TRUE ~ 0),
    
    walking_cannot_do_at_all = case_when(indiv_df$wgss_walking == "cannot_do_at_all" ~ 1, TRUE ~ 0),
    walking_a_lot_of_difficulty = case_when(indiv_df$wgss_walking == "a_lot_of_difficulty" ~ 1, TRUE ~ 0),
    walking_some_difficulty = case_when(indiv_df$wgss_walking == "some_difficulty" ~ 1, TRUE ~ 0),
    walking_no_difficulty = case_when(indiv_df$wgss_walking == "no_difficulty" ~ 1, TRUE ~ 0),
    
    remembering_cannot_do_at_all = case_when(indiv_df$wgss_remembering == "cannot_do_at_all" ~ 1, TRUE ~ 0),
    remembering_a_lot_of_difficulty = case_when(indiv_df$wgss_remembering == "a_lot_of_difficulty" ~ 1,TRUE ~ 0),
    remembering_some_difficulty = case_when(indiv_df$wgss_remembering == "some_difficulty" ~ 1, TRUE ~ 0),
    remembering_no_difficulty = case_when(indiv_df$wgss_remembering == "no_difficulty" ~ 1, TRUE ~ 0),
    
    selfcare_cannot_do_at_all = case_when(indiv_df$wgss_selfcare == "cannot_do_at_all" ~ 1, TRUE ~ 0),
    selfcare_a_lot_of_difficulty = case_when(indiv_df$wgss_selfcare == "a_lot_of_difficulty" ~ 1, TRUE ~ 0),
    selfcare_some_difficulty = case_when(indiv_df$wgss_selfcare == "some_difficulty" ~ 1, TRUE ~ 0),
    selfcare_no_difficulty = case_when(indiv_df$wgss_selfcare == "no_difficulty" ~ 1, TRUE ~ 0),
    
    communicating_cannot_do_at_all = case_when(indiv_df$wgss_communicating == "cannot_do_at_all" ~ 1,TRUE ~ 0),
    communicating_a_lot_of_difficulty = case_when(indiv_df$wgss_communicating == "a_lot_of_difficulty" ~ 1,TRUE ~ 0),
    communicating_some_difficulty = case_when(indiv_df$wgss_communicating == "some_difficulty" ~ 1,TRUE ~ 0),
    communicating_no_difficulty = case_when(indiv_df$wgss_communicating == "no_difficulty" ~ 1, TRUE ~ 0))

summarize_loop <- indiv_df %>% group_by(X_submission__uuid) %>%
  summarize(
    seeing_cannot_do_at_all = sum(seeing_cannot_do_at_all, na.rm = T),
    seeing_a_lot_of_difficulty = sum(seeing_a_lot_of_difficulty, na.rm = T),
    seeing_some_difficulty = sum(seeing_some_difficulty, na.rm = T),
    seeing_no_difficulty = sum(seeing_no_difficulty, na.rm = T),
    
    hearing_cannot_do_at_all = sum(hearing_cannot_do_at_all, na.rm = T),
    hearing_a_lot_of_difficulty = sum(hearing_a_lot_of_difficulty, na.rm = T),
    hearing_some_difficulty = sum(hearing_some_difficulty, na.rm = T),
    hearing_no_difficulty = sum(hearing_no_difficulty, na.rm = T),
    
    walking_cannot_do_at_all = sum(walking_cannot_do_at_all, na.rm = T),
    walking_a_lot_of_difficulty = sum(walking_a_lot_of_difficulty, na.rm = T),
    walking_some_difficulty = sum(walking_some_difficulty, na.rm = T),
    walking_no_difficulty = sum(walking_no_difficulty, na.rm = T),
    
    remembering_cannot_do_at_all = sum(remembering_cannot_do_at_all, na.rm = T),
    remembering_a_lot_of_difficulty = sum(remembering_a_lot_of_difficulty, na.rm = T),
    remembering_some_difficulty = sum(remembering_some_difficulty, na.rm = T),
    remembering_no_difficulty = sum(remembering_no_difficulty, na.rm = T),
    
    selfcare_cannot_do_at_all = sum(selfcare_cannot_do_at_all, na.rm = T),
    selfcare_a_lot_of_difficulty = sum(selfcare_a_lot_of_difficulty, na.rm = T),
    selfcare_some_difficulty = sum(selfcare_some_difficulty, na.rm = T),
    selfcare_no_difficulty = sum(selfcare_no_difficulty, na.rm = T),
    
    communicating_cannot_do_at_all = sum(communicating_cannot_do_at_all, na.rm = T),
    communicating_a_lot_of_difficulty = sum(communicating_a_lot_of_difficulty, na.rm = T),
    communicating_some_difficulty = sum(communicating_some_difficulty, na.rm = T),
    communicating_no_difficulty = sum(communicating_no_difficulty, na.rm = T))

df <-
  left_join(df, summarize_loop, by = c("X_uuid" = "X_submission__uuid"))

###########################################################################################

df$critical_shelter_with_disabled  <- case_when(
  df$critical_shelter == 1 &
    (
      df$walking_cannot_do_at_all >= 1 |
        df$walking_a_lot_of_difficulty >= 1
    ) ~ 1,
  T ~ 0
)


df$hlp_difficulty_rented  <- case_when(
  df$meet_shelter_needs %in% c("access", "financial", "both") & df$occupancy_arrangement == "rented" ~ 1,
  T ~ 0
)

df$hlp_issues_all <- case_when(
  df$hlp_issues.ownership_dispute == 1 |
    df$hlp_issues.inheritance_dispute == 1 |
    df$hlp_issues.tenant_dispute == 1 |
    df$hlp_issues.unlawful_occupation == 1 |
    df$hlp_issues.mortgage_dispute == 1 |
    df$hlp_issues.property_pledge == 1 |
    df$hlp_issues.seizure_process == 1 |
    df$hlp_issues.dispute_use_cultural_building == 1 |
    df$hlp_issues.eviction_harassment_landlord == 1 |
    df$hlp_issues.lack_loss_housing_land == 1 |
    df$hlp_issues.looting_property == 1 ~ 1,
  T ~ 0
)

df$hlp_issues_assistance  <- case_when(
  df$tenure_arrangement == "assistance" &
    df$hlp_issues_all == 1 ~ 1,
  T ~ 0
)
df$hlp_issues_employer_provided  <- case_when(
  df$tenure_arrangement == "employer_provided" &
    df$hlp_issues_all == 1 ~ 1,
  T ~ 0
)
df$hlp_issues_hosted_free  <- case_when(
  df$tenure_arrangement == "hosted_free" &
    df$hlp_issues_all == 1 ~ 1,
  T ~ 0
)
df$hlp_issues_informal_verbal  <- case_when(
  df$tenure_arrangement == "informal_verbal" &
    df$hlp_issues_all == 1 ~ 1,
  T ~ 0
)
df$hlp_issues_rent_agreement_after_92  <- case_when(
  df$tenure_arrangement == "rent_agreement_after_92" &
    df$hlp_issues_all == 1 ~ 1,
  T ~ 0
)
df$hlp_issues_rented_before_1992  <- case_when(
  df$tenure_arrangement == "rent_agreement_before_92" &
    df$hlp_issues_all == 1 ~ 1,
  T ~ 0
)
df$hlp_issues_rent_freeze_reduction  <- case_when(
  df$tenure_arrangement == "rent_freeze_reduction" &
    df$hlp_issues_all == 1 ~ 1,
  T ~ 0
)
df$hlp_issues_squatting_no_permission  <- case_when(
  df$tenure_arrangement == "squatting_no_permission" &
    df$hlp_issues_all == 1 ~ 1,
  T ~ 0
)

df$ci9  <- case_when(
  df$meet_education_needs == "yes" & df$occupancy_arrangement == "rented" ~ 1,
  T ~ 0
)

indiv_df$dropped_out_protection <- case_when(indiv_df$drop_out == "yes" &
                                    (indiv_df$drop_out_reason.child_marriage == 1 |
                                       indiv_df$drop_out_reason.protection_at_school == 1 |
                                       indiv_df$drop_out_reason.protection_commuting == 1) ~ 1,
                                    indiv_df$drop_out == "yes" ~ 0,
                                    T ~ NA_real_
                                  )

temp <- indiv_df %>% group_by(X_submission__uuid) %>%
  summarize(dropped_out_protection = sum(dropped_out_protection, na.rm = T),
            dropped_out = sum(drop_out == "yes", na.rm = T),
            school_aged_children = sum(age_yrs_ind %in% c(4:17))) %>%
  mutate(
    education_dropped_protection_concerns = case_when(dropped_out == 0 | school_aged_children == 0 ~ NA_real_,
                                                      dropped_out_protection >= 1 ~ 1,
                                                      T ~ 0)
  ) %>% dplyr::select(X_submission__uuid, education_dropped_protection_concerns, school_aged_children)

df <- left_join(df, temp, by = c("X_uuid" = "X_submission__uuid"))

mean_exps <- df %>% group_by(strata) %>%
  summarize(avg_food_expenses = mean(per_month_food, na.rm = T),
            avg_rent_expenses = mean(per_month_rent, na.rm = T),
            avg_health_expenses = mean(per_month_health, na.rm = T),
            avg_water_expenses = mean(per_month_water, na.rm = T))

df <- left_join(df, mean_exps, by="strata")

df$education_difficulty_food_expenses <- case_when(df$school_aged_children == 0 ~ NA_real_,
                                                   df$meet_education_needs %in% c("access", "financial", "both") &
                                                     df$per_month_food > df$avg_food_expenses ~ 1,
                                                   T ~ 0)

df$education_difficulty_rent_expenses <- case_when(df$school_aged_children == 0 ~ NA_real_,
                                                   df$meet_education_needs %in% c("access", "financial", "both") &
                                                     df$per_month_rent > df$avg_rent_expenses ~ 1,
                                                   T ~ 0)

df$education_difficulty_health_expenses <- case_when(df$school_aged_children == 0 ~ NA_real_,
                                                   df$meet_education_needs %in% c("access", "financial", "both") &
                                                     df$per_month_health > df$avg_health_expenses ~ 1,
                                                   T ~ 0)

df$education_difficulty_water_expenses <- case_when(df$school_aged_children == 0 ~ NA_real_,
                                                   df$meet_education_needs %in% c("access", "financial", "both") &
                                                     df$per_month_water > df$avg_water_expenses ~ 1,
                                                   T ~ 0)

df$health_barrier_cost_related <- case_when(df$health_barriers_experienced_unmet_need.cost_consultation == 1 |
                                              df$health_barriers_experienced_unmet_need.cost_transportation == 1 |
                                               df$health_barriers_experienced_unmet_need.cost_treatment == 1 ~ 1,
                                            is.na(df$health_barriers_experienced_unmet_need) ~ NA_real_,
                                            T ~ 0)


df$health_barrier_protection_issues <- case_when(df$health_barriers_experienced_unmet_need.disability == 1 |
                                                   df$health_barriers_experienced_unmet_need.distrust == 1 |
                                                   df$health_barriers_experienced_unmet_need.employer_prevented == 1 |
                                                   df$health_barriers_experienced_unmet_need.insecurity_at_facility == 1 |
                                                   df$health_barriers_experienced_unmet_need.insecurity_traveling == 1 |
                                                   df$health_barriers_experienced_unmet_need.lack_docs == 1 |
                                                   df$health_barriers_experienced_unmet_need.no_female_staff == 1 ~ 1,
                                                 is.na(df$health_barriers_experienced_unmet_need) ~ NA_real_,
                                                 T ~ 0)

df$health_medicine_barrier_cost_related <- case_when(df$medicine_barriers.too_expensive == 1 |
                                               df$medicine_barriers.not_afford_visit == 1 |
                                               df$medicine_barriers.insurance_not_honored == 1 ~ 1,
                                             is.na(df$medicine_barriers) ~ NA_real_,
                                             T ~ 0)

df$health_vaccine_barrier_protection_issues <- case_when(df$vaccine_barriers.disability == 1 |
                                                           df$vaccine_barriers.distrust == 1 |
                                                           df$vaccine_barriers.employer_prevented == 1 |
                                                           df$vaccine_barriers.insecurity_at_site == 1 |
                                                           df$vaccine_barriers.insecurity_traveling == 1 |
                                                           df$vaccine_barriers.lack_docs == 1 ~ 1,
                                                         is.na(df$vaccine_barriers) ~ NA_real_,
                                                         T ~ 0)

df$wash_adequate_sanitation <- case_when(df$gender_segregated == "yes" &
                                           df$adequate_lighting == "yes" & 
                                           df$locked_inside == "yes" &
                                           df$safe_route == "yes" ~ 1,
                                         df$shared_sanitation_facility != "yes" ~ NA_real_,
                                         T ~ 0)

df$wash_severe_coping_strategies <- case_when(df$water_coping.reduce_drinking == 1 |
                                              df$water_coping.modify_hygiene == 1 |
                                                df$water_coping.drink_water_domestic_purposes == 1 ~ 1,
                                              is.na(df$water_coping) ~ NA_real_,
                                              T ~ 0)


df$wash_sufficient_improved_water <- case_when(df$primary_drinking %in% c("bottled", "piped_public_tap", "piped_water", "protected_rainwater",
                                                                          "protected_spring", "protected_well", "water_trucking") &
                                                 df$sufficient_water_cooking == "yes" &
                                                 df$sufficient_water_drinking == "yes" &
                                                 df$sufficient_water_hygiene_domestic == "yes" &
                                                 df$sufficient_water_hygiene_personal == "yes" ~ 1,
                                               T ~ 0)

df$protection_lack_work_permit <- case_when(df$possess_id == "yes" &
                                              df$work_permit == "yes" ~ 1,
                                            T ~ 0)

df$no_food_score <- case_when(df$hhs_no_food == "yes" ~ 1, TRUE ~ 0)
df$no_food_score <-
  df$no_food_score * case_when(
    df$hhs_no_food_freq == "often" ~ 2,
    df$hhs_no_food_freq %in% c("rarely", "sometimes") ~ 1,
    TRUE ~ 0
  )

df$not_eating_score <-
  case_when(df$hhs_no_eat_day == "yes" ~ 1, TRUE ~ 0)
df$not_eating_score <-
  df$not_eating_score * case_when(
    df$hhs_no_eat_day_freq == "often" ~ 2,
    df$hhs_no_eat_day_freq %in% c("rarely", "sometimes") ~ 1,
    TRUE ~ 0
  )

df$hungry_score <- case_when(df$hhs_sleep_hungry == "yes" ~ 1, TRUE ~ 0)
df$hungry_score <-
  df$hungry_score * case_when(df$hhs_sleep_hungry_freq == "often" ~ 2,
                              df$hhs_sleep_hungry_freq %in% c("rarely", "sometimes") ~ 1,
                              TRUE ~ 0)

df$household_hunger_scale <-
  df$no_food_score + df$not_eating_score + df$hungry_score


df$hunger_category <-
  case_when(
    df$household_hunger_scale <= 1 ~ "Little to no hunger in the household (0-1)",
    df$household_hunger_scale <= 3 ~ "Moderate hunger in the household (2-3)",
    df$household_hunger_scale <= 6 ~ "Severe hunger in the household (4-6)"
  )

df$fcs <-
  (df$cereals * 2) + (df$nuts_seed * 3) + df$vegetables + df$fruits + (df$meat * 4) + (df$milk_dairy * 4) + (df$sweets * 0.5) + (df$oil_fats * 0.5)

df$fcs_category <- case_when(df$fcs <= 28.5 ~ "Poor",
                             df$fcs < 42.5 ~ "Borderline",
                             T ~ "Acceptable")



