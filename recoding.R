
#Fixed
df$hh1 <- case_when(
  df$shelter_type == "none" ~ 5,
  df$shelter_defects.damage_columns == 1 |
    df$shelter_defects.damage_roof == 1 |
    df$shelter_defects.collapse == 1 ~ 4,
  (
    df$shelter_type %in% c("apartment_house_room", "concierge_room", "hotel_room") &
      sum_row(df[, c(
        "shelter_defects.windows_doors_unsealed",
        "shelter_defects.sanitation_pipes_dysfunctional",
        "shelter_defects.water_pipes_dysfunctional",
        "shelter_defects.latrines_unusable",
        "shelter_defects.electricity_unsafe",
        "shelter_defects.leakage_rot",
        "shelter_defects.roof_leaking",
        "shelter_defects.damage_walls"
      )]) >= 3
  ) |
    (df$shelter_type %in% c(
      "construction_site",
      "agriculture_engine_pump",
      "factory",
      "farm",
      "garage",
      "school",
      "prefab",
      "workshop",
      "warehouse",
      "shop",
      "tent"
    ) &
      sum_row(df[, c(
        "shelter_defects.windows_doors_unsealed",
        "shelter_defects.sanitation_pipes_dysfunctional",
        "shelter_defects.water_pipes_dysfunctional",
        "shelter_defects.latrines_unusable",
        "shelter_defects.electricity_unsafe",
        "shelter_defects.leakage_rot",
        "shelter_defects.roof_leaking",
        "shelter_defects.damage_walls"
      )]) >= 1
    ) ~ 3,
  (
    df$shelter_type %in% c("apartment_house_room", "concierge_room", "hotel_room") &
      sum_row(df[, c(
        "shelter_defects.windows_doors_unsealed",
        "shelter_defects.sanitation_pipes_dysfunctional",
        "shelter_defects.water_pipes_dysfunctional",
        "shelter_defects.latrines_unusable",
        "shelter_defects.electricity_unsafe",
        "shelter_defects.leakage_rot",
        "shelter_defects.roof_leaking",
        "shelter_defects.damage_walls"
      )]) %in% c(1, 2)
  ) |
    (df$shelter_type %in% c(
      "construction_site",
      "agriculture_engine_pump",
      "factory",
      "farm",
      "garage",
      "school",
      "prefab",
      "workshop",
      "warehouse",
      "shop",
      "tent"
    ) &
      sum_row(df[, c(
        "shelter_defects.windows_doors_unsealed",
        "shelter_defects.sanitation_pipes_dysfunctional",
        "shelter_defects.water_pipes_dysfunctional",
        "shelter_defects.latrines_unusable",
        "shelter_defects.electricity_unsafe",
        "shelter_defects.leakage_rot",
        "shelter_defects.roof_leaking",
        "shelter_defects.damage_walls"
      )]) == 0
    ) ~ 2,
  T ~ 1
)

df$hh2 <-
  case_when(
    df$occupancy_arrangement == "no_agreement" |
      df$tenure_arrangement == "squatting_no_permission" |
      df$hlp_issues.seizure_process == 1 ~ 4,
    df$occupancy_arrangement == "hosted_no_rent" &
      df$tenure_arrangement %in% c("employer_provided", "informal_verbal") &
      df$hlp_issues.eviction_harassment_landlord == 1 ~ 3,
    df$occupancy_arrangement == "hosted_no_rent" &
      df$tenure_arrangement %in% c("employer_provided", "informal_verbal") ~ 2,
    T ~ 1
  )

df$hh3 <- case_when(df$sufficient_water_drinking == "no" ~ 5,
                    df$sufficient_water_cooking == "no" &
                      df$sufficient_water_hygiene_personal == "no" ~ 4,
                    df$sufficient_water_cooking == "no" |
                      df$sufficient_water_hygiene_personal == "no" ~ 3,
                    df$sufficient_water_hygiene_domestic == "no" ~ 2,
                    T ~ 1
)

df$hh4 <- case_when(df$sanitation_facility == "none" ~ 5,
                    df$wastewater_drain == "hole" |
                      df$wastewater_drain == "open_area"|
                      df$sanitation_facility %in% c(
                        "latrine_slab",
                        "open_hole",
                        "bucket",
                        "plastic_bag",
                        "hanging"
                      ) ~ 4,
                    df$sanitation_facility %in% c(
                      "latrine_no_slab",
                      "pit_vip",
                      "flush"
                    ) &
                      df$shared_sanitation_facility == "yes" ~ 3,
                    T ~ 1
)

df$hh5 <- case_when(df$presence_soap == "no" ~ 3,
                    T ~ 1
)

df$hh6 <- case_when(df$hygiene_coping.reduce_nfi_consumption_hygiene == 1 |
                      df$hygiene_coping.reduce_nfi_consumption_other == 1 ~ 3,
                    T ~ 1
)

################################### PWD calculcation #################################

indiv_df <- indiv_df %>%
  mutate(
    seeing_cannot = case_when(indiv_df$wgss_seeing == "cannot" ~ 1, TRUE ~ 0),
    seeing_a_lot = case_when(indiv_df$wgss_seeing == "a_lot" ~ 1, TRUE ~ 0),
    seeing_some = case_when(indiv_df$wgss_seeing == "some" ~ 1, TRUE ~ 0),
    
    hearing_cannot = case_when(indiv_df$wgss_hearing == "cannot" ~ 1, TRUE ~ 0),
    hearing_a_lot = case_when(indiv_df$wgss_hearing == "a_lot" ~ 1, TRUE ~ 0),
    hearing_some = case_when(indiv_df$wgss_hearing == "some" ~ 1, TRUE ~ 0),
    
    walking_cannot = case_when(indiv_df$wgss_walking == "cannot" ~ 1, TRUE ~ 0),
    walking_a_lot = case_when(indiv_df$wgss_walking == "a_lot" ~ 1, TRUE ~ 0),
    walking_some = case_when(indiv_df$wgss_walking == "some" ~ 1, TRUE ~ 0),
    
    remembering_cannot = case_when(indiv_df$wgss_remembering == "cannot" ~ 1, TRUE ~ 0),
    remembering_a_lot = case_when(indiv_df$wgss_remembering == "a_lot" ~ 1,TRUE ~ 0),
    remembering_some = case_when(indiv_df$wgss_remembering == "some" ~ 1, TRUE ~ 0),
    
    selfcare_cannot = case_when(indiv_df$wgss_selfcare == "cannot" ~ 1, TRUE ~ 0),
    selfcare_a_lot = case_when(indiv_df$wgss_selfcare == "a_lot" ~ 1, TRUE ~ 0),
    selfcare_some = case_when(indiv_df$wgss_selfcare == "some" ~ 1, TRUE ~ 0),
    
    communicating_cannot = case_when(indiv_df$wgss_communicating == "cannot" ~ 1,TRUE ~ 0),
    communicating_a_lot = case_when(indiv_df$wgss_communicating == "a_lot" ~ 1,TRUE ~ 0),
    communicating_some = case_when(indiv_df$wgss_communicating == "some" ~ 1,TRUE ~ 0))

summarize_loop <- indiv_df %>% group_by(X_submission__uuid) %>%
  summarize(
    seeing_cannot = sum(seeing_cannot, na.rm = T),
    seeing_a_lot = sum(seeing_a_lot, na.rm = T),
    seeing_some = sum(seeing_some, na.rm = T),
    
    hearing_cannot = sum(hearing_cannot, na.rm = T),
    hearing_a_lot = sum(hearing_a_lot, na.rm = T),
    hearing_some = sum(hearing_some, na.rm = T),
    
    walking_cannot = sum(walking_cannot, na.rm = T),
    walking_a_lot = sum(walking_a_lot, na.rm = T),
    walking_some = sum(walking_some, na.rm = T),
    
    remembering_cannot = sum(remembering_cannot, na.rm = T),
    remembering_a_lot = sum(remembering_a_lot, na.rm = T),
    remembering_some = sum(remembering_some, na.rm = T),
    
    selfcare_cannot = sum(selfcare_cannot, na.rm = T),
    selfcare_a_lot = sum(selfcare_a_lot, na.rm = T),
    selfcare_some = sum(selfcare_some, na.rm = T),
    
    communicating_cannot = sum(communicating_cannot, na.rm = T),
    communicating_a_lot = sum(communicating_a_lot, na.rm = T),
    communicating_some = sum(communicating_some, na.rm = T))

df <-
  left_join(df, summarize_loop, by = c("X_uuid" = "X_submission__uuid"))

df <- df %>%
  mutate(
    cannot = sum_row(df[, endsWith(names(df), "cannot")], na.rm = T),
    a_lot = sum_row(df[, endsWith(names(df), "a_lot")], na.rm = T),
    some = sum_row(df[, endsWith(names(df), "some")], na.rm = T)
  )

df$hh7 <- case_when(
  df$cannot >= 4 ~ 5,
  df$cannot > 0 | df$a_lot >= 4 ~ 4,
  df$a_lot > 0 |
       df$some >= 4 ~ 3,
    df$some > 0 ~ 2,
  T ~ 1
)
###########################################################################

df$hh8 <- case_when(
  sum_row(df[, c(
    "unsafe_locations.markets",
    "unsafe_locations.social_community_areas",
    "unsafe_locations.route_school",
    "unsafe_locations.route_community_health",
    "unsafe_locations.route_home_religious",
    "unsafe_locations.in_home",
    "unsafe_locations.public_transport",
    "unsafe_locations.other",
    "unsafe_locations.on_street_neighborhood"
  )]) >= 4 ~ 5,
  sum_row(df[, c(
    "unsafe_locations.markets",
    "unsafe_locations.social_community_areas",
    "unsafe_locations.route_school",
    "unsafe_locations.route_community_health",
    "unsafe_locations.route_home_religious",
    "unsafe_locations.in_home",
    "unsafe_locations.public_transport",
    "unsafe_locations.other",
    "unsafe_locations.on_street_neighborhood"
  )]) == 3 ~ 4,
  sum_row(df[, c(
    "unsafe_locations.markets",
    "unsafe_locations.social_community_areas",
    "unsafe_locations.route_school",
    "unsafe_locations.route_community_health",
    "unsafe_locations.route_home_religious",
    "unsafe_locations.in_home",
    "unsafe_locations.public_transport",
    "unsafe_locations.other",
    "unsafe_locations.on_street_neighborhood"
  )]) == 2 ~ 3,
  sum_row(df[, c(
    "unsafe_locations.markets",
    "unsafe_locations.social_community_areas",
    "unsafe_locations.route_school",
    "unsafe_locations.route_community_health",
    "unsafe_locations.route_home_religious",
    "unsafe_locations.in_home",
    "unsafe_locations.public_transport",
    "unsafe_locations.other",
    "unsafe_locations.on_street_neighborhood"
  )]) == 1 ~ 2,
  T ~ 1
)

df$hh9 <- case_when(df$possess_id == "dont_have" ~ 3,
                    df$possess_id == "dont_have_possession" ~ 2,
                    T ~ 1
)

#Fixec
df$hh10 <- case_when(
  df$aid_barriers.affiliation == 1 |
    df$aid_barriers.nationality == 1 |
    df$aid_barriers.lack_docs == 1 |
  df$aid_barriers.reside_inaccessible_area == 1 |
    df$aid_barriers.reside_area_no_providers == 1 ~ 3,
  df$aid_barriers.not_understand_procedures == 1 |
    df$aid_barriers.not_understand_how_apply == 1 ~ 2,
  T ~ 1
)


#################################### Area Indicators ##############################

indiv_df$children_dropped <-
  case_when(indiv_df$age_yrs_ind %in% 4:14 &
              indiv_df$drop_out == "yes" ~ 1,
            T ~ 0)

indiv_df$children_worked <-
  case_when(indiv_df$age_yrs_ind %in% 4:14 &
              indiv_df$work_disrupts_school == "yes" ~ 1,
            T ~ 0)

indiv_df$children <-
  case_when(indiv_df$age_yrs_ind %in% 4:14 ~ 1, T ~ 0)

summarize_indiv <- indiv_df %>% group_by(X_submission__uuid) %>%
  summarise(
    n_children = sum(children, na.rm = T),
    n_children_dropped = sum(children_dropped, na.rm = T),
    n_children_worked = sum(children_worked, na.rm = T)
  )


df <-
  left_join(df, summarize_indiv, by = c("X_uuid" = "X_submission__uuid"))

df$children_dropped <- case_when(df$n_children == 0 ~ NA_real_,
                                 df$n_children_dropped > 0 ~ 1,
                                 T ~ 0)

df$children_worked <- case_when(df$n_children == 0 ~ NA_real_,
                                df$n_children_worked > 0 ~ 1,
                                T ~ 0)

df$weight_children <- case_when(df$n_children == 0 ~ NA_real_,
                                T ~ df$weight)

df$access_distance <- case_when(df$health_facility_distance <= 60 ~ 1,
                     T ~ 0
)

df$access_healthcare_issue <- case_when(df$health_no_access_num > 0 ~ 1, T ~ 0)

area_indicators <- df %>% group_by(governorate, district) %>%
  summarize(
    perc_children_dropped = sum(children_dropped * weight_children, na.rm = T) / sum(weight_children, na.rm = T),
    perc_children_worked = sum(children_worked * weight_children, na.rm = T) / sum(weight_children, na.rm = T),
    perc_access_healthcare_issue = sum(access_healthcare_issue * weight, na.rm = T) / sum(weight, na.rm = T),
    perc_access_distance = sum(access_distance * weight, na.rm = T)/sum(weight,na.rm = T)
  )

area_indicators$children_dropped <-
  case_when(
    area_indicators$perc_children_dropped >= 0.25 ~ 5,
    area_indicators$perc_children_dropped >= 0.15 ~ 4,
    area_indicators$perc_children_dropped >= 0.5 ~ 3,
    area_indicators$perc_children_dropped > 0 ~ 2,
    T ~ 1
  )

area_indicators$children_worked <-
  case_when(
    area_indicators$perc_children_worked >= 0.40 ~ 5,
    area_indicators$perc_children_worked >= 0.15 ~ 4,
    area_indicators$perc_children_worked > 0 ~ 3,
    T ~ 1
  )

area_indicators$access_distance <-
  case_when(
    area_indicators$perc_access_distance < 0.7 ~ 5,
    area_indicators$perc_access_distance < 0.8 ~ 4,
    area_indicators$perc_access_distance < 0.9 ~ 3,
    area_indicators$perc_access_distance <= 0.95 ~ 2,
    T ~ 1
  )

area_indicators$access_healthcare_issue <-
  case_when(
    area_indicators$perc_access_healthcare_issue >= 0.35 ~ 4,
    area_indicators$perc_access_healthcare_issue >= 0.2 ~ 3,
    T ~ 1
  )

area_indicators$admin1Name <- area_indicators$governorate
area_indicators$admin1Pcod <- ocha_boundaries$admin1Pcod[match(area_indicators$admin1Name, ocha_boundaries$admin1Name)]
area_indicators$admin2Name <- area_indicators$district
area_indicators$admin2Pcod <- ocha_boundaries$admin2Pcod[match(area_indicators$admin2Name, ocha_boundaries$admin2Name)]

area_indicators <-
  area_indicators[order(area_indicators$admin1Name, area_indicators$admin2Name), c(
    "admin1Name",
    "admin1Pcod",
    "admin2Name",
    "admin2Pcod",
    "children_dropped",
    "children_worked",
    "access_distance",
    "access_healthcare_issue"
  )]

nutrition_indicators <- read_excel("input/dataset/SMART PiN Calculation District Level (Nutrition Indicators).xlsx")

area_indicators <- left_join(area_indicators, nutrition_indicators, by=c("admin2Name" = "District"))

#Fixed
area_indicators$MDD <- case_when(
  area_indicators$MDD <= 0.1 ~ 5,
  area_indicators$MDD <= 0.2 ~ 4,
  area_indicators$MDD <= 0.4 ~ 3,
  area_indicators$MDD < 0.7 ~ 2,
  T ~ 1
)

#Fixed
area_indicators$MAD <- case_when(
  area_indicators$MAD <= 0.1 ~ 5,
  area_indicators$MAD <= 0.2 ~ 4,
  area_indicators$MAD <= 0.4 ~ 3,
  area_indicators$MAD < 0.7 ~ 2,
  T ~ 1
)

area_indicators$Anaemia <- case_when(
  area_indicators$Anaemia >= 0.4 ~ 4,
  area_indicators$Anaemia >= 0.2 ~ 3,
  area_indicators$Anaemia >= 0.05 ~ 2,
  T ~ 1
)

health_indicators <- read_excel("input/dataset/Health Sector MSNA_LBN.xlsx")

area_indicators <- left_join(area_indicators, health_indicators, by=c("admin1Name"="Governorate"))

food_security_indicators <- read_excel("input/dataset/food_security_data.xlsx")

food_security_indicators <- food_security_indicators[,c("District", "FCS", "coping_strategy", "reduced_strategy_index")]

area_indicators <- left_join(area_indicators, food_security_indicators, by=c("admin2Name"="District"))







