rm(list = ls())

library(hypegrammaR)
library(dplyr)
library(surveyweights)
library(readxl)
library(srvyr)
library(expss)
library(readxl)
library(lubridate)
library(doParallel)
library(foreach)
library(stringr)
library(openxlsx)
library(sf)

source("analysis_function.R")

disaggregation <- "population_group"
aggregation <- "all"

# read data from excel file
df <-
  read_excel("input/dataset/MSNA_data_clean_2021-12-22.xlsx", sheet = 2, guess_max = 50000)

# import loop data
indiv_df <-
  read_excel("input/dataset/MSNA_data_clean_2021-12-22.xlsx", sheet = 3, guess_max = 50000) 

questions <- read_excel("input/questionnaire/LBN_MSNA_SurveyKobo_V5_TTC_translated.xlsx", sheet = 1)
choices <- read_excel("input/questionnaire/LBN_MSNA_SurveyKobo_V5_TTC_translated.xlsx", sheet = 2)

questions$type <- tolower(questions$type)
questions$name <- tolower(questions$name)

choices$list_name <- tolower(choices$list_name)
choices$name <- tolower(choices$name)

df$region <- case_when(df$governorate %in% c("Baalbek-El Hermel", "Bekaa") ~ "Baalbek-El Hermel and Bekaa",
                       df$governorate %in% c("Beirut", "Mount Lebanon") ~ "BML",
                       df$governorate %in% c("North", "Akkar") ~ "North and Akkar",
                       df$governorate %in% c("South", "El Nabatieh") ~ "South and Nabatieh")

df$area1 <- case_when(df$population_group == "LEB" ~ df$district, T ~ df$region)
df$area2 <- case_when(df$population_group == "LEB" ~ df$governorate, T ~ df$region)

df$all <- "all"

df$gender_head <- case_when(df$head_hh_gender == "2_female_coheaded" ~ "female_headed",
                            df$head_hh_gender == "2_male_coheaded" ~ "male_headed",
                            T ~ df$head_hh_gender)

weight_fun <- function(df){
  as.numeric(df$weight)
}


#creating the dap from the questionnaire
dap <- questions[,c("type","name","label::English")]
dap$question_type <- gsub("[ ].*", "", dap$type)
dap$choice_list_name <- gsub(".*[ ]", "", dap$type)
dap <- dap[dap$question_type %in% c("select_one", "integer", "select_multiple"),]

dap$hypothesis.type <- "direct_reporting"
dap$dependent.variable <- dap$name
dap$dependent.variable.type <-
  case_when(
    dap$question_type == "integer" &
      !dap$name %in% c(
        "cereals",
        "nuts_seed",
        "milk_dairy",
        "meat",
        "vegetables",
        "fruits",
        "oil_fats",
        "sweets",
        "spices_condiments"
      ) ~ "numerical",
    T ~ "categorical"
  )

dap$independent.variable <- "all"
dap$independent.variable.type <- "categorical"
dap$repeat.for.variable <- "all"

indiv_df <- indiv_df %>% 
  mutate(
    region = df$region[match(X_submission__uuid, df$X_uuid)],
    district = df$district[match(X_submission__uuid, df$X_uuid)],
    governorate = df$governorate[match(X_submission__uuid, df$X_uuid)],
    strata = df$strata[match(X_submission__uuid, df$X_uuid)],
    population_group = df$population_group[match(X_submission__uuid, df$X_uuid)],
    area1 = df$area1[match(X_submission__uuid, df$X_uuid)],
    area2 = df$area2[match(X_submission__uuid, df$X_uuid)],
    gender_head = df$gender_head[match(X_submission__uuid, df$X_uuid)],
    age_group = case_when(age_yrs_ind < 18 ~ "0_17",
                          age_yrs_ind < 60 ~ "18_59",
                          age_yrs_ind >= 60 ~ "60+")
  )

indiv_df$all <- "all"

weight_fun_indiv <- function(indiv_df){
  as.numeric(indiv_df$weight)
}


overall_analysis <- output_and_format(df, dap, weight_fun, questions, choices, "all", "all")
per_group_analysis <- output_and_format(df, dap, weight_fun, questions, choices, "all", "population_group")
per_gender_head_analysis <- output_and_format(df, dap, weight_fun, questions, choices, "all", "gender_head")
per_district_analysis <- output_and_format(df, dap, weight_fun, questions, choices, "area1", "population_group")
per_district_analysis$governorate <- df$governorate[match(per_district_analysis$repeat.var.value, df$district)]
per_governorate_analysis <- output_and_format(df, dap, weight_fun, questions, choices, "area2", "population_group")
overall_analysis_indiv <- output_and_format(indiv_df, dap, weight_fun, questions, choices, "all", "all")
per_age_group_analysis_indiv <- output_and_format(indiv_df, dap, weight_fun, questions, choices, "all", "age_group")
per_group_analysis_indiv <- output_and_format(indiv_df, dap, weight_fun, questions, choices, "all", "population_group")
per_gender_head_analysis_indiv <- output_and_format(indiv_df, dap, weight_fun, questions, choices, "all", "gender_head")
per_district_analysis_indiv <- output_and_format(indiv_df, dap, weight_fun, questions, choices, "area1", "population_group")
per_district_analysis_indiv$governorate <- df$governorate[match(per_district_analysis_indiv$repeat.var.value, df$district)]
per_governorate_analysis_indiv <- output_and_format(indiv_df, dap, weight_fun, questions, choices, "area2", "population_group")

write_list <- list(
  "overall_analysis" = overall_analysis,
  "per_group_analysis" = per_group_analysis,
  "per_gender_head_analysis" = per_gender_head_analysis,
  "per_district_analysis" = per_district_analysis,
  "per_governorate_analysis" = per_governorate_analysis,
  "overall_analysis_indiv" = overall_analysis_indiv,
  "per_age_group_analysis_indiv" = per_age_group_analysis_indiv,
  "per_group_analysis_indiv" = per_group_analysis_indiv,
  "per_gender_head_analysis_indiv" = per_gender_head_analysis_indiv,
  "per_district_analysis_indiv" = per_district_analysis_indiv,
  "per_governorate_analysis_indiv" = per_governorate_analysis_indiv
)

illuminate::write_excel_as_reach_format(write_list,
                                        paste0("output/",str_replace_all(Sys.Date(),"-","_"),"_analysis_all_questions.xlsx"))

