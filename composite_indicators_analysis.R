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

source("composite_indicators_recoding.R")
dap <- read.csv("input/dap/composite_indicators_dap.csv")

output_and_format <-
  function(df, dap, aggregation, disaggregation) {
    weight_fun <- function(df) {
      as.numeric(df$weight)
    }
    
    
    dap$independent.variable <- disaggregation
    dap$repeat.for.variable <- aggregation
    
    result <- from_analysisplan_map_to_output(
      data = df,
      analysisplan = dap,
      weighting = weight_fun,
      confidence_level = 0.9
    )
    
    summary <-   bind_rows(lapply(result[[1]], function(x) {
      x$summary.statistic
    }))
    
    summary <- correct.zeroes(summary)
    
    summary <-
      left_join(summary, dap, by = c("dependent.var" = "dependent.variable"))
    
    summary <-
      summary[summary$dependent.var.value != 0 |
                is.na(summary$dependent.var.value), c(
                  "ï..Sector",
                  "Sub.group.of.question",
                  "research.question",
                  "sub.research.question",
                  "repeat.var.value",
                  "independent.var.value",
                  "dependent.var.value",
                  "numbers",
                  "min",
                  "max",
                  "dependent.var"
                )]
  }


overall_analysis <- output_and_format(df, dap, "all", "all")
per_group_analysis <- output_and_format(df, dap, "all", "population_group")
per_gender_head_analysis <- output_and_format(df, dap, "all", "gender_head")
per_nationality_analysis <- output_and_format(df, dap, "all", "nationality_hh")
per_district_analysis <- output_and_format(df, dap, "area1", "population_group")
per_district_analysis$governorate <- df$governorate[match(per_district_analysis$repeat.var.value, df$district)]
per_governorate_analysis <- output_and_format(df, dap, "area2", "population_group")
per_region_analysis <- output_and_format(df, dap, "region", "population_group")


write_list <- list(
  "overall_analysis" = overall_analysis,
  "per_group_analysis" = per_group_analysis,
  "per_gender_head_analysis" = per_gender_head_analysis,
  "per_nationality_analysis" = per_nationality_analysis,
  "per_district_analysis" = per_district_analysis,
  "per_governorate_analysis" = per_governorate_analysis,
  "per_region_analysis" = per_region_analysis
)

illuminate::write_excel_as_reach_format(write_list,
                                        paste0("output/",str_replace_all(Sys.Date(),"-","_"),"_analysis_composite_indicators.xlsx"))


















