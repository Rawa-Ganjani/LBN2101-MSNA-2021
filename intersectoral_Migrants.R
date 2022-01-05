rm(list = ls(all = T))
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
library(sf)

# read data from excel file
df <-
  read_excel("input/dataset/MSNA_data_clean_2021-12-30.xlsx", sheet = 2, guess_max = 50000) %>%
  filter(population_group == "MIG")

# import loop data
indiv_df <-
  read_excel("input/dataset/MSNA_data_clean_2021-12-30.xlsx", sheet = 3, guess_max = 50000) 

ocha_boundaries <- st_read("input/spatial_data/LBN_Admin_level2.geojson")

sampling_frame <- read.csv("input/sample/sampling_frame.csv")

weight_fun <- function(df){
  df$weight
}

source("recoding.R")


dap <- read.csv("input/dap/pin_dap.csv")
dap <- dap %>% rbind(read.csv("input/dap/dap_extra.csv"))

df$region <- case_when(df$governorate %in% c("Baalbek-El Hermel", "Bekaa") ~ "Baalbek-El Hermel and Bekaa",
                       df$governorate %in% c("Beirut", "Mount Lebanon") ~ "BML",
                       df$governorate %in% c("North", "Akkar") ~ "North and Akkar",
                       df$governorate %in% c("South", "El Nabatieh") ~ "South and Nabatieh")

#calculating the new indicator for Migration group
df$hh11 <-
  case_when(sum_row(df[, c(
    "security_concerns_boys.corporal_punishment",
    "security_concerns_boys.being_threatened_with_violence",
    "security_concerns_boys.being_kidnapped",
    "security_concerns_boys.suffering_from_physical_harassment_or_violence_not_sexual",
    "security_concerns_boys.suffering_from_verbal_harassment",
    "security_concerns_boys.suffering_from_verbal_harassment",
    "security_concerns_boys.discrimination_or_persecution_because_of_ethnicity_status__etc",
    "security_concerns_boys.being_killed",
    "security_concerns_boys.being_detained",
    "security_concerns_boys.being_exploited_i_e_being_engaged_in_harmful_forms_of_labor_for_economic_gain_of_the_exploiter",
    "security_concerns_boys.being_sexually_exploited_in_exchange_of_humanitarian_aid_goods_services_money_or_preference_treatment",
    "security_concerns_boys.being_recruited_by_armed_groups",
    "security_concerns_boys.being_sent_abroad_to_find_work",
    "security_concerns_girls.corporal_punishment",
    "security_concerns_girls.being_threatened_with_violence",
    "security_concerns_girls.being_kidnapped",
    "security_concerns_girls.suffering_from_physical_harassment_or_violence_not_sexual",
    "security_concerns_girls.suffering_from_verbal_harassment",
    "security_concerns_girls.suffering_from_verbal_harassment",
    "security_concerns_girls.discrimination_or_persecution_because_of_ethnicity_status__etc",
    "security_concerns_girls.being_killed",
    "security_concerns_girls.being_detained",
    "security_concerns_girls.being_exploited_i_e_being_engaged_in_harmful_forms_of_labor_for_economic_gain_of_the_exploiter",
    "security_concerns_girls.being_sexually_exploited_in_exchange_of_humanitarian_aid_goods_services_money_or_preference_treatment",
    "security_concerns_girls.being_recruited_by_armed_groups",
    "security_concerns_girls.being_sent_abroad_to_find_work",
    "security_concerns_women.corp_punishment",
    "security_concerns_women.threats_violence",
    "security_concerns_women.abduction",
    "security_concerns_women.phys_harassment",
    "security_concerns_women.verb_harassment",
    "security_concerns_women.sex_harassment",
    "security_concerns_women.discrimination_ethnicity",
    "security_concerns_women.killed",
    "security_concerns_women.detention",
    "security_concerns_women.exploitation",
    "security_concerns_women.sex_exploitation",
    "security_concerns_women.recruitment_armed_grps",
    "security_concerns_women.sent_abroad_work",
    "security_concerns_women.deportation"
  )], na.rm = T) > 0 ~ 3, T ~ 1)

df$hh12 <- case_when(df$desired_info_type.assistance_return == 1 ~ 3, T ~ 1)

dap$repeat.for.variable <- "region"
dap$independent.variable <- "all"

df$all <- "nationwide"

result <- from_analysisplan_map_to_output(
  data = df,
  analysisplan = dap,
  weighting = weight_fun,
  confidence_level = 0.9
)

saveRDS(result, paste(sprintf("output/RDS/result_hno_%s.RDS", today())))

summary <-
  bind_rows(lapply(result[[1]], function(x) {
    x$summary.statistic
  }))

write.csv(summary,
          sprintf("output/raw_results/raw_results_hno_%s.csv", today()),
          row.names = F)


summary$dependent.var.value <- as.numeric(summary$dependent.var.value)


output <- data.frame(region = unique(summary$repeat.var.value))

for (indicator in unique(summary$dependent.var)) {
  for (region in unique(output$region)) {
    sub <-
      summary %>% filter(dependent.var == indicator &
                           repeat.var.value == region)
    score <- max(sub$dependent.var.value)
    num <- sub$numbers[sub$dependent.var.value == score]
    while (num < 0.25) {
      if ((score - 1) %in% sub$dependent.var.value) {
        num <- num + sub$numbers[sub$dependent.var.value == (score - 1)]
      }
      score <- score - 1
    }
    output[output$region == region, indicator] <- score
  }
}

#new nutrition/food security dataset to be used here
area_indicators <- area_indicators[,c(1:8)]

nutrition_indicators <- read_excel("input/dataset/nutrition dataset_migration.xlsx")

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

food_security_indicators <- read_excel("input/dataset/food_security_data_migration.xlsx")

food_security_indicators <- food_security_indicators[,c("District", "FCS", "coping_strategy", "reduced_strategy_index")]

area_indicators <- left_join(area_indicators, food_security_indicators, by=c("admin2Name"="District"))

indicator_names <- names(area_indicators)
indicator_names <- indicator_names[5:length(indicator_names)]

area_indicators$region <- case_when(area_indicators$admin1Name %in% c("Baalbek-El Hermel", "Bekaa") ~ "Baalbek-El Hermel and Bekaa",
                                    area_indicators$admin1Name %in% c("Beirut", "Mount Lebanon") ~ "BML",
                                    area_indicators$admin1Name %in% c("North", "Akkar") ~ "North and Akkar",
                                    area_indicators$admin1Name %in% c("South", "El Nabatieh") ~ "South and Nabatieh")

regional_indicators <- area_indicators %>% group_by(region) %>%
  summarize_at(.funs = mean, .vars = indicator_names) %>%
  mutate_at(.funs = ~round(.,0), .vars = indicator_names)

output <- left_join(regional_indicators, output, by="region")

output$mean_max_50 <- 0
for (i in 1:nrow(output)) {
  output$mean_max_50[i] <-
    round(mean(as.matrix(sort(
      output[i, c(indicator_names, dap$dependent.variable)], decreasing = T
    ))[1, 1:11]), 0)
}

output$total_population <- sampling_frame$individual[match(paste0(output$region, "_", "MIG"), sampling_frame$strata)]
output$minimum_population <- round(output$total_population * 0.25,0)

output_names <- data.frame(names = names(output))
output_names$labels <- dap$research.question[match(output_names$names, dap$dependent.variable)]
output_names$labels[is.na(output_names$labels)] <- output_names$names[is.na(output_names$labels)]

names(output) <- output_names$labels

write.csv(output, sprintf("output/Intersectoral/intersectoral_model_MIGRANTs_%s.csv", today()), row.names = F)








