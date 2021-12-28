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
  read_excel("input/dataset/MSNA_data_clean_2021-12-22.xlsx", sheet = 2, guess_max = 50000) %>%
  filter(population_group == "MIG")

# import loop data
indiv_df <-
  read_excel("input/dataset/MSNA_data_clean_2021-12-22.xlsx", sheet = 3, guess_max = 50000) 

ocha_boundaries <- st_read("input/spatial_data/LBN_Admin_level2.geojson")

sampling_frame <- read.csv("input/sample/sampling_frame.csv")

weight_fun <- function(df){
  df$weight
}

source("recoding.R")


dap <- read.csv("input/dap/pin_dap.csv")

df$region <- case_when(df$governorate %in% c("Baalbek-El Hermel", "Bekaa") ~ "Baalbek-El Hermel and Bekaa",
                       df$governorate %in% c("Beirut", "Mount Lebanon") ~ "BML",
                       df$governorate %in% c("North", "Akkar") ~ "North and Akkar",
                       df$governorate %in% c("South", "El Nabatieh") ~ "South and Nabatieh")

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

indicator_names <- names(area_indicators)
indicator_names <- indicator_names[5:length(indicator_names)]
indicator_names <- indicator_names[!indicator_names %in% c("CVD", "MMR")]

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
      output[i, indicator_names], decreasing = T
    ))[1, 1:8]), 0)
}

output$total_population <- sampling_frame$population[match(paste0(output$admin2Name, "_", "MIG"), sampling_frame$strata)]
output$minimum_population <- round(output$total_population * 0.25,0)

output_names <- data.frame(names = names(output))
output_names$labels <- dap$research.question[match(output_names$names, dap$dependent.variable)]
output_names$labels[is.na(output_names$labels)] <- output_names$names[is.na(output_names$labels)]

names(output) <- output_names$labels

write.csv(output, sprintf("output/Intersectoral/intersectoral_model_MIGRANTs_%s.csv", today()), row.names = F)








