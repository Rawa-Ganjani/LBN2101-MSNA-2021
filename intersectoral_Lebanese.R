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
  filter(population_group == "LEB")

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


dap$repeat.for.variable <- "district"
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


output <- data.frame(admin2Name = unique(summary$repeat.var.value))

for (indicator in unique(summary$dependent.var)) {
  for (district in unique(output$admin2Name)) {
    sub <-
      summary %>% filter(dependent.var == indicator &
                           repeat.var.value == district)
    score <- max(sub$dependent.var.value)
    num <- sub$numbers[sub$dependent.var.value == score]
    while (num < 0.25) {
      if ((score - 1) %in% sub$dependent.var.value) {
        num <- num + sub$numbers[sub$dependent.var.value == (score - 1)]
      }
      score <- score - 1
    }
    output[output$admin2Name == district, indicator] <- score
  }
}

output <- left_join(area_indicators, output, by="admin2Name")

indicator_names <- names(output)
indicator_names <- indicator_names[!grepl("admin", indicator_names)]

output$mean_max_50 <- 0
for (i in 1:nrow(output)) {
  output$mean_max_50[i] <-
    round(mean(as.matrix(sort(
      output[i, indicator_names], decreasing = T
    ))[1, 1:9]), 0)
}

output$total_population <- sampling_frame$population[match(paste0(output$admin2Name, "_", "LEB"), sampling_frame$strata)]
output$minimum_population <- round(output$total_population * 0.25,0)

output_names <- data.frame(names = names(output))
output_names$labels <- dap$research.question[match(output_names$names, dap$dependent.variable)]
output_names$labels[is.na(output_names$labels)] <- output_names$names[is.na(output_names$labels)]

names(output) <- output_names$labels

write.csv(output, sprintf("output/Intersectoral/intersectoral_model_%s.csv", today()), row.names = F)








