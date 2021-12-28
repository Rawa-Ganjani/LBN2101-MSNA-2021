output_and_format <- function(df, dap, weight.fun, questions, choices, aggregation, disaggregation){
  questionnaire <- load_questionnaire(df, questions, choices, "label::English")
  
  dap$independent.variable <- disaggregation
  dap$repeat.for.variable <- aggregation
  
  usecores <- detectCores() - 2
  cl <- makeCluster(usecores)
  registerDoParallel(cl)
  
  results <- foreach(i = 1:nrow(dap)) %dopar% {
    library(dplyr)
    library(hypegrammaR)
    
    result <- from_analysisplan_map_to_output(
      data = df,
      analysisplan = dap[i,],
      weighting = weight.fun,
      questionnaire = questionnaire,
      confidence_level = 0.9
    )
    
    bind_rows(lapply(result[[1]], function(x) {
      x$summary.statistic
    }))
  }
  stopCluster(cl)
  
  summary <- bind_rows(lapply(results, function(x) {
    x
  }))
  
  summary <- left_join(summary, dap, by=c("dependent.var"="dependent.variable"))
  summary <- summary[!is.na(summary$numbers),]
  
  summary$choice_label <- choices$`label::English`[match(paste0(summary$choice_list_name, summary$dependent.var.value), paste0(choices$list_name, choices$name))]
  summary$choice_label <- case_when(is.na(summary$choice_label) & summary$dependent.variable.type != "numerical" ~ summary$dependent.var.value, T ~ summary$choice_label)
  
  dap <- dap[dap$name %in% summary$dependent.var,]
  
  result <- data.frame(matrix(ncol = 5, nrow = 0))
  names(result) <- c("repeat.var.value", "independent.var.value", "frequency", "dependent.var", "dependent.var.value")
  
  for(column in dap$dependent.variable[dap$dependent.variable.type == "categorical"]) {
    abc <- group_by(df, repeat.var.value = df[[aggregation]], independent.var.value = df[[disaggregation]], dependent.var.value = as.character(df[[column]])) %>%
      summarise(frequency = n())
    abc$dependent.var = column
    names(abc) <- c("repeat.var.value", "independent.var.value", "dependent.var.value", "frequency", "dependent.var")
    result = rbind(result, abc)
  }
  
  result <- result[!is.na(result$dependent.var.value),]
  
  output <- left_join(summary, result, by = c("dependent.var", "independent.var.value", "repeat.var.value" , "dependent.var.value"))
  
  output <- output %>% dplyr::select(
    question_name = "dependent.var",
    question_label = "label::English",
    choice_name = "dependent.var.value",
    choice_label,
    independent.var.value,
    repeat.var.value,
    numbers,
    min_confidence_level = "min",
    max_confidence_level = "max",
    frequency,
    question_type
  )
  
  return(output)
}
