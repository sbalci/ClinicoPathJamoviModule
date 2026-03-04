lines <- readLines("R/psychopdaROC.b.R")

replace_get_with_add_and_get <- function(lines, plot_name) {
  pattern <- sprintf("image <- self\\$results\\$%s\\$get\\(key=var\\)", plot_name)
  replacement <- sprintf("self$results$%s$addItem(key=var)\n            image <- self$results$%s$get(key=var)", plot_name, plot_name)
  for (i in 1:length(lines)) {
    if (grepl(pattern, lines[i])) {
       lines[i] <- gsub(pattern, replacement, lines[i])
    }
  }
  return(lines)
}

lines <- replace_get_with_add_and_get(lines, "effectSizePlot")
lines <- replace_get_with_add_and_get(lines, "powerCurvePlot")
lines <- replace_get_with_add_and_get(lines, "bayesianTracePlot")
lines <- replace_get_with_add_and_get(lines, "decisionCurvePlot")

writeLines(lines, "R/psychopdaROC.b.R")
cat("Patched addItem calls for advanced plots.\n")
