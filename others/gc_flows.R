# # generating code for Flows ----
#
# functionsClinicoPath <- c(
#     "tableone",
#     "summarydata",
#     "reportcat",
#     "alluvial",
#     "agepyramid",
#
#     "crosstable",
#
#     # Survival
#     "survival",
#     "multisurvival",
#     "oddsratio",
#
#     # Agreement
#     "agreement",
#
#     # Decision
#     "decision",
#     "decisioncalculator",
#
#     # JJ Functions
#     "statsplot2",
#     "jjbarstats",
#     "jjbetweenstats",
#     "jjcorrmat",
#     "jjdotplotstats",
#     "jjhistostats",
#     "jjpiestats",
#     "jjscatterstats",
#     "jjwithinstats"
# )
#
#
#
#
# magicfor::magic_for()
# for (i in 1:length(functionsClinicoPath)) {
#     functionsClinicoPath_variable <- functionsClinicoPath[i]
#
#     explanation <- paste0("Workflow for ClinicoPath::", functionsClinicoPath_variable)
#
#     y <- paste0("**",explanation,"** ", "\n", "\n",
#
#
#                 "```{r flow_", functionsClinicoPath_variable, "}","\n","\n",
#                 "flow::flow_view(x = ClinicoPath::", functionsClinicoPath_variable, ", out = here::here('vignettes/figures/flow_", functionsClinicoPath_variable, ".png'))","\n","\n",
#                 "```","\n","\n",
#
#
#                 "<img src='figures/flow_", functionsClinicoPath_variable, ".png' align='center' width = 75% />",
# "\n","\n","\n"
#
#     )
#     put(y)
# }
# writeLines(magicfor::magic_result_as_vector(),
#            here::here("childRmd", "generatedCodeFlows.Rmd"))
# magicfor::magic_free()
