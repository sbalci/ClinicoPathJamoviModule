# ClinicoPathDrafts  update project ----

# Rscript ~/histopathRprojects/ClinicoPath/tododata/update_ClinicoPathDrafts.R

options(repos = c("https://cran.microsoft.com/snapshot/2020-08-24"))

print(paste0("copying files started at: ", Sys.time()))

readyfunctions <- c(
    # "^00",
    "refs",

    "^retracted\\."


    # "classification\\.",
    # "experimenter\\.",
    # "^tree\\."



    # "^pairchi2\\."


    # "^dendogram\\.",
    # "^nomogram\\.",
    # "^benford\\.",


    # Exploration

    ## ClinicoPath Descriptives

    # "^tableone",
    # "^summarydata",
    # "^reportcat"

    ## ClinicoPath Descriptive Plots

    # "^alluvial",
    # "^agepyramid",
    # "^lollipop",
    # "^linechart",

    ## ClinicoPath Comparisons
    # "^vartree",
    # "^crosstable"

    # # Survival
    # "^survival",
    # # "^survival2",
    # "^multisurvival",
    # "^oddsratio",
    # "^timeinterval"
    # "^competingsurvival",

    # # Agreement
    # "^agreement",
    # # "^icccoeff",

    # # Decision
    # "^decision",
    # "^decisioncalculator",
    # "^decision2\\.",
    # # "^roc",


    # # JJ Functions
    # "statsplot2",
    # "^jjbarstats\\.",
    # "^jjbetweenstats",
    # "^jjcorrmat",
    # "^jjdotplotstats",
    # "^jjhistostats",
    # "^jjpiestats",
    # "^jjscatterstats",
    # "^jjwithinstats"

    # # jjbarstats2

    # agepyramid
    # agreement

    # alluvial
    # casnet_SOURCE_jamovi.R
    # checkdata
    # cisingle
    # ClinicoPath-package.R
    # comparingsurvival
    # competingsurvival
    # conttables
    # conttablespaired
    # correlation
    # crosstable
    # data.R
    # decision
    # decision2
    # decisioncalculator
    # example00
    # example01
    # example02
    # example03
    # example04
    # example05
    # fadfa
    # fapsd
    # fasda
    # gc_flows.R
    # gtsummary
    # histogram
    # icccoeff
    # jjbarstats
    # jjbarstats2
    # jjbetweenstats
    # jjcorrmat
    # jjdotplotstats
    # jjhelpers_messages.R
    # jjhistostats
    # jjpiestats
    # jjscatterstats
    # jjwithinstats
    # "^jviolin"
    # kmeans
    # linechart
    # linreg
    # lollipop
    # manyttestsis
    # modellingsurvival
    # multisurvival
    # oddsratio
    # onesurvival
    # ppv
    # recnet
    # reportcat
    # retracted
    # roc
    # rqaau
    # rqacr
    # ssgbi
    # ssguni
    # statnet
    # statsplot2
    # summarydata
    # survival
    # tableone
    # timeinterval
    # tree
    # tsacf
    # tsent
    # tslvl
    # tspks
    # tsrr
    # tssur
    # tstst
    # utils-pipe.R
    # vartree
)


readyfunctions <- paste0(readyfunctions, collapse = "|")

files_R <-
    list.files(path = "~/histopathRprojects/ClinicoPath/R",
               pattern = readyfunctions,
               full.names = TRUE)

files_hR <- grep(pattern = "*.h.R$", x = files_R)

files_R <- files_R[-files_hR]


files_jamovi <-
    list.files(
        path = "~/histopathRprojects/ClinicoPath/jamovi",
        pattern = readyfunctions,
        full.names = TRUE
    )

files_jamovi_js <-
    list.files(
        path = "~/histopathRprojects/ClinicoPath/jamovi/js",
        pattern = readyfunctions,
        full.names = TRUE
    )

# readydata <- c(
#     "histopathology"
# )


# readydata <- paste0(readydata, collapse = "|")


# files_data <-
#     list.files(
#         path = "~/histopathRprojects/ClinicoPath/data",
#         # pattern = ,
#         full.names = TRUE
#     )



file.copy(from = files_R,
          to = "~/histopathRprojects/ClinicoPathDrafts/R/",
          overwrite = TRUE)


file.copy(from = files_jamovi,
          to = "~/histopathRprojects/ClinicoPathDrafts/jamovi/",
          overwrite = TRUE)

file.copy(from = files_jamovi_js,
          to = "~/histopathRprojects/ClinicoPathDrafts/jamovi/js/",
          overwrite = TRUE)



# file.copy(from = files_data,
#           to = "~/histopathRprojects/ClinicoPathDrafts/data/",
#           overwrite = TRUE)
#
# file.copy(from = "~/histopathRprojects/ClinicoPath/DESCRIPTION",
#           to = "~/histopathRprojects/ClinicoPathDrafts/",
#           overwrite = TRUE)


print(paste0("copying files ended at: ", Sys.time()))

setwd("~/histopathRprojects/ClinicoPathDrafts/")
jmvtools::prepare()
devtools::document()
jmvtools::install()
# setwd(here::here())




# CommitMessage <-
#     paste("WIP at: ", Sys.time(), sep = "")
#
# wd            <- "~/histopathRprojects/ClinicoPathDrafts/"
#
# gitCommand    <-
#     paste(
#         "cd ",
#         wd,
#         " \n git add . \n git commit --message '",
#         CommitMessage,
#         "' \n",
#         sep = ""
#     )
#
# system(
#     command = paste(gitCommand, "\n") ,
#     intern = TRUE,
#     wait = TRUE
# )





setwd("~/histopathRprojects/ClinicoPath/")
jmvtools::prepare()
devtools::document()
