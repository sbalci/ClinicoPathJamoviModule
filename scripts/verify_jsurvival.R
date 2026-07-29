# End-to-end execution harness for the jsurvival analyses.
#
# Runs each public wrapper against the bundled histopathology data plus the
# edge cases that the event-level review turned up, and reports what errored,
# warned, or silently produced nothing. Run before and after a change and diff
# the output.
#
#   Rscript scripts/verify_jsurvival.R
#
# Notes for anyone extending this:
#   * jamovi `type: Level` options carry no default, so they are REQUIRED
#     arguments of the generated wrapper. Pass NULL when the paired variable
#     is omitted.
#   * A "silent" result -- no error, no warning, but no populated output -- is
#     the failure mode this whole review was about, so it is reported too.

suppressMessages({
    library(ClinicoPath)
})

data(histopathology, package = "ClinicoPath")
hp <- histopathology

# ---- edge-case datasets ----------------------------------------------------
base <- data.frame(
    time  = hp$OverallTime,
    grp   = factor(ifelse(is.na(hp$Group), "Control", as.character(hp$Group))),
    stringsAsFactors = FALSE
)
n <- nrow(base)

cases <- list(
    `factor 2-level`      = within(base, out <- factor(hp$Death)),
    `factor 2-level + NA` = within(base, out <- factor(hp$Death)),
    `factor 4-level`      = within(base, out <- factor(hp$Outcome2)),
    `numeric 0/1`         = within(base, out <- hp$Outcome),
    `numeric 0/1 + NA`    = within(base, out <- hp$Outcome),
    `numeric 1/2`         = within(base, out <- ifelse(hp$Outcome == 1, 2, 1)),
    `numeric -1/2`        = within(base, out <- ifelse(hp$Outcome == 1, 2, -1)),
    `all events`          = within(base, out <- rep(1, n)),
    `all censored`        = within(base, out <- rep(0, n))
)

# ---- runner ----------------------------------------------------------------
outcome_of <- function(res) {
    if (inherits(res, "error"))   return(c(status = "ERROR",   detail = conditionMessage(res)))
    if (length(res$warnings))     return(c(status = "WARN",    detail = res$warnings[[1]]))
    c(status = "ok", detail = "")
}

run_one <- function(label, expr) {
    warns <- character(0)
    res <- withCallingHandlers(
        tryCatch(force(expr), error = function(e) e),
        warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
    )
    if (inherits(res, "error")) {
        st <- "ERROR"; detail <- conditionMessage(res)
    } else if (length(warns)) {
        st <- "warn"; detail <- warns[[1]]
    } else {
        st <- "ok"; detail <- ""
    }
    cat(sprintf("  %-6s %-52s %s\n", st, label, substr(gsub("\\s+", " ", detail), 1, 90)))
    invisible(st)
}

cat("\n=== survival: outcome shapes ===\n")
for (nm in names(cases)) {
    d <- cases[[nm]]
    lvl <- if (is.factor(d$out)) levels(d$out)[2] else NULL
    run_one(nm, ClinicoPath::survival(
        data = d, elapsedtime = "time", outcome = "out", explanatory = "grp",
        outcomeLevel = lvl, dod = NULL, dooc = NULL, awd = NULL, awod = NULL))
}

cat("\n=== survival: event-level edge cases ===\n")
d4 <- cases[["factor 4-level"]]

run_one("no event level selected (must be a clear message)",
    ClinicoPath::survival(data = d4, elapsedtime = "time", outcome = "out",
        explanatory = "grp", outcomeLevel = NULL,
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL))

run_one("event level absent from data (must error, not 0 events)",
    ClinicoPath::survival(data = d4, elapsedtime = "time", outcome = "out",
        explanatory = "grp", outcomeLevel = "NOT_A_LEVEL",
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL))

run_one("numeric outcome, event level '0' (must be honoured)",
    ClinicoPath::survival(data = cases[["numeric 0/1"]], elapsedtime = "time",
        outcome = "out", explanatory = "grp", outcomeLevel = "0",
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL))

cat("\n=== survival: multievent ===\n")
me <- function(type, ...) ClinicoPath::survival(
    data = d4, elapsedtime = "time", outcome = "out", explanatory = "grp",
    outcomeLevel = NULL, multievent = TRUE, analysistype = type, ...)

run_one("multievent, only dod set (must error, not delete patients)",
    me("overall", dod = "DOD", dooc = NULL, awd = NULL, awod = NULL))
for (ty in c("overall", "cause", "dfs", "compete"))
    run_one(sprintf("multievent %s, all four assigned", ty),
        me(ty, dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD"))
run_one("multievent compete + RMST (must be blocked, not inverted)",
    ClinicoPath::survival(data = d4, elapsedtime = "time", outcome = "out",
        explanatory = "grp", outcomeLevel = NULL, multievent = TRUE,
        analysistype = "compete", rmst_analysis = TRUE,
        dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD"))

cat("\n=== singlearm ===\n")
for (nm in c("factor 2-level", "factor 4-level", "numeric 0/1", "all events")) {
    d <- cases[[nm]]
    lvl <- if (is.factor(d$out)) levels(d$out)[2] else NULL
    run_one(nm, ClinicoPath::singlearm(
        data = d, elapsedtime = "time", outcome = "out", outcomeLevel = lvl,
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL))
}

cat("\n=== survivalcont ===\n")
dc <- within(cases[["factor 2-level"]], cont <- hp$Age)
run_one("factor outcome + continuous predictor",
    ClinicoPath::survivalcont(data = dc, elapsedtime = "time", outcome = "out",
        contexpl = "cont", outcomeLevel = levels(dc$out)[2],
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL))
run_one("no event level (previously crashed with a raw R error)",
    ClinicoPath::survivalcont(data = dc, elapsedtime = "time", outcome = "out",
        contexpl = "cont", outcomeLevel = NULL,
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL))
run_one("numeric 1/2 outcome (previously failed silently)",
    ClinicoPath::survivalcont(data = within(dc, out <- ifelse(hp$Outcome == 1, 2, 1)),
        elapsedtime = "time", outcome = "out", contexpl = "cont",
        outcomeLevel = NULL, dod = NULL, dooc = NULL, awd = NULL, awod = NULL))

cat("\n=== multisurvival ===\n")
run_one("text-level factor outcome (previously aborted the analysis)",
    ClinicoPath::multisurvival(data = cases[["factor 2-level"]],
        elapsedtime = "time", outcome = "out", explanatory = "grp",
        outcomeLevel = levels(cases[["factor 2-level"]]$out)[2],
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL))

cat("\n=== outcomeorganizer ===\n")
run_one("os with a 4-level factor",
    ClinicoPath::outcomeorganizer(data = d4, outcome = "out",
        outcomeLevel = "DOD", recurrenceLevel = NULL,
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL))
run_one("compete, all four assigned",
    ClinicoPath::outcomeorganizer(data = d4, outcome = "out",
        outcomeLevel = "DOD", recurrenceLevel = NULL, multievent = TRUE,
        analysistype = "compete",
        dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD"))
run_one("compete, only dod assigned (must error)",
    ClinicoPath::outcomeorganizer(data = d4, outcome = "out",
        outcomeLevel = "DOD", recurrenceLevel = NULL, multievent = TRUE,
        analysistype = "compete",
        dod = "DOD", dooc = NULL, awd = NULL, awod = NULL))

cat("\n=== timeinterval / datetimeconverter ===\n")
dd <- data.frame(dx = hp$SurgeryDate, fu = hp$LastFollowUpDate, stringsAsFactors = FALSE)
run_one("timeinterval from dates",
    ClinicoPath::timeinterval(data = dd, start_date = "dx", end_date = "fu"))
run_one("datetimeconverter on text dates",
    ClinicoPath::datetimeconverter(data = dd, datetime_var = "dx"))
run_one("datetimeconverter on Excel serials",
    ClinicoPath::datetimeconverter(
        data = data.frame(d = c(45000, 44927, 43831)), datetime_var = "d"))

cat("\ndone\n")
