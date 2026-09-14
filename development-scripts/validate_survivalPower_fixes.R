# Run from the repository root after jmvtools::prepare().
# Keeps repair evidence separate from the historical, report-only audit.
source("R/survivalPower.h.R")
source("R/survivalPower_distributions.R")
source("R/survivalPower.b.R")

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1]] else "development-ideas/survivalPower-fixes-2026-09-13"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
spec <- yaml::read_yaml("jamovi/survivalPower.r.yaml")
make <- function(...) {
  options <- modifyList(list(show_summary = TRUE, show_interpretation = TRUE,
    show_explanations = TRUE, show_glossary = TRUE, guided_mode = TRUE), list(...))
  a <- survivalPowerClass$new(options = do.call(survivalPowerOptions$new, options),
                            data = data.frame())
  a$run()
  a
}
analyses <- list(
  sample_size = make(sensitivity_analysis = TRUE, run_simulation_validation = TRUE,
                     simulation_runs = 1000),
  power = make(analysis_type = "power", sensitivity_analysis = TRUE),
  duration = make(analysis_type = "duration", sample_size_input = 500),
  effect = make(analysis_type = "effect_size"),
  ni = make(test_type = "non_inferiority", effect_size = 1, alpha_level = 0.025),
  multi_arm = make(study_design = "multi_arm", multiple_comparisons = "bonferroni"),
  sequential = make(test_type = "cox_regression", interim_analyses = 2,
                    alpha_spending = "obrien_fleming"),
  cluster = make(study_design = "cluster_randomized", cluster_size = 100,
                 icc = 0.01, effect_size = 0.1),
  sparse = make(analysis_type = "power", sample_size_input = 10,
                control_median_survival = 240, accrual_period = 1, follow_up_period = 0,
                run_simulation_validation = TRUE, simulation_runs = 1000)
)

population <- do.call(rbind, lapply(spec$items, function(item) {
  scenarios <- names(Filter(function(a) {
    x <- a$results$get(item$name)
    if (item$type == "Table") return(nrow(x$asDF) > 0 && ncol(x$asDF) > 1)
    if (item$type == "Image") return(!is.null(x$state) && nrow(x$state$data) > 0)
    !is.null(x$content) && any(nzchar(x$content))
  }, analyses))
  data.frame(output = item$name, type = item$type, populated = length(scenarios) > 0,
             examples = paste(scenarios, collapse = ";"))
}))
stopifnot(nrow(population) == 24, all(population$populated))
write.csv(population, file.path(out, "output-population.csv"), row.names = FALSE)

images <- Filter(function(x) x$type == "Image", spec$items)
dark_theme <- ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(
  plot.background = ggplot2::element_rect(fill = "#202124", colour = NA),
  panel.background = ggplot2::element_rect(fill = "#202124", colour = NA),
  text = ggplot2::element_text(colour = "white"),
  axis.text = ggplot2::element_text(colour = "white"),
  panel.grid.major = ggplot2::element_line(colour = "#555555"),
  panel.grid.minor = ggplot2::element_line(colour = "#333333")
)
render <- function(a, item, state = a$results$get(item$name)$state,
                   ggtheme = ggplot2::theme_minimal(base_size = 12)) {
  a$.__enclos_env__$private[[item$renderFun]](list(state = state),
    ggtheme = ggtheme, theme = NULL)
}
grDevices::pdf(file.path(out, "rendered-plots.pdf"), width = 8, height = 6)
rendered <- lapply(images, function(item) {
  valid <- render(analyses$sample_size, item)
  dark <- render(analyses$sample_size, item, ggtheme = dark_theme)
  empty <- render(analyses$sample_size, item, state = NULL)
  stopifnot(isTRUE(valid), isTRUE(dark), identical(empty, FALSE))
  data.frame(output = item$name, valid_state = valid, dark_theme = dark, null_state = empty)
})
power_item <- Filter(function(x) x$name == "power_curve_plot", images)[[1]]
old_state <- analyses$sample_size$results$power_curve_plot$state
old_state$options$power_target <- NULL
stopifnot(isTRUE(render(analyses$sample_size, power_item, state = old_state)))
grDevices::dev.off()
write.csv(do.call(rbind, rendered), file.path(out, "plot-rendering.csv"), row.names = FALSE)

# PNGs make the changed displays easy to inspect without a jamovi installation.
for (item in images) {
  ragg::agg_png(file.path(out, paste0(item$name, ".png")), width = 800, height = 600)
  stopifnot(isTRUE(render(analyses$sample_size, item)))
  grDevices::dev.off()
  ragg::agg_png(file.path(out, paste0(item$name, "-dark.png")), width = 800, height = 600)
  stopifnot(isTRUE(render(analyses$sample_size, item,
                        ggtheme = dark_theme)))
  grDevices::dev.off()
}
for (pair in list(c("duration", "accrual_timeline_plot"), c("effect", "survival_curves_plot"))) {
  item <- Filter(function(x) x$name == pair[2], images)[[1]]
  ragg::agg_png(file.path(out, paste0(pair[1], "-", pair[2], ".png")),
                width = 800, height = 600)
  stopifnot(isTRUE(render(analyses[[pair[1]]], item)))
  grDevices::dev.off()
}

registry <- yaml::read_yaml("jamovi/00refs.yaml")$refs
citations <- do.call(rbind, lapply(spec$refs, function(key) {
  entry <- registry[[key]]
  data.frame(key = key, exists = !is.null(entry), author = entry$author, year = entry$year)
}))
stopifnot(all(citations$exists), all(nzchar(citations$author)), all(nzchar(citations$year)))
write.csv(citations, file.path(out, "citation-integrity.csv"), row.names = FALSE)

snapshots <- lapply(analyses, function(a) {
  p <- a$.__enclos_env__$private
  list(design = p$.resolved_design(), primary = p$primary_numbers,
    notices = a$results$notices$content, summary = a$results$natural_language_summary$content,
    interpretation = a$results$clinical_interpretation$content,
    simulation = p$simulation_cache[c("empirical_power", "ci_lower", "ci_upper",
      "zero_event_trials", "failed_tests", "valid_sims", "numerical_warnings", "convergence")])
})
jsonlite::write_json(snapshots, file.path(out, "resolved-designs.json"),
  pretty = TRUE, auto_unbox = TRUE, digits = 12, null = "null")
runtime <- list(R = R.version.string, packages = setNames(lapply(
  c("jmvcore", "gsDesign", "survival", "testthat"), function(x) as.character(packageVersion(x))),
  c("jmvcore", "gsDesign", "survival", "testthat")))
jsonlite::write_json(runtime, file.path(out, "runtime.json"), pretty = TRUE, auto_unbox = TRUE)

files <- c("R/survivalPower.b.R", "R/survivalPower.h.R", "R/survivalPower_distributions.R",
  "jamovi/survivalPower.a.yaml", "jamovi/survivalPower.u.yaml", "jamovi/survivalPower.r.yaml",
  "jamovi/js/survivalPower.events.js", "jamovi/00refs.yaml", "man/survivalPower.Rd",
  "development-scripts/validate_survivalPower_fixes.R",
  list.files("tests/testthat", pattern = "^test-survivalPower.*[.]R$", full.names = TRUE))
write.csv(data.frame(path = files, md5 = unname(tools::md5sum(files))),
  file.path(out, "source-fingerprints.csv"), row.names = FALSE)
cat("24 outputs populated; 8 references resolve; all 5 renderers pass valid/NULL states.\n")
cat("Legacy power-plot state and solved duration/effect PNGs also render.\n")
