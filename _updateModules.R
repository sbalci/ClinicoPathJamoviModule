#!/usr/bin/env Rscript
# Distribute umbrella analyses to the jamovi submodules and build them.
#
#   Rscript _updateModules.R                       # every module enabled in the config
#   Rscript _updateModules.R --dry-run             # print the plan and the diff; write nothing
#   Rscript _updateModules.R OncoPath jsurvival    # only these modules (ignores the toggles)
#   Rscript _updateModules.R --no-install          # apply + build + verify, skip jmvtools::install()
#   Rscript _updateModules.R --config=path.yaml    # another registry (e.g. pointing at sandbox copies)
#
# Pipeline (see _updateModules_plan.R and the "Pipeline" section of _updateModules_utils.R):
#   1. plan every enabled module; any plan error -> nothing is written, exit 1
#   2. per module: apply (write/prune files, DESCRIPTION) -> build (prepare, document,
#      Imports sync) -> verify (Collate, pkg:: declarations, prune_imports, bare symbols)
#      -> install. A failing module stops at that step; the others continue.
#   3. summary: modules, analyses added/updated/removed, analyses still test-routed.
#      Exit status 1 if any module failed.

local({
  argv <- commandArgs(trailingOnly = TRUE)
  file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  script_dir <- if (length(file_arg)) dirname(normalizePath(file_arg))
                else tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
  config_path <- sub("^--config=", "", grep("^--config=", argv, value = TRUE))
  if (!length(config_path)) config_path <- file.path(script_dir, "_updateModules_config.yaml")
  dry_run <- "--dry-run" %in% argv
  no_install <- "--no-install" %in% argv
  only <- argv[!startsWith(argv, "--")]
  finish <- function(status) {
    if (!interactive()) quit(status = status, save = "no")
    invisible(status)
  }

  Sys.unsetenv("ELECTRON_RUN_AS_NODE")   # jmvtools::prepare() fails under VS Code's node
  source(file.path(script_dir, "_updateModules_utils.R"), local = TRUE)
  source(file.path(script_dir, "_updateModules_plan.R"), local = TRUE)

  reg <- read_registry(config_path, only)
  cfg <- reg$config
  modes <- cfg$modes %||% list()
  U <- reg$umbrella_root

  if (isTRUE(cfg$quick) && !dry_run) {
    cat("Quick mode: installing the umbrella package only\n")
    devtools::install(U, quick = TRUE, upgrade = FALSE, build_vignettes = FALSE)
    return(finish(0L))
  }

  cat("Planning", paste(names(Filter(function(m) m$enabled, reg$modules)), collapse = ", "), "...\n")
  plan <- compute_distribution_plan(reg)
  diffs <- lapply(plan$modules, function(mp) if (is.null(mp$files)) NULL else diff_module(mp, reg))
  print_plan(plan, reg, diffs)

  if (length(plan$errors)) {
    cat("\nPLAN ERRORS -- nothing was written:\n", paste0("  ", plan$errors, collapse = "\n"), "\n", sep = "")
    return(finish(1L))
  }
  if (dry_run) {
    print_run_summary(plan, diffs, dry_run = TRUE)
    return(finish(0L))
  }

  missing <- Filter(function(p) !requireNamespace(p, quietly = TRUE), c("yaml", "desc", "jmvtools", "devtools"))
  if (length(missing)) stop("Missing required packages: ", paste(missing, collapse = ", "))

  if (isTRUE(cfg$ClinicoPath)) {
    cat("\nUmbrella: version", cfg$new_version, "date", cfg$new_date, "\n")
    set_module_version(U, cfg$new_version, cfg$new_date)
  }

  all_dirs <- vapply(reg$modules, function(m) m$directory, "")
  opts <- list(build = modes$extended %||% TRUE, install = !no_install, check = isTRUE(modes$check),
               webpage = isTRUE(modes$webpage),
               guard_template = file.path(U, "_updateModules_test_dependency_guard.R"),
               other_dirs = lapply(setNames(names(all_dirs), names(all_dirs)),
                                   function(m) unname(all_dirs[setdiff(names(all_dirs), c(m, "JamoviTest"))])))
  results <- lapply(plan$modules, run_module, reg = reg, opts = opts)

  n_failed <- print_run_summary(plan, diffs, results)
  if (!isTRUE(opts$build)) cat("Files distributed; modules NOT built (modes$extended: false).\n")
  else if (n_failed == 0L) cat("All", length(results), "module(s) completed", if (opts$install) "and installed." else "(install skipped).", "\n")
  finish(if (n_failed) 1L else 0L)
})
