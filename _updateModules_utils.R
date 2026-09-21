# Writing side of _updateModules.R: DESCRIPTION/NAMESPACE maintenance, dependency checks,
# and the apply -> build -> verify -> install pipeline. Deciding WHAT goes where is
# _updateModules_plan.R's job; nothing here chooses files.

# Prune DESCRIPTION.backup.<timestamp> files older than `days` days.
# Uses the filename timestamp (not file mtime) so git checkouts / OS-level
# touches don't accidentally extend the lifespan. Silent on no-op; reports
# count + paths on actual removal.
prune_description_backups <- function(module_dir, days = 10) {
  if (!dir.exists(module_dir)) return(invisible(0L))

  pattern <- "^DESCRIPTION\\.backup\\.([0-9]{8}_[0-9]{6})$"
  candidates <- list.files(module_dir, pattern = pattern, full.names = TRUE)
  if (length(candidates) == 0) return(invisible(0L))

  stamps <- regmatches(basename(candidates), regexec(pattern, basename(candidates)))
  parsed <- vapply(stamps, function(m) if (length(m) == 2) m[[2]] else NA_character_, character(1))
  times <- as.POSIXct(parsed, format = "%Y%m%d_%H%M%S", tz = "UTC")

  cutoff <- Sys.time() - as.difftime(days, units = "days")
  stale <- !is.na(times) & times < cutoff
  if (!any(stale)) return(invisible(0L))

  removed <- file.remove(candidates[stale])
  n_removed <- sum(removed)
  if (n_removed > 0) {
    message("🧹 Pruned ", n_removed, " DESCRIPTION backup(s) older than ", days,
            " days in ", basename(module_dir))
  }
  invisible(n_removed)
}

# NAMESPACE-DESCRIPTION Synchronization: Check and update DESCRIPTION based on NAMESPACE
sync_namespace_with_description <- function(module_dir, dry_run = FALSE) {
  namespace_file <- file.path(module_dir, "NAMESPACE")
  desc_file <- file.path(module_dir, "DESCRIPTION")
  
  if (!file.exists(namespace_file)) {
    message("ℹ️ No NAMESPACE file found in ", basename(module_dir), " - skipping sync")
    return(TRUE)
  }
  
  if (!file.exists(desc_file)) {
    warning("❌ DESCRIPTION file not found in ", basename(module_dir), " - cannot sync")
    return(FALSE)
  }
  
  tryCatch({
    # Read NAMESPACE file and extract package imports
    namespace_lines <- readLines(namespace_file, warn = FALSE)
    
    # Extract packages from various import patterns
    imported_packages <- c()
    
    # Parse different import patterns
    for (line in namespace_lines) {
      line <- trimws(line)
      
      # import(package) or import(package, except = c(...)) -> take FIRST arg only.
      # The old `([^)]+)` captured up to the first ')', which for an
      # `import(dplyr, except = c(a, b))` directive grabbed
      # "dplyr, except = c(a, b" and injected it as a bogus package name into
      # DESCRIPTION Imports. Extract just the leading package identifier.
      if (grepl("^import\\(", line)) {
        pkg <- sub("^import\\(\\s*[\"']?([A-Za-z0-9._]+).*", "\\1", line)
        imported_packages <- c(imported_packages, pkg)
      }
      
      # importFrom(package, ...)
      if (grepl("^importFrom\\(", line)) {
        pkg <- gsub("^importFrom\\(([^,)]+).*", "\\1", line)
        imported_packages <- c(imported_packages, pkg)
      }
      
      # requireNamespace patterns in comments or code
      if (grepl("requireNamespace.*['\"]([^'\"]+)['\"]", line)) {
        pkg <- gsub(".*requireNamespace.*['\"]([^'\"]+)['\"].*", "\\1", line)
        imported_packages <- c(imported_packages, pkg)
      }
    }
    
    # Clean up package names
    imported_packages <- unique(trimws(imported_packages))
    imported_packages <- imported_packages[nchar(imported_packages) > 0]
    # Only `base` is implicit. Every OTHER base-priority package (grDevices,
    # grid, stats, utils, methods, graphics, ...) still needs a DESCRIPTION
    # declaration when it appears in NAMESPACE, or R CMD check emits
    # "'::' or ':::' import not declared from: 'grDevices'" plus a namespace
    # dependency NOTE. Filtering them out here is what let
    # importFrom(grDevices, hcl.colors) ship with grDevices absent from Imports.
    imported_packages <- imported_packages[imported_packages != "base"]
    
    if (length(imported_packages) == 0) {
      message("ℹ️ No external packages found in NAMESPACE for ", basename(module_dir))
      return(TRUE)
    }
    
    message("📦 Found packages in NAMESPACE: ", paste(imported_packages, collapse = ", "))
    
    # Read current DESCRIPTION via the `desc` package, which preserves the
    # multi-line formatting of fields like `Remotes:` that base R's
    # read.dcf() + write.dcf() round-trip would reflow.
    if (!requireNamespace("desc", quietly = TRUE)) {
      warning("❌ The 'desc' package is required for safe DESCRIPTION editing. ",
              "Install with: install.packages('desc'). Skipping sync for ",
              basename(module_dir))
      return(FALSE)
    }
    d <- desc::desc(file = desc_file)

    # Get current Imports and Suggests as character vectors (version specs included)
    current_imports_raw <- tryCatch(d$get_list("Imports"), error = function(e) character(0))
    current_suggests_raw <- tryCatch(d$get_list("Suggests"), error = function(e) character(0))

    # Clean package names (remove version specifications) for comparison
    strip_versions <- function(x) trimws(gsub("\\s*\\([^)]*\\)", "", x))
    current_imports <- strip_versions(current_imports_raw)
    current_suggests <- strip_versions(current_suggests_raw)
    current_imports <- current_imports[nchar(current_imports) > 0]
    current_suggests <- current_suggests[nchar(current_suggests) > 0]

    # Find missing packages
    all_declared <- c(current_imports, current_suggests)
    missing_packages <- imported_packages[!imported_packages %in% all_declared]

    if (length(missing_packages) == 0) {
      message("✅ All NAMESPACE packages are declared in DESCRIPTION for ", basename(module_dir))
      return(TRUE)
    }

    message("⚠️ Missing packages in DESCRIPTION for ", basename(module_dir), ": ", paste(missing_packages, collapse = ", "))

    if (dry_run) {
      message("🔍 DRY RUN: Would add packages to Imports: ", paste(missing_packages, collapse = ", "))
      return(TRUE)
    }

    # Update the Imports field — preserve any existing version specs on
    # already-declared packages, add bare names for new ones, sort for stable ordering.
    bare_to_raw <- setNames(current_imports_raw, current_imports)
    updated_imports_bare <- sort(unique(c(current_imports, missing_packages)))
    updated_imports_full <- vapply(
      updated_imports_bare,
      function(name) if (name %in% names(bare_to_raw)) bare_to_raw[[name]] else name,
      character(1)
    )
    # Write as a clean multi-line block (`Imports:\n    pkg1,\n    pkg2`).
    # desc::set_list collapses with a header offset that drops the space after
    # the colon; building the value as a single string with leading newline +
    # 4-space indent gives the standard CRAN-style formatting.
    d$set(Imports = paste0("\n    ", paste(updated_imports_full, collapse = ",\n    ")))

    # Prune backups older than 10 days before creating a new one (keeps the
    # working tree tidy without losing recent rollback options).
    prune_description_backups(module_dir, days = 10)

    # Create backup of original DESCRIPTION
    backup_file <- paste0(desc_file, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(desc_file, backup_file)
    message("💾 Created backup: ", basename(backup_file))

    # Write updated DESCRIPTION. `desc` rewrites only fields it touched and
    # preserves the original formatting of Remotes, Authors@R, etc.
    d$write(file = desc_file)
    message("✅ Updated DESCRIPTION for ", basename(module_dir), " - added: ", paste(missing_packages, collapse = ", "))

    return(TRUE)
    
  }, error = function(e) {
    warning("❌ Failed to sync NAMESPACE with DESCRIPTION for ", basename(module_dir), ": ", e$message)
    return(FALSE)
  })
}

# =============================================================================
# Dependency declaration checks (used by verify_module)
# -----------------------------------------------------------------------------
# Rationale: the distributed submodule DESCRIPTIONs are hand-maintained and the
# existing NAMESPACE->DESCRIPTION sync (sync_namespace_with_description) is driven
# by the NAMESPACE file, which only records roxygen @import/@importFrom directives.
# It therefore CANNOT see `pkg::fun()`-style namespaced calls. Real defects have
# shipped this way: jsurvival used cmprsk::cuminc() (hard crash) and meddecide used
# vcd::Kappa()/lme4::lmer() (silent statistical degradation / dead feature) while
# those packages were absent from the submodule Imports. jamovi installs only a
# submodule's Imports, so end users crashed even though the umbrella was fine.
#
# check_module_dependencies() closes that gap by walking each parsed R expression,
# so package-like text in comments and strings is ignored. Unguarded namespace or
# package-attachment use must be declared in Imports/Depends. Use that is proven
# optional by lexical requireNamespace() control flow may instead be in Suggests.
# Only `base` itself is implicit; every other base-priority package (grDevices,
# grid, stats, utils, ...) plus Recommended and transitive packages still require
# a direct declaration.
# =============================================================================

# `base` is the ONLY package R CMD check lets you use without declaring it.
# grDevices/grid/stats/utils/methods/graphics are base-PRIORITY but not implicit:
# using them via `pkg::` or importFrom() without an Imports entry produces
# "'::' or ':::' import not declared from: 'grDevices'". Treating all base-priority
# packages as implicit here is what hid the missing grDevices in
# ClinicoPathDescriptives and the missing grid/grDevices/stats/utils in OncoPath.
get_base_packages <- function() {
  "base"
}

# Extract packages referenced through `pkg::` / `pkg:::` and classify each use by
# lexical control flow. A use is optional only inside the true branch of a positive
# requireNamespace("pkg") check, or after a terminal negative guard such as
# `if (!requireNamespace("pkg")) return()` in the same block.
scan_r_package_usage <- function(r_dir) {
  required <- character(0)
  guarded <- character(0)
  parse_errors <- character(0)
  empty <- list(
    required = required,
    guarded = guarded,
    used = character(0),
    parse_errors = parse_errors
  )
  if (!dir.exists(r_dir)) return(empty)

  call_name <- function(expr) {
    if (!is.call(expr)) return(NA_character_)
    head <- expr[[1]]
    if (is.symbol(head)) return(as.character(head))
    if (is.call(head) && identical(head[[1]], as.name("::")) &&
        as.character(head[[2]]) == "base") {
      return(as.character(head[[3]]))
    }
    NA_character_
  }

  literal_package_arg <- function(expr) {
    if (!is.call(expr) || length(expr) < 2) return(character(0))
    args <- as.list(expr[-1])
    arg_names <- names(args)
    package_index <- match("package", arg_names)
    if (is.na(package_index)) package_index <- 1L
    arg <- args[[package_index]]
    if (is.character(arg) && length(arg) == 1) return(arg)
    character_only_index <- match("character.only", arg_names)
    character_only <- !is.na(character_only_index) &&
      isTRUE(args[[character_only_index]])
    if (is.symbol(arg) && !character_only) return(as.character(arg))
    character(0)
  }

  require_namespace_pkg <- function(expr) {
    if (!is.call(expr) || !identical(call_name(expr), "requireNamespace")) {
      return(character(0))
    }
    args <- as.list(expr[-1])
    if (length(args) == 0) return(character(0))
    package_index <- match("package", names(args))
    if (is.na(package_index)) package_index <- 1L
    arg <- args[[package_index]]
    if (is.character(arg) && length(arg) == 1) arg else character(0)
  }

  common <- function(x, y) intersect(unique(x), unique(y))

  available_when_true <- NULL
  available_when_false <- NULL

  available_when_true <- function(expr) {
    if (!is.call(expr)) return(character(0))
    pkg <- require_namespace_pkg(expr)
    if (length(pkg) > 0) return(pkg)

    head <- expr[[1]]
    name <- call_name(expr)
    if (identical(head, as.name("(")) || identical(name, "isTRUE")) {
      return(available_when_true(expr[[2]]))
    }
    if (identical(name, "isFALSE")) {
      return(available_when_false(expr[[2]]))
    }
    if (identical(head, as.name("!"))) {
      return(available_when_false(expr[[2]]))
    }
    if (identical(head, as.name("&&")) || identical(head, as.name("&"))) {
      return(unique(c(
        available_when_true(expr[[2]]),
        available_when_true(expr[[3]])
      )))
    }
    if (identical(head, as.name("||")) || identical(head, as.name("|"))) {
      return(common(
        available_when_true(expr[[2]]),
        available_when_true(expr[[3]])
      ))
    }
    character(0)
  }

  available_when_false <- function(expr) {
    if (!is.call(expr)) return(character(0))

    head <- expr[[1]]
    name <- call_name(expr)
    if (identical(head, as.name("(")) || identical(name, "isTRUE")) {
      return(available_when_false(expr[[2]]))
    }
    if (identical(name, "isFALSE")) {
      return(available_when_true(expr[[2]]))
    }
    if (identical(head, as.name("!"))) {
      return(available_when_true(expr[[2]]))
    }
    if (identical(head, as.name("&&")) || identical(head, as.name("&"))) {
      return(common(
        available_when_false(expr[[2]]),
        available_when_false(expr[[3]])
      ))
    }
    if (identical(head, as.name("||")) || identical(head, as.name("|"))) {
      return(unique(c(
        available_when_false(expr[[2]]),
        available_when_false(expr[[3]])
      )))
    }
    character(0)
  }

  is_terminal <- function(expr) {
    if (!is.call(expr)) return(FALSE)
    head <- expr[[1]]
    if (identical(head, as.name("{"))) {
      return(length(expr) >= 2 && is_terminal(expr[[length(expr)]]))
    }
    if (identical(head, as.name("if"))) {
      return(length(expr) >= 4 &&
             is_terminal(expr[[3]]) && is_terminal(expr[[4]]))
    }
    name <- call_name(expr)
    identical(name, "return") || identical(name, "stop") ||
      (is.call(head) && identical(head[[1]], as.name("::")) &&
       as.character(head[[2]]) == "jmvcore" &&
       as.character(head[[3]]) == "reject")
  }

  continuation_guards <- function(expr) {
    if (!is.call(expr) || !identical(expr[[1]], as.name("if"))) {
      return(character(0))
    }
    true_terminal <- is_terminal(expr[[3]])
    false_terminal <- length(expr) >= 4 && is_terminal(expr[[4]])
    if (true_terminal && !false_terminal) {
      return(available_when_false(expr[[2]]))
    }
    if (false_terminal && !true_terminal) {
      return(available_when_true(expr[[2]]))
    }
    character(0)
  }

  record_package <- function(pkg, active_guards) {
    if (length(pkg) != 1 || is.na(pkg) || !nzchar(pkg)) return(invisible(NULL))
    if (pkg %in% active_guards) guarded <<- c(guarded, pkg)
    else required <<- c(required, pkg)
    invisible(NULL)
  }

  walk <- NULL
  walk_condition <- function(expr, active_guards) {
    if (!is.call(expr)) return(invisible(NULL))
    head <- expr[[1]]
    if (identical(head, as.name("&&"))) {
      walk(expr[[2]], active_guards)
      walk(expr[[3]], unique(c(
        active_guards,
        available_when_true(expr[[2]])
      )))
      return(invisible(NULL))
    }
    if (identical(head, as.name("||"))) {
      walk(expr[[2]], active_guards)
      walk(expr[[3]], unique(c(
        active_guards,
        available_when_false(expr[[2]])
      )))
      return(invisible(NULL))
    }
    walk(expr, active_guards)
  }

  walk <- function(expr, active_guards = character(0)) {
    if (!is.call(expr)) return(invisible(NULL))

    head <- expr[[1]]
    if (identical(head, as.name("::")) || identical(head, as.name(":::"))) {
      record_package(as.character(expr[[2]]), active_guards)
      return(invisible(NULL))
    }

    name <- call_name(expr)
    if (name %in% c("library", "require")) {
      record_package(literal_package_arg(expr), active_guards)
    }

    if (identical(head, as.name("if"))) {
      condition <- expr[[2]]
      walk_condition(condition, active_guards)
      walk(expr[[3]], unique(c(
        active_guards,
        available_when_true(condition)
      )))
      if (length(expr) >= 4) {
        walk(expr[[4]], unique(c(
          active_guards,
          available_when_false(condition)
        )))
      }
      return(invisible(NULL))
    }

    if (identical(head, as.name("{"))) {
      block_guards <- active_guards
      if (length(expr) >= 2) {
        for (i in 2:length(expr)) {
          walk(expr[[i]], block_guards)
          block_guards <- unique(c(
            block_guards,
            continuation_guards(expr[[i]])
          ))
        }
      }
      return(invisible(NULL))
    }

    if (identical(head, as.name("&&")) || identical(head, as.name("||"))) {
      walk_condition(expr, active_guards)
      return(invisible(NULL))
    }

    for (i in seq_along(expr)) walk(expr[[i]], active_guards)
    invisible(NULL)
  }

  r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)
  for (f in r_files) {
    parsed <- tryCatch(
      parse(f, keep.source = FALSE),
      error = function(e) {
        parse_errors <<- c(
          parse_errors,
          paste0(basename(f), ": ", conditionMessage(e))
        )
        NULL
      }
    )
    if (!is.null(parsed)) for (expr in parsed) walk(expr)
  }

  required <- unique(required)
  guarded <- setdiff(unique(guarded), required)
  list(
    required = required,
    guarded = guarded,
    used = unique(c(required, guarded)),
    parse_errors = unique(parse_errors)
  )
}

# Packages declared in a DESCRIPTION, keeping runtime requirements separate
# from optional Suggests.
get_description_dependencies <- function(desc_file) {
  empty <- list(required = character(0), optional = character(0))
  if (!file.exists(desc_file)) return(empty)
  dcf <- tryCatch(read.dcf(desc_file), error = function(e) NULL)
  if (is.null(dcf)) return(empty)

  parse_fields <- function(fields) {
    fields <- intersect(fields, colnames(dcf))
    if (length(fields) == 0) return(character(0))
    vals <- unlist(lapply(fields, function(field) dcf[1, field]))
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(character(0))
    packages <- unlist(strsplit(paste(vals, collapse = ","), ","))
    packages <- trimws(gsub("\\s*\\([^)]*\\)", "", packages))
    unique(packages[nchar(packages) > 0 & packages != "R"])
  }

  list(
    required = parse_fields(c("Imports", "Depends")),
    optional = parse_fields("Suggests")
  )
}

# Check a single module directory. Unguarded usage requires Imports/Depends;
# lexically guarded usage may be declared in Imports/Depends or Suggests.
check_module_dependencies <- function(module_dir, module_name = basename(module_dir),
                                       base_packages = get_base_packages()) {
  r_dir <- file.path(module_dir, "R")
  desc_file <- file.path(module_dir, "DESCRIPTION")

  usage <- scan_r_package_usage(r_dir)
  dependencies <- get_description_dependencies(desc_file)
  package_name <- tryCatch(
    read.dcf(desc_file)[1, "Package"],
    error = function(e) module_name
  )
  if (is.na(package_name) || !nzchar(package_name)) package_name <- module_name

  ignore <- unique(c(base_packages, package_name))
  required_missing <- setdiff(
    usage$required,
    c(dependencies$required, ignore)
  )
  optional_missing <- setdiff(
    usage$guarded,
    c(dependencies$required, dependencies$optional, ignore)
  )

  list(module = module_name,
       errors = sort(required_missing),
       warnings = sort(optional_missing),
       parse_errors = usage$parse_errors,
       used = usage$used)
}

# =============================================================================
# Test infrastructure
# -----------------------------------------------------------------------------
# The planner ships the self-contained dependency-guard test (the runtime twin of
# check_module_dependencies()) and, with copy_test_files, the umbrella's
# test-<analysis>*.R files. A tests/testthat.R runner makes them run under
# devtools::test()/R CMD check.
# =============================================================================

# Write tests/testthat.R (the standard testthat runner) if the module lacks one.
ensure_testthat_runner <- function(module_dir) {
  desc_file <- file.path(module_dir, "DESCRIPTION")
  if (!file.exists(desc_file)) return(invisible(FALSE))
  pkg_name <- tryCatch(read.dcf(desc_file)[1, "Package"], error = function(e) NA_character_)
  if (is.na(pkg_name)) return(invisible(FALSE))

  tests_dir <- file.path(module_dir, "tests")
  if (!dir.exists(tests_dir)) dir.create(tests_dir, recursive = TRUE)
  runner <- file.path(tests_dir, "testthat.R")
  if (!file.exists(runner)) {
    writeLines(c(
      "library(testthat)",
      paste0("library(", pkg_name, ")"),
      "",
      paste0("test_check(\"", pkg_name, "\")")
    ), runner)
    cat("  🧪 Generated tests/testthat.R runner for ", pkg_name, "\n", sep = "")
  }
  invisible(TRUE)
}

prune_configured_module_imports <- function(module_dir, packages) {
  packages <- unlist(packages, use.names = FALSE)
  if (length(packages) == 0L) return(invisible(character(0)))
  if (!requireNamespace("desc", quietly = TRUE))
    stop("The 'desc' package is required to prune configured module imports")

  description <- file.path(module_dir, "DESCRIPTION")
  if (!file.exists(description)) stop("DESCRIPTION not found: ", description)
  d <- desc::desc(file = description)
  deps <- d$get_deps()
  removable <- intersect(packages, deps$package[deps$type == "Imports"])
  for (package in removable) d$del_dep(package, type = "Imports")
  if (length(removable) > 0L) {
    d$write(file = description)
    cat("  \U0001F9F9 Removed unused Imports: ",
        paste(removable, collapse = ", "), "\n", sep = "")
  }
  invisible(removable)
}

# Add Imports the usage scan cannot see.
#
# The import sync detects a dependency from `pkg::` calls, so a package used only
# through `requireNamespace("pkg")` -- an optional capability guarded at run time --
# is invisible to it and never reaches the submodule DESCRIPTION. jamovi installs a
# module's Imports on first run and cannot fetch a missing package on demand, so a
# guarded runtime dependency still has to be declared, or the capability is simply
# dead for every user. (2026-09-07: ICS/ICSOutlier in outlierdetection.)
add_configured_module_imports <- function(module_dir, packages) {
  packages <- unlist(packages, use.names = FALSE)
  if (length(packages) == 0L) return(invisible(character(0)))
  if (!requireNamespace("desc", quietly = TRUE))
    stop("The 'desc' package is required to add configured module imports")

  description <- file.path(module_dir, "DESCRIPTION")
  if (!file.exists(description)) stop("DESCRIPTION not found: ", description)
  d <- desc::desc(file = description)
  deps <- d$get_deps()
  already <- deps$package[deps$type == "Imports"]
  missing <- setdiff(packages, already)
  for (package in missing) d$set_dep(package, type = "Imports")
  if (length(missing) > 0L) {
    d$write(file = description)
    cat("  \U0001F4E6 Added guarded Imports: ",
        paste(missing, collapse = ", "), "\n", sep = "")
  }
  invisible(missing)
}

# ---------------------------------------------------------------------------
# Prune orphaned analyses from a submodule's jamovi/0000.yaml
# ---------------------------------------------------------------------------
# jmvtools::prepare() MERGES into 0000.yaml rather than rebuilding it, so an
# analysis stays listed forever once written -- including after it is re-routed
# out of the submodule (menuGroup gets a T/P/D suffix and its files are pruned). The jamovi compiler then emits exports for classes that no longer
# exist and the install dies with:
#     undefined exports: clinicalscoreClass, clinicalscoreOptions, ...
# That is exactly what happened to meddecide with 7 T-routed analyses.
#
# Called before prepare(), this drops any analyses: entry that has neither a
# jamovi/<name>.a.yaml nor an R/<name>.b.R in the target module.
#
# An emptied list is written as `analyses: []`. A bare `analyses:` is YAML null,
# and the jamovi compiler then dies with "packageInfo.analyses is not iterable"
# before it can add the analyses routed to the module - on EVERY later run, since
# prepare() cannot rewrite the file it failed to read. That is how JamoviTest
# became uninstallable (2026-09-18): emptied while no analysis was routed there,
# then unbuildable once waterfall and ihcheterogeneity were.
prune_orphan_analyses <- function(module_dir) {
  zero <- file.path(module_dir, "jamovi", "0000.yaml")
  if (!file.exists(zero)) return(invisible(0L))

  lines <- readLines(zero, warn = FALSE)
  start <- which(trimws(lines) == "analyses:")
  if (length(start) != 1L) return(invisible(0L))
  after <- which(grepl("^[A-Za-z]", lines))
  end <- after[after > start]
  end <- if (length(end)) end[1] else (length(lines) + 1L)

  avail_yaml <- tolower(list.files(file.path(module_dir, "jamovi"), pattern = "\\.a\\.yaml$"))
  avail_r    <- tolower(list.files(file.path(module_dir, "R"), pattern = "\\.b\\.R$"))

  starts <- which(grepl("^  - ", lines))
  starts <- starts[starts > start & starts < end]
  if (!length(starts)) {
    # left bare by an earlier run: repair it so prepare() can read the file
    lines[start] <- sub("analyses:\\s*$", "analyses: []", lines[start])
    writeLines(lines, zero)
    cat("  \U0001F9F9 Repaired an empty analyses: list in 0000.yaml (null -> [])\n")
    return(invisible(0L))
  }
  bounds <- c(starts, end)

  keep <- lines[seq_len(start)]
  dropped <- character(0)
  for (i in seq_along(starts)) {
    block <- lines[bounds[i]:(bounds[i + 1L] - 1L)]
    nm <- sub("^\\s*name:\\s*", "", grep("^\\s*name:\\s*\\S+\\s*$", block, value = TRUE)[1])
    nm <- trimws(nm %||% "")
    # Match case-INSENSITIVELY. 0000.yaml carries the analysis name as declared
    # (kappaSizePower, enhancedROC, psychopdaROC) while the files on disk are lower
    # case (kappasizepower.a.yaml). file.exists() happens to succeed on macOS because
    # HFS+/APFS is case-insensitive, but on Linux it would return FALSE and this
    # function would delete perfectly good analyses from the module.
    has_src <- nzchar(nm) &&
      (tolower(paste0(nm, ".a.yaml")) %in% avail_yaml ||
       tolower(paste0(nm, ".b.R")) %in% avail_r)
    if (isTRUE(has_src)) keep <- c(keep, block) else dropped <- c(dropped, nm)
  }
  if (length(keep) == start) keep[start] <- sub("analyses:\\s*$", "analyses: []", keep[start])
  # end is length + 1 when analyses: is the last key; end:length would then count
  # DOWN and append a literal "NA" line (unparseable YAML).
  if (end <= length(lines)) keep <- c(keep, lines[end:length(lines)])

  if (length(dropped)) {
    writeLines(keep, zero)
    cat(sprintf("  \U0001F9F9 Pruned %d orphaned analysis %s from 0000.yaml: %s\n",
                length(dropped), if (length(dropped) == 1) "entry" else "entries",
                paste(dropped, collapse = ", ")))
  }
  invisible(length(dropped))
}

# =============================================================================
# Pipeline: apply -> build -> verify -> install (driven by _updateModules.R)
#
# Every step stop()s on failure. run_module() turns the first failure into a
# FAILED result for that module only, so nothing after the failing step runs
# (in particular: no install of a module that did not verify).
# =============================================================================

# Writes lines only when they differ, so an unchanged file keeps its bytes.
.write_if_changed <- function(lines, target) {
  if (file.exists(target) && identical(readLines(target, warn = FALSE), lines)) return(invisible(FALSE))
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, target)
  invisible(TRUE)
}

# DESCRIPTION Version/Date, CITATION.cff and jamovi/0000.yaml version/date.
set_module_version <- function(module_dir, version, date) {
  d <- desc::desc(file = file.path(module_dir, "DESCRIPTION"))
  d$set(Version = version, Date = date)
  d$write(file = file.path(module_dir, "DESCRIPTION"))
  cff <- file.path(module_dir, "CITATION.cff")
  if (file.exists(cff)) {
    l <- readLines(cff, warn = FALSE)
    l <- sub("^version:.*$", paste0('version: "', version, '"'), l)
    .write_if_changed(sub("^date-released:.*$", paste0("date-released: '", date, "'"), l), cff)
  }
  zero <- file.path(module_dir, "jamovi", "0000.yaml")
  if (file.exists(zero)) {
    l <- readLines(zero, warn = FALSE)
    l <- sub("^version:.*$", paste0("version: ", version), l)
    .write_if_changed(sub("^date:.*$", paste0("date: '", date, "'"), l), zero)
  }
  invisible(TRUE)
}

# Write the analysis version into a set of jamovi/<name>.a.yaml files.
#
# jamovi requires the analysis version to be x.y.z, so it is the first THREE components of the
# package version (1.0.82.07 -> 1.0.82). Each submodule's library-audit test asserts that every
# shipped .a.yaml agrees with that module's DESCRIPTION, and OncoPath failed it: DESCRIPTION had
# moved to 1.0.82.07 while all four .a.yaml still said 1.0.81.
#
# The caller passes ONLY the umbrella .a.yaml files that are actually copied to a submodule. The
# umbrella also carries several hundred draft/pending analyses whose versions are deliberately
# their own (129 sit at 1.0.0, 63 at 0.0.31), and a blanket rewrite would destroy that.
#
# Rewriting the umbrella SOURCE rather than each copy is what keeps the two in step: the copy is
# byte-for-byte, so one edit fixes the umbrella and every submodule at once.
set_analysis_versions <- function(a_yaml_files, version) {
  v <- paste(strsplit(version, ".", fixed = TRUE)[[1]][1:3], collapse = ".")
  changed <- character()
  for (f in unique(a_yaml_files)) {
    if (!file.exists(f)) next
    l <- readLines(f, warn = FALSE)
    hit <- grep("^version:", l)
    if (!length(hit)) next
    l[hit] <- paste0("version: '", v, "'")
    if (isTRUE(.write_if_changed(l, f))) changed <- c(changed, f)
  }
  changed
}

# The umbrella .a.yaml files that ship to at least one submodule, from a computed plan.
planned_analysis_yaml <- function(plan) {
  unique(unlist(lapply(plan$modules, function(mp) {
    if (is.null(mp$files)) return(character())
    mp$files$src[grepl("\\.a\\.yaml$", mp$files$dest)]
  }), use.names = FALSE))
}

apply_distribution_plan <- function(mp, reg, other_module_dirs = character()) {
  dir <- mp$dir
  for (p in mp$delete) {
    f <- file.path(dir, p)
    if (file.exists(f) && !file.remove(f)) stop("could not delete ", f)
  }
  for (i in seq_len(nrow(mp$files))) {
    row <- mp$files[i, ]
    target <- file.path(dir, row$dest)
    lines <- render_entry(row, mp, reg)
    if (!is.null(lines)) {
      .write_if_changed(lines, target)
    } else if (!file.exists(target) || tools::md5sum(row$src) != tools::md5sum(target)) {
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(row$src, target, overwrite = TRUE)) stop("could not copy ", row$src, " to ", target)
    }
  }
  set_module_version(dir, mp$description$version, mp$description$date)
  # roxygen re-creates Collate (complete) only while an @include ships, and never
  # removes a stale one: OncoPath once listed 12 deleted stagemigration files.
  if (length(mp$description$collate)) desc::desc_del("Collate", file = file.path(dir, "DESCRIPTION"))
  # Shipped roxygen is written in the umbrella's markdown mode. Without the field a backticked
  # `{` reaches the Rd raw and document() stops: OncoPath lacked it (2026-09-18).
  desc::desc_set(Roxygen = "list(markdown = TRUE)", file = file.path(dir, "DESCRIPTION"))
  prune_configured_module_imports(dir, mp$description$prune_imports)
  add_configured_module_imports(dir, mp$description$extra_imports)
  document_module_omv(dir, mp$name, other_module_dirs)
  .ensure_rbuildignore_omv(dir)
  ensure_testthat_runner(dir)
  cat(sprintf("  applied: %d file(s) planned, %d deleted\n", nrow(mp$files), length(mp$delete)))
  invisible(TRUE)
}

# One step in a clean child R process. jmvtools::prepare() exits 0 on a YAML
# compile error, so the log is scanned as well as the exit status.
run_child <- function(dir, code, step) {
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(sprintf('Sys.unsetenv("ELECTRON_RUN_AS_NODE"); setwd(%s); %s', deparse(dir), code))),
    stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status") %||% 0L
  # ^\w*Error also catches the jamovi compiler's "TypeError: packageInfo.analyses is not
  # iterable", which exits 0: the pipeline used to document and install a half-built module.
  if (status != 0L || any(grepl("Unable to compile|^\\w*Error|^\\s*\\^+\\s*$", out)))
    stop(step, " failed (exit ", status, "):\n", paste(utils::tail(out, 25), collapse = "\n"), call. = FALSE)
  invisible(out)
}

build_module <- function(mp) {
  dir <- mp$dir
  prune_orphan_analyses(dir)
  # library-audit 2026-09-16 OncoPath [MEDIUM] DONE: i18n_files copies the whole umbrella catalog;
  #   i18nUpdate() keeps only this module's strings (and their translations) before install builds json
  if (file.exists(file.path(dir, "jamovi", "i18n", "catalog.pot")))
    run_child(dir, "jmvtools::i18nUpdate()", "i18n update")
  run_child(dir, "jmvtools::prepare()", "prepare")
  run_child(dir, "devtools::document()", "document")
  before <- tools::md5sum(file.path(dir, "DESCRIPTION"))
  if (!isTRUE(sync_namespace_with_description(dir))) stop("NAMESPACE -> DESCRIPTION sync failed", call. = FALSE)
  if (tools::md5sum(file.path(dir, "DESCRIPTION")) != before) {
    run_child(dir, "jmvtools::prepare()", "prepare (after Imports sync)")
    run_child(dir, "devtools::document()", "document (after Imports sync)")
  }
  postprocess_module_examples(dir, mp$pkg)
  invisible(TRUE)
}

# Problems that must stop the install; returns character(0) when clean.
verify_module <- function(mp, guard_template) {
  dir <- mp$dir
  problems <- character()

  d <- file.path(dir, "DESCRIPTION")
  if (desc::desc_has_fields("Collate", file = d)) {
    collate <- desc::desc_get_collate(file = d)
    on_disk <- list.files(file.path(dir, "R"), "\\.[Rr]$")
    if (!setequal(collate, on_disk))
      problems <- c(problems, paste0("Collate does not match R/ (case-exact). Missing from R/: ",
                                     paste(setdiff(collate, on_disk), collapse = ", "), "; not in Collate: ",
                                     paste(setdiff(on_disk, collate), collapse = ", ")))
  }

  deps <- check_module_dependencies(dir, mp$name)
  if (length(deps$parse_errors)) problems <- c(problems, paste("parse errors:", paste(deps$parse_errors, collapse = "; ")))
  if (length(deps$errors)) problems <- c(problems, paste("used via pkg:: but not in Imports:", paste(deps$errors, collapse = ", ")))
  if (length(deps$warnings)) problems <- c(problems, paste("guarded use not in Suggests/Imports:", paste(deps$warnings, collapse = ", ")))

  imported <- unique(vapply(parseNamespaceFile(basename(dir), dirname(dir))$imports, function(x) x[[1]], ""))
  imports <- get_description_dependencies(d)$required
  left <- intersect(mp$description$prune_imports, c(imported, imports))
  if (length(left))
    problems <- c(problems, paste0("prune_imports still imported: ", paste(left, collapse = ", "),
                                   " -- remove its @import/@importFrom tag (usually R/zzz_imports.R)"))

  guard <- new.env()
  for (e in parse(guard_template, keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && startsWith(as.character(e[[2]]), ".dependency_guard_"))
      eval(e, guard)
  usage <- guard$.dependency_guard_symbol_use(file.path(dir, "R"))
  resolution <- guard$.dependency_guard_importable(file.path(dir, "NAMESPACE"), usage$defined)
  if (length(resolution$unexpandable))
    problems <- c(problems, paste("import(pkg) of a package that is not installed:", paste(resolution$unexpandable, collapse = ", ")))
  unresolved <- setdiff(names(usage$used), c(resolution$importable, guard$.dependency_guard_language_symbols()))
  if (length(unresolved))
    problems <- c(problems, paste0("called but not resolvable from the namespace: ",
                                   paste(vapply(unresolved, function(s) paste0(s, " (", usage$used[[s]]$file, ")"), ""), collapse = ", ")))
  problems
}

postprocess_module_examples <- function(module_dir, module_name) {
  targets <- c(list.files(file.path(module_dir, "R"), "\\.h\\.R$", full.names = TRUE),
               list.files(file.path(module_dir, "man"), "\\.Rd$", full.names = TRUE))
  for (f in targets) {
    txt <- readLines(f, warn = FALSE)
    txt <- gsub('package = "ClinicoPath"', paste0('package = "', module_name, '"'), txt, fixed = TRUE)
    txt <- gsub("package = 'ClinicoPath'", paste0("package = '", module_name, "'"), txt, fixed = TRUE)
    # parent example datasets are not shipped, so these examples must not run under --run-donttest
    .write_if_changed(gsub("\\donttest{", "\\dontrun{", txt, fixed = TRUE), f)
  }
  invisible(TRUE)
}

# jmvtools::install() returns normally when the jamovi build fails, so trust the
# .jmo on disk, not the return value.
install_module_verified <- function(module_dir, pkg) {
  jmo_of <- function() list.files(module_dir, sprintf("^%s_.*\\.jmo$", pkg), full.names = TRUE)
  before <- suppressWarnings(max(file.mtime(jmo_of()), na.rm = TRUE))
  for (vdir in list.dirs(file.path(module_dir, "build"), recursive = FALSE))
    unlink(list.files(vdir, "^00LOCK", full.names = TRUE), recursive = TRUE, force = TRUE)
  old <- setwd(module_dir)
  on.exit(setwd(old), add = TRUE)
  jmvtools::install()
  after <- jmo_of()
  if (!length(after)) stop("no .jmo produced for ", pkg, call. = FALSE)
  newest <- after[which.max(file.mtime(after))]
  if (is.finite(before) && file.mtime(newest) <= before) stop(".jmo not regenerated for ", pkg, call. = FALSE)
  cat("  built", basename(newest), "\n")
  invisible(newest)
}

# pkgdown renders every root .md and fails on dev notes with invalid YAML; hide them.
build_module_site <- function(module_dir) {
  if (!file.exists(file.path(module_dir, "_pkgdown.yml"))) return(invisible(FALSE))
  old <- setwd(module_dir)
  on.exit(setwd(old), add = TRUE)
  dev <- intersect(c("AGENTS.md", "CLAUDE.md", "GEMINI.md", "TODO.md"), list.files())
  stash <- tempfile("pkgdown-dev-")
  dir.create(stash)
  file.rename(dev, file.path(stash, dev))
  on.exit(file.rename(file.path(stash, dev), dev), add = TRUE)
  pkgdown::build_site(lazy = TRUE)
  invisible(TRUE)
}

run_module <- function(mp, reg, opts) {
  res <- list(module = mp$name, status = "OK", step = "", message = "")
  step <- "apply"
  tryCatch({
    cat("\n==", mp$name, "==\n")
    apply_distribution_plan(mp, reg, opts$other_dirs[[mp$name]])
    if (!length(mp$analyses)) {
      # e.g. JamoviTest when no menuGroup ends in T: stale files are pruned above,
      # but a jamovi module with no analyses has nothing to prepare or install.
      prune_orphan_analyses(mp$dir)
      res$status <- "EMPTY"
      cat("  EMPTY: no analyses routed here; files pruned, build and install skipped\n")
    } else if (isTRUE(opts$build)) {
      step <- "build";   build_module(mp)
      step <- "verify";  problems <- verify_module(mp, opts$guard_template)
      if (length(problems)) stop(paste(problems, collapse = "\n"), call. = FALSE)
      if (isTRUE(opts$install)) { step <- "install"; install_module_verified(mp$dir, mp$pkg) }
      if (isTRUE(opts$check)) { step <- "check"; run_child(mp$dir, 'devtools::check(error_on = "error")', "R CMD check") }
      if (isTRUE(opts$webpage)) { step <- "site"; build_module_site(mp$dir) }
    }
    if (res$status == "OK") cat("  OK\n")
  }, error = function(e) {
    res$status <<- "FAILED"
    res$step <<- step
    res$message <<- conditionMessage(e)
    cat("  FAILED at", step, ":", conditionMessage(e), "\n")
  })
  res
}

# Analyses added / updated / removed, from a diff_module() table.
.analysis_changes <- function(df) {
  a_of <- function(p) sub("\\.(b\\.R|a\\.yaml|r\\.yaml|u\\.yaml)$", "",
                          basename(p[grepl("^(R/[^/]+\\.b\\.R|jamovi/[^/]+\\.(a|r|u)\\.yaml)$", p)]))
  added <- a_of(df$path[df$action == "add" & grepl("\\.b\\.R$", df$path)])
  removed <- a_of(df$path[df$action == "delete" & grepl("\\.b\\.R$|\\.a\\.yaml$", df$path)])
  updated <- setdiff(a_of(df$path[df$action %in% c("add", "update")]), added)
  other <- df$action != "same" & grepl("^R/", df$path) & !grepl("\\.(b|h)\\.R$", df$path) & !grepl("^R/data_", df$path)
  helpers <- paste0(c(add = "+", update = "~", delete = "-")[df$action[other]], basename(df$path[other]))
  list(added = sort(unique(added)), updated = sort(unique(updated)),
       removed = sort(unique(removed)), helpers = sort(unique(helpers)))
}

print_run_summary <- function(plan, diffs, results = NULL, dry_run = FALSE) {
  cat("\n================ _updateModules summary ================\n")
  if (dry_run) cat("(dry run: nothing was written)\n")
  cat(sprintf("%-24s %-8s %-8s %8s %6s %8s %8s\n", "module", "status", "step", "analyses", "added", "updated", "removed"))
  for (m in names(plan$modules)) {
    ch <- .analysis_changes(diffs[[m]])
    r <- if (!is.null(results)) results[[m]] else list(status = if (dry_run) "planned" else "-", step = "")
    cat(sprintf("%-24s %-8s %-8s %8d %6d %8d %8d\n", m, r$status, r$step,
                length(plan$modules[[m]]$analyses), length(ch$added), length(ch$updated), length(ch$removed)))
  }
  for (m in names(plan$modules)) {
    ch <- .analysis_changes(diffs[[m]])
    if (!length(c(ch$added, ch$updated, ch$removed, ch$helpers))) next
    cat("\n", m, ":\n", sep = "")
    if (length(ch$added))   cat("  added:  ", paste(ch$added, collapse = ", "), "\n")
    if (length(ch$updated)) cat("  updated:", paste(ch$updated, collapse = ", "), "\n")
    if (length(ch$removed)) cat("  removed:", paste(ch$removed, collapse = ", "), "\n")
    if (length(ch$helpers)) cat("  other R files (+added ~updated -removed):", paste(ch$helpers, collapse = ", "), "\n")
  }
  # Categories by menuGroup suffix (see route_analyses): tests go to JamoviTest,
  # pending and drafts stay in the umbrella.
  r <- plan$routes
  bare <- trimws(sub("#.*$", "", r$menu_group))
  category <- ifelse(endsWith(bare, "T"), "tests",
              ifelse(is.na(r$module) & endsWith(bare, "P"), "pending",
              ifelse(is.na(r$module), "drafts", "production")))
  cat(sprintf("\nAnalyses by category: production %d, tests %d, pending %d, drafts %d\n",
              sum(category == "production"), sum(category == "tests"), sum(category == "pending"), sum(category == "drafts")))
  for (k in c("tests", "pending")) {
    sel <- r[category == k, ]
    cat(sprintf("  %s (%d): %s\n", k, nrow(sel),
                if (nrow(sel)) paste(sprintf("%s [%s]", sel$analysis, sel$menu_group), collapse = ", ") else "none"))
  }
  failed <- Filter(function(x) x$status == "FAILED", results %||% list())
  for (f in failed) cat(sprintf("\nFAILED %s at %s:\n%s\n", f$module, f$step, f$message))
  cat("========================================================\n")
  invisible(length(failed))
}

# Moved from _updateModules.R: .omv documentation and build-ignore rules for data assets.
# Ensure a submodule's .Rbuildignore excludes the non-R payload that lives in data/:
# .omv (jamovi assets) and .csv (raw example data). R CMD check treats a data/*.csv as a
# user-level dataset and demands documentation for it -- the main repo has ignored them
# since forever (see its .Rbuildignore), the submodules were missing the csv rule and so
# reported "Undocumented data sets" for every csv without an .rda twin.
.ensure_rbuildignore_omv <- function(module_dir) {
  rbi <- file.path(module_dir, ".Rbuildignore")
  # Built source tarballs are release artifacts, never package payload: a stale
  # one committed at the OncoPath root was swept into source builds.
  want <- c("^data/.*\\.omv$", "^inst/extdata/.*\\.omv$", "^data/.*\\.csv$",
            "^.*\\.tar\\.gz$")
  cur <- if (file.exists(rbi)) readLines(rbi, warn = FALSE) else character(0)
  add <- setdiff(want, cur)
  if (length(add) > 0) {
    writeLines(c(cur, add), rbi)
    cat("    📦 .Rbuildignore: added", length(add), "asset rule(s)\n")
  }
}

# omv documentation: ensure every .omv in a submodule's data/ is listed in its 0000.yaml
# `datasets:` (jamovi's dataset browser). prepare() preserves that section, so entries persist.
.omv_title_map <- c(
  agepyramid = "Age Pyramid", benford = "Benford Analysis", checkdata = "Data Quality Check",
  dataquality = "Data Quality", reportcat = "Categorical Variables Report",
  summarydata = "Continuous Variables Summary", treatmentResponse = "Treatment Response",
  tableone = "Table One", swimmerplot = "Swimmer Plot", waterfall = "Treatment Response Waterfall",
  # Without an entry here the fallback is tools::toTitleCase(), which produces
  # "Diagnosticmeta" / "Ihcheterogeneity" in jamovi's dataset browser.
  diagnosticmeta = "Diagnostic Test Meta-Analysis", ihcheterogeneity = "IHC Heterogeneity")

.omv_stem <- function(omv) sub("_(test|sample|example|basic|raw|longitudinal|percentage|data)([_.].*)?$", "",
                              sub("\\.omv$", "", omv))

.omv_entry <- function(omv) {
  stem <- .omv_stem(omv)
  title <- if (!is.na(.omv_title_map[stem])) unname(.omv_title_map[stem]) else tools::toTitleCase(gsub("[_-]", " ", stem))
  c(paste0("  - name: ", title), paste0("    path: ", omv),
    paste0("    description: Example dataset for the ", title, " analysis."),
    "    tags:", paste0("      - ", title))
}

# Add datasets entries for present-but-undocumented omv, SKIPPING omv owned by another module
# (documented in that module's 0000.yaml). Only appends to an EXISTING datasets: section.
document_module_omv <- function(module_dir, module_name, other_module_dirs = character(0)) {
  zero <- file.path(module_dir, "jamovi", "0000.yaml")
  data_dir <- file.path(module_dir, "data")
  if (!file.exists(zero) || !dir.exists(data_dir)) return(invisible())
  zl <- readLines(zero, warn = FALSE)
  omv_here <- basename(list.files(data_dir, pattern = "\\.omv$"))
  if (length(omv_here) == 0) return(invisible())
  path_of <- function(lines) basename(trimws(gsub(".*path:\\s*", "", grep("path:.*\\.omv", lines, value = TRUE))))
  documented <- path_of(zl)
  owned_elsewhere <- character(0)
  for (od in other_module_dirs) {
    oz <- file.path(od, "jamovi", "0000.yaml")
    if (file.exists(oz)) owned_elsewhere <- c(owned_elsewhere, path_of(readLines(oz, warn = FALSE)))
  }
  to_add <- setdiff(omv_here, unique(c(documented, owned_elsewhere)))
  if (length(to_add) == 0) return(invisible())
  ds_line <- grep("^datasets:", zl)
  if (length(ds_line) == 0) return(invisible())  # no datasets section -> don't fabricate
  ds_line <- ds_line[1]
  end <- length(zl) + 1L
  if (ds_line < length(zl)) for (i in (ds_line + 1):length(zl)) if (grepl("^[A-Za-z]", zl[i])) { end <- i; break }
  entries <- unlist(lapply(to_add, .omv_entry))
  writeLines(append(zl, entries, after = end - 1L), zero)
  cat(sprintf("    📄 documented %d omv in %s 0000.yaml datasets: %s\n",
              length(to_add), module_name, paste(to_add, collapse = ", ")))
}

