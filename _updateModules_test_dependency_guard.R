# Regression guard for the "undeclared pkg:: dependency" defect class.
# ---------------------------------------------------------------------------
# GENERATED FILE -- do not edit here. Source of truth:
#   ClinicoPathJamoviModule/_updateModules_test_dependency_guard.R
# It is copied verbatim into each submodule's tests/testthat/ by _updateModules.R.
#
# Why this exists: a submodule that calls `pkg::fun()` without declaring `pkg`
# in its DESCRIPTION Imports crashes on a clean jamovi install, because jamovi
# installs only a package's Imports. This has shipped real bugs (cmprsk in
# jsurvival -> competing-risks crash; vcd/lme4 in meddecide -> silent-wrong /
# dead agreement stats; haven in jjstatsplot; viridis in ClinicoPathDescriptives).
#
# The build-time check in _updateModules.R is the primary guard; this test is the
# runtime twin, so the defect also surfaces in the submodule's own `devtools::test()`.
# It self-discovers the package source, so it needs no fixtures or namespace refs.
# Under R CMD check (installed package, no parseable source R/) it skips cleanly.

testthat::test_that("every pkg:: dependency is declared in DESCRIPTION Imports", {
  # Locate this package's source tree (works under devtools::test / pkgload).
  candidates <- c(
    tryCatch(testthat::test_path("..", ".."), error = function(e) NA_character_),
    file.path(getwd(), "..", "..")
  )
  root <- NA_character_
  for (cand in candidates) {
    if (!is.na(cand) && dir.exists(file.path(cand, "R")) &&
        file.exists(file.path(cand, "DESCRIPTION"))) {
      root <- normalizePath(cand)
      break
    }
  }
  testthat::skip_if(is.na(root),
                    "package source tree not available (installed / R CMD check context)")

  r_files <- list.files(file.path(root, "R"), pattern = "\\.[Rr]$", full.names = TRUE)
  testthat::skip_if(length(r_files) == 0, "no R source files found")

  # Extract real pkg:: usages via the R parser (ignores comments/strings) and the
  # set of packages named inside a requireNamespace() guard.
  used <- character(0)
  guarded <- character(0)
  for (f in r_files) {
    p <- tryCatch(parse(f, keep.source = TRUE), error = function(e) NULL)
    if (!is.null(p)) {
      pd <- tryCatch(utils::getParseData(p), error = function(e) NULL)
      if (!is.null(pd) && nrow(pd) > 0)
        used <- c(used, pd$text[pd$token == "SYMBOL_PACKAGE"])
    }
    ln <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
    g <- unlist(regmatches(
      ln, gregexpr("requireNamespace\\(\\s*[\"']([A-Za-z0-9.]+)[\"']", ln)))
    if (length(g) > 0)
      guarded <- c(guarded, sub(
        "requireNamespace\\(\\s*[\"']([A-Za-z0-9.]+)[\"'].*", "\\1", g))
  }
  used <- unique(used)
  guarded <- unique(guarded)

  # Declared dependencies (Imports + Depends + Suggests), version-stripped.
  dcf <- read.dcf(file.path(root, "DESCRIPTION"))
  pkg_name <- dcf[1, "Package"]
  flds <- intersect(c("Imports", "Depends", "Suggests"), colnames(dcf))
  declared <- unlist(strsplit(paste(dcf[1, flds], collapse = ","), ","))
  declared <- trimws(gsub("\\s*\\([^)]*\\)", "", declared))
  declared <- declared[nchar(declared) > 0 & declared != "R"]

  # Base + recommended packages ship with R and never need declaring.
  ip <- utils::installed.packages()
  prio <- ip[, "Priority"]
  base_rec <- rownames(ip)[!is.na(prio) & prio %in% c("base", "recommended")]
  ignore <- unique(c(base_rec, pkg_name))

  # Packages available transitively through a declared dependency won't crash,
  # even if not declared directly (that is an R CMD check hygiene NOTE, not a crash).
  transitive <- tryCatch(
    unique(unlist(tools::package_dependencies(
      declared, db = ip, recursive = TRUE,
      which = c("Depends", "Imports", "LinkingTo")))),
    error = function(e) character(0))

  undeclared <- setdiff(used, c(declared, ignore))
  # Genuine crash risk = used via pkg::, unguarded, undeclared, not transitively present.
  offenders <- setdiff(setdiff(undeclared, guarded), transitive)

  # Non-failing WARNING pass: packages the hard test structurally excuses but the jamovi
  # library reviewer still expects declared. Two blind spots of the crash-only rule above:
  #   (1) R "recommended" packages (MASS, boot, cluster, survival, ...) are in `ignore`, so a
  #       genuine `MASS::ginv` / `boot::boot` use never trips `offenders` even when undeclared.
  #   (2) `requireNamespace`-guarded packages are subtracted, so a guarded optional dep
  #       (e.g. BaylorEdPsych) is invisible too.
  # These don't crash a clean install, but they SHOULD be declared (Imports if required for the
  # feature, Suggests if graceful-degradation). Surface them without failing the build.
  recommended <- rownames(ip)[!is.na(prio) & prio %in% "recommended"]
  soft <- setdiff(intersect(used, c(recommended, guarded)),
                  c(declared, pkg_name, transitive))
  if (length(soft) > 0)
    message(
      "[dependency-guard WARNING] Used via pkg:: but NOT declared in DESCRIPTION ",
      "(guarded and/or R-recommended, so not a clean-install crash, but the jamovi library ",
      "reviewer flags these): ", paste(sort(soft), collapse = ", "),
      ". Declare them in Imports (required) or Suggests (optional/guarded).")

  testthat::expect_true(
    length(offenders) == 0,
    info = paste0(
      "Packages used via pkg:: in R/ but NOT declared in DESCRIPTION Imports ",
      "(will crash on a clean jamovi install): ",
      paste(sort(offenders), collapse = ", "),
      ". Add them to this package's DESCRIPTION Imports."))
})
