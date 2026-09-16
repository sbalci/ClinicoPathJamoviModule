# Installed-namespace smoke check for a generated submodule (OncoPath, jsurvival, ...).
#
# Usage:  Rscript --vanilla tools/submodule_smoke.R <module_dir> [<module_dir> ...]
# Exit:   0 pass | 1 findings | 2 install or namespace-load failure | 3 refused (not --vanilla)
#
# Why: code verified in the umbrella can break in a submodule, whose DESCRIPTION,
# NAMESPACE and R/zzz_imports.R differ. The 2026-09-16 OncoPath audit [CRITICAL]:
# `%>%` had no importFrom, so `waterfall` could not run, yet every local check
# passed because ~/.Rprofile attaches magrittr and load_all() puts everything in
# scope. This installs the module into a temp library and checks the INSTALLED
# namespace, so always run it with --vanilla.
#
#   UNDECLARED  NAMESPACE imports a package DESCRIPTION does not declare
#   INSTALL/LOAD FAIL  stale Collate, undefined export, importFrom of a non-export, ...
#   UNRESOLVED  a function called inside a namespace function or R6 method that
#               cannot be found from the namespace (same lookup R6 methods use at run time)
#
# Complements tests/testthat/test-zzz-dependency-declaration.R (the source-level guard,
# which also sees formula specials and pkg:: declarations). Deliberately not done: running
# each analysis on example data (needs a per-analysis option mapping).
local({
  # A profile-attached package or global function would make a missing import look resolved:
  # ~/.Rprofile attaches magrittr in every directory without its own .Rprofile (all siblings).
  extra <- setdiff(search(), c(".GlobalEnv", "Autoloads", paste0("package:",
    c("base", "stats", "graphics", "grDevices", "utils", "datasets", "methods"))))
  if (length(extra) || length(ls(globalenv(), all.names = TRUE))) {
    cat("REFUSED: not a clean session (", paste(c(extra, ls(globalenv(), all.names = TRUE)),
        collapse = ", "), "). Run with Rscript --vanilla.\n")
    quit(status = 3, save = "no")
  }
  here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)))
  # eval() only defines the allowlist function parsed from our own guard file (never runs code from input)
  for (e in parse(file.path(here, "..", "_updateModules_test_dependency_guard.R"), keep.source = FALSE))
    if (is.call(e) && identical(e[[2]], as.name(".dependency_guard_language_symbols"))) eval(e)
  allow <- .dependency_guard_language_symbols()   # one allowlist, shared with the source guard
  status <- 0L
  for (dir in normalizePath(commandArgs(TRUE))) {
    pkg <- read.dcf(file.path(dir, "DESCRIPTION"), "Package")[[1]]
    cat("==", pkg, dir, "\n")
    n <- 0L
    d <- read.dcf(file.path(dir, "DESCRIPTION"), c("Imports", "Depends"))
    declared <- trimws(sub("\\(.*", "", unlist(strsplit(d[!is.na(d)], ","))))
    used <- unique(vapply(parseNamespaceFile(basename(dir), dirname(dir))$imports,
                          function(x) x[[1]], ""))
    for (p in setdiff(used, c(declared, "base"))) {
      cat("UNDECLARED", p, "\n")
      n <- n + 1L
    }
    lib <- tempfile("smoke-lib-")
    dir.create(lib)
    log <- suppressWarnings(system2(
      file.path(R.home("bin"), "R"),
      c("CMD", "INSTALL", "--no-docs", "--no-multiarch", "--no-byte-compile", "--no-test-load",
        paste0("--library=", shQuote(lib)), shQuote(dir)),
      stdout = TRUE, stderr = TRUE, env = "R_PROFILE_USER=/dev/null"))
    ns <- if (is.null(attr(log, "status")))
      tryCatch(loadNamespace(pkg, lib.loc = lib),
               error = function(e) { log <<- conditionMessage(e); NULL })
    if (is.null(ns)) {
      cat("INSTALL/LOAD FAIL", tail(log, 15), sep = "\n")
      cat(sprintf("RESULT %s INSTALL/LOAD FAIL (%d findings before install)\n", pkg, n))
      status <- 2L
      unlink(lib, recursive = TRUE)
      next
    }
    funs <- list()
    for (nm in ls(ns, all.names = TRUE)) {
      o <- get(nm, ns)
      if (inherits(o, "R6ClassGenerator")) {
        m <- c(o$public_methods, o$private_methods, o$active)
        m <- m[vapply(m, is.function, NA)]
        if (length(m)) funs[paste0(nm, "$", names(m))] <- m
      } else if (is.function(o)) {
        funs[[nm]] <- o
      }
    }
    miss <- list()
    for (f in names(funs))
      for (s in setdiff(codetools::findGlobals(funs[[f]], merge = FALSE)$functions, allow))
        if (!exists(s, envir = ns, mode = "function", inherits = TRUE)) miss[[s]] <- c(miss[[s]], f)
    for (s in names(miss)) {
      per <- table(sub("\\$.*", "", miss[[s]]))
      cat(sprintf("UNRESOLVED %s in %d function(s): %s\n", s, length(miss[[s]]),
                  paste0(names(per), " (", per, ")", collapse = ", ")))
    }
    n <- n + length(miss)
    cat(sprintf("RESULT %s %s (%d findings, %d functions scanned)\n",
                pkg, if (n) "FAIL" else "PASS", n, length(funs)))
    if (n && !status) status <- 1L
    unlink(lib, recursive = TRUE)
  }
  quit(status = status, save = "no")
})
