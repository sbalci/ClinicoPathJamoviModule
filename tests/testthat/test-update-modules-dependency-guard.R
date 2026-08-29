dependency_guard_utils_path <- testthat::test_path(
  "..", "..", "_updateModules_utils.R"
)
dependency_guard_env <- new.env(parent = globalenv())
if (file.exists(dependency_guard_utils_path)) {
  sys.source(dependency_guard_utils_path, envir = dependency_guard_env)
}

skip_if_dependency_guard_utils_missing <- function() {
  testthat::skip_if_not(
    file.exists(dependency_guard_utils_path),
    "module updater utilities are excluded from the installed package"
  )
}

make_dependency_fixture <- function(code, imports = NULL, suggests = NULL) {
  root <- tempfile("dependency-guard-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  description <- c(
    "Package: FixtureModule",
    "Version: 0.0.1",
    "Depends: R (>= 4.1)"
  )
  if (length(imports) > 0) {
    description <- c(description, paste("Imports:", paste(imports, collapse = ", ")))
  }
  if (length(suggests) > 0) {
    description <- c(description, paste("Suggests:", paste(suggests, collapse = ", ")))
  }
  writeLines(description, file.path(root, "DESCRIPTION"))
  writeLines(code, file.path(root, "R", "fixture.R"))
  root
}

testthat::test_that("dependency scanner follows lexical requireNamespace guards", {
  skip_if_dependency_guard_utils_missing()
  root <- make_dependency_fixture(c(
    "text <- 'commentPkg::run()'",
    "# anotherCommentPkg::run()",
    "dynamic_name <- 'dynamicPkg'",
    "library(dynamic_name, character.only = TRUE)",
    "requiredPkg::run()",
    "library(attachedPkg)",
    "if (requireNamespace('branchPkg', quietly = TRUE)) branchPkg::run()",
    "after_guard <- function() {",
    "  if (!requireNamespace('afterPkg', quietly = TRUE)) return(NULL)",
    "  afterPkg::run()",
    "}",
    "else_guard <- function() {",
    "  if (isFALSE(requireNamespace('elsePkg', quietly = TRUE))) stop('missing')",
    "  elsePkg::run()",
    "}",
    "short_guard <- function() {",
    "  requireNamespace('shortPkg', quietly = TRUE) && shortPkg::run()",
    "}",
    "or_guard <- function() {",
    "  !requireNamespace('orPkg', quietly = TRUE) || orPkg::run()",
    "}",
    "if (requireNamespace('mixedPkg', quietly = TRUE)) mixedPkg::one()",
    "mixedPkg::two()"
  ))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  usage <- dependency_guard_env$scan_r_package_usage(file.path(root, "R"))

  testthat::expect_setequal(
    usage$required,
    c("requiredPkg", "attachedPkg", "mixedPkg")
  )
  testthat::expect_setequal(
    usage$guarded,
    c("branchPkg", "afterPkg", "elsePkg", "shortPkg", "orPkg")
  )
  testthat::expect_false("commentPkg" %in% usage$used)
  testthat::expect_false("anotherCommentPkg" %in% usage$used)
  testthat::expect_false("dynamic_name" %in% usage$used)
  testthat::expect_length(usage$parse_errors, 0)
})

testthat::test_that("dependency policy requires direct declarations", {
  skip_if_dependency_guard_utils_missing()
  root <- make_dependency_fixture(
    code = c(
      "MASS::ginv(matrix(1))",
      "rlang::abort('stop')",
      "suggestedOnly::run()",
      paste0(
        "if (requireNamespace('optionalPkg', quietly = TRUE)) ",
        "optionalPkg::run()"
      ),
      paste0(
        "if (requireNamespace('missingOptional', quietly = TRUE)) ",
        "missingOptional::run()"
      )
    ),
    imports = "testthat",
    suggests = c("MASS", "optionalPkg", "suggestedOnly")
  )
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  result <- dependency_guard_env$check_module_dependencies(
    root,
    module_name = "FixtureModule",
    base_packages = c("base", "stats", "utils")
  )

  testthat::expect_setequal(
    result$errors,
    c("MASS", "rlang", "suggestedOnly")
  )
  testthat::expect_equal(result$warnings, "missingOptional")
})

testthat::test_that("Imports and Suggests satisfy their respective use classes", {
  skip_if_dependency_guard_utils_missing()
  root <- make_dependency_fixture(
    code = c(
      "MASS::ginv(matrix(1))",
      "optional_feature <- function() {",
      "  if (!requireNamespace('optionalPkg', quietly = TRUE)) return(NULL)",
      "  optionalPkg::run()",
      "}"
    ),
    imports = "MASS",
    suggests = "optionalPkg"
  )
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  result <- dependency_guard_env$check_module_dependencies(
    root,
    module_name = "FixtureModule",
    base_packages = c("base", "stats", "utils")
  )

  testthat::expect_length(result$errors, 0)
  testthat::expect_length(result$warnings, 0)
  testthat::expect_length(result$parse_errors, 0)
})

testthat::test_that("aggregate dependency check fails guarded declaration gaps", {
  skip_if_dependency_guard_utils_missing()
  root <- make_dependency_fixture(
    code = paste0(
      "if (requireNamespace('missingOptional', quietly = TRUE)) ",
      "missingOptional::run()"
    )
  )
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  testthat::expect_error(
    capture.output(
      dependency_guard_env$check_all_modules_dependencies(
        list(FixtureModule = root),
        fail_on_error = TRUE
      )
    ),
    "Dependency reconciliation failed"
  )
})

testthat::test_that("dependency scanner reports source parse failures", {
  skip_if_dependency_guard_utils_missing()
  root <- make_dependency_fixture("broken <- function(")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  usage <- dependency_guard_env$scan_r_package_usage(file.path(root, "R"))

  testthat::expect_length(usage$parse_errors, 1)
  testthat::expect_match(usage$parse_errors, "fixture[.]R")
})

testthat::test_that("distributed dependency guard scanner matches updater", {
  skip_if_dependency_guard_utils_missing()
  template <- testthat::test_path(
    "..", "..", "_updateModules_test_dependency_guard.R"
  )
  testthat::skip_if_not(
    file.exists(template),
    "module updater templates are excluded from the installed package"
  )
  expressions <- parse(template, keep.source = FALSE)
  is_scan_definition <- vapply(expressions, function(expr) {
    is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name(".dependency_guard_scan"))
  }, logical(1))
  testthat::expect_equal(sum(is_scan_definition), 1)

  template_env <- new.env(parent = globalenv())
  eval(expressions[[which(is_scan_definition)]], envir = template_env)
  root <- make_dependency_fixture(c(
    "requiredPkg::run()",
    paste0(
      "if (requireNamespace('optionalPkg', quietly = TRUE)) ",
      "optionalPkg::run()"
    )
  ))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  updater_result <- dependency_guard_env$scan_r_package_usage(
    file.path(root, "R")
  )
  template_result <- template_env$.dependency_guard_scan(file.path(root, "R"))
  testthat::expect_equal(template_result$required, updater_result$required)
  testthat::expect_equal(template_result$guarded, updater_result$guarded)
  testthat::expect_equal(
    template_result$parse_errors,
    updater_result$parse_errors
  )
})

testthat::test_that("selected helper distribution writes only configured symbols", {
  skip_if_dependency_guard_utils_missing()
  root <- tempfile("selected-helper-source-")
  module <- tempfile("selected-helper-module-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  dir.create(file.path(module, "R"), recursive = TRUE)
  on.exit(unlink(c(root, module), recursive = TRUE), add = TRUE)
  writeLines(c(
    "needed <- function(x) x + 1L",
    "also_needed <- function(x) needed(x)",
    "`%or%` <- function(x, y) if (is.null(x)) y else x",
    "must_not_ship <- function() stop('unused')"
  ), file.path(root, "R", "helpers.R"))

  dependency_guard_env$distribute_selected_r_symbols(
    module, root,
    list(list(
      source = "helpers.R",
      destination = "helpers.R",
      symbols = c("needed", "also_needed", "%or%")
    ))
  )

  distributed <- paste(
    readLines(file.path(module, "R", "helpers.R"), warn = FALSE),
    collapse = "\n"
  )
  testthat::expect_match(distributed, "needed <- function", fixed = TRUE)
  testthat::expect_match(distributed, "also_needed <- function", fixed = TRUE)
  testthat::expect_match(distributed, "`%or%` <- function", fixed = TRUE)
  testthat::expect_false(grepl("must_not_ship", distributed, fixed = TRUE))
})

testthat::test_that("configured translation catalogs are distributed verbatim", {
  skip_if_dependency_guard_utils_missing()
  testthat::skip_if_not_installed("fs")
  root <- tempfile("i18n-source-")
  module <- tempfile("i18n-module-")
  dir.create(file.path(root, "jamovi", "i18n"), recursive = TRUE)
  dir.create(module, recursive = TRUE)
  on.exit(unlink(c(root, module), recursive = TRUE), add = TRUE)

  catalogs <- c("catalog.pot", "en.po", "tr.po")
  for (catalog in catalogs) {
    writeLines(paste("fixture", catalog), file.path(root, "jamovi", "i18n", catalog))
  }

  written <- dependency_guard_env$distribute_module_i18n(
    module, root, catalogs
  )

  testthat::expect_setequal(basename(written), catalogs)
  for (catalog in catalogs) {
    testthat::expect_identical(
      readLines(file.path(module, "jamovi", "i18n", catalog), warn = FALSE),
      readLines(file.path(root, "jamovi", "i18n", catalog), warn = FALSE)
    )
  }
})

testthat::test_that("translation distribution rejects missing or nested paths", {
  skip_if_dependency_guard_utils_missing()
  root <- tempfile("i18n-source-")
  module <- tempfile("i18n-module-")
  dir.create(file.path(root, "jamovi", "i18n"), recursive = TRUE)
  dir.create(module, recursive = TRUE)
  on.exit(unlink(c(root, module), recursive = TRUE), add = TRUE)

  testthat::expect_error(
    dependency_guard_env$distribute_module_i18n(module, root, "missing.po"),
    "do not exist"
  )
  testthat::expect_error(
    dependency_guard_env$distribute_module_i18n(module, root, "nested/tr.po"),
    "plain file names"
  )
})

testthat::test_that("configured pruning removes only named stale files and Imports", {
  skip_if_dependency_guard_utils_missing()
  testthat::skip_if_not_installed("desc")
  module <- tempfile("module-pruning-")
  dir.create(file.path(module, "R"), recursive = TRUE)
  on.exit(unlink(module, recursive = TRUE), add = TRUE)
  writeLines("stale <- TRUE", file.path(module, "R", "stale.R"))
  writeLines("keep <- TRUE", file.path(module, "R", "keep.R"))
  writeLines(c(
    "Package: FixtureModule",
    "Version: 0.0.1",
    "Imports: cluster, tidyr, stats"
  ), file.path(module, "DESCRIPTION"))

  removed_files <- dependency_guard_env$prune_configured_module_r_files(
    module, "stale.R"
  )
  removed_imports <- dependency_guard_env$prune_configured_module_imports(
    module, c("cluster", "tidyr")
  )

  testthat::expect_equal(basename(removed_files), "stale.R")
  testthat::expect_false(file.exists(file.path(module, "R", "stale.R")))
  testthat::expect_true(file.exists(file.path(module, "R", "keep.R")))
  testthat::expect_setequal(removed_imports, c("cluster", "tidyr"))
  remaining <- desc::desc(file.path(module, "DESCRIPTION"))$get_deps()
  testthat::expect_equal(remaining$package[remaining$type == "Imports"], "stats")
})
