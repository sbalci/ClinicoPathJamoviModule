# Unit tests for _updateModules_plan.R (routing, helper resolution, pruning).
# Fixture umbrellas are built in tempdirs; nothing real is read or written.

plan_path <- testthat::test_path("..", "..", "_updateModules_plan.R")
plan_env <- new.env(parent = globalenv())
if (file.exists(plan_path)) sys.source(plan_path, envir = plan_env)

skip_if_plan_missing <- function() {
  testthat::skip_if_not(file.exists(plan_path),
                        "module updater planner is excluded from the installed package")
}

# files: named list of relative path -> lines
write_tree <- function(root, files) {
  for (p in names(files)) {
    dir.create(dirname(file.path(root, p)), recursive = TRUE, showWarnings = FALSE)
    writeLines(files[[p]], file.path(root, p))
  }
  root
}

analysis_files <- function(name, group, b = "x <- 1", u = "name: x") {
  setNames(list(b, c(paste0("name: ", name), paste0("menuGroup: ", group)), "items: []", u),
           c(paste0("R/", name, ".b.R"), paste0("jamovi/", name, c(".a.yaml", ".r.yaml", ".u.yaml"))))
}

fixture_registry <- function(umbrella, modules, extra = list()) {
  cfg <- c(list(new_version = "9.9.9", new_date = "2026-01-01",
                global = list(base_repo_dir = umbrella), modules = modules), extra)
  for (m in names(modules)) cfg[[m]] <- TRUE
  path <- file.path(umbrella, "_updateModules_config.yaml")
  yaml::write_yaml(cfg, path)
  plan_env$read_registry(path)
}

module_dir <- function(pkg) {
  d <- tempfile(paste0("mod-", pkg, "-"))
  write_tree(d, list(DESCRIPTION = c(paste0("Package: ", pkg), "Version: 0.0.1")))
}

base_umbrella <- function(extra = list()) {
  u <- tempfile("umbrella-")
  write_tree(u, c(list("jamovi/00refs.yaml" = c("---", "refs:", "    ClinicoPathJamoviModule:", "        type: software", "..."),
                       "_updateModules_test_dependency_guard.R" = "# guard"), extra))
}

testthat::test_that("routing: production, tests (T incl. DT/PT), pending (P) and drafts (D)", {
  skip_if_plan_missing()
  u <- base_umbrella(c(analysis_files("aa", "Survival"),
                       analysis_files("bb", "Power #meddecide"),
                       analysis_files("cc", "SurvivalD"),
                       analysis_files("dd", "ClinicoPathP"),
                       analysis_files("ee", "SurvivalT"),
                       analysis_files("ff", "ClinicoPathDT"),
                       analysis_files("gg", "SurvivalPT")))
  mods <- list(jsurvival = list(directory = "x", menu_groups = "Survival"),
               meddecide = list(directory = "x", menu_groups = c("meddecide", "Power #meddecide")),
               JamoviTest = list(directory = "x", menu_group_suffix = "T"))
  r <- plan_env$route_analyses(u, mods, umbrella_only_suffixes = c("D", "P"))
  got <- setNames(r$module, r$analysis)
  testthat::expect_identical(unname(got[c("aa", "bb", "ee", "ff", "gg")]),
                             c("jsurvival", "meddecide", "JamoviTest", "JamoviTest", "JamoviTest"))
  testthat::expect_true(all(is.na(got[c("cc", "dd")])))
  testthat::expect_length(attr(r, "errors"), 0)
})

testthat::test_that("routing: unknown group and missing menuGroup are errors", {
  skip_if_plan_missing()
  u <- base_umbrella(c(analysis_files("aa", "Surviva1"),
                       list("jamovi/bb.a.yaml" = "name: bb")))
  r <- plan_env$route_analyses(u, list(jsurvival = list(directory = "x", menu_groups = "Survival")))
  errs <- attr(r, "errors")
  testthat::expect_true(any(grepl("aa.a.yaml: menuGroup 'Surviva1' matches no module and no suffix", errs, fixed = TRUE)))
  testthat::expect_true(any(grepl("bb.a.yaml: needs exactly one top-level", errs, fixed = TRUE)))
})

testthat::test_that("routing: trailing spaces, CRLF, quotes and #comment are tolerated; nested/commented keys are not", {
  skip_if_plan_missing()
  u <- tempfile("umbrella-")
  dir.create(file.path(u, "jamovi"), recursive = TRUE)
  writeBin(charToRaw("name: aa\r\nmenuGroup: Survival  \r\n"), file.path(u, "jamovi", "aa.a.yaml"))
  writeLines(c("name: bb", "menuGroup: \"Survival\""), file.path(u, "jamovi", "bb.a.yaml"))
  writeLines(c("name: cc", "menuGroup: PowerT #meddecide"), file.path(u, "jamovi", "cc.a.yaml"))  # tests suffix
  writeLines(c("name: dd", "# menuGroup: Survival", "menuGroup: SurvivalD",
               "description:", "  menuGroup: Survival"), file.path(u, "jamovi", "dd.a.yaml"))
  mods <- list(jsurvival = list(directory = "x", menu_groups = "Survival"),
               JamoviTest = list(directory = "x", menu_group_suffix = "T"))
  r <- plan_env$route_analyses(u, mods)
  got <- setNames(r$module, r$analysis)
  testthat::expect_identical(unname(got[c("aa", "bb", "cc")]), c("jsurvival", "jsurvival", "JamoviTest"))
  testthat::expect_true(is.na(got[["dd"]]))
  testthat::expect_length(attr(r, "errors"), 0)
})

testthat::test_that("registry: a group listed in two modules is rejected", {
  skip_if_plan_missing()
  u <- base_umbrella()
  path <- file.path(u, "cfg.yaml")
  yaml::write_yaml(list(modules = list(a = list(directory = "x", menu_groups = "G"),
                                       b = list(directory = "y", menu_groups = "G"))), path)
  testthat::expect_error(plan_env$read_registry(path), "more than one module: G")
})

testthat::test_that("resolver: follows helper -> helper chains, infix, constants, R6 inherit and string lookups", {
  skip_if_plan_missing()
  u <- base_umbrella(list(
    "R/aa.b.R" = c("aaClass <- R6::R6Class('aaClass', inherit = PartTwo, private = list(",
                   "  .run = function() { x <- a %or% b; y <- LEVELS; f <- get0('byname')",
                   "    lapply(1:2, byvalue); self$options$notme; private$.alsonotme(); other::pkgfn() }))"),
    "R/aa-parts.R" = c("PartOne <- R6::R6Class('PartOne')", "PartTwo <- R6::R6Class('PartTwo', inherit = PartOne)"),
    "R/utils.R" = "`%or%` <- function(x, y) if (is.null(x)) y else deep(x)",
    "R/utils-deep.R" = "deep <- function(x) x",
    "R/utils-constants.R" = "LEVELS <- c('a', 'b')",
    "R/utils-lookup.R" = c("byname <- function() 1", "byvalue <- function(i) i"),
    "R/utils-unused.R" = c("notme <- function() 1", ".alsonotme <- function() 1", "pkgfn <- function() 1",
                           "# commented <- function() 1"),
    "R/data_docs.R" = c("#' doc", "\"LEVELS\""),
    "R/zzz.R" = ".onAttach <- function(...) NULL"))
  idx <- plan_env$index_r_sources(u)
  res <- plan_env$resolve_helpers(idx, "aa.b.R", ignore = character())
  testthat::expect_setequal(res$files, c("aa-parts.R", "utils.R", "utils-deep.R", "utils-constants.R", "utils-lookup.R"))
  testthat::expect_length(res$errors, 0)
})

testthat::test_that("resolver: duplicate helper definitions are errors and @include is followed", {
  skip_if_plan_missing()
  u <- base_umbrella(list(
    "R/aa.b.R" = c("#' @include aa-extra.R", "x <- function() dup()"),
    "R/aa-extra.R" = "unrelated <- 1",
    "R/one.R" = "dup <- function() 1",
    "R/two.R" = "dup <- function() 2"))
  res <- plan_env$resolve_helpers(plan_env$index_r_sources(u), "aa.b.R", ignore = character())
  testthat::expect_true("aa-extra.R" %in% res$files)
  testthat::expect_true(any(grepl("`dup` is defined in several helper files: one.R, two.R", res$errors, fixed = TRUE)))
})

testthat::test_that("plan: ships routed analyses, referenced JS and helpers; prunes only what the updater owns", {
  skip_if_plan_missing()
  u <- base_umbrella(c(
    analysis_files("survival", "Survival", b = "f <- function() .h()", u = c("events: ./survival.events::onLoad")),
    analysis_files("survivalPower", "SurvivalP"),
    list("R/utils-h.R" = ".h <- function() 1",
         "jamovi/js/survival.events.js" = "//", "jamovi/js/survivalPower.events.js" = "//")))
  d <- module_dir("jsurvival")
  write_tree(d, list(
    "R/survival.h.R" = "# gen", "R/gone.b.R" = "x", "R/gone.h.R" = "# gen", "R/stale_helper.R" = "x",
    "R/zzz_imports.R" = "# hand", "R/data.R" = "# hand", "R/jsurvival-package.R" = "# hand",
    "R/jsurvival-data.R" = "# hand", "R/oldanalysis_data-data.R" = "\"stale\"",
    "jamovi/gone.a.yaml" = "x", "jamovi/js/survivalPower.events.js" = "//",
    "jamovi/i18n/tr.po" = "# own catalog", "data/old.rda" = "x", "data/example.csv" = "x", "data/example.omv" = "x"))
  reg <- fixture_registry(u, list(jsurvival = list(directory = d, menu_groups = "Survival")))
  plan <- plan_env$compute_distribution_plan(reg)
  testthat::expect_length(plan$errors, 0)
  mp <- plan$modules$jsurvival
  testthat::expect_setequal(mp$analyses, "survival")
  testthat::expect_true(all(c("R/survival.b.R", "R/utils-h.R", "jamovi/js/survival.events.js",
                              "jamovi/00refs.yaml", "tests/testthat/test-zzz-dependency-declaration.R") %in% mp$files$dest))
  testthat::expect_setequal(mp$delete, c("R/gone.b.R", "R/gone.h.R", "R/stale_helper.R", "jamovi/gone.a.yaml",
                                         "jamovi/js/survivalPower.events.js", "data/old.rda",
                                         "R/oldanalysis_data-data.R"))
})

testthat::test_that("plan: a module whose last analysis left has every managed file deleted", {
  skip_if_plan_missing()
  u <- base_umbrella(analysis_files("aa", "SurvivalD"))
  d <- module_dir("jsurvival")
  write_tree(d, list("R/aa.b.R" = "x", "R/aa.h.R" = "# gen", "jamovi/aa.a.yaml" = "x", "R/zzz_imports.R" = "# hand",
                     "jamovi/0000.yaml" = c("---", "name: jsurvival", "analyses:", "  - title: A", "    name: aa", "    ns: jsurvival", "")))
  reg <- fixture_registry(u, list(jsurvival = list(directory = d, menu_groups = "Survival")))
  plan <- plan_env$compute_distribution_plan(reg)
  # paste0(character(0), ".b.R") is ".b.R": an empty module once failed with "R/.b.R does not exist"
  testthat::expect_equal(plan$errors, character(0))
  mp <- plan$modules$jsurvival
  testthat::expect_length(mp$analyses, 0)
  testthat::expect_setequal(mp$delete, c("R/aa.b.R", "R/aa.h.R", "jamovi/aa.a.yaml"))

  # Nothing to build: the module is pruned, and prepare/document/install are skipped.
  utils_path <- testthat::test_path("..", "..", "_updateModules_utils.R")
  testthat::skip_if_not(file.exists(utils_path))
  sys.source(utils_path, envir = plan_env)
  capture.output(res <- plan_env$run_module(mp, reg, list(build = TRUE, install = TRUE)))
  testthat::expect_identical(res$status, "EMPTY")
  testthat::expect_false(any(grepl("name: aa", readLines(file.path(d, "jamovi", "0000.yaml")))))
  testthat::expect_false(file.exists(file.path(d, "R", "aa.b.R")))
  testthat::expect_true(file.exists(file.path(d, "R", "zzz_imports.R")))
})

testthat::test_that("rendering: namespace rename, trimmed refs and generated dataset docs", {
  skip_if_plan_missing()
  files <- analysis_files("aa", "Survival", b = c("#' data(histo, package = \"ClinicoPath\")", "ClinicoPath::helper()"))
  files[["jamovi/aa.r.yaml"]] <- c("refs:", "    - used", "#   refs: commented")
  u <- base_umbrella(files)
  write_tree(u, list("jamovi/00refs.yaml" = c("---", "refs:", "    used:", "        x: 1", "    commented:", "        x: 2",
                                              "    ClinicoPathJamoviModule:", "        x: 3", "...")))
  histo <- data.frame(a = 1:2, `b%` = c("x", "y"), check.names = FALSE)
  dir.create(file.path(u, "data"))
  save(histo, file = file.path(u, "data", "histo.rda"))
  d <- module_dir("jsurvival")
  reg <- fixture_registry(u, list(jsurvival = list(directory = d, menu_groups = "Survival", data_files = "histo.rda")))
  mp <- plan_env$compute_distribution_plan(reg)$modules$jsurvival
  row <- function(dest) mp$files[mp$files$dest == dest, ]
  b <- plan_env$render_entry(row("R/aa.b.R"), mp, reg)
  testthat::expect_identical(b, c("#' data(histo, package = \"jsurvival\")", "jsurvival::helper()"))
  refs <- plan_env$render_entry(row("jamovi/00refs.yaml"), mp, reg)
  testthat::expect_true(any(grepl("^    used:", refs)) && any(grepl("^    ClinicoPathJamoviModule:", refs)))
  testthat::expect_false(any(grepl("^    commented:", refs)))
  doc <- plan_env$render_entry(row("R/data_histo.R"), mp, reg)
  testthat::expect_null(names(doc))
  testthat::expect_true("#'   \\item{b\\%}{character variable}" %in% doc)
  testthat::expect_identical(utils::tail(doc, 2), c("\"histo\"", ""))
})

testthat::test_that("plan: .omv example files are never copied; the submodule owns them", {
  skip_if_plan_missing()
  u <- base_umbrella(c(analysis_files("aa", "Survival"),
                       list("data-raw/non-rda/listed.omv" = "umbrella", "data/extra.omv" = "umbrella")))
  d <- module_dir("jsurvival")
  write_tree(d, list("jamovi/0000.yaml" = c("datasets:", "  - name: Listed", "    path: listed.omv",
                                            "  - name: Gone", "    path: gone.omv"),
                     "data/listed.omv" = "example analysis saved in the submodule"))
  reg <- fixture_registry(u, list(jsurvival = list(directory = d, menu_groups = "Survival", data_files = "extra.omv")))
  mp <- plan_env$compute_distribution_plan(reg)$modules$jsurvival
  testthat::expect_false(any(grepl("\\.omv$", c(mp$files$dest, mp$delete))))
  testthat::expect_true(any(grepl("data_files.*extra\\.omv", mp$warnings)))
  testthat::expect_true(any(grepl("missing from data/: gone\\.omv", mp$warnings)))
  testthat::expect_false(any(grepl("listed\\.omv", mp$warnings)))
})

testthat::test_that("apply: writes and prunes the plan, drops a stale Collate, enables roxygen markdown, and a second plan is a no-op", {
  skip_if_plan_missing()
  utils_path <- testthat::test_path("..", "..", "_updateModules_utils.R")
  testthat::skip_if_not(file.exists(utils_path))
  testthat::skip_if_not_installed("desc")
  sys.source(utils_path, envir = plan_env)
  u <- base_umbrella(c(analysis_files("aa", "Survival", b = "f <- function() .h()"),
                       list("R/utils-h.R" = ".h <- function() 1")))
  d <- module_dir("jsurvival")
  write_tree(d, list("R/gone.b.R" = "x", "R/zzz_imports.R" = "# hand",
                     DESCRIPTION = c("Package: jsurvival", "Version: 0.0.1", "Collate:", "    'gone.b.R'")))
  reg <- fixture_registry(u, list(jsurvival = list(directory = d, menu_groups = "Survival")))
  mp <- plan_env$compute_distribution_plan(reg)$modules$jsurvival
  capture.output(plan_env$apply_distribution_plan(mp, reg))
  testthat::expect_true(all(file.exists(file.path(d, c("R/aa.b.R", "R/utils-h.R", "jamovi/aa.u.yaml", "tests/testthat.R")))))
  testthat::expect_false(file.exists(file.path(d, "R/gone.b.R")))
  testthat::expect_true(file.exists(file.path(d, "R/zzz_imports.R")))
  testthat::expect_identical(unname(desc::desc_get_version(file.path(d, "DESCRIPTION"))), package_version("9.9.9"))
  testthat::expect_false(desc::desc_has_fields("Collate", file = file.path(d, "DESCRIPTION")))
  testthat::expect_identical(unname(desc::desc_get("Roxygen", file = file.path(d, "DESCRIPTION"))), "list(markdown = TRUE)")
  again <- plan_env$compute_distribution_plan(reg)$modules$jsurvival
  df <- plan_env$diff_module(again, reg)
  testthat::expect_true(all(df$action == "same"))
  changes <- plan_env$.analysis_changes(plan_env$diff_module(mp, reg))
  testthat::expect_length(changes$added, 0)
})
