# Regression guard for undeclared runtime dependencies.
# ---------------------------------------------------------------------------
# GENERATED FILE -- do not edit the copies in submodules. Source of truth:
#   ClinicoPathJamoviModule/_updateModules_test_dependency_guard.R
# It is copied into each submodule's tests/testthat/ by _updateModules.R.

.dependency_guard_scan <- function(r_dir) {
  required <- character(0)
  guarded <- character(0)
  parse_errors <- character(0)

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
  for (file in r_files) {
    parsed <- tryCatch(
      parse(file, keep.source = FALSE),
      error = function(e) {
        parse_errors <<- c(
          parse_errors,
          paste0(basename(file), ": ", conditionMessage(e))
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
    parse_errors = unique(parse_errors)
  )
}

.dependency_guard_description <- function(path) {
  dcf <- read.dcf(path)
  parse_fields <- function(fields) {
    fields <- intersect(fields, colnames(dcf))
    if (length(fields) == 0) return(character(0))
    values <- unlist(lapply(fields, function(field) dcf[1, field]))
    values <- values[!is.na(values)]
    packages <- unlist(strsplit(paste(values, collapse = ","), ","))
    packages <- trimws(gsub("\\s*\\([^)]*\\)", "", packages))
    unique(packages[nzchar(packages) & packages != "R"])
  }
  list(
    package = dcf[1, "Package"],
    required = parse_fields(c("Imports", "Depends")),
    optional = parse_fields("Suggests")
  )
}

.dependency_guard_base_packages <- function() {
  fallback <- c(
    "base", "compiler", "datasets", "graphics", "grDevices", "grid",
    "methods", "parallel", "splines", "stats", "stats4", "tcltk", "tools",
    "utils"
  )
  tryCatch({
    installed <- utils::installed.packages()
    priority <- installed[, "Priority"]
    unique(c(
      fallback,
      rownames(installed)[!is.na(priority) & priority == "base"]
    ))
  }, error = function(e) fallback)
}

testthat::test_that("runtime dependencies are declared at the correct strength", {
  candidates <- c(
    tryCatch(testthat::test_path("..", ".."), error = function(e) NA_character_),
    file.path(getwd(), "..", "..")
  )
  root <- NA_character_
  for (candidate in candidates) {
    if (!is.na(candidate) && dir.exists(file.path(candidate, "R")) &&
        file.exists(file.path(candidate, "DESCRIPTION"))) {
      root <- normalizePath(candidate)
      break
    }
  }
  testthat::skip_if(
    is.na(root),
    "package source tree not available in the installed test context"
  )

  usage <- .dependency_guard_scan(file.path(root, "R"))
  dependencies <- .dependency_guard_description(file.path(root, "DESCRIPTION"))
  ignored <- unique(c(
    .dependency_guard_base_packages(),
    dependencies$package
  ))

  required_missing <- setdiff(
    usage$required,
    c(dependencies$required, ignored)
  )
  optional_missing <- setdiff(
    usage$guarded,
    c(dependencies$required, dependencies$optional, ignored)
  )

  testthat::expect_equal(
    usage$parse_errors,
    character(0),
    info = paste(usage$parse_errors, collapse = "; ")
  )
  testthat::expect_equal(
    sort(required_missing),
    character(0),
    info = paste0(
      "Unguarded package use must be declared in Imports/Depends: ",
      paste(sort(required_missing), collapse = ", ")
    )
  )
  testthat::expect_equal(
    sort(optional_missing),
    character(0),
    info = paste0(
      "Guarded optional package use must be declared in Suggests or ",
      "Imports/Depends: ", paste(sort(optional_missing), collapse = ", ")
    )
  )
})
