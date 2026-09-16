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

# `base` is the ONLY package that may be used without a DESCRIPTION entry.
# grDevices/grid/stats/utils/methods/graphics are base-PRIORITY but NOT implicit:
# R CMD check reports "'::' or ':::' import not declared from: 'grDevices'" for
# them. Exempting every base-priority package here is why this guard stayed green
# while ClinicoPathDescriptives shipped grDevices::adjustcolor() undeclared.
.dependency_guard_base_packages <- function() {
  "base"
}

.dependency_guard_root <- function() {
  candidates <- c(
    tryCatch(testthat::test_path("..", ".."), error = function(e) NA_character_),
    file.path(getwd(), "..", "..")
  )
  for (candidate in candidates) {
    if (!is.na(candidate) && dir.exists(file.path(candidate, "R")) &&
        file.exists(file.path(candidate, "DESCRIPTION"))) {
      return(normalizePath(candidate))
    }
  }
  NA_character_
}

testthat::test_that("runtime dependencies are declared at the correct strength", {
  root <- .dependency_guard_root()
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


# ---------------------------------------------------------------------------
# Bare-symbol resolution.
#
# Added after the 2026-09-16 OncoPath audit [CRITICAL]: `%>%` was used 114 times
# in OncoPath with no `importFrom` anywhere, so `waterfall` could not run inside
# jamovi at all and `swimmerplot`'s summary tables failed. The scan above only
# recognises `pkg::` calls and library()/require(); a bare infix operator is
# neither shape, so it was invisible to it. devtools::load_all(), and any
# interactive session that has attached dplyr, hide the failure -- R CMD check
# reports it only as a NOTE ("no visible global function definition"), so the
# package still installs and the break surfaces in a user's session.
#
# Verified empirically before this was written: a function whose environment
# cannot see `%>%` dies with `could not find function "%>%"`, while the same
# setup with `:=` inside dplyr::mutate() returns normally -- tidy-eval quotes
# `:=` and never looks it up. `:=` is therefore allowed below: it costs an
# R CMD check NOTE, not a failed analysis.
.dependency_guard_symbol_use <- function(r_dir) {
  used <- list()
  defined <- character(0)

  walk <- function(expr, file, line) {
    if (!is.call(expr)) return(invisible(NULL))
    srcref <- attr(expr, "srcref")
    if (inherits(srcref, "srcref")) line <- as.integer(srcref)[1]

    head <- expr[[1]]

    if ((identical(head, as.name("<-")) || identical(head, as.name("=")) ||
         identical(head, as.name("<<-"))) && length(expr) >= 2) {
      target <- expr[[2]]
      if (is.symbol(target)) defined <<- c(defined, as.character(target))
      if (is.character(target) && length(target) == 1) defined <<- c(defined, target)
    }

    # formals are locals; a parameter called as fun() is not a dependency
    if (identical(head, as.name("function")) && length(expr) >= 2 &&
        !is.null(names(expr[[2]]))) {
      defined <<- c(defined, names(expr[[2]]))
    }

    # pkg::fn(), pkg:::fn(), obj$method() and obj@slot() need no import
    if (identical(head, as.name("::")) || identical(head, as.name(":::")) ||
        identical(head, as.name("$")) || identical(head, as.name("@"))) {
      for (i in seq_along(expr)[-1]) {
        if (is.call(expr[[i]])) walk(expr[[i]], file, line)
      }
      return(invisible(NULL))
    }

    if (is.symbol(head)) {
      name <- as.character(head)
      previous <- used[[name]]
      if (is.null(previous)) {
        used[[name]] <<- list(file = basename(file), line = NA_integer_, uses = 1L)
      } else {
        previous$uses <- previous$uses + 1L
        used[[name]] <<- previous
      }
    }

    for (i in seq_along(expr)) {
      if (i == 1 && is.symbol(head)) next
      walk(expr[[i]], file, line)
    }
    invisible(NULL)
  }

  for (file in list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)) {
    parsed <- tryCatch(parse(file, keep.source = TRUE), error = function(e) NULL)
    if (is.null(parsed)) next
    for (expr in parsed) walk(expr, file, NA_integer_)

    # Fill in the exact line each newly seen symbol first appears on. Walking
    # srcrefs cannot do this: an infix operator nested inside a 2,000-line R6
    # class would report the line the class starts on.
    parse_data <- tryCatch(utils::getParseData(parsed), error = function(e) NULL)
    if (is.null(parse_data) || !nrow(parse_data)) next
    tokens <- parse_data[parse_data$token %in%
      c("SPECIAL", "SYMBOL_FUNCTION_CALL", "SYMBOL", "LEFT_ASSIGN"), ]
    for (name in names(used)) {
      entry <- used[[name]]
      if (!identical(entry$file, basename(file)) || !is.na(entry$line)) next
      lines <- tokens$line1[tokens$text == name]
      if (length(lines)) {
        entry$line <- min(lines)
        # plain <- : this runs in the function body, where `used` is local.
        # `<<-` here would skip past it and look in the global environment.
        used[[name]] <- entry
      }
    }
  }

  list(used = used, defined = unique(defined))
}

# Everything callable without a pkg:: prefix: the package's own definitions, the
# packages attached in every R session, and every name the NAMESPACE imports --
# expanding import(pkg) into that package's complete export list.
.dependency_guard_importable <- function(namespace_path, defined) {
  always_attached <- c("base", "stats", "utils", "graphics", "grDevices",
                       "methods", "datasets")
  exports_of <- function(pkg) {
    tryCatch(getNamespaceExports(pkg), error = function(e) character(0))
  }

  importable <- unlist(lapply(always_attached, exports_of), use.names = FALSE)
  unexpandable <- character(0)

  directives <- tryCatch(
    parse(namespace_path, keep.source = FALSE),
    error = function(e) list()
  )
  for (directive in directives) {
    if (!is.call(directive)) next
    verb <- as.character(directive[[1]])
    args <- as.list(directive[-1])
    if (verb == "import") {
      for (arg in args) {
        pkg <- if (is.symbol(arg)) as.character(arg) else
               if (is.character(arg)) arg else NULL
        if (is.null(pkg) || identical(pkg, "except")) next
        exported <- exports_of(pkg)
        if (length(exported) == 0) unexpandable <- c(unexpandable, pkg)
        importable <- c(importable, exported)
      }
    } else if (verb == "importFrom" && length(args) >= 2) {
      for (arg in args[-1]) {
        name <- if (is.symbol(arg)) as.character(arg) else
                if (is.character(arg)) arg else NULL
        if (!is.null(name)) importable <- c(importable, name)
      }
    }
  }

  list(importable = unique(c(importable, defined)),
       unexpandable = unique(unexpandable))
}

# Language constructs parse as calls but never resolve through the namespace.
# `:=` is here because tidy-eval quotes it -- see the note above.
.dependency_guard_language_symbols <- function() {
  c("if", "for", "while", "repeat", "function", "return", "break", "next",
    "{", "(", "<-", "<<-", "=", "~", "?", "@", "$", "[", "[[", "::", ":::",
    ":=",
    # Supplied by the data mask inside dplyr/ggplot2 verbs, so -- like `:=` --
    # they are never looked up in the package namespace.
    "n", "desc", "across", "cur_group", "cur_group_id", "cur_column",
    "after_stat", "after_scale", "stage", "vars")
  # Deliberately NOT here: formula specials such as survival's strata(),
  # cluster(), frailty(), tt(), pspline() and mgcv's s()/te()/ti(). Those ARE
  # resolved through the namespace when the model function evaluates the
  # formula, so a missing importFrom for them is a real break, not noise.
}

testthat::test_that("bare symbols used as functions resolve from the package namespace", {
  root <- .dependency_guard_root()
  testthat::skip_if(
    is.na(root),
    "package source tree not available in the installed test context"
  )

  usage <- .dependency_guard_symbol_use(file.path(root, "R"))
  resolution <- .dependency_guard_importable(
    file.path(root, "NAMESPACE"),
    usage$defined
  )

  testthat::expect_equal(
    resolution$unexpandable,
    character(0),
    info = paste0(
      "import(pkg) could not be expanded because the package is not installed: ",
      paste(resolution$unexpandable, collapse = ", ")
    )
  )

  unresolved <- setdiff(
    names(usage$used),
    c(resolution$importable, .dependency_guard_language_symbols())
  )
  locations <- vapply(unresolved, function(symbol) {
    entry <- usage$used[[symbol]]
    paste0(symbol, " (", entry$file, ":", entry$line, ", ", entry$uses, " uses)")
  }, character(1))

  testthat::expect_equal(
    sort(unresolved),
    character(0),
    info = paste0(
      "Called but not resolvable from the installed namespace. Add an ",
      "importFrom to R/zzz_imports.R and the package to Imports: ",
      paste(sort(locations), collapse = ", ")
    )
  )
})
