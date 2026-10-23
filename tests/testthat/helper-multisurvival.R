# The generated R wrapper requires all Level options as formal arguments, even
# though allowNone makes them optional in jamovi. Keep tests focused on analysis
# behaviour by always passing unused mappings explicitly as NULL.
.multisurvival_ns <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    for (p in c("ClinicoPath", "jsurvival")) {
      if (p %in% loadedNamespaces()) {
        cand <- asNamespace(p)
        if (exists("multisurvival", envir = cand, inherits = FALSE)) {
          cached <<- cand
          return(cached)
        }
      }
    }
    r_dir <- NULL
    for (d in c("R", "../R", "../../R", testthat::test_path("..", "..", "R"))) {
      if (file.exists(file.path(d, "multisurvival.b.R"))) {
        r_dir <- d
        break
      }
    }
    if (!is.null(r_dir)) {
      env <- new.env(parent = globalenv())
      env$. <- jmvcore::`.`
      env$Surv <- survival::Surv
      env$`%>%` <- magrittr::`%>%`
      for (f in c("00jmv.R", "utils.R", "utils-Notices.R", "utils-formula.R",
                  "utils-eventindicator.R", "utils-followup.R", "survival-competingrisks.R",
                  "multisurvival-formula.R", "multisurvival-interactions.R",
                  "multisurvival-metrics.R", "multisurvival-nomogram.R",
                  "multisurvival.h.R", "multisurvival.b.R")) {
        fp <- file.path(r_dir, f)
        if (file.exists(fp)) sys.source(fp, envir = env)
      }
      cached <<- env
      return(cached)
    }
    for (p in c("ClinicoPath", "jsurvival")) {
      if (requireNamespace(p, quietly = TRUE)) {
        cand <- asNamespace(p)
        if (exists("multisurvival", envir = cand, inherits = FALSE)) {
          cached <<- cand
          return(cached)
        }
      }
    }
    NULL
  }
})

if (!exists("multisurvival", mode = "function")) {
  .ns_init <- .multisurvival_ns()
  if (!is.null(.ns_init) && exists("multisurvival", envir = .ns_init, inherits = FALSE)) {
    multisurvival <- get("multisurvival", envir = .ns_init)
  }
}

# Preload multisurvival_*.rda fixtures when running from the source checkout
# without an installed ClinicoPath package in .libPaths().
for (.d in c("data", "../data", "../../data", testthat::test_path("..", "..", "data"))) {
  if (dir.exists(.d)) {
    for (.rda in list.files(.d, pattern = "^multisurvival.*\\.rda$", full.names = TRUE)) {
      load(.rda, envir = parent.env(environment()))
      load(.rda, envir = environment())
    }
    break
  }
}
if (!nzchar(system.file(package = "ClinicoPath"))) {
  data <- function(..., list = character(), package = NULL, lib.loc = NULL,
                   verbose = getOption("verbose"), envir = .GlobalEnv, overwrite = TRUE) {
    if (identical(package, "ClinicoPath")) {
      dots <- as.character(substitute(list(...))[-1L])
      nms <- c(dots, list)
      for (nm in nms) {
        for (d in c("data", "../data", "../../data", testthat::test_path("..", "..", "data"))) {
          fp <- file.path(d, paste0(nm, ".rda"))
          if (file.exists(fp)) {
            load(fp, envir = envir)
            break
          }
        }
      }
      return(invisible(nms))
    }
    utils::data(..., list = list, package = package, lib.loc = lib.loc,
                verbose = verbose, envir = envir, overwrite = overwrite)
  }
}

.run_multisurvival <- function(...,
                               outcomeLevel = NULL,
                               dod = NULL,
                               dooc = NULL,
                               awd = NULL,
                               awod = NULL) {
  fn <- if (exists("multisurvival", mode = "function")) multisurvival else get("multisurvival", envir = .multisurvival_ns())
  fn(
    ...,
    outcomeLevel = outcomeLevel,
    dod = dod,
    dooc = dooc,
    awd = awd,
    awod = awod
  )
}
