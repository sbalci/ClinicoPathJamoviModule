# Mirror an umbrella fix into ONE generated sibling submodule, so it can be verified in the
# environment the jamovi library reviewer audits -- without a full `Rscript _updateModules.R`.
#
# Usage:  Rscript --vanilla tools/mirror_to_submodule.R --module <Module> [--regen] [paths...]
#         Rscript --vanilla tools/mirror_to_submodule.R --route <paths...>   (read-only: where each analysis ships)
#         paths are umbrella-relative, e.g. R/waterfall.b.R jamovi/waterfall.r.yaml
#         --dir <path> targets a clone/worktree of the sibling instead of its config directory
#
# Each path prints one line:
#   COPIED   written exactly as a full _updateModules.R run would write it (same planner, same renderer)
#   SKIP     not shipped to this module (umbrella-only, or not used by its analyses)
#   MANUAL   needs a full _updateModules.R run (analysis new to the module)
#   REFUSED  generated file, or the analysis ships to a different module
# then REVERT-RISK lines (a later regeneration would undo sibling edits) and REGEN advice.
# --regen runs prepare() + document() in the sibling (child Rscript --vanilla each).
# Never commits and never runs _updateModules.R. Used by the library-audit skill.
local({
  argv <- commandArgs(TRUE)
  U <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))), ".."))
  M <- argv[which(argv == "--module") + 1]
  regen <- "--regen" %in% argv
  route_only <- "--route" %in% argv
  dir_arg <- if ("--dir" %in% argv) argv[which(argv == "--dir") + 1]
  paths <- setdiff(argv, c("--module", M, "--regen", "--dir", dir_arg, "--route"))

  sys.source(file.path(U, "_updateModules_utils.R"), envir = environment())
  sys.source(file.path(U, "_updateModules_plan.R"), envir = environment())
  reg <- read_registry(file.path(U, "_updateModules_config.yaml"), if (length(M)) M else character())
  reg$umbrella_root <- U
  routes <- route_analyses(U, reg$modules, unlist(reg$config$umbrella_only_suffixes %||% c("D", "P")))
  analysis_of <- function(base) {                      # longest analysis name owning this file
    own <- routes$analysis[startsWith(base, paste0(routes$analysis, ".")) | startsWith(base, paste0(routes$analysis, "-"))]
    if (length(own)) own[which.max(nchar(own))] else NA_character_
  }

  if (route_only) {                                    # read-only: no snapshot, no copy
    for (p in paths) {
      a <- analysis_of(basename(p))
      to <- if (!is.na(a)) routes$module[routes$analysis == a]
      cat(sprintf("%-36s %s\n", p, if (is.na(a)) "not an analysis file"
                  else if (is.na(to)) paste0(a, " -> umbrella only (menuGroup ", routes$menu_group[routes$analysis == a], ")")
                  else paste(a, "->", to)))
    }
    quit(save = "no")
  }

  if (!length(M) || is.null(reg$modules[[M]])) stop("--module must name a module in _updateModules_config.yaml")
  if (length(dir_arg)) reg$modules[[M]]$directory <- normalizePath(dir_arg)
  S <- reg$modules[[M]]$directory
  plan <- compute_distribution_plan(reg)
  mp <- plan$modules[[M]]
  if (length(mp$errors)) stop("plan errors for ", M, ":\n  ", paste(mp$errors, collapse = "\n  "))

  # Snapshot first: `git stash create` writes a dangling commit of the dirty tree and
  # touches no ref or file. Restore (working tree only, index untouched):
  #   git -C <S> restore --source=<sha> --worktree -- <path>
  git <- function(...) suppressWarnings(system2("git", c("-C", shQuote(S), ...), stdout = TRUE, stderr = TRUE))
  snap <- git("stash", "create")
  snap <- if (length(snap) && nzchar(snap[1])) snap[1] else git("rev-parse", "HEAD")[1]
  cat("SNAPSHOT", snap, "(", S, ")\n")

  needs_regen <- FALSE
  say <- function(tag, p, why = "") cat(sprintf("%-8s %s%s\n", tag, p, if (nzchar(why)) paste0("  -- ", why) else ""))
  write_row <- function(i) {
    row <- mp$files[i, ]
    target <- file.path(S, row$dest)
    lines <- render_entry(row, mp, reg)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (is.null(lines)) file.copy(row$src, target, overwrite = TRUE) else writeLines(lines, target)
  }
  for (p in paths) {
    base <- basename(p)
    rows <- which(normalizePath(mp$files$src, mustWork = FALSE) == normalizePath(file.path(U, p), mustWork = FALSE))
    a <- analysis_of(base)
    if (grepl("\\.h\\.R$|^00jmv\\.R$", base) || dirname(p) == "man" || p %in% c("NAMESPACE", "jamovi/0000.yaml")) {
      say("REFUSED", p, "generated; regenerate instead (--regen)")
    } else if (p == "_updateModules_config.yaml") {
      prune_configured_module_imports(S, mp$description$prune_imports)
      add_configured_module_imports(S, mp$description$extra_imports)
      say("COPIED", p, "applied this module's prune_imports / extra_imports"); needs_regen <- TRUE
    } else if (length(rows)) {
      if (!is.na(a) && a %in% mp$analyses && !file.exists(file.path(S, "jamovi", paste0(a, ".a.yaml")))) {
        say("MANUAL", p, paste0(a, " is new to ", M, " (0000.yaml, data): full regeneration"))
      } else {
        for (i in rows) write_row(i)
        say("COPIED", p, paste(unique(mp$files$why[rows]), collapse = "; "))
        if (!grepl("\\.u\\.yaml$|^tests/", p)) needs_regen <- TRUE
      }
    } else if (dirname(p) == "tests/testthat") {
      # a module's library-audit test travels even when new (the reviewer reads the sibling's tests)
      slug <- c(OncoPath = "oncopath", ClinicoPathDescriptives = "clinicopath-descriptives",
                jjstatsplot = "jjstatsplot", meddecide = "meddecide", jsurvival = "jsurvival")[[M]]
      if (file.exists(file.path(S, p)) || grepl(paste0("^test-", slug, "-.*audit\\.R$"), base)) {
        writeLines(rename_namespace(readLines(file.path(U, p), warn = FALSE), mp$pkg), file.path(S, p)); say("COPIED", p)
      } else say("SKIP", p, "sibling has no such test (copy_test_files: false); copy by hand if it must ship")
    } else if (!is.na(a) && !is.na(to <- routes$module[routes$analysis == a]) && to != M) {
      say("REFUSED", p, paste0(a, " ships to ", to))
    } else if (grepl("^(jamovi/i18n|data)(/|$)", dirname(p))) {
      say("MANUAL", p, "not in this module's i18n_files/data_files; edit the config, then run _updateModules.R")
    } else {
      say("SKIP", p, "not shipped to this module")
    }
  }

  # A later regeneration would undo these sibling edits.
  d <- read.dcf(file.path(S, "DESCRIPTION"), c("Imports", "Depends"))
  imports <- trimws(sub("\\(.*", "", unlist(strsplit(d[!is.na(d)], ","))))
  zzz <- file.path(S, "R", "zzz_imports.R")
  tags <- if (file.exists(zzz)) sub("^#'\\s*@import(From)?\\s+(\\S+).*", "\\2",
                                    grep("^#'\\s*@import", readLines(zzz, warn = FALSE), value = TRUE))
  for (pkg in intersect(unique(c(imports, tags)), mp$description$prune_imports))
    cat("REVERT-RISK", pkg, "is imported by", M, "but listed in prune_imports\n")
  for (f in intersect(mp$delete, file.path("R", list.files(file.path(S, "R")))))
    cat("REVERT-RISK", f, "exists in", M, "but a regeneration deletes it (no shipped analysis uses it)\n")
  sync_namespace_with_description(S, dry_run = TRUE)

  if (regen) {
    Sys.unsetenv("ELECTRON_RUN_AS_NODE")               # jmvtools::prepare() fails under VS Code's node
    prune_orphan_analyses(S)
    for (step in c("jmvtools::prepare()", "devtools::document()")) {
      out <- tryCatch(run_child(S, step, step), error = function(e) {
        stop("REGEN FAILED at ", step, ". Run tools/check_uyaml_duplicate_names.py in ", S,
             " before retrying (a failed prepare() still injects .u.yaml controls).\n", conditionMessage(e))
      })
      cat(utils::tail(out, 6), sep = "\n")
    }
    postprocess_module_examples(S, mp$pkg)
    # prepare() deletes the compiled inst/i18n/*.json catalogs (seen on jsurvival); this flow never
    # rebuilds them, so put back whatever the snapshot had rather than ship a module without them.
    gone <- git("diff", "--name-only", "--diff-filter=D", snap, "--", "inst/i18n")
    if (length(gone) && nzchar(gone[1])) {
      git("restore", paste0("--source=", snap), "--worktree", "--", gone)
      cat("RESTORED", gone, "(deleted by prepare())\n")
    }
    cat("REGEN done: review `git -C", S, "diff --stat <snapshot sha>`\n")
  } else if (needs_regen) {
    cat("REGEN needed: rerun with --module", M, if (length(dir_arg)) paste("--dir", shQuote(S)), "--regen\n")
  }
  changed <- git("diff", "--stat", snap)
  cat("CHANGED since", snap, "-- anything you did not intend is drift; restore it from the snapshot:\n")
  cat(if (length(changed)) changed else "  (nothing)", sep = "\n")
})
