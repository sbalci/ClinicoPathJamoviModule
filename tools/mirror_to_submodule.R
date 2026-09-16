# Mirror an umbrella fix into ONE generated sibling submodule, so it can be verified in the
# environment the jamovi library reviewer audits -- without a full `Rscript _updateModules.R`.
#
# Usage:  Rscript --vanilla tools/mirror_to_submodule.R --module <Module> [--regen] [paths...]
#         Rscript --vanilla tools/mirror_to_submodule.R --route <paths...>   (read-only: where each analysis ships)
#         paths are umbrella-relative, e.g. R/waterfall.b.R jamovi/waterfall.r.yaml
#         --dir <path> targets a clone/worktree of the sibling instead of its config directory
#
# Each path prints one line:
#   COPIED   copied with the updater's own function (same bytes a regeneration writes)
#   SKIP     umbrella-only, or the analysis is dev/test-routed (ships nowhere)
#   MANUAL   needs a full _updateModules.R run (new analysis, data, i18n catalogs)
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
  cfg <- yaml::read_yaml(file.path(U, "_updateModules_config.yaml"))
  mc <- if (length(M)) cfg$modules[[M]]
  if (!route_only && (!length(M) || is.null(mc$directory))) stop("--module must name a module in _updateModules_config.yaml")
  S <- if (!route_only) normalizePath(if (length(dir_arg)) dir_arg else mc$directory)
  main_repo_dir <- U                                   # free variable of copy_refs_yaml()

  # Reuse the updater's copy functions. _updateModules.R cannot be sourced (it setwd()s and
  # runs the build), so only the needed definitions are evaluated from its parse tree.
  sys.source(file.path(U, "_updateModules_utils.R"), envir = environment())
  want <- c("copy_module_files", "copy_jamovi_assets", "collect_used_refs",
            "trim_refs_file", "copy_refs_yaml", "postprocess_module_examples")
  for (e in parse(file.path(U, "_updateModules.R"), keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]])[1] %in% want) eval(e)
  stopifnot(all(vapply(want, exists, NA, envir = environment(), inherits = FALSE)))

  # Non-WIP routing, copied from _updateModules.R (the `*_a_yaml_files` blocks near L1674-1855).
  route <- list(
    jjstatsplot = "menuGroup: JJStatsPlot$",
    meddecide = "menuGroup: meddecide$",
    jsurvival = "menuGroup: Survival$",
    ClinicoPathDescriptives = cfg$modules$ClinicoPathDescriptives$menuGroup_pattern %||%
      "menuGroup: Exploration$|menuGroup: OncoPathology$",
    OncoPath = cfg$modules$OncoPath$menuGroup_pattern %||% "menuGroup: OncoPath$")
  ships_to <- function(a) {
    l <- readLines(file.path(U, "jamovi", paste0(a, ".a.yaml")), warn = FALSE)
    hit <- names(route)[vapply(names(route), function(m) any(grepl(route[[m]], l)) ||
      any(grepl(paste0("menuGroup:[[:space:]]*Power[[:space:]]+#", m, "[[:space:]]*$"), l)), NA)]
    if (length(hit) == 1L) hit else NA_character_
  }
  analyses <- sub("\\.a\\.yaml$", "", list.files(file.path(U, "jamovi"), "\\.a\\.yaml$"))
  analysis_of <- function(base) {                      # longest analysis name owning this file
    own <- analyses[startsWith(base, paste0(analyses, ".")) | startsWith(base, paste0(analyses, "-")) |
                    startsWith(base, paste0(analyses, "_"))]
    if (length(own)) own[which.max(nchar(own))] else NA_character_
  }

  if (route_only) {                                    # read-only: no snapshot, no copy
    for (p in paths) {
      a <- analysis_of(basename(p))
      to <- if (!is.na(a)) ships_to(a)
      cat(sprintf("%-36s %s\n", p, if (is.na(a)) "not an analysis file"
                  else if (is.na(to)) paste(a, "-> ships nowhere (dev/test menuGroup)") else paste(a, "->", to)))
    }
    quit(save = "no")
  }

  # Snapshot first: a shared helper (r_symbol_files, config) regenerates EVERY file of its spec,
  # so it can carry unrelated umbrella drift. `git stash create` writes a dangling commit of the
  # dirty tree and touches no ref or file. Restore (working tree only, index untouched):
  #   git -C <S> restore --source=<sha> --worktree -- <path>
  git <- function(...) suppressWarnings(system2("git", c("-C", shQuote(S), ...), stdout = TRUE, stderr = TRUE))
  snap <- git("stash", "create")
  snap <- if (length(snap) && nzchar(snap[1])) snap[1] else git("rev-parse", "HEAD")[1]
  cat("SNAPSHOT", snap, "(", S, ")\n")

  needs_regen <- FALSE
  say <- function(tag, p, why = "") cat(sprintf("%-8s %s%s\n", tag, p, if (nzchar(why)) paste0("  -- ", why) else ""))
  for (p in paths) {
    base <- basename(p)
    dir <- dirname(p)
    if (grepl("\\.h\\.R$|^00jmv\\.R$", base) || dir == "man" || p %in% c("NAMESPACE", "jamovi/0000.yaml")) {
      say("REFUSED", p, "generated; regenerate instead (--regen)")
    } else if (grepl("^(jamovi/i18n|inst/i18n|data)(/|$)", dir)) {
      say("MANUAL", p, "catalogs/data are distributed only by a full _updateModules.R run")
    } else if (p == "_updateModules_test_dependency_guard.R") {
      write_dependency_guard_test(S, file.path(U, p)); say("COPIED", p)
    } else if (p == "_updateModules_config.yaml") {
      prune_configured_module_r_files(S, mc$prune_r_files)
      for (f in mc$r_files) fs::file_copy(file.path(U, "R", f), file.path(S, "R", f), overwrite = TRUE)
      distribute_selected_r_symbols(S, U, mc$r_symbol_files)
      prune_configured_module_imports(S, mc$prune_imports)
      add_configured_module_imports(S, mc$extra_imports)
      say("COPIED", p, "applied this module's r_files / r_symbol_files / prune / extra_imports"); needs_regen <- TRUE
    } else if (p == "jamovi/00refs.yaml") {
      copy_refs_yaml(S, M); say("COPIED", p, "trimmed to cited keys"); needs_regen <- TRUE
    } else if (dir == "R" && base %in% mc$r_files) {
      fs::file_copy(file.path(U, p), file.path(S, p), overwrite = TRUE); say("COPIED", p, "r_files"); needs_regen <- TRUE
    } else if (dir == "R" && base %in% vapply(mc$r_symbol_files, function(s) s$source, "")) {
      distribute_selected_r_symbols(S, U, mc$r_symbol_files); say("COPIED", p, "r_symbol_files (named symbols only)"); needs_regen <- TRUE
    } else if (dir == "tests/testthat") {
      # a module's library-audit test travels even when new (the reviewer reads the sibling's tests)
      slug <- c(OncoPath = "oncopath", ClinicoPathDescriptives = "clinicopath-descriptives",
                jjstatsplot = "jjstatsplot", meddecide = "meddecide", jsurvival = "jsurvival")[[M]]
      if (file.exists(file.path(S, p)) || grepl(paste0("^test-", slug, "-.*audit\\.R$"), base)) { fs::file_copy(file.path(U, p), file.path(S, p), overwrite = TRUE); say("COPIED", p) }
      else say("SKIP", p, "sibling has no such test (copy_test_files: false); copy by hand if it must ship")
    } else if (dir %in% c("R", "jamovi", "jamovi/js", "jamovi/html") && !is.na(a <- analysis_of(base))) {
      to <- ships_to(a)
      if (is.na(to)) {
        say("SKIP", p, paste0(a, " is dev/test-routed and ships nowhere"))
      } else if (to != M) {
        say("REFUSED", p, paste0(a, " ships to ", to))
      } else if (!file.exists(file.path(S, "jamovi", paste0(a, ".a.yaml")))) {
        say("MANUAL", p, paste0(a, " is new to ", M, " (0000.yaml, data): full regeneration"))
      } else if (dir == "R") {
        copy_module_files(a, file.path(U, "R"), file.path(S, "R"), ".b.R"); say("COPIED", p, "with companions"); needs_regen <- TRUE
      } else if (dir == "jamovi") {
        ext <- sub(paste0("^", a), "", base)
        copy_module_files(a, file.path(U, "jamovi"), file.path(S, "jamovi"), ext, pkg_name = M)
        say("COPIED", p); if (ext != ".u.yaml") needs_regen <- TRUE
      } else {
        copy_jamovi_assets(a, U, S, M); say("COPIED", p)
      }
    } else {
      say("SKIP", p, "umbrella-only file")
    }
  }

  # A later regeneration would undo these sibling edits.
  d <- read.dcf(file.path(S, "DESCRIPTION"), c("Imports", "Depends"))
  imports <- trimws(sub("\\(.*", "", unlist(strsplit(d[!is.na(d)], ","))))
  zzz <- file.path(S, "R", "zzz_imports.R")
  tags <- if (file.exists(zzz)) sub("^#'\\s*@import(From)?\\s+(\\S+).*", "\\2",
                                    grep("^#'\\s*@import", readLines(zzz, warn = FALSE), value = TRUE))
  for (pkg in intersect(unique(c(imports, tags)), mc$prune_imports))
    cat("REVERT-RISK", pkg, "is imported by", M, "but listed in prune_imports\n")
  for (f in intersect(list.files(file.path(S, "R")), mc$prune_r_files))
    cat("REVERT-RISK R/", f, " exists in ", M, " but is listed in prune_r_files\n", sep = "")
  sync_namespace_with_description(S, dry_run = TRUE)

  if (regen) {
    Sys.unsetenv("ELECTRON_RUN_AS_NODE")               # jmvtools::prepare() fails under VS Code's node
    prune_orphan_analyses(S)
    for (step in c("jmvtools::prepare()", "devtools::document()")) {
      out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
        c("--vanilla", "-e", shQuote(sprintf("setwd('%s'); %s", S, step))), stdout = TRUE, stderr = TRUE))
      cat(tail(out, 6), sep = "\n")
      # prepare() exits 0 on a YAML compile error, so read the log too
      if (!is.null(attr(out, "status")) || any(grepl("Unable to compile|^Error|^\\s*\\^+\\s*$", out)))
        stop("REGEN FAILED at ", step, ". Run tools/check_uyaml_duplicate_names.py in ", S,
             " before retrying (a failed prepare() still injects .u.yaml controls).")
    }
    postprocess_module_examples(S, M)
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
