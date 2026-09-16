# Distribution planner for _updateModules.R
#
# Pure: reads the umbrella and each submodule folder, never writes. Safe to
# sys.source() from tests and tools. The driver computes the whole plan first and
# writes nothing when the plan has errors.
#
#   reg  <- read_registry("_updateModules_config.yaml")
#   plan <- compute_distribution_plan(reg)
#   print_plan(plan)
#
# What ships to a module:
#   - every analysis whose menuGroup is listed in the module's `menu_groups`
#   - every umbrella helper file the shipped code needs, found by resolving the
#     symbols it uses (transitively). Helper file NAMES do not decide anything.
#   - JS files named by the shipped .u.yaml, data/i18n from the registry, trimmed
#     00refs.yaml, generated dataset docs, the dependency-guard test.
# What is deleted: any file in a managed set (see .plan_managed_files) that is not
# planned, except the hand-maintained files matched by .PLAN_KEEP_R.

`%||%` <- function(x, y) if (is.null(x)) y else x

# Submodule R/ files the updater never writes or deletes (hand-maintained).
.PLAN_KEEP_R <- "^(00jmv|zzz_imports|data)\\.R$|^[A-Za-z0-9.]+-(package|data)\\.R$|^data-.*\\.R$"

# Umbrella R/ files that are package infrastructure, never shipped as helpers.
.PLAN_INFRA_R <- "^(00jmv|zzz)\\.R$|-package\\.R$|^data[-_]"

# Registry -------------------------------------------------------------------

read_registry <- function(config_path, only = character()) {
  cfg <- yaml::read_yaml(config_path)
  mods <- cfg$modules %||% list()
  errors <- character()
  for (m in names(mods)) {
    x <- mods[[m]]
    if (!length(x$directory)) errors <- c(errors, paste0(m, ": `directory` is missing"))
    if (!length(x$menu_groups) && !length(x$menu_group_suffix))
      errors <- c(errors, paste0(m, ": needs `menu_groups` or `menu_group_suffix`"))
    x$name <- m
    x$enabled <- if (length(only)) m %in% only else isTRUE(cfg[[x$toggle %||% m]])
    mods[[m]] <- x
  }
  unknown <- setdiff(only, names(mods))
  if (length(unknown)) errors <- c(errors, paste("Unknown module(s):", paste(unknown, collapse = ", ")))
  groups <- unlist(lapply(mods, function(x) x$menu_groups), use.names = FALSE)
  dup <- unique(groups[duplicated(groups)])
  if (length(dup)) errors <- c(errors, paste("menu group listed in more than one module:", paste(dup, collapse = ", ")))
  suffixes <- unlist(lapply(mods, function(x) x$menu_group_suffix), use.names = FALSE)
  dup <- unique(c(suffixes[duplicated(suffixes)], intersect(suffixes, unlist(cfg$umbrella_only_suffixes))))
  if (length(dup)) errors <- c(errors, paste("menu group suffix claimed twice:", paste(dup, collapse = ", ")))
  if (length(errors)) stop("Invalid _updateModules_config.yaml:\n  ", paste(errors, collapse = "\n  "), call. = FALSE)
  list(config = cfg, modules = mods,
       umbrella_root = cfg$global$base_repo_dir %||% dirname(normalizePath(config_path)))
}

# Routing --------------------------------------------------------------------

# The value of the single top-level `menuGroup:` key, or NA.
read_menu_group <- function(a_yaml) {
  ln <- sub("\r$", "", readLines(a_yaml, warn = FALSE))
  hit <- grep("^menuGroup:", ln, value = TRUE)
  if (length(hit) != 1L) return(NA_character_)
  v <- trimws(sub("^menuGroup:", "", hit))
  v <- sub("^(['\"])(.*)\\1$", "\\2", v)
  gsub("[[:space:]]+", " ", v)
}

# Categories, by menuGroup:
#   <Group>             production -> the module listing it in `menu_groups`
#   <Group>T            tests      -> the module whose `menu_group_suffix` is "T" (JamoviTest);
#                                     includes <Group>DT / <Group>PT from the dev/test slash commands
#   <Group>P, <Group>D  pending, drafts -> umbrella only (`umbrella_only_suffixes`)
# Exact match first (`Power #meddecide` is listed literally), then the value with its
# `#comment` stripped, then suffixes. Anything else is an error, so a typo cannot
# silently drop an analysis.
route_analyses <- function(umbrella_root, modules, umbrella_only_suffixes = c("D", "P")) {
  files <- list.files(file.path(umbrella_root, "jamovi"), "\\.a\\.yaml$")
  analyses <- sub("\\.a\\.yaml$", "", files)
  groups <- vapply(file.path(umbrella_root, "jamovi", files), read_menu_group, "", USE.NAMES = FALSE)
  owner <- unlist(lapply(names(modules), function(m) {
    g <- unlist(modules[[m]]$menu_groups)
    setNames(rep(m, length(g)), g)
  }))
  suffix_owner <- unlist(lapply(names(modules), function(m) {
    s <- unlist(modules[[m]]$menu_group_suffix)
    setNames(rep(m, length(s)), s)
  }))
  module <- rep(NA_character_, length(analyses))
  errors <- character()
  for (i in seq_along(analyses)) {
    g <- groups[i]
    if (is.na(g)) {
      errors <- c(errors, paste0(files[i], ": needs exactly one top-level `menuGroup:` line"))
      next
    }
    bare <- trimws(sub("#.*$", "", g))
    by_suffix <- Filter(function(sfx) endsWith(bare, sfx), as.character(names(suffix_owner)))
    if (!is.na(owner[g])) module[i] <- owner[[g]]
    else if (!is.na(owner[bare])) module[i] <- owner[[bare]]
    else if (length(by_suffix)) module[i] <- suffix_owner[[by_suffix[1]]]
    else if (!any(endsWith(bare, umbrella_only_suffixes)))
      errors <- c(errors, sprintf("%s: menuGroup '%s' matches no module and no suffix (%s)", files[i], g,
                                  paste(c(as.character(names(suffix_owner)), umbrella_only_suffixes), collapse = ", ")))
  }
  structure(data.frame(analysis = analyses, menu_group = groups, module = module,
                       stringsAsFactors = FALSE),
            errors = errors)
}

# Source index -----------------------------------------------------------------

# Top-level definitions, descending into top-level `if` and `{` but not function bodies.
.plan_top_defs <- function(exprs) {
  out <- character()
  visit <- function(e) {
    if (!is.call(e)) return(invisible())
    h <- e[[1]]
    if ((identical(h, as.name("<-")) || identical(h, as.name("="))) && length(e) == 3L) {
      lhs <- e[[2]]
      if (is.symbol(lhs)) out <<- c(out, as.character(lhs))
      else if (is.character(lhs) && length(lhs) == 1L) out <<- c(out, lhs)
    } else if (identical(h, as.name("if")) && length(e) >= 3L) {
      for (i in 3:length(e)) visit(e[[i]])
    } else if (identical(h, as.name("{")) && length(e) >= 2L) {
      for (i in 2:length(e)) visit(e[[i]])
    }
    invisible()
  }
  for (e in exprs) visit(e)
  unique(out)
}

# One R file: its top-level definitions and every name it could look up.
#   syms    symbols and infix operators, except `x$name`, `x@name` and `pkg::name`
#   calls   the subset used in call position (for the cross-analysis check)
#   strings identifier-like string constants: exists("f"), get0("f"), do.call("f")
index_r_file <- function(path) {
  empty <- list(defs = character(), syms = character(), calls = character(),
                strings = character(), includes = character(), parse_error = NULL)
  exprs <- tryCatch(parse(path, keep.source = TRUE, encoding = "UTF-8"), error = function(e) e)
  if (inherits(exprs, "error")) return(modifyList(empty, list(parse_error = conditionMessage(exprs))))
  pd <- utils::getParseData(exprs)
  if (is.null(pd) || !nrow(pd)) return(modifyList(empty, list(defs = .plan_top_defs(exprs))))
  pd <- pd[pd$terminal, c("line1", "col1", "token", "text")]
  pd <- pd[order(pd$line1, pd$col1), ]
  prev <- c("", pd$token[-nrow(pd)])
  after_accessor <- prev %in% c("'$'", "'@'", "NS_GET", "NS_GET_INT")
  unquote <- function(x) gsub("^`|`$", "", x)
  is_sym <- pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SPECIAL") & !after_accessor
  is_call <- pd$token %in% c("SYMBOL_FUNCTION_CALL", "SPECIAL") & !after_accessor
  str <- pd$text[pd$token == "STR_CONST"]
  str <- substr(str, 2L, nchar(str) - 1L)
  str <- str[grepl("^([A-Za-z.][A-Za-z0-9._]*|%[^%]*%)$", str)]
  lines <- readLines(path, warn = FALSE)
  inc <- sub("^#'\\s*@include\\s+", "", grep("^#'\\s*@include\\s+", lines, value = TRUE))
  list(defs = .plan_top_defs(exprs),
       syms = unique(unquote(pd$text[is_sym])),
       calls = unique(unquote(pd$text[is_call])),
       strings = unique(str),
       includes = unique(unlist(strsplit(trimws(inc), "\\s+"))),
       parse_error = NULL)
}

# Every umbrella R file except generated .h.R headers and 00jmv.R.
index_r_sources <- function(umbrella_root) {
  files <- list.files(file.path(umbrella_root, "R"), "\\.[Rr]$")
  files <- files[!grepl("\\.h\\.R$", files) & files != "00jmv.R"]
  idx <- lapply(file.path(umbrella_root, "R", files), index_r_file)
  names(idx) <- files
  for (f in files) {
    idx[[f]]$is_analysis <- grepl("\\.b\\.R$", f)
    idx[[f]]$is_infra <- !idx[[f]]$is_analysis && grepl(.PLAN_INFRA_R, f)
  }
  idx
}

# Helper resolution -------------------------------------------------------------

# Transitive closure of helper files needed by `seeds` (file names in the index).
# A helper file is any non-analysis, non-infra file that defines something; it is
# needed when a seed or an already-needed helper uses one of its definitions.
resolve_helpers <- function(index, seeds,
                            ignore = tryCatch(getNamespaceExports("jmvcore"), error = function(e) character())) {
  foreign <- list()
  is_provider <- vapply(index, function(x) !x$is_analysis && !x$is_infra && length(x$defs) > 0L, NA)
  prov <- index[is_provider]
  provider_of <- split(rep(names(prov), lengths(lapply(prov, `[[`, "defs"))),
                       unlist(lapply(prov, `[[`, "defs"), use.names = FALSE))
  analysis_files <- names(index)[vapply(index, `[[`, NA, "is_analysis")]
  analysis_owner <- split(rep(analysis_files, lengths(lapply(index[analysis_files], `[[`, "defs"))),
                          unlist(lapply(index[analysis_files], `[[`, "defs"), use.names = FALSE))
  errors <- character()
  warnings <- character()
  need <- character()
  why <- character()
  queue <- seeds
  seen <- seeds
  while (length(queue)) {
    f <- queue[[1]]
    queue <- queue[-1]
    x <- index[[f]]
    if (is.null(x)) { errors <- c(errors, paste0("R/", f, " does not exist")); next }
    if (!is.null(x$parse_error)) errors <- c(errors, paste0("R/", f, ": ", x$parse_error))
    wanted <- setdiff(intersect(c(x$syms, x$strings), names(provider_of)), x$defs)
    for (nm in wanted) {
      p <- provider_of[[nm]]
      if (length(p) > 1L)
        errors <- c(errors, sprintf("`%s` is defined in several helper files: %s", nm, paste(p, collapse = ", ")))
      for (pf in setdiff(p, seen)) {
        seen <- c(seen, pf); need <- c(need, pf); queue <- c(queue, pf)
        why[[pf]] <- paste0(f, " uses ", nm)
      }
    }
    for (inc in setdiff(x$includes, seen)) {
      seen <- c(seen, inc); need <- c(need, inc); queue <- c(queue, inc)
      why[[inc]] <- paste0(f, " @include")
    }
    if (x$is_analysis) {
      shadow <- intersect(x$defs, names(provider_of))
      if (length(shadow))
        warnings <- c(warnings, sprintf("R/%s redefines helper(s) %s", f, paste(shadow, collapse = ", ")))
    }
    # A warning, not an error: the static index also sees dead top-level fallbacks
    # such as `if (!exists(".")) . <- ...`. A real miss fails the bare-symbol gate.
    for (nm in setdiff(intersect(x$calls, names(analysis_owner)), c(names(provider_of), x$defs, ignore)))
      if (!any(analysis_owner[[nm]] %in% seeds)) foreign[[nm]] <- c(foreign[[nm]], f)
  }
  for (nm in names(foreign))
    warnings <- c(warnings, sprintf("`%s` is called by %s but defined only in R/%s; move it to a helper file",
                                    nm, paste(foreign[[nm]], collapse = ", "), paste(analysis_owner[[nm]], collapse = ", R/")))
  list(files = need, why = unname(why[need]), errors = unique(errors), warnings = unique(warnings))
}

# Rendering helpers (pure: return lines) -------------------------------------------

rename_namespace <- function(lines, pkg) {
  lines <- gsub("library(ClinicoPath)", paste0("library(", pkg, ")"), lines, fixed = TRUE)
  lines <- gsub("ClinicoPath::", paste0(pkg, "::"), lines, fixed = TRUE)
  gsub("package\\s*=\\s*(['\"])ClinicoPath\\1", paste0("package = \\1", pkg, "\\1"), lines)
}

# Citation keys used by yaml files (refs: blocks and inline refs: [a, b]).
# Over-inclusive by design; a commented `# refs: key` is not a citation.
collect_used_refs <- function(files) {
  used <- character()
  for (f in files[file.exists(files)]) {
    in_refs <- FALSE
    for (line in readLines(f, warn = FALSE)) {
      if (grepl("^\\s*#", line)) next
      inl <- regmatches(line, regexec("refs:\\s*\\[([^]]*)\\]", line))[[1]]
      if (length(inl) == 2) { used <- c(used, trimws(gsub("['\"]", "", strsplit(inl[2], ",")[[1]]))); next }
      one <- regmatches(line, regexec("refs:\\s*([A-Za-z0-9._-]+)\\s*$", line))[[1]]
      if (length(one) == 2) { used <- c(used, trimws(gsub("['\"]", "", one[2]))); next }
      if (grepl("^\\s*refs:\\s*$", line)) { in_refs <- TRUE; next }
      if (in_refs) {
        item <- regmatches(line, regexec("^\\s*-\\s*(\\S+)\\s*$", line))[[1]]
        if (length(item) == 2) used <- c(used, gsub("['\"]", "", item[2]))
        else if (nzchar(trimws(line))) in_refs <- FALSE
      }
    }
  }
  unique(used[nzchar(used)])
}

# 00refs.yaml lines keeping only the cited blocks (plus the module self-reference).
trim_refs_lines <- function(lines, used_keys) {
  keep <- union(used_keys, "ClinicoPathJamoviModule")
  n <- length(lines); i <- 1L; out <- character()
  while (i <= n) {
    out <- c(out, lines[i])
    if (grepl("^refs:\\s*$", lines[i])) { i <- i + 1L; break }
    i <- i + 1L
  }
  key_re <- "^    ([A-Za-z0-9._-]+):\\s*$"
  while (i <= n) {
    m <- regmatches(lines[i], regexec(key_re, lines[i]))[[1]]
    if (length(m) == 2) {
      j <- i + 1L
      while (j <= n && !grepl(key_re, lines[j]) && !grepl("^\\.\\.\\.", lines[j])) j <- j + 1L
      if (m[2] %in% keep) out <- c(out, lines[i:(j - 1L)])
      i <- j
    } else {
      out <- c(out, lines[i]); i <- i + 1L
    }
  }
  out
}

.rd_escape <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("{", "\\{", x, fixed = TRUE)
  gsub("}", "\\}", x, fixed = TRUE)
}

# roxygen block documenting one bundled dataset (clears "undocumented data").
dataset_doc_lines <- function(objname, obj, module_name) {
  hdr <- c("# GENERATED by _updateModules.R -- do not edit. Auto-documents the bundled",
           paste0("# example dataset '", objname, "' so R CMD check does not flag undocumented data."),
           paste0("#' ", objname, ": example dataset for the ", module_name, " module"),
           "#'",
           paste0("#' Example dataset distributed with the ", module_name,
                  " jamovi module for demonstration and testing."))
  body <- if (is.data.frame(obj)) {
    c("#'", sprintf("#' @format A data frame with %d rows and %d variables:", nrow(obj), ncol(obj)),
      "#' \\describe{",
      vapply(names(obj), function(cn) sprintf("#'   \\item{%s}{%s variable}", .rd_escape(cn),
                                               .rd_escape(paste(class(obj[[cn]]), collapse = "/"))), "", USE.NAMES = FALSE),
      "#' }")
  } else {
    c("#'", sprintf("#' @format An object of class %s of length %d.",
                    .rd_escape(paste(class(obj), collapse = "/")), length(obj)))
  }
  c(hdr, body, "#' @keywords datasets internal", paste0('"', objname, '"'), "")
}

# Module plan --------------------------------------------------------------------

# Files in a submodule the updater owns (relative paths). .h.R is handled separately.
.plan_managed_files <- function(dir, copy_vignettes = FALSE) {
  ls_files <- function(sub, pattern = NULL) {
    p <- file.path(dir, sub)
    f <- list.files(p, pattern = pattern)
    f <- f[!dir.exists(file.path(p, f))]
    if (length(f)) file.path(sub, f) else character()
  }
  r <- ls_files("R", "\\.[Rr]$")
  r <- r[!grepl("\\.h\\.R$", r) & !grepl(.PLAN_KEEP_R, basename(r))]
  # Not managed, so never deleted: jamovi/i18n (jsurvival keeps its own catalogs),
  # data/*.omv and data/*.csv (hand-placed; the updater only ever wrote .rda/.RData).
  c(r,
    ls_files("jamovi", "\\.(a|r|u)\\.yaml$"),
    ls_files("jamovi/js"), ls_files("jamovi/html"),
    ls_files("data", "\\.(rda|RData)$"),
    if (copy_vignettes) ls_files("vignettes"))
}

plan_module <- function(m, routes, index, reg, ignore = character()) {
  U <- reg$umbrella_root
  cfg <- reg$config
  modes <- cfg$modes %||% list()
  dir <- m$directory
  errors <- character()
  warnings <- character()
  dest <- character(); src <- character(); kind <- character(); why <- character()
  add <- function(d, s, k, w = "") {
    dest <<- c(dest, d); src <<- c(src, s); kind <<- c(kind, k); why <<- c(why, w)
  }

  if (!dir.exists(dir) || !file.exists(file.path(dir, "DESCRIPTION")))
    return(list(name = m$name, dir = dir, errors = paste0(m$name, ": no package at ", dir)))
  pkg <- read.dcf(file.path(dir, "DESCRIPTION"), "Package")[[1]]
  analyses <- sort(routes$analysis[routes$module %in% m$name])

  # analyses
  for (a in analyses) {
    for (ext in c(".b.R", ".a.yaml", ".r.yaml", ".u.yaml")) {
      sub <- if (ext == ".b.R") "R" else "jamovi"
      s <- file.path(U, sub, paste0(a, ext))
      if (!file.exists(s)) errors <- c(errors, paste0(m$name, ": ", a, " has no ", sub, "/", a, ext))
      else add(file.path(sub, paste0(a, ext)), s, if (ext == ".r.yaml" || ext == ".u.yaml") "copy" else "text", a)
    }
    u <- file.path(U, "jamovi", paste0(a, ".u.yaml"))
    if (file.exists(u)) {
      refs <- unique(regmatches(readLines(u, warn = FALSE), gregexpr("\\./[A-Za-z0-9_.-]+::", readLines(u, warn = FALSE))))
      for (js in unique(sub("^\\./(.*)::$", "\\1", unlist(refs)))) {
        s <- file.path(U, "jamovi", "js", paste0(js, ".js"))
        if (!file.exists(s)) errors <- c(errors, paste0(m$name, ": ", a, ".u.yaml references missing jamovi/js/", js, ".js"))
        else if (!paste0("jamovi/js/", js, ".js") %in% dest) add(file.path("jamovi", "js", paste0(js, ".js")), s, "copy", a)
      }
    }
  }

  # helpers
  helpers <- resolve_helpers(index, paste0(analyses, ".b.R", recycle0 = TRUE), ignore)
  errors <- c(errors, if (length(helpers$errors)) paste0(m$name, ": ", helpers$errors))
  warnings <- c(warnings, helpers$warnings)
  for (i in seq_along(helpers$files))
    add(file.path("R", helpers$files[i]), file.path(U, "R", helpers$files[i]), "text", helpers$why[i])

  # i18n
  for (f in unlist(m$i18n_files)) {
    s <- file.path(U, "jamovi", "i18n", f)
    if (!file.exists(s)) errors <- c(errors, paste0(m$name, ": i18n catalog not found: jamovi/i18n/", f))
    else add(file.path("jamovi", "i18n", f), s, "copy", "i18n_files")
  }

  # data + generated dataset docs
  hand_doc <- function(obj) {
    candidates <- c(file.path(U, "R", c(helpers$files, paste0(analyses, ".b.R", recycle0 = TRUE))),
                    file.path(dir, "R", list.files(file.path(dir, "R"), .PLAN_KEEP_R)))
    for (f in candidates[file.exists(candidates)]) {
      l <- readLines(f, warn = FALSE)
      if (any(grepl(paste0("@name\\s+", obj, "\\b"), l)) || any(grepl(paste0("@aliases\\b.*\\b", obj, "\\b"), l)) ||
          any(grepl(paste0('^"', obj, '"\\s*$'), l))) return(TRUE)
    }
    FALSE
  }
  for (f in unlist(m$data_files)) {
    s <- file.path(U, "data", f)
    if (!file.exists(s)) { errors <- c(errors, paste0(m$name, ": data file not found: data/", f)); next }
    add(file.path("data", f), s, "copy", "data_files")
    if (grepl("\\.(rda|RData)$", f)) {
      e <- new.env()
      for (obj in load(s, envir = e))
        if (!hand_doc(obj)) add(file.path("R", paste0("data_", obj, ".R")), s, "datadoc", obj)
    }
  }
  zero <- file.path(dir, "jamovi", "0000.yaml")
  if (file.exists(zero)) {
    zl <- readLines(zero, warn = FALSE)
    for (o in unique(trimws(basename(gsub(".*path:\\s*", "", grep("path:.*\\.omv", zl, value = TRUE)))))) {
      cand <- file.path(U, c("data-raw/non-rda", "inst/extdata", "data"), o)
      hit <- cand[file.exists(cand)]
      if (length(hit)) add(file.path("data", o), hit[1], "copy", "0000.yaml datasets")
      else warnings <- c(warnings, paste0(m$name, ": omv listed in 0000.yaml datasets not found in the umbrella: ", o))
    }
  }

  # refs, guard test, optional tests and vignettes
  add("jamovi/00refs.yaml", file.path(U, "jamovi", "00refs.yaml"), "refs", "cited keys")
  add("tests/testthat/test-zzz-dependency-declaration.R", file.path(U, "_updateModules_test_dependency_guard.R"), "copy", "guard")
  if (isTRUE(modes$copy_test_files)) {
    tests <- list.files(file.path(U, "tests", "testthat"), "^(test|helper)-.*\\.R$")
    for (a in analyses)
      for (t in tests[grepl(paste0("^(test|helper)-", a, "(\\.R$|[.-])"), tests)])
        add(file.path("tests", "testthat", t), file.path(U, "tests", "testthat", t), "text", a)
  }
  if (isTRUE(modes$copy_vignettes)) {
    vf <- cfg$vignette_folders %||% list()
    ext <- vf$extensions %||% c(".qmd", ".Rmd", ".md")
    ext_re <- paste0("(", paste(gsub(".", "\\.", ext, fixed = TRUE), collapse = "|"), ")$")
    for (folder in names(vf$folder_mapping)) {
      if (!m$name %in% unlist(vf$folder_mapping[[folder]])) next
      files <- list.files(file.path(U, folder), ext_re)
      for (p in unlist(vf$exclude_patterns))
        files <- files[!grepl(if (startsWith(p, "*")) utils::glob2rx(p) else p, files)]
      for (f in files) add(file.path("vignettes", f), file.path(U, folder, f), "text", folder)
    }
  }

  files <- data.frame(dest = dest, src = src, kind = kind, why = why, stringsAsFactors = FALSE)
  dups <- unique(files$dest[duplicated(files$dest)])
  if (length(dups)) errors <- c(errors, paste0(m$name, ": planned twice: ", paste(dups, collapse = ", ")))
  keep_hit <- files$dest[dirname(files$dest) == "R" & grepl(.PLAN_KEEP_R, basename(files$dest))]
  if (length(keep_hit)) errors <- c(errors, paste0(m$name, ": would overwrite hand-maintained file(s): ", paste(keep_hit, collapse = ", ")))

  h <- list.files(file.path(dir, "R"), "\\.h\\.R$")
  orphan_h <- file.path("R", h[!sub("\\.h\\.R$", "", h) %in% analyses])
  delete <- sort(c(setdiff(.plan_managed_files(dir, isTRUE(modes$copy_vignettes)), files$dest), orphan_h))

  description <- list(pkg = pkg, version = cfg$new_version, date = cfg$new_date,
                      collate = if (desc::desc_has_fields("Collate", file = file.path(dir, "DESCRIPTION")))
                        desc::desc_get_collate(file = file.path(dir, "DESCRIPTION")) else character(),
                      prune_imports = unlist(m$prune_imports), extra_imports = unlist(m$extra_imports))

  list(name = m$name, dir = dir, pkg = pkg, analyses = analyses, files = files, delete = delete,
       description = description, errors = unique(errors), warnings = unique(warnings))
}

compute_distribution_plan <- function(reg) {
  U <- reg$umbrella_root
  routes <- route_analyses(U, reg$modules, unlist(reg$config$umbrella_only_suffixes %||% c("D", "P")))
  index <- index_r_sources(U)
  enabled <- Filter(function(m) isTRUE(m$enabled), reg$modules)
  ignore <- tryCatch(getNamespaceExports("jmvcore"), error = function(e) character())
  modules <- lapply(enabled, plan_module, routes = routes, index = index, reg = reg, ignore = ignore)
  list(routes = routes, index = index, modules = modules,
       errors = c(attr(routes, "errors"), unlist(lapply(modules, `[[`, "errors"), use.names = FALSE)))
}

# Rendering, diff and report -------------------------------------------------------

# Lines to write for a planned file, or NULL when the file is copied byte-for-byte.
render_entry <- function(row, mp, reg) {
  pkg <- mp$pkg
  switch(row$kind,
    copy = NULL,
    text = {
      l <- rename_namespace(readLines(row$src, warn = FALSE), pkg)
      if (grepl("\\.a\\.yaml$", row$dest) && isTRUE(reg$config$modes$sync_analysis_versions)) {
        v <- paste(strsplit(reg$config$new_version, ".", fixed = TRUE)[[1]][1:3], collapse = ".")
        l <- sub("^version:.*$", paste0("version: '", v, "'"), l)
      }
      l
    },
    refs = {
      yaml_src <- mp$files$src[grepl("\\.(a|r)\\.yaml$", mp$files$dest)]
      used <- collect_used_refs(c(yaml_src, file.path(mp$dir, "jamovi", "0000.yaml")))
      trim_refs_lines(readLines(row$src, warn = FALSE), used)
    },
    datadoc = {
      e <- new.env()
      load(row$src, envir = e)
      dataset_doc_lines(row$why, get(row$why, envir = e), mp$name)
    },
    stop("unknown plan entry kind: ", row$kind))
}

# add / update / delete against the submodule as it is on disk.
diff_module <- function(mp, reg) {
  action <- character(nrow(mp$files))
  for (i in seq_len(nrow(mp$files))) {
    row <- mp$files[i, ]
    target <- file.path(mp$dir, row$dest)
    if (!file.exists(target)) { action[i] <- "add"; next }
    new <- render_entry(row, mp, reg)
    same <- if (is.null(new)) unname(tools::md5sum(row$src) == tools::md5sum(target))
            else identical(new, readLines(target, warn = FALSE))
    action[i] <- if (same) "same" else "update"
  }
  rbind(data.frame(action = action, path = mp$files$dest, why = mp$files$why, stringsAsFactors = FALSE),
        data.frame(action = rep("delete", length(mp$delete)), path = mp$delete, why = rep("not planned", length(mp$delete)),
                   stringsAsFactors = FALSE))
}

# `diffs`: named list of diff_module() tables (NULL prints no file lists).
print_plan <- function(plan, reg, diffs = NULL) {
  r <- plan$routes
  cat(sprintf("\nRouting: %d analyses -> %s; %d umbrella-only\n", nrow(r),
              paste(sprintf("%s %d", names(table(r$module)), table(r$module)), collapse = ", "),
              sum(is.na(r$module))))
  for (mp in plan$modules) {
    cat("\n==", mp$name, "==", mp$dir, "\n")
    if (is.null(mp$files)) { cat("  ERROR", mp$errors, sep = "\n  "); next }
    cat(sprintf("  %d analyses; helpers: %s\n", length(mp$analyses),
                paste(basename(mp$files$dest[mp$files$kind == "text" & grepl("^R/", mp$files$dest) &
                                             !grepl("\\.b\\.R$", mp$files$dest)]), collapse = ", ")))
    hl <- mp$files[grepl("^R/", mp$files$dest) & !grepl("\\.b\\.R$", mp$files$dest) & mp$files$kind == "text", ]
    for (i in seq_len(nrow(hl))) cat(sprintf("    %-32s %s\n", basename(hl$dest[i]), hl$why[i]))
    d <- mp$description
    cat(sprintf("  DESCRIPTION: version %s, date %s%s%s%s\n", d$version, d$date,
                if (length(d$collate)) sprintf(", remove Collate (%d entries)", length(d$collate)) else "",
                if (length(d$prune_imports)) paste0(", prune Imports ", paste(d$prune_imports, collapse = " ")) else "",
                if (length(d$extra_imports)) paste0(", add Imports ", paste(d$extra_imports, collapse = " ")) else ""))
    df <- diffs[[mp$name]]
    if (!is.null(df)) {
      counts <- table(factor(df$action, c("add", "update", "delete", "same")))
      cat(sprintf("  files: %d add, %d update, %d delete, %d unchanged\n",
                  counts[["add"]], counts[["update"]], counts[["delete"]], counts[["same"]]))
      for (a in c("add", "update", "delete"))
        for (p in df$path[df$action == a]) cat(sprintf("    %-6s %s\n", a, p))
    }
    for (w in mp$warnings) cat("  WARNING", w, "\n")
    for (e in mp$errors) cat("  ERROR", e, "\n")
  }
  for (e in attr(r, "errors")) cat("ERROR", e, "\n")
  invisible(plan)
}
