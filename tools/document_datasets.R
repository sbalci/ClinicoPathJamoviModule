# Writes roxygen documentation for every dataset in data/ that no Rd file documents.
#
#   Rscript --vanilla tools/document_datasets.R && Rscript -e 'devtools::document()'
#
# Output: R/data-datasets.R (regenerated in full on every run; hand-written dataset
# docs belong in another R/data-<topic>.R file, which then takes precedence).
# Each entry states what the data contain (built from the object itself), which
# analysis uses it (file-name prefix, else the tests that load it) and which
# data-raw/ script writes it. R CMD check needs one \alias per data object.

out_file <- "R/data-datasets.R"

# ---- what is already documented (ignoring Rd generated from out_file) ----------
rd_files <- list.files("man", "\\.Rd$", full.names = TRUE)
aliases <- unique(unlist(lapply(rd_files, function(f) {
    rd <- readLines(f, warn = FALSE, encoding = "UTF-8")
    if (any(grepl(out_file, rd[1:min(3, length(rd))], fixed = TRUE))) return(character(0))
    sub("^\\\\alias\\{(.*)\\}$", "\\1", grep("^\\\\alias\\{", rd, value = TRUE))
})))
rd_names <- tolower(sub("\\.Rd$", "", basename(rd_files)))

# ---- every data object, with the file that provides it -------------------------
# Files load in alphabetical order under LazyData, so for a name found in several
# files the LAST one is what users get; that is the version described.
data_files <- sort(list.files("data", "\\.(rda|RData|R)$", full.names = TRUE))
objects <- list()
for (f in data_files) {
    e <- new.env()
    nm <- if (grepl("\\.R$", f)) tryCatch({ sys.source(f, envir = e, chdir = TRUE); ls(e) }, error = function(err) character(0))
          else load(f, envir = e)
    for (n in nm) objects[[n]] <- list(object = n, value = get(n, envir = e),
                                       files = c(objects[[n]]$files, basename(f)))
}
todo <- objects[!names(objects) %in% aliases]
todo <- todo[order(tolower(names(todo)))]

# ---- who uses it: analysis prefix, tests, data-raw generator -------------------
analyses <- sub("\\.a\\.yaml$", "", list.files("jamovi", "\\.a\\.yaml$"))
analyses <- analyses[order(-nchar(analyses))]
owner_of <- function(stem) {
    for (a in analyses) {
        nxt <- substr(stem, nchar(a) + 1, nchar(a) + 1)
        if (startsWith(tolower(stem), tolower(a)) && (nxt == "" || nxt %in% c("_", "-", ".") || grepl("[A-Z0-9]", nxt)))
            return(a)
    }
    NA_character_
}
token_index <- function(paths) {
    idx <- new.env()
    for (p in paths) {
        tok <- unique(regmatches(txt <- paste(readLines(p, warn = FALSE), collapse = "\n"),
                                 gregexpr("[A-Za-z0-9_.]+", txt))[[1]])
        for (t in tok) assign(t, c(get0(t, idx, inherits = FALSE), p), envir = idx)
    }
    idx
}
test_files <- list.files("tests/testthat", "^test-.*\\.R$", full.names = TRUE)
test_idx <- token_index(test_files)
raw_files <- list.files("data-raw", "\\.R$", full.names = TRUE, recursive = TRUE)
raw_files <- raw_files[vapply(raw_files, function(p) any(grepl("save\\(|use_data\\(|saveRDS\\(", readLines(p, warn = FALSE))), logical(1))]
raw_idx <- token_index(raw_files)
lookup <- function(idx, keys) as.character(unique(unlist(lapply(keys, function(k) get0(k, idx, inherits = FALSE)))))

# ---- Rd-safe text ---------------------------------------------------------------
rd <- function(x) {
    x <- gsub("\\", "\\\\", x, fixed = TRUE)
    x <- gsub("%", "\\%", x, fixed = TRUE)
    x <- gsub("{", "\\{", x, fixed = TRUE)
    x <- gsub("}", "\\}", x, fixed = TRUE)
    x <- gsub("[\r\n\t]+", " ", x)
    ascii <- gsub("[\"'^`~]", "", iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = "?"))
    ifelse(is.na(ascii) | ascii == x, x, sprintf("\\enc{%s}{%s}", x, ascii))
}
num <- function(x) format(signif(x, 4), big.mark = "", scientific = FALSE, trim = TRUE)
list_values <- function(v, max = 8) {
    v <- ifelse(nchar(v) > 40, paste0(substr(v, 1, 37), "..."), v)
    shown <- paste(rd(head(v, max)), collapse = ", ")
    if (length(v) > max) paste0(shown, ", ...") else shown
}
describe_column <- function(x) {
    miss <- sum(is.na(x))
    na <- if (miss > 0) sprintf("; %d missing", miss) else ""
    cls <- class(x)[1]
    body <- if (is.factor(x)) {
        sprintf("%s with %d levels: %s", if (is.ordered(x)) "ordered factor" else "factor", nlevels(x), list_values(levels(x)))
    } else if (inherits(x, c("Date", "POSIXt"))) {
        r <- range(x, na.rm = TRUE); if (all(is.finite(r))) sprintf("%s, %s to %s", cls, format(r[1]), format(r[2])) else cls
    } else if (is.logical(x)) {
        sprintf("logical; TRUE in %d of %d", sum(x, na.rm = TRUE), length(x))
    } else if (is.numeric(x)) {
        if (all(is.na(x))) sprintf("%s; all missing", cls) else {
            r <- range(x, na.rm = TRUE); sprintf("%s, %s to %s", cls, num(r[1]), num(r[2]))
        }
    } else if (is.character(x)) {
        u <- unique(x[!is.na(x)])
        if (length(u) <= 8) sprintf("character: %s", list_values(sort(u))) else sprintf("character; %d distinct values", length(u))
    } else cls
    paste0(body, na)
}
format_of <- function(v) {
    if (is.data.frame(v)) {
        vars <- names(v); shown <- head(vars, 80)
        items <- vapply(shown, function(n) sprintf("#'   \\item{\\code{%s}}{%s}", rd(n), describe_column(v[[n]])), "")
        more <- if (length(vars) > 80) sprintf("#'   \\item{\\dots}{%d further variables}", length(vars) - 80) else NULL
        c(sprintf("#' @format A data frame (%s) with %d rows and %d variables:", rd(class(v)[1]), nrow(v), ncol(v)),
          "#' \\describe{", items, more, "#' }")
    } else if (is.matrix(v)) {
        sprintf("#' @format A %s matrix with %d rows and %d columns.", typeof(v), nrow(v), ncol(v))
    } else if (is.list(v)) {
        nm <- names(v)
        sprintf("#' @format A list of %d elements%s.", length(v), if (length(nm)) paste0(": ", list_values(nm, 12)) else "")
    } else {
        sprintf("#' @format A %s vector of length %d.", rd(class(v)[1]), length(v))
    }
}
code_list <- function(x) paste(sprintf("\\code{%s}", rd(x)), collapse = ", ")
humanize <- function(s) {
    s <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", s)
    s <- gsub("[_.]+", " ", s)
    s <- trimws(gsub("\\s+", " ", s))
    paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}

# ---- one roxygen block per object --------------------------------------------------
used_rd <- rd_names
blocks <- character(0)
for (o in todo) {
    n <- o$object
    stems <- unique(sub("\\.(rda|RData|R)$", "", o$files))
    stem <- tail(stems, 1)
    owner <- owner_of(stem)
    if (is.na(owner)) owner <- owner_of(n)
    tests <- lookup(test_idx, c(n, stems, paste0(stems, ".rda")))
    test_analyses <- unique(na.omit(vapply(sub("^test-", "", sub("\\.R$", "", basename(tests))), owner_of, "")))
    raw <- lookup(raw_idx, c(n, paste0(stems, ".rda")))
    onco <- grepl("_(tbl_)?df$", n) && grepl(sprintf("`%s`", n), paste(readLines("R/data-oncology.R", warn = FALSE), collapse = "\n"), fixed = TRUE) ||
        grepl("_(tbl_)?df$", n) && any(grepl("OncoDataSets", unlist(lapply(raw, readLines, warn = FALSE))))

    title <- if (!is.na(owner)) {
        base <- if (startsWith(tolower(n), tolower(owner))) n else stem
        rest <- trimws(gsub("[_.]+", " ", substr(base, nchar(owner) + 1, nchar(base))))
        sprintf("Example data for %s%s", owner, if (nzchar(rest)) sprintf(" (%s)", rest) else "")
    } else if (onco) sprintf("%s (OncoDataSets)", humanize(sub("_(tbl_)?df$", "", n))) else humanize(n)
    desc <- c(
        if (onco) "Cancer dataset imported from the OncoDataSets package (Caceres Rossi 2024); see \\link{oncology-datasets}."
        else if (!is.na(owner)) sprintf("Example dataset for the \\code{%s} analysis.", owner)
        else "Example dataset shipped with ClinicoPath.",
        if (length(setdiff(test_analyses, owner)))
            sprintf("Used in the tests of %s.", code_list(sort(setdiff(test_analyses, owner)))),
        if (length(stems) > 1)
            sprintf("Several data files define this name (%s), possibly with different contents; the lazy-loaded object, described here, comes from \\code{%s}.",
                    code_list(o$files), rd(tail(o$files, 1)))
        else if (stem != n)
            sprintf("Load it with \\code{data(\"%s\")}: the file name differs from the object name.", rd(stem))
    )
    rdname <- n
    if (tolower(n) %in% used_rd) { rdname <- paste0(n, "-data"); k <- 2
        while (tolower(rdname) %in% used_rd) { rdname <- paste0(n, "-data", k); k <- k + 1 } }
    used_rd <- c(used_rd, tolower(rdname))
    blocks <- c(blocks, paste(c(
        sprintf("#' %s", rd(title)), "#'",
        sprintf("#' %s", desc), "#'",
        format_of(o$value),
        if (onco) "#' @source OncoDataSets R package, \\url{https://CRAN.R-project.org/package=OncoDataSets}."
        else if (length(raw)) sprintf("#' @source Generated by %s.", code_list(sort(raw)))
        else "#' @source Simulated example data.",
        if (rdname != n) sprintf("#' @rdname %s", rdname),
        "#' @noMd",
        "#' @keywords datasets",
        sprintf("\"%s\"", n)), collapse = "\n"))
}

header <- c(
    "# Generated by tools/document_datasets.R - do not edit by hand; re-run the tool.",
    "# A dataset documented in any other R/ file is left out of this one.",
    "")
writeLines(c(header, paste(blocks, collapse = "\n\n")), out_file, useBytes = TRUE)
cat(sprintf("%s: %d datasets documented (%d data objects, %d already documented elsewhere)\n",
            out_file, length(blocks), length(objects), length(objects) - length(todo)))
