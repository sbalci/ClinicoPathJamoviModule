# List string literals that contain English words and are not the argument of a .() call.
#
# usage: Rscript --vanilla tools/i18n_scan_literals.R R/<analysis>.b.R [R/utils-<topic>.R ...]
#
# Why this exists beside release_gate.py's check_untranslated_prose: that check counts runs of
# four or more words, so it reads 0 on a file whose English is short labels spliced between
# translated parts ("<br> Time basis: ", " person-", plot axis titles, table row labels). The
# 2026-11-23 jsurvival audit found such text in all nine analyses on a tree the gate called clean.
# This uses R's parser, so a comment or a string inside .() is never a hit.
#
# It is a list to READ, not a gate: a hit can be a code key (an estimand identifier, a method name
# compared with identical()/startsWith()), a label that is also written to the data sheet, or
# generated R code. Each line prints file:line, the innermost enclosing call, and the literal.
# Mark intentional English with `# i18n-exempt: <why>` on that line or the line above.
files <- commandArgs(TRUE)
skip_calls <- c(".", "switch", "identical", "grepl", "gsub", "sub", "regexpr", "gregexpr", "startsWith", "endsWith",
  "match.arg", "inherits", "requireNamespace", "library", "getOption", "Sys.getenv", "setNote", "addColumn", "addRow",
  "setRow", "addHtmlMessage", "addNotice", "stop", "warning", "message", "reject", "format", "sprintf", "paste", "paste0",
  "c", "list", "ifelse", "if", "%in%", "==", "!=", "<-", "=", "[[", "$", "[")
for (f in files) {
  pd <- utils::getParseData(parse(f, keep.source = TRUE), includeText = TRUE)
  strs <- pd[pd$token == "STR_CONST", ]
  parent_of <- setNames(pd$parent, pd$id)
  call_fun <- function(id) {      # name of the function of the innermost enclosing call
    p <- parent_of[[as.character(id)]]
    while (!is.na(p) && p != 0) {
      kids <- pd[pd$parent == p, ]
      if (nrow(kids) && kids$token[1] == "expr") {
        first <- pd[pd$parent == kids$id[1] & pd$token %in% c("SYMBOL_FUNCTION_CALL"), ]
        if (nrow(first)) return(first$text[1])
      }
      p <- parent_of[[as.character(p)]]
    }
    NA_character_
  }
  inside_dot <- function(id) {
    p <- parent_of[[as.character(id)]]
    while (!is.na(p) && p != 0) {
      kids <- pd[pd$parent == p, ]
      if (nrow(kids) && kids$token[1] == "expr") {
        first <- pd[pd$parent == kids$id[1] & pd$token == "SYMBOL_FUNCTION_CALL", ]
        if (nrow(first) && first$text[1] == ".") return(TRUE)
      }
      p <- parent_of[[as.character(p)]]
    }
    FALSE
  }
  src <- readLines(f, warn = FALSE)
  for (i in seq_len(nrow(strs))) {
    txt <- strs$text[i]; body <- substr(txt, 2, nchar(txt) - 1)
    plain <- gsub("<[^>]*>", " ", body)
    plain <- gsub("%[-0-9.]*[sdfg]|\\{[A-Za-z0-9]*\\}|\\\\[nt]", " ", plain)
    words <- regmatches(plain, gregexpr("[A-Za-z]{3,}", plain))[[1]]
    if (length(words) < 2 && !grepl("^[^A-Za-z]*[A-Z][a-z]{3,}[^A-Za-z]*:?[^A-Za-z]*$", plain)) next
    if (grepl("^[a-z_.]+$|^[A-Za-z0-9_.$]+$|style=|rgba\\(|::|^#|\\$|^[a-z]+_[a-z_]+$|\\.b\\.R|^https?:", body)) next
    if (grepl("i18n-exempt", src[strs$line1[i]], fixed = TRUE) || (strs$line1[i] > 1 && grepl("i18n-exempt", src[strs$line1[i] - 1], fixed = TRUE))) next
    if (inside_dot(strs$id[i])) next
    fn <- call_fun(strs$id[i])
    cat(sprintf("%s:%d [%s] %s\n", basename(f), strs$line1[i], fn, substr(txt, 1, 110)))
  }
}
