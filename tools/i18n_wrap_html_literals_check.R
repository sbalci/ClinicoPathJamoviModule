# Proves a run of tools/i18n_wrap_html_literals.py changed no English output: evaluates each old
# literal and its paste0(.()) replacement with `.` as identity and compares them, ignoring only
# whitespace runs and whitespace next to a block tag (which HTML ignores too).
#   Rscript --vanilla tools/i18n_wrap_html_literals_check.R pairs.json
pairs <- jsonlite::fromJSON(commandArgs(TRUE)[1], simplifyVector = FALSE)
block <- "div|p|h[1-6]|ul|ol|li|table|thead|tbody|tfoot|tr|td|th|br|hr|details|summary|section|blockquote|pre|dl|dt|dd|caption|center|style"
norm <- function(x) {
  x <- gsub("\\s+", " ", x)
  x <- gsub(sprintf("\\s*(<\\s*/?\\s*(?:%s)\\b[^>]*>)\\s*", block), "\\1", x, perl = TRUE, ignore.case = TRUE)
  # an inline element that wraps a whole node is peeled, so the node's edge whitespace goes too
  x <- gsub("(<[A-Za-z][^>]*>)\\s+", "\\1 ", x, perl = TRUE)
  trimws(gsub("\\s+", " ", x))
}
loose <- function(x) gsub("\\s+", "", x)
env <- new.env(); assign(".", function(text) text, envir = env)
bad <- 0L; nodes <- 0L
for (p in pairs) {
  old <- eval(parse(text = p$old, keep.source = FALSE), env)
  new <- eval(parse(text = p$new, keep.source = FALSE), env)
  nodes <- nodes + lengths(regmatches(p$new, gregexpr("(?<![\\w.])\\.\\(\"", p$new, perl = TRUE)))
  if (!identical(norm(old), norm(new)) && !identical(loose(old), loose(new))) {
    bad <- bad + 1L
    cat(sprintf("DIFFERS %s:%d %s\n", p$file, p$line, p$owner))
  } else if (!identical(norm(old), norm(new))) {
    cat(sprintf("whitespace-only %s:%d %s\n", p$file, p$line, p$owner))
  }
}
cat(sprintf("%d literal(s), %d .() call(s), %d differ\n", length(pairs), nodes, bad))
quit(status = if (bad > 0) 1L else 0L)
