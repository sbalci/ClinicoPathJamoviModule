# Notice panels shared by waterfall, diagnosticmeta, ihcheterogeneity and swimmerplot, so the four
# look the same and there is one place to change. A jmvcore::Notice holds one plain-text string with
# no title, which is why these are Html panels (guide section 13).
# library-audit 2026-10-23 OncoPath [LOW] PARTIAL: one shared renderer; DEFERRED the native Notice;
#   revisit when jamovi's Notice takes a title and a multi-paragraph body (feature request drafted)

# Append a notice. An exact duplicate is skipped: a helper that runs once per consumer within a
# run must not report twice.
notice_append <- function(notices, type, title, content) {
    for (n in notices)
        if (identical(n$type, type) && identical(n$title, title) && identical(n$content, content))
            return(notices)
    c(notices, list(list(type = type, title = title, content = content)))
}

# The panel HTML: most severe first, one tinted banner per notice. The title names the severity in
# words, so it does not rest on color alone. Titles inherit the text color (fixed hues fell to
# 2.7-2.9:1 on jamovi's dark theme); the translucent tint and the border carry the severity.
# Never returns "": the jamovi client skips an empty Html update and would leave the previous
# run's notices on screen. `self` is the analysis, so .() finds its translations.
notice_panel_html <- function(self, notices) {
    if (length(notices) == 0)
        return("<div></div>")
    styles <- list(
        ERROR          = c(.("Error"), "rgba(220, 38, 38, 0.10)", "#fca5a5"),
        STRONG_WARNING = c(.("Strong warning"), "rgba(234, 88, 12, 0.10)", "#fdba74"),
        WARNING        = c(.("Warning"), "rgba(202, 138, 4, 0.12)", "#fde047"),
        INFO           = c(.("Note"), "rgba(37, 99, 235, 0.08)", "#93c5fd"))
    rank <- match(vapply(notices, function(n) n$type, ""), names(styles), nomatch = 4L)
    banners <- vapply(notices[order(rank)], function(n) {
        s <- styles[[n$type]]
        if (is.null(s)) s <- styles$INFO
        paste0(
            "<div style='background-color: ", s[2], "; border-left: 4px solid ", s[3], "; ",
            "padding: 12px; margin: 8px 0; border-radius: 4px; max-width: 100%; ",
            "overflow-wrap: anywhere; word-break: break-word;'>",
            "<strong style='color: inherit;'>", htmltools::htmlEscape(paste0(s[1], ": ", n$title)),
            "</strong><br>",
            "<span style='color: inherit; white-space: pre-line;'>", htmltools::htmlEscape(n$content),
            "</span></div>")
    }, character(1))
    paste0("<div style='margin: 10px 0;'>", paste(banners, collapse = ""), "</div>")
}
