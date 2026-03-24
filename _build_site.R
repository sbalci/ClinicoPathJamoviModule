# Build pkgdown site with exclusions
# pkgdown doesn't support excluding .md files from home page processing,
# so we temporarily hide them during the build.

exclude_from_home <- c("TODO.md", "AGENTS.md", "CLAUDE.md", "GEMINI.md", "sonograph_log.md")

# Temporarily rename files so pkgdown won't find them
hidden <- character(0)
for (f in exclude_from_home) {
  if (file.exists(f)) {
    hidden_name <- paste0(".", f, ".pkgdown-hide")
    file.rename(f, hidden_name)
    hidden <- c(hidden, setNames(hidden_name, f))
  }
}

# Restore function
restore_files <- function() {
  for (i in seq_along(hidden)) {
    file.rename(hidden[[i]], names(hidden)[[i]])
  }
}

# Build, then always restore
tryCatch(
  pkgdown::build_site(),
  finally = restore_files()
)
