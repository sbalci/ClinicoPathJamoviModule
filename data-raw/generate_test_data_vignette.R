#!/usr/bin/env Rscript
# Generate comprehensive test data vignette with download links
# Author: ClinicoPath Team
# Date: 2025-01-07

library(dplyr)
library(stringr)

# Get all .omv files in data directory
omv_files <- list.files(
  path = here::here("data"),
  pattern = "\\.omv$",
  full.names = FALSE
)

cat("Found", length(omv_files), ".omv files\n")

# Base GitHub raw URL
github_base <- "https://raw.githubusercontent.com/sbalci/ClinicoPathJamoviModule/master/data/"

# Function to create markdown link
create_link <- function(filename) {
  sprintf("- [%s](%s%s)", filename, github_base, filename)
}

# Group files by function (extract prefix before first underscore or filename without extension)
get_function_name <- function(filename) {
  # Remove .omv extension
  base <- sub("\\.omv$", "", filename)

  # Extract function name (everything before first underscore, or entire name if no underscore)
  func <- str_extract(base, "^[^_]+")

  return(func)
}

# Create data frame with files and functions
omv_df <- tibble(
  filename = omv_files,
  function_name = sapply(filename, get_function_name),
  link = sapply(filename, create_link)
) %>%
  arrange(function_name, filename)

# Get function counts
function_counts <- omv_df %>%
  group_by(function_name) %>%
  summarise(
    count = n(),
    files = list(link)
  ) %>%
  arrange(desc(count), function_name)

# Write the vignette header
vignette_file <- here::here("vignettes/test-data-complete-catalog.Rmd")

cat("# Complete Test Data Catalog - All .omv Files with Download Links\n\n",
    file = vignette_file)

cat("**Total Files:**", length(omv_files), "\n",
    "**Last Generated:**", as.character(Sys.Date()), "\n\n",
    file = vignette_file, append = TRUE)

cat("## Download Instructions\n\n",
    "Click any filename below to download the .omv file directly from GitHub.\n\n",
    "---\n\n",
    file = vignette_file, append = TRUE)

# Write function sections
for (i in 1:nrow(function_counts)) {
  func_name <- function_counts$function_name[i]
  func_count <- function_counts$count[i]
  func_files <- function_counts$files[[i]]

  # Write function header
  cat("### ", func_name, " (", func_count, " files)\n\n",
      file = vignette_file, append = TRUE, sep = "")

  # Write file links
  for (link in func_files) {
    cat(link, "\n", file = vignette_file, append = TRUE)
  }

  cat("\n", file = vignette_file, append = TRUE)
}

cat("\n---\n\n## Summary Statistics\n\n",
    "- **Total .omv Files:**", length(omv_files), "\n",
    "- **Total Functions:**", nrow(function_counts), "\n",
    "- **Average Files per Function:**", round(mean(function_counts$count), 1), "\n",
    "- **Most Files:**", function_counts$function_name[1], "(", function_counts$count[1], "files)\n\n",
    file = vignette_file, append = TRUE, sep = "")

cat("Vignette generated successfully:", vignette_file, "\n")

# Also create a CSV index
csv_file <- here::here("data/test_data_index.csv")
write.csv(omv_df, csv_file, row.names = FALSE)
cat("CSV index created:", csv_file, "\n")
