# install.packages(c("rvest","httr2","stringr","purrr","dplyr","readr"))

library(rvest)
library(httr2)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)
library(readr)

base <- "https://library.jamovi.org/"
rx <- regex("^(ClinicoPath|ClinicoPathDescriptives|meddecide|jsurvival|jjstatsplot|OncoPath)-.*\\.jmo$", ignore_case = TRUE)

get_links <- function(url) {
  resp <- request(url) |> req_perform()
  html <- resp_body_html(resp)
  hrefs <- html |> html_elements("a") |> html_attr("href") |> na.omit()
  hrefs <- hrefs[!(hrefs %in% c("../","./")) & !startsWith(hrefs, "#")]
  url_absolute(hrefs, url)
}

# robust helpers for URL segments
last_seg <- function(x) sub(".*/([^/?#]+)/?$", "\\1", x)
parent_url <- function(x) sub("(/[^/]+)/?$", "/", x)

os_dirs <- get_links(base) |>
  keep(~ str_detect(.x, "/(linux|macos|win64)/$")) |>
  sort()

rows <- map_dfr(os_dirs, function(os_dir) {
  os <- last_seg(os_dir)               # <-- fix (was str_remove(...))
  r_dirs <- get_links(os_dir) |>
    keep(~ str_detect(.x, "/R\\d") && str_ends(.x, "/")) |>
    sort()

  map_dfr(r_dirs, function(r_dir) {
    rver <- last_seg(r_dir)            # <-- fix
    lvl1 <- get_links(r_dir)
    subdirs <- lvl1[str_ends(lvl1, "/")]
    files1  <- lvl1[!str_ends(lvl1, "/")]
    files2 <- map(subdirs, get_links) |> list_c()
    all_files <- c(files1, files2)

    tibble(
      os = os,
      r_version = rver,
      filename = basename(all_files),
      url = all_files
    ) |>
      filter(str_detect(filename, rx))
  })
})

rows

rows |>
  arrange(os, r_version, filename) |>
  write_csv("./vignettes/jamovi_ClinicoPath_modules.csv")

cat("Wrote", nrow(rows), "rows to jamovi_ClinicoPath_modules.csv\n")


# --- Setup ---
# assumes you already have `rows` as a tibble/data.frame
# with columns: os, r_version, filename, url
# (from fetch-jamovi-mine.R or CSV)

# If you already exported CSV:
# rows <- read_csv("jamovi_ClinicoPath_modules.csv")

# Example: rows <- tibble(
#   os = c("win64","win64","macos"),
#   r_version = c("R4.5.0-x64","R4.5.0-x64","R4.5.0-arm64"),
#   filename = c("jsurvival-0.0.3.90.jmo","meddecide-0.0.3.90.jmo","jjstatsplot-0.0.3.90.jmo"),
#   url = c("https://.../jsurvival-0.0.3.90.jmo","https://.../meddecide-0.0.3.90.jmo","https://.../jjstatsplot-0.0.3.90.jmo")
# )

library(dplyr)
library(stringr)
library(purrr)
library(readr)

# rows should have: os, r_version, filename, url
# e.g., rows <- read_csv("jamovi_my_modules.csv")

# 1) Repair missing os / r_version from URL (if any)
rows <- rows %>%
  mutate(
    os = if_else(is.na(os) | os == "",
                 str_match(url, "^https?://[^/]+/([^/]+)/")[,2],
                 os),
    r_version = if_else(is.na(r_version) | r_version == "",
                        str_match(url, "/(R[^/]+)/")[,2],
                        r_version)
  )

# 2) Write grouped Markdown with proper headings
make_markdown <- function(df, outfile = "jamovi_my_modules.md") {
  out <- character()

  df %>%
    arrange(os, r_version, filename) %>%
    group_by(os, r_version) %>%
    group_walk(~{
      # Derive robust headings from URL first; fall back to columns
      ex_url <- .x$url[1]
      os_h   <- stringr::str_match(ex_url, "^https?://[^/]+/([^/]+)/")[, 2]
      r_h    <- stringr::str_match(ex_url, "/(R[^/]+)/")[, 2]
      if (is.na(os_h) || os_h == "") os_h <- unique(.x$os)[1]
      if (is.na(r_h)  || r_h  == "") r_h  <- unique(.x$r_version)[1]
      header <- paste0("## ", os_h, " / ", r_h)

      tbl <- paste0(
        "| File | Link |\n",
        "|------|------|\n",
        paste0("| ", .x$filename, " | [Download](", .x$url, ") |", collapse = "\n")
      )
      out <<- c(out, header, "", tbl, "")
    })

  writeLines(out, outfile)
  message("Wrote markdown to ", outfile)
}

# Run it:
make_markdown(rows, "vignettes/jamovi_ClinicoPath_modules.md")
