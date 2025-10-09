# install.packages(c("rvest","httr2","stringr","purrr","dplyr","readr"))
library(rvest)
library(httr2)
library(stringr)
library(purrr)
library(dplyr)
library(readr)

base <- "https://library.jamovi.org/"
pattern <- ".*"  # e.g. "\\.jmo$" to restrict to packages

# helper: list absolute links on an index page
get_links <- function(url) {
  resp <- request(url) |> req_perform()
  html <- resp_body_html(resp)
  hrefs <- html |> html_elements("a") |> html_attr("href") |> na.omit()
  hrefs <- hrefs[!(hrefs %in% c("../","./")) & !startsWith(hrefs, "#")]
  url_absolute(hrefs, url)
}

# robust helpers for URL segments
last_seg   <- function(x) sub(".*/([^/?#]+)/?$", "\\1", x)
is_dir_url <- function(x) str_ends(x, "/")

os_dirs <- get_links(base) |>
  keep(~ str_detect(.x, "/(linux|macos|win64)/$")) |>
  sort()

rows <- map_dfr(os_dirs, function(os_dir) {
  os <- last_seg(os_dir)
  r_dirs <- get_links(os_dir) |>
    keep(~ str_detect(.x, "/R\\d") && is_dir_url(.x)) |>
    sort()

  map_dfr(r_dirs, function(r_dir) {
    rver <- last_seg(r_dir)

    lvl1 <- get_links(r_dir)
    subdirs <- lvl1[is_dir_url(lvl1)]
    files1  <- lvl1[!is_dir_url(lvl1)]

    files2 <- map(subdirs, get_links) |> list_c()
    all_files <- c(files1, files2)

    tibble(
      os = os,
      r_version = rver,
      filename = basename(all_files),
      url = all_files
    ) |>
      filter(str_detect(filename, pattern))
  })
})

# write CSV
rows |>
  arrange(os, r_version, filename) |>
  write_csv("vignettes/jamovi_library_files.csv")

cat("Wrote", nrow(rows), "rows to vignettes/jamovi_library_files.csv\n")

# also write grouped Markdown with correct OS/R headings
make_markdown <- function(df, outfile = "vignettes/jamovi_library_files.md") {
  df <- df |> arrange(os, r_version, filename)
  out <- character()

  df |>
    group_by(os, r_version) |>
    group_walk(~{
      # derive headings from URL for robustness
      ex_url <- .x$url[1]
      os_h   <- str_match(ex_url, "^https?://[^/]+/([^/]+)/")[, 2]
      r_h    <- str_match(ex_url, "/(R[^/]+)/")[, 2]
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

make_markdown(rows, "vignettes/jamovi_library_files.md")
