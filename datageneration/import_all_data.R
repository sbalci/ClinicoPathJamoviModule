# ──────────────────────────────────────────────────────────────────────────────
# import_all_data.R
#
# Scan "data/" for all .csv and .rda files, read each one into R, then
# save them into your package’s data/ folder via usethis::use_data(..., overwrite = TRUE).
#
# USAGE:
#   1) Place this script in your package root (e.g. in data-raw/ or scripts/).
#   2) setwd() so that your working directory is the package root.
#   3) Run source("data-raw/import_all_data.R")  (or copy–paste into the console).
#
# After running, you'll find one .rda file per object in the package's data/ folder.
# ──────────────────────────────────────────────────────────────────────────────

library(usethis)
library(fs)   # for dir_ls(); if you prefer base R, see the commented alternative below

# 1) Define the folder where your raw CSV/.rda currently live.
#    Here we'll assume it's "data/" relative to the package root.
raw_data_dir <- "data"

# If you prefer base R’s list.files, you could do:
# all_files <- list.files(path = raw_data_dir, pattern = "\\.(csv|rda)$",
#                         full.names = TRUE, recursive = FALSE)

# 2) List all .csv and .rda (non‐recursive). Adjust `recurse = TRUE` if you have subfolders.
all_files <- dir_ls(path = raw_data_dir, regexp = "\\.(csv|rda)$", recurse = FALSE)

# 3) Function to process each .csv: read into a data.frame, name it after the file basename.
process_csv <- function(path) {
    # e.g. path = "data/my_data.csv"
    fname    <- path_file(path)                  # "my_data.csv"
    obj_name <- path_ext_remove(fname)           # "my_data"
    message("Reading CSV: ", fname, " → object ‘", obj_name, "’")

    # Read with base::read.csv (you can swap in readr::read_csv if you prefer)
    df <- read.csv(path, stringsAsFactors = FALSE, check.names = TRUE)

    if (!is.data.frame(df)) {
        stop("Expected a data.frame from read.csv for ", fname)
    }

    # Assign to the global environment (so that use_data can find it)
    assign(obj_name, df, envir = .GlobalEnv)
    invisible(obj_name)
}

# 4) Function to process each .rda: load into a temp env, then assign each object to global.
process_rda <- function(path) {
    # e.g. path = "data/more_things.rda"
    fname <- path_file(path)               # "more_things.rda"
    message("Loading RDA: ", fname)

    temp_env <- new.env(parent = emptyenv())
    load(path, envir = temp_env)           # loads one or more objects into temp_env

    obj_names <- ls(envir = temp_env)
    if (length(obj_names) == 0L) {
        warning("No objects found in ", fname)
        return(character(0))
    }

    # For each object in temp_env, copy it into the global environment:
    for (nm in obj_names) {
        assign(nm, get(nm, envir = temp_env), envir = .GlobalEnv)
        message("  → loaded object: ", nm)
    }
    invisible(obj_names)
}

# 5) Main loop: for each file, call the appropriate processor, then call use_data().
all_obj_names <- character(0)

for (path in all_files) {
    ext <- tolower(path_ext(path))

    if (ext == "csv") {
        # Read CSV into data.frame
        obj_name <- process_csv(path)
        all_obj_names <- c(all_obj_names, obj_name)

    } else if (ext == "rda") {
        # Load .rda into environment, returning a vector of object names
        obj_names <- process_rda(path)
        all_obj_names <- c(all_obj_names, obj_names)
    }
}

# 6) Finally, call usethis::use_data() on each object that we just created.
#    This will save each object into the package’s data/ folder as "<object>.rda".
#    The overwrite = TRUE flag ensures that if a dataset of the same name already
#    exists, it will be replaced.

if (length(all_obj_names) == 0) {
    message("No .csv or .rda files found under ", raw_data_dir)
} else {
    message("\n┌ Saving ", length(all_obj_names), " object(s) into data/ via use_data():\n└─")
    for (nm in unique(all_obj_names)) {
        message("  • Saving object ‘", nm, "’ …")
        # Expect the object to exist in .GlobalEnv
        if (!exists(nm, envir = .GlobalEnv)) {
            warning("  → Object ‘", nm, "’ not found in global environment; skipping.")
            next
        }
        do.call(use_data,
                list(as.name(nm),
                     overwrite = TRUE
                )
        )
    }
    message("\nDone! All documented objects are now in your package’s data/ folder.")
}

# Optionally: clean up the global environment if you don’t want these raw objects lingering:
rm(list = unique(all_obj_names), envir = .GlobalEnv)
