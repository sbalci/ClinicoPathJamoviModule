# The clinical presets live in jamovi/js/survivalPower.events.js, which nothing
# in R ever loads, so a preset can only be wrong at runtime in jamovi -- where a
# bad value is silently clamped or rejected and the user just sees a design they
# did not ask for. These tests read the .js table and check it against the
# .a.yaml schema and against the analysis itself.
#
# The whole feature was dead before this: the handler was R source at
# jamovi/survivalPower.events, a path and language jamovi does not load.

events_js <- testthat::test_path("..", "..", "jamovi", "js", "survivalPower.events.js")
a_yaml <- testthat::test_path("..", "..", "jamovi", "survivalPower.a.yaml")

# Minimal reader for the PRESET_CONFIGS literal. The file is written one
# "key: value," per line specifically so this stays a few lines of regex rather
# than a JS dependency the test machine may not have.
read_presets <- function(path) {
    lines <- readLines(path, warn = FALSE)
    start <- grep("^const PRESET_CONFIGS = \\{", lines)
    stop_at <- grep("^\\};", lines)
    stop_at <- stop_at[stop_at > start][1]
    lines <- lines[(start + 1):(stop_at - 1)]
    lines <- lines[!grepl("^\\s*//", lines)]

    presets <- list()
    current <- NULL
    for (line in lines) {
        open <- regmatches(line, regexec("^    ([A-Za-z_][A-Za-z0-9_]*): \\{", line))[[1]]
        if (length(open) == 2) {
            current <- open[2]
            presets[[current]] <- list()
            next
        }
        kv <- regmatches(line, regexec("^        ([A-Za-z_][A-Za-z0-9_]*): (.+?),?\\s*$", line))[[1]]
        if (length(kv) == 3 && !is.null(current)) {
            value <- kv[3]
            if (grepl("^'.*'$", value)) {
                presets[[current]][[kv[2]]] <- gsub("^'|'$", "", value)
            } else {
                presets[[current]][[kv[2]]] <- as.numeric(value)
            }
        }
    }
    presets
}

read_option_schema <- function(path) {
    spec <- yaml::yaml.load_file(path)
    schema <- list()
    for (opt in spec$options) {
        schema[[opt$name]] <- list(
            type = opt$type,
            min = opt$min,
            max = opt$max,
            levels = if (!is.null(opt$options)) vapply(opt$options, function(o) o$name, character(1)) else NULL
        )
    }
    schema
}


test_that("the events file is wired to the control that references it", {
    expect_true(file.exists(events_js))

    # jamovi resolves "./survivalPower.events" against the compiled js directory,
    # so the handler must be jamovi/js/<name>.events.js. The previous file was
    # jamovi/survivalPower.events and was never loaded.
    u <- readLines(testthat::test_path("..", "..", "jamovi", "survivalPower.u.yaml"), warn = FALSE)
    expect_true(any(grepl("survivalPower.events::onChange_clinicalPreset", u, fixed = TRUE)))

    js <- paste(readLines(events_js, warn = FALSE), collapse = "\n")
    expect_match(js, "onChange_clinicalPreset", fixed = TRUE)
    expect_match(js, "module.exports = events", fixed = TRUE)
})


test_that("every preset key and value is legal under the .a.yaml schema", {
    skip_if_not_installed("yaml")

    presets <- read_presets(events_js)
    schema <- read_option_schema(a_yaml)

    expect_gt(length(presets), 1)

    for (preset_name in names(presets)) {
        config <- presets[[preset_name]]
        expect_gt(length(config), 0)

        for (key in names(config)) {
            info <- paste0(preset_name, "$", key)
            expect_true(key %in% names(schema), info = info)

            opt <- schema[[key]]
            value <- config[[key]]

            if (opt$type == "List") {
                expect_true(value %in% opt$levels, info = info)
            } else {
                expect_true(is.numeric(value), info = info)
                if (!is.null(opt$min)) expect_gte(value, opt$min)
                if (!is.null(opt$max)) expect_lte(value, opt$max)
            }
        }
    }
})


test_that("the custom preset restores the .a.yaml defaults", {
    skip_if_not_installed("yaml")

    # Otherwise a worked example's numbers stay in the boxes after the user
    # switches back to "Custom Design", where they read as values the user chose.
    presets <- read_presets(events_js)
    spec <- yaml::yaml.load_file(a_yaml)
    defaults <- list()
    for (opt in spec$options) defaults[[opt$name]] <- opt$default

    for (key in names(presets$custom)) {
        expect_equal(presets$custom[[key]], defaults[[key]],
            info = paste("custom$", key, "must equal the .a.yaml default"))
    }
})


test_that("every preset produces a usable analysis, not a validation error", {
    skip_if_not_installed("yaml")
    skip_if_not_installed("gsDesign")

    # The real payoff: a preset that trips .validate_inputs() would put an ERROR
    # in front of the user the moment they picked it from the menu. The
    # non-inferiority preset is the live risk -- its assumed HR must stay
    # strictly below its margin or no sample size exists.
    presets <- read_presets(events_js)

    for (preset_name in names(presets)) {
        args <- presets[[preset_name]]
        args$clinical_preset <- preset_name

        analysis <- survivalPowerClass$new(
            options = do.call(survivalPowerOptions$new, args),
            data = data.frame(x = 1)
        )
        suppressWarnings(try(analysis$run(), silent = TRUE))

        notices <- as.character(analysis$results$notices$content)
        expect_false(grepl("ERROR:", notices, fixed = TRUE),
            info = paste(preset_name, "raised:", notices))

        value <- as.character(analysis$results$power_summary$asDF$calculated_value[1])
        expect_match(value, "Total Sample Size",
            info = paste(preset_name, "produced:", value))
    }
})
