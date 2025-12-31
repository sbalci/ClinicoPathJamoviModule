
devtools::load_all()

test_eurostat_data <- data.frame(
  geo = c("AT", "BE", "DE", "FR"),
  values = c(10, 20, 30, 40),
  TIME_PERIOD = rep(2022, 4)
)

print("Running eurostatmap...")
tryCatch({
    results <- ClinicoPath::eurostatmap(
        data = test_eurostat_data,
        indicator = "values",
        use_local_data = TRUE,
        add_to_data = FALSE,
        geo_level = "nuts0",
        map_type = "static" 
    )
    print("Run completed.")
    print(results)
}, error = function(e) {
    print(paste("ERROR:", e$message))
})
