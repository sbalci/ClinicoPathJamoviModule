# Test data for the winratio analysis (Win Ratio for Hierarchical Composite Endpoints)
# One row per subject; two-arm trial with a prioritized composite:
#   priority 1: death (time-to-event)
#   priority 2: hospitalization (time-to-event)
#   priority 3: 6-minute-walk-distance change (continuous, higher = better)

set.seed(2026)
n_per_arm <- 150
admin_cens <- 36  # months

sim_arm <- function(n, arm, death_rate, hosp_rate, walk_mean) {
    death_t <- rexp(n, rate = death_rate)
    hosp_t  <- rexp(n, rate = hosp_rate)
    data.frame(
        arm         = arm,
        deathTime   = pmin(death_t, admin_cens),
        deathEvent  = as.integer(death_t <= admin_cens),
        hospTime    = pmin(hosp_t, admin_cens),
        hospEvent   = as.integer(hosp_t <= admin_cens),
        walkChange  = rnorm(n, mean = walk_mean, sd = 40),
        stringsAsFactors = FALSE
    )
}

winratio_test_data <- rbind(
    sim_arm(n_per_arm, "Treatment", death_rate = 1/48, hosp_rate = 1/30, walk_mean =  25),
    sim_arm(n_per_arm, "Control",   death_rate = 1/24, hosp_rate = 1/15, walk_mean = -10)
)
winratio_test_data$arm        <- factor(winratio_test_data$arm, levels = c("Control", "Treatment"))
winratio_test_data$deathEvent <- factor(winratio_test_data$deathEvent)
winratio_test_data$hospEvent  <- factor(winratio_test_data$hospEvent)
winratio_test_data <- winratio_test_data[sample(nrow(winratio_test_data)), ]
rownames(winratio_test_data) <- NULL

usethis::use_data(winratio_test_data, overwrite = TRUE)
write.csv(winratio_test_data,
          file.path("data", "winratio_test_data.csv"), row.names = FALSE)
