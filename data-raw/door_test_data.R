# Test data for door (Desirability of Outcome Ranking): 4-level ordinal DOOR by arm.
set.seed(2026); n <- 120
mk <- function(arm, prob) data.frame(arm = arm,
    door_category = factor(sample(
        c("Alive, no event", "Alive, minor event", "Alive, major event", "Dead"),
        n, replace = TRUE, prob = prob),
        levels = c("Alive, no event", "Alive, minor event", "Alive, major event", "Dead")))
door_test_data <- rbind(mk("Treatment", c(.45, .30, .15, .10)),
                        mk("Control",   c(.25, .28, .25, .22)))
door_test_data$arm <- factor(door_test_data$arm, levels = c("Control", "Treatment"))
usethis::use_data(door_test_data, overwrite = TRUE)
write.csv(door_test_data, "data/door_test_data.csv", row.names = FALSE)
