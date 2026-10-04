# Independent numerical checks for DOI 10.1007/s10985-026-09729-7.
# Run from the repository root:
# Rscript --vanilla literature/putter-andersen-2026-cure-models-checks.R /tmp/cure-review/bmt.csv
# Public input: https://multi-state-book.github.io/companion/data/bmt.csv
# Patient-level data remain outside this repository; outputs contain aggregates only.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, file.exists(args[1]))
library(survival)
out_dir <- "literature"
bmt <- read.csv(args[1])
d <- subset(bmt, all == 0)
d$older <- as.integer(d$age > 30)
d$event <- ifelse(d$rel == 1, 1L, ifelse(d$death == 1, 2L, 0L))
d$time <- ifelse(d$event == 1, d$timerel, d$timedeath)
stopifnot(nrow(d) == 1406, sum(d$older) == 836,
          sum(d$event == 1) == 154, sum(d$event == 2) == 343,
          !anyNA(d[c("time", "event", "older")]), all(d$time >= 0))
cat("Cohort counts verified\n")
print(table(age_above_30 = d$older, event = d$event))

# Direct Aalen-Johansen recursion, independent of survival/cmprsk internals.
aj <- function(time, status) {
  times <- sort(unique(time))
  s <- 1
  cif <- c(0, 0)
  rows <- lapply(times, function(t) {
    n <- sum(time >= t)
    events <- vapply(1:2, function(k) sum(time == t & status == k), numeric(1))
    cif <<- cif + s * events / n
    s <<- s * (1 - sum(events) / n)
    c(time = t, n_risk = n, event_free = s, relapse = cif[1], death = cif[2])
  })
  as.data.frame(do.call(rbind, rows))
}
curves <- lapply(0:1, function(g) {
  z <- subset(d, older == g)
  a <- aj(z$time, z$event)
  ci <- cmprsk::cuminc(z$time, z$event, cencode = 0)
  for (k in 1:2) {
    # Evaluate the package's right-continuous step function on the event grid.
    package_curve <- ci[[paste(1, k)]]
    ii <- findInterval(a$time, package_curve$time)
    expected <- c(0, package_curve$est)[ii + 1L]
    stopifnot(max(abs(a[[c("relapse", "death")[k]]] - expected)) < 1e-12)
  }
  stopifnot(max(abs(a$event_free + a$relapse + a$death - 1)) < 1e-12)
  a
})
observed <- do.call(rbind, lapply(0:1, function(g) {
  z <- subset(d, older == g)
  a <- curves[[g + 1L]]
  horizons <- c(120, max(z$time))
  do.call(rbind, lapply(horizons, function(t) {
    row <- a[max(which(a$time <= t)), ]
    km <- summary(survfit(Surv(time, event == 1) ~ 1, data = z), times = t)$surv
    data.frame(age_group = if (g == 0) "<=30" else ">30", months = t,
               n_risk = sum(z$time >= t), relapse_cif = row$relapse,
               death_in_remission_cif = row$death, event_free = row$event_free,
               naive_one_minus_km = 1 - km)
  }))
}))
write.csv(observed, file.path(out_dir, "putter-andersen-2026-observed-risk-checks.csv"),
          row.names = FALSE)
print(observed, digits = 9, row.names = FALSE)

cox_results <- do.call(rbind, lapply(1:2, function(k) {
  f <- coxph(Surv(time, event == k) ~ older, data = d)
  b <- unname(coef(f))
  se <- sqrt(diag(vcov(f)))
  data.frame(event = c("relapse", "death_in_remission")[k], log_hr = b,
             se = se, hr = exp(b), lower95 = exp(b - qnorm(.975) * se),
             upper95 = exp(b + qnorm(.975) * se),
             p = 2 * pnorm(-abs(b / se)), ph_test_p = cox.zph(f)$table[1, "p"])
}))
write.csv(cox_results, file.path(out_dir, "putter-andersen-2026-cox-checks.csv"),
          row.names = FALSE)
print(cox_results, digits = 9, row.names = FALSE)

# Appendix 2: integrating 2/(1+exp(2t)) gives a finite cumulative hazard log(2).
# Thus the nominally susceptible component retains probability 1/2 at infinity.
grid <- seq(0, 15, length.out = 1501)
su_nonph <- (1 + exp(-2 * grid)) / 2
s_nonph <- 1 / 3 + (2 / 3) * su_nonph
s_ph <- 2 / 3 + (1 / 3) * exp(-2 * grid)
stopifnot(max(abs(s_nonph - s_ph)) < 1e-14)
cat("Appendix 2 maximum survival-curve difference:", max(abs(s_nonph - s_ph)), "\n")
latent <- do.call(rbind, lapply(seq(0, 1, .2), function(lambda) {
  cumulative_hazard <- if (lambda == 0) Inf else integrate(function(t) {
    2 * exp(-lambda * (2 * t + log1p(exp(-2 * t))))
  }, 0, Inf, rel.tol = 1e-11)$value
  residual <- exp(-cumulative_hazard)
  never_event <- 1 / 3 + (2 / 3) * residual
  data.frame(lambda = lambda, susceptible_hazard_integral = cumulative_hazard,
             susceptible_mass_at_infinity = residual, latent_weight_x1 = 1 / 3,
             total_never_event_x1 = never_event,
             latent_weight_odds_ratio = .5,
             total_never_event_odds_ratio = never_event / (1 - never_event))
}))
stopifnot(abs(latent$susceptible_hazard_integral[6] - log(2)) < 1e-10,
          abs(latent$total_never_event_x1[6] - 2 / 3) < 1e-10)
write.csv(latent, file.path(out_dir, "putter-andersen-2026-latent-weight-checks.csv"),
          row.names = FALSE)
print(latent, digits = 9, row.names = FALSE)

# Appendix 1: a test calibrated above the 5th null percentile has about 95%
# rejection under a continuous null distribution, not a 5% upper-tail size.
cat("Upper-tail probability beyond the 5th percentile:", 1 - .05, "\n")
cat("Exponential survivor probabilities at t=3 for the stated rates:",
    exp(-3 * c(.256, .274)), "\n")

# Original figure from the independent AJ calculation; each curve ends at its
# own observed support. The display horizon matches the authors' example.
png(file.path(out_dir, "putter-andersen-2026-observed-cif.png"),
    width = 1400, height = 650, res = 140)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
for (cause in c("relapse", "death")) {
  plot(NA, xlim = c(0, 144), ylim = c(0, .4),
       xlab = "Months since transplantation", ylab = "Cumulative incidence",
       main = if (cause == "relapse") "Relapse" else "Death in remission")
  for (g in 0:1) {
    a <- curves[[g + 1L]]
    lines(c(0, a$time), c(0, a[[cause]]), type = "s",
          col = c("#1565C0", "#D45B00")[g + 1L], lwd = 2)
  }
  legend("topleft", c("Age <=30", "Age >30"),
         col = c("#1565C0", "#D45B00"), lwd = 2, bty = "n")
}
invisible(dev.off())
cat("Package versions:\n")
print(sapply(c("survival", "cmprsk"), function(p) as.character(packageVersion(p))))
