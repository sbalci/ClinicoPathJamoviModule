# Independent checks for the review of Duoerkongjiang, Zapf, Kirchhof, Ozga (2026)
# "Methods for analyzing recurrent and terminal events considering composite and
# competing endpoints in clinical trials: a practical overview".
# BMC Med Res Methodol 26:206. doi:10.1186/s12874-026-03008-7
#
# Run from the repository root:  Rscript --vanilla literature/duoerkongjiang-2026-recurrent-terminal-checks.R
# Needs: survival, MASS, reReg (>= 1.4.7), frailtypack (data only), WA (CRAN, no imports).
# No patient-level data from the article (EAST-AFNET 4 is not public); public R datasets only.

suppressMessages({
  library(survival)
  for (p in c("MASS", "reReg", "frailtypack", "WA"))
    if (!requireNamespace(p, quietly = TRUE)) stop("install.packages('", p, "') first")
})

ci <- function(b, se) sprintf("%.2f (%.2f-%.2f)", exp(b), exp(b - 1.96 * se), exp(b + 1.96 * se))

## 1. Arithmetic reported in the article ----------------------------------------
w <- 0.2018; l <- 0.1538; t <- 1 - w - l                   # wins / losses (Results)
stopifnot(abs(w / l - 1.31) < 0.01,                        # "WR: 31%"
          abs((w + t / 2) / (l + t / 2) - 1.10) < 0.01,    # "WO: 10%"
          abs((l + t / 2) / (w + t / 2) - 0.91) < 0.01)    # inverse WO 0.91 (Fig. 4)
wt <- c(cv_death = 1, whf = 0.5, acs = 0.5, stroke = 0.8)  # WHR/LWYY weights
stopifnot(all(abs(round(wt / sum(wt), 2) - c(0.36, 0.18, 0.18, 0.29)) < 0.006))
cat(sprintf("[1] ties %.1f%%; WR %.2f; WO %.2f; net benefit from wins %.3f (GPC reported 0.0674)\n",
            100 * t, w / l, (w + t / 2) / (l + t / 2), w - l))

## 2. While-alive loss rate: hand-coded estimator vs WA::LRfit ------------------
# frailtypack::readmission: colorectal-cancer re-hospitalisations, death terminal.
# Observational (chemo not randomised): estimand mechanics only, no causal reading.
data(readmission, package = "frailtypack")
r <- readmission[order(readmission$id, readmission$t.stop), ]
last <- !duplicated(r$id, fromLast = TRUE)
d <- data.frame(id = r$id, time = r$t.stop,
                status = ifelse(r$event == 1, 1L, ifelse(last & r$death == 1, 2L, 0L)),
                trt = as.integer(r$chemo == "Treated"))

# mu(tau) = sum S * dR (Ghosh-Lin / Cook-Lawless mean count, death terminal);
# rate = mu(tau) / RMST(tau).  convention "textbook": S(t-) and exact KM area;
# convention "WA": S(t) and right-endpoint sum on the event grid (WA 1.0 source).
while_alive_rate <- function(d, tau, convention = c("textbook", "WA")) {
  convention <- match.arg(convention)
  ex <- tapply(d$time, d$id, max); dead <- tapply(d$status == 2, d$id, any)
  tt <- sort(unique(d$time[d$status != 0 & d$time > 0])); keep <- tt <= tau
  Y <- vapply(tt, function(u) sum(ex >= u), 1)
  dD <- vapply(tt, function(u) sum(ex == u & dead), 1)
  dR <- vapply(tt, function(u) sum(d$time == u & d$status == 1), 1) / Y
  St <- cumprod(1 - dD / Y); Stm <- c(1, head(St, -1))
  if (convention == "WA")
    return(sum((St * dR)[keep]) / sum((St * diff(c(0, tt)))[keep]))
  sum((Stm * dR)[keep]) / sum(diff(c(0, tt[keep], tau)) * c(1, St[keep]))
}
tau <- 1500
ratio <- function(cv) while_alive_rate(d[d$trt == 1, ], tau, cv) / while_alive_rate(d[d$trt == 0, ], tau, cv)
wa_fit <- WA::LRfit(d$id, d$time, d$status, d$trt)
wa_ratio <- exp(summary(wa_fit, tau = tau)$LRtab[2, 1])   # row 2 = "Group 1 vs 0" log LR ratio
stopifnot(abs(ratio("WA") - wa_ratio) < 1e-5)           # exact agreement with WA's convention
cat(sprintf("[2] while-alive ratio, readmission tau=%d: WA-convention %.5f (WA::LRfit %.5f); textbook %.5f\n",
            tau, ratio("WA"), wa_ratio, ratio("textbook")))

## 3. Same trial, different estimands: survival::bladder1 (thiotepa vs placebo) --
b <- subset(bladder1, treatment %in% c("placebo", "thiotepa"))
b <- b[order(b$id, b$stop), ]
b <- b[b$stop > b$start, ]                               # drops zero-length rows
b$trt <- as.integer(b$treatment == "thiotepa")
b$rec <- as.integer(b$status == 1); b$dth <- as.integer(b$status %in% 2:3)
f1 <- b[!duplicated(b$id), ]
tfe <- coxph(Surv(stop, status > 0) ~ trt, f1)          # first recurrence or death
ag  <- coxph(Surv(start, stop, rec) ~ trt, b)           # Andersen-Gill, model-based SE
lw  <- coxph(Surv(start, stop, rec) ~ trt + cluster(id), b)   # LWYY = AG + robust SE
b$k <- pmin(b$enum, 4)
pwp <- coxph(Surv(start, stop, rec) ~ trt + strata(k) + cluster(id), b)  # PWP counting process
s <- data.frame(n = as.vector(tapply(b$rec, b$id, sum)), fu = as.vector(tapply(b$stop, b$id, max)),
                trt = as.vector(tapply(b$trt, b$id, max)))
nb <- MASS::glm.nb(n ~ trt + offset(log(fu)), s)
# reReg requires each subject to end on a censoring/terminal record (the article's
# Ghosh-Lin note); 9 subjects end on a recurrence, so add a closing record.
lr <- b[!duplicated(b$id, fromLast = TRUE) & b$rec == 1, ]
lr$start <- lr$stop; lr$stop <- lr$stop + 0.01; lr$rec <- 0L; lr$dth <- 0L
bg <- rbind(b, lr); bg <- bg[order(bg$id, bg$stop), ]
gl <- reReg::reReg(reReg::Recur(stop, id, rec, dth) ~ trt, data = bg, model = "cox.GL", B = 0)
db <- data.frame(id = bg$id, time = bg$stop, trt = bg$trt,
                 status = ifelse(bg$rec == 1, 1L, ifelse(bg$dth == 1, 2L, 0L)))
wa_b <- WA::LRfit(db$id, db$time, db$status, db$trt)
cat(sprintf("[3] bladder1: %d subjects, %d recurrences, %d deaths, %d closing records added\n",
            length(unique(b$id)), sum(b$rec), sum(b$dth), nrow(lr)))
print(c(TTFE_HR = ci(coef(tfe), sqrt(vcov(tfe))), AG_HR = ci(coef(ag), sqrt(vcov(ag))),
        LWYY_RR = ci(coef(lw), sqrt(vcov(lw))), PWP_CP_HR = ci(coef(pwp), sqrt(vcov(pwp))),
        NB_IRR = ci(coef(nb)[2], sqrt(vcov(nb)[2, 2])),
        GhoshLin_RR = sprintf("%.2f (point, B=0)", exp(gl$par1))), quote = FALSE)
summary(wa_b, tau = 50)

## 4. Module defect: marginalrecurrent reads summary(reReg) by the wrong names ----
# R/marginalrecurrent.b.R .populateCoefficients() uses summary(fit)$coef and columns
# "SE", "z", "Pr(>|z|)". reReg 1.4.7 returns coefficients.rec / coefficients.haz with
# columns StdErr, z.value, p.value; `$coef` matches both partially, so it is NULL.
data(readmission, package = "frailtypack")
fj <- reReg::reReg(reReg::Recur(t.stop, id, event, death) ~ chemo, data = readmission,
                   model = "cox|cox", se = "sand")
sj <- summary(fj)
stopifnot(is.null(sj$coef), !"SE" %in% colnames(sj$coefficients.rec))
cat("[4] summary(reReg) slots:", paste(names(sj), collapse = ", "),
    "| columns:", paste(colnames(sj$coefficients.rec), collapse = ", "),
    "| sandwich SE under cox|cox:", sj$coefficients.rec[1, "StdErr"], "\n")
cat(sprintf("    same data, cox|cox recurrent RR %.2f vs cox.GL marginal-mean RR %.2f (different estimands)\n",
            exp(sj$coefficients.rec[1, "Estimate"]),
            exp(reReg::reReg(reReg::Recur(t.stop, id, event, death) ~ chemo, data = readmission,
                             model = "cox.GL", B = 0)$par1)))
