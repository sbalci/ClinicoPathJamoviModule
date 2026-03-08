library(survival)
library(survminer)
library(ggplot2)

set.seed(123)
n <- 200
time <- rexp(n)
status <- sample(0:1, n, replace=TRUE)
risk_groups <- factor(sample(c("Low Risk", "High Risk"), n, replace=TRUE), levels = c("Low Risk", "High Risk"))
plot_data <- data.frame(time, status, risk_groups)
fit <- survfit(Surv(time, status) ~ risk_groups, data = plot_data)

p <- ggsurvplot(
    fit,
    data = plot_data,
    risk.table = TRUE,
    risk.table.y.text = FALSE,  # Testing FALSE
    pval = TRUE,
    conf.int = TRUE,
    title = "Survival Curves",
    xlab = "Time",
    ylab = "Survival Probability",
    legend.title = "Risk Group",
    legend.labs = c("Low Risk", "High Risk"),
    palette = c("#2166AC", "#B2182B")
)

ggsave("test_table_FALSE.png", print(p), width=6, height=4)

p2 <- ggsurvplot(
    fit,
    data = plot_data,
    risk.table = TRUE,
    risk.table.y.text = TRUE,  # Testing TRUE
    pval = TRUE,
    conf.int = TRUE,
    title = "Survival Curves",
    xlab = "Time",
    ylab = "Survival Probability",
    legend.title = "Risk Group",
    legend.labs = c("Low Risk", "High Risk"),
    palette = c("#2166AC", "#B2182B")
)

ggsave("test_table_TRUE.png", print(p2), width=6, height=4)
