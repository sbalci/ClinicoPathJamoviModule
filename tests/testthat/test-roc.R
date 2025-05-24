context("ROC Analysis")

library(pROC)

# synthetic dataset
scores_neg <- c(0.1, 0.2, 0.3, 0.25, 0.35, 0.15, 0.4, 0.45, 0.05, 0.3)
scores_pos <- c(0.2, 0.3, 0.4, 0.6, 0.7, 0.65, 0.8, 0.75, 0.9, 0.5)
response <- factor(c(rep("neg", length(scores_neg)), rep("pos", length(scores_pos))),
                   levels = c("neg", "pos"))
values <- c(scores_neg, scores_pos)

roc_obj <- pROC::roc(response, values, quiet = TRUE)

# Basic ROC metrics
auc_val <- pROC::auc(roc_obj)
best_coords <- pROC::coords(roc_obj, "best", best.method = "youden")

test_that("basic ROC output is correct", {
  expect_equal(round(as.numeric(auc_val), 2), 0.88)
  expect_equal(round(best_coords$threshold, 2), 0.5)
})

# Partial AUC
p_auc <- pROC::auc(roc_obj, partial.auc = c(0.8, 1),
                   partial.auc.focus = "specificity")

test_that("partial AUC is computed", {
  expect_true(as.numeric(p_auc) < as.numeric(auc_val))
  expect_gt(as.numeric(p_auc), 0)
})

# DeLong test comparing with a second predictor
scores_pos_b <- c(0.25, 0.35, 0.45, 0.65, 0.75, 0.7, 0.85, 0.8, 0.95, 0.55)
values_b <- c(scores_neg, scores_pos_b)
roc_obj_b <- pROC::roc(response, values_b, quiet = TRUE)

delong_res <- pROC::roc.test(roc_obj, roc_obj_b, paired = TRUE, method = "delong")

test_that("DeLong test runs", {
  expect_true(is.numeric(delong_res$p.value))
  expect_equal(delong_res$method, "DeLong's test for two correlated ROC curves")
})
