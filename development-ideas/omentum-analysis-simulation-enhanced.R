# ===== Enhanced Omentum Analysis Simulation =====
# This simulation generates data for testing the refactored pathsampling function
# with support for:
# - Multiple sample types (Serous, Endometrioid, Clear Cell)
# - Type-specific metastasis probabilities (tumor biology)
# - Complete positive sample tracking
# - Spatial clustering metrics
# - Multiple output formats for different analysis levels

# Set seed for reproducibility
set.seed(123)

# Define number of patients
N <- 1000

# ===== Simulate Sample Types =====
sample_types <- sample(c("Serous", "Endometrioid", "Clear Cell"),
                     N,
                     replace=TRUE,
                     prob=c(0.6, 0.3, 0.1))  # Realistic distribution

# ===== Type-Specific Metastasis Probabilities =====
metastasis_prob <- ifelse(sample_types == "Serous", 0.70,
                   ifelse(sample_types == "Endometrioid", 0.30, 0.50))

# Randomly assign metastasis presence based on sample type
metastasis_present <- rbinom(N, 1, prob=metastasis_prob)

# Randomly assign number of samples per patient (between 5 and 15)
n_samples <- sample(5:15, N, replace=TRUE)

# Prepare vectors/containers for simulation results
pos_count <- integer(N)
first_pos <- integer(N)
first_pos[] <- NA
pos_samples <- vector("list", N)  # Will store ALL positive sample positions

# ===== Track Spatial Clustering =====
clustering_index <- numeric(N)
clustering_index[] <- NA

# Loop over each patient to simulate sample results
for(i in 1:N) {
  if(metastasis_present[i] == 1) {

    # ===== Type-Specific Involvement Pattern =====
    if (sample_types[i] == "Serous") {
      # Serous: Tends to high involvement OR very sparse (bimodal)
      if(runif(1) < 0.6) {
        # High involvement
        p_sample <- runif(1, min=0.6, max=0.9)
      } else {
        # Sparse involvement
        p_sample <- runif(1, min=0.05, max=0.15)
      }
    } else if (sample_types[i] == "Endometrioid") {
      # Endometrioid: Moderate involvement
      p_sample <- runif(1, min=0.2, max=0.5)
    } else {
      # Clear cell: Variable
      p_sample <- runif(1, min=0.1, max=0.7)
    }

    # Simulate each sample as positive or negative for tumor
    positives <- which(runif(n_samples[i]) < p_sample)
    pos_count[i] <- length(positives)
    pos_samples[[i]] <- positives

    if(pos_count[i] > 0) {
      first_pos[i] <- positives[1]

      # ===== Calculate Clustering Index =====
      if (length(positives) > 1) {
        distances <- diff(sort(positives))
        mean_distance <- mean(distances)
        expected_distance <- n_samples[i] / length(positives)
        clustering_index[i] <- mean_distance / expected_distance
      }
    }
  } else {
    # No metastasis present
    pos_count[i] <- 0
    pos_samples[[i]] <- integer(0)
  }
}

# ===== Convert pos_samples list to comma-separated strings =====
pos_samples_string <- sapply(pos_samples, function(x) {
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse=",")
})

# Combine into enhanced data frame
sampling_data <- data.frame(
  patient_id = 1:N,
  sample_type = sample_types,
  metastasis_present_sim = metastasis_present,  # Ground truth (simulation only)
  n_samples = n_samples,
  pos_count = pos_count,
  first_pos = first_pos,
  pos_samples_string = pos_samples_string,
  clustering_index = clustering_index,
  stringsAsFactors = FALSE
)

# Add the list-column of positive sample indices
sampling_data$pos_samples <- pos_samples

# Enforce pathologist final diagnosis rule
# In real data, we only know what we find - no separate ground truth
sampling_data$metastasis_present <- as.integer(sampling_data$pos_count > 0)

cat("\n===== Simulation Summary =====\n")
cat(sprintf("Total cases simulated: %d\n", N))
cat(sprintf("True metastasis cases: %d (%.1f%%)\n",
           sum(metastasis_present), 100*mean(metastasis_present)))
cat(sprintf("Detected metastasis cases: %d (%.1f%%)\n",
           sum(sampling_data$metastasis_present),
           100*mean(sampling_data$metastasis_present)))
cat(sprintf("Missed cases (false negatives): %d\n",
           sum(metastasis_present == 1 & sampling_data$metastasis_present == 0)))

# ===== Sample Type Distribution =====
cat("\n===== Sample Type Distribution =====\n")
type_summary <- table(sample_types)
for (type in names(type_summary)) {
  n_type <- type_summary[type]
  n_positive <- sum(sample_types == type & metastasis_present == 1)
  cat(sprintf("%s: %d cases, %d positive (%.1f%% prevalence)\n",
             type, n_type, n_positive, 100*n_positive/n_type))
}

# ===== Empirical Cumulative Detection by Sample Type =====
cat("\n===== Cumulative Detection by Sample Type =====\n")
for (type in unique(sample_types)) {
  cat(sprintf("\n%s carcinoma:\n", type))

  type_data <- subset(sampling_data, sample_type == type & metastasis_present_sim == 1)

  if (nrow(type_data) > 0) {
    # Calculate empirical cumulative detection
    max_n <- max(type_data$n_samples)
    cumulative <- sapply(1:min(max_n, 10), function(n) {
      sum(type_data$first_pos <= n, na.rm=TRUE) / nrow(type_data)
    })

    # Print for key thresholds
    for (n in c(3, 5, 7, 10)) {
      if (n <= min(max_n, 10)) {
        cat(sprintf("  %d samples: %.1f%% detection\n", n, 100*cumulative[n]))
      }
    }

    # Per-sample probability (q) estimates
    q_geometric <- 1 / mean(type_data$first_pos, na.rm=TRUE)
    q_empirical <- sum(type_data$pos_count) / sum(type_data$n_samples)

    cat(sprintf("  q (geometric MLE): %.3f\n", q_geometric))
    cat(sprintf("  q (empirical proportion): %.3f\n", q_empirical))
  }
}

# ===== Clustering Analysis =====
cat("\n===== Spatial Clustering Analysis =====\n")
clustered <- sum(clustering_index < 0.7, na.rm=TRUE)
random <- sum(clustering_index >= 0.7 & clustering_index <= 1.3, na.rm=TRUE)
dispersed <- sum(clustering_index > 1.3, na.rm=TRUE)
total_with_clustering <- sum(!is.na(clustering_index))

cat(sprintf("Clustered (index < 0.7): %d cases (%.1f%%)\n",
           clustered, 100*clustered/total_with_clustering))
cat(sprintf("Random (0.7-1.3): %d cases (%.1f%%)\n",
           random, 100*random/total_with_clustering))
cat(sprintf("Dispersed (>1.3): %d cases (%.1f%%)\n",
           dispersed, 100*dispersed/total_with_clustering))

# ===== Tumor Burden Effects =====
cat("\n===== Tumor Burden Effects =====\n")
positive_cases <- subset(sampling_data, metastasis_present == 1)

# Categorize by burden
positive_cases$burden_cat <- cut(positive_cases$pos_count,
                                breaks=c(0, 1, 3, Inf),
                                labels=c("Single", "Low (2-3)", "High (4+)"))

burden_summary <- aggregate(first_pos ~ burden_cat,
                           data=positive_cases,
                           FUN=function(x) c(
                             count=length(x),
                             mean=mean(x, na.rm=TRUE),
                             median=median(x, na.rm=TRUE)
                           ))

print(burden_summary)

# ===== Distribution of First Positive =====
cat("\n===== Distribution of All Positive Samples =====\n")
multi_positive <- subset(sampling_data, pos_count > 1)

if (nrow(multi_positive) > 0) {
  for (threshold in c(3, 5, 7, 10)) {
    samples_in_range <- sapply(multi_positive$pos_samples, function(samples) {
      sum(samples <= threshold)
    })
    total_samples <- sapply(multi_positive$pos_samples, length)

    pct_found <- 100 * sum(samples_in_range) / sum(total_samples)
    cat(sprintf("First %d samples contain %.1f%% of all positive samples\n",
               threshold, pct_found))
  }
}

# ===== Save Datasets =====
cat("\n===== Saving Datasets =====\n")

# Level 1: Basic data (backward compatible with original pathsampling)
basic_data <- sampling_data[, c("patient_id", "n_samples", "first_pos")]
write.csv(basic_data,
         file.path(here::here(), "data", "pathsampling_basic.csv"),
         row.names=FALSE)
cat("Saved: pathsampling_basic.csv (Level 1: Basic analysis)\n")

# Level 2: Enhanced data (with positiveCount for empirical proportion method)
enhanced_data <- sampling_data[, c("patient_id", "n_samples", "first_pos", "pos_count")]
write.csv(enhanced_data,
         file.path(here::here(), "data", "pathsampling_enhanced.csv"),
         row.names=FALSE)
cat("Saved: pathsampling_enhanced.csv (Level 2: Enhanced q estimation)\n")

# Level 3: Complete data (with sample list and sample type for all analyses)
complete_data <- sampling_data[, c("patient_id", "sample_type", "n_samples",
                                  "pos_count", "first_pos", "pos_samples_string",
                                  "clustering_index")]
write.csv(complete_data,
         file.path(here::here(), "data", "pathsampling_complete.csv"),
         row.names=FALSE)
cat("Saved: pathsampling_complete.csv (Level 3: Complete analysis)\n")

# Also save full simulation data with ground truth for validation
full_data <- sampling_data[, c("patient_id", "sample_type", "metastasis_present_sim",
                              "metastasis_present", "n_samples", "pos_count",
                              "first_pos", "pos_samples_string", "clustering_index")]
write.csv(full_data,
         file.path(here::here(), "data", "pathsampling_simulation_full.csv"),
         row.names=FALSE)
cat("Saved: pathsampling_simulation_full.csv (For validation with ground truth)\n")

cat("\n===== Simulation Complete =====\n")
cat("Use these datasets to test pathsampling function at different analysis levels.\n")
