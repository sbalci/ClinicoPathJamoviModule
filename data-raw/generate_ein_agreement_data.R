# Generate Synthetic Data Replicating Usubutun et al. (2012)
# Endometrial Intraepithelial Neoplasia (EIN) Diagnostic Agreement Study
# Modern Pathology 25: 877-884

library(dplyr)
library(tidyr)

set.seed(2012)  # Year of publication

# Study parameters
n_cases <- 62
n_pathologists <- 20

# Expert consensus reference diagnoses
# From paper: 27 benign, 26 EIN, 9 adenocarcinoma
reference_dx <- c(
  rep("Benign", 27),
  rep("EIN", 26),
  rep("Adenocarcinoma", 9)
)

# Pathologist names (from Table 2)
pathologist_names <- c(
  "T", "S", "R", "N", "H", "J", "K", "B", "C", "I",
  "D", "M", "L", "Q", "P", "O", "A", "G", "F", "E"
)

# Diagnostic style groups from Figure 1
# Green (conservative): 4 pathologists - favor benign
# Yellow (balanced): 11 pathologists - balanced use
# Red (sensitive): 5 pathologists - favor EIN
style_groups <- c(
  rep("Green", 4),   # S, R, T, N
  rep("Yellow", 11), # H, J, K, B, C, I, D, M, L, Q, P
  rep("Red", 5)      # O, A, G, F, E
)

# Pathologist characteristics (from Table 2)
pathologist_info <- data.frame(
  pathologist = pathologist_names,
  style_group = style_groups,
  years_experience = c(25, 13, 12, 4, 33, 8, 8, 13, 16, 18,
                       12, 1, 7, 21, 5, 3, 13, 17, 6, 15),
  specialty = c("GYN", "GYN", "GYN", "GYN", "GYN", "GYN", "GYN", "GYN", "GYN", "GYN",
                "GYN", "GEN", "GYN", "GYN", "GEN", "GEN", "GYN", "GYN", "GYN", "GYN"),
  uses_ein = c("No", "No", "No", "Yes", "No", "No", "No", "No", "No", "No",
               "No", "No", "No", "No", "Yes", "No", "No", "Yes", "Yes", "Yes"),
  institution = c("EUFM", "EUFM", "MUFM", "BUFM", "AUFM", "DEUFM", "CUFM", "AUFM",
                  "IUCFM", "IUCFM", "EZH", "HUFM", "KUFM", "DEUFM", "HUFM", "BUFM",
                  "EZH", "UUFM", "BUFM", "HUFM"),
  agreement_pct = c(66.13, 69.35, 72.58, 75.81, 82.46, 80.65, 80.65, 87.10,
                    86.89, 83.33, 85.48, 79.03, 79.03, 75.81, 75.81, 74.19,
                    88.71, 83.05, 85.48, 83.87),
  kappa = c(0.4521, 0.5109, 0.6276, 0.6809, 0.7656, 0.7255, 0.7429, 0.8225,
            0.8229, 0.7734, 0.7932, 0.7180, 0.6797, 0.6904, 0.6873, 0.6630,
            0.8351, 0.7813, 0.7734, 0.7685)
)

# Generate diagnoses based on style groups and reference
# Key findings from paper:
# - Overall 79% agreement with reference
# - Green group: favors benign, misses some EIN
# - Yellow group: balanced, best agreement
# - Red group: sensitive to EIN, some overdiagnosis

generate_pathologist_diagnosis <- function(ref_dx, style, agreement_rate) {
  diagnoses <- character(length(ref_dx))

  for (i in seq_along(ref_dx)) {
    # Base agreement probability
    agree_prob <- agreement_rate / 100

    if (runif(1) < agree_prob) {
      # Agree with reference
      diagnoses[i] <- ref_dx[i]
    } else {
      # Disagree based on style
      if (style == "Green") {
        # Conservative: shift toward Benign
        if (ref_dx[i] == "EIN") {
          diagnoses[i] <- sample(c("Benign", "Adenocarcinoma"), 1, prob = c(0.8, 0.2))
        } else if (ref_dx[i] == "Adenocarcinoma") {
          diagnoses[i] <- sample(c("Benign", "EIN"), 1, prob = c(0.3, 0.7))
        } else {
          diagnoses[i] <- sample(c("EIN", "Adenocarcinoma"), 1, prob = c(0.7, 0.3))
        }
      } else if (style == "Red") {
        # Sensitive: shift toward EIN
        if (ref_dx[i] == "Benign") {
          diagnoses[i] <- sample(c("EIN", "Adenocarcinoma"), 1, prob = c(0.8, 0.2))
        } else if (ref_dx[i] == "Adenocarcinoma") {
          diagnoses[i] <- sample(c("Benign", "EIN"), 1, prob = c(0.2, 0.8))
        } else {
          diagnoses[i] <- sample(c("Benign", "Adenocarcinoma"), 1, prob = c(0.3, 0.7))
        }
      } else {
        # Balanced: random disagreement
        diagnoses[i] <- sample(setdiff(c("Benign", "EIN", "Adenocarcinoma"), ref_dx[i]), 1)
      }
    }
  }

  return(diagnoses)
}

# Generate diagnosis matrix
diagnosis_matrix <- matrix(nrow = n_cases, ncol = n_pathologists)
colnames(diagnosis_matrix) <- pathologist_names

for (j in 1:n_pathologists) {
  diagnosis_matrix[, j] <- generate_pathologist_diagnosis(
    reference_dx,
    pathologist_info$style_group[j],
    pathologist_info$agreement_pct[j]
  )
}

# Create long-format data
ein_agreement_long <- data.frame(
  case_id = rep(1:n_cases, n_pathologists),
  case_number = rep(1:n_cases, n_pathologists),
  reference_diagnosis = rep(reference_dx, n_pathologists),
  pathologist = rep(pathologist_names, each = n_cases),
  diagnosis = as.vector(diagnosis_matrix)
)

# Add pathologist characteristics
ein_agreement_long <- ein_agreement_long %>%
  left_join(pathologist_info, by = "pathologist")

# Add case difficulty markers (from Figure 1 arrows)
# Discordant cases that distinguish style groups
discordant_cases <- c(
  # Cases distinguishing Red from others (prefer EIN)
  51, 6, 25, 44, 61,
  # Cases distinguishing Green from others (prefer Benign)
  11, 26, 14, 38, 16
)

ein_agreement_long$discordant_case <- ifelse(
  ein_agreement_long$case_number %in% discordant_cases,
  "Yes", "No"
)

# Create wide-format data for jamovi
ein_agreement_wide <- data.frame(
  case_id = 1:n_cases,
  reference = reference_dx,
  discordant = ifelse(1:n_cases %in% discordant_cases, "Yes", "No")
)

# Add pathologist diagnoses as columns
for (j in 1:n_pathologists) {
  ein_agreement_wide[[pathologist_names[j]]] <- diagnosis_matrix[, j]
}

# Save datasets
write.csv(ein_agreement_long,
          "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data/ein_agreement_long.csv",
          row.names = FALSE)

write.csv(ein_agreement_wide,
          "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data/ein_agreement_wide.csv",
          row.names = FALSE)

write.csv(pathologist_info,
          "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data/ein_pathologist_info.csv",
          row.names = FALSE)

# Save as RDS for R use
saveRDS(ein_agreement_long,
        "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data/ein_agreement_long.rds")
saveRDS(ein_agreement_wide,
        "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data/ein_agreement_wide.rds")
saveRDS(pathologist_info,
        "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/data/ein_pathologist_info.rds")

# Print summary
cat("EIN Agreement Study Data Generated\n")
cat("==================================\n")
cat("Cases:", n_cases, "\n")
cat("Pathologists:", n_pathologists, "\n")
cat("Diagnostic categories: Benign, EIN, Adenocarcinoma\n")
cat("\nReference distribution:\n")
print(table(reference_dx))
cat("\nStyle groups:\n")
print(table(pathologist_info$style_group))
cat("\nDiscordant cases:", length(discordant_cases), "\n")
cat("\nFiles saved:\n")
cat("- ein_agreement_long.csv (62 × 20 = 1240 rows)\n")
cat("- ein_agreement_wide.csv (62 rows, 23 columns)\n")
cat("- ein_pathologist_info.csv (20 rows)\n")
