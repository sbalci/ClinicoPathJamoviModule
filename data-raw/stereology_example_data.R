# Stereology Example Data Generation
# Based on Kayser et al. (2009) stereology methods
# Simulates vessel density measurements in tumor tissue

set.seed(42)

n_samples <- 50

# Simulate measurements for different diagnoses
diagnosis <- rep(c("Normal", "Benign", "Malignant"), length.out = n_samples)

# Reference area (constant for this example: 1000x1000 μm)
reference_area <- rep(1000000, n_samples)  # μm²

# Grid spacing (10 μm between grid lines)
grid_spacing <- rep(10, n_samples)

# Total grid points examined (10x10 grid = 100 points)
total_points <- rep(100, n_samples)

# Generate intersection counts based on diagnosis
# Normal: low vessel density (Aa ~ 0.05-0.10)
# Benign: moderate vessel density (Aa ~ 0.15-0.25)
# Malignant: high vessel density (Aa ~ 0.30-0.50) - angiogenesis

intersections <- numeric(n_samples)
for (i in 1:n_samples) {
  if (diagnosis[i] == "Normal") {
    intersections[i] <- round(rnorm(1, mean = 7.5, sd = 2))
  } else if (diagnosis[i] == "Benign") {
    intersections[i] <- round(rnorm(1, mean = 20, sd = 4))
  } else {
    intersections[i] <- round(rnorm(1, mean = 40, sd = 6))
  }
}

# Ensure reasonable bounds
intersections <- pmax(0, pmin(intersections, total_points))

# Boundary intersections (for boundary density)
# Assume ~2-3 times the intersection count
boundary_intersections <- round(intersections * runif(n_samples, 2, 3))

# Object count (discrete vessels counted)
# Assume ~0.5-0.7 of intersection count
object_count <- round(intersections * runif(n_samples, 0.5, 0.7))

# Additional variables
tissue_type <- rep("Blood Vessels", n_samples)
section_thickness <- rep(5, n_samples)  # 5 μm standard
image_id <- paste0("IMG_", sprintf("%03d", 1:n_samples))

# Create data frame
stereology_vessel_data <- data.frame(
  image_id = image_id,
  diagnosis = factor(diagnosis, levels = c("Normal", "Benign", "Malignant")),
  tissue_type = tissue_type,
  intersections = as.integer(intersections),
  total_points = as.integer(total_points),
  boundary_intersections = as.integer(boundary_intersections),
  object_count = as.integer(object_count),
  reference_area = reference_area,
  grid_spacing = grid_spacing,
  section_thickness = section_thickness,
  stringsAsFactors = FALSE
)

# Calculate derived parameters for reference
stereology_vessel_data$Aa <- stereology_vessel_data$intersections / stereology_vessel_data$total_points
stereology_vessel_data$Vv <- stereology_vessel_data$Aa  # Vv = Aa for isotropic sections

# Summary
cat("Stereology Vessel Density Data Summary:\n")
cat("========================================\n\n")

cat("Sample Size:", nrow(stereology_vessel_data), "\n\n")

cat("Diagnosis Distribution:\n")
print(table(stereology_vessel_data$diagnosis))
cat("\n")

cat("Area Density (Aa) by Diagnosis:\n")
aa_by_diag <- aggregate(Aa ~ diagnosis, data = stereology_vessel_data,
                        FUN = function(x) c(mean = mean(x), sd = sd(x)))
print(aa_by_diag)
cat("\n")

cat("Mean Intersections by Diagnosis:\n")
int_by_diag <- aggregate(intersections ~ diagnosis, data = stereology_vessel_data,
                         FUN = function(x) c(mean = mean(x), sd = sd(x)))
print(int_by_diag)
cat("\n")

# Save as RDA
save(stereology_vessel_data,
     file = "data/stereology_vessel_data.rda",
     compress = "bzip2")

# Save as CSV
write.csv(stereology_vessel_data,
          file = "data/stereology_vessel_data.csv",
          row.names = FALSE)

cat("✅ Data saved to:\n")
cat("   - data/stereology_vessel_data.rda\n")
cat("   - data/stereology_vessel_data.csv\n")
