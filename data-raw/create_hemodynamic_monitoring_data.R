# Create synthetic hemodynamic monitoring data for clinmon function testing
# This script creates realistic physiological time series data

library(dplyr)

set.seed(42)

# Create time series (1 hour of data at 1 Hz sampling)
duration_seconds <- 3600  # 1 hour
sampling_freq <- 1  # 1 Hz
n_points <- duration_seconds * sampling_freq

# Time vector
time <- seq(0, duration_seconds - 1, by = 1/sampling_freq)

# Helper function to add realistic noise and artifacts
add_noise <- function(signal, noise_level = 0.02) {
  signal + rnorm(length(signal), mean = 0, sd = sd(signal) * noise_level)
}

# Generate realistic arterial blood pressure (ABP)
# Systolic: ~120 mmHg, Diastolic: ~80 mmHg
# Add respiratory and cardiac variations
respiratory_freq <- 0.25  # 15 breaths per minute
cardiac_freq <- 1.2  # 72 beats per minute

abp_baseline <- 100  # Mean arterial pressure
abp_pulse <- 20  # Pulse pressure amplitude
abp_respiratory <- 5  # Respiratory variation

abp <- abp_baseline + 
       abp_pulse * sin(2 * pi * cardiac_freq * time) +
       abp_respiratory * sin(2 * pi * respiratory_freq * time) +
       rnorm(n_points, 0, 2)  # Random noise

# Ensure ABP stays within physiological range
abp <- pmax(60, pmin(180, abp))

# Generate intracranial pressure (ICP)
# Normal: 5-15 mmHg, with some correlation to ABP
icp_baseline <- 10
icp_variation <- 2
icp_abp_coupling <- 0.1  # Weak coupling to ABP

icp <- icp_baseline + 
       icp_variation * sin(2 * pi * respiratory_freq * time) +
       icp_abp_coupling * (abp - mean(abp)) +
       rnorm(n_points, 0, 1)

# Ensure ICP stays within realistic range
icp <- pmax(0, pmin(40, icp))

# Calculate cerebral perfusion pressure (CPP = ABP - ICP)
cpp <- abp - icp

# Generate middle cerebral artery velocity (MCAv)
# Normal: 30-80 cm/s, inversely related to CPP (autoregulation)
mcav_baseline <- 55
mcav_cpp_coupling <- -0.2  # Inverse relationship (autoregulation)
mcav_variation <- 5

mcav <- mcav_baseline + 
        mcav_cpp_coupling * (cpp - mean(cpp)) +
        mcav_variation * sin(2 * pi * cardiac_freq * time) +
        rnorm(n_points, 0, 2)

# Ensure MCAv stays within physiological range
mcav <- pmax(20, pmin(100, mcav))

# Generate heart rate (HR)
# Normal: 60-100 bpm, with some variability
hr_baseline <- 75
hr_variation <- 5
hr_trend <- 0.001  # Slight increase over time (stress response)

hr <- hr_baseline + 
      hr_variation * sin(2 * pi * 0.1 * time) +  # Slow variation
      hr_trend * time +
      rnorm(n_points, 0, 2)

# Ensure HR stays within reasonable range
hr <- pmax(50, pmin(120, hr))

# Create main hemodynamic monitoring dataset
hemodynamic_monitoring_data <- data.frame(
  time_seconds = time,
  abp_mmhg = round(abp, 1),
  icp_mmhg = round(icp, 1),
  cpp_mmhg = round(cpp, 1),
  mcav_cms = round(mcav, 1),
  hr_bpm = round(hr, 1)
)

# Create a smaller high-frequency dataset (100 Hz for 10 minutes)
duration_hf <- 600  # 10 minutes
freq_hf <- 100  # 100 Hz
n_points_hf <- duration_hf * freq_hf

time_hf <- seq(0, duration_hf - 1/freq_hf, by = 1/freq_hf)

# Generate high-frequency signals with more cardiac detail
cardiac_freq_hf <- 1.2  # 72 bpm
respiratory_freq_hf <- 0.25  # 15 breaths per minute

# High-frequency ABP with clear cardiac cycles
abp_hf <- 100 + 
          25 * sin(2 * pi * cardiac_freq_hf * time_hf) +
          5 * sin(2 * pi * respiratory_freq_hf * time_hf) +
          rnorm(n_points_hf, 0, 1)

# High-frequency ICP
icp_hf <- 12 + 
          2 * sin(2 * pi * respiratory_freq_hf * time_hf) +
          0.5 * sin(2 * pi * cardiac_freq_hf * time_hf) +
          rnorm(n_points_hf, 0, 0.5)

# High-frequency MCAv
mcav_hf <- 60 + 
           10 * sin(2 * pi * cardiac_freq_hf * time_hf) +
           3 * sin(2 * pi * respiratory_freq_hf * time_hf) +
           rnorm(n_points_hf, 0, 1)

hemodynamic_hf_data <- data.frame(
  time_seconds = time_hf,
  abp_mmhg = round(abp_hf, 1),
  icp_mmhg = round(icp_hf, 1),
  cpp_mmhg = round(abp_hf - icp_hf, 1),
  mcav_cms = round(mcav_hf, 1)
)

# Create dataset with pathological patterns
# Simulate raised ICP scenario
pathological_monitoring_data <- hemodynamic_monitoring_data
# Gradually increase ICP over time (simulate developing pathology)
icp_increase <- seq(0, 15, length.out = nrow(pathological_monitoring_data))
pathological_monitoring_data$icp_mmhg <- pathological_monitoring_data$icp_mmhg + icp_increase
pathological_monitoring_data$cpp_mmhg <- pathological_monitoring_data$abp_mmhg - pathological_monitoring_data$icp_mmhg

# Adjust MCAv to show impaired autoregulation
pathological_monitoring_data$mcav_cms <- pathological_monitoring_data$mcav_cms * 
  (1 - 0.3 * (icp_increase / max(icp_increase)))

# Save datasets
usethis::use_data(hemodynamic_monitoring_data, overwrite = TRUE)
usethis::use_data(hemodynamic_hf_data, overwrite = TRUE)
usethis::use_data(pathological_monitoring_data, overwrite = TRUE)

# Print summary
cat("✅ Created hemodynamic monitoring datasets:\n\n")

cat("📊 hemodynamic_monitoring_data:\n")
cat("   - Duration: 1 hour at 1 Hz\n")
cat("   - Variables: time, ABP, ICP, CPP, MCAv, HR\n")
cat("   - Size:", nrow(hemodynamic_monitoring_data), "observations\n\n")

cat("⚡ hemodynamic_hf_data:\n")
cat("   - Duration: 10 minutes at 100 Hz\n")
cat("   - Variables: time, ABP, ICP, CPP, MCAv\n")
cat("   - Size:", nrow(hemodynamic_hf_data), "observations\n\n")

cat("🚨 pathological_monitoring_data:\n")
cat("   - Duration: 1 hour with simulated raised ICP\n")
cat("   - Variables: time, ABP, ICP, CPP, MCAv, HR\n")
cat("   - Size:", nrow(pathological_monitoring_data), "observations\n\n")

cat("💡 Usage examples:\n")
cat("   clinmon(data = hemodynamic_monitoring_data, time_var = 'time_seconds', 
          abp = 'abp_mmhg', icp = 'icp_mmhg', mcav = 'mcav_cms')\n")
cat("   clinmon(data = hemodynamic_hf_data, time_var = 'time_seconds',
          abp = 'abp_mmhg', mcav = 'mcav_cms', freq = 100)\n")