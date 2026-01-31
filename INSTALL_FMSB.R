# Install fmsb package
# Run this script once to install the missing package

install.packages("fmsb")

# Verify installation
if (require("fmsb", quietly = TRUE)) {
  message("✓ fmsb package installed successfully")
} else {
  stop("✗ fmsb package installation failed")
}
