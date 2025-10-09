#!/bin/bash

# ClinicoPath Mermaid Diagram Generator
# This script installs mermaid-cli if needed and converts all .mmd files to PNG images

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if we're in the right directory
if [ ! -f "MERMAID_DIAGRAMS_README.md" ]; then
    print_error "This script must be run from the vignettes directory containing .mmd files"
    print_error "Current directory: $(pwd)"
    print_error "Expected files: *.mmd and MERMAID_DIAGRAMS_README.md"
    exit 1
fi

print_status "ClinicoPath Mermaid Diagram Generator Starting..."
print_status "Working directory: $(pwd)"

# Check if Node.js is installed
if ! command -v node &> /dev/null; then
    print_error "Node.js is not installed. Please install Node.js first:"
    print_error "  macOS: brew install node"
    print_error "  Ubuntu/Debian: sudo apt-get install nodejs npm"
    print_error "  Windows: Download from https://nodejs.org/"
    exit 1
fi

print_status "Node.js version: $(node --version)"

# Check if npm is installed
if ! command -v npm &> /dev/null; then
    print_error "npm is not installed. Please install npm first."
    exit 1
fi

print_status "npm version: $(npm --version)"

# Check if mermaid-cli is installed
if ! command -v mmdc &> /dev/null; then
    print_warning "mermaid-cli not found. Installing..."
    
    # Try to install globally
    if npm install -g @mermaid-js/mermaid-cli; then
        print_success "mermaid-cli installed successfully"
    else
        print_error "Failed to install mermaid-cli globally."
        print_warning "Trying to install locally..."
        if npm install @mermaid-js/mermaid-cli; then
            print_success "mermaid-cli installed locally"
            # Use local installation
            MMDC_CMD="npx mmdc"
        else
            print_error "Failed to install mermaid-cli. Please install manually:"
            print_error "  npm install -g @mermaid-js/mermaid-cli"
            exit 1
        fi
    fi
else
    print_success "mermaid-cli already installed"
fi

# Set the command to use
if [ -z "$MMDC_CMD" ]; then
    MMDC_CMD="mmdc"
fi

print_status "Using mermaid command: $MMDC_CMD"

# Create images directory if it doesn't exist
if [ ! -d "images" ]; then
    mkdir -p images
    print_status "Created images directory"
fi

# Count .mmd files
mmd_count=$(ls *.mmd 2>/dev/null | wc -l)
if [ $mmd_count -eq 0 ]; then
    print_error "No .mmd files found in current directory"
    exit 1
fi

print_status "Found $mmd_count .mmd files to convert"

# Convert each .mmd file to PNG
success_count=0
error_count=0

for mmd_file in *.mmd; do
    if [ -f "$mmd_file" ]; then
        # Get filename without extension
        filename=$(basename "$mmd_file" .mmd)
        output_file="images/${filename}.png"
        
        print_status "Converting $mmd_file to $output_file..."
        
        # Convert with optimized high-quality settings for documentation
        # Try highest quality first (some complex diagrams may need fallback)
        if $MMDC_CMD -i "$mmd_file" \
                     -o "$output_file" \
                     -t default \
                     -b transparent \
                     -w 1800 \
                     -s 2 \
                     --quiet 2>/dev/null; then
            print_success "✓ $filename.png created (ultra-high quality: 1800px, 2x scale)"
            ((success_count++))
        elif $MMDC_CMD -i "$mmd_file" \
                       -o "$output_file" \
                       -t default \
                       -b transparent \
                       -w 1600 \
                       -s 1.8 \
                       --quiet 2>/dev/null; then
            print_success "✓ $filename.png created (high quality: 1600px, 1.8x scale)"
            ((success_count++))
        elif $MMDC_CMD -i "$mmd_file" \
                       -o "$output_file" \
                       -t default \
                       -b transparent \
                       -w 1400 \
                       -s 1.5 \
                       --quiet 2>/dev/null; then
            print_success "✓ $filename.png created (good quality: 1400px, 1.5x scale)"
            ((success_count++))
        else
            print_warning "Complex diagram detected, using adaptive quality settings..."
            # Final fallback: let mermaid determine optimal size
            if $MMDC_CMD -i "$mmd_file" \
                         -o "$output_file" \
                         -t default \
                         -b transparent \
                         --quiet 2>/dev/null; then
                print_success "✓ $filename.png created (adaptive quality)"
                ((success_count++))
            else
                print_error "✗ Failed to convert $mmd_file"
                ((error_count++))
            fi
        fi
    fi
done

echo
print_status "=== Conversion Summary ==="
print_success "$success_count files converted successfully"
if [ $error_count -gt 0 ]; then
    print_warning "$error_count files failed to convert"
fi

# List generated files with dimensions
if [ $success_count -gt 0 ]; then
    echo
    print_status "Generated files with dimensions:"
    for png_file in images/*.png; do
        if [ -f "$png_file" ]; then
            filename=$(basename "$png_file")
            filesize=$(ls -lh "$png_file" | awk '{print $5}')
            # Try to get dimensions (works on macOS and Linux with file command)
            dimensions=$(file "$png_file" 2>/dev/null | grep -o '[0-9]* x [0-9]*' | head -1)
            if [ -n "$dimensions" ]; then
                echo "  ✓ $filename ($dimensions, $filesize)"
            else
                echo "  ✓ $filename ($filesize)"
            fi
        fi
    done
fi

# Check if all expected files were created
expected_files=(
    "01-overall-data-flow.png"
    "02-jamovi-4file-architecture.png" 
    "03-component-interaction-sequence.png"
    "04-stagemigration-data-processing.png"
    "05-stagemigration-component-flow.png"
    "06-stagemigration-detailed-interaction.png"
    "07-a-yaml-options-flow.png"
    "08-option-type-decision-tree.png"
    "09-results-yaml-mapping.png"
    "10-results-organization-pattern.png"
)

echo
print_status "=== Checking Expected Files ==="
missing_count=0
for expected in "${expected_files[@]}"; do
    if [ -f "images/$expected" ]; then
        print_success "✓ $expected"
    else
        print_warning "✗ $expected (missing)"
        ((missing_count++))
    fi
done

if [ $missing_count -eq 0 ]; then
    echo
    print_success "🎉 All diagrams generated successfully!"
    print_status "You can now build the vignette and the diagrams will display correctly."
    print_status "To build the vignette:"
    print_status "  R -e \"rmarkdown::render('module-development-jamovi.Rmd')\""
else
    echo
    print_warning "⚠️  $missing_count expected files are missing."
    print_status "You may need to check the .mmd files or conversion process."
fi

echo
print_status "Diagram generation complete!"