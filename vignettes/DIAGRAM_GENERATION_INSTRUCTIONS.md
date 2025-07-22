# ClinicoPath Diagram Generation Instructions

This directory contains an automated script to generate PNG images from mermaid diagrams for the ClinicoPath module development vignette.

## Quick Start

```bash
# Navigate to the vignettes directory
cd vignettes

# Run the diagram generation script
./generate_diagrams.sh
```

## What the Script Does

1. **Checks Prerequisites**: Verifies Node.js and npm are installed
2. **Installs mermaid-cli**: If not already installed (globally or locally)
3. **Creates Images Directory**: Sets up `images/` folder if it doesn't exist
4. **Converts All .mmd Files**: Automatically finds and converts all mermaid files
5. **Validates Output**: Checks that all expected diagram files were created

## Generated Files

The script converts these mermaid files to PNG images:

- `01-overall-data-flow.mmd` → `images/01-overall-data-flow.png`
- `02-jamovi-4file-architecture.mmd` → `images/02-jamovi-4file-architecture.png`
- `03-component-interaction-sequence.mmd` → `images/03-component-interaction-sequence.png`
- `04-stagemigration-data-processing.mmd` → `images/04-stagemigration-data-processing.png`
- `05-stagemigration-component-flow.mmd` → `images/05-stagemigration-component-flow.png`
- `06-stagemigration-detailed-interaction.mmd` → `images/06-stagemigration-detailed-interaction.png`
- `07-a-yaml-options-flow.mmd` → `images/07-a-yaml-options-flow.png`
- `08-option-type-decision-tree.mmd` → `images/08-option-type-decision-tree.png`
- `09-results-yaml-mapping.mmd` → `images/09-results-yaml-mapping.png`
- `10-results-organization-pattern.mmd` → `images/10-results-organization-pattern.png`

## Troubleshooting

### Node.js Not Found
```bash
# macOS (with Homebrew)
brew install node

# Ubuntu/Debian
sudo apt-get install nodejs npm

# Windows
# Download from https://nodejs.org/
```

### Permission Denied
```bash
chmod +x generate_diagrams.sh
```

### Manual mermaid-cli Installation
```bash
npm install -g @mermaid-js/mermaid-cli
```

## Building the Vignette

After generating the diagrams, you can build the vignette:

```r
# In R
rmarkdown::render('module-development-jamovi.Rmd')
```

or

```bash
# From command line
R -e "rmarkdown::render('module-development-jamovi.Rmd')"
```

## Re-running the Script

The script can be run multiple times safely. It will:
- Skip installation if mermaid-cli is already available
- Overwrite existing PNG files with updated versions
- Report on success/failure for each diagram

## Advanced Usage

### Manual Conversion
If you need to convert individual files:

```bash
mmdc -i 01-overall-data-flow.mmd -o images/01-overall-data-flow.png
```

### Ultra-High Quality Output Settings
The script uses these optimized settings with intelligent quality fallbacks:

**Primary (Ultra-High Quality)** ✅ **Achieved**:
- **Width**: 1800px base (scales to ~2300-3500px actual)
- **Scale**: 2x (true retina/high-DPI quality)
- **Background**: Transparent
- **Theme**: Default mermaid theme  
- **Format**: PNG
- **Result**: 2000x2000+ pixel diagrams, 100-400KB files

**Fallback Levels (if needed)**:
1. **High Quality**: 1600px, 1.8x scale
2. **Good Quality**: 1400px, 1.5x scale  
3. **Adaptive**: Let mermaid optimize automatically

### Actual Quality Achieved
Recent generation produced these dimensions:
- `01-overall-data-flow.png`: **2310 x 2000** pixels (139KB)
- `04-stagemigration-data-processing.png`: **2292 x 4540** pixels (312KB)  
- `07-a-yaml-options-flow.png`: **2922 x 1720** pixels (353KB)
- `09-results-yaml-mapping.png`: **3568 x 1804** pixels (391KB)

These are **ultra-high resolution** images perfect for:
- ✅ **Retina displays** - Crystal clear on high-DPI screens
- ✅ **Print quality** - Suitable for academic publications
- ✅ **Zoom friendly** - Maintain clarity when enlarged
- ✅ **Professional docs** - Publication-grade quality

## Script Features

✅ **Automatic Prerequisites Check**: Verifies Node.js and npm  
✅ **Intelligent Installation**: Tries global, falls back to local  
✅ **Robust Error Handling**: Continues on individual file failures  
✅ **Colored Output**: Easy to see status, success, warnings, and errors  
✅ **Progress Reporting**: Shows conversion status for each file  
✅ **Validation**: Confirms all expected files were created  
✅ **Cross-Platform**: Works on macOS, Linux, and Windows (with bash)  

The generated PNG files will be automatically referenced by the `module-development-jamovi.Rmd` vignette, providing high-quality visual documentation for ClinicoPath developers.