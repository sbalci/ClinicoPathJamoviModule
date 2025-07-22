# ClinicoPath Vignette Images

This directory contains rendered diagrams for the ClinicoPath module development vignette.

## Required Screenshot Files

The following PNG files should be created from the corresponding .mmd files in the parent directory:

1. **01-overall-data-flow.png** - Overall module data flow architecture
2. **02-jamovi-4file-architecture.png** - jamovi 4-file architecture interaction
3. **03-component-interaction-sequence.png** - Component interaction sequence diagram
4. **04-stagemigration-data-processing.png** - ClinicoPath data processing flow
5. **05-stagemigration-component-flow.png** - ClinicoPath component interaction flow
6. **06-stagemigration-detailed-interaction.png** - Detailed stagemigration interaction sequence
7. **07-a-yaml-options-flow.png** - .a.yaml to self$options flow
8. **08-option-type-decision-tree.png** - Option type decision tree
9. **09-results-yaml-mapping.png** - Results YAML mapping
10. **10-results-organization-pattern.png** - Results organization pattern

## How to Create These Images

### Method 1: Mermaid Live Editor (Recommended)
1. Go to https://mermaid.live
2. Copy the content from the corresponding .mmd file
3. Export as PNG with transparent background
4. Save with the exact filename listed above

### Method 2: Mermaid CLI
```bash
npm install -g @mermaid-js/mermaid-cli
mmdc -i ../01-overall-data-flow.mmd -o 01-overall-data-flow.png -t default -b transparent
```

### Method 3: VS Code Extension
1. Install "Mermaid Preview" extension
2. Open .mmd file and export to PNG

## Image Specifications

### High-Quality Settings (Preferred)
- **Format**: PNG with transparent background
- **Width**: 2400px 
- **Height**: 1800px
- **Scale**: 2x (retina/high-DPI quality)
- **Theme**: Default mermaid theme
- **Quality**: Ultra-high resolution for crisp display

### Fallback Settings
- **Width**: 1600px
- **Scale**: 1.5x  
- **Still high quality, good for most displays**

### Benefits of High-Quality Images
- ✅ **Retina Display Ready**: Sharp on high-DPI screens
- ✅ **Print Quality**: Suitable for printed documentation
- ✅ **Zoom Friendly**: Maintain clarity when zoomed
- ✅ **Professional Appearance**: Crisp text and lines

## Note for Developers

Once these images are created and placed in this directory, the ClinicoPath module development vignette will display the diagrams correctly when built.