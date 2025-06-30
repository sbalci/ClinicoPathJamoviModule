# Economist Fonts Installation Guide for jamovi

This guide helps you install The Economist's fonts for optimal use with the `economistplots` module in jamovi.

## Required Fonts

The ggeconodist package and The Economist visualizations use these fonts (in order of preference):

1. **ITC Officina Sans** (The Economist's primary font)
2. **EconSansCndReg** (Economist Sans Condensed Regular)
3. **EconSansCndLig** (Economist Sans Condensed Light)
4. **EconSansCndBol** (Economist Sans Condensed Bold)
5. **Verdana** (Good substitute, usually pre-installed)
6. **Arial/Helvetica** (Fallback fonts)

## Installation Methods

### Method 1: System-Wide Installation (Recommended)

#### macOS
1. Download the font files (.ttf or .otf format)
2. Double-click each font file
3. Click "Install Font" in Font Book
4. Restart jamovi after installation

#### Windows
1. Download the font files (.ttf or .otf format)
2. Right-click each font file
3. Select "Install" or "Install for all users"
4. Restart jamovi after installation

#### Linux (Ubuntu/Debian)
```bash
# Create fonts directory if it doesn't exist
mkdir -p ~/.fonts

# Copy font files to the directory
cp *.ttf ~/.fonts/
cp *.otf ~/.fonts/

# Update font cache
fc-cache -fv

# Restart jamovi
```

### Method 2: Font Sources

#### Option A: Official Economist Fonts
- **Note**: The Economist's fonts are proprietary
- Contact The Economist for licensing information
- May require commercial license for professional use

#### Option B: Alternative Sources
1. **Google Fonts Alternatives**:
   - IBM Plex Sans Condensed (similar to Economist Sans)
   - Source Sans Pro Condensed
   - Roboto Condensed

2. **Download alternatives**:
   ```bash
   # Install IBM Plex Sans as alternative
   # Go to fonts.google.com/specimen/IBM+Plex+Sans
   # Download and install following steps above
   ```

#### Option C: Use Pre-installed Fonts
The module automatically falls back to these system fonts:
- **Verdana** (excellent substitute)
- **Arial**
- **Helvetica**
- **sans** (system default)

### Method 3: Verify Installation in R

You can check if fonts are available in your R environment:

```r
# Check available system fonts
if (requireNamespace("systemfonts", quietly = TRUE)) {
  available_fonts <- systemfonts::system_fonts()
  
  # Look for Economist-style fonts
  economist_fonts <- c("ITC Officina Sans", "EconSansCndReg", "Economist Sans", 
                      "IBM Plex Sans", "Verdana", "Arial")
  
  found_fonts <- available_fonts$family[available_fonts$family %in% economist_fonts]
  print("Available Economist-style fonts:")
  print(found_fonts)
}

# Alternative check using extrafont
if (requireNamespace("extrafont", quietly = TRUE)) {
  extrafont::fonts()
}
```

## jamovi-Specific Considerations

### Font Detection in economistplots Module

The ClinicoPath `economistplots` module includes automatic font detection:

1. **Automatic Fallback**: The module checks for available fonts and uses the best option
2. **User Instructions**: When optimal fonts aren't found, the module provides installation guidance
3. **Quality Assurance**: Even with fallback fonts, plots maintain Economist-style aesthetics

### Font Priority Order

The module searches for fonts in this order:
1. ITC Officina Sans
2. EconSansCndReg
3. Economist Sans
4. IBM Plex Sans Condensed
5. Verdana
6. Arial
7. Helvetica
8. sans (system default)

## Troubleshooting

### Fonts Not Recognized in jamovi

1. **Restart jamovi**: Font changes require application restart
2. **Check font installation**: Verify fonts are system-wide, not user-specific
3. **Clear font cache** (Linux):
   ```bash
   fc-cache -fv
   ```
4. **Use font verification**: Run R code above to check font availability

### Alternative Solutions

1. **Use Verdana**: Pre-installed on most systems, excellent substitute
2. **Install IBM Plex Sans**: Free alternative from Google Fonts
3. **Accept fallback**: Arial/Helvetica still produce professional-looking plots

### Performance Considerations

- System fonts (Verdana, Arial) load faster than custom fonts
- Custom fonts may require additional rendering time
- jamovi module handles all font loading automatically

## Best Practices

1. **Install system-wide**: Ensures fonts work across all applications
2. **Test in R first**: Verify font availability before using in jamovi
3. **Use fallbacks**: Don't rely exclusively on proprietary fonts
4. **Document fonts**: Note which fonts were used for reproducibility

## Legal Considerations

- **ITC Officina Sans**: Commercial font, requires license
- **Economist Sans**: Proprietary to The Economist
- **IBM Plex Sans**: Open source, free to use
- **Verdana/Arial**: System fonts, generally safe to use

## Support

If fonts still don't work after following this guide:

1. Check the jamovi console for font-related warnings
2. Verify R can detect the fonts using code above
3. Try using Verdana or Arial as alternatives
4. Contact ClinicoPath module support with font detection results

## Updates

The `economistplots` module will be updated to provide better font detection feedback and installation guidance based on user feedback.