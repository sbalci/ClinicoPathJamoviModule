# Quick Font Setup for Economist Plots in jamovi

## TL;DR - Quick Setup

### Option 1: Use Pre-installed Fonts (Fastest)
✅ **No installation needed** - The module automatically uses **Verdana** (excellent substitute) or **Arial** if available on your system.

### Option 2: Install Free Alternative (Recommended)
1. Download **IBM Plex Sans** from [Google Fonts](https://fonts.google.com/specimen/IBM+Plex+Sans)
2. Install on your system (double-click font files)
3. Restart jamovi
4. ✅ Enjoy enhanced Economist-style typography!

### Option 3: Premium Fonts (Optimal)
- **ITC Officina Sans** (The Economist's actual font - requires license)
- **Economist Sans** fonts (proprietary)

## Font Installation by Operating System

### macOS
```bash
# Download fonts, then:
# Double-click each .ttf/.otf file → "Install Font"
# Restart jamovi
```

### Windows
```bash
# Download fonts, then:
# Right-click each .ttf/.otf file → "Install"
# Restart jamovi
```

### Linux
```bash
mkdir -p ~/.fonts
cp *.ttf *.otf ~/.fonts/
fc-cache -fv
# Restart jamovi
```

## How the jamovi Module Handles Fonts

The `economistplots` module in ClinicoPath automatically:

1. **Detects Available Fonts**: Scans your system for optimal fonts
2. **Provides Feedback**: Shows current font status in the analysis
3. **Graceful Fallback**: Uses best available font automatically
4. **Installation Guidance**: Gives specific recommendations when optimal fonts aren't found

### Font Priority (Best to Acceptable)
1. ✨ **ITC Officina Sans** (The Economist's font)
2. ✨ **EconSansCndReg** (Economist Sans)
3. ⭐ **IBM Plex Sans** (free, excellent alternative)
4. ⭐ **Verdana** (very good substitute, usually pre-installed)
5. ✅ **Arial/Helvetica** (acceptable fallbacks)
6. ✅ **sans** (system default)

## Verification

After installation, the module will automatically detect and use the best available font. You'll see font status information in the analysis results.

## Need Help?

- **Font not detected**: Restart jamovi after installation
- **Still having issues**: The module works perfectly with system defaults
- **Want perfect Economist style**: Install IBM Plex Sans (free) or ITC Officina Sans (commercial)

The plots look great with any font - the enhanced typography is just a bonus! 🎨