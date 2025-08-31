const events = {
    // Color palette change with accessible options
    onChange_colorPalette: function(ui) {
        let palette = ui.color_palette.value();
        let engine = ui.plot_engine.value();
        
        switch(palette) {
            case "standard":
                if (engine === "ggcharts") {
                    ui.color1.setValue("#FF69B4");  // Hot pink for Female
                    ui.color2.setValue("#4169E1");  // Royal blue for Male
                } else {
                    ui.color1.setValue("#F8766D");  // ggplot2 red/pink for Female
                    ui.color2.setValue("#00BFC4");  // ggplot2 teal for Male
                }
                break;
                
            case "accessible":
                // Colorblind-friendly palette (Okabe-Ito colors)
                ui.color1.setValue("#E69F00");  // Orange for Female
                ui.color2.setValue("#56B4E9");  // Sky blue for Male
                break;
                
            case "custom":
                // Keep current colors when switching to custom
                break;
                
            default:
                ui.color1.setValue("#1F77B4");  // Default blue
                ui.color2.setValue("#FF7F0E");  // Default orange
                break;
        }
    },

    // Plot engine change with dynamic color configuration for age pyramids
    onChange_plotEngine: function(ui) {
        let engine = ui.plot_engine.value();
        let palette = ui.color_palette.value();
        
        // Only update colors if not using custom palette
        if (palette !== "custom") {
            this.onChange_colorPalette(ui);
        }
    },

    // Color validation and feedback
    onChange_color1: function(ui) {
        let color = ui.color1.value();
        // Basic color validation - ensure it's a valid hex color or named color
        if (color && !color.match(/^#[0-9A-Fa-f]{6}$/) && !color.match(/^[a-zA-Z]+$/)) {
            // Could add validation feedback here in future
        }
    },

    onChange_color2: function(ui) {
        let color = ui.color2.value();
        // Basic color validation - ensure it's a valid hex color or named color
        if (color && !color.match(/^#[0-9A-Fa-f]{6}$/) && !color.match(/^[a-zA-Z]+$/)) {
            // Could add validation feedback here in future
        }
    },

    // Bin width validation with guidance
    onChange_binWidth: function(ui) {
        let binWidth = ui.bin_width.value();
        
        if (binWidth !== null) {
            if (binWidth <= 0) {
                // Reset to default if invalid
                ui.bin_width.setValue(5);
            } else if (binWidth < 1) {
                // Warn about very small bin widths
                // Could add user feedback here
            } else if (binWidth > 20) {
                // Warn about very large bin widths
                // Could add user feedback here
            }
        }
    }
};

module.exports = events;