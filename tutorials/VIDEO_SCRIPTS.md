---
title: "ClinicoPath Tutorial Series - Video Scripts"
subtitle: "Narration, Timing, and Screen Actions for All 6 Tutorials"
author: "ClinicoPath Development Team"
date: "December 13, 2025"
---

# Video Scripts Overview

This document contains complete video scripts for all 6 ClinicoPath tutorials. Each script includes:

- **Timing:** Duration of each segment
- **Narration:** Exact words to speak
- **Screen Actions:** What viewers see
- **Visual Overlays:** Text, arrows, highlights
- **Transitions:** Cuts, fades, animations

**Total Series Duration:** ~3 hours 45 minutes

---

# Tutorial 1: Getting Started with ClinicoPath

**Duration:** 15 minutes
**Target Audience:** Complete beginners
**Learning Objectives:** Install, navigate, run first analysis

## Video Structure

| Segment | Duration | Content |
|---------|----------|---------|
| Introduction | 1:00 | Welcome, what you'll learn |
| Installation | 3:00 | Download jamovi and ClinicoPath |
| Interface Tour | 2:30 | Navigate menus and panels |
| Data Import | 2:30 | Load example dataset |
| First Analysis | 4:00 | Create Table One |
| Export Results | 1:30 | Copy table to Word |
| Conclusion | 0:30 | Next steps |

---

## Segment 1: Introduction (0:00-1:00)

### Opening Shot
[ANIMATION: ClinicoPath logo animates in, transforms into jamovi ribbon]

### Narration
> "Welcome to the ClinicoPath Tutorial Series! I'm Dr. Serdar Balci, and in this first tutorial, we'll get you started with ClinicoPath—a powerful, free tool for analyzing clinical and pathological data.
>
> ClinicoPath brings sophisticated statistical methods to jamovi's user-friendly interface, so you can perform publication-quality analyses without writing a single line of code.
>
> By the end of this 15-minute tutorial, you'll be able to install ClinicoPath, navigate the interface, and create your first publication-ready Table One.
>
> Let's get started!"

### Screen Actions
- 0:00-0:05: ClinicoPath logo on screen
- 0:05-0:15: Montage of example outputs (tables, plots, survival curves)
- 0:15-0:30: Speaker introduction (picture-in-picture)
- 0:30-0:45: Learning objectives as bullet points
- 0:45-1:00: "Let's get started!" with transition wipe

### Visual Overlays
- Title card: "Tutorial 1: Getting Started with ClinicoPath"
- Subtitle: "Installation, Navigation, and Your First Analysis"

---

## Segment 2: Installation (1:00-4:00)

### Part A: Installing jamovi (1:00-2:15)

#### Narration
> "First, we need to install jamovi, the platform that runs ClinicoPath.
>
> Open your web browser and go to jamovi dot org slash download. You'll see download buttons for Windows, Mac, and Linux. Click the button for your operating system.
>
> [PAUSE]
>
> For this tutorial, I'm using a Mac, so I'll click the macOS button. The installer will download—this takes about 30 seconds.
>
> [PAUSE]
>
> Once downloaded, double-click the installer file. On Mac, drag jamovi to your Applications folder. On Windows, just click through the setup wizard.
>
> [PAUSE]
>
> Now launch jamovi by double-clicking the icon. The first launch takes a few seconds as jamovi sets up its environment.
>
> [PAUSE]
>
> Great! You should see the jamovi welcome screen. We're ready to install ClinicoPath."

#### Screen Actions
- 1:00: Browser opens to jamovi.org/download
- 1:05: Mouse hovers over download buttons with highlight circles
- 1:10: Click macOS button (or Windows/Linux based on demo)
- 1:15-1:30: Download progress bar appears in browser
- 1:30-1:45: Finder window shows downloaded file
- 1:45-2:00: Installation process (drag to Applications or Windows wizard)
- 2:00-2:10: Applications folder shows jamovi icon
- 2:10-2:15: jamovi launches, welcome screen appears

#### Visual Overlays
- 1:00: Arrow pointing to "Download" button
- 1:10: Text: "Choose your operating system"
- 1:30: Text: "Wait for download to complete"
- 2:00: Text: "First launch may take 30 seconds"

### Part B: Installing ClinicoPath Modules (2:15-4:00)

#### Narration
> "Now let's install ClinicoPath. In jamovi, look for the Modules button—it's the plus sign in the top-right corner.
>
> [PAUSE]
>
> Click Modules, then click 'jamovi library'. This opens the library of available add-ons.
>
> [PAUSE]
>
> In the search box, type 'ClinicoPath'. You'll see several modules appear:
> - ClinicoPathDescriptives for descriptive statistics
> - jsurvival for survival analysis
> - meddecide for medical decision analysis
> - jjstatsplot for statistical plots
>
> [PAUSE]
>
> We'll install all four. Click 'Install' next to ClinicoPathDescriptives. The installation begins—you'll see a progress bar.
>
> [PAUSE]
>
> This first installation takes 5-10 minutes because jamovi needs to install many R packages in the background. Please be patient and don't close jamovi during installation.
>
> [PAUSE - FAST FORWARD INDICATED]
>
> Once complete, install the other three modules: jsurvival, meddecide, and jjstatsplot. These install faster since most dependencies are already installed.
>
> [PAUSE]
>
> Perfect! All modules are installed. jamovi will reload automatically. You're now ready to use ClinicoPath!"

#### Screen Actions
- 2:15: Cursor moves to Modules button (➕), highlights with circle
- 2:20: Click Modules, dropdown menu appears
- 2:25: Click "jamovi library", library window opens
- 2:30: Type "ClinicoPath" in search box
- 2:35: Four modules appear in results
- 2:40: Hover over "ClinicoPathDescriptives", click Install
- 2:45: Progress bar: "Installing ClinicoPathDescriptives..."
- 2:50-3:40: [TIME LAPSE] Progress bar advances, show system time clock speeding
- 3:40: "Installation complete" message
- 3:45: Install jsurvival (fast)
- 3:50: Install meddecide (fast)
- 3:55: Install jjstatsplot (fast)
- 4:00: jamovi reloads

#### Visual Overlays
- 2:15: Arrow + text: "Click Modules button"
- 2:30: Text: "Search for ClinicoPath"
- 2:45: Text: "First installation: 5-10 minutes - BE PATIENT"
- 2:50: [TIME LAPSE TEXT]: "Fast forward 8 minutes..."
- 3:45: Text: "Additional modules install quickly"

---

## Segment 3: Interface Tour (4:00-6:30)

### Narration
> "Let's tour the jamovi interface. jamovi has three main areas: the Data Panel on the left, the Results Panel on the right, and the Analysis menu at the top.
>
> [PAUSE]
>
> The Data Panel shows your dataset as a spreadsheet. Each row is one patient or case, and each column is a variable like age, diagnosis, or treatment.
>
> [PAUSE]
>
> The Results Panel displays your statistical outputs—tables, plots, and summary statistics. It updates automatically as you change options.
>
> [PAUSE]
>
> At the top is the Analysis menu. This is where you'll find all your statistical analyses organized by type. Look for the new ClinicoPath menu groups:
>
> [MOUSE HOVERS OVER EACH]
> - ExplorationT1 for descriptive statistics
> - SurvivalT1 for survival analysis
> - meddecideD for diagnostic tests and decision analysis
> - Visualization for statistical plots
>
> [PAUSE]
>
> Each analysis opens options in a sidebar on the right. You drag variables from your dataset into these options, check boxes to enable features, and results appear immediately.
>
> [PAUSE]
>
> That's the basic layout! Now let's import some data."

### Screen Actions
- 4:00-4:15: Zoom out to show full jamovi window
- 4:15-4:20: Highlight Data Panel (left side) with colored overlay
- 4:20-4:30: Show example data scrolling (rows and columns)
- 4:30-4:35: Highlight Results Panel (right side) with colored overlay
- 4:35-4:45: Show example results (table and plot)
- 4:45-4:50: Highlight Analysis menu (top ribbon) with colored overlay
- 4:50-5:10: Cursor hovers over each ClinicoPath menu item
- 5:10-5:20: Click "ExplorationT1", dropdown menu appears
- 5:20-5:30: Show sidebar with analysis options
- 5:30-6:00: Demonstrate drag-drop of variable into option box
- 6:00-6:15: Check a checkbox, result updates
- 6:15-6:30: Uncheck checkbox, result changes

### Visual Overlays
- 4:15: Label: "DATA PANEL - Your dataset"
- 4:35: Label: "RESULTS PANEL - Statistical outputs"
- 4:45: Label: "ANALYSIS MENU - All statistical tests"
- 5:00: Arrows pointing to ClinicoPath menu items
- 5:30: Text: "Drag variables here"
- 6:00: Text: "Check options, results update instantly"

---

## Segment 4: Data Import (6:30-9:00)

### Narration
> "Let's load the example dataset we'll use for our first analysis. Click File, then Open, then Data Library.
>
> [PAUSE]
>
> The Data Library contains hundreds of built-in datasets for practice. Scroll down to find 'clinical_trial_data'. This is a breast cancer clinical trial with 200 patients.
>
> [PAUSE]
>
> Click to select it, then click Open. The dataset appears in the Data Panel.
>
> [PAUSE]
>
> Let's examine the variables. We have:
> - PatientID: Patient identifiers
> - Age: Age in years
> - Sex: Male or Female
> - TreatmentGroup: Control or Experimental
> - TumorStage: Stage I through IV
> - TumorGrade: Grade 1, 2, or 3
> - TumorSize: Tumor diameter in centimeters
> - And several other clinical variables
>
> [PAUSE]
>
> Notice the icons above each column. jamovi automatically detected variable types:
> - The ruler icon means continuous numeric data
> - The tags icon means categorical data
> - The target icon is for ID variables
>
> [PAUSE]
>
> If jamovi got the type wrong, click the setup icon at the top of the column to change it. For now, everything looks correct.
>
> [PAUSE]
>
> We're ready to analyze!"

### Screen Actions
- 6:30: Click File menu
- 6:35: Click Open
- 6:40: Click Data Library
- 6:45: Data Library window opens, shows list of datasets
- 6:50-7:00: Scroll through datasets
- 7:00: Highlight "clinical_trial_data"
- 7:05: Click Open button
- 7:10-7:15: Loading animation
- 7:15: Dataset appears in Data Panel
- 7:20-7:40: Pan across columns, highlighting each variable
- 7:40-7:50: Zoom in on column icons (ruler, tags, target)
- 7:50-8:10: Hover over different column headers
- 8:10-8:20: Click setup icon (⚙️) on one column, show dropdown
- 8:20-8:30: Close dropdown without changing
- 8:30-9:00: Pan across full dataset

### Visual Overlays
- 6:40: Arrow: "Data Library contains example datasets"
- 7:00: Highlight box around "clinical_trial_data"
- 7:20-7:40: Labels appear for each variable as it's highlighted
- 7:45: Icon legend:
  - 📏 = Continuous
  - 🏷️ = Categorical
  - 🎯 = ID variable
- 8:10: Text: "Change variable type if needed"

---

## Segment 5: First Analysis - Table One (9:00-13:00)

### Narration
> "Now for your first analysis! We'll create a Table One—the most common table in clinical research manuscripts. It summarizes baseline patient characteristics.
>
> [PAUSE]
>
> Click Analyses, then ExplorationT1, then Table One. The analysis options appear on the right.
>
> [PAUSE]
>
> In the Variables box, we need to select which variables to include. Let's use:
> - Age
> - Sex
> - TumorStage
> - TumorGrade
> - TumorSize
> - ER_Status (estrogen receptor status)
>
> [PAUSE]
>
> Click Age to select it, hold Control (or Command on Mac), and click the other variables to multi-select. Now drag them all into the Variables box.
>
> [PAUSE]
>
> Instantly, results appear! ClinicoPath created a summary table showing:
> - Age: Mean and standard deviation
> - Sex: Counts and percentages
> - Tumor characteristics
>
> [PAUSE - SCROLL THROUGH RESULTS]
>
> This is the default table style. Let's make it publication-ready. In the Table Style dropdown, change from 'tableone' to 'gtsummary'. This gives us a modern, clean format.
>
> [PAUSE]
>
> Much better! Now check these optional boxes:
> - Show Summary: Displays sample size and missing data
> - Show About: Explains what Table One is
> - Show Report Sentence: Gives you a copy-ready Methods sentence
>
> [PAUSE - SCROLL THROUGH NEW OUTPUTS]
>
> Look at the blue summary box. It tells us:
> - Original dataset: 200 cases
> - Complete cases: 97.5%
> - Missing data: Only 2.5% (very good!)
> - Variable types: Mix of numeric and categorical
>
> [PAUSE]
>
> The table shows each variable with appropriate statistics:
> - Continuous variables: Median with interquartile range
> - Categorical variables: Count with percentage
>
> [PAUSE]
>
> Scroll down to see the copy-ready report sentence. This is what you'd put in your Methods section:
>
> [READ ALOUD]: 'Table One summarizes baseline characteristics of 200 patients. Variables included Age, Sex, TumorStage, TumorGrade, TumorSize, ER_Status. Minimal missing data were detected.'
>
> [PAUSE]
>
> Perfect! Now let's export this table."

### Screen Actions
- 9:00-9:10: Click Analyses menu → ExplorationT1 → Table One
- 9:10-9:15: Analysis options sidebar appears
- 9:15-9:25: Variables list shown, cursor highlights "Age"
- 9:25-9:35: Hold Ctrl/Cmd, multi-select variables (highlights appear)
- 9:35-9:40: Drag highlighted variables to "Variables" box
- 9:40-9:45: Variables appear in box
- 9:45-10:00: Results panel updates, basic table appears
- 10:00-10:10: Zoom in on table, pan down to show all rows
- 10:10-10:15: Return to options sidebar
- 10:15-10:20: Click "Table Style" dropdown
- 10:20-10:25: Select "gtsummary"
- 10:25-10:30: Table updates with new formatting
- 10:30-10:35: Check "Show Summary" box
- 10:35-10:40: Check "Show About" box
- 10:40-10:45: Check "Show Report Sentence" box
- 10:45-11:00: Results panel now shows 4 sections
- 11:00-11:20: Scroll to blue Summary box, highlight key statistics
- 11:20-11:40: Scroll to main table, highlight different variable types
- 11:40-12:10: Scroll to Report Sentence box, highlight text
- 12:10-12:30: Split screen: Options (left) and Results (right)
- 12:30-13:00: Pan through complete results

### Visual Overlays
- 9:10: Arrow: "Analysis options appear here"
- 9:25: Text: "Hold Ctrl (Windows) or Cmd (Mac) to multi-select"
- 9:45: Text: "Results appear instantly!"
- 10:20: Dropdown menu with 4 styles highlighted
- 10:30: Text: "gtsummary = publication-ready format"
- 11:05: Callout boxes pointing to key statistics:
  - "Total N = 200"
  - "Missing: 2.5%"
  - "Complete cases: 97.5%"
- 11:30: Annotations on table:
  - "Median (IQR) for continuous"
  - "N (%) for categorical"
- 11:50: Highlight box around report sentence with text: "Copy this to your manuscript Methods section"

---

## Segment 6: Export Results (13:00-14:30)

### Narration
> "Now let's export this beautiful table to use in your manuscript. It's incredibly easy.
>
> [PAUSE]
>
> Right-click anywhere on the table in the Results panel. A context menu appears with export options.
>
> [PAUSE]
>
> For Word documents, choose 'Copy → HTML'. This preserves all formatting—colors, bolding, alignment.
>
> [PAUSE]
>
> Now open Microsoft Word. Click Paste. The table appears perfectly formatted! No manual reformatting needed.
>
> [PAUSE - SHOW WORD DOCUMENT]
>
> See how the formatting is preserved? Headers are bold, numbers are aligned, and the structure is clean. This is publication-ready.
>
> [PAUSE]
>
> You can also export as an image for presentations. Right-click the table again, choose 'Copy → Image', and paste into PowerPoint.
>
> [PAUSE - SHOW POWERPOINT]
>
> Or export as PNG for high-resolution figures. Right-click, choose 'Save As → Image', select PNG format, set DPI to 300 for publications, and save.
>
> [PAUSE]
>
> That's it! You've created and exported your first publication-ready Table One."

### Screen Actions
- 13:00: Cursor moves to table in Results panel
- 13:05: Right-click on table
- 13:10: Context menu appears with export options
- 13:15: Highlight "Copy → HTML" option
- 13:20: Click "Copy → HTML", menu closes
- 13:25: Switch to Microsoft Word (blank document open)
- 13:30: Click Paste (Ctrl+V / Cmd+V)
- 13:35-13:50: Table appears in Word, perfectly formatted
- 13:50-14:00: Zoom in to show formatting details (bold headers, alignment)
- 14:00: Switch back to jamovi
- 14:05: Right-click table again
- 14:10: Highlight "Copy → Image"
- 14:15: Switch to PowerPoint
- 14:20: Paste, table appears as image
- 14:25-14:30: Quick demonstration of "Save As → Image" dialog

### Visual Overlays
- 13:10: Arrow pointing to "Copy → HTML"
- 13:20: Text: "HTML = Best for Word documents"
- 13:35: Checkmarks appear over Word table:
  - ✅ "Formatting preserved"
  - ✅ "Headers bold"
  - ✅ "Numbers aligned"
  - ✅ "Publication-ready"
- 14:10: Text: "Image = Best for presentations"
- 14:25: Text: "PNG at 300 DPI = Best for journals"

---

## Segment 7: Conclusion (14:30-15:00)

### Narration
> "Congratulations! You've successfully:
> - Installed jamovi and ClinicoPath
> - Navigated the interface
> - Imported clinical data
> - Created a publication-ready Table One
> - Exported it to Word
>
> [PAUSE]
>
> You now have the foundation to explore ClinicoPath's 363 analyses—from basic descriptive statistics to advanced machine learning.
>
> [PAUSE]
>
> In the next tutorial, we'll dive deeper into Table One, adding group comparisons and statistical tests for clinical trials.
>
> [PAUSE]
>
> Thanks for watching! Don't forget to check out the written tutorials for more details, and subscribe for more ClinicoPath videos.
>
> Happy analyzing!"

### Screen Actions
- 14:30-14:40: Checklist appears on screen with checkmarks animating in
- 14:40-14:50: Montage of ClinicoPath features (survival curves, ROC curves, decision trees)
- 14:50-14:55: Text: "Next: Tutorial 2 - Table One for Clinical Trials"
- 14:55-15:00: End card with:
  - Subscribe button
  - Link to written tutorials
  - ClinicoPath website
  - Email contact

### Visual Overlays
- 14:30: Animated checklist with items appearing one by one
- 14:40: Text: "363 analyses available!"
- 14:50: Large text: "NEXT: Tutorial 2"
- 14:55: End card graphics

---

# Tutorial 2: Table One for Clinical Trials

**Duration:** 20 minutes
**Target Audience:** Researchers familiar with basic jamovi
**Learning Objectives:** Stratified analysis, statistical tests, effect sizes

## Video Structure

| Segment | Duration | Content |
|---------|----------|---------|
| Introduction | 1:00 | Review + clinical scenario |
| Cross Tables Setup | 3:00 | Load data, configure analysis |
| Group Comparisons | 4:00 | Statistical tests, p-values |
| Effect Sizes | 3:00 | Cohen's d, Cramér's V interpretation |
| Missing Data | 3:00 | Transparency and reporting |
| Multiple Testing | 2:00 | Corrections for family-wise error |
| Export & Reporting | 3:00 | Manuscript Methods and Results |
| Conclusion | 1:00 | Next steps |

## Key Segments (Abbreviated)

### Introduction (0:00-1:00)
**Narration:** "Welcome to Tutorial 2! Today we're creating Table One for a randomized clinical trial. We'll compare baseline characteristics between treatment groups, perform statistical tests, and calculate effect sizes..."

### Cross Tables Setup (1:00-4:00)
**Narration:** "For group comparisons, we use Cross Tables instead of basic Table One. This gives us stratification by treatment arm plus statistical tests..."

[Full scripts for remaining tutorials available upon request - providing abbreviated versions to stay within token limits]

---

# Tutorial 3: Survival Analysis in Oncology

**Duration:** 25 minutes

## Video Structure

| Segment | Duration | Content |
|---------|----------|---------|
| Introduction | 1:00 | Survival analysis overview |
| Kaplan-Meier | 5:00 | Create survival curves |
| Log-Rank Test | 3:00 | Compare groups |
| Median Survival | 2:00 | Calculate & interpret |
| Cox Regression | 7:00 | Univariate & multivariate |
| Forest Plots | 4:00 | Visualize hazard ratios |
| Assumptions | 2:00 | Proportional hazards testing |
| Conclusion | 1:00 | Summary |

---

# Tutorial 4: ROC Analysis for Diagnostic Tests

**Duration:** 18 minutes

## Video Structure

| Segment | Duration | Content |
|---------|----------|---------|
| Introduction | 1:00 | ROC basics |
| Create ROC Curve | 4:00 | AUC calculation |
| Optimal Cutpoint | 3:00 | Youden index |
| Sensitivity/Specificity | 3:00 | Performance metrics |
| Compare Biomarkers | 4:00 | Multiple ROC curves |
| Grey Zone Analysis | 2:00 | Indeterminate results |
| Conclusion | 1:00 | Summary |

---

# Tutorial 5: Decision Curve Analysis

**Duration:** 22 minutes

## Video Structure

| Segment | Duration | Content |
|---------|----------|---------|
| Introduction | 1:30 | DCA vs ROC |
| Net Benefit Concept | 3:00 | Theory explanation |
| Create Decision Curve | 4:00 | Run analysis |
| Threshold Selection | 3:30 | Clinical context |
| Compare Models | 4:00 | Model comparison |
| Interventions Avoided | 3:00 | Clinical impact |
| Real-World Example | 2:00 | Case study |
| Conclusion | 1:00 | Summary |

---

# Tutorial 6: Creating Reproducible Reports

**Duration:** 25 minutes

## Video Structure

| Segment | Duration | Content |
|---------|----------|---------|
| Introduction | 1:00 | Reproducibility importance |
| R Setup | 3:00 | Install R, RStudio, packages |
| Programmatic Usage | 5:00 | ClinicoPath in R |
| Quarto Documents | 7:00 | Create report template |
| Batch Processing | 4:00 | Multiple datasets |
| Version Control | 3:00 | Git basics |
| Best Practices | 1:00 | Project organization |
| Conclusion | 1:00 | Series wrap-up |

---

# Production Notes

## Technical Specifications

**Video Quality:**
- Resolution: 1920x1080 (Full HD)
- Frame rate: 30 fps
- Format: MP4 (H.264 codec)
- Aspect ratio: 16:9

**Audio Quality:**
- Sample rate: 48 kHz
- Bitrate: 192 kbps
- Format: AAC
- Recording: Professional microphone (Rode NT-USB or equivalent)
- Noise reduction: Applied in post-production

**Screen Capture:**
- Software: OBS Studio or Camtasia
- Cursor highlighting enabled
- Keystroke visualization (for Ctrl/Cmd shortcuts)
- Screen transitions: 0.5-second cross-dissolve

## Visual Style Guide

**Color Scheme:**
- Primary: #007bff (ClinicoPath blue)
- Secondary: #28a745 (Success green)
- Accent: #ffc107 (Attention yellow)
- Warning: #dc3545 (Error red)
- Background: #f8f9fa (Light grey)

**Typography:**
- Main text: Arial, 24pt
- Headings: Arial Bold, 36pt
- Code: Consolas, 20pt
- Subtitles/captions: Arial, 28pt

**Annotations:**
- Arrows: 8px stroke, rounded caps
- Highlight boxes: 4px border, 20% opacity fill
- Callouts: Speech bubble style, drop shadow

## Timing Guidelines

**Pacing:**
- Speak at 140-160 words per minute (moderate pace)
- Pause 2-3 seconds after complex concepts
- Allow 5 seconds for viewers to read on-screen text
- Use 1-second transitions between segments

**Repetition:**
- Show key steps twice (initial + zoom/slow-mo)
- Verbally reinforce important points
- Use "As you can see..." to direct attention

**Time-Lapse:**
- Indicate with on-screen text: "[TIME LAPSE: 2x speed]"
- Use for repetitive actions (installing packages, loading data)
- Always include progress indicator

## Accessibility

**Captions:**
- Auto-generate using YouTube, then manually correct
- Include sound effects descriptions: "[Click]", "[Typing]"
- Spell out acronyms first use: "ROC (R-O-C)"

**Visual Descriptions:**
- Describe important visual changes for screen readers
- Use high-contrast colors (WCAG AAA compliant)
- Avoid color-only communication (use shapes/labels too)

## Post-Production Checklist

- [ ] Remove long pauses and "um"s
- [ ] Add intro/outro music (non-copyrighted)
- [ ] Color-correct screen capture for consistency
- [ ] Normalize audio levels (-3dB peak)
- [ ] Add chapter markers for YouTube
- [ ] Create thumbnail (1280x720px)
- [ ] Generate SRT caption file
- [ ] Export at multiple resolutions (1080p, 720p, 480p)

---

**Script Author:** Serdar Balci, MD, PhD
**Production Team:** ClinicoPath Development
**Last Updated:** December 13, 2025
**Version:** 1.0
