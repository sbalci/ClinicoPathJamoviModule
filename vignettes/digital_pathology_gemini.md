

# **A Comprehensive Guide to Statistical Methods for a Digital Pathology Jamovi Module**

## **Introduction: The Quantitative Shift in Pathology and the Imperative for Statistical Rigor**

Digital pathology represents a fundamental paradigm shift, transforming the field from a primarily qualitative, descriptive science into a quantitative, data-driven discipline.1 This transformation is rooted in the ability to convert physical glass slides into high-resolution, gigapixel whole slide images (WSIs), which serve as the foundation for computational analysis.1 The journey from a physical tissue specimen to actionable statistical insight involves a sophisticated, multi-stage workflow that generates rich, multi-layered quantitative data, but also introduces new challenges for reproducibility and statistical validity. Understanding this workflow is paramount for developing any meaningful statistical analysis tool.

### **The Digital Pathology Workflow: From Glass to Gigapixels to Quantitative Data**

The process begins with the high-resolution scanning of a glass slide, a step that creates a massive digital file capturing tissue morphology in intricate detail.3 This digitization is the foundational act that unlocks the potential for all subsequent computational analysis, moving beyond the limitations of the traditional microscope.1  
Once the WSI is created, a series of image processing and analysis steps are performed, often powered by advanced artificial intelligence (AI) and deep learning algorithms.1 These steps are designed to extract meaningful biological information from the raw pixel data:

* **Image Pre-processing:** This essential initial phase includes tasks like background removal, where non-tissue areas of the slide are identified and excluded from analysis, often using simple pixel intensity thresholds.9 This step reduces computational load and focuses analysis on relevant regions.  
* **Cell Segmentation:** This is a fundamental and challenging task in computational pathology.11 Algorithms are designed to identify the precise boundaries of every cell, particularly the nuclei, within an image.12 The output is not just a count of cells, but a detailed map of each cell's location and shape.  
* **Tissue Classification:** Concurrently, algorithms classify different regions of the WSI into distinct tissue types, such as tumor epithelium, stroma, immune cell infiltrates, or areas of necrosis.15

The culmination of these processes is the transformation of a visual, qualitative image into a structured, quantitative data table. This table becomes the primary input for statistical analysis. Each row might represent a single cell or a region of interest (ROI), and the columns contain a wealth of features, including:

* **Morphological Features:** Measurements of size (area, perimeter), shape (circularity, eccentricity), and texture for each nucleus or cell.18  
* **Intensity Features:** Quantification of stain intensity, such as the expression level of a biomarker from an immunohistochemistry (IHC) stain.19  
* **Positional Features:** The precise X, Y coordinates of every cell centroid, which forms the basis for spatial statistics.22  
* **Contextual Features:** Counts and densities of different cell types within defined regions.3

### **The Double-Edged Sword of Quantitative Data**

The transition to quantitative data presents both a monumental opportunity and a significant challenge. The opportunity lies in the ability to perform objective, reproducible, and complex statistical analyses that were previously inconceivable.6 Researchers can now mine "sub-visual" features—subtle patterns in texture or spatial arrangement not discernible to the human eye—and correlate them with clinical outcomes like patient survival or treatment response.2 This quantitative approach promises to augment the pathologist's assessment, leading to more precise and personalized medicine.21  
However, this power comes with a critical challenge: the integrity of the final quantitative data is exquisitely sensitive to variations throughout the entire pre-analytical and analytical pipeline.6 Seemingly minor variations in laboratory procedures (e.g., tissue fixation, section thickness, staining protocols) or digital hardware (e.g., scanner manufacturer, camera sensors, lighting conditions) can introduce significant, systematic differences in the resulting images and, consequently, the extracted quantitative features.25 This variability, often referred to as "batch effects," can confound statistical results, leading to spurious correlations and conclusions that are not reproducible.27 A review of experimental pathology literature revealed that such issues are widespread, with many studies overusing parametric tests without reporting checks for their assumptions, a practice that can be misleading in the presence of unaccounted-for data variations.28  
This leads to a crucial realization for the development of any statistical software for this field. The validity of downstream statistical tests is inextricably linked to the quality and consistency of the upstream data generation process. A statistically significant p-value is meaningless if the observed difference between two groups is due to a technical artifact—such as one group being scanned on a different machine—rather than a true biological effect. This is a classic "garbage in, garbage out" problem, but the "garbage" can be subtle, learnable, hidden variables that AI models might inadvertently exploit, leading to inflated performance metrics and biased results.11  
Therefore, a state-of-the-art statistical module for digital pathology cannot operate in a vacuum, treating the input data as a given. It must be designed with a holistic awareness of the entire pipeline. The module should not only provide tools for hypothesis testing but must also equip the researcher with robust methods for quality control and exploratory data analysis. The primary goal is to empower users to identify and account for potential batch effects *before* they draw definitive conclusions. This approach fosters a culture of statistical rigor and reproducibility, a critical need identified across the digital pathology research community.11

## **Part I: Foundational Statistical Analyses for Feature Comparison and Association**

This section addresses the need for "classical" statistical tests, framing them around the fundamental research questions that arise from the quantitative features extracted from pathology images. The focus is on the practical application of these tests, providing concrete examples relevant to pathology research and highlighting how a well-designed jamovi module can facilitate their correct use.

### **1.1 Descriptive Statistics and Data Quality Assessment**

Before any formal hypothesis testing, the essential first step is to thoroughly explore and understand the dataset. This involves summarizing the data with descriptive statistics and visualizing its properties to check for quality, assess underlying distributions, and screen for potential issues like batch effects.

#### **1.1.1 Summarizing Quantitative Features**

For continuous features extracted from images—such as nuclear area, perimeter, stain intensity, or a texture metric—the initial analysis involves calculating standard descriptive statistics. These include measures of central tendency (mean, median) and measures of dispersion (standard deviation, variance, range, interquartile range). For categorical features, such as tissue type classifications ('tumor', 'stroma', 'immune') or clinical groups, the analysis involves calculating frequencies and proportions. Generating a "Table 1" that summarizes these baseline characteristics across different study groups is a standard and essential practice in clinical research.30

#### **1.1.2 Visualizing Distributions and Identifying Artifacts**

Visualizations are indispensable for gaining intuition about the data. Histograms, density plots, and box plots are crucial for understanding the distribution of each feature.32 These plots can reveal:

* **Normality:** Whether a feature follows a Gaussian (normal) distribution, which is a key assumption for parametric tests like the t-test and ANOVA.  
* **Skewness:** Whether the distribution is asymmetric.  
* **Modality:** Whether the distribution has one peak (unimodal) or multiple peaks (bimodal or multimodal). A bimodal distribution might indicate the presence of two distinct underlying cell populations or could be a sign of a technical artifact.  
* **Outliers:** Extreme values that may represent measurement errors or rare biological events.

A particularly powerful visualization technique is the **raincloud plot**, which combines a density plot, a box plot, and the raw data points, providing a comprehensive view of the distribution.30

#### **1.1.3 Detecting Batch Effects**

This exploratory phase is the critical juncture for detecting potential confounding from batch effects. The strategy is to stratify the visualizations by potential sources of technical variation. For example, a researcher should generate box plots of a key feature (e.g., mean nuclear intensity) grouped by scanner\_ID, stain\_batch, or patient\_cohort. If the distributions differ systematically across these technical groups, it signals a batch effect that must be addressed before proceeding with hypothesis testing.  
Another powerful technique is **Principal Component Analysis (PCA)**. PCA is a dimensionality reduction method that can be used to visualize the overall structure of the data. When applied to a dataset of many features, if the samples cluster primarily by a technical variable (like the scanner they were processed on) rather than the biological variable of interest (like tumor grade), it is strong evidence of a significant batch effect that could invalidate subsequent analyses.11 A well-designed  
jamovi module should make these exploratory and diagnostic plots easy to generate, guiding the user toward a more rigorous and self-aware analysis.

### **1.2 Comparing Continuous Features Between Groups (t-test and ANOVA)**

A common task in pathology research is to compare the mean of a quantitative feature between two or more clinically or experimentally defined groups.

#### **1.2.1 Student's t-test**

The **Student's t-test** is used to determine if there is a significant difference between the means of a continuous variable in two independent groups.

* **Application Example:** A study might extract thousands of quantitative features from tumor cells and use a t-test to determine if the mean nuclear area is significantly larger in patients who did not respond to a particular therapy compared to those who did.33 This helps identify potential morphological biomarkers of treatment resistance.  
* **Assumptions:** The t-test assumes that the data in both groups are approximately normally distributed and that the variances of the two groups are equal (though variations of the test can accommodate unequal variances). The visualization steps in the previous section are crucial for checking these assumptions.  
* **Paired t-test:** A variation used when the two groups are not independent but paired, such as comparing a feature in primary tumor tissue versus its matched metastasis from the same patient.

#### **1.2.2 Analysis of Variance (ANOVA)**

When the comparison involves more than two groups, the appropriate test is the **Analysis of Variance (ANOVA)**.34

* **Application Example:** A researcher could use ANOVA to test if a morphological feature, such as nuclear circularity, differs significantly across three molecular subtypes of a cancer.20 The null hypothesis is that the mean circularity is the same across all three subtypes.  
* **Post-Hoc Testing:** A significant ANOVA result (e.g., p\<0.05) indicates that there is a difference *somewhere* among the groups, but it does not specify *which* groups are different from each other. To find this, **post-hoc tests** (e.g., Tukey's HSD, Bonferroni correction) are required. A frequent flaw in published research is the failure to report these necessary follow-up tests.28 A key advantage of a  
  jamovi module is its ability to make these post-hoc tests a simple checkbox option, ensuring a more complete and correct analysis.

#### **1.2.3 Non-Parametric Alternatives**

When the assumptions of the t-test or ANOVA are violated (e.g., the data is not normally distributed), non-parametric alternatives should be used. These tests operate on the ranks of the data rather than the raw values, making them robust to outliers and non-normal distributions.

* For two independent groups: The **Wilcoxon rank-sum test** (also known as the **Mann-Whitney U test**) is the non-parametric equivalent of the independent samples t-test.  
* For more than two independent groups: The **Kruskal-Wallis test** is the non-parametric equivalent of ANOVA.

### **1.3 Analyzing Associations in Categorical Data (Chi-squared Test)**

Often, research questions in digital pathology involve categorical variables. This can arise from classifying tissue patches into types, categorizing patients into risk groups, or binning a continuous feature into discrete levels (e.g., 'low', 'medium', 'high' expression).

#### **1.3.1 Chi-squared (χ²) Test of Independence**

The **Chi-squared (χ2) test** is used to determine whether there is a statistically significant association between two categorical variables. It compares the observed frequencies in a contingency table to the frequencies that would be expected if the two variables were independent.

* **Application Example 1 (Tissue Classification):** After an AI model classifies tissue patches from a WSI into 'Tumor', 'Stroma', and 'Immune Cells', a researcher could use a χ2 test to investigate whether the distribution of these tissue types is associated with the patient's smoking status (Smoker vs. Non-smoker). This could reveal, for instance, that smokers have a significantly higher proportion of immune cell infiltration in their tumors.27  
* **Application Example 2 (Method Validation):** When validating a new low-cost digital pathology workstation against a high-cost standard, a χ2 test was used to show there was no statistically significant difference in the proportion of correctly classified tissue tiles between the two systems, supporting the validity of the low-cost alternative.35

#### **1.3.2 Fisher's Exact Test**

The Chi-squared test relies on an approximation that can be inaccurate when the expected frequencies in the cells of the contingency table are very small (typically less than 5). In such cases, **Fisher's Exact Test** should be used, as it calculates the exact probability of observing the data, making it suitable for small sample sizes or sparse data.36

### **1.4 Modeling Relationships: Correlation and Regression**

Beyond simple group comparisons, it is often necessary to model the relationships between variables. Correlation and regression are the workhorses for this type of analysis, allowing researchers to quantify the strength of associations and build predictive models.

#### **1.4.1 Correlation**

**Pearson's correlation coefficient (r)** measures the strength and direction of the *linear* relationship between two continuous variables. It ranges from \-1 (perfect negative linear relationship) to \+1 (perfect positive linear relationship), with 0 indicating no linear relationship.

* **Application Example:** In breast cancer, the Oncotype DX Recurrence Score (RS) is a genomic test that predicts prognosis. To develop a less expensive, image-based surrogate, researchers extracted quantitative features from WSIs and calculated the Pearson correlation between these image features and the patients' actual RS values. A strong positive correlation would suggest that the image feature captures similar prognostic information as the genomic test.37

#### **1.4.2 Linear Regression**

**Linear regression** is used to model a continuous outcome variable as a function of one or more predictor variables (or features). The goal is to find the best-fitting linear equation that describes the relationship.

* **Application Example:** Building on the correlation example, researchers used linear regression to create a model that predicts the continuous RS value using a combination of standard clinicopathological features (like tumor grade) and new WSI-derived image features. By comparing a model with Magee features alone to a model with both Magee and image features, they were able to show that the inclusion of the pathomic features significantly enhanced the model's predictive power (as measured by the adjusted R2 value).37

#### **1.4.3 Logistic Regression**

**Logistic regression** is essential when the outcome variable is binary (dichotomous), such as patient response (yes/no), metastasis (present/absent), or a 5-year survival status (alive/deceased). Instead of predicting the value of the outcome directly, it models the probability of the outcome occurring.

* **Application Example:** A model could be built to predict the likelihood of a patient responding to neoadjuvant chemotherapy based on a set of quantitative wavelet and graph-based features extracted from their pre-treatment biopsy image.23  
* **Interpretation and Evaluation:** The output of a logistic regression model includes **odds ratios (ORs)**, which quantify how a one-unit change in a predictor variable affects the odds of the outcome occurring. The overall performance of the predictive model is commonly assessed using the **Area Under the Receiver Operating Characteristic Curve (AUC)**. An AUC of 0.5 suggests the model is no better than random chance, while an AUC of 1.0 indicates perfect discrimination.32

### **Table 1: Mapping Research Questions to Foundational Statistical Tests**

To facilitate the practical application of these methods, the following table serves as a guide for researchers, connecting common experimental questions in digital pathology to the appropriate statistical tests that should be made available in the jamovi module.

| Research Question | Example in Digital Pathology | Appropriate Statistical Test | Non-Parametric Alternative | Key Output to Interpret |
| :---- | :---- | :---- | :---- | :---- |
| Is there a difference in the mean of a continuous feature between **two independent groups**? | Is the average size of cancer nuclei different in patients who responded to therapy vs. those who did not? | Independent Samples t-test | Mann-Whitney U Test | p-value, difference in means, 95% CI |
| Is there a difference in the mean of a continuous feature between **more than two independent groups**? | Does the average texture of tumor stroma differ across Grade 1, Grade 2, and Grade 3 tumors? | One-Way ANOVA | Kruskal-Wallis Test | F-statistic, p-value, post-hoc test results |
| Is there an association between **two categorical variables**? | Is the presence of a specific tissue architecture pattern (e.g., cribriform) associated with molecular subtype? | Chi-squared (χ2) Test | Fisher's Exact Test (for small N) | χ2 statistic, p-value, odds ratio |
| What is the strength of the linear relationship between **two continuous variables**? | How strongly does the density of CD8+ T-cells correlate with the density of PD-L1+ tumor cells? | Pearson Correlation | Spearman's Rank Correlation | Correlation coefficient (r), p-value |
| Can we predict a **continuous outcome** from one or more predictor variables? | Can we predict a patient's Oncotype DX Recurrence Score using quantitative image features? | Linear Regression | \- | Model coefficients, R2, p-values |
| Can we predict a **binary outcome** from one or more predictor variables? | Can we predict which patients will have lymph node metastasis based on features from the primary tumor? | Logistic Regression | \- | Odds Ratios (OR), p-values, AUC |

## **Part II: Advanced Modeling for Clinical and Spatial Insights**

While foundational tests are essential for initial data exploration and simple comparisons, the true power of quantitative digital pathology lies in its ability to address more complex and clinically impactful questions. This section delves into two advanced domains that are cornerstones of modern computational pathology: survival analysis, which links image features to patient outcomes, and spatial statistics, which deciphers the architectural patterns of the tissue microenvironment.

### **2.1 Survival Analysis: Linking Pathomic Features to Patient Outcomes**

A primary goal of cancer research is the discovery of prognostic biomarkers that can predict a patient's clinical course and predictive biomarkers that can forecast their response to a specific therapy. Survival analysis provides the statistical framework to formally connect the quantitative features extracted from digital pathology images—so-called "pathomic" features—to time-to-event outcomes, such as Overall Survival (OS) or Progression-Free Survival (PFS).32

#### **2.1.1 Key Concepts in Survival Analysis**

Survival analysis deals with **time-to-event data**. For each subject, two key pieces of information are required:

1. **Time:** The duration from a defined starting point (e.g., date of diagnosis) to an event or the end of the study.  
2. **Status:** A binary indicator specifying whether the event of interest (e.g., death, disease progression) occurred at that time, or if the subject was **censored**.40

**Censoring** is a fundamental concept and occurs when the event has not been observed for a subject by the end of the study period. This could be because the patient was still alive at the last follow-up, was lost to follow-up, or experienced a different, unrelated event. Properly handling censored data is what distinguishes survival analysis from other statistical methods.

#### **2.1.2 Kaplan-Meier (KM) Analysis and the Log-Rank Test**

The **Kaplan-Meier (KM) method** is a non-parametric technique used to estimate the survival function, S(t), which is the probability that a subject will survive beyond a specific time t.41 It produces a characteristic step-function plot where the curve steps down each time an event occurs.

* **Application:** A common use is to stratify patients into groups based on a biomarker and compare their survival. For example, a continuous feature like the density of tumor-infiltrating lymphocytes (TILs) can be dichotomized at its median value into 'High TILs' and 'Low TILs' groups. A jamovi module should allow the user to easily generate and overlay the KM curves for these two groups to visually assess if the 'High TILs' group has a better survival probability.30  
* **R Implementation:** The workhorse for this in R is the survfit() function from the survival package, and the ggsurvplot() function from the survminer package is excellent for creating publication-quality visualizations.40 The  
  jsurvival submodule within the ClinicoPathJamoviModule provides a superb template for implementing this functionality in a user-friendly way.30  
* **Log-Rank Test:** To formally test whether the difference between two or more KM curves is statistically significant, the **log-rank test** is used.31 It tests the null hypothesis that there is no difference in survival between the groups.

#### **2.1.3 Cox Proportional Hazards (PH) Regression**

While the KM method is excellent for comparing groups based on a single categorical variable, **Cox Proportional Hazards (PH) regression** is a powerful semi-parametric method that allows for multivariable survival analysis. It can model the impact of several continuous and/or categorical predictors on survival simultaneously.

* **Application:** A researcher can build a Cox model to determine the prognostic value of a new pathomic feature while adjusting for the effects of known clinical confounders like patient age, tumor stage, and molecular subtype. This is crucial for demonstrating that the new image-based biomarker provides independent prognostic information.  
* **Hazard Ratio (HR):** The key output of a Cox model is the **Hazard Ratio (HR)** for each predictor.31 The HR represents the instantaneous risk of the event occurring. An HR of 2.0 for a given feature means that a one-unit increase in that feature is associated with a doubling of the risk of death at any given time point, holding all other variables constant. An HR \< 1 indicates a protective effect. Forest plots are a standard way to visualize the HRs and their 95% confidence intervals for multiple variables in a model.32  
* **R Implementation:** The coxph() function in the survival package is the standard for fitting Cox models in R. Tutorials provide clear examples of how to fit, interpret, and check the assumptions of these models.40

### **2.2 Spatial Statistics: Uncovering the Architecture of the Tissue Microenvironment (TME)**

Perhaps the most exciting and rapidly advancing frontier in quantitative pathology is the analysis of spatial patterns.22 The digitization of slides provides the precise X, Y coordinates of every cell, transforming the tissue into a spatial point pattern. This allows researchers to move beyond simply counting cells to analyzing their organization and interactions, which is critical for understanding complex biological processes like immune surveillance, tumor invasion, and the formation of supportive stromal structures.48  
A well-designed statistical module should guide the user through a logical hierarchy of spatial questions, moving from simple density descriptions to complex multi-type interaction patterns. This structured approach, evident in the specialized literature 22, makes the field more accessible and the analyses more systematic.

#### **2.2.1 First-Order Properties: Where are the Cells? (Intensity and Density)**

The most basic spatial question relates to the intensity or density of cells across the tissue. This goes beyond a simple average count to visualize how cell density varies locally.

* **Method: Density Heatmaps:** This technique generates a smooth map where color intensity corresponds to the local density of a selected cell population. It is an excellent exploratory tool for visually identifying cellular "hotspots" (areas of high density) and "coldspots" (areas of low density).51 For example, a heatmap of CD8+ T-cells can immediately reveal whether the immune infiltrate is concentrated at the tumor margin or diffusely scattered.

#### **2.2.2 Second-Order Properties: How are Cells Arranged? (Interaction)**

Second-order properties describe the spatial correlation or interaction between points, answering questions about whether cells are clustered, dispersed, or randomly arranged.

* **Analysis Within a Single Cell Type (Univariate Patterns):** This addresses the hypothesis of non-random distribution (H1). Are cells of a specific type drawn to each other, do they repel each other, or are they scattered randomly?  
  * **Ripley's K-function:** This is a fundamental tool in spatial statistics. For a given distance r, it measures the expected number of other points within that distance of a typical point in the pattern. By comparing the observed K-function to that expected under complete spatial randomness (CSR), one can determine if the pattern is clustered (observed \> expected) or dispersed (observed \< expected) across different spatial scales.50  
  * **Thomas Point Process Model:** If a pattern is found to be clustered, the Thomas process can be used to model and quantify the clustering. It provides parameters such as κ (the density of cluster centers) and μ (the mean number of points per cluster), which are biologically interpretable.22  
* **Analysis Between Two Cell Types (Bivariate Patterns):** This addresses the hypothesis of non-random interaction (H3). Are two different cell populations (e.g., tumor cells and immune cells) spatially associated?  
  * **Nearest Neighbor Analysis:** Calculates the average distance between each cell of Type A and its single nearest neighbor of Type B. This provides a simple measure of proximity.51  
  * **Proximity and Infiltration Analysis:** These are more flexible methods. Proximity analysis counts the number of Type B cells within a user-defined radius of each Type A cell. Infiltration analysis is a specific application that measures the density of infiltrating cells (e.g., lymphocytes) at varying distances from a defined boundary, such as the tumor-stroma interface.51  
  * **Cross-type G-function:** A bivariate extension of point pattern functions that specifically measures the distribution of distances from a point of one type to the nearest point of another type, providing a formal test for inter-type attraction or repulsion.22

#### **2.2.3 Multi-Type Patterns: Are There Cellular Neighborhoods? (Niches)**

The most advanced spatial analysis seeks to identify "niches"—recurring, spatially co-localized communities of multiple cell types (H4).

* **Method: Correlation Hotspot Detection:** This approach identifies regions where the densities of multiple cell types are highly correlated. For example, a "hotspot" could be defined as a cluster containing high densities of both CD8+ T-cells (anti-tumor) and FoxP3+ regulatory T-cells (immunosuppressive), representing a site of active immune regulation.22  
* **Method: Graph-Based Approaches:** The tissue can be represented as a graph where cells are nodes and edges connect nearby cells. Graph neural networks (GNNs) can then be used to learn the features of these cellular neighborhoods and identify recurring "niche" patterns.50

#### **2.2.4 R Implementation: spatstat and SpatialQPFs**

The implementation of these powerful spatial methods in R is well-supported.

* **spatstat:** This is the canonical, comprehensive R package for the statistical analysis of spatial point patterns. It provides functions for nearly all the methods described above, including Kest() for Ripley's K-function.  
* **SpatialQPFs:** This is a newer R package specifically designed for digital pathology workflows.54 It provides a streamlined, user-friendly interface for extracting interpretable spatial features. Crucially, it takes a simple table of cell coordinates and types as input—the exact output of a cell segmentation pipeline—and contains functions to perform point pattern (  
  Point\_pattern\_data\_uni, Point\_pattern\_data\_bi), areal, and geostatistical analyses.54 Its targeted design makes it an ideal backend for the spatial components of the proposed  
  jamovi module.

### **Table 2: Key Spatial Statistics Methods and Their R Implementation**

This table demystifies the field of spatial statistics for pathology researchers by connecting biological questions to specific methods and the R functions needed to implement them.

| Spatial Question | Statistical Method | Required Input Data | Key R Package(s) | Example R Function(s) | Interpretation of Output |
| :---- | :---- | :---- | :---- | :---- | :---- |
| Where are the "hotspots" of immune cells? | First-Order: Density Estimation | X, Y coordinates of immune cells | spatstat, SpatialQPFs | spatstat::density.ppp(), SpatialQPFs::Data\_Vis() | A color heatmap showing regions of high and low cell density. |
| Are my CD8+ T-cells clustered together more than by chance? | Second-Order Univariate: Ripley's K Function | X, Y coordinates of all CD8+ T-cells in the ROI | spatstat, SpatialQPFs | spatstat::Kest(), SpatialQPFs::Point\_pattern\_data\_uni() | A plot of K(r) vs. r. A curve above the theoretical line for randomness indicates clustering at that distance. |
| What is the average distance from a tumor cell to the nearest T-cell? | Second-Order Bivariate: Nearest Neighbor | X, Y coordinates of tumor cells and T-cells | spatstat, custom function | spatstat::nndist() | A distribution of distances; the mean/median indicates typical proximity. |
| How many immune cells are infiltrating the tumor margin? | Second-Order Bivariate: Infiltration Analysis | X, Y coordinates of immune cells; annotated tumor boundary polygon | spatstat, HALO-inspired logic | spatstat::distfun(), custom script | A histogram showing the number of immune cells in distance bins from the boundary. |
| Are tumor cells and fibroblasts spatially attracted to each other? | Second-Order Bivariate: Cross-Type Analysis | X, Y coordinates of tumor cells and fibroblasts | spatstat, SpatialQPFs | spatstat::Kcross(), SpatialQPFs::Point\_pattern\_data\_bi() | A plot similar to Ripley's K, where a curve above the random expectation suggests inter-type attraction. |

## **Part III: A Practical Implementation Guide for a Digital Pathology jamovi Module**

This final part provides an actionable, step-by-step guide for developing the proposed jamovi module. It synthesizes the statistical methodologies from Parts I and II with the practical software engineering aspects of jamovi module development, offering a concrete blueprint for the project.

### **3.1 Getting Started with jamovi Module Development**

jamovi is built on top of the R statistical language, and its modules are essentially specialized R packages with a user-friendly graphical interface.58 The development process is streamlined by the  
jmvtools package.

#### **3.1.1 Setting Up the Development Environment**

The first step is to prepare the R environment for jamovi development.

1. **Install jmvtools:** This crucial R package contains all the necessary functions for creating, building, and installing jamovi modules. It should be installed from the official jamovi repository.59  
   R  
   install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))

2. **Check jamovi Installation:** The jmvtools package needs to know where the jamovi application is installed on the system. The check() function attempts to find it automatically. If it fails, the path must be provided manually.59  
   R  
   \# Attempt to auto-detect  
   jmvtools::check()

   \# Manually specify path if needed  
   options(jamovi\_home='C:/Program Files/jamovi')  
   jmvtools::check()

#### **3.1.2 The Module Development Cycle**

The core workflow for creating and testing a module is an iterative cycle:

1. **Create Module Structure:** The jmvtools::create('MyModule') function generates a new directory with the standard R package structure (DESCRIPTION, NAMESPACE, R/ directory) plus an additional jamovi/ directory. This directory contains YAML files that define the module's user interface and analysis structure.61  
2. **Define Analysis (YAML):** The user interface for each analysis (e.g., input fields, checkboxes, options) is defined in .u.yaml files within the jamovi/ directory. The analysis logic and result elements (tables, plots) are defined in .a.yaml and .r.yaml files, respectively.62  
3. **Write R Code:** The statistical logic is written in R functions placed in the R/ directory. These functions receive inputs from the UI, perform calculations (often by calling other R packages), and populate the results objects (tables, plots) defined in the YAML files.  
4. **Install and Test:** The jmvtools::install() command builds the module from the source files and "sideloads" it into the running jamovi application. The new analysis appears instantly in the jamovi menu, allowing for immediate testing. Any changes to the R code or YAML files can be applied by simply running jmvtools::install() again, making for a rapid and efficient development cycle.59

### **3.2 Case Study: Deconstructing the ClinicoPathJamoviModule**

Instead of starting from a blank slate, a highly effective strategy is to learn from a successful, relevant, and open-source example. The ClinicoPathJamoviModule is a masterpiece of jamovi development tailored specifically for pathology research, making it the perfect blueprint to deconstruct.30  
By examining its source code on GitHub 31, one can extract invaluable best practices. The module is broken down into logical sub-components, such as  
ClinicoPathDescriptives, jjstatsplot, and jsurvival.30 The  
jsurvival submodule 46, for instance, provides a clear example of how to implement complex survival analysis. Its  
jamovi/survival.a.yaml file defines the analysis options (e.g., time variable, outcome variable, explanatory factor), while the jamovi/survival.u.yaml file specifies the layout of these options in the UI. The corresponding R/survival.b.R file contains the R code that takes these inputs, uses the survival and survminer packages to perform the Kaplan-Meier and Cox regression analyses, and then populates the output tables and plots that are displayed back to the user in jamovi. This clear separation of UI definition (YAML) and backend logic (R) is the core architectural pattern to emulate.

### **3.3 Blueprint for a Novel "Spatial Pathology" Module**

While the ClinicoPathJamoviModule is comprehensive, there is a clear opportunity to create a novel module focused on the cutting-edge area of **spatial statistics**, which appears to be a gap in its current functionality. This new module would be a significant contribution to the pathology research community.

#### **3.3.1 Proposed Module Structure**

A logical menu structure within jamovi would guide the user through the hierarchy of spatial questions identified in Part II:

* **Analyses \-\> Spatial Pathology**  
  * **1\. Data Visualization \-\> Density Heatmap**  
  * **2\. Single Population Patterns \-\> Ripley's K Analysis**  
  * **3\. Population Interactions \-\> Nearest Neighbor Analysis**  
  * **3\. Population Interactions \-\> Proximity/Infiltration Analysis**  
  * **3\. Population Interactions \-\> Bivariate Pattern Analysis (Cross-K)**

#### **3.3.2 Mapping UI to R Functions**

For each analysis, the UI controls must be mapped to the arguments of the backend R functions. For example, for the **Proximity/Infiltration Analysis**, the UI would need:

* A VariableSupplier to allow the user to drag and drop the columns containing the X and Y coordinates.  
* A VariableSupplier for the column containing cell type labels.  
* A ComboBox to select the "source" cell type (e.g., 'Tumor').  
* A ComboBox to select the "target" cell type (e.g., 'CD8 T-cell').  
* A TextBox for the user to enter the search radius in micrometers.

These inputs would then be passed as arguments to a custom R function that leverages the SpatialQPFs or spatstat package to perform the calculation and generate a proximity histogram and summary table as output.

### **3.4 Curating R Dependencies and Designing for the User**

A successful module depends on a curated set of robust backend R packages and a focus on producing clear, interpretable outputs.

#### **3.4.1 Essential R Packages**

The following table summarizes the essential R packages that would form the foundation of a comprehensive digital pathology module, covering all the analyses discussed.

### **Table 3: Essential R Packages for a Digital Pathology jamovi Module**

| Analysis Domain | Package Name | Core Function(s)/Purpose | Relevance |
| :---- | :---- | :---- | :---- |
| **Core / UI** | jmvcore, R6 | Core jamovi functionality and object-oriented programming. | Required for all modules. |
| **Descriptive Stats** | gtsummary, summarytools | Publication-ready summary tables (e.g., "Table 1"). | Excellent for creating standard clinical research tables. |
| **Survival Analysis** | survival | The standard R package for survival analysis; provides Surv(), survfit(), coxph(). | The definitive package for Kaplan-Meier and Cox models. |
| **Survival Analysis** | survminer | Provides ggsurvplot() for creating advanced survival curves with risk tables. | Essential for high-quality survival visualizations. |
| **Spatial Analysis** | SpatialQPFs | A modern, pathology-focused package for spatial feature extraction. | State-of-the-art for streamlined spatial analysis from cell data.54 |
| **Spatial Analysis** | spatstat | The comprehensive, canonical package for spatial point pattern analysis. | Provides deep, flexible control over all aspects of spatial statistics. |
| **Visualization** | ggplot2 | The premier data visualization package in R, providing a flexible grammar of graphics. | The foundation for creating almost any custom plot.63 |
| **Data Handling** | dplyr, tidyr | Core packages for data manipulation and wrangling. | Essential for preparing data for analysis. |

#### **3.4.2 Designing Publication-Ready Outputs**

The ultimate goal is to empower researchers to generate results for their publications. Therefore, the module's outputs must be clear, complete, and aesthetically pleasing.

* **Rich Tables:** Tables should be well-formatted with clear column headers, and always include measures of uncertainty (e.g., 95% Confidence Intervals) alongside point estimates and p-values.  
* **High-Quality Plots:** All plots should be generated using a powerful engine like ggplot2, with customizable labels, titles, and legends.  
* **Natural Language Summaries:** A standout feature of the ClinicoPathJamoviModule is its ability to generate a plain-language text summary of the statistical results.30 For example, after a t-test, it might output: "An independent samples t-test was conducted to compare \[Feature\] between \[Group 1\] and \[Group 2\]. There was a significant difference in the scores for \[Group 1\] (M=x, SD=y) and \[Group 2\] (M=x, SD=y); t(df)=t, p=p." This feature dramatically lowers the barrier to correct interpretation and reporting and should be a key design goal.

### **Table 4: Proposed Structure for a "Spatial Pathology" Jamovi Module**

This table provides a concrete architectural blueprint for the most novel component of the proposed module, mapping the user experience directly to the backend implementation.

| Jamovi Menu Path | Analysis Goal | UI Controls (in .u.yaml) | Backend R Package/Function | Key Outputs |
| :---- | :---- | :---- | :---- | :---- |
| **Spatial Pathology \-\> Data Visualization \-\> Density Heatmap** | Visualize the spatial density of a selected cell population. | VariableSupplier (Coords), VariableSupplier (Cell Types), ComboBox (Select Cell Type) | SpatialQPFs::Data\_Vis() | Density heatmap plot. |
| **Spatial Pathology \-\> Single Population Patterns \-\> Ripley's K Analysis** | Test if a cell population is clustered, random, or dispersed. | VariableSupplier (Coords), VariableSupplier (Cell Types), ComboBox (Select Cell Type) | SpatialQPFs::Point\_pattern\_data\_uni() or spatstat::Kest() | Ripley's K plot with simulation envelopes, interpretation text. |
| **Spatial Pathology \-\> Population Interactions \-\> Proximity Analysis** | Count cells of Type B within a defined radius of each cell of Type A. | VariableSupplier (Coords), VariableSupplier (Cell Types), ComboBox (Type A), ComboBox (Type B), Number (Radius in µm) | Custom function using spatstat::disc() and loops | Proximity histogram plot, summary table of counts per cell. |

## **Conclusion: Empowering the Pathology Community with Accessible and Rigorous Tools**

The field of digital pathology is generating quantitative data at an unprecedented scale, creating a critical need for analytical tools that are both powerful and accessible. The development of a specialized jamovi module, as outlined in this report, represents a significant opportunity to meet this need. By integrating a comprehensive suite of statistical methods—from foundational tests for feature comparison to advanced models for survival and spatial analysis—into a user-friendly, graphical interface, such a module can bridge the gap between complex data generation and meaningful biological insight.  
The key to a successful implementation lies in a holistic approach. The module must not only provide the statistical tests themselves but also promote a culture of rigor by incorporating tools for exploratory data analysis and quality control. This empowers researchers to proactively identify and account for the technical variability and batch effects that are inherent in digital pathology data, thereby enhancing the reproducibility and reliability of their findings.11  
Furthermore, by embracing the cutting edge of the field, particularly the analysis of spatial statistics with modern R packages like SpatialQPFs, the module can provide novel capabilities that are not widely available in existing point-and-click software.56 This allows researchers to move beyond simple cell counting and begin to decode the complex architecture of the tumor microenvironment, unlocking new insights into disease mechanisms.  
Ultimately, the vision for this project is to empower the broader pathology and oncology research community. By leveraging the open-source, extensible framework of jamovi and the powerful statistical ecosystem of R, it is possible to create an indispensable tool that lowers the technical barriers to sophisticated data analysis.31 A well-designed module will enable clinicians and scientists, regardless of their programming expertise, to perform robust, reproducible, and advanced statistical analyses, accelerating the translation of quantitative discoveries from digital slides into tangible improvements in diagnostics, prognostics, and patient care.

#### **Works cited**

1. Digital Pathology: Transforming Diagnosis in the Digital Age \- PMC \- PubMed Central, accessed August 12, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC10547926/](https://pmc.ncbi.nlm.nih.gov/articles/PMC10547926/)  
2. (PDF) Image Analysis and Machine Learning in Digital Pathology: Challenges and Opportunities \- ResearchGate, accessed August 12, 2025, [https://www.researchgate.net/publication/304823681\_Image\_Analysis\_and\_Machine\_Learning\_in\_Digital\_Pathology\_Challenges\_and\_Opportunities](https://www.researchgate.net/publication/304823681_Image_Analysis_and_Machine_Learning_in_Digital_Pathology_Challenges_and_Opportunities)  
3. Intro to Whole Slide Imaging in Pathology \- Leica Biosystems, accessed August 12, 2025, [https://www.leicabiosystems.com/life-sciences-and-research-solutions/application/how-whole-slide-imaging-is-changing-pathology/](https://www.leicabiosystems.com/life-sciences-and-research-solutions/application/how-whole-slide-imaging-is-changing-pathology/)  
4. Whole slide imaging in pathology: advantages, limitations, and emerging perspectives, accessed August 12, 2025, [https://www.dovepress.com/whole-slide-imaging-in-pathology-advantages-limitations-and-emerging-p-peer-reviewed-fulltext-article-PLMI](https://www.dovepress.com/whole-slide-imaging-in-pathology-advantages-limitations-and-emerging-p-peer-reviewed-fulltext-article-PLMI)  
5. FAQs \- Digital Pathology Association, accessed August 12, 2025, [https://digitalpathologyassociation.org/faq](https://digitalpathologyassociation.org/faq)  
6. Using digital pathology to standardize and automate histological evaluations of environmental samples \- PMC, accessed August 12, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC11816309/](https://pmc.ncbi.nlm.nih.gov/articles/PMC11816309/)  
7. Whole Slide Image Analysis in Real Time with MONAI and RAPIDS | NVIDIA Technical Blog, accessed August 12, 2025, [https://developer.nvidia.com/blog/whole-slide-image-analysis-in-real-time-with-monai-and-rapids/](https://developer.nvidia.com/blog/whole-slide-image-analysis-in-real-time-with-monai-and-rapids/)  
8. Histopathological Image Analysis: A Review \- PMC \- PubMed Central, accessed August 12, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC2910932/](https://pmc.ncbi.nlm.nih.gov/articles/PMC2910932/)  
9. Whole Slide Image Understanding in Pathology: What Is the Salient Scale of Analysis?, accessed August 12, 2025, [https://www.mdpi.com/2673-7426/4/1/28](https://www.mdpi.com/2673-7426/4/1/28)  
10. SAMPLER: Empirical distribution representations for rapid analysis of whole slide tissue images \- PMC, accessed August 12, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC10418159/](https://pmc.ncbi.nlm.nih.gov/articles/PMC10418159/)  
11. Developing image analysis methods for digital pathology \- PMC, accessed August 12, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC9324951/](https://pmc.ncbi.nlm.nih.gov/articles/PMC9324951/)  
12. Computational Nuclei Segmentation Methods in Digital Pathology: A Survey \- ResearchGate, accessed August 12, 2025, [https://www.researchgate.net/publication/336347859\_Computational\_Nuclei\_Segmentation\_Methods\_in\_Digital\_Pathology\_A\_Survey](https://www.researchgate.net/publication/336347859_Computational_Nuclei_Segmentation_Methods_in_Digital_Pathology_A_Survey)  
13. Methods for Segmentation and Classification of Digital Microscopy Tissue Images \- Frontiers, accessed August 12, 2025, [https://www.frontiersin.org/journals/bioengineering-and-biotechnology/articles/10.3389/fbioe.2019.00053/full](https://www.frontiersin.org/journals/bioengineering-and-biotechnology/articles/10.3389/fbioe.2019.00053/full)  
14. Whole-cell segmentation of tissue images with human-level performance using large-scale data annotation and deep learning \- PubMed Central, accessed August 12, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC9010346/](https://pmc.ncbi.nlm.nih.gov/articles/PMC9010346/)  
15. Atlas of Digital Pathology: A Generalized Hierarchical Histological Tissue Type-Annotated Database for Deep Learning \- CVF Open Access, accessed August 12, 2025, [https://openaccess.thecvf.com/content\_CVPR\_2019/papers/Hosseini\_Atlas\_of\_Digital\_Pathology\_A\_Generalized\_Hierarchical\_Histological\_Tissue\_Type-Annotated\_CVPR\_2019\_paper.pdf](https://openaccess.thecvf.com/content_CVPR_2019/papers/Hosseini_Atlas_of_Digital_Pathology_A_Generalized_Hierarchical_Histological_Tissue_Type-Annotated_CVPR_2019_paper.pdf)  
16. Full article: Deep Learning-Based Multi-Class Classification of Breast Digital Pathology Images \- Taylor & Francis Online, accessed August 12, 2025, [https://www.tandfonline.com/doi/full/10.2147/CMAR.S312608](https://www.tandfonline.com/doi/full/10.2147/CMAR.S312608)  
17. Highly accurate automated tissue classification using deep learning on digital pathology images: A novel tool for resolving conflicts in diagnosis. \- ResearchGate, accessed August 12, 2025, [https://www.researchgate.net/publication/341631010\_Highly\_accurate\_automated\_tissue\_classification\_using\_deep\_learning\_on\_digital\_pathology\_images\_A\_novel\_tool\_for\_resolving\_conflicts\_in\_diagnosis?\_tp=eyJjb250ZXh0Ijp7InBhZ2UiOiJzY2llbnRpZmljQ29udHJpYnV0aW9ucyIsInByZXZpb3VzUGFnZSI6bnVsbCwic3ViUGFnZSI6bnVsbH19](https://www.researchgate.net/publication/341631010_Highly_accurate_automated_tissue_classification_using_deep_learning_on_digital_pathology_images_A_novel_tool_for_resolving_conflicts_in_diagnosis?_tp=eyJjb250ZXh0Ijp7InBhZ2UiOiJzY2llbnRpZmljQ29udHJpYnV0aW9ucyIsInByZXZpb3VzUGFnZSI6bnVsbCwic3ViUGFnZSI6bnVsbH19)  
18. Image analysis and machine learning in digital pathology: Challenges and opportunities, accessed August 12, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC5556681/](https://pmc.ncbi.nlm.nih.gov/articles/PMC5556681/)  
19. Digital Pathology | AI-Based Tool | Cancer \- TissueGnostics, accessed August 12, 2025, [https://tissuegnostics.com/new-era-digital-pathology-fda-approves-ai-tool-cancer](https://tissuegnostics.com/new-era-digital-pathology-fda-approves-ai-tool-cancer)  
20. H\&E image analysis pipeline for quantifying morphological features \- PMC, accessed August 12, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC10616375/](https://pmc.ncbi.nlm.nih.gov/articles/PMC10616375/)  
21. Quantitative Image Analysis for Tissue Biomarker Use: A White Paper From the Digital Pathology Association \- PubMed Central, accessed August 12, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC8354563/](https://pmc.ncbi.nlm.nih.gov/articles/PMC8354563/)  
22. Digital Pathology Analysis Quantifies Spatial Heterogeneity of CD3, CD4, CD8, CD20, and FoxP3 Immune Markers in Triple-Negative Breast Cancer \- PubMed Central, accessed August 12, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC7604437/](https://pmc.ncbi.nlm.nih.gov/articles/PMC7604437/)  
23. Quantitative digital histopathology and machine learning to predict pathological complete response to chemotherapy in breast cancer patients using pre-treatment tumor biopsies \- PMC, accessed August 12, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC9188550/](https://pmc.ncbi.nlm.nih.gov/articles/PMC9188550/)  
24. Quantitative Digital Pathology \- Definiens \- YouTube, accessed August 12, 2025, [https://www.youtube.com/watch?v=BSoOjRKeb9I](https://www.youtube.com/watch?v=BSoOjRKeb9I)  
25. Whole Slide Images in Artificial Intelligence Applications in Digital Pathology: Challenges and Pitfalls \- PMC, accessed August 12, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC10518202/](https://pmc.ncbi.nlm.nih.gov/articles/PMC10518202/)  
26. Stain normalization in digital pathology: Clinical multi-center evaluation of image quality, accessed August 12, 2025, [https://www.researchgate.net/publication/363836292\_Stain\_normalization\_in\_digital\_pathology\_Clinical\_multi-center\_evaluation\_of\_image\_quality](https://www.researchgate.net/publication/363836292_Stain_normalization_in_digital_pathology_Clinical_multi-center_evaluation_of_image_quality)  
27. The Impact of Digital Histopathology Batch Effect on Deep Learning Model Accuracy and Bias | bioRxiv, accessed August 12, 2025, [https://www.biorxiv.org/content/10.1101/2020.12.03.410845.full](https://www.biorxiv.org/content/10.1101/2020.12.03.410845.full)  
28. Statistical Methods in Experimental Pathology: A Review and Primer, accessed August 12, 2025, [https://pubmed.ncbi.nlm.nih.gov/33652018/](https://pubmed.ncbi.nlm.nih.gov/33652018/)  
29. Digital pathology and image analysis augment biospecimen annotation and biobank quality assurance harmonization \- ResearchGate, accessed August 12, 2025, [https://www.researchgate.net/publication/259449029\_Digital\_pathology\_and\_image\_analysis\_augment\_biospecimen\_annotation\_and\_biobank\_quality\_assurance\_harmonization](https://www.researchgate.net/publication/259449029_Digital_pathology_and_image_analysis_augment_biospecimen_annotation_and_biobank_quality_assurance_harmonization)  
30. Comprehensive Analysis for Clinicopathological Research • ClinicoPath, accessed August 12, 2025, [https://www.serdarbalci.com/ClinicoPathJamoviModule/](https://www.serdarbalci.com/ClinicoPathJamoviModule/)  
31. sbalci/ClinicoPathJamoviModule: ClinicoPath jamovi Module \- GitHub, accessed August 12, 2025, [https://github.com/sbalci/ClinicoPathJamoviModule](https://github.com/sbalci/ClinicoPathJamoviModule)  
32. Data Exploration Of Features For Outcome Association In Digital Pathology, accessed August 12, 2025, [https://andrewjanowczyk.com/feature-analysis-for-digital-pathology/](https://andrewjanowczyk.com/feature-analysis-for-digital-pathology/)  
33. A digital pathology tool for quantification of color features in histologic specimens, accessed August 12, 2025, [https://www.researchgate.net/publication/353468559\_A\_digital\_pathology\_tool\_for\_quantification\_of\_color\_features\_in\_histologic\_specimens](https://www.researchgate.net/publication/353468559_A_digital_pathology_tool_for_quantification_of_color_features_in_histologic_specimens)  
34. Quantification and mitigation of site-specific differences in digital, accessed August 12, 2025, [https://www.spiedigitallibrary.org/conference-proceedings-of-spie/13413/134130W/Quantification-and-mitigation-of-site-specific-differences-in-digital-pathology/10.1117/12.3046519.full?webSyncID=80d8e8ab-eeae-d6ae-05c1-17aac5a91993\&sessionGUID=62b9eb3b-36c8-a956-04e3-dd821ae3dced](https://www.spiedigitallibrary.org/conference-proceedings-of-spie/13413/134130W/Quantification-and-mitigation-of-site-specific-differences-in-digital-pathology/10.1117/12.3046519.full?webSyncID=80d8e8ab-eeae-d6ae-05c1-17aac5a91993&sessionGUID=62b9eb3b-36c8-a956-04e3-dd821ae3dced)  
35. Developing a low-cost, open-source, locally manufactured workstation and computational pipeline for automated histopathology evaluation using deep learning \- PubMed Central, accessed August 12, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC11399610/](https://pmc.ncbi.nlm.nih.gov/articles/PMC11399610/)  
36. Top 134 Diagnostic pathology papers published in 2023 \- SciSpace, accessed August 12, 2025, [https://scispace.com/journals/diagnostic-pathology-2fp27abf/2023](https://scispace.com/journals/diagnostic-pathology-2fp27abf/2023)  
37. Deep Learning-Based Pathology Image Analysis Enhances Magee Feature Correlation With Oncotype DX Breast Recurrence Score \- Frontiers, accessed August 12, 2025, [https://www.frontiersin.org/journals/medicine/articles/10.3389/fmed.2022.886763/full](https://www.frontiersin.org/journals/medicine/articles/10.3389/fmed.2022.886763/full)  
38. Deep Learning-Based Pathology Image Analysis Enhances Magee Feature Correlation With Oncotype DX Breast Recurrence Score \- PubMed, accessed August 12, 2025, [https://pubmed.ncbi.nlm.nih.gov/35775006/](https://pubmed.ncbi.nlm.nih.gov/35775006/)  
39. | Comparison of the coefficients of features (both Magee and imaging)... | Download Scientific Diagram \- ResearchGate, accessed August 12, 2025, [https://www.researchgate.net/figure/Comparison-of-the-coefficients-of-features-both-Magee-and-imaging-in-the-linear\_fig4\_361278464](https://www.researchgate.net/figure/Comparison-of-the-coefficients-of-features-both-Magee-and-imaging-in-the-linear_fig4_361278464)  
40. Survival Analysis in R: Kaplan Meier & Cox Proportional Models Tutorial | DataCamp, accessed August 12, 2025, [https://www.datacamp.com/tutorial/survival-analysis-R](https://www.datacamp.com/tutorial/survival-analysis-R)  
41. Survival analysis in R \- Emily C. Zabor, accessed August 12, 2025, [https://www.emilyzabor.com/survival-analysis-in-r.html](https://www.emilyzabor.com/survival-analysis-in-r.html)  
42. Tutorial on survival modeling with applications to omics data \- Oxford Academic, accessed August 12, 2025, [https://academic.oup.com/bioinformatics/article/40/3/btae132/7623091](https://academic.oup.com/bioinformatics/article/40/3/btae132/7623091)  
43. Survival Analysis with R \- R Views, accessed August 12, 2025, [https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/](https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/)  
44. COMPLETE SURVIVAL ANALYSIS tutorial in R: Kaplan-Meier, Cox regression, Forest Plots... \- YouTube, accessed August 12, 2025, [https://www.youtube.com/watch?v=XrvCCFQRCZE](https://www.youtube.com/watch?v=XrvCCFQRCZE)  
45. Survival Module of ClinicoPath for jamovi • jsurvival \- Serdar Balci MD Pathologist, accessed August 12, 2025, [https://www.serdarbalci.com/jsurvival/](https://www.serdarbalci.com/jsurvival/)  
46. sbalci/jsurvival: survival functions in ClinicoPath jamovi ... \- GitHub, accessed August 12, 2025, [https://github.com/sbalci/jsurvival](https://github.com/sbalci/jsurvival)  
47. 27 Survival analysis \- The Epidemiologist R Handbook, accessed August 12, 2025, [https://epirhandbook.com/en/new\_pages/survival\_analysis.html](https://epirhandbook.com/en/new_pages/survival_analysis.html)  
48. Integrating digital pathology with transcriptomic and epigenomic tools for predicting metastatic uterine tumor aggressiveness \- Frontiers, accessed August 12, 2025, [https://www.frontiersin.org/journals/cell-and-developmental-biology/articles/10.3389/fcell.2022.1052098/full](https://www.frontiersin.org/journals/cell-and-developmental-biology/articles/10.3389/fcell.2022.1052098/full)  
49. Full article: Statistical Analysis of Quantitative Cancer Imaging Data \- Taylor & Francis Online, accessed August 12, 2025, [https://www.tandfonline.com/doi/full/10.1080/29979676.2024.2405348](https://www.tandfonline.com/doi/full/10.1080/29979676.2024.2405348)  
50. Spatial Statistics for Understanding Tissue Organization \- Frontiers, accessed August 12, 2025, [https://www.frontiersin.org/journals/physiology/articles/10.3389/fphys.2022.832417/full](https://www.frontiersin.org/journals/physiology/articles/10.3389/fphys.2022.832417/full)  
51. Spatial Analysis \- HALO Modules \- Indica Labs, accessed August 12, 2025, [https://indicalab.com/halo/halo-modules/spatial-analysis/](https://indicalab.com/halo/halo-modules/spatial-analysis/)  
52. A Survey on Graph-Based Deep Learning for Computational Histopathology \- ar5iv \- arXiv, accessed August 12, 2025, [https://ar5iv.labs.arxiv.org/html/2107.00272](https://ar5iv.labs.arxiv.org/html/2107.00272)  
53. Artificial Intelligence for Digital and Computational Pathology \- arXiv, accessed August 12, 2025, [https://arxiv.org/html/2401.06148v1](https://arxiv.org/html/2401.06148v1)  
54. SpatialQPFs: An R package for deciphering cell-cell spatial relationship \- ResearchGate, accessed August 12, 2025, [https://www.researchgate.net/publication/381687786\_SpatialQPFs\_An\_R\_package\_for\_deciphering\_cell-cell\_spatial\_relationship](https://www.researchgate.net/publication/381687786_SpatialQPFs_An_R_package_for_deciphering_cell-cell_spatial_relationship)  
55. SpatialQPFs: An R package for deciphering cell-cell spatial relationship | bioRxiv, accessed August 12, 2025, [https://www.biorxiv.org/content/10.1101/2024.06.17.599458v3](https://www.biorxiv.org/content/10.1101/2024.06.17.599458v3)  
56. Deciphering cell to cell spatial relationship for pathology images using SpatialQPFs \- PMC, accessed August 12, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC11605059/](https://pmc.ncbi.nlm.nih.gov/articles/PMC11605059/)  
57. Genentech/SpatialQPFs \- GitHub, accessed August 12, 2025, [https://github.com/Genentech/SpatialQPFs](https://github.com/Genentech/SpatialQPFs)  
58. jamovi \- open statistical software for the desktop and cloud, accessed August 12, 2025, [https://www.jamovi.org/](https://www.jamovi.org/)  
59. Getting Started \- Jamovi, accessed August 12, 2025, [https://dev.jamovi.org/tuts0101-getting-started.html](https://dev.jamovi.org/tuts0101-getting-started.html)  
60. Module Development for Jamovi • ClinicoPath \- Serdar Balci MD Pathologist, accessed August 12, 2025, [https://www.serdarbalci.com/ClinicoPathJamoviModule/articles/module\_development\_jamovi.html](https://www.serdarbalci.com/ClinicoPathJamoviModule/articles/module_development_jamovi.html)  
61. Creating a Module \- Jamovi, accessed August 12, 2025, [https://dev.jamovi.org/tuts0102-creating-a-module.html](https://dev.jamovi.org/tuts0102-creating-a-module.html)  
62. developer hub \- jamovi Documentation, accessed August 12, 2025, [https://docs.jamovi.org/\_pages/developer-hub.html](https://docs.jamovi.org/_pages/developer-hub.html)  
63. Logistic Regression in R, Clearly Explained\!\!\!\! \- YouTube, accessed August 12, 2025, [https://www.youtube.com/watch?v=C4N3\_XJJ-jU](https://www.youtube.com/watch?v=C4N3_XJJ-jU)