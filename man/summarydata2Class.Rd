% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarydata2.b.R
\name{summarydata2Class}
\alias{summarydata2Class}
\title{Enhanced Summary Statistics for Continuous and Date Variables}
\value{
A results object containing HTML summaries, pivot tables, and visualizations
}
\description{
This function provides comprehensive descriptive statistics for continuous and date variables
with multiple output formats and enhanced visualization capabilities. It supports various
summary formats including standard descriptives, enhanced pivot tables, and automated EDA
reports using the summarytools package.

\strong{Key Features:}
\itemize{
\item Multiple summary formats (standard, enhanced, pivot tables, summarytools)
\item Date variable support with automatic format detection
\item Distribution diagnostics (normality tests, skewness, kurtosis)
\item Grouping capabilities for stratified analysis
\item Professional HTML output with embedded visualizations
\item Export-ready pivot tables with multiple layout styles
}

\strong{Summary Formats:}
\itemize{
\item \strong{Standard}: Basic descriptive statistics (mean, SD, median, range)
\item \strong{Enhanced (sumvar style)}: Comprehensive statistics with confidence intervals
\item \strong{Pivot Enhanced}: Professional pivot tables with customizable layouts
\item \strong{summarytools Integration}: Automated EDA with embedded visualizations
}
}
\details{
\strong{Date Variable Support:}
Automatically detects and parses multiple date formats including:
\itemize{
\item YYYY-MM-DD (ISO 8601)
\item DD/MM/YYYY and MM/DD/YYYY
\item Date-time combinations with HMS
\item Provides date-specific statistics (range, median date, time span)
}

\strong{Distribution Diagnostics:}
When enabled, provides comprehensive distributional analysis:
\itemize{
\item Shapiro-Wilk normality test (for n=3-5000)
\item Skewness and kurtosis measures
\item Normality interpretation and recommendations
}

\strong{Pivot Table Layouts:}
\itemize{
\item \strong{Clinical}: Optimized for clinical research presentations
\item \strong{Statistical}: Focused on statistical analysis requirements
\item \strong{Comparative}: Designed for comparative studies and meta-analyses
}

\strong{summarytools Integration:}
Leverages the summarytools package for automated EDA:
\itemize{
\item dfSummary: Comprehensive dataset overview with embedded plots
\item descr: Advanced descriptive statistics with robust measures
\item freq: Enhanced frequency tables for categorical variables
}
}
\examples{
\donttest{
# Basic continuous variable summary
summarydata(
  data = mtcars,
  vars = c("mpg", "hp", "wt"),
  summary_format = "standard",
  distr = TRUE
)

# Enhanced pivot table summary
summarydata(
  data = clinical_data,
  vars = c("age", "weight", "height"),
  summary_format = "pivot",
  pivot_layout = "clinical",
  include_confidence = TRUE,
  advanced_metrics = TRUE
)

# Date variable analysis
summarydata(
  data = study_data,
  date_vars = c("enrollment_date", "follow_up_date"),
  summary_format = "sumvar"
)

# Grouped analysis with summarytools
summarydata(
  data = trial_data,
  vars = c("baseline_score", "outcome_measure"),
  grvar = "treatment_group",
  summary_format = "summarytools_df",
  summarytools_graphs = TRUE
)
}

}
\section{Super classes}{
\code{\link[jmvcore:Analysis]{jmvcore::Analysis}} -> \code{ClinicoPath::summarydata2Base} -> \code{summarydata2Class}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-summarydata2Class-clone}{\code{summarydata2Class$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id=".createImage"><a href='../../jmvcore/html/Analysis.html#method-Analysis-.createImage'><code>jmvcore::Analysis$.createImage()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id=".createImages"><a href='../../jmvcore/html/Analysis.html#method-Analysis-.createImages'><code>jmvcore::Analysis$.createImages()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id=".createPlotObject"><a href='../../jmvcore/html/Analysis.html#method-Analysis-.createPlotObject'><code>jmvcore::Analysis$.createPlotObject()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id=".load"><a href='../../jmvcore/html/Analysis.html#method-Analysis-.load'><code>jmvcore::Analysis$.load()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id=".render"><a href='../../jmvcore/html/Analysis.html#method-Analysis-.render'><code>jmvcore::Analysis$.render()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id=".save"><a href='../../jmvcore/html/Analysis.html#method-Analysis-.save'><code>jmvcore::Analysis$.save()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id=".savePart"><a href='../../jmvcore/html/Analysis.html#method-Analysis-.savePart'><code>jmvcore::Analysis$.savePart()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id=".setCheckpoint"><a href='../../jmvcore/html/Analysis.html#method-Analysis-.setCheckpoint'><code>jmvcore::Analysis$.setCheckpoint()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id=".setParent"><a href='../../jmvcore/html/Analysis.html#method-Analysis-.setParent'><code>jmvcore::Analysis$.setParent()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id=".setReadDatasetHeaderSource"><a href='../../jmvcore/html/Analysis.html#method-Analysis-.setReadDatasetHeaderSource'><code>jmvcore::Analysis$.setReadDatasetHeaderSource()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id=".setReadDatasetSource"><a href='../../jmvcore/html/Analysis.html#method-Analysis-.setReadDatasetSource'><code>jmvcore::Analysis$.setReadDatasetSource()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id=".setResourcesPathSource"><a href='../../jmvcore/html/Analysis.html#method-Analysis-.setResourcesPathSource'><code>jmvcore::Analysis$.setResourcesPathSource()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id=".setStatePathSource"><a href='../../jmvcore/html/Analysis.html#method-Analysis-.setStatePathSource'><code>jmvcore::Analysis$.setStatePathSource()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id="addAddon"><a href='../../jmvcore/html/Analysis.html#method-Analysis-addAddon'><code>jmvcore::Analysis$addAddon()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id="asProtoBuf"><a href='../../jmvcore/html/Analysis.html#method-Analysis-asProtoBuf'><code>jmvcore::Analysis$asProtoBuf()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id="asSource"><a href='../../jmvcore/html/Analysis.html#method-Analysis-asSource'><code>jmvcore::Analysis$asSource()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id="check"><a href='../../jmvcore/html/Analysis.html#method-Analysis-check'><code>jmvcore::Analysis$check()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id="init"><a href='../../jmvcore/html/Analysis.html#method-Analysis-init'><code>jmvcore::Analysis$init()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id="optionsChangedHandler"><a href='../../jmvcore/html/Analysis.html#method-Analysis-optionsChangedHandler'><code>jmvcore::Analysis$optionsChangedHandler()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id="postInit"><a href='../../jmvcore/html/Analysis.html#method-Analysis-postInit'><code>jmvcore::Analysis$postInit()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id="print"><a href='../../jmvcore/html/Analysis.html#method-Analysis-print'><code>jmvcore::Analysis$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id="readDataset"><a href='../../jmvcore/html/Analysis.html#method-Analysis-readDataset'><code>jmvcore::Analysis$readDataset()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id="run"><a href='../../jmvcore/html/Analysis.html#method-Analysis-run'><code>jmvcore::Analysis$run()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id="serialize"><a href='../../jmvcore/html/Analysis.html#method-Analysis-serialize'><code>jmvcore::Analysis$serialize()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id="setError"><a href='../../jmvcore/html/Analysis.html#method-Analysis-setError'><code>jmvcore::Analysis$setError()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id="setStatus"><a href='../../jmvcore/html/Analysis.html#method-Analysis-setStatus'><code>jmvcore::Analysis$setStatus()</code></a></span></li>
<li><span class="pkg-link" data-pkg="jmvcore" data-topic="Analysis" data-id="translate"><a href='../../jmvcore/html/Analysis.html#method-Analysis-translate'><code>jmvcore::Analysis$translate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ClinicoPath" data-topic="summarydata2Base" data-id="initialize"><a href='../../ClinicoPath/html/summarydata2Base.html#method-summarydata2Base-initialize'><code>ClinicoPath::summarydata2Base$initialize()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-summarydata2Class-clone"></a>}}
\if{latex}{\out{\hypertarget{method-summarydata2Class-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{summarydata2Class$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
