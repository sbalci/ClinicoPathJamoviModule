% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportcat2.b.R
\name{reportcat2Class}
\alias{reportcat2Class}
\title{Summary of Categorical Variables}
\value{
A results object containing HTML-formatted text summaries and visual tables
}
\description{
Generates a comprehensive summary of categorical variables including
frequency counts, percentages, missing value information, and optional visual
summaries. Supports multiple output formats and sorting options for enhanced
data exploration.
}
\examples{
\donttest{
# Example 1: Basic categorical summary
data <- data.frame(
  treatment = factor(c("A", "B", "A", "C", "B", "A")),
  grade = factor(c("High", "Low", "Medium", "High", "Low", "Medium"))
)
result <- reportcat(data = data, vars = c("treatment", "grade"))

# Example 2: Enhanced summary with cumulative percentages  
result_enhanced <- reportcat(
  data = data, 
  vars = "treatment",
  sumvar_style = TRUE,
  show_proportions = TRUE
)

# Example 3: Sort categories by frequency
result_sorted <- reportcat(
  data = data,
  vars = "grade", 
  sort_by_frequency = TRUE
)
}

}
\section{Super classes}{
\code{\link[jmvcore:Analysis]{jmvcore::Analysis}} -> \code{ClinicoPath::reportcat2Base} -> \code{reportcat2Class}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-reportcat2Class-clone}{\code{reportcat2Class$clone()}}
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
<li><span class="pkg-link" data-pkg="ClinicoPath" data-topic="reportcat2Base" data-id="initialize"><a href='../../ClinicoPath/html/reportcat2Base.html#method-reportcat2Base-initialize'><code>ClinicoPath::reportcat2Base$initialize()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-reportcat2Class-clone"></a>}}
\if{latex}{\out{\hypertarget{method-reportcat2Class-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{reportcat2Class$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
