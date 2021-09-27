## Agepyramid

### Description

Age Pyramid

### Usage

    agepyramid(data, age, gender, female)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>age</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>gender</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>female</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$pyramidTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

Tables can be converted to data frames with `asDF` or `as.data.frame`.
For example:

`results$pyramidTable$asDF`

`as.data.frame(results$pyramidTable)`


---
## AgepyramidClass

### Description

Age Pyramid

Age Pyramid

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::agepyramidBase` ->
`agepyramidClass`

### Methods

#### Public methods

-   [`agepyramidClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="agepyramidBase"
    data-id="initialize"></span>

    [`ClinicoPath::agepyramidBase$initialize()`](../../ClinicoPath/html/agepyramidBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    agepyramidClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Agreement

### Description

Function for Interrater Reliability.

### Usage

    agreement(data, vars, sft = FALSE, wght = "unweighted", exct = FALSE)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>vars</code></td>
<td><p>A string naming the variable from <code>data</code> that contains the diagnosis given by the observer, variable can be categorical, ordinal or numeric.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>sft</code></td>
<td><p>Boolean selection whether to show frequency table. Default is 'false'.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>wght</code></td>
<td><p>A list for the argument weight (wght), for weighted kappa analysis. Default is 'unweighted'. 'squared' or 'equal' should be selected only with ordinal variables. The function gives error if the variable type is not ordinal.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>exct</code></td>
<td><p>Boolean selection whether to use exact kappa. Effects only more than 3 observers.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$irrtable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

Tables can be converted to data frames with `asDF` or `as.data.frame`.
For example:

`results$irrtable$asDF`

`as.data.frame(results$irrtable)`

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## AgreementClass

### Description

Interrater Reliability Analysis

Interrater Reliability Analysis

### Value

Table

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::agreementBase` -> `agreementClass`

### Methods

#### Public methods

-   [`agreementClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="agreementBase"
    data-id="initialize"></span>

    [`ClinicoPath::agreementBase$initialize()`](../../ClinicoPath/html/agreementBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    agreementClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Alluvial

### Description

Alluvial Diagrams

### Usage

    alluvial(
      data,
      vars,
      condensationvar,
      excl = TRUE,
      marg = FALSE,
      fill = "first_variable",
      bin = "default",
      orient = "vert",
      usetitle = FALSE,
      mytitle = "Alluvial Plot"
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>vars</code></td>
<td><p>a string naming the variables from <code>data</code> that contains the values used for the Alluvial Diagram.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>condensationvar</code></td>
<td><p>The primary variable to be used for condensation.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>marg</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>fill</code></td>
<td><p>A list for the argument fill for selecting the variable to be represented by color. Default is 'first_variable'.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>bin</code></td>
<td><p>labels for the bins from low to high</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>orient</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>usetitle</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>mytitle</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>


---
## AlluvialClass

### Description

Alluvial Plot

Alluvial Plot

### Value

Alluvial Plot

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::alluvialBase` -> `alluvialClass`

### Methods

#### Public methods

-   [`alluvialClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="alluvialBase"
    data-id="initialize"></span>

    [`ClinicoPath::alluvialBase$initialize()`](../../ClinicoPath/html/alluvialBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    alluvialClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Benford

### Description

Benford Analysis

### Usage

    benford(data, var)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>var</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$text2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>


---
## Checkdata

### Description

Checking data

### Usage

    checkdata(data, dep)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$missingvals</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$minvals</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$maxvals</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

Tables can be converted to data frames with `asDF` or `as.data.frame`.
For example:

`results$minvals$asDF`

`as.data.frame(results$minvals)`


---
## Chisq.multcomp

### Description

Title

### Usage

    chisq.multcomp(x, p.method = "fdr")

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>x</code></td>
<td><p>vector</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>p.method</code></td>
<td><p>fdr</p></td>
</tr>
</tbody>
</table>


---
## CiSingle

### Description

Confidence interval for a mean

### Usage

    ciSingle(data, deps, splitBy, ciWidth = 95)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>deps</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>splitBy</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>ciWidth</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$conflevel</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$citable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

Tables can be converted to data frames with `asDF` or `as.data.frame`.
For example:

`results$citable$asDF`

`as.data.frame(results$citable)`


---
## Classification

### Description

Decision tree

### Usage

    classification(
      data,
      dep,
      indep,
      testSize = 0.33,
      noOfFolds = 10,
      testing,
      reporting = list("classifMetrices"),
      classifier,
      minSplit = 20,
      minBucket = 0,
      complecity = 0.01,
      maxCompete = 4,
      maxSurrogate = 5,
      unsurrogate = 2,
      noCrossValidations = 10,
      maxDepth = 30,
      noOfTrees = 10,
      maxDepthRandFor = 30,
      sampleFraction = 1,
      splitRule,
      plotDecisionTree = FALSE,
      predictedFreq = FALSE,
      printRandForest = FALSE,
      predictedFreqRF = FALSE
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>indep</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>testSize</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>noOfFolds</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>testing</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>reporting</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>classifier</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>minSplit</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>minBucket</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>complecity</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>maxCompete</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>maxSurrogate</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>unsurrogate</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>noCrossValidations</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>maxDepth</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>noOfTrees</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>maxDepthRandFor</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>sampleFraction</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>splitRule</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>plotDecisionTree</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>predictedFreq</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>printRandForest</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>predictedFreqRF</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$modelSettings</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$confusion$matrix</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$classificationMetrics$general</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$classificationMetrics$class</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$rocCurvePlot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$decisionTreeModel</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$predictedFreqPlot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$printRandForest$randomForestModel</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>


---
## ClinicoPath-package

### Description

`ClinicoPath` ClinicoPath help researchers to generate natural language
summaries of their dataset, generate cross tables with statistical
tests, and survival analysis with survival tables, survival plots, and
natural language summaries.

For more documentation, see the
[Website](https://sbalci.github.io/ClinicoPathJamoviModule/).

### Details

`ClinicoPath`

### Author(s)

**Maintainer**: Serdar Balci <drserdarbalci@gmail.com>
([ORCID](https://orcid.org/0000-0002-7852-3851))

### See Also

Useful links:

-   <https://www.serdarbalci.com/ClinicoPathJamoviModule>

-   <https://github.com/sbalci/ClinicoPathJamoviModule/>

-   <https://github.com/sbalci/ClinicoPathDescriptives/>

-   <https://github.com/sbalci/jsurvival>

-   <https://github.com/sbalci/meddecide>

-   <https://github.com/sbalci/jjstatsplot>

-   <https://sbalci.github.io/ClinicoPathJamoviModule/>

-   Report bugs at
    <https://github.com/sbalci/ClinicoPathJamoviModule/issues/>


---
## ComparingSurvival

### Description

Comparing Survival Outcomes

### Usage

    comparingSurvival(
      data,
      times,
      status,
      groups,
      ciyn = FALSE,
      loglogyn = FALSE,
      timeunits = "None"
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>times</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>status</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>groups</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>ciyn</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>loglogyn</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>timeunits</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$compsurvTable1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$compsurvTable2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$compsurvTable3</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

Tables can be converted to data frames with `asDF` or `as.data.frame`.
For example:

`results$compsurvTable1$asDF`

`as.data.frame(results$compsurvTable1)`


---
## Competingsurvival

### Description

Overall, Cause Specific, and Competing Survival.

### Usage

    competingsurvival(
      data,
      explanatory,
      overalltime,
      outcome,
      dod,
      dooc,
      awd,
      awod,
      analysistype = "overall"
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>explanatory</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>overalltime</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>outcome</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>dod</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dooc</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>awd</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>awod</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>analysistype</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## CompetingsurvivalClass

### Description

Competing Survival Analysis

Competing Survival Analysis

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::competingsurvivalBase` ->
`competingsurvivalClass`

### Methods

#### Public methods

-   [`competingsurvivalClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath"
    topic="competingsurvivalBase" data-id="initialize"></span>

    [`ClinicoPath::competingsurvivalBase$initialize()`](../../ClinicoPath/html/competingsurvivalBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    competingsurvivalClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## ContTables

### Description

The X² test of association (not to be confused with the X² goodness of
fit) is used to test whether two categorical variables are independent
or associated. If the p-value is low, it suggests the variables are not
independent, and that there is a relationship between the two variables.

### Usage

    contTables(
      data,
      rows,
      cols,
      counts = NULL,
      layers = NULL,
      chiSq = TRUE,
      chiSqCorr = FALSE,
      likeRat = FALSE,
      fisher = FALSE,
      contCoef = FALSE,
      phiCra = FALSE,
      logOdds = FALSE,
      odds = FALSE,
      relRisk = FALSE,
      ci = TRUE,
      ciWidth = 95,
      gamma = FALSE,
      taub = FALSE,
      obs = TRUE,
      exp = FALSE,
      pcRow = FALSE,
      pcCol = FALSE,
      pcTot = FALSE,
      formula
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>the data as a data frame</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>rows</code></td>
<td><p>the variable to use as the rows in the contingency table (not necessary when providing a formula, see the examples)</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>cols</code></td>
<td><p>the variable to use as the columns in the contingency table (not necessary when providing a formula, see the examples)</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>counts</code></td>
<td><p>the variable to use as the counts in the contingency table (not necessary when providing a formula, see the examples)</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>layers</code></td>
<td><p>the variables to use to split the contingency table (not necessary when providing a formula, see the examples)</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>chiSq</code></td>
<td><p><code>TRUE</code> (default) or <code>FALSE</code>, provide X²</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>chiSqCorr</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide X² with continuity correction</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>likeRat</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide the likelihood ratio</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>fisher</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide Fisher's exact test</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>contCoef</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide the contingency coefficient</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>phiCra</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide Phi and Cramer's V</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>logOdds</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide the log odds ratio (only available for 2x2 tables)</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>odds</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide the odds ratio (only available for 2x2 tables)</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>relRisk</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide the relative risk (only available for 2x2 tables)</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>ci</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide confidence intervals for the comparative measures</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>ciWidth</code></td>
<td><p>a number between 50 and 99.9 (default: 95), width of the confidence intervals to provide</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>gamma</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide gamma</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>taub</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide Kendall's tau-b</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>obs</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide the observed counts</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>exp</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide the expected counts</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>pcRow</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide row percentages</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>pcCol</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide column percentages</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>pcTot</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide total percentages</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>formula</code></td>
<td><p>(optional) the formula to use, see the examples</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$freqs</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table of proportions</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$chiSq</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table of X² test results</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$odds</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table of comparative measures</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$nom</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table of the 'nominal' test results</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$gamma</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table of the gamma test results</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$taub</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table of the Kendall's tau-b test results</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

Tables can be converted to data frames with `asDF` or `as.data.frame`.
For example:

`results$freqs$asDF`

`as.data.frame(results$freqs)`

### Examples

```r
## Not run: 
# data('HairEyeColor')
# dat <- as.data.frame(HairEyeColor)

# contTables(formula = Freq ~ Hair:Eye, dat)

#
#  CONTINGENCY TABLES
#
#  Contingency Tables
#  -----------------------------------------------------
#Hair BrownBlueHazelGreenTotal
#  -----------------------------------------------------
#Black   68  20   155  108
#Brown  119  84   54   29  286
#Red 26  17   14   14   71
#Blond7  94   10   16  127
#Total  220 215   93   64  592
#  -----------------------------------------------------
#
#
#  X² Tests
#  -------------------------------
#  Valuedfp
#  -------------------------------
#X²  138 9< .001
#N   592
#  -------------------------------
#

# Alternatively, omit the left of the formula (`Freq`) if each row
# represents a single observation:

# contTables(formula = ~ Hair:Eye, dat)

## End(Not run)
```


---
## ContTablesPaired

### Description

McNemar test

### Usage

    contTablesPaired(
      data,
      rows,
      cols,
      counts = NULL,
      chiSq = TRUE,
      chiSqCorr = FALSE,
      exact = FALSE,
      pcRow = FALSE,
      pcCol = FALSE,
      formula
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>the data as a data frame</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>rows</code></td>
<td><p>the variable to use as the rows in the contingency table (not necessary when providing a formula, see the examples)</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>cols</code></td>
<td><p>the variable to use as the columns in the contingency table (not necessary when providing a formula, see the examples)</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>counts</code></td>
<td><p>the variable to use as the counts in the contingency table (not necessary when providing a formula, see the examples)</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>chiSq</code></td>
<td><p><code>TRUE</code> (default) or <code>FALSE</code>, provide X²</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>chiSqCorr</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide X² with continuity correction</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>exact</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide an exact log odds ratio (requires exact2x2 to be installed)</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>pcRow</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide row percentages</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>pcCol</code></td>
<td><p><code>TRUE</code> or <code>FALSE</code> (default), provide column percentages</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>formula</code></td>
<td><p>(optional) the formula to use, see the examples</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$freqs</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a proportions table</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$test</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table of test results</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

Tables can be converted to data frames with `asDF` or `as.data.frame`.
For example:

`results$freqs$asDF`

`as.data.frame(results$freqs)`

### Examples

```r
dat <- data.frame(
`1st survey` = c('Approve', 'Approve', 'Disapprove', 'Disapprove'),
`2nd survey` = c('Approve', 'Disapprove', 'Approve', 'Disapprove'),
`Counts` = c(794, 150, 86, 570),
check.names=FALSE)

contTablesPaired(formula = Counts ~ `1st survey`:`2nd survey`, data = dat)

#
#  PAIRED SAMPLES CONTINGENCY TABLES
#
#  Contingency Tables
#  ------------------------------------------------
#1st surveyApproveDisapproveTotal
#  ------------------------------------------------
#Approve   794   150  944
#Disapprove 86   570  656
#Total 880   720 1600
#  ------------------------------------------------
#
#
#  McNemar Test
#  -----------------------------------------------------
#Valuedfp
#  -----------------------------------------------------
#X²   17.4 1< .001
#X² continuity correction 16.8 1< .001
#  -----------------------------------------------------
#


# Alternatively, omit the left of the formula (`Counts`) from the
# formula if each row represents a single observation:

contTablesPaired(formula = ~ `1st survey`:`2nd survey`, data = dat)
```


---
## Correlation

### Description

Function for Correlation.

### Usage

    correlation(data, vars)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>vars</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## CorrelationClass

### Description

Correlation Analysis

Correlation Analysis

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::correlationBase` ->
`correlationClass`

### Methods

#### Public methods

-   [`correlationClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="correlationBase"
    data-id="initialize"></span>

    [`ClinicoPath::correlationBase$initialize()`](../../ClinicoPath/html/correlationBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    correlationClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Crosstable

### Description

Function for making Cross Tables.

### Usage

    crosstable(
      data,
      vars,
      group,
      sty = "nejm",
      excl = TRUE,
      cont = "mean",
      pcat = "chisq"
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>vars</code></td>
<td><p>variable in the rows</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>group</code></td>
<td><p>variable in the column</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>sty</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>cont</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>pcat</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$tablestyle1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$tablestyle2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$tablestyle3</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$tablestyle4</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## CrosstableClass

### Description

Cross Table

Cross Table

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::crosstableBase` ->
`crosstableClass`

### Methods

#### Public methods

-   [`crosstableClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="crosstableBase"
    data-id="initialize"></span>

    [`ClinicoPath::crosstableBase$initialize()`](../../ClinicoPath/html/crosstableBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    crosstableClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Decision

### Description

Function for Medical Decision Analysis. Sensitivity, specificity,
positive predictive value, negative predictive value.

### Usage

    decision(
      data,
      gold,
      goldPositive,
      newtest,
      testPositive,
      pp = FALSE,
      pprob = 0.3,
      od = FALSE,
      fnote = FALSE,
      ci = FALSE,
      fagan = FALSE
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>gold</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>goldPositive</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>newtest</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>testPositive</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>pp</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>pprob</code></td>
<td><p>Prior probability (disease prevelance in the community). Requires a value between 0.001 and 0.999, default 0.300.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>od</code></td>
<td><p>Boolean selection whether to show frequency table. Default is 'false'.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>fnote</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>ci</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>fagan</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$text1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$cTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$nTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$ratioTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$epirTable_ratio</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$epirTable_number</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

Tables can be converted to data frames with `asDF` or `as.data.frame`.
For example:

`results$cTable$asDF`

`as.data.frame(results$cTable)`

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## Decision2

### Description

Function for Medical Decision Analysis. Sensitivity, specificity,
positive predictive value, negative predictive value.

### Usage

    decision2(data, gold, goldPositive, newtest, testPositive)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>gold</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>goldPositive</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>newtest</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>testPositive</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$cTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

Tables can be converted to data frames with `asDF` or `as.data.frame`.
For example:

`results$cTable$asDF`

`as.data.frame(results$cTable)`

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## Decisioncalculator

### Description

Function for Medical Decision Calculator.

### Usage

    decisioncalculator(
      TP = 90,
      TN = 80,
      FP = 30,
      FN = 20,
      pp = FALSE,
      pprob = 0.3,
      fnote = FALSE,
      ci = FALSE,
      fagan = FALSE
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>TP</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>TN</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>FP</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>FN</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>pp</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>pprob</code></td>
<td><p>Prior probability (disease prevelance in the community). Requires a value between 0.001 and 0.999, default 0.300.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>fnote</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>ci</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>fagan</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$cTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$nTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$ratioTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$epirTable_ratio</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$epirTable_number</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

Tables can be converted to data frames with `asDF` or `as.data.frame`.
For example:

`results$cTable$asDF`

`as.data.frame(results$cTable)`

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## DecisioncalculatorClass

### Description

Decision Calculator

Decision Calculator

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::decisioncalculatorBase` ->
`decisioncalculatorClass`

### Methods

#### Public methods

-   [`decisioncalculatorClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath"
    topic="decisioncalculatorBase" data-id="initialize"></span>

    [`ClinicoPath::decisioncalculatorBase$initialize()`](../../ClinicoPath/html/decisioncalculatorBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    decisioncalculatorClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## DecisionClass

### Description

Medical Decision Making

Medical Decision Making

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::decisionBase` -> `decisionClass`

### Methods

#### Public methods

-   [`decisionClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="decisionBase"
    data-id="initialize"></span>

    [`ClinicoPath::decisionBase$initialize()`](../../ClinicoPath/html/decisionBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    decisionClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Dendogram

### Description

Dendogram

### Usage

    dendogram(data, dep, group, alt = "notequal", varEq = TRUE)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>group</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>alt</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>varEq</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>


---
## Gtsummary

### Description

Tables via gtsummary

### Usage

    gtsummary(data, dep, group, alt = "notequal", varEq = TRUE)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>group</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>alt</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>varEq</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$gtsum1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>


---
## GtsummaryClass

### Description

Tables via gtsummary

Tables via gtsummary

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::gtsummaryBase` -> `gtsummaryClass`

### Methods

#### Public methods

-   [`gtsummaryClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="gtsummaryBase"
    data-id="initialize"></span>

    [`ClinicoPath::gtsummaryBase$initialize()`](../../ClinicoPath/html/gtsummaryBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    gtsummaryClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Histopathology

### Description

Fake histopathology research data.

Fake histopathology research data.

### Usage

    data(histopathology)

    data(histopathology)

### Format

A data frame

A data frame


---
## Icccoeff

### Description

Function for Interclass Correlation Coefficient Calculation.

### Usage

    icccoeff(data, vars)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>vars</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## IcccoeffClass

### Description

Interclass Correlation Coefficient

Also see
<http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/#ordinal-data-weighted-kappa>

Interclass Correlation Coefficient

Also see
<http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/#ordinal-data-weighted-kappa>

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::icccoeffBase` -> `icccoeffClass`

### Methods

#### Public methods

-   [`icccoeffClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="icccoeffBase"
    data-id="initialize"></span>

    [`ClinicoPath::icccoeffBase$initialize()`](../../ClinicoPath/html/icccoeffBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    icccoeffClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Jjbarstats

### Description

'Wrapper Function for ggstatsplot::ggbarstats and
ggstatsplot::grouped\_ggbarstats to generate Bar Charts.'

### Usage

    jjbarstats(data, dep, group, grvar, excl = TRUE, originaltheme = FALSE)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>group</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>grvar</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>originaltheme</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## Jjbarstats2

### Description

'Wrapper Function for ggstatsplot::ggbarstats and
ggstatsplot::grouped\_ggbarstats to generate Bar Charts.'

### Usage

    jjbarstats2(
      data,
      dep,
      group,
      grvar,
      direction = "independent",
      excl = TRUE,
      originaltheme = FALSE
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>group</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>grvar</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>direction</code></td>
<td><p>select measurement type (repeated or independent)</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>originaltheme</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## Jjbarstats2Class

### Description

Bar Charts

Bar Charts

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::jjbarstats2Base` ->
`jjbarstats2Class`

### Methods

#### Public methods

-   [`jjbarstats2Class$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="jjbarstats2Base"
    data-id="initialize"></span>

    [`ClinicoPath::jjbarstats2Base$initialize()`](../../ClinicoPath/html/jjbarstats2Base.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    jjbarstats2Class$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## JjbarstatsClass

### Description

Bar Charts

Bar Charts

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::jjbarstatsBase` ->
`jjbarstatsClass`

### Methods

#### Public methods

-   [`jjbarstatsClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="jjbarstatsBase"
    data-id="initialize"></span>

    [`ClinicoPath::jjbarstatsBase$initialize()`](../../ClinicoPath/html/jjbarstatsBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    jjbarstatsClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Jjbetweenstats

### Description

Violin Plots to Compare Between Groups

### Usage

    jjbetweenstats(
      data,
      dep,
      group,
      grvar = NULL,
      excl = TRUE,
      typestatistics = "parametric",
      pairwisecomparisons = TRUE,
      pairwisedisplay = "significant",
      padjustmethod = "holm",
      plottype = "boxviolin",
      originaltheme = FALSE
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>group</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>grvar</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>typestatistics</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>pairwisecomparisons</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>pairwisedisplay</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>padjustmethod</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>plottype</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>originaltheme</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## JjbetweenstatsClass

### Description

Violin Plots to Compare Between Groups

Violin Plots to Compare Between Groups

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::jjbetweenstatsBase` ->
`jjbetweenstatsClass`

### Methods

#### Public methods

-   [`jjbetweenstatsClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="jjbetweenstatsBase"
    data-id="initialize"></span>

    [`ClinicoPath::jjbetweenstatsBase$initialize()`](../../ClinicoPath/html/jjbetweenstatsBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    jjbetweenstatsClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Jjcorrmat

### Description

Correlation Matrix

### Usage

    jjcorrmat(
      data,
      dep,
      grvar,
      excl = TRUE,
      typestatistics = "parametric",
      originaltheme = FALSE
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>grvar</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>typestatistics</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>originaltheme</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## JjcorrmatClass

### Description

Correlation Matrix

Correlation Matrix

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::jjcorrmatBase` -> `jjcorrmatClass`

### Methods

#### Public methods

-   [`jjcorrmatClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="jjcorrmatBase"
    data-id="initialize"></span>

    [`ClinicoPath::jjcorrmatBase$initialize()`](../../ClinicoPath/html/jjcorrmatBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    jjcorrmatClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Jjdotplotstats

### Description

Dot Chart

### Usage

    jjdotplotstats(
      data,
      dep,
      group,
      grvar,
      excl = TRUE,
      typestatistics = "parametric",
      originaltheme = FALSE
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>group</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>grvar</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>typestatistics</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>originaltheme</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## JjdotplotstatsClass

### Description

Dot Chart

Dot Chart

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::jjdotplotstatsBase` ->
`jjdotplotstatsClass`

### Methods

#### Public methods

-   [`jjdotplotstatsClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="jjdotplotstatsBase"
    data-id="initialize"></span>

    [`ClinicoPath::jjdotplotstatsBase$initialize()`](../../ClinicoPath/html/jjdotplotstatsBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    jjdotplotstatsClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Jjhistostats

### Description

'Wrapper Function for ggstatsplot::gghistostats and
ggstatsplot::grouped\_gghistostats to generate Histogram.'

### Usage

    jjhistostats(
      data,
      dep,
      grvar,
      excl = TRUE,
      typestatistics = "parametric",
      centralityparameter = "mean",
      changebinwidth = FALSE,
      binwidth = 1.1,
      barmeasure = "count",
      usexlab = FALSE,
      xlab = "",
      useylab = FALSE,
      ylab = "",
      usetitle = FALSE,
      title = "",
      usesubtitle = FALSE,
      subtitle = "",
      useplotcaption = FALSE,
      plotcaption = "",
      usetitleprefix = FALSE,
      titleprefix = "",
      resultssubtitle = TRUE,
      normalcurve = FALSE,
      testvalueline = FALSE
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>grvar</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>typestatistics</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>centralityparameter</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>changebinwidth</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>binwidth</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>barmeasure</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>usexlab</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>xlab</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>useylab</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>ylab</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>usetitle</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>title</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>usesubtitle</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>subtitle</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>useplotcaption</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>plotcaption</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>usetitleprefix</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>titleprefix</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>resultssubtitle</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>normalcurve</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>testvalueline</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## JjhistostatsClass

### Description

Histogram

Histogram

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::jjhistostatsBase` ->
`jjhistostatsClass`

### Methods

#### Public methods

-   [`jjhistostatsClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="jjhistostatsBase"
    data-id="initialize"></span>

    [`ClinicoPath::jjhistostatsBase$initialize()`](../../ClinicoPath/html/jjhistostatsBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    jjhistostatsClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Jjpiestats

### Description

Pie Charts

### Usage

    jjpiestats(data, dep, group, grvar, excl = TRUE, originaltheme = FALSE)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>group</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>grvar</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>originaltheme</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot4</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot3</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## JjpiestatsClass

### Description

Pie Charts

Pie Charts

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::jjpiestatsBase` ->
`jjpiestatsClass`

### Methods

#### Public methods

-   [`jjpiestatsClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="jjpiestatsBase"
    data-id="initialize"></span>

    [`ClinicoPath::jjpiestatsBase$initialize()`](../../ClinicoPath/html/jjpiestatsBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    jjpiestatsClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Jjscatterstats

### Description

Scatter Plot

### Usage

    jjscatterstats(
      data,
      dep,
      group,
      grvar,
      excl = TRUE,
      typestatistics = "parametric",
      originaltheme = FALSE
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>group</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>grvar</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>typestatistics</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>originaltheme</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## JjscatterstatsClass

### Description

Scatter Plot

Scatter Plot

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::jjscatterstatsBase` ->
`jjscatterstatsClass`

### Methods

#### Public methods

-   [`jjscatterstatsClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="jjscatterstatsBase"
    data-id="initialize"></span>

    [`ClinicoPath::jjscatterstatsBase$initialize()`](../../ClinicoPath/html/jjscatterstatsBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    jjscatterstatsClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Jjstatsplot-package

### Description

`jjstatsplot` A wrapper for ggstatsplot: jjstatsplot help researchers to
generate plots in jamovi based on ggstatsplot package.

### Details

`jjstatsplot`

The main functions are-

-   `jjhistostats` function to produce histogram.

<!-- -->

-   `jjbarstats` function to produce Bar Graphs.

For more documentation, see the
[Website](https://sbalci.github.io/jjstatsplot/).

### Author(s)

**Maintainer**: Serdar Balci <drserdarbalci@gmail.com>
([ORCID](https://orcid.org/0000-0002-7852-3851))

### See Also

Useful links:

-   <https://www.serdarbalci.com/ClinicoPathJamoviModule>

-   <https://github.com/sbalci/ClinicoPathJamoviModule/>

-   <https://github.com/sbalci/ClinicoPathDescriptives/>

-   <https://github.com/sbalci/jsurvival>

-   <https://github.com/sbalci/meddecide>

-   <https://github.com/sbalci/jjstatsplot>

-   <https://sbalci.github.io/ClinicoPathJamoviModule/>

-   Report bugs at
    <https://github.com/sbalci/ClinicoPathJamoviModule/issues/>


---
## Jjwithinstats

### Description

Violin Plots to Compare Within Groups

### Usage

    jjwithinstats(
      data,
      dep,
      group,
      grvar,
      typestatistics = "parametric",
      originaltheme = FALSE,
      pointpath = TRUE,
      meanpath = TRUE
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>group</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>grvar</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>typestatistics</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>originaltheme</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>pointpath</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>meanpath</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## Jjwithinstats2

### Description

Violin Plots to Compare Within Groups for Wide Data Format

### Usage

    jjwithinstats2(data, pairs, excl = TRUE, originaltheme = FALSE)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>pairs</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>originaltheme</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## Jjwithinstats2Class

### Description

Violin Plots to Compare Within Group for Wide Data Format

Violin Plots to Compare Within Group for Wide Data Format

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::jjwithinstats2Base` ->
`jjwithinstats2Class`

### Methods

#### Public methods

-   [`jjwithinstats2Class$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="jjwithinstats2Base"
    data-id="initialize"></span>

    [`ClinicoPath::jjwithinstats2Base$initialize()`](../../ClinicoPath/html/jjwithinstats2Base.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    jjwithinstats2Class$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Jjwithinstats3

### Description

Violin Plots to Compare Repeated Measurements for Continuous Variables

### Usage

    jjwithinstats3(data, dep, excl = TRUE, originaltheme = FALSE)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>originaltheme</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## Jjwithinstats3Class

### Description

Violin Plots to Compare Within Group for Wide Data Format 3

Violin Plots to Compare Within Group for Wide Data Format 3

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::jjwithinstats3Base` ->
`jjwithinstats3Class`

### Methods

#### Public methods

-   [`jjwithinstats3Class$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="jjwithinstats3Base"
    data-id="initialize"></span>

    [`ClinicoPath::jjwithinstats3Base$initialize()`](../../ClinicoPath/html/jjwithinstats3Base.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    jjwithinstats3Class$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## JjwithinstatsClass

### Description

Violin Plots to Compare Within Group

Violin Plots to Compare Within Group

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::jjwithinstatsBase` ->
`jjwithinstatsClass`

### Methods

#### Public methods

-   [`jjwithinstatsClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="jjwithinstatsBase"
    data-id="initialize"></span>

    [`ClinicoPath::jjwithinstatsBase$initialize()`](../../ClinicoPath/html/jjwithinstatsBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    jjwithinstatsClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Jviolin

### Description

Violin Plot

### Usage

    jviolin(
      data,
      dep,
      group,
      col,
      fill,
      excl = TRUE,
      flip = FALSE,
      themex = "ipsum",
      usexlabel = FALSE,
      xlabel = "",
      useylabel = FALSE,
      ylabel = ""
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>group</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>col</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>fill</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>flip</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>themex</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>usexlabel</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>xlabel</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>useylabel</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>ylabel</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>


---
## JviolinClass

### Description

Violin Plot

Violin Plot

### Value

Violin Plot

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::jviolinBase` -> `jviolinClass`

### Methods

#### Public methods

-   [`jviolinClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="jviolinBase"
    data-id="initialize"></span>

    [`ClinicoPath::jviolinBase$initialize()`](../../ClinicoPath/html/jviolinBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    jviolinClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Linechart

### Description

Line Chart

### Usage

    linechart(data, xvar, yvar)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>xvar</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>yvar</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>


---
## Lollipop

### Description

Lollipop Chart

### Usage

    lollipop(data, dep, group, highlight)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>group</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>highlight</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>


---
## Multisurvival

### Description

Function for Multivariable Survival Analysis using Cox-regression.

### Usage

    multisurvival(
      data,
      elapsedtime,
      tint = FALSE,
      dxdate,
      fudate,
      outcome,
      outcomeLevel,
      dod,
      dooc,
      awd,
      awod,
      explanatory,
      contexpl,
      multievent = FALSE,
      analysistype = "overall",
      timetypedata = "ymd",
      timetypeoutput = "months",
      hr = FALSE,
      sty = "t1",
      km = FALSE,
      ac = FALSE,
      adjexplanatory,
      endplot = 60,
      byplot = 12,
      ci95 = FALSE,
      risktable = FALSE
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>elapsedtime</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>tint</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dxdate</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>fudate</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>outcome</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>outcomeLevel</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dod</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>dooc</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>awd</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>awod</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>explanatory</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>contexpl</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>multievent</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>analysistype</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>timetypedata</code></td>
<td><p>select the time type in data</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>timetypeoutput</code></td>
<td><p>select the time type in output</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>hr</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>sty</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>km</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>ac</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>adjexplanatory</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>endplot</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>byplot</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>ci95</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>risktable</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$text2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot3</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plotKM</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot7</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$calculatedtime</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an output</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$outcomeredifened</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an output</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## MultisurvivalClass

### Description

Multivariable Survival Analysis

Multivariable Survival Analysis

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::multisurvivalBase` ->
`multisurvivalClass`

### Methods

#### Public methods

-   [`multisurvivalClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="multisurvivalBase"
    data-id="initialize"></span>

    [`ClinicoPath::multisurvivalBase$initialize()`](../../ClinicoPath/html/multisurvivalBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    multisurvivalClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Nomogram

### Description

Nomogram

### Usage

    nomogram(data, dep, group, alt = "notequal", varEq = TRUE)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>group</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>alt</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>varEq</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>


---
## Oddsratio

### Description

Function for Odds Ratio Table and Plot.

### Usage

    oddsratio(data, explanatory, outcome)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>explanatory</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>outcome</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$text2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## OddsratioClass

### Description

Odds Ratio Table and Plot

Odds Ratio Table and Plot

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::oddsratioBase` -> `oddsratioClass`

### Methods

#### Public methods

-   [`oddsratioClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="oddsratioBase"
    data-id="initialize"></span>

    [`ClinicoPath::oddsratioBase$initialize()`](../../ClinicoPath/html/oddsratioBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    oddsratioClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## OneSurvival

### Description

One Survival Outcome

### Usage

    oneSurvival(data, times, status, ciyn = FALSE, timeunits = "None")

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>times</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>status</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>ciyn</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>timeunits</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$onesurvTable1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$onesurvPlot1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

Tables can be converted to data frames with `asDF` or `as.data.frame`.
For example:

`results$onesurvTable1$asDF`

`as.data.frame(results$onesurvTable1)`


---
## Pairchi2

### Description

Pairwise Chi-Square Test

### Usage

    pairchi2(data, row, col)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>row</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>col</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$conftable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$chi2test</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$pairwise1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$pairwise2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$pairwise3</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>


---
## Pairchi2Class

### Description

Title

Title

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::pairchi2Base` -> `pairchi2Class`

### Methods

#### Public methods

-   [`pairchi2Class$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="pairchi2Base"
    data-id="initialize"></span>

    [`ClinicoPath::pairchi2Base$initialize()`](../../ClinicoPath/html/pairchi2Base.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    pairchi2Class$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Parallelplot

### Description

Parallel Plot

### Usage

    parallelplot(data, dep, group, alt = "notequal", varEq = TRUE)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>group</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>alt</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>varEq</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>


---
## Pipe

### Description

Pipe operator from
<https://github.com/poldham/covidlens/blob/master/R/utils-pipe.R> See
`magrittr::%>%` for details.

### Usage

    lhs %>% rhs


---
## Ppv

### Description

Positive Predictive Value

### Usage

    ppv(percTrue = 50, alpha = 0.05, power = 0.8, percHack = 0)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>percTrue</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>alpha</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>power</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>percHack</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$confusion</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table containing the true/false positives/negatives</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$ppv</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$dotPlot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

Tables can be converted to data frames with `asDF` or `as.data.frame`.
For example:

`results$confusion$asDF`

`as.data.frame(results$confusion)`


---
## Reportcat

### Description

Function for Generating Summaries for Categorical Variables.

### Usage

    reportcat(data, vars)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>the data as a data frame</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>vars</code></td>
<td><p>string naming the variables from <code>data</code> that contains the values used for the report.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## ReportcatClass

### Description

Summary of Categorical Variables

Summary of Categorical Variables

### Value

Text

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::reportcatBase` -> `reportcatClass`

### Methods

#### Public methods

-   [`reportcatClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="reportcatBase"
    data-id="initialize"></span>

    [`ClinicoPath::reportcatBase$initialize()`](../../ClinicoPath/html/reportcatBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    reportcatClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Retracted

### Description

Find Retracted Papers from DOI

### Usage

    retracted(data, doi)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>doi</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$text4</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text5</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>


---
## RetractedClass

### Description

Find Retracted Papers from DOI

Find Retracted Papers from DOI

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::retractedBase` -> `retractedClass`

### Methods

#### Public methods

-   [`retractedClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="retractedBase"
    data-id="initialize"></span>

    [`ClinicoPath::retractedBase$initialize()`](../../ClinicoPath/html/retractedBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    retractedClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Roc

### Description

Function for ROC Analysis.

### Usage

    roc(
      data,
      measurement,
      status,
      excl = TRUE,
      sty = FALSE,
      quant = FALSE,
      label = FALSE,
      inter = FALSE
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>measurement</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>status</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>sty</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>quant</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>label</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>inter</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$textplot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## RocClass

### Description

ROC Analysis

ROC Analysis

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::rocBase` -> `rocClass`

### Methods

#### Public methods

-   [`rocClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="rocBase"
    data-id="initialize"></span>

    [`ClinicoPath::rocBase$initialize()`](../../ClinicoPath/html/rocBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    rocClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Rocdata

### Description

Example data for ROC analysis

### Usage

    data(rocdata)

### Format

A data frame


---
## Statsplot2

### Description

Function for Generating Plots and Graphs Based on Variable Types.

### Usage

    statsplot2(
      data,
      dep,
      group,
      grvar,
      direction = "independent",
      distribution = "p",
      alluvsty = "t1",
      excl = TRUE,
      originaltheme = FALSE
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dep</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>group</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>grvar</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>direction</code></td>
<td><p>select measurement type (repeated or independent)</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>distribution</code></td>
<td><p>select distribution type (parametric or nonparametric)</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>alluvsty</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>originaltheme</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text4</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## Statsplot2Class

### Description

Plots and Graphs Based on Variable Types

Plots and Graphs Based on Variable Types

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::statsplot2Base` ->
`statsplot2Class`

### Methods

#### Public methods

-   [`statsplot2Class$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="statsplot2Base"
    data-id="initialize"></span>

    [`ClinicoPath::statsplot2Base$initialize()`](../../ClinicoPath/html/statsplot2Base.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    statsplot2Class$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Summarydata

### Description

Function for Generating Summaries for Continuous Variables.

### Usage

    summarydata(data, vars)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>vars</code></td>
<td><p>a string naming the variables from <code>data</code> that contains the continuous values used for the report</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## SummarydataClass

### Description

Summary of Continuous Variables

Summary of Continuous Variables

### Value

Text

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::summarydataBase` ->
`summarydataClass`

### Methods

#### Public methods

-   [`summarydataClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="summarydataBase"
    data-id="initialize"></span>

    [`ClinicoPath::summarydataBase$initialize()`](../../ClinicoPath/html/summarydataBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    summarydataClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Survival

### Description

Function for Generating Summaries for Survival Analysis.

### Usage

    survival(
      data,
      elapsedtime,
      tint = FALSE,
      dxdate,
      fudate,
      explanatory,
      outcome,
      outcomeLevel,
      dod,
      dooc,
      awd,
      awod,
      analysistype = "overall",
      cutp = "12, 36, 60",
      timetypedata = "ymd",
      timetypeoutput = "months",
      uselandmark = FALSE,
      landmark = 3,
      pw = FALSE,
      padjustmethod = "holm",
      sc = FALSE,
      kmunicate = FALSE,
      ce = FALSE,
      ch = FALSE,
      endplot = 60,
      byplot = 12,
      multievent = FALSE,
      ci95 = FALSE,
      risktable = FALSE,
      censored = FALSE,
      sas = FALSE
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>elapsedtime</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>tint</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dxdate</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>fudate</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>explanatory</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>outcome</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>outcomeLevel</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>dod</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dooc</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>awd</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>awod</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>analysistype</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>cutp</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>timetypedata</code></td>
<td><p>select the time type in data</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>timetypeoutput</code></td>
<td><p>select the time type in output</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>uselandmark</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>landmark</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>pw</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>padjustmethod</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>sc</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>kmunicate</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>ce</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>ch</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>endplot</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>byplot</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>multievent</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>ci95</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>risktable</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>censored</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>sas</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$medianSummary</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$medianTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$coxSummary</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$coxTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$tCoxtext2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$survTableSummary</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$survTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$pairwiseSummary</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$pairwiseTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot3</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot6</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$calculatedtime</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an output</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$outcomeredifened</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an output</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

Tables can be converted to data frames with `asDF` or `as.data.frame`.
For example:

`results$medianTable$asDF`

`as.data.frame(results$medianTable)`

### Examples

```r
# example will be added
```


---
## SurvivalClass

### Description

Survival Analysis

Survival Analysis

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::survivalBase` -> `survivalClass`

### Methods

#### Public methods

-   [`survivalClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="survivalBase"
    data-id="initialize"></span>

    [`ClinicoPath::survivalBase$initialize()`](../../ClinicoPath/html/survivalBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    survivalClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Survivalcont

### Description

Survival Analysis for Continuous Variable

### Usage

    survivalcont(
      data,
      elapsedtime,
      tint = FALSE,
      dxdate,
      fudate,
      contexpl,
      outcome,
      outcomeLevel,
      dod,
      dooc,
      awd,
      awod,
      analysistype = "overall",
      cutp = "12, 36, 60",
      timetypedata = "ymd",
      timetypeoutput = "months",
      sc = FALSE,
      kmunicate = FALSE,
      ce = FALSE,
      ch = FALSE,
      endplot = 60,
      byplot = 12,
      findcut = FALSE,
      multievent = FALSE,
      ci95 = FALSE,
      risktable = FALSE
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>elapsedtime</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>tint</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dxdate</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>fudate</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>contexpl</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>outcome</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>outcomeLevel</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>dod</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>dooc</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>awd</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>awod</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>analysistype</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>cutp</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>timetypedata</code></td>
<td><p>select the time type in data</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>timetypeoutput</code></td>
<td><p>select the time type in output</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>sc</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>kmunicate</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>ce</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>ch</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>endplot</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>byplot</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>findcut</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>multievent</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>ci95</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>risktable</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$coxSummary</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$coxTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$tCoxtext2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$rescutTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot4</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot5</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$medianSummary</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$medianTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$survTableSummary</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$survTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$plot3</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot6</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$calculatedtime</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an output</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$outcomeredifened</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an output</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$calculatedcutoff</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an output</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

Tables can be converted to data frames with `asDF` or `as.data.frame`.
For example:

`results$coxTable$asDF`

`as.data.frame(results$coxTable)`


---
## SurvivalcontClass

### Description

Survival Analysis for Continuous Explanatory Variable

Survival Analysis for Continuous Explanatory Variable

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::survivalcontBase` ->
`survivalcontClass`

### Methods

#### Public methods

-   [`survivalcontClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="survivalcontBase"
    data-id="initialize"></span>

    [`ClinicoPath::survivalcontBase$initialize()`](../../ClinicoPath/html/survivalcontBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    survivalcontClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Tableone

### Description

Function for making Table One.

### Usage

    tableone(data, vars, sty = "t1", excl = TRUE)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>vars</code></td>
<td><p>a string naming the variables from <code>data</code> that contains the values used for the Table One.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>sty</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$tablestyle1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$tablestyle2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$tablestyle3</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$tablestyle4</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
data('histopathology')
dat <- as.data.frame(histopathology)
ClinicoPath::tableone(
data = dat,
vars = vars(Sex, PreinvasiveComponent, LVI, PNI, Grade, Age),
sty = "t3",
excl = TRUE)

## End(Not run)
```


---
## TableoneClass

### Description

Table One

Table One

### Value

Table

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::tableoneBase` -> `tableoneClass`

### Methods

#### Public methods

-   [`tableoneClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="tableoneBase"
    data-id="initialize"></span>

    [`ClinicoPath::tableoneBase$initialize()`](../../ClinicoPath/html/tableoneBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    tableoneClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Timeinterval

### Description

Time Interval

### Usage

    timeinterval(data, explanatory, overalltime, outcome, outcomeLevel)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>explanatory</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>overalltime</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>outcome</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>outcomeLevel</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$text1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text1table</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$text1html</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$medianTable</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a table</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

Tables can be converted to data frames with `asDF` or `as.data.frame`.
For example:

`results$medianTable$asDF`

`as.data.frame(results$medianTable)`


---
## Tree

### Description

Function for making Decision Trees.

### Usage

    tree(
      data,
      vars,
      facs,
      target,
      targetLevel,
      train,
      trainLevel,
      showPlot = FALSE
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>vars</code></td>
<td><p>continuous explanatory variables</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>facs</code></td>
<td><p>categorical explanatory variables</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>target</code></td>
<td><p>target variable</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>targetLevel</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>train</code></td>
<td><p>Variable containing Test/Train information</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>trainLevel</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>showPlot</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$text2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text2a</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$text2b</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text3</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$text4</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## TreeClass

### Description

Decision Tree

Decision Tree

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::treeBase` -> `treeClass`

### Methods

#### Public methods

-   [`treeClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="treeBase"
    data-id="initialize"></span>

    [`ClinicoPath::treeBase$initialize()`](../../ClinicoPath/html/treeBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    treeClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Vartree

### Description

Function for Generating Tree Summaries of Variables.

### Usage

    vartree(
      data,
      vars,
      percvar,
      percvarLevel,
      summaryvar,
      prunebelow,
      pruneLevel1,
      pruneLevel2,
      follow,
      followLevel1,
      followLevel2,
      excl = TRUE,
      vp = TRUE,
      horizontal = FALSE,
      sline = TRUE,
      varnames = FALSE,
      nodelabel = TRUE,
      pct = FALSE,
      showcount = TRUE,
      legend = FALSE,
      pattern = FALSE,
      sequence = FALSE,
      ptable = FALSE,
      mytitle = "",
      useprunesmaller = FALSE,
      prunesmaller = 5,
      summarylocation = "leafonly"
    )

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>vars</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>percvar</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>percvarLevel</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>summaryvar</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>prunebelow</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>pruneLevel1</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>pruneLevel2</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>follow</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>followLevel1</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>followLevel2</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>excl</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>vp</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>horizontal</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>sline</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>varnames</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>nodelabel</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>pct</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>showcount</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>legend</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>pattern</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>sequence</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>ptable</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>mytitle</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>useprunesmaller</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>prunesmaller</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>summarylocation</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$text1</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><code>results$text2</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a preformatted</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>

### Examples

```r
## Not run: 
# example will be added

## End(Not run)
```


---
## VartreeClass

### Description

Variable Tree

Variable Tree

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::vartreeBase` -> `vartreeClass`

### Methods

#### Public methods

-   [`vartreeClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="vartreeBase"
    data-id="initialize"></span>

    [`ClinicoPath::vartreeBase$initialize()`](../../ClinicoPath/html/vartreeBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    vartreeClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
## Venn

### Description

Venn Diagram

### Usage

    venn(data, var1, var1true, var2, var2true, var3, var3true, var4, var4true)

### Arguments

<table data-summary="R argblock">
<tbody>
<tr class="odd" data-valign="top">
<td><code>data</code></td>
<td><p>The data as a data frame.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>var1</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>var1true</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>var2</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>var2true</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>var3</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>var3true</code></td>
<td><p>.</p></td>
</tr>
<tr class="even" data-valign="top">
<td><code>var4</code></td>
<td><p>.</p></td>
</tr>
<tr class="odd" data-valign="top">
<td><code>var4true</code></td>
<td><p>.</p></td>
</tr>
</tbody>
</table>

### Value

A results object containing:

<table data-summary="Rd table">
<tbody>
<tr class="odd">
<td style="text-align: left;"><code>results$todo</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">a html</td>
</tr>
<tr class="even">
<td style="text-align: left;"><code>results$plot</code></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;">an image</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
<td style="text-align: left;"></td>
</tr>
</tbody>
</table>


---
## VennClass

### Description

Venn Diagram

Venn Diagram

### Super classes

`jmvcore::Analysis` -> `ClinicoPath::vennBase` -> `vennClass`

### Methods

#### Public methods

-   [`vennClass$clone()`](#method-clone)

Inherited methods

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImage"></span>

    [`jmvcore::Analysis$.createImage()`](../../jmvcore/html/Analysis.html#method-.createImage)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createImages"></span>

    [`jmvcore::Analysis$.createImages()`](../../jmvcore/html/Analysis.html#method-.createImages)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".createPlotObject"></span>

    [`jmvcore::Analysis$.createPlotObject()`](../../jmvcore/html/Analysis.html#method-.createPlotObject)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".load"></span>

    [`jmvcore::Analysis$.load()`](../../jmvcore/html/Analysis.html#method-.load)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".render"></span>

    [`jmvcore::Analysis$.render()`](../../jmvcore/html/Analysis.html#method-.render)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".save"></span>

    [`jmvcore::Analysis$.save()`](../../jmvcore/html/Analysis.html#method-.save)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".savePart"></span>

    [`jmvcore::Analysis$.savePart()`](../../jmvcore/html/Analysis.html#method-.savePart)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setCheckpoint"></span>

    [`jmvcore::Analysis$.setCheckpoint()`](../../jmvcore/html/Analysis.html#method-.setCheckpoint)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setParent"></span>

    [`jmvcore::Analysis$.setParent()`](../../jmvcore/html/Analysis.html#method-.setParent)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetHeaderSource"></span>

    [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetHeaderSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setReadDatasetSource"></span>

    [`jmvcore::Analysis$.setReadDatasetSource()`](../../jmvcore/html/Analysis.html#method-.setReadDatasetSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setResourcesPathSource"></span>

    [`jmvcore::Analysis$.setResourcesPathSource()`](../../jmvcore/html/Analysis.html#method-.setResourcesPathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id=".setStatePathSource"></span>

    [`jmvcore::Analysis$.setStatePathSource()`](../../jmvcore/html/Analysis.html#method-.setStatePathSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="addAddon"></span>

    [`jmvcore::Analysis$addAddon()`](../../jmvcore/html/Analysis.html#method-addAddon)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asProtoBuf"></span>

    [`jmvcore::Analysis$asProtoBuf()`](../../jmvcore/html/Analysis.html#method-asProtoBuf)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="asSource"></span>

    [`jmvcore::Analysis$asSource()`](../../jmvcore/html/Analysis.html#method-asSource)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="check"></span>

    [`jmvcore::Analysis$check()`](../../jmvcore/html/Analysis.html#method-check)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="init"></span>

    [`jmvcore::Analysis$init()`](../../jmvcore/html/Analysis.html#method-init)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="optionsChangedHandler"></span>

    [`jmvcore::Analysis$optionsChangedHandler()`](../../jmvcore/html/Analysis.html#method-optionsChangedHandler)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="postInit"></span>

    [`jmvcore::Analysis$postInit()`](../../jmvcore/html/Analysis.html#method-postInit)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="print"></span>

    [`jmvcore::Analysis$print()`](../../jmvcore/html/Analysis.html#method-print)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="readDataset"></span>

    [`jmvcore::Analysis$readDataset()`](../../jmvcore/html/Analysis.html#method-readDataset)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="run"></span>

    [`jmvcore::Analysis$run()`](../../jmvcore/html/Analysis.html#method-run)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="serialize"></span>

    [`jmvcore::Analysis$serialize()`](../../jmvcore/html/Analysis.html#method-serialize)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setError"></span>

    [`jmvcore::Analysis$setError()`](../../jmvcore/html/Analysis.html#method-setError)

-   <span class="pkg-link" pkg="jmvcore" topic="Analysis"
    data-id="setStatus"></span>

    [`jmvcore::Analysis$setStatus()`](../../jmvcore/html/Analysis.html#method-setStatus)

-   <span class="pkg-link" pkg="ClinicoPath" topic="vennBase"
    data-id="initialize"></span>

    [`ClinicoPath::vennBase$initialize()`](../../ClinicoPath/html/vennBase.html#method-initialize)

------------------------------------------------------------------------

<span id="method-clone"></span>

#### Method `clone()`

The objects of this class are cloneable with this method.

##### Usage

    vennClass$clone(deep = FALSE)

##### Arguments

`deep`  
Whether to make a deep clone.


---
