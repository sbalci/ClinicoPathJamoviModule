patientfollowupintroClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "patientfollowupintroClass",
    inherit = patientfollowupintroBase,
    private = list(

        .init = function() {
            self$results$overview$setContent(private$.overviewHtml())
            self$results$dataStructures$setContent(private$.dataStructuresHtml())
            self$results$limitations$setContent(private$.limitationsHtml())
            self$results$recistRules$setContent(private$.recistRulesHtml())
            self$results$glossary$setContent(private$.glossaryHtml())
        },

        .run = function() {
            # Nothing to compute: this analysis is documentation.
        },

        # =====================================================================
        # Shared styling
        # =====================================================================

        .cssBlock = function() {
            '<style>
            .pfg { max-width: 860px; line-height: 1.6; }
            .pfg h3 { margin-top: 22px; margin-bottom: 6px; }
            .pfg table { width: 100%; border-collapse: collapse; margin-bottom: 18px; }
            .pfg th { padding: 8px; text-align: left; border-bottom: 2px solid #333; vertical-align: top; }
            .pfg td { padding: 8px; border-bottom: 1px solid #ccc; vertical-align: top; }
            .pfg td.name { width: 22%; font-weight: 600; }
            .pfg pre { background: rgba(105, 138, 172, 0.06); color: inherit; border: 1px solid #d0d7de; padding: 10px;
                       border-radius: 4px; overflow-x: auto; font-size: 12px; }
            .pfg .callout { background: rgba(188, 149, 33, 0.1); color: inherit; border-left: 4px solid #c9a825;
                            padding: 12px 16px; margin: 16px 0; }
            .pfg .warn { background: rgba(255, 72, 38, 0.06); color: inherit; border-left: 4px solid #d1493f;
                         padding: 12px 16px; margin: 16px 0; }
            .pfg .ok { background: rgba(33, 144, 49, 0.06); color: inherit; border-left: 4px solid #2e7d32;
                       padding: 12px 16px; margin: 16px 0; }
            </style>'
        },

        # =====================================================================
        # 1. Which analysis fits my data?
        # =====================================================================

        .overviewHtml = function() {
            paste0(private$.cssBlock(), '<div class="pfg">

<p>Start from the data you already have. The analyses differ mainly in how finely
your tumour measurements are recorded, and each one needs a specific layout.</p>

<table>
<tr><th>If your data has...</th><th>Use</th><th>What you get</th></tr>

<tr><td>One number per patient: a percent change from baseline you already calculated</td>
    <td class="name">Treatment Response: Patient-Level Burden<br><i>(waterfall)</i></td>
    <td>Waterfall plot, response categories, ORR and DCR with exact confidence intervals,
        group comparison, a copy-ready paragraph</td></tr>

<tr><td>One number per patient <b>per visit</b>: a single measurement or summed burden at each scan</td>
    <td class="name">Treatment Response: Patient-Level Burden<br><i>(waterfall)</i></td>
    <td>The above, plus a spider plot, time to response and duration of response.
        Progression is measured against the patient\'s smallest recorded burden (nadir)</td></tr>

<tr><td>One row <b>per lesion per visit</b>: each lesion measured at each scan</td>
    <td class="name">Treatment Response: Lesion-Level RECIST v1.1 Algorithm<br><i>(waterfallrecist)</i>
        <br>or<br>RECIST 1.1 Multi-Lesion Aggregation<br><i>(recist)</i></td>
    <td>Target lesion summation, nadir-referenced progression with the 5 mm rule,
        new-lesion detection, non-target assessment, confirmation, best overall response</td></tr>

<tr><td>An <b>already-summed</b> target lesion sum per visit, plus new-lesion and
        non-target columns, in an <b>immunotherapy</b> trial</td>
    <td class="name">iRECIST Analysis<br><i>(irecist)</i></td>
    <td>iRECIST categories including iUPD and iCPD, so that pseudoprogression is not
        mistaken for treatment failure</td></tr>

<tr><td>One row per patient with a <b>start and end time</b>, and optionally milestones
        (surgery, response, progression)</td>
    <td class="name">Swimmer Plot<br><i>(swimmerplot)</i></td>
    <td>A horizontal timeline per patient showing treatment duration, milestones and
        ongoing status. A picture of the course of treatment, not a response calculation</td></tr>

<tr><td>One row per patient per <b>adverse event</b></td>
    <td class="name">Adverse Event Plot<br><i>(aeplot)</i></td>
    <td>Butterfly / tornado plot comparing event frequencies between arms. Safety, not response</td></tr>
</table>

<div class="callout">
<b>Two analyses read lesion-level data.</b> They apply the same RECIST v1.1 criteria
(they share one engine), and differ in presentation: <i>waterfallrecist</i> leads with the
waterfall and spider plots, <i>recist</i> leads with the per-lesion and per-visit tables.
Pick whichever output you want to read; the response assignments agree.
</div>

<h3>Not follow-up analyses</h3>
<p>Two neighbours in the same menu answer a different question and need only a single
timepoint:</p>
<table>
<tr><td class="name">Lymph Node Ratio</td>
    <td>Positive nodes divided by examined nodes, as a prognostic index. One row per patient</td></tr>
<tr><td class="name">Residual Cancer Burden</td>
    <td>RCB index and class from post-neoadjuvant pathology. One row per patient</td></tr>
</table>
</div>')
        },

        # =====================================================================
        # 2. Data layouts
        # =====================================================================

        .dataStructuresHtml = function() {
            paste0(private$.cssBlock(), '<div class="pfg">

<p>Each layout below is a complete, minimal example. Match yours to one of these before
choosing an analysis.</p>

<h3>A. One value per patient (waterfall, percentage input)</h3>
<p>You have already calculated each patient\'s best percent change from baseline.</p>
<pre>PatientID   BestResponse   Treatment
PT1              -45          A
PT2              -10          A
PT3              +30          B</pre>
<p>Negative values are shrinkage. One row per patient; no time column needed.</p>

<h3>B. One value per patient per visit (waterfall, raw input)</h3>
<p>A single measurement, or a burden you summed yourself, recorded at each scan.</p>
<pre>PatientID   Time   TumorSize
PT1            0        100
PT1            6         60
PT1           12         78
PT2            0        100
PT2            6         55</pre>
<div class="callout">
The baseline row <b>must</b> be present and is identified by <code>Time = 0</code>.
Percent change is computed from it. This layout also unlocks the spider plot and
duration of response.
</div>

<h3>C. One row per lesion per visit (waterfallrecist, recist)</h3>
<p>The layout RECIST v1.1 actually requires: every lesion followed individually.</p>
<pre>PatientID  LesionID  VisitTime  LesionType   Location  Diameter  IsNew
PT1        L1            0       Target       Liver        60       No
PT1        L2            0       Target       Lung         40       No
PT1        L1            8       Target       Liver        36       No
PT1        L2            8       Target       Lung         24       No
PT1        N1            8       Non-Target   Bone          9       Yes</pre>
<p>Required: patient, lesion, visit time, diameter. Each optional column unlocks a rule:</p>
<table>
<tr><td class="name">Lesion Type</td><td>Target / Non-Target / New. Without it every lesion is
    treated as a target lesion, and non-target progression cannot be assessed</td></tr>
<tr><td class="name">New Lesion Indicator</td><td>Yes/No, 1/0 or TRUE/FALSE. Any new lesion is
    automatic progression</td></tr>
<tr><td class="name">Location (organ)</td><td>Applies the limit of two target lesions per organ</td></tr>
<tr><td class="name">Non-Target Response</td><td>The radiologist\'s CR / Non-CR/Non-PD / PD call.
    Strongly recommended - see the limitations section</td></tr>
</table>

<h3>D. Pre-summed target burden per visit (irecist)</h3>
<pre>PatientID  AssessmentTime  TargetLesionSum  NewLesions  NonTargetStatus
PT1              0                110            No         Non-CR/Non-PD
PT1              8                 70            No         Non-CR/Non-PD
PT1             16                 95           Yes         Non-CR/Non-PD</pre>
<p>You have done the summation; iRECIST applies the immunotherapy criteria on top.</p>

<h3>E. One row per patient with a time span (swimmerplot)</h3>
<pre>PatientID  StartTime  EndTime  Response  Ongoing  Surgery
PT1              0        14       PR       Yes         2
PT2              0         6       PD        No         1</pre>
<p>Milestone columns are optional and are drawn as markers on each patient\'s bar.</p>
</div>')
        },

        # =====================================================================
        # 3. Limitations
        # =====================================================================

        .limitationsHtml = function() {
            paste0(private$.cssBlock(), '<div class="pfg">

<p>Every analysis here is honest about its scope, and each states its limits in its own
output as well. Read this before reporting a number.</p>

<h3>Treatment Response: Patient-Level Burden (waterfall)</h3>
<div class="warn">
<b>This is not a RECIST v1.1 implementation.</b> It sees one number per patient per visit
and never sees individual lesions, so it cannot:
<ul>
<li>sum target lesions - it assumes the number you gave it is the whole burden</li>
<li>detect a new lesion - no column can express one, and a new lesion is automatic
    progression under RECIST</li>
<li>assess non-target disease</li>
<li>apply the 4-week confirmation rule itself (you may supply your own confirmation column)</li>
</ul>
Categories are named CR, PR, SD and PD and the thresholds are adapted from RECIST v1.1,
but a patient labelled PR here has not had a RECIST assessment.
</div>
<p>What it does do well: when a time variable is supplied, progression is measured against
the patient\'s <b>smallest recorded burden</b> (the nadir), not against baseline - so a
patient who shrinks and then regrows is correctly recorded as progressing. It also has the
fullest reporting of the family: exact binomial confidence intervals, group comparison,
Kaplan-Meier duration of response and a copy-ready paragraph.</p>

<h3>Lesion-level RECIST (waterfallrecist, recist)</h3>
<div class="warn">
<b>Not validated against a reference implementation.</b> These apply the RECIST v1.1
algorithm, but neither has been checked against a certified RECIST tool or a regulatory
dataset. Treat the output as a research result and confirm response assignments against the
source imaging before they are recorded or reported.
</div>
<div class="callout">
<b>Non-target progression is estimated unless you supply it.</b> RECIST defines it as
<i>unequivocal progression</i> - a qualitative radiological judgement. With no reader
assessment the analysis falls back to a lesion-count heuristic (two or more extra
non-target lesions is called progression), which is not the RECIST criterion and can both
miss and over-call progression. Assign the <b>Non-Target Response</b> variable to record
the reader\'s own call; it overrides the estimate.
</div>
<div class="callout">
<b>Target lesions are selected automatically.</b> RECIST follows at most five target
lesions and two per organ. If your data exceeds that, the largest are selected and the rest
are followed as non-target disease, with a message saying which moved. Size alone does not
establish that a lesion is reproducibly measurable, so use the <b>Target Lesion Selection</b>
variable when the reader\'s choice differs.
</div>

<h3>iRECIST</h3>
<p>Requires you to have summed the target lesions already. It adds the immunotherapy
criteria (iUPD, iCPD) on top of that sum, so any error in your summation carries straight
through.</p>

<h3>Swimmer plot</h3>
<p>A visualisation, not a response calculation. It draws whatever response labels you give
it and applies no criteria of its own. Use it alongside a response analysis, not instead of one.</p>

<h3>Adverse event plot</h3>
<p>Describes safety, not efficacy. Frequencies are descriptive; a difference between arms
here is not a formal safety comparison.</p>

<div class="ok">
<b>None of these analyses is validated for regulatory submission.</b> They are for
exploratory work, pilot studies, teaching and hypothesis generation. For registration
endpoints use RECIST-certified software.
</div>
</div>')
        },

        # =====================================================================
        # 4. RECIST rules
        # =====================================================================

        .recistRulesHtml = function() {
            paste0(private$.cssBlock(), '<div class="pfg">

<p>The criteria the lesion-level analyses apply, in plain language
(Eisenhauer et al., <i>Eur J Cancer</i> 2009;45:228-247).</p>

<table>
<tr><th>Category</th><th>Target lesions</th></tr>
<tr><td class="name">Complete response (CR)</td>
    <td>All target lesions have disappeared</td></tr>
<tr><td class="name">Partial response (PR)</td>
    <td>The sum of diameters has fallen by <b>at least 30%</b>, compared with the
        <b>baseline</b> sum</td></tr>
<tr><td class="name">Progressive disease (PD)</td>
    <td>The sum has risen by <b>at least 20%</b> compared with the <b>smallest sum recorded
        so far</b> (the nadir, which may be baseline), <b>and</b> by at least 5 mm in
        absolute terms. A new lesion is progression regardless of the sum</td></tr>
<tr><td class="name">Stable disease (SD)</td>
    <td>Neither enough shrinkage for PR nor enough growth for PD</td></tr>
</table>

<div class="callout">
<b>Why the nadir matters.</b> Take a patient whose burden goes 100 mm, then 60 mm, then
78 mm. Against baseline that last scan is still 22% <i>below</i> where they started, so a
baseline-referenced rule sees no progression. Against their nadir of 60 mm it is 30% higher
and 18 mm larger - which is progression. Measuring from baseline instead of the nadir is
the single most common way a responder who later regrows is missed.
</div>

<h3>Two rules that need more than measurements</h3>
<ul>
<li><b>Confirmation.</b> A CR or PR must be repeated at a later assessment, at least four
    weeks on, before it counts. An unconfirmed response followed by progression is not a
    response.</li>
<li><b>Best overall response.</b> The best assessment recorded from the start of treatment
    <b>until progression</b>. Anything measured after progression belongs to a later line
    of therapy.</li>
</ul>

<h3>iRECIST, for immunotherapy</h3>
<p>Immunotherapy can cause tumours to enlarge before they shrink (pseudoprogression).
iRECIST therefore treats a first progression as <b>unconfirmed</b> (iUPD) and requires a
later scan to confirm it (iCPD). If the tumour shrinks again instead, the patient returns
to a response category rather than being called a treatment failure.</p>
</div>')
        },

        # =====================================================================
        # 5. Glossary
        # =====================================================================

        .glossaryHtml = function() {
            paste0(private$.cssBlock(), '<div class="pfg">
<table>
<tr><td class="name">Baseline</td><td>The pre-treatment assessment all change is measured from.
    In these analyses it is the visit at time 0 and it must be present</td></tr>
<tr><td class="name">Nadir</td><td>The smallest tumour burden recorded so far for that patient.
    Progression is judged against it, not against baseline</td></tr>
<tr><td class="name">Target lesion</td><td>A measurable lesion chosen at baseline and followed
    throughout. At most five, at most two per organ</td></tr>
<tr><td class="name">Non-target lesion</td><td>Disease that is present but not measured. Assessed
    qualitatively as present, absent, or unequivocally progressing</td></tr>
<tr><td class="name">BOR</td><td>Best overall response: the best assessment recorded from the start
    of treatment until progression</td></tr>
<tr><td class="name">ORR</td><td>Objective response rate: the proportion of patients whose best
    response was CR or PR</td></tr>
<tr><td class="name">DCR</td><td>Disease control rate: the proportion whose best response was CR,
    PR or SD</td></tr>
<tr><td class="name">DoR</td><td>Duration of response: time from first response until progression</td></tr>
<tr><td class="name">NE</td><td>Not evaluable. A patient or visit that cannot be assessed - for
    example no post-baseline scan, or a target lesion that was not re-measured</td></tr>
<tr><td class="name">iUPD</td><td>iRECIST unconfirmed progression: a first progression that may yet
    turn out to be pseudoprogression</td></tr>
<tr><td class="name">iCPD</td><td>iRECIST confirmed progression: progression confirmed on a
    subsequent assessment</td></tr>
<tr><td class="name">Waterfall plot</td><td>One bar per patient showing their best change from
    baseline, sorted from worst to best</td></tr>
<tr><td class="name">Spider plot</td><td>One line per patient showing tumour burden over time</td></tr>
<tr><td class="name">Swimmer plot</td><td>One horizontal bar per patient showing treatment duration
    and events along it</td></tr>
</table>
</div>')
        }
    )
)
