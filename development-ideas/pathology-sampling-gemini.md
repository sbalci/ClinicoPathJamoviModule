
# **A Statistical Framework for Determining the Minimum Number of Samples in Pathological Diagnosis: From Probabilistic Modeling to Clinical Validation**

## **Foundational Principles of Diagnostic Sampling Sufficiency**

The determination of the minimum number of tissue samples necessary for an accurate pathological diagnosis is a critical challenge at the intersection of clinical medicine, laboratory science, and biostatistics. Pathologists and surgeons frequently face questions regarding the optimal sampling depth: How many lymph nodes must be dissected to confidently stage a tumor? How many sections of an omentum are sufficient to rule out microscopic metastasis? How many serial sections of a biopsy are required to ensure a lesion is not missed? These are not merely questions of procedural preference; they are fundamental to establishing diagnostic criteria, defining tumor stages, and guiding patient treatment. An insufficient number of samples risks a false-negative diagnosis, leading to under-staging and under-treatment, while an excessive number can increase patient morbidity, procedural time, and laboratory costs with diminishing diagnostic returns.1  
This report provides a comprehensive statistical framework for addressing these questions. It is essential to distinguish between two related but distinct concepts of "sample size." The first, commonly understood in clinical trials, refers to the number of *patients* required to test a hypothesis about a treatment or intervention. The second, which is the focus of this report, refers to the number of *tissue specimens* or *sections* taken from a single patient to establish a diagnosis. The process of defining a standard for this second type of sampling involves a two-layered statistical approach. The first layer involves creating a probabilistic model to understand the relationship between the number of specimens and the probability of detecting a pathological finding within an individual. The second layer involves designing an inferential study across a population of patients to validate that model and establish a sampling cutoff that is robust, reliable, and clinically justified.

### **Framing the Clinical Question as a Statistical Hypothesis**

To move from a clinical question to a scientifically rigorous answer, the problem must first be framed in the language of statistical hypothesis testing.4 This involves formulating a null hypothesis (), which represents the status quo or a statement of no effect, and an alternative hypothesis (), which represents the effect the researcher aims to demonstrate.

* **Null Hypothesis ():** This hypothesis posits that increasing the number of samples beyond a certain point does not significantly improve the diagnostic detection rate. For example, a study on omental sampling might test the null hypothesis that "the microscopic positivity rate (MPR) from submitting more than two omental blocks is not statistically different from the MPR achieved by submitting one to two blocks".5 In this scenario, the burden of proof is on the data to show that more intensive sampling is necessary.  
* **Alternative Hypothesis ():** This hypothesis posits that an increased number of samples does lead to a meaningful improvement in detection. For instance, a study evaluating a new diagnostic test might hypothesize that "the sensitivity of a protocol using three serial sections is greater than the sensitivity of a protocol using only one section".6

This formalization is the essential first step for any quantitative analysis, as it defines the precise question the study is designed to answer and sets the stage for power analysis and sample size determination.6

### **Statistical Power: The Probability of Not Missing a Lesion**

Statistical power is the probability that a study will correctly reject the null hypothesis when the alternative hypothesis is in fact true.8 In the context of pathology sampling, it is the probability of correctly concluding that a more intensive sampling protocol is superior (i.e., detects more lesions) when it truly is. Power is expressed as , where  is the probability of a Type II error (a false negative). A Type II error would occur if a study failed to detect a real improvement in diagnostic yield from taking more samples, potentially leading to the adoption of a suboptimal, less sensitive sampling standard.  
By convention, studies are designed to have a power of at least 80%, and often 90% in pivotal trials.6 A study with low power that yields a non-significant result is uninformative; it is impossible to distinguish between a true lack of effect and the study's inability to detect one.8 Power is a critical function of three other factors: the sample size of the study (number of patients), the magnitude of the effect being studied (effect size), and the threshold for statistical significance ().

### **Significance Level (α) and Confidence Level**

The significance level, denoted as , is the probability of making a Type I error. A Type I error occurs when the null hypothesis is incorrectly rejected, meaning the study concludes that more samples are better when, in reality, the observed difference in detection rate was due to chance.13 This threshold is typically set at 0.05, which corresponds to a 5% risk of recommending a more burdensome or costly sampling protocol that provides no real diagnostic benefit.6  
The confidence level of a study is calculated as  and is most often 95%. It reflects the level of confidence in the study's findings, often expressed through a 95% confidence interval (CI). A 95% CI provides a range of values within which the true effect (e.g., the true sensitivity of a sampling protocol) is likely to lie.1

### **Effect Size: Quantifying the "Findability" of a Lesion**

The effect size is a measure of the magnitude of the phenomenon under investigation. It is arguably the most important input for a power analysis, yet it can be the most difficult to determine.6 In the context of diagnostic sampling, the effect size can be conceptualized in several ways:

1. **The underlying prevalence of the abnormality per sample:** This could be the proportion of lymph nodes that contain micrometastases in a specific patient population or the probability that any given serial section will contain diagnostic cells.  
2. **The difference in diagnostic performance:** This is the clinically meaningful improvement in a metric like sensitivity that a researcher aims to detect. For example, a study might be designed to detect an increase in sensitivity from 60% with one tissue block to 80% with three tissue blocks.6

Estimating the effect size is a critical prerequisite for designing a validation study. This estimate can be derived from pilot studies, a systematic review of existing literature, or, in the absence of data, from expert consensus on what constitutes a clinically meaningful improvement.6 The entire statistical endeavor of determining a "minimum number" of samples is ultimately an optimization problem. The goal is to identify the point on the diagnostic yield curve where the marginal gain in detection probability from an additional sample is no longer justified by the associated costs, risks, and labor. This point of diminishing returns is not a fixed biological constant but an equilibrium determined by statistical modeling, clinical judgment, and resource constraints. A study on lymph node dissection in uterine cancer perfectly illustrates this principle, finding that while the probability of detecting a positive node increased significantly up to 25 nodes, resecting more than 25 nodes did not offer a further statistically significant benefit.16

## **Probabilistic Models for Lesion Detection in a Sample Set**

At the heart of determining sampling sufficiency is the need for a mathematical model that describes the relationship between the number of samples taken and the probability of detecting a lesion. Each sample taken—be it a biopsy core, a lymph node, or a tissue section—can be viewed as an independent trial with a binary outcome: either the lesion is detected ("success") or it is not ("failure"). Several probabilistic distributions can model this process, each with different assumptions and applications.

### **The Binomial Distribution: The Foundational Model for "Success/Failure" Sampling**

The most fundamental model for this type of problem is the Binomial distribution.17 It describes the probability of observing a certain number of successes in a fixed number of independent trials. Its application in this context rests on four key assumptions 19:

1. **Fixed Number of Trials ():** The number of samples to be taken is predetermined (e.g., examining exactly 5 serial sections).  
2. **Independent Trials:** The outcome of one sample is independent of the others. For example, finding a lesion in the first section does not alter the probability of finding it in the second. This assumption may be violated if lesions are clustered, but it serves as a useful starting point.  
3. **Binary Outcome:** Each sample either contains the diagnostic feature ("success") or it does not ("failure").  
4. **Constant Probability of Success ():** The probability of success, *p*, is the same for each trial. This parameter represents the prevalence of the lesion per sample (e.g., the proportion of all possible sections from a block that would contain the lesion).

For the purpose of establishing a minimum number of samples, the most critical calculation is the probability of achieving *at least one success* in *n* trials. It is more straightforward to calculate the probability of the complementary event—observing zero successes—and subtracting this from 1\. The probability of failure in a single trial is . Since the trials are independent, the probability of *n* consecutive failures is . Therefore, the probability of at least one success is given by the formula 21:  
This formula allows for the construction of a **diagnostic yield curve**, which plots the cumulative probability of detection as a function of the number of samples, *n*. For example, if prior research suggests that the per-section probability of detecting a specific lesion is 0.3 (), the diagnostic yield would be:

* For :  (30% chance of detection)  
* For :  (51% chance of detection)  
* For :  (65.7% chance of detection)  
* For :  (76.0% chance of detection)

This simple example clearly illustrates the principle of diminishing returns: the first section provides a 30% yield, the second adds 21%, the third adds 14.7%, and the fourth adds only 10.3%.

### **The Negative Binomial Distribution: How Many Samples to Find k Positives?**

While the Binomial distribution answers "How many successes in *n* trials?", a different clinical question is often posed, particularly in cancer staging: "How many samples must be examined to find a specific number of positive ones?" For example, staging criteria may depend on finding 1 versus 4 or more positive lymph nodes. The Negative Binomial distribution is the appropriate model for this question.18  
This distribution models the number of trials required to achieve a fixed number of successes, *k*. It is defined by the same probability of success, *p*, as the Binomial model. Its application is crucial for understanding the surgical or procedural burden required to meet specific staging criteria. For instance, if the prevalence of metastatic lymph nodes is estimated to be 10% (), a researcher could use the Negative Binomial distribution to calculate the probability that a surgeon would need to dissect 10, 20, or even 40 nodes to find the 4th positive node required for a specific substage classification. This provides a quantitative basis for surgical guidelines and for counseling patients on the extent of a planned dissection.

### **The Beta-Binomial Distribution: A More Realistic Model for Heterogeneous Tissues**

A significant limitation of the standard Binomial model is its assumption of a constant probability of success, *p*. In pathology, this assumption is often biologically implausible. The prevalence of metastatic cells or dysplastic changes is not uniform across all patients; some individuals have a high disease burden (a high *p*), while others have a very low, sparse burden (a low *p*).24 When data from such a heterogeneous population are pooled, the observed variance in the number of positive samples is often greater than what the Binomial model would predict—a phenomenon known as **overdispersion**.26  
The Beta-Binomial distribution is a more sophisticated model designed to handle such overdispersed data.24 Instead of treating *p* as a fixed number, it models *p* as a random variable that itself follows a Beta distribution. The Beta distribution is a flexible probability distribution defined on the interval , making it ideal for modeling probabilities. By using this two-stage model, the Beta-Binomial distribution accounts for both the random sampling variation within a single patient (the Binomial component) and the patient-to-patient variability in underlying disease prevalence (the Beta component).  
The practical implication of using a Beta-Binomial model is that it often leads to more conservative and realistic estimates of the required number of samples. By accounting for the possibility that a given patient may have a very low lesion prevalence, the model will typically require a larger number of samples to achieve the same level of diagnostic confidence compared to a simple Binomial model that assumes an average prevalence for all patients. This makes it a more robust choice for developing guidelines intended for a diverse clinical population. The choice between a Binomial and a Beta-Binomial model is not merely a statistical nuance; it reflects a fundamental assumption about the underlying biology of the disease being studied—whether it is a homogeneous process or one characterized by "hot spots" and patient-level heterogeneity.  
The following table provides a summary to help researchers select the appropriate model for their specific question.

| Model Name | Core Question It Answers | Key Assumptions | Required Parameters | Example Pathological Application |
| :---- | :---- | :---- | :---- | :---- |
| **Binomial Distribution** | What is the probability of finding *k* positive samples in a fixed set of *n* samples? | Fixed number of trials, independence, binary outcome, constant probability of success (*p*). | *n* (number of samples), *p* (prevalence per sample). | Calculating the probability of detecting at least one focus of dysplasia in 3 serial sections of a biopsy, assuming a uniform risk of dysplasia throughout the tissue. |
| **Negative Binomial Distribution** | How many samples (*n*) must be taken to find a predetermined number of positive samples (*k*)? | Same as Binomial, but the number of trials is the variable of interest. | *k* (target number of positives), *p* (prevalence per sample). | Determining the likely number of lymph nodes a surgeon must dissect to find the 4th positive node needed to confirm N2 staging in breast cancer. |
| **Beta-Binomial Distribution** | What is the probability of finding *k* positive samples in *n* samples, when the underlying prevalence varies between subjects? | Same as Binomial, but the probability of success (*p*) is not constant; it is a random variable following a Beta distribution. | *n* (number of samples), *α* and *β* (parameters of the Beta distribution that define the mean and variance of *p*). | Modeling the detection of omental metastases across a population of ovarian cancer patients, where some have extensive seeding (high *p*) and others have only microscopic, sparse disease (low *p*). |

## **Establishing and Validating Diagnostic Cut-offs**

Once a theoretical understanding of the sampling process is established through probabilistic modeling, the next step is to design and execute a clinical study to validate a proposed sampling cut-off. This involves moving from theoretical probabilities to empirical evidence gathered from a cohort of patients. The goal of such a study is to quantify the diagnostic accuracy of a specific sampling protocol (e.g., "take 3 biopsies") and to provide the statistical evidence needed to establish it as a clinical standard.

### **Designing a Diagnostic Accuracy Study**

A typical study to validate a sampling protocol is a prospective or retrospective cohort study. The fundamental design involves 31:

1. **Defining the Cohort:** A group of patients with suspected disease is identified.  
2. **Applying the Sampling Protocol:** The sampling procedure is performed, and each specimen is analyzed and recorded individually. For example, in a study of serial sections, each of the, say, 10 sections is evaluated separately.  
3. **Establishing a Gold Standard:** A definitive "gold standard" diagnosis is obtained for every patient to determine their true disease status. This is the most critical and often most challenging aspect of the study design. The gold standard could be the complete pathologic examination of a subsequent resection specimen, long-term clinical follow-up, or results from a more sensitive diagnostic modality.7 Without an accurate gold standard, it is impossible to correctly classify the results of the sampling protocol as true positive, false positive, true negative, or false negative.  
4. **Evaluating Performance:** The results from various sampling depths (e.g., diagnosis based on section 1 alone, sections 1-2, sections 1-3, etc.) are compared against the gold standard to calculate performance metrics.

### **Key Metrics: Sensitivity, Specificity, and Predictive Values**

The performance of a diagnostic sampling protocol is quantified using standard metrics of diagnostic accuracy, adapted for this context 14:

* Sensitivity (True Positive Rate): This is the probability that the sampling protocol (e.g., examining a set of 3 sections) will correctly identify the disease in a patient who truly has the disease. It is the single most important metric for a test designed to rule out disease. It is calculated as:

  where TP is the number of true positives and FN is the number of false negatives.  
* Specificity (True Negative Rate): This is the probability that the sampling protocol will correctly yield a negative result for a patient who is truly disease-free. It is calculated as:

  where TN is the number of true negatives and FP is the number of false positives.  
* **Positive and Negative Predictive Values (PPV and NPV):** These metrics provide a more direct answer to the clinical question: "Given this test result, what is the probability my patient has the disease?" PPV is the probability that a patient with a positive result from the sampling protocol truly has the disease. NPV is the probability that a patient with a negative result is truly disease-free. While clinically intuitive, PPV and NPV are highly dependent on the prevalence of the disease in the population being tested, unlike sensitivity and specificity which are considered intrinsic properties of the test.14

### **Receiver Operating Characteristic (ROC) Curve Analysis**

Receiver Operating Characteristic (ROC) curve analysis is a powerful graphical method for evaluating the performance of a diagnostic test across a range of thresholds and for identifying an optimal cut-off.35 While typically used for tests with a continuous output (e.g., a biomarker level), the method can be adapted to evaluate the number of samples, which is a discrete variable.38  
In this adaptation, each point on the ROC curve represents a different sampling intensity (e.g., 1 section, 2 sections, 3 sections, etc.). For each intensity level, the corresponding sensitivity is plotted on the y-axis against the false-positive rate (1 \- Specificity) on the x-axis.

* **The Area Under the Curve (AUC):** The AUC is a global measure of the diagnostic performance of the sampling strategy. It represents the probability that the test will correctly rank a randomly chosen diseased individual higher than a randomly chosen non-diseased individual. An AUC of 1.0 represents a perfect test, while an AUC of 0.5 indicates the test performs no better than random chance.35  
* **Identifying the Optimal Cut-off:** The "optimal" number of samples is the point on the ROC curve that represents the best trade-off between sensitivity and specificity. Common methods for identifying this point include 37:  
  * **Youden's Index:** Finding the point that maximizes the value of (Sensitivity \+ Specificity \- 1). This gives equal weight to both sensitivity and specificity.  
  * **Closest to (0,1) Corner:** Finding the point on the curve with the minimum geometric distance to the top-left corner of the plot, which represents perfect classification (100% sensitivity, 100% specificity).

### **Sample Size Calculation for the Validation Study**

Before embarking on a validation study, it is imperative to calculate the number of *patients* required to ensure the study has adequate statistical power to produce reliable estimates of sensitivity and specificity. An underpowered study may yield wide, uninformative confidence intervals for its performance metrics, rendering the results inconclusive.14  
Several formulas exist for this purpose. A common approach, described by Buderer (1996), calculates the number of patients needed for sensitivity () and specificity () separately, and the final required sample size is the larger of the two.33  
The formula for the number of diseased patients needed to estimate sensitivity with a desired precision is:

The total number of patients needed for the sensitivity calculation is then N1​=⌈pn1​​⌉, where p is the prevalence of the disease.  
The formula for the number of non-diseased patients needed to estimate specificity is:

The total number of patients needed for the specificity calculation is then N2​=⌈1−pn2​​⌉.  
The final sample size for the study is the maximum of  and , often increased to account for potential patient dropouts.  
**Where:**

* is the z-score corresponding to the desired confidence level (e.g., 1.96 for 95% confidence).  
* is the anticipated sensitivity of the sampling protocol.  
* is the anticipated specificity.  
* is the desired margin of error, or the half-width of the confidence interval (e.g., 0.05 for a precision of 5%).  
* is the estimated prevalence of the disease in the study population.

The following table serves as a practical checklist for researchers planning such a study.

| Parameter | Definition | How to Estimate/Source | Example Value |
| :---- | :---- | :---- | :---- |
| **Estimated Prevalence ()** | The proportion of patients in the target population who truly have the disease or lesion of interest. | Previous institutional data, epidemiological studies, published literature. | 0.20 (20%) |
| **Target Sensitivity ()** | The minimum acceptable sensitivity for the sampling protocol to be considered clinically useful. | Based on clinical judgment of the consequences of a false negative. For cancer detection, this is often set high. | 0.95 (95%) |
| **Target Specificity ()** | The minimum acceptable specificity. Set based on the consequences of a false positive (e.g., unnecessary further procedures). | Clinical judgment, existing data on similar tests. | 0.90 (90%) |
| **Desired Precision ()** | The acceptable margin of error for the sensitivity and specificity estimates (half-width of the confidence interval). | A common choice is 0.05 to 0.10. A smaller *w* requires a larger sample size. | 0.05 (5%) |
| **Confidence Level** | The desired level of confidence that the true sensitivity/specificity lies within the calculated confidence interval. | Conventionally set at 95%, corresponding to a z-score of 1.96. | 95% () |
| **Statistical Power ()** | The probability of detecting a true difference if one exists (used for comparative studies). | Conventionally set at 80% or 90%. | 80% |
| **Expected Dropout Rate** | The percentage of enrolled patients who may be lost to follow-up or have unusable data. | Based on experience from similar studies at the institution. | 10% |

## **Advanced Statistical Modeling and Application-Specific Methodologies**

While diagnostic accuracy metrics and ROC curves provide a robust framework for validating a cut-off, more advanced statistical models can offer deeper insights into the relationship between sampling depth and diagnostic yield. These models can more precisely identify the point of diminishing returns and link sampling intensity not just to diagnosis, but to long-term patient outcomes.

### **Logistic Regression: Modeling Diagnostic Probability and Diminishing Returns**

Logistic regression is a powerful technique for modeling a binary outcome (e.g., positive vs. negative diagnosis) as a function of one or more predictor variables.4 In this context, the model can be formulated as:  
The model estimates the coefficients ( and ) that best describe the relationship between the number of samples taken and the log-odds of obtaining a positive diagnosis. The output can be converted into probabilities and odds ratios (ORs), which quantify the increase in the odds of detection for each additional sample or for moving from one category of sampling intensity to another. This approach is particularly well-suited to identifying a plateau in diagnostic yield.  
Case Study 1: Lymph Node Dissection in Uterine Cancer  
A landmark study on endometrioid uterine cancer provides a clear example of this method's utility.16 Researchers used a logistic regression model to analyze data from over 11,000 patients, examining the relationship between the number of lymph nodes resected (categorized into groups) and the probability of detecting at least one positive node. The model revealed that the odds of detecting a positive node increased significantly as the number of resected nodes rose from 1 to 25\. The largest increase was observed when moving into the 21-25 node category (Odds Ratio \= 1.45; 95% CI, 1.08-1.94). Crucially, resecting more than 25 lymph nodes did not provide a further statistically significant improvement in the probability of detection (Odds Ratio \= 1.23; p \= 0.13). This analysis provided strong statistical evidence to define 21-25 nodes as the target for an "adequate" lymphadenectomy, effectively identifying the point of diminishing returns.  
Case Study 2: Omental Sampling for Metastasis  
In a study of neoplasms involving the female genital tract, researchers investigated the optimal number of omental blocks needed to detect metastasis.5 They compared the microscopic positivity rate (MPR) across different levels of sampling (1-2 blocks, 3-4 blocks, etc.). Using statistical methods including the Cochran-Armitage test for trend and binomial logistic regression, they found no statistically significant linear trend between an increasing number of blocks and the MPR. The MPR for the 1-2 block group was not significantly lower than that for groups with higher levels of sampling. This demonstrates how logistic regression can be used not only to justify more sampling but also to provide evidence for less intensive sampling, thereby optimizing laboratory workflow and reducing costs without compromising diagnostic accuracy.

### **Survival Analysis: Linking Sampling Depth to Patient Outcomes**

Establishing that a certain number of samples is sufficient for diagnosis is one goal; demonstrating that this number has prognostic significance is another, more powerful justification. Survival analysis methods can be used to determine if the number of samples taken (or the findings from those samples) correlates with patient outcomes like overall survival or disease-free survival.

* **Kaplan-Meier Curves and the Log-Rank Test:** This is the most common method for comparing survival distributions between two or more groups.41 Patients can be stratified into groups based on a sampling cut-off (e.g., \<15 lymph nodes harvested vs. 15 lymph nodes harvested). The Kaplan-Meier method is used to generate survival curves for each group, and the log-rank test is applied to determine if the difference between the curves is statistically significant.44 A significant result suggests that the sampling depth is a meaningful prognostic factor.  
* **Maximally Selected Rank Statistics:** A challenge with survival analysis is choosing the cut-off for a continuous variable like the number of nodes. Testing multiple arbitrary cut-offs increases the risk of finding a spurious association. Maximally selected rank statistics is a data-driven method that identifies the cut-off point that yields the most significant separation between the survival curves.41 This provides an unbiased way to determine the most prognostically relevant threshold. However, it is critical to validate this data-driven cut-off against clinical plausibility or in an independent dataset to avoid overfitting, where the chosen cut-off is an artifact of the specific dataset and not generalizable.41

### **Bayesian Approaches: Incorporating Prior Knowledge into Study Design**

The statistical methods discussed thus far belong to the frequentist school of thought. An alternative paradigm, the Bayesian approach, is gaining traction in medical research for its intuitive framework and potential for more efficient study designs.31  
The core of Bayesian statistics is the use of prior information. Instead of starting from a position of ignorance, a Bayesian analysis begins with a "prior probability distribution" that represents existing knowledge about a parameter (e.g., from previous studies or expert opinion). This prior is then updated with the data collected in the current study to generate a "posterior probability distribution," which represents our updated state of knowledge.48  
In the context of sample size determination, Bayesian methods offer several advantages:

* **Assurance:** Instead of calculating power based on a single, fixed estimate of the effect size, a Bayesian approach can calculate "assurance" (or "expected power"). This is the average power calculated over the entire prior distribution of the effect size, providing a more realistic estimate of the probability of study success by formally accounting for uncertainty in the initial assumptions.50  
* **Precision-Based Sample Size:** Bayesian methods can directly calculate the sample size needed to achieve a desired level of precision for a parameter, such as ensuring that the 95% posterior credible interval for sensitivity is no wider than a specified amount (e.g., 10 percentage points). This directly links sample size to the post-study certainty of the result.32  
* **Incorporating Existing Data:** Bayesian methods provide a formal mathematical framework for incorporating data from earlier, smaller studies, which can sometimes reduce the required sample size for a new validation study.32

The following table summarizes how these advanced methods have been applied to answer specific clinical sampling questions.

| Clinical Problem | Tissue Type | Statistical Method(s) Used | Key Finding / Cut-off | Source(s) |
| :---- | :---- | :---- | :---- | :---- |
| Determining adequate lymphadenectomy for staging. | Lymph Nodes (Uterine Cancer) | Logistic Regression | Resecting 21-25 nodes significantly increases the odds of finding a positive node; \>25 nodes does not add further statistical benefit. | 16 |
| Justifying a minimal sampling protocol. | Omentum (Gynecologic Cancer) | Cochran-Armitage Test for Trend, Logistic Regression | Submitting 1-2 blocks results in a microscopic positivity rate that is not statistically lower than higher levels of sampling. | 5 |
| Finding a prognostically significant cut-off for a continuous sampling variable. | Lymph Nodes (Head and Neck Cancer) | Maximally Selected Rank Statistics, Kaplan-Meier Analysis, Log-Rank Test | A lymph node yield of at least 15 nodes was identified as the optimal cut-off associated with significantly better overall survival. | 41 |
| Designing a diagnostic accuracy study with high precision. | General Biopsy | Bayesian Credible Interval Approach | Calculates the number of patients needed to ensure the final estimate of sensitivity/specificity has a desired level of precision (e.g., a 95% credible interval width of 5%). | 32 |

## **A Practical Workflow for Designing and Executing a Sampling Study**

Synthesizing the principles and methods discussed, a researcher can follow a structured workflow to determine and validate a minimum number of samples for a given diagnostic purpose. This process is centered on the development and empirical validation of a **Diagnostic Yield Curve**.

### **Step 1: Define the Clinical and Statistical Objective**

The first step is to articulate a precise and answerable question. This involves specifying the tissue type, the target lesion or pathological finding, and the desired level of diagnostic performance. This question must then be translated into a formal statistical hypothesis.6

* **Example Objective:** "To determine the minimum number of endoscopic biopsy specimens required to achieve a diagnostic sensitivity of at least 98% for advanced gastric cancer."  
* **Null Hypothesis ():** The sensitivity of taking 3 biopsies is not significantly different from the sensitivity of taking 6 biopsies.  
* **Alternative Hypothesis ():** The sensitivity of taking 6 biopsies is significantly greater than that of taking 3 biopsies.

### **Step 2: Estimate Key Parameters from Pilot Data or Literature**

A robust study design requires plausible estimates for several key parameters. This step involves a thorough review of existing literature, analysis of preliminary institutional data, or conducting a small pilot study.6 Key parameters to estimate include:

* The per-sample prevalence of the lesion ().  
* The overall prevalence of the disease in the target population.  
* The expected variability in prevalence between patients (for a Beta-Binomial model).  
* A clinically meaningful effect size (e.g., the minimum improvement in sensitivity that would justify a change in practice).

### **Step 3: Model the Theoretical Diagnostic Yield Curve**

Using the estimated per-sample prevalence () and an appropriate probabilistic model from Section 2, plot the theoretical diagnostic yield against the number of samples taken (n=1, 2, 3,...). The Binomial probability formula, , is often sufficient for this initial modeling. This curve will provide a visual representation of the expected diminishing returns and help identify a plausible range for the optimal cut-off.21 This curve illustrates the theoretical maximum yield and at what point additional sampling provides only marginal gains.

### **Step 4: Identify a Candidate Cut-off and Design the Validation Study**

Based on the theoretical yield curve and clinical considerations, select a candidate "minimum number" of samples. This is often the "knee" of the curve, where the rate of increase in yield begins to flatten. Then, design a formal diagnostic accuracy study to validate this cut-off.

* **Set Performance Goals:** Define the target sensitivity and specificity for the protocol using the candidate number of samples.  
* **Calculate Patient Sample Size:** Use the formulas and principles from Section 3.4 to calculate the number of patients (both diseased and non-diseased) required to validate the performance goals with adequate statistical power and precision.9

### **Step 5: Execute the Study and Analyze the Results**

With a robust study design in place, the data can be collected. It is crucial that the gold standard diagnosis is applied rigorously and that pathologists evaluating the samples are blinded to other clinical information where appropriate.

* **Empirical Analysis:** Plot an empirical diagnostic yield curve based on the actual data collected.  
* **ROC Analysis:** Use ROC analysis as described in Section 3.3 to empirically determine the optimal cut-off that best balances sensitivity and specificity in the study cohort.35  
* **Advanced Modeling:** Apply logistic regression to model the probability of detection as a function of the number of samples and to calculate odds ratios for the incremental value of each additional sample.16 If long-term follow-up data are available, use survival analysis to assess the prognostic significance of the chosen cut-off.41

### **Step 6: Incorporate Cost-Effectiveness Analysis**

The final step in justifying a sampling guideline is to demonstrate that it is not only diagnostically effective but also cost-effective. The concept of diminishing returns is fundamentally an economic one, balancing the cost of a resource against the benefit it provides.3  
A formal cost-effectiveness analysis can be conducted by calculating the **Incremental Cost-Effectiveness Ratio (ICER)** for each additional sample.55 The ICER is calculated as the change in cost divided by the change in effectiveness:  
This ratio quantifies the additional cost required to gain one additional percentage point of diagnostic yield. For example, a study on advanced gastrointestinal cancer found that the cumulative diagnostic rate for gastric cancer increased from 81.3% with one biopsy to 94.9% with two, and 98.3% with three. The fourth and fifth biopsies provided no further increase in yield.57 While the first and second additional biopsies provide substantial gains in yield for their cost, the ICER for the fourth biopsy would be infinite (cost increases but yield does not), providing a clear economic justification for recommending 3 or 4 biopsies as the standard. The recommended "minimum number" is therefore the point after which the ICER exceeds a predefined willingness-to-pay threshold.

### **Conclusion**

The determination of a minimum number of samples for pathological diagnosis is a complex process that requires a synthesis of clinical knowledge, probabilistic theory, and rigorous empirical validation. It is not a search for a single, universal number, but rather the application of a universal statistical *process* to a specific clinical context. The optimal number is dependent on the prevalence of the disease, the spatial distribution of the lesion within the tissue, the diagnostic accuracy of the test itself, and the clinical and economic consequences of both false-negative and false-positive results.  
By framing the clinical question as a testable hypothesis, using appropriate probabilistic models like the Binomial or Beta-Binomial distributions to understand the theoretical yield, and designing adequately powered diagnostic accuracy studies, researchers can establish evidence-based sampling guidelines. Advanced methods such as logistic regression and survival analysis can further refine these guidelines by identifying the point of diminishing returns and confirming the prognostic relevance of the chosen cut-off. Ultimately, this structured approach ensures that pathological sampling protocols are optimized to maximize diagnostic accuracy while responsibly managing resources and minimizing patient burden, forming a cornerstone of evidence-based practice in pathology.

#### **Works cited**

1. Sample size determination \- Wikipedia, accessed October 4, 2025, [https://en.wikipedia.org/wiki/Sample\_size\_determination](https://en.wikipedia.org/wiki/Sample_size_determination)  
2. How to Determine Sample Size \- Qualtrics, accessed October 4, 2025, [https://www.qualtrics.com/experience-management/research/determine-sample-size/](https://www.qualtrics.com/experience-management/research/determine-sample-size/)  
3. Diminishing returns on the road to diagnostic certainty \- PubMed, accessed October 4, 2025, [https://pubmed.ncbi.nlm.nih.gov/1901611/](https://pubmed.ncbi.nlm.nih.gov/1901611/)  
4. Introduction to Statistical Methods in Pathology | Request PDF \- ResearchGate, accessed October 4, 2025, [https://www.researchgate.net/publication/321680836\_Introduction\_to\_Statistical\_Methods\_in\_Pathology](https://www.researchgate.net/publication/321680836_Introduction_to_Statistical_Methods_in_Pathology)  
5. Pathologic sampling of the omentum for neoplasms that involve the ..., accessed October 4, 2025, [https://www.researchgate.net/publication/394467672\_Pathologic\_sampling\_of\_the\_omentum\_for\_neoplasms\_that\_involve\_the\_female\_genital\_tract\_A\_retrospective\_analysis\_of\_1055\_cases](https://www.researchgate.net/publication/394467672_Pathologic_sampling_of_the_omentum_for_neoplasms_that_involve_the_female_genital_tract_A_retrospective_analysis_of_1055_cases)  
6. Guidance on Conducting Sample Size and Power Calculations \- Preventive Medicine, accessed October 4, 2025, [https://www.preventivemedicine.northwestern.edu/docs/applied-statistics-presentation-materials/sample-size-and-power-presentation.pdf](https://www.preventivemedicine.northwestern.edu/docs/applied-statistics-presentation-materials/sample-size-and-power-presentation.pdf)  
7. Statistical Matching Analysis for Correlating Pores Detected with XCT and Serial Sectioning, accessed October 4, 2025, [https://www.researchgate.net/publication/375442274\_Statistical\_Matching\_Analysis\_for\_Correlating\_Pores\_Detected\_with\_XCT\_and\_Serial\_Sectioning](https://www.researchgate.net/publication/375442274_Statistical_Matching_Analysis_for_Correlating_Pores_Detected_with_XCT_and_Serial_Sectioning)  
8. Statistical Power in Plant Pathology Research \- APS Journals, accessed October 4, 2025, [https://apsjournals.apsnet.org/doi/10.1094/PHYTO-03-17-0098-LE](https://apsjournals.apsnet.org/doi/10.1094/PHYTO-03-17-0098-LE)  
9. How To Use Power Analysis To Determine The Appropriate Sample Size Of A Study, accessed October 4, 2025, [https://www.graphpad.com/support/faq/how-to-use-power-analysis-to-determine-the-appropriate-sample-size-of-a-study/](https://www.graphpad.com/support/faq/how-to-use-power-analysis-to-determine-the-appropriate-sample-size-of-a-study/)  
10. How to Use Power Analysis | Amplitude, accessed October 4, 2025, [https://amplitude.com/explore/experiment/power-analysis](https://amplitude.com/explore/experiment/power-analysis)  
11. How to Calculate Sample Size for Animal Studies \- Anilocus, accessed October 4, 2025, [https://anilocus.com/how-to-calculate-sample-size-for-animal-studies/](https://anilocus.com/how-to-calculate-sample-size-for-animal-studies/)  
12. Statistical Power in Plant Pathology Research \- APS Journals, accessed October 4, 2025, [https://apsjournals.apsnet.org/doi/pdf/10.1094/PHYTO-03-17-0098-LE](https://apsjournals.apsnet.org/doi/pdf/10.1094/PHYTO-03-17-0098-LE)  
13. Sample size calculation in medical studies \- PMC, accessed October 4, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC4017493/](https://pmc.ncbi.nlm.nih.gov/articles/PMC4017493/)  
14. User's guide to sample size estimation in diagnostic accuracy studies \- PMC, accessed October 4, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC9639742/](https://pmc.ncbi.nlm.nih.gov/articles/PMC9639742/)  
15. How to Write the Sample Size Justification Section in Your Clinical Protocol \- Diogo Ribeiro, accessed October 4, 2025, [https://diogoribeiro7.github.io/clinical%20research/biostatistics/sample\_size\_clinical/](https://diogoribeiro7.github.io/clinical%20research/biostatistics/sample_size_clinical/)  
16. Lymphadenectomy in endometrioid uterine cancer staging: how ..., accessed October 4, 2025, [https://pubmed.ncbi.nlm.nih.gov/17503431/](https://pubmed.ncbi.nlm.nih.gov/17503431/)  
17. Binomial Sampling and the Binomial Distribution, accessed October 4, 2025, [https://sites.warnercnr.colostate.edu/wp-content/uploads/sites/73/2017/04/BinomialDistribution.pdf](https://sites.warnercnr.colostate.edu/wp-content/uploads/sites/73/2017/04/BinomialDistribution.pdf)  
18. Binomial Distribution: A Complete Guide with Examples | DataCamp, accessed October 4, 2025, [https://www.datacamp.com/tutorial/binomial-distribution](https://www.datacamp.com/tutorial/binomial-distribution)  
19. The Binomial Distribution, accessed October 4, 2025, [http://www.stat.yale.edu/Courses/1997-98/101/binom.htm](http://www.stat.yale.edu/Courses/1997-98/101/binom.htm)  
20. The Binomial Test \- Technology Networks, accessed October 4, 2025, [https://www.technologynetworks.com/informatics/articles/the-binomial-test-366022](https://www.technologynetworks.com/informatics/articles/the-binomial-test-366022)  
21. Risk-Benefit Analysis of Sampling Methods for Fine-Needle ..., accessed October 4, 2025, [https://academic.oup.com/ajcp/article/139/3/336/1766176](https://academic.oup.com/ajcp/article/139/3/336/1766176)  
22. Probability of a certain number of people having a disease after a ..., accessed October 4, 2025, [https://math.stackexchange.com/questions/4787262/probability-of-a-certain-number-of-people-having-a-disease-after-a-pooled-test-c](https://math.stackexchange.com/questions/4787262/probability-of-a-certain-number-of-people-having-a-disease-after-a-pooled-test-c)  
23. probability \- grouping blood tests (binomial distribution ..., accessed October 4, 2025, [https://math.stackexchange.com/questions/1967808/grouping-blood-tests-binomial-distribution](https://math.stackexchange.com/questions/1967808/grouping-blood-tests-binomial-distribution)  
24. Advanced Discussion and Illustration: Beta-binomial Analysis, accessed October 4, 2025, [https://www.apsnet.org/edcenter/sites/EcologyAndEpidemiologyInR/SpatialAnalysis/Pages/AdvancedDiscussionAndIllustration.aspx](https://www.apsnet.org/edcenter/sites/EcologyAndEpidemiologyInR/SpatialAnalysis/Pages/AdvancedDiscussionAndIllustration.aspx)  
25. Using the Beta-Binomial Distribution to Describe Aggregated Patterns of Disease Incidence, accessed October 4, 2025, [https://apsjournals.apsnet.org/doi/10.1094/Phyto-83-759](https://apsjournals.apsnet.org/doi/10.1094/Phyto-83-759)  
26. Using the Beta-Binomial Distribution to Describe ... \- APS Journals, accessed October 4, 2025, [https://apsjournals.apsnet.org/doi/pdf/10.1094/Phyto-83-759](https://apsjournals.apsnet.org/doi/pdf/10.1094/Phyto-83-759)  
27. On the beta-binomial model for analysis of spectral count data in label-free tandem mass spectrometry-based proteomics | Bioinformatics | Oxford Academic, accessed October 4, 2025, [https://academic.oup.com/bioinformatics/article/26/3/363/215277](https://academic.oup.com/bioinformatics/article/26/3/363/215277)  
28. Model to improve specificity for identification of clinically-relevant expanded T cells in peripheral blood \- PubMed, accessed October 4, 2025, [https://pubmed.ncbi.nlm.nih.gov/30870493/](https://pubmed.ncbi.nlm.nih.gov/30870493/)  
29. Beta-binomial distribution \- Wikipedia, accessed October 4, 2025, [https://en.wikipedia.org/wiki/Beta-binomial\_distribution](https://en.wikipedia.org/wiki/Beta-binomial_distribution)  
30. An introduction to the Beta-Binomial model \- Computational Cognitive Science, accessed October 4, 2025, [https://compcogsci-3016.djnavarro.net/technote\_betabinomial.pdf](https://compcogsci-3016.djnavarro.net/technote_betabinomial.pdf)  
31. (PDF) Sample size calculations for studies designed to evaluate diagnostic test accuracy, accessed October 4, 2025, [https://www.researchgate.net/publication/226990339\_Sample\_size\_calculations\_for\_studies\_designed\_to\_evaluate\_diagnostic\_test\_accuracy](https://www.researchgate.net/publication/226990339_Sample_size_calculations_for_studies_designed_to_evaluate_diagnostic_test_accuracy)  
32. Sample size calculations for diagnostic studies | Request PDF \- ResearchGate, accessed October 4, 2025, [https://www.researchgate.net/publication/332392216\_Sample\_size\_calculations\_for\_diagnostic\_studies](https://www.researchgate.net/publication/332392216_Sample_size_calculations_for_diagnostic_studies)  
33. Diagnostic Accuracy Sample Size Calculator \- Resub \- AI Writing ..., accessed October 4, 2025, [https://resub.app/da-sample-size](https://resub.app/da-sample-size)  
34. Diagnostic Testing Accuracy: Sensitivity, Specificity, Predictive Values and Likelihood Ratios, accessed October 4, 2025, [https://www.ncbi.nlm.nih.gov/books/NBK557491/](https://www.ncbi.nlm.nih.gov/books/NBK557491/)  
35. ROC curve analysis (AUC, Sensitivity, Specificity, etc.), accessed October 4, 2025, [https://www.medcalc.org/en/manual/roc-curves.php](https://www.medcalc.org/en/manual/roc-curves.php)  
36. Statistic Definitions \- Basic Science \- Orthobullets, accessed October 4, 2025, [https://www.orthobullets.com/basic-science/9073/statistic-definitions](https://www.orthobullets.com/basic-science/9073/statistic-definitions)  
37. On determining the most appropriate test cut-off value: the case of tests with continuous results \- PMC, accessed October 4, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC5082211/](https://pmc.ncbi.nlm.nih.gov/articles/PMC5082211/)  
38. ROC Analysis | AJR \- American Journal of Roentgenology, accessed October 4, 2025, [https://ajronline.org/doi/full/10.2214/ajr.184.2.01840364](https://ajronline.org/doi/full/10.2214/ajr.184.2.01840364)  
39. William Turechek, Research Plant Pathologist \- USDA ARS, accessed October 4, 2025, [https://www.ars.usda.gov/ARSUserFiles/40663/Documents/Turechek\_ROC\_WebPageMaterial.pdf](https://www.ars.usda.gov/ARSUserFiles/40663/Documents/Turechek_ROC_WebPageMaterial.pdf)  
40. On determining the most appropriate test cut-off value: the case of tests with continuous results \- Biochemia Medica, accessed October 4, 2025, [https://www.biochemia-medica.com/en/journal/26/3/10.11613/BM.2016.034/fullArticle](https://www.biochemia-medica.com/en/journal/26/3/10.11613/BM.2016.034/fullArticle)  
41. Survival analysis in hematologic malignancies: recommendations ..., accessed October 4, 2025, [https://www.haematologica.org/article/view/7133](https://www.haematologica.org/article/view/7133)  
42. Multivariate Statistical Analysis for Pathologists, accessed October 4, 2025, [https://academic.oup.com/ajcp/article-pdf/105/1/115/24878050/ajcpath105-0115.pdf](https://academic.oup.com/ajcp/article-pdf/105/1/115/24878050/ajcpath105-0115.pdf)  
43. Survival analysis in hematologic malignancies: recommendations for clinicians \- Haematologica, accessed October 4, 2025, [https://haematologica.org/article/view/7133/43013](https://haematologica.org/article/view/7133/43013)  
44. Impact of Lymph Node Yield on Outcome of Patients with Head and Neck Cancer and pN0 Neck | Anticancer Research, accessed October 4, 2025, [https://ar.iiarjournals.org/content/38/9/5347](https://ar.iiarjournals.org/content/38/9/5347)  
45. Cancer-Specific Survival Outcome in Early-Stage Young Breast Cancer: Evidence From the SEER Database Analysis \- Frontiers, accessed October 4, 2025, [https://www.frontiersin.org/journals/endocrinology/articles/10.3389/fendo.2021.811878/full](https://www.frontiersin.org/journals/endocrinology/articles/10.3389/fendo.2021.811878/full)  
46. Bayesian Clinical Trial Design Software \- nQuery, accessed October 4, 2025, [https://www.statsols.com/nquery/bayes](https://www.statsols.com/nquery/bayes)  
47. Bayesian approach for sample size estimation and re-adjustment in clinical trials, accessed October 4, 2025, [https://anatomisebiostats.com/biostatistics-blog/bayesian-sample-size-estimation-in-clinical-study-design-rtcs/](https://anatomisebiostats.com/biostatistics-blog/bayesian-sample-size-estimation-in-clinical-study-design-rtcs/)  
48. Practical approaches to Bayesian sample size determination in non‐inferiority trials with binary outcomes \- PMC, accessed October 4, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC7615731/](https://pmc.ncbi.nlm.nih.gov/articles/PMC7615731/)  
49. A Bayesian method of sample size determination with practical applications \- University of Southampton, accessed October 4, 2025, [https://www.southampton.ac.uk/\~sks/research/papers/SahuSmith06.pdf](https://www.southampton.ac.uk/~sks/research/papers/SahuSmith06.pdf)  
50. A systematic review of sample size determination in Bayesian randomized clinical trials: full Bayesian methods are rarely used \- arXiv, accessed October 4, 2025, [https://arxiv.org/pdf/2505.15735](https://arxiv.org/pdf/2505.15735)  
51. Bayesian sample size determination for Normal means and differences between Normal means \- Faculty of Medicine and Health Sciences \- McGill University, accessed October 4, 2025, [https://www.medicine.mcgill.ca/epidemiology/Joseph/publications/Methodological/normsize.pdf](https://www.medicine.mcgill.ca/epidemiology/Joseph/publications/Methodological/normsize.pdf)  
52. A Bayesian approach to sample size determination for studies designed to evaluate continuous medical tests | Request PDF \- ResearchGate, accessed October 4, 2025, [https://www.researchgate.net/publication/46494142\_A\_Bayesian\_approach\_to\_sample\_size\_determination\_for\_studies\_designed\_to\_evaluate\_continuous\_medical\_tests](https://www.researchgate.net/publication/46494142_A_Bayesian_approach_to_sample_size_determination_for_studies_designed_to_evaluate_continuous_medical_tests)  
53. 18 Statistical models \- R4PDE.net, accessed October 4, 2025, [https://r4pde.net/yieldloss-regression-models](https://r4pde.net/yieldloss-regression-models)  
54. Cost-Effectiveness Analysis in Pathology \- ResearchGate, accessed October 4, 2025, [https://www.researchgate.net/publication/12727955\_Cost-Effectiveness\_Analysis\_in\_Pathology](https://www.researchgate.net/publication/12727955_Cost-Effectiveness_Analysis_in_Pathology)  
55. Cost-effectiveness analysis of whole-mount pathology processing for patients with early breast cancer undergoing breast conservation, accessed October 4, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC4780586/](https://pmc.ncbi.nlm.nih.gov/articles/PMC4780586/)  
56. Cost Effectiveness Analysis for Diagnostic Testing \- ARUP Laboratories, accessed October 4, 2025, [https://arup.utah.edu/media/costEffectiveEval/Cost%20Effectiveness%20Analysis.pdf](https://arup.utah.edu/media/costEffectiveEval/Cost%20Effectiveness%20Analysis.pdf)  
57. Optimal Number of Endoscopic Biopsies in Diagnosis of Advanced Gastric and Colorectal Cancer \- PMC, accessed October 4, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC3247772/](https://pmc.ncbi.nlm.nih.gov/articles/PMC3247772/)
