Data Preparation (Age Groups & Raking)
===

Data Preparation provides two diagnostic utilities prior to norm estimation with `cNORM`:

1. **Norm Group Formation (`getGroups`):** Partitions a continuous explanatory variable (such as chronological age) into discrete cohorts and evaluates subsample sizes across intervals. Inspecting cohort sizes helps determine an appropriate grouping granularity for distribution-free Taylor norming.
2. **Post-Stratification Raking (`computeWeights`):** Computes compensatory survey weights via Iterative Proportional Fitting (IPF / raking; Deming & Stephan, 1940) so that the marginal distributions of demographic stratification variables (e.g., sex, parental education, region, migration background) match known target population census proportions (Gary et al., 2023a, 2023b).

Both sections can be used independently or simultaneously.

---

### Methodological Recommendations

* **Norm Group Sizes:** For distribution-free continuous norming with Taylor polynomials, individual age groups ideally contain *N* ≥ 100 observations (and should generally not fall below *N* = 50) so that initial within-group Rankit percentiles are estimated reliably.
* **Raking & Non-Representativeness:** Post-stratification raking effectively reduces norm score bias under moderate deviations from census marginals when stratification variables correlate with the measured trait. However, very small cell counts or extreme target-to-sample discrepancies inflate weight variance, increasing the **Design Effect (DEFF)** and reducing the **Effective Sample Size (N<sub>eff</sub>)** (Gary et al., 2023a).
* **Inspecting Weight Extremes:** As a rule of thumb, inspect *Max Weight / Min Weight* ratios and ensure that *Design Effect* remains modest (e.g., DEFF < 1.5–2.0). If weights become overly extreme, consider collapsing sparse demographic categories.

---

### Input

#### Assignment Box
* **Continuous Explanatory Variable (e.g., Age):** Continuous or ordinal variable to divide into norm groups (requires at least 6 valid observations).
* **Stratification Variables (Raking):** One or more categorical (*nominal* or *ordinal*) demographic variables used for marginal distribution tables and raking weight calculation (only cases with complete data across all selected stratification variables are included).

#### Norm Groups (getGroups)
* **Number of norm groups (0 = auto):** Desired number of cohorts. When set to `0`, `cNORM::getGroups()` automatically determines a suitable group count based on sample size.
* **Equidistant intervals (instead of equal sizes):**
  * *Unchecked (default):* Quantile-based grouping producing groups with approximately equal sample sizes.
  * *Checked:* Divides the observed range into intervals of equal width (e.g., uniform 6-month or 1-year spans), which may result in unequal subsample sizes if age is unevenly sampled.

#### Raking Weights (computeWeights)
* **Proportions (var: lvl=p, ...):** Target population proportions specified as semicolon-separated variable blocks, with colon-separated variable names and comma-separated `level=proportion` pairs:
  * Format: `variable1: levelA=0.50, levelB=0.50; variable2: 1=0.30, 2=0.70`
  * Variable names and category labels must match the dataset's variable names and observed level values exactly.
  * The proportions for each stratification variable must sum to approximately `1.00` (between `0.95` and `1.05`).
  * If left empty, the *Stratification Marginals* table still displays observed sample counts and proportions so you can inspect the exact level labels before entering census targets.
* **Standardize raking weights (min weight = 1):** Rescales the raked weights by dividing by the minimum weight so that the smallest weight equals `1.0` (recommended for weighted Rankit estimation in `cNORM`).

---

### Output

#### Norm Group Summary
* **Group:** Sequential cohort index.
* **Group Mean (Midpoint):** Group midpoint value assigned by `cNORM::getGroups()`.
* **Min / Max:** Minimum and maximum observed values within each cohort.
* **N:** Subsample size in the cohort.
* **% of Sample:** Percentage of valid observations falling into the cohort.

#### Norm Group Sample Sizes Plot
Bar chart displaying the sample size (*N*) per norm group along with its midpoint and `[Min–Max]` interval. Two horizontal reference lines aid sample evaluation:
* **Green dashed line (*N* = 100):** Recommended target subsample size per group.
* **Orange dotted line (*N* = 50):** Practical lower bound warning threshold per group.

#### Stratification Marginals & Raking Weights
* **Variable / Level:** Demographic stratification variable and observed category label.
* **Sample N / Sample Prop.:** Observed complete-case count and proportion in the sample.
* **Target Prop.:** User-specified population census proportion.
* **Mean Raked Weight:** Average post-stratification weight assigned to observations in that category level.

#### Raking Weight Diagnostics
* **Complete Cases:** Number of observations with non-missing values across all stratification variables (*N*).
* **Min Weight / Max Weight / Mean Weight:** Summary statistics of the computed raking weights.
* **Design Effect:** Kish's design effect due to unequal weighting, `DEFF = N / N_eff`. Values close to `1.00` indicate minimal variance inflation; larger values indicate substantial precision loss from extreme weights.
* **Effective N:** Kish's effective sample size, `N_eff = (∑w)² / ∑(w²)` (Kish, 1965).

---

### References

* Deming, W. E., & Stephan, F. F. (1940). On a least squares adjustment of a sampled frequency table when the expected marginal totals are known. *The Annals of Mathematical Statistics*, *11*(4), 427–444. https://doi.org/10.1214/aoms/1177731829
* Gary, S., Lenhard, A., Lenhard, W., & Herzberg, D. S. (2023a). Reducing the bias of norm scores in non-representative samples: Weighting as an adjunct to continuous norming methods. *Assessment*, *30*(8), 2491–2509. https://doi.org/10.1177/10731911231153832
* Gary, S., Lenhard, W., Lenhard, A., & Herzberg, D. S. (2023b). A tutorial on automatic post-stratification and weighting in conventional and regression-based norming of psychometric tests. *Behavior Research Methods*, *56*, 4632–4642. https://doi.org/10.3758/s13428-023-02207-0
* Kish, L. (1965). *Survey sampling*. Wiley.
* Lenhard, A., Lenhard, W., Suggate, S., & Segerer, R. (2018). A continuous solution to the norming problem. *Assessment*, *25*(1), 112–125. https://doi.org/10.1177/1073191116656437

---

### R Packages

* [cNORM](https://cran.r-project.org/package=cNORM)
* [ggplot2](https://cran.r-project.org/package=ggplot2)