Single Group Norming (Conventional Norming)
===

Single Group Norming transforms raw test scores from a single homogeneous normative sample into percentiles and standard norm scores (e.g., *T*, *IQ*, *z*, *Wechsler*, or *PISA* scores) without an age or grouping variable.

Empirical percentiles are first determined via Rankit normal-quantile estimation (Bliss, 1967), optionally incorporating survey/raking weights. Users can either output these manifest empirical Rankit norms directly or apply **polynomial regression smoothing** (`cNORM`), which models raw scores as a polynomial function of person location (*L*). Smoothing regularizes sampling irregularities, interpolates missing intermediate raw scores, and reduces norm table jaggedness in moderate-sized samples (Lenhard et al., 2018).

---

### Methodological Recommendations

* **When to Use:** Use *Single Group Norming* when test performance does not systematically depend on age or grade (e.g., adult-only or single-grade norms), or when norming a single pre-selected cohort. If scores vary across age or grade, *Continuous Norming* across the full sample yields substantially higher precision than separate single-group normings (Lenhard & Lenhard, 2021).
* **Sample Size:** Conventional single-group norming typically requires *N* ≥ 250–300 cases for stable extreme norm scores, though polynomial smoothing helps mitigate random sampling artefacts in smaller samples.
* **Smoothing Degree:** A location degree of *k* = 4 or 5 with automatic term selection captures skewness and ceiling/floor effects well in most psychometric distributions. Inspect the **Norm Score Conversion** plot to verify that the orange model curve tracks the empirical points monotonically without boundary oscillations.

---

### Input

#### Assignment Box
* **Raw score variable:** Numeric variable containing the observed test raw scores (minimum 5 valid cases with at least 2 distinct values).
* **Weights (optional):** Positive numeric case weights (e.g., raking weights computed in *Data Preparation*) for weighted empirical percentile estimation and weighted polynomial regression (Gary et al., 2023).

#### Scale & Ranking Options
* **Norm scale:** Target standard score metric:
  * *T (m = 50, sd = 10)*
  * *IQ (m = 100, sd = 15)*
  * *z (m = 0, sd = 1)*
  * *Wechsler (m = 10, sd = 3)*
  * *PISA (m = 500, sd = 100)*
* **Invert ranking order (descending):** Check when *lower* raw scores represent *better* performance (e.g., completion time or error counts).

#### Regression Smoothing
* **Smooth with polynomial regression:** Fits a univariate Taylor polynomial regressing raw scores on powers of the Rankit norm location (*L*, *L*², …, *L*<sup>*k*</sup>). If unchecked, manifest empirical Rankit percentiles and norm scores are reported directly for observed raw scores.
* **Degree of location (k):** Maximum polynomial degree *k* (1 to 6; default = 5).
* **Automatic term selection / Manual number of terms:** Automatically selects the optimal subset of polynomial terms via `leaps` subset selection, or fits a user-specified **Number of terms** (1 to *k*).

#### Output
* **Result plot:** Selects the vertical axis of the *Norm Score Conversion* plot (*Norm score* or *Percentile*).
* **Model fit and coefficients:** Displays the *Model Fit* and *Regression Coefficients* tables (available when polynomial smoothing is enabled).
* **Norm Score Table:**
  * **Norm table range (± SD):** Restricts smoothed norm scores to *M* ± *range* × *SD* (default = ±3.0 SD).
  * **Minimum raw score / Maximum raw score (0 = auto):** Custom raw score bounds for smoothed lookup tables (applied when *Maximum raw score* > *Minimum raw score*).
  * **Step size:** Raw score increment for smoothed lookup tables (default = 1.0). When smoothing is disabled, all distinct observed raw scores are listed.
* **Confidence Intervals:**
  * **Include confidence intervals:** Computes true-score confidence intervals accounting for regression to the mean (*z*<sub>pred</sub> = *r*<sub>*xx*</sub> · *z*, *SE*<sub>*E*</sub> = √(*r*<sub>*xx*</sub>(1 − *r*<sub>*xx*</sub>))).
  * **Confidence level:** Desired confidence level (default = `0.90`).
  * **Reliability coefficient:** Test reliability estimate *r*<sub>*xx*</sub> (default = `0.90`).

---

### Output

#### Norm Score Conversion Plot
Plots Raw Scores (x-axis) against observed Rankit Norm Scores or Percentiles (black points). When polynomial regression smoothing is active, the fitted conversion function is overlaid as an orange curve, allowing visual inspection of fit and monotonicity at the tails.

#### Model Fit
* **N:** Number of valid complete cases.
* **Terms:** Number of polynomial terms retained in the regression model.
* **RMSE:** Root mean square error in raw score units.
* **Adj. R²:** Adjusted proportion of variance explained.
* **BIC:** Bayesian Information Criterion of the selected subset model.

#### Regression Coefficients
Lists the intercept and estimated polynomial weights (*L*<sub>1</sub>, *L*<sub>2</sub>, …) used to map norm locations to raw scores.

#### Norm Score Table
* **Raw Score:** Test raw score.
* **Norm Score:** Standard score on the chosen norm scale.
* **Percentile:** Cumulative percentile rank (0–100).
* **Lower CI / Upper CI (Norm & PR):** True-score confidence interval bounds on the standard score and percentile rank scales (when enabled).

---

### References

* Bliss, C. I. (1967). *Statistics in biology* (Vol. 1). McGraw-Hill.
* Gary, S., Lenhard, A., Lenhard, W., & Herzberg, D. S. (2023). Reducing the bias of norm scores in non-representative samples: Weighting as an adjunct to continuous norming methods. *Assessment*, *30*(8), 2491–2509. https://doi.org/10.1177/10731911231153832
* Lenhard, A., Lenhard, W., Suggate, S., & Segerer, R. (2018). A continuous solution to the norming problem. *Assessment*, *25*(1), 112–125. https://doi.org/10.1177/1073191116656437
* Lenhard, W., & Lenhard, A. (2021). Improvement of norm score quality via regression-based continuous norming. *Educational and Psychological Measurement*, *81*(2), 229–261. https://doi.org/10.1177/0013164420928457

---

### R Packages

* [cNORM](https://cran.r-project.org/package=cNORM)
* [ggplot2](https://cran.r-project.org/package=ggplot2)