Continuous Norming
===

Continuous norming estimates psychometric, biometric, or medical norm scores (percentiles and standard scores) as a continuous mathematical function of an explanatory variable such as age, grade, or duration of schooling. By modeling the entire normative sample simultaneously rather than splitting it into isolated subsamples, continuous norming reduces required sample sizes, smooths random sampling fluctuations across groups, and eliminates norming gaps between adjacent age or grade intervals (Lenhard et al., 2018; Lenhard & Lenhard, 2021).

This analysis provides four complementary continuous norming approaches from the `cNORM` package:

* **Distribution-Free (Taylor Polynomials):** Models raw scores as a bivariate polynomial function of person location (*L*) and the explanatory variable (*A*) without assuming a specific parametric raw score distribution (Lenhard et al., 2018). Recommended as a versatile all-round method, especially for skewed, multi-modal, or irregular distributions and scores spanning negative values.
* **Beta-Binomial:** Parametric continuous norming for bounded accuracy scales with a fixed number of dichotomous items *n* (e.g., 1PL IRT / Rasch or sum-score power tests without strict time limits). Models the shape parameters α and β across age via polynomial regression on the log scale.
* **Sinh-Arcsinh (SHASH):** Parametric four-parameter transformation of the normal distribution modeling location (μ), scale (σ), skewness (ε), and tail weight (δ) continuously across age (Jones & Pewsey, 2009; Lenhard et al., 2019). Well suited for continuous or quasi-continuous scales with age-dependent floor/ceiling skewness and varying kurtosis.
* **Conway-Maxwell-Poisson (CMP):** Parametric count-data regression modeling mean (μ) and dispersion (ν) across age (Conway & Maxwell, 1962; Huang, 2017). Specifically suited for speeded cognitive tasks and open-ended or ceiling-truncated count data because it accommodates over-dispersion (ν < 1), equi-dispersion (ν = 1, Poisson), and under-dispersion (ν > 1, variance smaller than the mean).

---

### Methodological Recommendations

* **Sample Size:** Continuous norming requires substantially smaller samples than conventional norming per age cohort; however, at least *N* = 50–100 observations per age cohort/segment (typically *N* ≥ 300–500 total across the age range) are recommended for stable extreme percentiles.
* **Choosing Polynomial Degrees:** Higher polynomial degrees increase flexibility but raise the risk of overfitting or intersecting percentile curves in sparse boundary regions. Always inspect the **Percentile Curves** plot across the full range of the explanatory variable.
* **Extrapolation:** Norm tables should primarily be generated within the observed range of the explanatory variable. Extrapolating beyond the minimum or maximum observed age/group can lead to boundary distortions.
* **Information Criteria:** AIC and BIC values can be used to compare competing parameterizations *within* the same parametric model family, but are not directly comparable across discrete (Beta-Binomial, CMP), continuous (SHASH), and distribution-free (Taylor) model classes.

---

### Input

#### Assignment Box
* **Raw score variable:** Numeric variable containing the observed test raw scores.
  * *Beta-Binomial* and *CMP* require non-negative integer count scores.
* **Explanatory / grouping variable (e.g., age):** Continuous or ordinal variable on which raw scores depend (e.g., exact chronological age, grade level). Must contain at least two distinct values.
* **Weights (optional):** Positive numeric case weights (e.g., post-stratification / raking weights from *Data Preparation*) to compensate for non-representative sampling (Gary et al., 2023).

#### General Settings
* **Modeling method:** Selects the statistical norming model (*Taylor Polynomials*, *Beta-Binomial*, *SHASH*, or *CMP*).
* **Norm scale:** Target standard score metric:
  * *T (m = 50, sd = 10)*
  * *IQ (m = 100, sd = 15)*
  * *z (m = 0, sd = 1)*
  * *Wechsler (m = 10, sd = 3)*
  * *PISA (m = 500, sd = 100)*

#### Model Parameters

##### Taylor Polynomial Options
* **Invert ranking order (descending):** Check when *lower* raw scores indicate *higher* performance (e.g., reaction times, completion times, or error counts).
* **Norm groups for continuous age (0 = auto):** Number of discrete norm groups used for initial within-group Rankit percentile estimation before fitting the continuous Taylor model. When set to `0`, `cNORM` automatically groups continuous explanatory variables with more than 20 unique values. If the explanatory variable is already pre-grouped, set this to `0`.
* **Equidistant group intervals:** Forms intervals of equal width across the explanatory variable rather than intervals of approximately equal sample size (quantile-based).
* **Degree of location (k):** Maximum polynomial degree of the norm score location *L* (1 to 6; default = 5). Controls how flexibly distributional shape (skewness, kurtosis) is modeled within age levels.
* **Degree of explanatory variable (t):** Maximum polynomial degree of the explanatory variable *A* (1 to 6; default = 3). Controls curvature of developmental trajectories across age ( automatically capped below the number of distinct groups).
* **Automatic term selection / Manual number of terms:** In automatic mode, `bestModel` selects a parsimonious subset of powers and interactions (*L*<sup>*i*</sup> *A*<sup>*j*</sup>) using `leaps` subset selection (*R*²<sub>adj</sub> / Mallows' *C*<sub>*p*</sub> / BIC). In manual mode, the exact **Number of terms** is fitted.

##### Beta-Binomial Options
* **Degree of α (alpha):** Polynomial degree of age for modeling log(α) (0 to 5; default = 3).
* **Degree of β (beta):** Polynomial degree of age for modeling log(β) (0 to 5; default = 3).
* **Number of items in test (0 = max observed):** Fixed upper bound *n* (total number of dichotomous items). If `0`, the maximum observed raw score in the sample is used.

##### Sinh-Arcsinh (SHASH) Options
* **Degree of μ (location):** Polynomial degree of age for the location parameter μ (0 to 5; default = 3).
* **Degree of σ (scale):** Polynomial degree of age for log(σ) (0 to 5; default = 2).
* **Degree of ε (skewness):** Polynomial degree of age for the skewness parameter ε (0 to 5; default = 2).
* **Degree of δ (tail weight):** Polynomial degree of age for log(δ) when tail weight varies with age (0 to 5; default = 1).
* **Fix tail weight δ to constant / Fixed δ value:** Fixes δ across all ages instead of estimating age polynomials (δ = 1 corresponds to normal-like tail weight; δ > 1 produces lighter tails, δ < 1 heavier tails).

##### Conway-Maxwell-Poisson (CMP) Options
* **Degree of μ (location):** Polynomial degree of age for the mean parameter μ (1 to 5; default = 3).
* **Degree of ν (dispersion):** Polynomial degree of age for log(ν) (0 to 5; default = 2).
* **Fix dispersion ν / Fixed ν value:** Fixes ν to a constant across age (e.g., ν = 1 yields a standard Poisson regression model; ν > 1 models under-dispersed speeded counts; ν < 1 models over-dispersed counts).
* **Maximum score ceiling (0 = untruncated):** Optional upper truncation limit if the count task has a maximum attainable score ceiling. Set to `0` for open-ended count scales.

#### Output & Norm Table
* **Model fit and parameter estimates:** Displays the *Model Fit* summary table and *Parameter Estimates* table.
* **Percentile curves plot:** Plots observed raw scores against the explanatory variable overlaid with fitted percentile curves (2.5%, 10%, 25%, 50%, 75%, 90%, and 97.5%).
* **Norm Table Specification:**
  * **Value for norm table (e.g., age):** Enter a numeric value of the explanatory variable (e.g., `8.5` for age 8 years 6 months) to generate a raw-to-norm lookup table for that specific point. Leave empty to omit the lookup table.
  * **Norm score range (± SD):** Clips predicted norm scores to *M* ± *range* × *SD* of the chosen norm scale (default = ±3.0 SD, e.g., T = 20 to 80).
  * **Minimum raw score / Maximum raw score (0 = auto):** Custom raw score range for the lookup table (used when *Maximum raw score* > *Minimum raw score*; ignored for Beta-Binomial, which tabulates `0` to *n*).
  * **Step size:** Increment between consecutive raw scores in the lookup table (default = 1.0; rounded to integers ≥ 1 for CMP; fixed at 1 for Beta-Binomial).
* **Confidence Intervals:**
  * **Include confidence intervals:** Adds classical true-score confidence intervals adjusted for regression to the mean: *z*<sub>pred</sub> = *r*<sub>*xx*</sub> · *z* with standard error of estimation *SE*<sub>*E*</sub> = √(*r*<sub>*xx*</sub>(1 − *r*<sub>*xx*</sub>)).
  * **Confidence level:** Coverage probability (e.g., `0.90` or `0.95`).
  * **Reliability coefficient:** Internal consistency or test-retest reliability *r*<sub>*xx*</sub> (0.01 to 0.999).

---

### Output

#### Percentile Curves Plot
Displays observed individual scores (semi-transparent points) alongside the model-implied conditional percentile trajectories (PR 2.5%, 10%, 25%, 50%, 75%, 90%, 97.5%). Smooth, non-intersecting curves that track empirical score density indicate an appropriate polynomial specification.

#### Model Fit
* **Method:** Fitted continuous norming model family.
* **N:** Number of valid complete observations used in estimation.
* **Terms / Parameters:** Number of selected regression terms (Taylor) or estimated model parameters (Beta-Binomial, SHASH, CMP).
* **AIC / BIC:** Akaike and Bayesian Information Criteria (AIC is omitted for subset-selected Taylor models).
* **R²:** Adjusted *R*² of the polynomial regression (Taylor) or squared correlation / variance explained between manifest and model-predicted norm scores (parametric models).
* **RMSE:** Root mean square error of prediction.

#### Parameter Estimates
* **Term:** Intercept, polynomial powers, or distributional parameter terms (*L*, *A*, α, β, μ, σ, ε, δ, ν).
* **Estimate:** Estimated regression coefficient.
* **SE / z / p** *(Parametric models)*: Asymptotic standard error, Wald *z*-statistic, and two-sided *p*-value from the observed Fisher information / Hessian matrix.

#### Norm Score Table
* **Raw Score:** Raw test score.
* **Norm Score:** Corresponding standard score on the selected norm scale.
* **Percentile:** Cumulative percentile rank (0–100).
* **Lower CI / Upper CI (Norm & PR):** Lower and upper true-score confidence bounds on the norm score and percentile rank scales (shown when *Include confidence intervals* is enabled).

---

### References

* Conway, R. W., & Maxwell, W. L. (1962). A queuing model with state dependent service rates. *Journal of Industrial Engineering*, *13*(2), 132–136.
* Gary, S., Lenhard, A., Lenhard, W., & Herzberg, D. S. (2023). Reducing the bias of norm scores in non-representative samples: Weighting as an adjunct to continuous norming methods. *Assessment*, *30*(8), 2491–2509. https://doi.org/10.1177/10731911231153832
* Gary, S., Lenhard, W., Lenhard, A., & Herzberg, D. S. (2023). A tutorial on automatic post-stratification and weighting in conventional and regression-based norming of psychometric tests. *Behavior Research Methods*, *56*, 4632–4642. https://doi.org/10.3758/s13428-023-02207-0
* Huang, A. (2017). Mean-parametrized Conway–Maxwell–Poisson regression models for dispersed counts. *Statistical Modelling*, *17*(6), 359–380. https://doi.org/10.1177/1471082X17697749
* Jones, M. C., & Pewsey, A. (2009). Sinh-arcsinh distributions. *Biometrika*, *96*(4), 761–780. https://doi.org/10.1093/biomet/asp053
* Lenhard, A., Lenhard, W., & Gary, S. (2019). Continuous norming of psychometric tests: A simulation study of parametric and semi-parametric approaches. *PLOS ONE*, *14*(9), e0222279. https://doi.org/10.1371/journal.pone.0222279
* Lenhard, A., Lenhard, W., Suggate, S., & Segerer, R. (2018). A continuous solution to the norming problem. *Assessment*, *25*(1), 112–125. https://doi.org/10.1177/1073191116656437
* Lenhard, W., & Lenhard, A. (2021). Improvement of norm score quality via regression-based continuous norming. *Educational and Psychological Measurement*, *81*(2), 229–261. https://doi.org/10.1177/0013164420928457

---

### R Packages

* [cNORM](https://cran.r-project.org/package=cNORM)
* [ggplot2](https://cran.r-project.org/package=ggplot2)