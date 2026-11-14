# jaspNorming — Continuous and Conventional Norming for JASP

## Overview

**jaspNorming** provides conventional single-group and regression-based continuous norming methods for psychometric test construction, biometrics, and clinical/medical reference curves in [JASP](https://jasp-stats.org/). It is based on the [`cNORM` R package](https://cran.r-project.org/package=cNORM) (Lenhard et al., 2018; v3.7.0+).

Continuous norming models the functional relationship across:
* **Raw scores**
* **Norm scores / percentiles** (*T*, *IQ*, *z*, *Wechsler*, and *PISA* scales)
* **Explanatory variables** (e.g., chronological age, grade level, or duration of schooling)

Compared to conventional subgroup norming, continuous norming:
* Substantially reduces required normative sample sizes
* Closes norming gaps between discrete age or grade intervals
* Smooths random sampling fluctuations across cohorts
* Minimizes error variance and bias in extreme percentiles

---

## Included Analyses

### 1. Continuous Norming
Models norm scores continuously across an explanatory variable (e.g., age or grade), generates percentile curve plots, evaluates model fit and parameter estimates, produces age-specific raw-to-norm lookup tables (with optional classical true-score confidence intervals), and saves individual predicted norm scores or percentiles directly to the JASP spreadsheet. Four modeling approaches are supported:
* **Distribution-Free (Taylor Polynomials):** Bivariate polynomial regression of raw scores on person location (*L*) and age (*A*) without parametric distributional assumptions.
* **Beta-Binomial:** Parametric continuous norming for bounded accuracy scales with a fixed number of dichotomous items (e.g., 1PL IRT / power tests).
* **Sinh-Arcsinh (SHASH):** Flexible four-parameter parametric modeling of location (μ), scale (σ), skewness (ε), and tail weight (δ) across age for continuous and biometric measures.
* **Conway-Maxwell-Poisson (CMP):** Parametric count-data regression modeling location (μ) and dispersion (ν) across age for speeded cognitive tasks and count outcomes (supporting under-, equi-, and over-dispersion).

### 2. Single Group Norming (Conventional Norming)
Computes empirical Rankit percentiles and standard norm scores within a single homogeneous cohort, with optional univariate polynomial regression smoothing, conversion plots, lookup tables with confidence intervals, and saving of individual norm scores and percentiles to the dataset.

### 3. Data Preparation (Age Groups & Raking)
* **Norm Group Formation (`getGroups`):** Splits continuous explanatory variables into quantile-based (equal-sized) or equidistant age cohorts, visualizes subsample sizes against recommended thresholds (*N* = 50 and *N* = 100), and saves group midpoints to the dataset.
* **Post-Stratification Raking (`computeWeights`):** Computes survey weights via Iterative Proportional Fitting (IPF) to align non-representative sample marginals with target census proportions, reports Kish's effective sample size (*N*<sub>eff</sub>) and design effect (*DEFF*), and saves raked weights to the dataset for weighted norming.

---

## Example Datasets

The module ships with four example datasets (in `inst/datasets/`):

* **`elfe.csv`** (*N* = 1,400): Raw scores (`0`–`28`) and grade levels (`2`–`5`) from the speeded sentence comprehension subtest of *ELFE 1–6* (Lenhard & Schneider, 2006).
* **`speed.csv`** (*N* = 1,438): Synthetic dataset preserving the empirical age trajectory and variance structure of the speeded word reading fluency subtest from *ELFE II* (Lenhard, Lenhard, & Schneider, 2017), ideal for demonstrating CMP and continuous norming.
* **`ppvt.csv`** (*N* = 4,542): Receptive vocabulary development data across ages 2.5 to 17 years based on the German adaptation of the *PPVT-4* (Lenhard, Lenhard, Segerer, & Suggate, 2015), including demographic stratification variables (`sex`, `migration`, `region`) for demonstrating post-stratification raking and continuous norming.
* **`CDC.csv`** (*N* = 45,035): Public-domain U.S. NHANES anthropometric data (ages 2 to 25 years; `age`, `group`, `month`, `sex`, `height`, `weight`, `bmi`) from the Centers for Disease Control and Prevention (2012) for modeling biometric growth and BMI reference percentile curves (e.g., via SHASH or Taylor polynomials).

---

## Key References

* Conway, R. W., & Maxwell, W. L. (1962). A queuing model with state dependent service rates. *Journal of Industrial Engineering*, *13*(2), 132–136.
* Gary, S., Lenhard, W., & Lenhard, A. (2021). Modelling norm scores with the cNORM package in R. *Psych*, *3*(3), 501–521. https://doi.org/10.3390/psych3030033
* Gary, S., Lenhard, A., Lenhard, W., & Herzberg, D. S. (2023). Reducing the bias of norm scores in non-representative samples: Weighting as an adjunct to continuous norming methods. *Assessment*, *30*(8), 2491–2509. https://doi.org/10.1177/10731911231153832
* Gary, S., Lenhard, W., Lenhard, A., & Herzberg, D. S. (2023). A tutorial on automatic post-stratification and weighting in conventional and regression-based norming of psychometric tests. *Behavior Research Methods*, *56*, 4632–4642. https://doi.org/10.3758/s13428-023-02207-0
* Huang, A. (2017). Mean-parametrized Conway–Maxwell–Poisson regression models for dispersed counts. *Statistical Modelling*, *17*(6), 359–380. https://doi.org/10.1177/1471082X17697749
* Jones, M. C., & Pewsey, A. (2009). Sinh-arcsinh distributions. *Biometrika*, *96*(4), 761–780. https://doi.org/10.1093/biomet/asp053
* Laurenz, R., & Lenhard, W. (2025). Comparison of cNORM and LMS methods for estimating reference percentile curves from biometric data. *Scientific Reports*, *15*, 29580. https://doi.org/10.1038/s41598-025-29580-4
* Lenhard, A., Lenhard, W., & Gary, S. (2019). Continuous norming of psychometric tests: A simulation study of parametric and semi-parametric approaches. *PLOS ONE*, *14*(9), e0222279. https://doi.org/10.1371/journal.pone.0222279
* Lenhard, A., Lenhard, W., Suggate, S., & Segerer, R. (2018). A continuous solution to the norming problem. *Assessment*, *25*(1), 112–125. https://doi.org/10.1177/1073191116656437
* Lenhard, W., & Lenhard, A. (2021). Improvement of norm score quality via regression-based continuous norming. *Educational and Psychological Measurement*, *81*(2), 229–261. https://doi.org/10.1177/0013164420928457

---

## Further Information & Tutorials

Detailed methodological guides, vignettes, and tutorials are available at [Psychometrica](https://www.psychometrica.de/cNorm_en.html).

## License

GPL (>= 3)