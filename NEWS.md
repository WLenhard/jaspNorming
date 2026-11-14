# jaspNorming 3.7.0

* Initial release of the **jaspNorming** module for JASP (version number synchronized with `cNORM` 3.7.0 on CRAN).
* Implemented **Continuous Norming** supporting distribution-free Taylor polynomials (`cnorm`), Beta-Binomial regression (`cnorm.betabinomial`), Sinh-Arcsinh regression (`cnorm.shash`), and Conway-Maxwell-Poisson count regression (`cnorm.cmp`).
* Implemented **Single Group Norming** for conventional Rankit norming with optional univariate polynomial regression smoothing.
* Implemented **Data Preparation** for cohort partitioning (`getGroups`) and post-stratification survey weighting via Iterative Proportional Fitting / raking (`computeWeights`) with Kish design-effect diagnostics.
* Implemented support for saving computed norm group midpoints, raked case weights, individual predicted norm scores, and percentiles directly to the JASP dataset.
* Included example datasets (`elfe.csv`, `speed.csv`, `ppvt.csv`, and `CDC.csv`), comprehensive built-in Markdown help files, and dynamic literature citations.