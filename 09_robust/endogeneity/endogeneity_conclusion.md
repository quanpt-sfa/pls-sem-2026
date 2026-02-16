# Gaussian Copula Endogeneity Test — Results

Date: 2026-02-16 03:07:12.566598
N = 400, B = 5000, seed = 18
Primary rule: 95% bootstrap percentile CI excludes 0 (Park & Gupta, 2012)
Strategy: one-at-a-time (one COP_X per augmented model)

## Pre-screening gates

- **Practical non-normality**: |skewness| >= 1 OR |excess kurtosis| >= 1
- **Collinearity pre-screen**: cor(X, COP_X) <= 0.95
- Shapiro-Wilk & JB reported but intentionally NOT used for gating (inflated type-I at large N)
- Variables tested: 2, Skipped: 1 (of 3 suspected)

### Skipped variables

- **AJ** (AQ eq): cor(X, COP_X)=0.9599 > 0.95 — near-linear transform

## Decisions per tested variable

- **COM -> AJ**: NO_ENDOGENEITY (gamma CI [-0.1213, 0.1539] includes 0)
- **PS -> AJ**: INCONCLUSIVE (VIF(COP_PS)=6.7 > 5; copula too collinear with X)

## Interpretation

**Inconclusive** for at least one variable (high VIF even in one-at-a-time model).
The copula term COP_X uses rank-based transform: u = (rank(x) - 0.5)/n; COP = Φ⁻¹(u).
When X is moderately non-normal (|skew| ~1–2), cor(X, COP_X) can reach 0.90+,
inflating VIF beyond 5 and making the gamma test inconclusive.

**Supplementary evidence for endogeneity assessment:**
1. Model B robustness (Section E): if substantive paths are stable after adding
   control variables, this reduces omitted-variable-bias concern.
2. Theoretical argumentation for causal direction (cite prior literature).
3. Data sensitivity (Section A): pre- vs post-screening path stability.
4. Consider IV/2SLS if suitable instruments are identifiable.
5. Report as a limitation and qualify causal language accordingly.

## References
- Park, S., & Gupta, S. (2012). Handling endogenous regressors by joint estimation using copulas. *Marketing Science*, 31(4), 567-586.
- Hair, J. F., Hult, G. T. M., Ringle, C. M., & Sarstedt, M. (2022). *A Primer on Partial Least Squares Structural Equation Modeling (PLS-SEM)* (3rd ed.). Sage.
- Hult, G. T. M., Hair, J. F., Proksch, D., Sarstedt, M., Pinkwart, A., & Ringle, C. M. (2018). Addressing endogeneity in international marketing applications of partial least squares structural equation modeling. *JIBS*, 49(6), 713-729.
