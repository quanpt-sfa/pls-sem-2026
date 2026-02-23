# Disclaimer — Level-based Heterogeneity Sensitivity Analysis

This analysis is a **sensitivity check for level-based observed heterogeneity**
using score-based clustering on **exogenous** construct scores only.

## What this IS:
- A post-hoc sensitivity analysis to assess whether path coefficient
  conclusions are robust across naturally occurring subpopulations.
- Clustering is performed exclusively on exogenous construct scores
  to avoid circularity (endogenous constructs are excluded).
- MICOM (Henseler et al., 2016) is applied **before** MGA to verify
  measurement comparability across groups.

## What this is NOT:
- This is **NOT** FIMIX-PLS (Finite Mixture PLS) or latent class segmentation.
  FIMIX-PLS identifies unobserved heterogeneity via mixture model estimation
  directly on the structural model, which is methodologically different.
- This is **NOT** PLS-POS (Prediction-Oriented Segmentation).
- This analysis does not replace formal latent heterogeneity methods.

## Rationale for this approach:
- Open-source R tools (seminr, cSEM) have limited support for FIMIX-PLS
  with formative constructs and complex structural models.
- Score-based clustering on exogenous constructs provides a practical
  level-based check without violating model assumptions.
- This follows the recommendation in Hair et al. (2022, Ch. 11): when
  FIMIX is not feasible, researchers should still check for potential
  heterogeneity using available techniques.

Configuration: seed=18, clustering=kmeans, k=2/3/4/5, MICOM permutations=5000, MGA permutations=5000

---
Generated: 2026-02-19 00:56:11
