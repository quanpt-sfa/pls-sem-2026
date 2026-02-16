# Nonlinear Robustness Check — Conclusion
Date: 2026-02-16 03:07:43.908888
Seed: 18, Bootstrap: 5000, CI: percentile 95%
Center method: mean_center

## Summary
Relations tested: 3
Significant quadratic effects: 1

### Competence → Audit Judgment
- beta_linear (base):       0.202
- beta_linear (quad model): 0.222, CI[0.081, 0.368]*
- beta_quadratic:           0.010, CI[-0.024, 0.041]
- delta R²: +0.0005
- Conclusion: Linear conclusion robust — no significant quadratic effect

### Audit Judgment → Audit Quality
- beta_linear (base):       0.385
- beta_linear (quad model): 0.380, CI[0.283, 0.480]*
- beta_quadratic:           -0.008, CI[-0.023, 0.009]
- delta R²: +0.0003
- Conclusion: Linear conclusion robust — no significant quadratic effect

### Ethics → Audit Quality
- beta_linear (base):       0.517
- beta_linear (quad model): 0.472, CI[0.369, 0.573]*
- beta_quadratic:           -0.021, CI[-0.038, -0.005]*
- delta R²: +0.0025
- Conclusion: Evidence of nonlinearity — linear effect stable (same sign/sig); quadratic adds curvature

## Interpretation Guide
This analysis is a ROBUSTNESS CHECK, not a hypothesis test.
The base linear model remains the primary basis for inference.

- If beta_quadratic CI excludes 0: evidence of nonlinearity;
  check whether the linear coefficient changes sign or significance.
- If beta_quadratic CI includes 0 and beta_linear is stable:
  'linear specification is robust to quadratic augmentation'.

Reference: Hair et al. (2022), Henseler & Chin (2010), Sarstedt et al. (2020).
