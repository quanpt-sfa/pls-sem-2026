# MICOM Results — Measurement Invariance of Composite Models

## Reference
Henseler, J., Ringle, C. M., & Sarstedt, M. (2016). Testing measurement
invariance of composites using partial least squares. *International
Marketing Review*, 33(3), 405–431.

## Configuration: 5000 permutations, alpha = 0.05

## Method: cSEM::testMICOM() (Rademaker & Schuberth, 2020; R package cSEM)

## Step 1 — Configural Invariance
Configural invariance is a **qualitative assessment** (not a statistical test).
It is **assumed** because the same model specification
(constructs, indicators, structural paths) is applied to all groups.

## Step 2 — Compositional Invariance

| Pair | Construct | c (original) | CI Lower 5% | p-value | Decision |
|------|-----------|-------------|-------------|---------|----------|
| Group_1_vs_2 | COM | 0.8625 | 0.8878 | 0.0158 | NOT INVARIANT |
| Group_1_vs_2 | PS | 0.9986 | 0.9997 | <0.001 | NOT INVARIANT |
| Group_1_vs_2 | MO | 0.8457 | 0.8635 | 0.0270 | NOT INVARIANT |
| Group_1_vs_2 | TP | 0.9946 | 0.9927 | 0.1004 | INVARIANT |
| Group_1_vs_2 | IT | 0.9992 | 0.9986 | 0.1568 | INVARIANT |
| Group_1_vs_2 | CEV | 0.9997 | 0.9990 | 0.5534 | INVARIANT |
| Group_1_vs_2 | CG | 0.8396 | 0.8333 | 0.0606 | INVARIANT |
| Group_1_vs_2 | TC | 0.8439 | 0.7845 | 0.1514 | INVARIANT |
| Group_1_vs_2 | ETH | 0.9683 | 0.9782 | 0.0104 | NOT INVARIANT |
| Group_1_vs_2 | AJ | 0.9998 | 0.9998 | 0.0350 | NOT INVARIANT |
| Group_1_vs_2 | AQ | 0.9786 | 0.9663 | 0.2406 | INVARIANT |

**Result**: Compositional invariance is **NOT fully established**.
MGA results should be interpreted with caution (exploratory only).

## Step 3 — Equality of Composite Means and Variances

### Group_1_vs_2
- Constructs with unequal means: 11 / 11
- Constructs with unequal variances: 9 / 11
- Note: Partial measurement invariance established. MGA should
  use permutation-based approach (which does not require full
  metric invariance).

## Methodology Notes

1. **Step 1 (Configural invariance)** is a qualitative assessment, not a
   statistical test. It is established by ensuring the same model specification
   (same constructs, indicators, and structural paths) is applied to all groups.

2. **Steps 2–3 were assessed using `cSEM::testMICOM()`** following
   Henseler, Ringle & Sarstedt (2016). The cSEM package implements
   the permutation-based MICOM procedure as described in the original paper.

3. **If Step 2 (compositional invariance) fails**, any subsequent
   multi-group comparison (MGA) should be treated as **exploratory only**,
   because group differences in path coefficients may reflect measurement
   artefacts rather than true structural differences.

4. **This is a level-based observed heterogeneity analysis** using
   score-based clustering on exogenous constructs. It is methodologically
   distinct from FIMIX-PLS (latent class segmentation on the structural model)
   and PLS-POS (prediction-oriented segmentation).

---
Generated: 2026-02-16 03:20:40
