# Thesis Insert — Level-based Heterogeneity Sensitivity

**4.X.X Sensitivity Analysis for Level-based Heterogeneity**

To assess whether the structural model results are robust across naturally occurring subpopulations, a sensitivity analysis for level-based observed heterogeneity was conducted. Following Hair et al. (2022, Chapter 11), construct scores of the exogenous variables (COM, PS, MO, TP, IT, CEV, CG, TC, ETH) were extracted from the base PLS-SEM model and used as input for kmeans clustering. The optimal number of segments was determined to be k = 2 based on the mean silhouette criterion (average silhouette width = 0.297), yielding groups of n = 261 and 139 respondents respectively. Importantly, only exogenous construct scores were used for segmentation to prevent circularity (Becker et al., 2013).

MICOM (Henseler et al., 2016) was assessed using cSEM::testMICOM() (Rademaker & Schuberth, 2020) with 5000 permutations. Configural invariance (Step 1) holds by design (identical model specification). However, compositional invariance (Step 2) was NOT fully established for all constructs. The following MGA results should therefore be interpreted as exploratory only, because group differences may reflect measurement artefacts rather than true structural differences.

Permutation-based multi-group analysis (MGA; 5000 permutations) was conducted for all 10 path–pair comparisons. After p-value adjustment (holm method), no statistically significant differences were found in any structural path coefficient between the identified segments (all p_adj > 0.05). This result supports the conclusion that the structural model findings are robust against level-based observed heterogeneity.

*Note.* This analysis is a level-based sensitivity check using score-based clustering on exogenous constructs. It does not replace formal latent heterogeneity methods such as FIMIX-PLS or PLS-POS, which were not feasible given the model’s formative constructs and current open-source tool limitations. Configural invariance (MICOM Step 1) is a qualitative assessment established by identical model specification across groups. Steps 2–3 were assessed using cSEM::testMICOM() following Henseler et al. (2016).

---
Generated: 2026-02-16 03:20:40 (seed = 18)
