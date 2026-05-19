# Pilot Summary Report (Type A — Feasibility)
Date: 2026-02-16 20:14:28.120967
Seed: 42

## Sample
- Raw observations:           34
- After QC screening:         32
- After missing data:         32
- Total removed:              2 (5.9%)

## Pre-PLS Feasibility Check
- Status: RED FLAG (1 issue(s))
  - Perfect correlations (|r|>=1.000): ETH1 ~ ETH4 (r=1.0000)
- Phase 2 (CMV + Measurement): SKIPPED due to red flags
- Note: Data singularity is expected with small N. The main study (N>>30) will not have this issue.

## Indicator Review
No items auto-removed (auto_drop = DISABLED for Type A pilot).

## Final Instrument
- COM [formative]: 9 indicators — COM1, COM2, COM3, COM4, COM5, COM6, COM7, COM8, COM9
- PS [reflective]: 5 indicators — PS1, PS2, PS3, PS4, PS5
- MO [formative]: 5 indicators — MO1, MO2, MO3, MO4, MO5
- TP [reflective]: 4 indicators — TP1, TP2, TP3, TP4
- IT [reflective]: 4 indicators — IT1, IT2, IT3, IT4
- CC [reflective]: 5 indicators — CC1, CC2, CC3, CC4, CC5
- CG [formative]: 9 indicators — CG1, CG2, CG3, CG4, CG5, CG6, CG7, CG8, CG9
- ETH [formative]: 4 indicators — ETH1, ETH2, ETH3, ETH4
- TC [formative]: 6 indicators — TC1, TC2, TC3, TC4, TC5, TC6
- AJ [reflective]: 3 indicators — AJ1, AJ2, AJ3
- AQ [formative]: 6 indicators — AQ1, AQ2, AQ3, AQ4, AQ5, AQ6

## Bootstrap
- bootstrap_samples: 0
- HTMT CI and formative weight significance: NOT AVAILABLE
- Point estimates (loadings, weights, VIF, reliability) are reported.

## Instrument Lock
- instrument_locked.json = frozen snapshot (no statistical trimming)
- Pilot did NOT remove or add any indicators.
- Statistical refinement (if needed) is performed in MAIN stage.

## CMV
- CMV inference not performed in pilot (insufficient conditions for full collinearity VIF).
- CMV/CMB is evaluated in the MAIN study with full sample.

## Interpretation Guide (Type A Pilot)
This is a Type A feasibility pilot (~30 respondents).
Purpose: spot catastrophic issues (process, data quality, instrument).

Red flags to investigate:
- Reflective loadings < 0.30 (catastrophic)
- Outer VIF > 10 (severe multicollinearity)
- Reliability (CR, rho_A) < 0.60
- HTMT > 0.95 (near-total lack of discriminant validity)

Do NOT use p-values or bootstrap to remove indicators.
Do NOT interpret results as final research conclusions.
