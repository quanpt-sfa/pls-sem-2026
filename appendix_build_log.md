# Appendix Build Log

Build time: 2026-05-05 10:36:50

## 1) Repository folders inspected
- 00_meta, 01_raw, 02_clean, 03_qc, 04_cmv, 04_descriptives, 05_measurement, 06_structural, 07_predict, 08_complex, 09_robust, 10_report, config, R

## 2) Pipeline files read
- README.md
- run_pipeline.R
- R/run_core.R
- run_main.R
- run_pilot.R
- config/main.yml
- config/pilot.yml
- R/14_report_export.R
- R/utils_tables.R
- R/04_descriptives.R
- R/05_cmv_assessment.R

## 3) CSV files included in Word
- 02_clean/missing_data_report.csv
- 03_qc/qc_summary.csv
- 03_qc/sample_flow.csv
- 04_cmv/full_collinearity_vif.csv
- 04_descriptives/construct_descriptive_stats.csv
- 04_descriptives/control_var_coding.csv
- 04_descriptives/correlation_matrix.csv
- 04_descriptives/demographic_profile.csv
- 04_descriptives/demographics/bang_cap.csv
- 04_descriptives/demographics/chung_chi_hanh_nghe.csv
- 04_descriptives/demographics/gioi_tinh.csv
- 04_descriptives/demographics/kinh_nghiem.csv
- 04_descriptives/demographics/vi_tri.csv
- 04_descriptives/descriptive_stats.csv
- 04_descriptives/sample_demographics.csv
- 05_measurement/formative/indicator_decisions.csv
- 05_measurement/formative/nomological_validity.csv
- 05_measurement/formative/outer_loadings_formative.csv
- 05_measurement/formative/outer_vif.csv
- 05_measurement/formative/outer_weights_bootstrap.csv
- 05_measurement/reflective/htmt_ci_bootstrap.csv
- 05_measurement/reflective/htmt_matrix.csv
- 05_measurement/reflective/htmt_matrix_full.csv
- 05_measurement/reflective/indicator_decisions.csv
- 05_measurement/reflective/outer_loadings.csv
- 05_measurement/reflective/reliability_table.csv
- 06_structural/f_squared.csv
- 06_structural/inner_vif.csv
- 06_structural/path_coefficients_bootstrap.csv
- 06_structural/r_squared.csv
- 07_predict/plspredict_results.csv
- 07_predict/plspredict_vs_lm.csv
- 07_predict/prediction_classification.csv
- 08_complex/mediation/mediation_classification.csv
- 08_complex/mediation/specific_indirect_effects.csv
- 08_complex/moderation/interaction_coefficients.csv
- 08_complex/moderation/simple_slopes_ETH.csv
- 08_complex/moderation/simple_slopes_TC.csv
- 09_robust/controlled_model/control_path_coefficients.csv
- 09_robust/controlled_model/indirect_effects_A_vs_B.csv
- 09_robust/controlled_model/path_comparison_A_vs_B.csv
- 09_robust/controlled_model/r2_comparison_A_vs_B.csv
- 09_robust/endogeneity/copula_decisions.csv
- 09_robust/endogeneity/copula_nonnormality.csv
- 09_robust/endogeneity/copula_results.csv
- 09_robust/endogeneity/copula_terms.csv
- 09_robust/endogeneity/copula_vif.csv
- 09_robust/endogeneity/distribution_check.csv
- 09_robust/level_heterogeneity/tables/clustering_k_selection.csv
- 09_robust/level_heterogeneity/tables/mga_results.csv
- 09_robust/level_heterogeneity/tables/micom_results.csv
- 09_robust/level_heterogeneity/tables/micom_step3_Group_1_vs_2.csv
- 09_robust/nonlinear/f2_quadratic_effects.csv
- 09_robust/nonlinear/nonlinear_quadratic_results.csv
- 09_robust/sensitivity_data/com6_diagnostics.csv
- 09_robust/sensitivity_data/com6_sensitivity_paths.csv
- 09_robust/sensitivity_data/com6_sensitivity_r2.csv
- 09_robust/sensitivity_data/comparison_pre_post_screening.csv
- 10_report/com6_sensitivity/baseline/f_squared.csv
- 10_report/com6_sensitivity/baseline/indicator_decisions.csv
- 10_report/com6_sensitivity/baseline/path_coefficients_bootstrap.csv
- 10_report/com6_sensitivity/baseline/prediction_classification.csv
- 10_report/com6_sensitivity/baseline/r_squared.csv
- 10_report/com6_sensitivity/compare/f_squared_compare.csv
- 10_report/com6_sensitivity/compare/formative_com_compare.csv
- 10_report/com6_sensitivity/compare/instrument_com_compare.csv
- 10_report/com6_sensitivity/compare/path_coefficients_compare.csv
- 10_report/com6_sensitivity/compare/prediction_compare.csv
- 10_report/com6_sensitivity/compare/r_squared_compare.csv
- 10_report/com6_sensitivity/no_com6/f_squared.csv
- 10_report/com6_sensitivity/no_com6/indicator_decisions.csv
- 10_report/com6_sensitivity/no_com6/path_coefficients_bootstrap.csv
- 10_report/com6_sensitivity/no_com6/prediction_classification.csv
- 10_report/com6_sensitivity/no_com6/r_squared.csv
- 10_report/mediation_classification_saturated.csv
- appendix_csv_manifest.csv

## 4) CSV files excluded and reasons
- 02_clean/tech_validation_report.csv :: Tệp rỗng
- 03_qc/qc_flags_detail.csv :: Bảng mức dòng quá dài; chỉ tóm tắt trong phụ lục
- 09_robust/level_heterogeneity/tables/groups.csv :: Bảng trung gian quá dài, ưu tiên ghi trong manifest
- 10_report/main/package_versions.csv :: Tệp metadata phiên bản/ID, không phải bảng kết quả phân tích
- 10_report/main_no_com6/package_versions.csv :: Tệp metadata phiên bản/ID, không phải bảng kết quả phân tích
- 10_report/pilot/package_versions.csv :: Tệp metadata phiên bản/ID, không phải bảng kết quả phân tích
- 10_report/pilot/pilot_ids.csv :: Tệp metadata phiên bản/ID, không phải bảng kết quả phân tích

## 5) Unresolved issues or assumptions
- Encoding được thử theo nhiều bộ mã; kết quả cuối cùng ghi trong cột notes của manifest.
- Các bảng quá dài/quá rộng có thể được giới hạn dòng để đảm bảo tính đọc được trong Word; tệp gốc vẫn được đối chiếu trong manifest.

## 6) Non-modification confirmation
- Không chỉnh sửa bất kỳ file code phân tích, file cấu hình, hoặc file CSV kết quả gốc.
- Chỉ tạo mới các file phụ lục: .docx, manifest, build log và script lắp ráp Word.
