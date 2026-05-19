# Appendix V2 Build Log

Build time: 2026-05-05 11:00:08

## 1) Files inspected
- Phu_luc_ket_qua_phan_tich_du_lieu.docx
- README.md
- run_pipeline.R
- R/run_core.R
- run_main.R
- run_pilot.R
- config/main.yml
- config/pilot.yml
- R/14_report_export.R
- R/utils_tables.R
- Toàn bộ tệp CSV trong repository

## 2) CSV files included fully (Category 1)
- 02_clean/missing_data_report.csv
- 02_clean/tech_validation_report.csv
- 03_qc/qc_summary.csv
- 03_qc/sample_flow.csv
- 04_cmv/full_collinearity_vif.csv
- 04_descriptives/construct_descriptive_stats.csv
- 04_descriptives/control_var_coding.csv
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
- 06_structural/r_squared.csv
- 07_predict/plspredict_results.csv
- 07_predict/plspredict_vs_lm.csv
- 07_predict/prediction_classification.csv
- 08_complex/mediation/mediation_classification.csv
- 08_complex/moderation/simple_slopes_ETH.csv
- 08_complex/moderation/simple_slopes_TC.csv
- 09_robust/controlled_model/control_path_coefficients.csv
- 09_robust/controlled_model/indirect_effects_A_vs_B.csv
- 09_robust/controlled_model/r2_comparison_A_vs_B.csv
- 09_robust/endogeneity/copula_decisions.csv
- 09_robust/endogeneity/copula_nonnormality.csv
- 09_robust/endogeneity/copula_terms.csv
- 09_robust/endogeneity/copula_vif.csv
- 09_robust/endogeneity/distribution_check.csv
- 09_robust/level_heterogeneity/tables/clustering_k_selection.csv
- 09_robust/level_heterogeneity/tables/mga_results.csv
- 09_robust/level_heterogeneity/tables/micom_results.csv
- 09_robust/level_heterogeneity/tables/micom_step3_Group_1_vs_2.csv
- 09_robust/nonlinear/f2_quadratic_effects.csv
- 09_robust/sensitivity_data/com6_diagnostics.csv
- 09_robust/sensitivity_data/com6_sensitivity_r2.csv
- 09_robust/sensitivity_data/comparison_pre_post_screening.csv
- 10_report/com6_sensitivity/baseline/f_squared.csv
- 10_report/com6_sensitivity/baseline/indicator_decisions.csv
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
- 10_report/com6_sensitivity/no_com6/prediction_classification.csv
- 10_report/com6_sensitivity/no_com6/r_squared.csv
- appendix_csv_manifest.csv

## 3) CSV files split into multiple Word tables (Category 2)
- 06_structural/path_coefficients_bootstrap.csv
- 09_robust/controlled_model/path_comparison_A_vs_B.csv
- 09_robust/sensitivity_data/com6_sensitivity_paths.csv
- 10_report/com6_sensitivity/baseline/path_coefficients_bootstrap.csv
- 10_report/com6_sensitivity/no_com6/path_coefficients_bootstrap.csv

## 4) CSV files summarized only (Category 3)
- 03_qc/qc_flags_detail.csv
- 04_descriptives/correlation_matrix.csv
- 08_complex/mediation/specific_indirect_effects.csv
- 08_complex/moderation/interaction_coefficients.csv
- 09_robust/endogeneity/copula_results.csv
- 09_robust/level_heterogeneity/tables/groups.csv
- 09_robust/nonlinear/nonlinear_quadratic_results.csv
- 10_report/mediation_classification_saturated.csv

## 5) CSV files excluded from Word (Category 4)
- 10_report/main/package_versions.csv
- 10_report/main_no_com6/package_versions.csv
- 10_report/pilot/package_versions.csv
- 10_report/pilot/pilot_ids.csv

## 6) Formatting checks performed
- Áp dụng khổ A4, lề trên 3.5 cm, dưới 3.0 cm, trái 3.5 cm, phải 2.5 cm.
- Chuẩn hóa font Times New Roman; cỡ chữ nội dung 13pt; bảng 10pt/9pt.
- Chuẩn hóa đánh số bảng theo hệ PL.A.1 ... PL.I.n.
- Bảng rộng được tách khối cột logic hoặc chuyển sang tóm tắt.
- Chỉ dùng landscape khi cần thiết cho độ rộng bảng.
- Dòng nguồn dưới mỗi bảng luôn ghi đường dẫn CSV tương đối.

## 7) Non-modification confirmation
- Không chỉnh sửa bất kỳ script phân tích hoặc tệp CSV kết quả gốc.
- Không chạy lại pipeline phân tích thống kê.
- Chỉ tạo mới file DOCX V2, manifest V2, build log V2 và script lắp ráp.

## 8) Unresolved assumptions
- Việc kiểm tra tràn lề được kiểm soát bằng quy tắc phân loại full/split/summary và fit-to-width; các bảng quá rộng không được đưa nguyên khối vào Word.
- Các tệp ở mức dòng quan sát được ưu tiên tóm tắt để phù hợp tài liệu in.
