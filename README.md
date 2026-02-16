# PLS-SEM theo khung CCA (Confirmatory Composite Analysis)
## Pipeline phân tích cho nghiên cứu Chất lượng Kiểm toán

> **Nghiên cứu:** Các nhân tố ảnh hưởng đến Chất lượng Kiểm toán (Audit Quality)
> thông qua Xét đoán Kiểm toán (Audit Judgment), có điều tiết bởi Sự phức tạp
> nhiệm vụ (TC) và Đạo đức nghề nghiệp (ETH).

---

## 📐 Mô hình nghiên cứu

```
Eq(1):  AJ = f(COM, PS, MO, CG, TP, IT, CEV)
Eq(2):  AQ = f(AJ)
Eq(3):  AQ = f(AJ, AJ×TC, AJ×ETH)
```

| Vai trò | Biến | Loại đo lường |
|---|---|---|
| **Biến độc lập** | COM, MO, CG | Formative |
| **Biến độc lập** | PS, TP, IT, CEV | Reflective |
| **Biến trung gian** | AJ (Audit Judgment) | Reflective |
| **Biến phụ thuộc** | AQ (Audit Quality) | Formative |
| **Biến điều tiết** | TC (Task Complexity) | Formative |
| **Biến điều tiết** | ETH (Professional Ethics) | Formative |

---

## 📁 Cấu trúc thư mục

```
C4/
│
├── 📄 data.xlsx                  ← Dữ liệu gốc (sheet "Data" + "Code_book")
├── 📄 README.md                  ← File hướng dẫn này
├── 📄 run_pilot.R                ← Entrypoint: PILOT STUDY
├── 📄 run_main.R                 ← Entrypoint: MAIN STUDY
│
├── config/                       ← Cấu hình theo giai đoạn
│   ├── pilot.yml                 ← Config pilot: auto-drop ON, tạo instrument lock
│   └── main.yml                  ← Config main: instrument frozen, chạy đầy đủ
│
├── 00_meta/                      ← Cấu hình & metadata
│   └── analysis_config.yaml      ← CẤU HÌNH CHÍNH — mọi ngưỡng, mô hình, thang đo
│
├── 01_raw/                       ← Snapshot dữ liệu gốc
│   └── raw_data.rds              ← (tự động) Bản sao dữ liệu ngay sau import
│
├── 02_clean/                     ← Dữ liệu qua các bước làm sạch
│   ├── clean_tech.rds            ← Sau Step 1 — kiểm tra kỹ thuật
│   ├── clean_qc_flagged.rds      ← Sau Step 2 — có cờ QC, CHƯA loại
│   ├── clean_qc.rds              ← Sau Step 2 — ĐÃ loại mẫu đáng ngờ
│   ├── clean_final.rds           ← Sau Step 3 — xử lý thiếu xong
│   ├── analysis_ready.rds        ← Dataset CUỐI CÙNG dùng cho PLS-SEM
│   ├── tech_validation_report.csv
│   └── missing_data_report.csv
│
├── 03_qc/                        ← Kết quả sàng lọc hành vi
│   ├── qc_flags_detail.csv       ← Chi tiết cờ từng quan sát
│   ├── qc_summary.csv            ← Tổng hợp: bao nhiêu loại, vì sao
│   └── screening_funnel.png      ← Biểu đồ phễu sàng lọc
│
├── 04_cmv/                       ← Kiểm soát sai lệch phương pháp chung
│   ├── full_collinearity_vif.csv ← Full collinearity VIF (Kock, 2015) — TEST CHÍNH
│   └── harman_reference.txt      ← Harman test — tham khảo phụ, KHÔNG dùng kết luận
│
├── 04_descriptives/              ← (tự động tạo) Thống kê mô tả
│   ├── descriptive_stats.csv     ← M, SD, Skewness, Kurtosis từng indicator
│   ├── correlation_matrix.csv    ← Ma trận tương quan
│   ├── sample_demographics.csv   ← Mô tả mẫu nhân khẩu
│   └── distribution_plots.pdf    ← Histogram phân phối theo construct
│
├── 05_measurement/               ← Đánh giá mô hình đo lường (CCA)
│   ├── reflective/               ← Cấu trúc phản xạ (PS, TP, IT, CEV, AJ)
│   │   ├── outer_loadings.csv
│   │   ├── indicator_decisions.csv  ← Quyết định KEEP/REVIEW/DROP + lý do
│   │   ├── reliability_table.csv    ← Alpha, ρA, CR, AVE
│   │   ├── htmt_matrix.csv          ← HTMT (giá trị phân biệt)
│   │   └── htmt_ci_bootstrap.csv    ← Khoảng tin cậy bootstrap HTMT
│   │
│   ├── formative/                ← Cấu trúc tạo thành (COM, MO, CG, ETH, TC, AQ)
│   │   ├── outer_vif.csv            ← VIF giữa indicators
│   │   ├── outer_weights_bootstrap.csv
│   │   ├── outer_loadings_formative.csv
│   │   ├── indicator_decisions.csv  ← Cây quyết định: weight sig? → loading? → content?
│   │   └── nomological_validity.csv
│   │
│   └── hoc/                      ← Cấu trúc bậc cao (nếu có)
│
├── 06_structural/                ← Mô hình cấu trúc — giải thích trong mẫu
│   ├── pls_model.rds             ← Object mô hình PLS (seminr)
│   ├── boot_model.rds            ← Object bootstrap model
│   ├── inner_vif.csv             ← Đa cộng tuyến giữa predictors
│   ├── path_coefficients_bootstrap.csv ← Hệ số đường dẫn + CI
│   ├── r_squared.csv             ← R² cho AJ, AQ
│   ├── f_squared.csv             ← Kích thước tác động f²
│   └── srmr_reference.csv        ← SRMR — tham khảo, KHÔNG phải fit index
│
├── 07_predict/                   ← PLSpredict — dự báo ngoài mẫu
│   ├── plspredict_results.csv    ← Q²_predict, RMSE, MAE
│   ├── plspredict_vs_lm.csv     ← So sánh PLS vs Linear Model benchmark
│   └── prediction_classification.csv ← Phân loại: HIGH / MEDIUM / LOW
│
├── 08_complex/                   ← Giả thuyết phức hợp
│   ├── mediation/
│   │   ├── specific_indirect_effects.csv ← Tác động gián tiếp + CI
│   │   └── mediation_classification.csv  ← Nitzl et al.: full/complementary/competitive
│   │
│   └── moderation/
│       ├── pls_moderation_model.rds
│       ├── boot_moderation_model.rds
│       ├── interaction_coefficients.csv ← Beta, t, CI cho AJ×TC, AJ×ETH
│       ├── simple_slopes_TC.csv
│       ├── simple_slopes_ETH.csv
│       └── moderation_plots/
│           ├── mod_AJ_x_TC.png          ← Đồ thị simple slopes TC
│           └── mod_AJ_x_ETH.png         ← Đồ thị simple slopes ETH
│
├── 09_robust/                    ← Kiểm tra độ vững / độ nhạy
│   ├── sensitivity_data/
│   │   ├── comparison_pre_post_screening.csv  ← So sánh path trước/sau sàng lọc
│   │   └── sensitivity_conclusion.md
│   ├── micom_mga/
│   │   └── micom_status.md       ← MICOM-MGA (điều kiện, steps)
│   ├── endogeneity/
│   │   ├── distribution_check.csv
│   │   └── endogeneity_conclusion.md
│   └── fimix/                    ← FIMIX-PLS (cần SmartPLS)
│
├── 10_report/                    ← Output cuối — bảng/hình cho luận án
│   ├── pilot/                    ← Output pilot stage (Phase 1-2 only)
│   │   ├── instrument_locked.json  ← Đóng băng instrument → main đọc file này
│   │   ├── pilot_summary.md        ← Tóm tắt pilot: sample, items removed
│   │   ├── config_snapshot_*.yml   ← Audit trail
│   │   ├── session_info.txt
│   │   └── package_versions.csv
│   ├── main/                     ← Output main stage (full pipeline)
│   │   ├── all_tables.docx         ← Bảng main (luận án)
│   │   └── figures/
│   ├── all_tables.docx           ← (legacy) TẤT CẢ bảng trong 1 file Word
│   └── figures/                  ← Copy các hình từ các bước
│
├── R/                            ← MÃ NGUỒN — 18 R scripts
│   └── (xem bảng chi tiết bên dưới)
│
└── logs/                         ← Nhật ký chạy pipeline
    ├── pilot_YYYYMMDD_HHMMSS.log   ← Log pilot stage
    ├── main_YYYYMMDD_HHMMSS.log    ← Log main stage
    └── pipeline_YYYYMMDD_HHMMSS.log ← Log legacy mode
```

---

## 📜 Danh sách R Scripts (`R/`)

Pipeline chạy tuần tự qua 4 giai đoạn (phase), mỗi giai đoạn gồm nhiều bước (step).
Tất cả được điều phối bởi `run_pipeline.R`.

### Hạ tầng

| Script | Mục đích |
|---|---|
| `00_config.R` | Đọc `analysis_config.yaml`, validate, tạo helper functions |
| `utils_logging.R` | Ghi nhật ký (console + file), tracking gate pass/fail |
| `utils_tables.R` | Tạo bảng thesis-ready bằng `flextable`, xuất Word |

### Phase 1: Chuẩn bị dữ liệu

| Step | Script | Gate | Ý đồ |
|---|---|---|---|
| 1 | `01_import_validate.R` | Đúng định dạng | Import Excel, đối chiếu codebook, ép kiểu, reverse code, loại dòng trống |
| 2 | `02_qc_behavioral.R` | Phản hồi có nhận thức | Phát hiện straight-lining, long-string, speeding, IRV thấp. Loại khi ≥ 2 cờ |
| 3 | `03_missing_data.R` | Đủ dữ liệu | Loại quan sát > 10% thiếu, impute rải rác, Little's MCAR test |
| 4 | `04_descriptives.R` | Không bất thường | M/SD/skew/kurtosis, ma trận tương quan, demographics, histograms |

### Phase 2: CMV & Đo lường (Quality Gate chính)

| Step | Script | Gate | Ý đồ |
|---|---|---|---|
| 5 | `05_cmv_assessment.R` | Không CMV quá mạnh | Full collinearity VIF < 3.3 (Kock 2015). Harman phụ trợ |
| 6 | `06_measurement_reflective.R` | Đo lường reflective OK | Loadings ≥ 0.708, ρA/CR ∈ [0.70, 0.95], AVE ≥ 0.50, HTMT < 0.85 |
| 7 | `07_measurement_formative.R` | Composite ổn định | Outer VIF < 5 (tham chiếu), ưu tiên < 3 (lý tưởng), weights bootstrap, cây quyết định |
| 8 | `08_hoc_assessment.R` | Không trộn tiêu chí | Bậc cao two-stage (nếu có) |

### Phase 3: Cấu trúc & Giả thuyết

| Step | Script | Gate | Ý đồ |
|---|---|---|---|
| 9 | `09_structural_insample.R` | Không cộng tuyến | Inner VIF, path coefficients + CI, R², f², SRMR (tham khảo) |
| 10 | `10_plspredict.R` | Có sức mạnh dự báo | PLSpredict k=10 folds, so sánh RMSE/MAE với LM benchmark |
| 11a | `11_mediation.R` | — | 7 indirect effects: X → AJ → AQ. Phân loại Nitzl et al. (2016) |
| 11b | `12_moderation.R` | — | AJ×TC → AQ, AJ×ETH → AQ. Simple slopes + đồ thị điều tiết |

### Phase 4: Độ vững & Báo cáo

| Step | Script | Gate | Ý đồ |
|---|---|---|---|
| 12 | `13_robustness.R` | Kết luận ổn định | So sánh pre/post screening, MICOM-MGA, endogeneity, FIMIX |
| 13 | `14_report_export.R` | — | Xuất tất cả bảng → Word, copy figures → `10_report/` |

### Master

| Script | Mục đích |
|---|---|
| **`run_pipeline.R`** | Tương thích ngược — nếu `config/main.yml` tồn tại → gọi `run_core.R` |
| **`run_core.R`** | **Lõi pipeline stage-aware.** Hàm `run_stage()` dùng chung cho pilot & main |
| `run_pilot.R` | Entrypoint pilot: `run_stage("config/pilot.yml")` |
| `run_main.R` | Entrypoint main: `run_stage("config/main.yml")` |

---

## 🔧 File cấu hình chính: `analysis_config.yaml`

File này nằm ở `00_meta/analysis_config.yaml` và chứa **tất cả** quyết định phân tích,
được khóa **trước** khi chạy mô hình:

| Mục | Nội dung |
|---|---|
| `project` | Tên, seed (42), bootstrap (5000, sensitivity: 10000), alpha (0.05) |
| `constructs` | 11 biến: tên, loại đo lường (reflective/formative), danh sách indicators |
| `structural_paths` | 10 đường: 7 IVs→AJ, AJ→AQ, TC→AQ, ETH→AQ |
| `mediation` | 7 tác động gián tiếp: X → AJ → AQ |
| `moderation` | 2 tương tác: AJ×TC→AQ, AJ×ETH→AQ (two-stage) |
| `screening` | Ngưỡng sàng lọc: missing 10%, speeding P5, straight-line 90% |
| `thresholds_reflective` | Loadings ≥ 0.708, AVE ≥ 0.50, HTMT < 0.85 |
| `thresholds_formative` | VIF < 5 (tham chiếu), ưu tiên < 3 (lý tưởng); loading keep ≥ 0.50 |
| `thresholds_structural` | Inner VIF < 5, f² Cohen benchmarks |
| `cmv` | Full collinearity VIF < 3.3 |
| `plspredict` | k=10 folds, 10 repetitions |

> ⚠️ **Nguyên tắc:** Sửa file này TRƯỚC KHI chạy pipeline. Không sửa sau khi đã thấy kết quả.
>
> **Kiểm tra độ nhạy bootstrap:** Nếu kết quả nhạy (p-value gần ngưỡng 0.05), config đã khóa sẵn
> `bootstrap_sensitivity: 10000` để chạy kiểm chứng với 10.000 mẫu.

---

## 🚀 Cách chạy

### Yêu cầu

- **R ≥ 4.1.0**
- Packages bắt buộc:

```r
install.packages(c("yaml", "readxl", "dplyr", "tidyr", "stringr",
                    "seminr", "ggplot2", "flextable", "officer", "jsonlite"))

# Tùy chọn (cho Little's MCAR test):
install.packages("naniar")
```

### Chế độ Pilot / Main (khuyến nghị)

Pipeline hỗ trợ hai giai đoạn phân tích riêng biệt:

| Giai đoạn | Entrypoint | Config | Mục đích |
|---|---|---|---|
| **PILOT** | `run_pilot.R` | `config/pilot.yml` | Lấy 108 dòng đầu, chỉ chạy Phase 1-2 (data prep + CCA đo lường), tạo `instrument_locked.json` |
| **MAIN** | `run_main.R` | `config/main.yml` | Phân tích đầy đủ: đóng băng instrument, chạy tất cả module (structural, mediation, PLSpredict…) |

```bash
# Bước 1: Chạy pilot (lấy 108 dòng đầu, chỉ Phase 1-2)
Rscript run_pilot.R

# → Output: 10_report/pilot/
# → Tạo:   10_report/pilot/instrument_locked.json
# → Tạo:   10_report/pilot/pilot_summary.md
# → KHÔNG chạy structural/mediation/moderation/robustness/report export

# Bước 2: Cập nhật data_file trong config/main.yml → dữ liệu main
# Bước 3: Chạy main study (full pipeline)
Rscript run_main.R

# → Output: 10_report/main/
# → Đọc instrument lock từ pilot → đóng băng measurement instrument
```

**Instrument Lock:** Sau khi pilot hoàn thành Phase 2 (CCA đo lường), file `instrument_locked.json` ghi lại:
- Danh sách indicators active/removed cho mỗi construct
- Lý do từng item bị loại (loading, iteration)
- Timestamp, git commit, R version

> **Lưu ý:** Pilot chỉ dùng 108 dòng đầu và dừng ở cuối Phase 2 (CMV + measurement).
> Mục đích duy nhất là tinh chỉnh instrument. KHÔNG diễn giải giả thuyết từ kết quả pilot.

Main study **bắt buộc** phải có instrument lock. Nếu không tìm thấy,
pipeline sẽ dừng và yêu cầu chạy pilot trước.

**Freeze Rules:** Trong `config/main.yml`, bạn có thể thêm `freeze_rules` để
ghi đè quyết định pilot (ví dụ: giữ lại một item đã bị loại, hoặc loại thêm item).

### Chế độ Legacy (tương thích ngược)

File `run_pipeline.R` gốc vẫn hoạt động:
- Nếu `config/main.yml` tồn tại → tự động chuyển sang chế độ main stage
- Nếu không → chạy pipeline gốc (không phân chia pilot/main)

```r
# Cách 1: Trong RStudio
setwd("d:/Works/Data analysis/C4")
source("R/run_pipeline.R")

# Cách 2: Từ terminal
cd "d:\Works\Data analysis\C4"
Rscript R/run_pipeline.R
```

### Kết quả

- Bảng Word: `10_report/all_tables.docx`
- Hình: `10_report/figures/`
- Log: `logs/pipeline_*.log`
- Dữ liệu phân tích: `02_clean/analysis_ready.rds`
- Mô hình PLS: `06_structural/pls_model.rds`

---

## 📚 Tài liệu tham chiếu

| Tài liệu | Vai trò |
|---|---|
| Hair, J.F. et al. (2022). *A Primer on PLS-SEM*, 3rd ed. | Khung PLS-SEM/CCA chính |
| Sarstedt, M. et al. (2022). *Advanced Issues in PLS-SEM* | Mở rộng nâng cao |
| Shmueli, G. et al. (2019). | PLSpredict — dự báo ngoài mẫu |
| Kock, N. (2015). | Full collinearity VIF cho CMV |
| Nitzl, C. et al. (2016). | Phân loại trung gian |
| Dillman, D.A. et al. (2014). | Thiết kế khảo sát |
| Cohen, J. (1988). | Ngưỡng effect size f² |
