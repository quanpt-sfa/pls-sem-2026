# ==============================================================================
# run_pilot.R — PILOT STUDY entrypoint
# ==============================================================================
# Chạy pipeline PLS-SEM cho giai đoạn PILOT:
#   - Auto-drop optimizer bật (tinh chỉnh instrument)
#   - Tạo instrument_locked.json khi hoàn thành
#   - Tắt PLSpredict, mediation, moderation, robustness
#
# Cách dùng:
#   Rscript run_pilot.R
#   hoặc source("run_pilot.R") trong RStudio
# ==============================================================================

source("R/run_core.R")
run_stage("config/pilot.yml")
