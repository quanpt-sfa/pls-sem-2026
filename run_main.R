# ==============================================================================
# run_main.R — MAIN STUDY entrypoint
# ==============================================================================
# Chạy pipeline PLS-SEM cho giai đoạn MAIN STUDY:
#   - Đọc instrument_locked.json từ pilot → đóng băng measurement instrument
#   - Auto-drop optimizer tắt (dùng indicator đã locked)
#   - Bật đầy đủ: PLSpredict, mediation, moderation, robustness
#
# QUAN TRỌNG: Phải chạy run_pilot.R trước để tạo instrument lock.
#
# Cách dùng:
#   Rscript run_main.R
#   hoặc source("run_main.R") trong RStudio
# ==============================================================================

source("R/run_core.R")
run_stage("config/main.yml")
