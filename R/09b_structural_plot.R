# ==============================================================================
# 09b_structural_plot.R  —  Sơ đồ mô hình PLS-SEM (Path Diagram)
# Phiên bản 2.0  |  Tác giả: được tạo tự động bởi pipeline C4
#
# Cấu trúc module (phân lớp rõ ràng):
#   Layer 1 — model_parser   : đọc dữ liệu mô hình (config + CSV)
#   Layer 2 — layout_engine  : tính toán vị trí x/y của mọi phần tử
#   Layer 3 — style_config   : màu sắc, font, đường nét
#   Layer 4 — renderer       : vẽ từng lớp (indicators → paths → constructs → legend)
#   API     — plot_structural_model / run_structural_plot
#
# Hai phiên bản hình:
#   "PLS_RESULT"        → Chương 4 (beta, R², significance, reflective/formative)
#   "THEORETICAL_CLEAN" → Chương 3 (giả thuyết, tùy chọn residuals, tương tác tường minh)
#
# Tham số cấu hình:
#   diagram_mode       : "PLS_RESULT" | "THEORETICAL_CLEAN"
#   moderation_style   : "software" | "interaction_construct"
#   show_residuals     : TRUE | FALSE
#   compact_outer      : TRUE | FALSE (rút gọn indicator labels)
#   show_stats_labels  : TRUE | FALSE (hiện beta/p/R²)
#   export_formats     : vector gồm "png", "pdf", "svg"
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tibble)
})

# ==============================================================================
# LAYER 1 — MODEL PARSER
# Đọc cấu trúc mô hình từ config + file CSV kết quả
# ==============================================================================

#' Đọc path coefficients từ CSV (hoặc từ seminr model)
mp_get_paths <- function(pls_model = NULL, boot_model = NULL) {
  path_file <- "06_structural/path_coefficients_bootstrap.csv"
  if (file.exists(path_file)) {
    df        <- read.csv(path_file, stringsAsFactors = FALSE)
    names(df) <- tolower(trimws(names(df)))

    pick_num <- function(alts) {
      hit <- intersect(alts, names(df))
      if (length(hit)) as.numeric(df[[hit[1]]]) else NA_real_
    }
    pick_chr <- function(alts) {
      hit <- intersect(alts, names(df))
      if (length(hit)) as.character(df[[hit[1]]]) else NA_character_
    }

    has_from <- any(c("from","iv","predictor","x") %in% names(df))
    has_to   <- any(c("to","dv","criterion","y")   %in% names(df))

    if (has_from && has_to) {
      from_vec <- pick_chr(c("from","iv","predictor","x"))
      to_vec   <- pick_chr(c("to","dv","criterion","y"))
    } else {
      pc <- intersect(c("path","relation","relationship"), names(df))
      if (!length(pc)) stop("[mp_get_paths] Khong tim thay cot from/to hoac path trong CSV")
      pts      <- strsplit(df[[pc[1]]], "\\s*->\\s*")
      from_vec <- trimws(sapply(pts, `[`, 1))
      to_vec   <- trimws(sapply(pts, `[`, 2))
    }

    res <- data.frame(
      from     = from_vec, to = to_vec,
      coef     = pick_num(c("beta","coef","estimate","path_coef","original")),
      p_value  = pick_num(c("p_value","p.value","pval","p")),
      ci_lower = pick_num(c("ci_low","ci_lower","ci.lower","lower_2.5%","lower")),
      ci_upper = pick_num(c("ci_high","ci_upper","ci.upper","upper_97.5%","upper")),
      stringsAsFactors = FALSE
    )
    res$sig_stars <- mp_sig_stars(res$p_value, res$ci_lower, res$ci_upper)
    return(res)
  }
  message("[mp_get_paths] Khong co CSV — skeleton tu config")
  return(NULL)
}

#' Đọc R² từ CSV
mp_get_r2 <- function() {
  f <- "06_structural/r_squared.csv"
  if (!file.exists(f)) return(data.frame(construct=character(), r2=numeric()))
  df        <- read.csv(f, stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))
  r2c  <- intersect(c("r2","r_squared","r.squared","rsquared"), names(df))
  conc <- intersect(c("construct","dv","endogenous","variable"), names(df))
  if (!length(r2c) || !length(conc)) return(data.frame(construct=character(), r2=numeric()))
  data.frame(construct=df[[conc[1]]], r2=as.numeric(df[[r2c[1]]]), stringsAsFactors=FALSE)
}

#' Đọc active indicators từ RDS hoặc fallback từ config
mp_get_indicators <- function(cfg) {
  rds <- "06_structural/active_indicators.rds"
  if (file.exists(rds)) {
    ai <- tryCatch(readRDS(rds), error=function(e) NULL)
    if (is.list(ai) && length(ai) > 0) return(ai)
  }
  setNames(lapply(cfg$constructs, `[[`, "indicators"),
           sapply(cfg$constructs, `[[`, "name"))
}

#' Significant stars (ưu tiên CI > p-value)
mp_sig_stars <- function(pv, cil = NA, ciu = NA) {
  if (!all(is.na(cil)) && !all(is.na(ciu))) {
    sig_ci <- !(cil < 0 & ciu > 0)
    return(dplyr::case_when(
      !sig_ci                        ~ "ns",
      !is.na(pv) & pv < 0.001       ~ "***",
      !is.na(pv) & pv < 0.01        ~ "**",
      !is.na(pv) & pv < 0.05        ~ "*",
      sig_ci                         ~ "*",
      TRUE                           ~ "ns"
    ))
  }
  dplyr::case_when(
    is.na(pv)   ~ "ns",
    pv < 0.001  ~ "***",
    pv < 0.01   ~ "**",
    pv < 0.05   ~ "*",
    TRUE        ~ "ns"
  )
}

#' Tổng hợp toàn bộ model data
mp_build_model <- function(cfg, pls_model = NULL, boot_model = NULL) {
  full_names <- setNames(sapply(cfg$constructs, `[[`, "full_name"),
                         sapply(cfg$constructs, `[[`, "name"))
  meas_types <- setNames(sapply(cfg$constructs, `[[`, "measurement_type"),
                         sapply(cfg$constructs, `[[`, "name"))

  paths_data <- mp_get_paths(pls_model, boot_model)
  if (is.null(paths_data)) {
    paths_data <- data.frame(
      from      = sapply(cfg$structural_paths, `[[`, "from"),
      to        = sapply(cfg$structural_paths, `[[`, "to"),
      coef      = NA_real_, p_value  = NA_real_,
      ci_lower  = NA_real_, ci_upper = NA_real_, sig_stars = "ns",
      stringsAsFactors = FALSE
    )
  }

  list(
    constructs  = sapply(cfg$constructs, `[[`, "name"),
    full_names  = full_names,
    meas_types  = meas_types,
    struct_paths= cfg$structural_paths,
    paths_data  = paths_data,
    r2          = mp_get_r2(),
    indicators  = mp_get_indicators(cfg),
    moderation  = cfg$moderation,
    mediation   = cfg$mediation
  )
}

# ==============================================================================
# LAYER 2 — LAYOUT ENGINE
# Tính toán vị trí x/y cho tất cả phần tử: constructs, indicators, residuals
# ==============================================================================

#' Layout chính cho constructs
#' ETH phía TRÊN path AJ→AQ, TC phía DƯỚI
le_construct_layout <- function(model, moderation_style = "software") {

  # --- Inner model positions ---
  iv_names <- c("COM","PS","MO","TP","IT","CC","CG")
  n_iv     <- length(iv_names)
  y_iv     <- seq(7.5, 7.5 - (n_iv-1)*1.55, length.out = n_iv)

  base <- tibble::tribble(
    ~construct,  ~x,    ~y,    ~role,
    "AJ",        7.5,   3.0,   "mediator",
    "AQ",       12.0,   3.0,   "dv"
  )

  iv_df <- data.frame(
    construct = iv_names,
    x         = 2.5,
    y         = y_iv,
    role      = "iv",
    stringsAsFactors = FALSE
  )

  # --- Moderators ---
  # ETH phía TRÊN (y > 3.0 = đường AJ→AQ), TC phía DƯỚI (y < 3.0)
  mid_x <- (7.5 + 12.0) / 2  # 9.75

  if (moderation_style == "interaction_construct") {
    # Thêm interaction construct nodes: AJ×ETH và AJ×TC
    mod_df <- data.frame(
      construct = c("ETH","TC","AJ*ETH","AJ*TC"),
      x         = c(7.5,  7.5,  10.5,   10.5),
      y         = c(6.5,  -1.0,  5.5,   -0.5),
      role      = c("moderator","moderator","interaction","interaction"),
      stringsAsFactors = FALSE
    )
  } else {
    mod_df <- data.frame(
      construct = c("ETH","TC"),
      x         = c(mid_x, mid_x),
      y         = c(5.5,   0.2),    # ETH trên cao, TC dưới thấp
      role      = c("moderator","moderator"),
      stringsAsFactors = FALSE
    )
  }

  nodes <- rbind(iv_df, as.data.frame(base), mod_df)

  # Chỉ giữ construct có trong model
  nodes <- nodes %>% filter(construct %in% c(model$constructs,
                                               grep("\\*",nodes$construct,value=TRUE)))
  nodes
}

#' Tính vị trí indicators theo layout non-overlap
le_indicator_layout <- function(nodes, model, hw_nd, hh_nd,
                                 hw_in = 0.40, hh_in = 0.17,
                                 gap   = 0.12) {
  active_inds <- model$indicators
  ind_rows    <- list()
  spacing     <- 0.30

  # ---- IVs: indicators BÊN TRÁI, x cố định, y sequential top-down ----
  iv_nds   <- nodes %>% filter(role == "iv") %>% arrange(desc(y))
  x_iv_ind <- if (nrow(iv_nds) > 0) iv_nds$x[1] - hw_nd - gap - hw_in else -0.5
  y_cursor <- Inf

  for (i in seq_len(nrow(iv_nds))) {
    nd   <- iv_nds[i,]
    inds <- active_inds[[nd$construct]]
    if (is.null(inds) || !length(inds)) next
    n    <- length(inds)
    span <- (n-1) * spacing
    y_top_ideal <- nd$y + span/2
    if (y_top_ideal > y_cursor - gap) y_top_ideal <- y_cursor - gap
    y_bot    <- y_top_ideal - span
    y_cursor <- y_bot - gap

    ind_rows[[length(ind_rows)+1]] <- data.frame(
      construct = nd$construct, ind_name = inds,
      x_ind = x_iv_ind, y_ind = seq(y_top_ideal, y_bot, length.out=n),
      direction = "left", stringsAsFactors = FALSE
    )
  }

  # ---- AQ: indicators BÊN PHẢI ----
  aq_nd   <- nodes %>% filter(construct == "AQ")
  aq_inds <- active_inds[["AQ"]]
  if (nrow(aq_nd)>0 && !is.null(aq_inds) && length(aq_inds)>0) {
    n        <- length(aq_inds)
    x_aq_ind <- aq_nd$x + hw_nd + gap + hw_in
    span     <- (n-1)*spacing
    ind_rows[[length(ind_rows)+1]] <- data.frame(
      construct="AQ", ind_name=aq_inds,
      x_ind=x_aq_ind,
      y_ind=seq(aq_nd$y+span/2, aq_nd$y-span/2, length.out=n),
      direction="right", stringsAsFactors=FALSE
    )
  }

  # ---- AJ: indicators PHÍA TRÊN ----
  aj_nd   <- nodes %>% filter(construct == "AJ")
  aj_inds <- active_inds[["AJ"]]
  if (nrow(aj_nd)>0 && !is.null(aj_inds) && length(aj_inds)>0) {
    n       <- length(aj_inds)
    y_above <- aj_nd$y + hh_nd + gap + hh_in
    xs      <- seq(aj_nd$x - (n-1)/2*0.95, aj_nd$x + (n-1)/2*0.95, length.out=n)
    ind_rows[[length(ind_rows)+1]] <- data.frame(
      construct="AJ", ind_name=aj_inds,
      x_ind=xs, y_ind=y_above, direction="up", stringsAsFactors=FALSE
    )
  }

  # ---- ETH (moderator phía trên): indicators PHÍA TRÊN ETH ----
  eth_nd   <- nodes %>% filter(construct == "ETH")
  eth_inds <- active_inds[["ETH"]]
  if (nrow(eth_nd)>0 && !is.null(eth_inds) && length(eth_inds)>0) {
    n       <- length(eth_inds)
    y_above <- eth_nd$y + hh_nd + gap + hh_in
    xs      <- seq(eth_nd$x - (n-1)/2*0.90, eth_nd$x + (n-1)/2*0.90, length.out=n)
    ind_rows[[length(ind_rows)+1]] <- data.frame(
      construct="ETH", ind_name=eth_inds,
      x_ind=xs, y_ind=y_above, direction="up", stringsAsFactors=FALSE
    )
  }

  # ---- TC (moderator phía dưới): indicators PHÍA DƯỚI TC ----
  tc_nd   <- nodes %>% filter(construct == "TC")
  tc_inds <- active_inds[["TC"]]
  if (nrow(tc_nd)>0 && !is.null(tc_inds) && length(tc_inds)>0) {
    n       <- length(tc_inds)
    y_below <- tc_nd$y - hh_nd - gap - hh_in
    xs      <- seq(tc_nd$x - (n-1)/2*0.90, tc_nd$x + (n-1)/2*0.90, length.out=n)
    ind_rows[[length(ind_rows)+1]] <- data.frame(
      construct="TC", ind_name=tc_inds,
      x_ind=xs, y_ind=y_below, direction="down", stringsAsFactors=FALSE
    )
  }

  if (!length(ind_rows)) return(data.frame())
  do.call(rbind, ind_rows)
}

# ==============================================================================
# LAYER 3 — STYLE CONFIG
# Màu sắc, font, đường nét — tối ưu in đen trắng và màu
# ==============================================================================

# Tên tiếng Việt (ASCII) cho các construct — override full_name từ config
VN_CONSTRUCT_NAMES <- c(
  COM = "Năng lực chuyên môn",
  PS  = "Hoài nghi nghề nghiệp",
  MO  = "Động lực",
  TP  = "Áp lực quỹ thời gian",
  IT  = "Sử dụng công nghệ thông tin",
  CC = "Văn hóa doanh nghiệp",
  CG  = "Quản trị doanh nghiệp",
  ETH = "Đạo đức nghề nghiệp",
  TC  = "Độ phức tạp nghề nghiệp",
  AJ  = "Xét đoán kiểm toán",
  AQ  = "Chất lượng kiểm toán"
)

sc_theme <- function(diagram_mode = "PLS_RESULT") {
  list(
    # Construct box dimensions (enlarged for legibility)
    hw = 1.30,   # half-width
    hh = 0.60,   # half-height
    hw_in = 0.58, hh_in = 0.26, # indicator box

    # Construct fills (màu nhạt để in grayscale vẫn phân biệt được)
    fill = c(
      iv          = "#EBF5FB",
      mediator    = "#EAFAF1",
      dv          = "#FDEDEC",
      moderator   = "#FEFDE7",
      interaction = "#F5EEF8"
    ),
    # Construct border colors
    border = c(
      iv          = "#1A5276",
      mediator    = "#1E8449",
      dv          = "#922B21",
      moderator   = "#7D6608",
      interaction = "#6C3483"
    ),
    # Path arrow colors
    arrow_sig   = "#1a1a1a",
    arrow_ns    = "#bbbbbb",
    arrow_mod   = "#7D6608",
    arrow_resid = "#888888",
    # Line widths
    lwd_main    = 0.75,
    lwd_ns      = 0.45,
    lwd_ind     = 0.45,
    lwd_mod     = 0.60,
    # Font sizes (pt) — construct & indicator x2
    size_construct_code = 6.4,
    size_construct_name = 5.2,
    size_indicator      = 4.0,
    size_path_label     = 3.2,
    size_r2             = 4.6,
    size_legend         = 3.6,
    size_residual       = 4.4,
    # Curve factor for moderator arrows
    curv_mod    = 0.28
  )
}

# ==============================================================================
# LAYER 4 — GEOMETRY HELPERS
# ==============================================================================

#' Điểm tại rìa hình chữ nhật (từ tâm hướng về phía đích)
ge_edge <- function(x0, y0, x1, y1, hw, hh) {
  dx <- x1-x0; dy <- y1-y0
  nm <- sqrt(dx^2+dy^2)
  if (nm < 1e-9) return(c(x0, y0))
  ux <- dx/nm; uy <- dy/nm
  t  <- min(if(abs(ux)>1e-9) hw/abs(ux) else Inf,
            if(abs(uy)>1e-9) hh/abs(uy) else Inf)
  c(x0+ux*t, y0+uy*t)
}

#' Tính tất cả edge points từ data.frame các segments
ge_compute_segments <- function(seg_df, hw, hh) {
  if (nrow(seg_df) == 0) {
    seg_df$xs <- seg_df$ys <- seg_df$xe <- seg_df$ye <- numeric(0)
    return(seg_df)
  }
  epf <- mapply(ge_edge, seg_df$x_from, seg_df$y_from,
                         seg_df$x_to,   seg_df$y_to,
                         MoreArgs=list(hw=hw, hh=hh))
  ept <- mapply(ge_edge, seg_df$x_to,   seg_df$y_to,
                         seg_df$x_from, seg_df$y_from,
                         MoreArgs=list(hw=hw, hh=hh))
  seg_df$xs <- epf[1,]; seg_df$ys <- epf[2,]
  seg_df$xe <- ept[1,]; seg_df$ye <- ept[2,]
  seg_df
}

# ==============================================================================
# LAYER 4 — RENDERER PRIMITIVES
# ==============================================================================

#' Vẽ lớp indicators (outer model)
rd_outer_model <- function(p, nodes, ind_df, model, theme,
                            compact_outer = FALSE) {
  if (nrow(ind_df) == 0) return(p)

  meas_types <- model$meas_types
  hw    <- theme$hw; hh    <- theme$hh
  hw_in <- theme$hw_in; hh_in <- theme$hh_in

  ind_df$meas       <- meas_types[ind_df$construct]
  ind_df$meas       <- ifelse(is.na(ind_df$meas), "reflective", ind_df$meas)
  ind_df$border_col <- theme$border[nodes$role[match(ind_df$construct, nodes$construct)]]
  ind_df$fill_col   <- theme$fill  [nodes$role[match(ind_df$construct, nodes$construct)]]

  for (i in seq_len(nrow(ind_df))) {
    ind <- ind_df[i,]
    xi  <- ind$x_ind; yi <- ind$y_ind
    nd  <- nodes %>% filter(construct == ind$construct)
    if (!nrow(nd)) next
    xc <- nd$x; yc <- nd$y

    ep_c <- ge_edge(xc, yc, xi, yi, hw,    hh)
    ep_i <- ge_edge(xi, yi, xc, yc, hw_in, hh_in)

    # Reflective: construct → indicator; Formative: indicator → construct
    if (identical(ind$meas, "reflective")) {
      xa=ep_c[1]; ya=ep_c[2]; xb=ep_i[1]; yb=ep_i[2]
    } else {
      xa=ep_i[1]; ya=ep_i[2]; xb=ep_c[1]; yb=ep_c[2]
    }

    lbl <- if (compact_outer) {
      # Rút gọn: chỉ ký hiệu viết tắt AQ1, AQ2...
      gsub("^([A-Za-z]+)(\\d+)$", "\\1\\2", ind$ind_name)
    } else ind$ind_name

    # Kiểu đường của indicator box
    lt_box  <- if (ind$meas == "formative") "dashed" else "solid"
    lt_arr  <- "solid"

    p <- p +
      annotate("segment", x=xa, y=ya, xend=xb, yend=yb,
                color=ind$border_col, linewidth=theme$lwd_ind,
                arrow=arrow(length=unit(0.09,"cm"), type="open")) +
      annotate("rect",
                xmin=xi-hw_in, xmax=xi+hw_in,
                ymin=yi-hh_in, ymax=yi+hh_in,
                fill=ind$fill_col, color=ind$border_col,
                linewidth=0.40, linetype=lt_box) +
      annotate("text", x=xi, y=yi, label=lbl,
                size=theme$size_indicator, color=ind$border_col)
  }
  p
}

#' Vẽ mũi tên inner model (structural paths)
rd_inner_paths <- function(p, nodes, seg_df, theme,
                            show_stats = TRUE) {
  if (nrow(seg_df) == 0) return(p)

  for (i in seq_len(nrow(seg_df))) {
    r     <- seg_df[i,]
    col   <- if (!is.na(r$sig_stars) && r$sig_stars != "ns")
               theme$arrow_sig else theme$arrow_ns
    lwd   <- if (col == theme$arrow_sig) theme$lwd_main else theme$lwd_ns
    p <- p + annotate("segment",
                       x=r$xs, y=r$ys, xend=r$xe, yend=r$ye,
                       color=col, linewidth=lwd,
                       arrow=arrow(length=unit(0.18,"cm"), type="closed"))
  }

  if (show_stats && any(!is.na(seg_df$coef))) {
    lbl_df <- seg_df %>%
      filter(!is.na(coef)) %>%
      mutate(
        path_label = paste0(sprintf("%.3f", coef),
                            ifelse(!is.na(sig_stars) & sig_stars!="ns",
                                   sig_stars, "(ns)")),
        col_lbl = ifelse(!is.na(sig_stars) & sig_stars!="ns",
                         theme$arrow_sig, theme$arrow_ns),
        xm = (xs+xe)/2, ym = (ys+ye)/2
      )
    p <- p + annotate("label",
                       x=lbl_df$xm, y=lbl_df$ym,
                       label=lbl_df$path_label, size=theme$size_path_label,
                       label.padding=unit(0.10,"lines"), label.size=0.18,
                       fill="white", color=lbl_df$col_lbl)
  }
  p
}

#' Vẽ mũi tên điều tiết — chế độ "software" (đường cong chạm path)
rd_moderation_software <- function(p, nodes, paths_data, model, theme,
                                    show_stats = TRUE) {
  # ETH phía trên → cong nhẹ từ phải
  # TC  phía dưới → cong nhẹ từ phải
  # Mỗi cái nhắm vào điểm KHÁC NHAU trên path AJ→AQ
  aj_x <- nodes$x[nodes$construct=="AJ"]; aj_y <- nodes$y[nodes$construct=="AJ"]
  aq_x <- nodes$x[nodes$construct=="AQ"]; aq_y <- nodes$y[nodes$construct=="AQ"]
  if (!length(aj_x)||!length(aq_x)) return(p)

  mid_x <- (aj_x + aq_x) / 2

  mod_nds <- nodes %>% filter(role=="moderator") %>%
    arrange(desc(y))  # ETH (y cao hơn) trước

  if (!nrow(mod_nds)) return(p)

  # Offsets trên trục x của điểm target (tránh chồng nhau)
  n_mod    <- nrow(mod_nds)
  x_targets <- seq(mid_x - 0.5, mid_x + 0.5, length.out = n_mod)
  # Curvature: ETH từ trên → cong âm; TC từ dưới → cong dương
  curvs    <- ifelse(mod_nds$y > aj_y, -0.30, 0.30)

  for (i in seq_len(n_mod)) {
    mn      <- mod_nds[i,]
    x_tgt   <- x_targets[i]
    y_tgt   <- aj_y   # target là điểm trên đường AJ→AQ

    ep <- ge_edge(mn$x, mn$y, x_tgt, y_tgt, theme$hw, theme$hh)

    # Path coef TC→AQ hoặc ETH→AQ
    pc  <- paths_data %>% filter(from==mn$construct & to=="AQ")
    if (show_stats && nrow(pc)>0 && !is.na(pc$coef[1])) {
      lbl <- paste0(mn$construct, "\n",
                    sprintf("%.3f", pc$coef[1]),
                    ifelse(pc$sig_stars[1]!="ns", pc$sig_stars[1], "(ns)"))
    } else {
      lbl <- mn$construct
    }

    # Hướng label: ETH lệch trái, TC lệch phải
    lbl_xoff <- if (mn$y > aj_y) -0.8 else 0.8
    lbl_yoff <- if (mn$y > aj_y)  0.3 else -0.3

    p <- p +
      annotate("curve", x=ep[1], y=ep[2],
                xend=x_tgt, yend=y_tgt,
                curvature=curvs[i], color=theme$arrow_mod,
                linewidth=theme$lwd_mod, linetype="dashed",
                arrow=arrow(length=unit(0.14,"cm"), type="open")) +
      annotate("label",
                x=(ep[1]+x_tgt)/2 + lbl_xoff,
                y=(ep[2]+y_tgt)/2 + lbl_yoff,
                label=lbl, size=theme$size_path_label - 0.3,
                label.padding=unit(0.09,"lines"), label.size=0.12,
                fill="#FFFDE7", color=theme$arrow_mod, lineheight=0.85)
  }
  p
}

#' Vẽ tương tác tường minh — chế độ "interaction_construct"
rd_moderation_interaction <- function(p, nodes, paths_data, model, theme,
                                       show_stats = TRUE) {
  int_nds <- nodes %>% filter(role == "interaction")
  if (!nrow(int_nds)) return(p)

  hw <- theme$hw; hh <- theme$hh
  int_fill   <- theme$fill["interaction"]
  int_border <- theme$border["interaction"]

  # Vẽ interaction construct boxes và các mũi tên đầu vào/ra
  for (i in seq_len(nrow(int_nds))) {
    nd   <- int_nds[i,]
    xi   <- nd$x; yi <- nd$y

    # Xác định 2 component (AJ và moderator)
    parts <- strsplit(nd$construct, "\\*")[[1]]
    comp1 <- parts[1]  # "AJ"
    comp2 <- parts[2]  # "ETH" hoặc "TC"

    # Vẽ box interaction
    p <- p +
      annotate("rect", xmin=xi-hw*0.8, xmax=xi+hw*0.8,
                ymin=yi-hh*0.85, ymax=yi+hh*0.85,
                fill=int_fill, color=int_border, linewidth=0.60,
                linetype="solid") +
      annotate("text", x=xi, y=yi, size=theme$size_construct_code,
                label=nd$construct, color=int_border, fontface="bold")

    # Mũi tên component1 → interaction, component2 → interaction
    for (src in c(comp1, comp2)) {
      src_nd <- nodes %>% filter(construct == src)
      if (!nrow(src_nd)) next
      ep_s <- ge_edge(src_nd$x, src_nd$y, xi, yi, hw, hh)
      ep_e <- ge_edge(xi, yi, src_nd$x, src_nd$y, hw*0.8, hh*0.85)
      p <- p + annotate("segment", x=ep_s[1], y=ep_s[2],
                          xend=ep_e[1], yend=ep_e[2],
                          color=int_border, linewidth=0.45, linetype="dotted",
                          arrow=arrow(length=unit(0.10,"cm"), type="open"))
    }

    # Mũi tên interaction → AQ
    aq_nd <- nodes %>% filter(construct=="AQ")
    if (!nrow(aq_nd)) next
    ep_i <- ge_edge(xi, yi, aq_nd$x, aq_nd$y, hw*0.8, hh*0.85)
    ep_a <- ge_edge(aq_nd$x, aq_nd$y, xi, yi, hw, hh)

    # Path coef từ data (nếu có)
    pc  <- paths_data %>% filter(from == nd$construct & to == "AQ")
    lbl <- if (show_stats && nrow(pc)>0 && !is.na(pc$coef[1]))
             paste0(sprintf("%.3f", pc$coef[1]),
                    ifelse(pc$sig_stars[1]!="ns", pc$sig_stars[1], "(ns)"))
           else ""

    p <- p +
      annotate("segment", x=ep_i[1], y=ep_i[2],
                xend=ep_a[1], yend=ep_a[2],
                color=theme$arrow_sig, linewidth=theme$lwd_main,
                arrow=arrow(length=unit(0.16,"cm"), type="closed"))
    if (nchar(lbl)>0) {
      p <- p + annotate("label",
                          x=(ep_i[1]+ep_a[1])/2, y=(ep_i[2]+ep_a[2])/2,
                          label=lbl, size=theme$size_path_label,
                          label.padding=unit(0.09,"lines"), label.size=0.15,
                          fill="white", color=theme$arrow_sig)
    }
  }
  p
}

#' Vẽ residual/disturbance terms cho biến nội sinh
rd_residuals <- function(p, nodes, theme) {
  endo <- nodes %>% filter(construct %in% c("AJ","AQ"))
  if (!nrow(endo)) return(p)

  hw <- theme$hw; hh <- theme$hh

  for (i in seq_len(nrow(endo))) {
    nd   <- endo[i,]
    # Vẽ mũi tên từ phía trên vào construct (residual)
    xr   <- nd$x + hw * 0.5
    yr_s <- nd$y + hh + 0.55   # điểm bắt đầu mũi tên residual
    yr_e <- nd$y + hh           # đỉnh construct

    lbl  <- paste0("e_", nd$construct)

    p <- p +
      # Vòng tròn nhỏ (dùng point lớn)
      annotate("point", x=xr, y=yr_s+0.20, size=4.5,
                shape=21, fill="white", color=theme$arrow_resid, stroke=0.8) +
      annotate("text", x=xr, y=yr_s+0.20, label=lbl,
                size=theme$size_residual, color=theme$arrow_resid) +
      annotate("segment", x=xr, y=yr_s+0.10,
                xend=xr, yend=yr_e,
                color=theme$arrow_resid, linewidth=0.40,
                arrow=arrow(length=unit(0.10,"cm"), type="open"))
  }
  p
}

#' Vẽ construct boxes (lớp trên cùng để che đầu mũi tên)
rd_construct_boxes <- function(p, nodes, model, theme, show_stats=TRUE) {
  r2_df <- model$r2
  hw    <- theme$hw; hh <- theme$hh

  for (i in seq_len(nrow(nodes))) {
    nd    <- nodes[i,]
    meas  <- model$meas_types[nd$construct]
    if (is.na(meas)) meas <- "reflective"

    lt    <- if (meas=="formative") "dashed" else "solid"
    fc    <- theme$fill[nd$role];   if(is.na(fc))   fc   <- "#F5F5F5"
    bc    <- theme$border[nd$role]; if(is.na(bc))   bc   <- "#333333"

    # Interaction construct: box nhỏ hơn (đã vẽ ở rd_moderation_interaction)
    if (nd$role == "interaction") next

    # R² label
    r2_row <- r2_df %>% filter(construct == nd$construct)
    r2_lbl <- if (show_stats && nrow(r2_row)>0)
                paste0("R2=", sprintf("%.3f", r2_row$r2[1]))
              else NA_character_

    y_text_off <- if (!is.na(r2_lbl)) 0.10 else 0

    p <- p +
      annotate("rect", xmin=nd$x-hw, xmax=nd$x+hw,
                ymin=nd$y-hh, ymax=nd$y+hh,
                fill=fc, color=bc, linewidth=0.78, linetype=lt) +
      annotate("text", x=nd$x, y=nd$y+y_text_off,
                label=nd$construct, fontface="bold",
                size=theme$size_construct_code, color=bc) +
      annotate("text", x=nd$x, y=nd$y-0.12,
                label=VN_CONSTRUCT_NAMES[nd$construct] %||% model$full_names[nd$construct],
                size=theme$size_construct_name, color=bc, lineheight=0.85)

    if (!is.na(r2_lbl)) {
      p <- p + annotate("text", x=nd$x, y=nd$y-hh+0.12,
                          label=r2_lbl, size=theme$size_r2,
                          color="#444", fontface="italic")
    }
  }
  p
}

#' Vẽ legend
rd_legend <- function(p, nodes, theme, ind_df = NULL) {
  x_max <- if (nrow(nodes)>0) max(nodes$x, na.rm=TRUE) else 12
  y_max <- if (nrow(nodes)>0) max(nodes$y, na.rm=TRUE) else 7.5

  # Tính cạnh phải thực tế (bao gồm indicator boxes)
  x_right_ind <- x_max
  if (!is.null(ind_df) && nrow(ind_df) > 0 && "x_ind" %in% names(ind_df)) {
    x_right_ind <- max(x_right_ind, max(ind_df$x_ind, na.rm=TRUE) + theme$hw_in)
  }
  lx <- x_right_ind + 2.5; ly <- y_max

  bc_ref <- theme$border["iv"]
  bc_for <- theme$border["iv"]

  p <- p +
    # --- Mo hinh do luong ---
    annotate("text", x=lx, y=ly+0.50, label="Mô hình đo lường",
              fontface="bold", size=theme$size_legend, color="#333") +
    annotate("rect", xmin=lx-0.88, xmax=lx+0.88,
              ymin=ly-0.35, ymax=ly+0.35,
              fill=theme$fill["iv"], color=bc_ref, linewidth=0.55, linetype="solid") +
    annotate("text", x=lx, y=ly, label="Phản xạ (Reflective)",
              size=theme$size_legend-0.4, color=bc_ref) +
    annotate("rect", xmin=lx-0.88, xmax=lx+0.88,
              ymin=ly-1.25, ymax=ly-0.65,
              fill=theme$fill["iv"], color=bc_for, linewidth=0.55, linetype="dashed") +
    annotate("text", x=lx, y=ly-0.95, label="Hình thành (Formative)",
              size=theme$size_legend-0.4, color=bc_for) +
    # --- Huong mui ten ---
    annotate("text", x=lx, y=ly-1.80, label="Hướng chỉ báo",
              fontface="bold", size=theme$size_legend, color="#333") +
    annotate("segment", x=lx-0.6, y=ly-2.25, xend=lx+0.6, yend=ly-2.25,
              color=bc_ref, linewidth=0.55,
              arrow=arrow(length=unit(0.14,"cm"), type="open")) +
    annotate("text", x=lx, y=ly-2.55, label="Phản xạ (C → chỉ báo)",
              size=theme$size_legend-0.5, color="#555") +
    annotate("segment", x=lx+0.6, y=ly-3.00, xend=lx-0.6, yend=ly-3.00,
              color=bc_for, linewidth=0.55,
              arrow=arrow(length=unit(0.14,"cm"), type="open")) +
    annotate("text", x=lx, y=ly-3.30, label="Hình thành (chỉ báo → C)",
              size=theme$size_legend-0.5, color="#555") +
    # --- Phan loai bien ---
    annotate("text", x=lx, y=ly-3.85, label="Phân loại biến",
              fontface="bold", size=theme$size_legend, color="#333") +
    annotate("rect", xmin=lx-0.28, xmax=lx+0.28,
              ymin=ly-4.30, ymax=ly-3.98,
              fill=theme$fill["iv"], color=theme$border["iv"], linewidth=0.50) +
    annotate("text", x=lx+0.65, y=ly-4.14, label="Độc lập (IV)",
              size=theme$size_legend-0.5, color=theme$border["iv"], hjust=0) +
    annotate("rect", xmin=lx-0.28, xmax=lx+0.28,
              ymin=ly-4.90, ymax=ly-4.58,
              fill=theme$fill["mediator"], color=theme$border["mediator"], linewidth=0.50) +
    annotate("text", x=lx+0.65, y=ly-4.74, label="Trung gian",
              size=theme$size_legend-0.5, color=theme$border["mediator"], hjust=0) +
    annotate("rect", xmin=lx-0.28, xmax=lx+0.28,
              ymin=ly-5.50, ymax=ly-5.18,
              fill=theme$fill["dv"], color=theme$border["dv"], linewidth=0.50) +
    annotate("text", x=lx+0.65, y=ly-5.34, label="Phụ thuộc (DV)",
              size=theme$size_legend-0.5, color=theme$border["dv"], hjust=0) +
    annotate("rect", xmin=lx-0.28, xmax=lx+0.28,
              ymin=ly-6.10, ymax=ly-5.78,
              fill=theme$fill["moderator"], color=theme$border["moderator"], linewidth=0.50,
              linetype="dashed") +
    annotate("text", x=lx+0.65, y=ly-5.94, label="Điều tiết",
              size=theme$size_legend-0.5, color=theme$border["moderator"], hjust=0)

  p
}

# ==============================================================================
# LAYER 4 — ASSEMBLY: Ghép các lớp thành hình hoàn chỉnh
# ==============================================================================

.assemble_plot <- function(model, nodes, ind_df, seg_df, paths_data, theme,
                            diagram_mode      = "PLS_RESULT",
                            moderation_style  = "software",
                            show_residuals    = FALSE,
                            show_stats_labels = TRUE,
                            show_outer        = TRUE,
                            compact_outer     = FALSE,
                            title             = NULL) {

  subtitle <- if (diagram_mode == "PLS_RESULT")
    "PLS-SEM — Hệ số đường đã chuẩn hóa; ***p<.001, **p<.01, *p<.05, (ns) không có ý nghĩa thống kê"
  else
    "Mô hình lý thuyết — cấu trúc giả thuyết và quan hệ nhân quả"

  if (is.null(title)) {
    title <- if (diagram_mode=="PLS_RESULT")
      "Mô hình cấu trúc PLS-SEM (Kết quả)"
    else
      "Mô hình PLS-SEM (Lý thuyết)"
  }

  p <- ggplot() +
    theme_void(base_size = 11) +
    theme(
      plot.background = element_rect(fill="white", color=NA),
      plot.title      = element_text(face="bold", size=14, hjust=0.5,
                                     margin=margin(b=7)),
      plot.subtitle   = element_text(size=9, hjust=0.5, color="#555",
                                     margin=margin(b=5)),
      plot.margin     = margin(18, 18, 18, 18)
    ) +
    labs(title=title, subtitle=subtitle)

  # (1) Outer model indicators — lớp nền, vẽ trước
  if (show_outer) {
    p <- rd_outer_model(p, nodes, ind_df, model, theme, compact_outer)
  }

  # (2) Residuals — vẽ trước construct box
  if (show_residuals) {
    p <- rd_residuals(p, nodes, theme)
  }

  # (3) Main structural paths
  p <- rd_inner_paths(p, nodes, seg_df, theme, show_stats=show_stats_labels)

  # (4) Moderation
  if (moderation_style == "interaction_construct") {
    p <- rd_moderation_interaction(p, nodes, paths_data, model, theme,
                                    show_stats=show_stats_labels)
  } else {
    p <- rd_moderation_software(p, nodes, paths_data, model, theme,
                                 show_stats=show_stats_labels)
  }

  # (5) Construct boxes — phủ lên mũi tên
  p <- rd_construct_boxes(p, nodes, model, theme, show_stats=show_stats_labels)

  # (6) Legend
  p <- rd_legend(p, nodes, theme, ind_df = ind_df)

  p
}

# ==============================================================================
# PUBLIC API — Hai hàm vẽ chính + hàm chạy độc lập
# ==============================================================================

#' Vẽ và lưu sơ đồ mô hình cấu trúc PLS-SEM
#'
#' @param cfg         list config (từ load_config)
#' @param log_info    list log (từ init_pipeline_log)
#' @param pls_model   seminr model (tuỳ chọn)
#' @param boot_model  seminr bootstrap model (tuỳ chọn)
#' @param output_dir  thư mục xuất file
#' @param filename    tên file (không extension)
#' @param width,height kích thước hình (inch)
#' @param dpi         độ phân giải PNG
#' @param diagram_mode "PLS_RESULT" | "THEORETICAL_CLEAN"
#' @param moderation_style "software" | "interaction_construct"
#' @param show_residuals   TRUE/FALSE — vẽ residual terms
#' @param compact_outer    TRUE/FALSE — rút gọn indicator labels
#' @param show_stats_labels TRUE/FALSE — hiện beta/p/R²
#' @param export_formats    vector: c("png","pdf","svg")
#' @param title       tiêu đề (NULL = tự động)
#' @return invisible ggplot object
plot_structural_model <- function(cfg, log_info,
                                   pls_model         = NULL,
                                   boot_model        = NULL,
                                   output_dir        = "10_report/figures",
                                   filename          = "structural_model",
                                   width             = 30,
                                   height            = 18,
                                   dpi               = 300,
                                   diagram_mode      = "PLS_RESULT",
                                   moderation_style  = "software",
                                   show_residuals    = FALSE,
                                   compact_outer     = FALSE,
                                   show_stats_labels = TRUE,
                                   show_outer        = TRUE,
                                   export_formats    = c("png","pdf"),
                                   title             = NULL) {

  log_step(log_info, paste0("Step 9b: Structural Plot — chế độ: ", diagram_mode))
  dir.create(output_dir, showWarnings=FALSE, recursive=TRUE)

  # --- Model data ---
  model  <- mp_build_model(cfg, pls_model, boot_model)
  theme  <- sc_theme(diagram_mode)

  # --- Layout ---
  nodes  <- le_construct_layout(model, moderation_style)

  # --- Theo chế độ: THEORETICAL_CLEAN ẩn stats nếu không có dữ liệu ---
  if (diagram_mode == "THEORETICAL_CLEAN") {
    show_stats_labels <- show_stats_labels  # giữ nguyên quyết định của caller
    show_residuals    <- show_residuals
    if (is.null(show_residuals) || isFALSE(show_residuals)) show_residuals <- FALSE
    compact_outer     <- TRUE               # mặc định compact cho bản lý thuyết
  }

  # --- Indicators ---
  ind_df <- if (show_outer)
    le_indicator_layout(nodes, model, theme$hw, theme$hh, theme$hw_in, theme$hh_in)
  else
    data.frame()

  # --- Main paths (loại trừ moderation paths) ---
  paths_data <- model$paths_data
  main_paths <- paths_data %>%
    filter(!grepl("\\*|MInt|_x_", from, ignore.case=TRUE))

  seg_df <- main_paths %>%
    left_join(nodes %>% select(construct, x, y), by=c("from"="construct")) %>%
    rename(x_from=x, y_from=y) %>%
    left_join(nodes %>% select(construct, x, y), by=c("to"="construct")) %>%
    rename(x_to=x, y_to=y) %>%
    filter(!is.na(x_from) & !is.na(x_to))

  seg_df <- ge_compute_segments(seg_df, theme$hw, theme$hh)

  # --- Assemble ---
  p <- .assemble_plot(
    model             = model,
    nodes             = nodes,
    ind_df            = ind_df,
    seg_df            = seg_df,
    paths_data        = paths_data,
    theme             = theme,
    diagram_mode      = diagram_mode,
    moderation_style  = moderation_style,
    show_residuals    = show_residuals,
    show_stats_labels = show_stats_labels,
    show_outer        = show_outer,
    compact_outer     = compact_outer,
    title             = title
  )

  # --- Export ---
  for (fmt in export_formats) {
    out_f <- file.path(output_dir, paste0(filename, ".", fmt))
    if (fmt == "svg") {
      if (requireNamespace("svglite", quietly=TRUE)) {
        svglite::svglite(out_f, width=width, height=height)
        print(p)
        grDevices::dev.off()
      } else {
        log_msg(log_info, "  [WARN] svglite chua cai — bo qua SVG. install.packages('svglite')")
        next
      }
    } else if (fmt == "png") {
      if (requireNamespace("ragg", quietly=TRUE)) {
        ragg::agg_png(out_f, width=width, height=height, units="in",
                      res=dpi, background="white")
        print(p)
        grDevices::dev.off()
      } else {
        ggplot2::ggsave(out_f, plot=p, width=width, height=height,
                        dpi=dpi, bg="white",
                        device=grDevices::png, type="cairo")
      }
    } else if (fmt == "pdf") {
      ggplot2::ggsave(out_f, plot=p, width=width, height=height,
                      bg="white", device=grDevices::cairo_pdf)
    } else {
      ggplot2::ggsave(out_f, plot=p, width=width, height=height,
                      dpi=72, bg="white")
    }
    log_msg(log_info, paste0("  Da xuat: ", out_f))
  }

  invisible(p)
}

#' Xuất ĐỒNG THỜI cả 2 phiên bản: PLS_RESULT + THEORETICAL_CLEAN
#'
#' Wrapper tiện lợi gọi plot_structural_model() hai lần với preset phù hợp.
#' @param ... Tất cả tham số chuyển tiếp tới plot_structural_model()
#'            (trừ diagram_mode, show_residuals, moderation_style)
#' @param mod_result_style  moderation style cho PLS_RESULT
#' @param mod_theory_style  moderation style cho THEORETICAL_CLEAN
plot_all_versions <- function(cfg, log_info, ...,
                               mod_result_style = "software",
                               mod_theory_style = "interaction_construct") {
  dots <- list(...)

  log_msg(log_info, "  Xuat phien ban PLS_RESULT...")
  do.call(plot_structural_model, c(
    list(cfg=cfg, log_info=log_info,
         diagram_mode     = "PLS_RESULT",
         moderation_style = mod_result_style,
         show_residuals   = FALSE,
         show_stats_labels= TRUE,
         compact_outer    = FALSE,
         filename         = paste0(dots$filename %||% "structural_model", "_result")),
    dots[!names(dots) %in% c("diagram_mode","moderation_style",
                               "show_residuals","show_stats_labels",
                               "compact_outer","filename")]
  ))

  log_msg(log_info, "  Xuat phien ban THEORETICAL_CLEAN...")
  do.call(plot_structural_model, c(
    list(cfg=cfg, log_info=log_info,
         diagram_mode      = "THEORETICAL_CLEAN",
         moderation_style  = mod_theory_style,
         show_residuals    = TRUE,
         show_stats_labels = FALSE,
         compact_outer     = TRUE,
         filename          = paste0(dots$filename %||% "structural_model", "_theory")),
    dots[!names(dots) %in% c("diagram_mode","moderation_style",
                               "show_residuals","show_stats_labels",
                               "compact_outer","filename")]
  ))

  invisible(NULL)
}

# Helper: %||% (NULL coalesce) nếu chưa có
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ==============================================================================
# STANDALONE — Chạy không cần pipeline đầy đủ
# ==============================================================================

#' Chạy độc lập: đọc config + CSV → xuất 2 phiên bản hình
#'
#' @param config_path       Đường dẫn file config chính (có constructs)
#' @param stage_config_path Đường dẫn config stage (override output_dir, seed)
#' @param output_dir        Thư mục xuất PNG/PDF/SVG
#' @param filename          Tên file gốc (sẽ thêm _result / _theory)
#' @param both_versions     TRUE = xuất cả 2; FALSE = chỉ xuất diagram_mode
#' @param diagram_mode      "PLS_RESULT" | "THEORETICAL_CLEAN"
#' @param moderation_style  "software" | "interaction_construct"
#' @param show_residuals    TRUE/FALSE
#' @param compact_outer     TRUE/FALSE
#' @param show_stats_labels TRUE/FALSE
#' @param export_formats    c("png","pdf","svg")
#' @param width,height      Kích thước hình (inch)
run_structural_plot <- function(config_path       = "00_meta/analysis_config.yaml",
                                 stage_config_path = "config/main.yml",
                                 output_dir        = "10_report/figures",
                                 filename          = "structural_model",
                                 both_versions     = TRUE,
                                 diagram_mode      = "PLS_RESULT",
                                 moderation_style  = "software",
                                 show_residuals    = FALSE,
                                 compact_outer     = FALSE,
                                 show_stats_labels = TRUE,
                                 export_formats    = c("png","pdf"),
                                 width             = 30,
                                 height            = 18) {

  if (!requireNamespace("yaml", quietly=TRUE))
    stop("Can cai: install.packages('yaml')")
  source("R/00_config.R")
  source("R/utils_logging.R")

  cfg <- load_config(config_path)

  if (file.exists(stage_config_path)) {
    sc <- yaml::read_yaml(stage_config_path)
    if (!is.null(sc$output_dir)) cfg$output_dir <- sc$output_dir
    if (!is.null(sc$seed))       cfg$project$seed <- sc$seed
  }
  if (is.null(cfg$output_dir)) cfg$output_dir <- output_dir

  log_info <- init_pipeline_log(log_dir="logs", prefix="structural_plot")

  if (both_versions) {
    plot_all_versions(
      cfg        = cfg,
      log_info   = log_info,
      output_dir = output_dir,
      filename   = filename,
      export_formats = export_formats,
      width      = width,
      height     = height
    )
  } else {
    plot_structural_model(
      cfg               = cfg,
      log_info          = log_info,
      output_dir        = output_dir,
      filename          = filename,
      diagram_mode      = diagram_mode,
      moderation_style  = moderation_style,
      show_residuals    = show_residuals,
      compact_outer     = compact_outer,
      show_stats_labels = show_stats_labels,
      export_formats    = export_formats,
      width             = width,
      height            = height
    )
  }

  close_pipeline_log(log_info)
  message("Hoan thanh. Kiem tra thu muc: ", output_dir)
}
