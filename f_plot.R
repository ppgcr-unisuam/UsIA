plot.trajectory <- function(res.dir, info, calib_factor) {
  # ---- Segurança do layout ----
  # Sempre retorna o layout à configuração original
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par), add = TRUE)
  
  # ---- Verificação da existência dos arquivos ----
  stopifnot(file.exists(file.path(res.dir, "trajectory_measured.csv")))
  stopifnot(file.exists(file.path(res.dir, "displacement.csv")))
  stopifnot(file.exists(file.path(res.dir, "max_cross_correlation.csv")))
  
  # ---- Leitura dos arquivos ----
  trajectory.meas <- read.csv(file.path(res.dir, "trajectory_measured.csv"))
  shifts.all      <- read.csv(file.path(res.dir, "displacement.csv"))
  max.crosscorrel <- read.csv(file.path(res.dir, "max_cross_correlation.csv"))
  
  # ---- Subsetting com segurança ----
  nF <- info$video$frames
  trajectory.meas <- trajectory.meas[seq_len(max(1, nF - 1)), , drop = FALSE] * calib_factor
  shifts.all      <- shifts.all[seq_len(max(1, nF - 2)), , drop = FALSE] * calib_factor
  max.crosscorrel <- max.crosscorrel[seq_len(max(1, nF - 2)), , drop = FALSE]
  
  # ---- Layout vertical (3 painéis) ----
  layout(matrix(1:3, nrow = 3, byrow = TRUE))
  
  # ===============================================================
  #  Painel 1 – Trajetória
  # ===============================================================
  plot(
    trajectory.meas,
    asp = 1,
    col = "blue",
    type = "b",
    pch = 0,
    main = "Trajectory of the object",
    xlab = "X (mm)",
    ylab = "Y (mm)"
  )
  
  # moving average (janela = framerate)
  epoch <- round(info$video$framerate)
  filt  <- rep(1 / epoch, epoch)
  
  moving.avg <- data.frame(
    X = stats::filter(trajectory.meas$X, filt, sides = 2),
    Y = stats::filter(trajectory.meas$Y, filt, sides = 2)
  )
  
  lines(moving.avg, col = "red", lwd = 1)
  
  # ===============================================================
  #  Painel 2 – Shifts
  # ===============================================================
  plot(
    unlist(shifts.all),
    xlab = "Frame",
    ylab = "Shifts (mm)",
    col = "blue",
    type = "b",
    pch = 0,
    main = "Shifting of the object"
  )
  
  # ===============================================================
  #  Painel 3 – Cross-Correlation
  # ===============================================================
  plot(
    unlist(max.crosscorrel),
    xlab = "Frame",
    ylab = "Cross-correlation (a.u.)",
    col = "blue",
    type = "b",
    ylim = c(0, 1),
    pch = 0,
    main = "Maximum of cross-correlation"
  )
}
