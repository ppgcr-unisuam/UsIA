# ###################### SET UP ######################
if (!is.na(match("batch.simulation", ls(all.names = TRUE, envir = .GlobalEnv)))) {
  # do nothing on batch run
} else {
  # restart all variables
  rm(list = ls(all.names = TRUE, envir = .GlobalEnv))
  options(warn = -1)
}

# tipe-I error
alpha <- 0.05

# dir for storing results
res.dir <- "7 CSV"

# read trajectory data from CSV file
trajectory.sim <-
  read.csv(file.path(res.dir, "trajectory_simulated.csv"), sep = ",")
trajectory.meas <-
  read.csv(file.path(res.dir, "trajectory_measured.csv"), sep = ",")

if (!is.na(match("batch.simulation", ls(all.names = TRUE, envir = .GlobalEnv)))) {
  # decimate vector size due to jump
  iter <- seq(from = 1,
              to = info$video$frames,
              by = jump + 1)
  trajectory.sim <- trajectory.sim[iter, ]
} else {
  # do nothing on single run
}

# calibration analysis
fit.X <- lm(trajectory.meas$X ~ trajectory.sim$X)
r.sq.X <- summary(fit.X)$r.squared
fit.Y <- lm(trajectory.meas$Y ~ trajectory.sim$Y)
r.sq.Y <- summary(fit.Y)$r.squared

# LOA
mean.X <- (trajectory.meas$X + trajectory.sim$X) / 2
diff.X <- trajectory.meas$X - trajectory.sim$X
mean.diff.X <- mean(diff.X)
sd.diff.X <- sd(diff.X)

mean.Y <- (trajectory.meas$Y + trajectory.sim$Y) / 2
diff.Y <- trajectory.meas$Y - trajectory.sim$Y
mean.diff.Y <- mean(diff.Y)
sd.diff.Y <- sd(diff.Y)

# output results
loa.results <- round(c(r.sq.X, r.sq.Y, mean.diff.X, mean.diff.Y), 0)

if (!is.na(match("batch.simulation", ls(all.names = TRUE, envir = .GlobalEnv)))) {
  # do nothing on batch run
} else {
  # plot data from CSV file
  dev.new()
  layout(matrix(
    c(1, 2, 3, 4),
    nrow = 2,
    ncol = 2,
    byrow = TRUE
  ))
  par(oma = c(2, 2, 2, 2), mar = c(4, 3, 3, 1))
  old_par = par()
  par(pty = "s")
  
  # calibration, X
  plot(
    trajectory.sim[, 1],
    trajectory.meas[, 1],
    xlab = "Simulated, px",
    ylab = "Measured, px",
    main = "Calibration plot, X axis",
    xlim = c(
      min(trajectory.sim[, 1], trajectory.meas[, 1]),
      max(trajectory.sim[, 1], trajectory.meas[, 1])
    ),
    ylim = c(
      min(trajectory.sim[, 1], trajectory.meas[, 1]),
      max(trajectory.sim[, 1], trajectory.meas[, 1])
    ),
    asp = NA,
    col = "black",
    type = "p",
    lty = 1,
    lwd = 1,
    pch = 19
  )
  legend(
    "topleft",
    legend = c("Data", "Calibration", paste("R² = ", round(r.sq.X, digits = 3), sep = "")),
    col = c("black", "red", NA),
    lty = 1,
    lwd = 1,
    pch = c(0, 4, NA),
    bty = "n"
  )
  
  # calibration, Y
  plot(
    trajectory.sim[, 2],
    trajectory.meas[, 2],
    xlab = "Simulated, px",
    ylab = "Measured, px",
    main = "Calibration plot, Y axis",
    xlim = c(
      min(trajectory.sim[, 2], trajectory.meas[, 2]),
      max(trajectory.sim[, 2], trajectory.meas[, 2])
    ),
    ylim = c(
      min(trajectory.sim[, 2], trajectory.meas[, 2]),
      max(trajectory.sim[, 2], trajectory.meas[, 2])
    ),
    asp = NA,
    col = "black",
    type = "p",
    lty = 1,
    lwd = 1,
    pch = 19
  )
  legend(
    "topleft",
    legend = c("Data", "Calibration", paste("R² = ", round(r.sq.Y, digits = 3), sep = "")),
    col = c("black", "red", NA),
    lty = 1,
    lwd = 1,
    pch = c(0, 4, NA),
    bty = "n"
  )
  
  # LOA, X
  ceil <-
    ceiling(max(
      abs(mean.diff.X - qnorm(1 - alpha) * sd.diff.X),
      abs(mean.diff.Y - qnorm(1 - alpha) * sd.diff.Y),
      abs(mean.diff.X + qnorm(1 - alpha) * sd.diff.X),
      abs(mean.diff.Y +
            qnorm(1 - alpha) * sd.diff.Y)
    ) * 1.2)
  
  plot(
    mean.X,
    diff.X,
    type = "p",
    xlim = c(
      min(trajectory.sim[, 1], trajectory.meas[, 1]),
      max(trajectory.sim[, 1], trajectory.meas[, 1])
    ),
    ylim = c(-ceil, ceil),
    xlab = "Average X, px",
    ylab = "Bias X, px",
    main = "Limits of agreement plot, X axis",
    bty = "n"
  )
  abline(h = mean(diff.X),
         col = "black",
         lty = 2)
  lines(mean.X, rep(mean.diff.X - qnorm(1 - alpha) * sd.diff.X, length(mean.X)), col = "red")
  lines(mean.X, rep(mean.diff.X + qnorm(1 - alpha) * sd.diff.X, length(mean.X)), col = "red")
  abline(h = 0, col = "gray", lty = 2)
  legend(
    "topleft",
    legend = c(paste(
      "Bias = ", round(mean.diff.X, digits = 3), sep = ""
    ), "95%CI"),
    col = c("black", "red"),
    lty = c(2, 1),
    lwd = 1,
    pch = c(NA),
    bty = "n"
  )
  
  # LOA, Y
  plot(
    mean.Y,
    diff.Y,
    type = "p",
    xlim = c(
      min(trajectory.sim[, 2], trajectory.meas[, 2]),
      max(trajectory.sim[, 2], trajectory.meas[, 2])
    ),
    ylim = c(-ceil, ceil),
    xlab = "Average Y, px",
    ylab = "Bias Y, px",
    main = "Limits of agreement plot, Y axis",
    bty = "n"
  )
  abline(h = mean(diff.Y),
         col = "black",
         lty = 2)
  lines(mean.Y, rep(mean.diff.Y - qnorm(1 - alpha) * sd.diff.Y, length(mean.Y)), col = "red")
  lines(mean.Y, rep(mean.diff.Y + qnorm(1 - alpha) * sd.diff.Y, length(mean.Y)), col = "red")
  abline(h = 0, col = "gray", lty = 2)
  legend(
    "topleft",
    legend = c(paste(
      "Bias = ", round(mean.diff.Y, digits = 3), sep = ""
    ), "95%CI"),
    col = c("black", "red"),
    lty = c(2, 1),
    lwd = 1,
    pch = c(NA),
    bty = "n"
  )
  
  title("Validity analysis", outer = TRUE)
  par(old_par)
}
