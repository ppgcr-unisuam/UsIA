plot.trajectory <- function(res.dir, info) {
  # read and plot trajectory data from CSV file
  trajectory.meas <-
    read.csv(file.path(res.dir, "trajectory_measured.csv"), sep = ",")
  trajectory.meas <- trajectory.meas[1:(info$video$frames - 1),]
  
  trajectory.sim <-
    read.csv(file.path(res.dir, "trajectory_simulated.csv"), sep = ",")
  trajectory.sim <- trajectory.sim[1:(info$video$frames - 1),]
  
  max.crosscorrel <-
    read.csv(file.path(res.dir, "max_crosscorrel.csv"), sep = ",")
  max.crosscorrel <- max.crosscorrel[1:(info$video$frames - 2),]
  
  shifts.all <-
    read.csv(file.path(res.dir, "shifts_all.csv"), sep = ",")
  shifts.all <- shifts.all[1:(info$video$frames - 2),]
  
  layout(matrix(
    c(1, 2, 1, 3, 1, 4),
    nrow = 3,
    ncol = 2,
    byrow = TRUE
  ))
  
  # panel 1
  plot(
    trajectory.sim,
    xlab = "X axis, pixels",
    ylab = "Y axis, pixels",
    xlim = c(0, info$video$width),
    ylim = c(0, info$video$height),
    asp = 1,
    col = "blue",
    type = "b",
    lty = 1,
    lwd = 1,
    pch = 0
  )
  lines(
    trajectory.meas,
    xlim = c(0, info$video$width),
    ylim = c(0, info$video$height),
    asp = 1,
    col = "red",
    type = "b",
    lty = 1,
    lwd = 1,
    pch = 4
  )
  legend(
    "topleft",
    legend = c("Simulated", "Measured"),
    col = c("blue", "red"),
    lty = 1,
    lwd = 1,
    pch = c(0, 4)
  )
  title("Tracking trajectory", outer = FALSE)
  
  # panel 2
  plot(
    seq(1:length(max.crosscorrel)),
    t(max.crosscorrel),
    xlab = "Frame",
    ylab = "Cross-correlation, a.u.",
    ylim = c(0, 1),
    asp = 1,
    col = "blue",
    type = "b",
    lty = 1,
    lwd = 1,
    pch = 0
  )
  title("Maximum of cross-correlation", outer = FALSE)
  
  # panel 3
  plot(
    seq(1:length(shifts.all)),
    t(shifts.all),
    xlab = "Frame",
    ylab = "Shifts, pixels",
    asp = 1,
    col = "blue",
    type = "b",
    lty = 1,
    lwd = 1,
    pch = 0
  )
  title("Shifting of the object", outer = FALSE)
  
  # panel 4
  plot(
    trajectory.meas,
    xlim = c(min(trajectory.meas$X), max(trajectory.meas$X)),
    ylim = c(min(trajectory.meas$Y), max(trajectory.meas$Y)),
    asp = 1,
    col = "blue",
    type = "b",
    lty = 0,
    lwd = 1,
    pch = 0
  )
  epoch <- round(info$video$framerate, digits = 0)
  moving.avg.X <-
    stats::filter(trajectory.meas$X,
                  filter = rep(1 / epoch, epoch),
                  sides = 2)
  moving.avg.Y <-
    stats::filter(trajectory.meas$Y,
                  filter = rep(1 / epoch, epoch),
                  sides = 2)
  moving.avg <- data.frame(X = moving.avg.X, Y = moving.avg.Y)
  lines(
    moving.avg,
    xlim = c(0, info$video$width),
    ylim = c(0, info$video$height),
    asp = 1,
    col = "red",
    type = "l",
    lty = 1,
    lwd = 1,
    pch = 4
  )
  title("Trajectory of the object", outer = FALSE)
  # reset plot config
  local({
    par(mfrow = c(1, 1))
  })
}
