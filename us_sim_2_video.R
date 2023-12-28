# dir for storing images
raw.dir <- "1 raw"
# delete folder if it already exists
if (dir.exists(raw.dir) ) {unlink(file.path(raw.dir), recursive = TRUE)}
dir.create(file.path(raw.dir), showWarnings = FALSE)

# dir for storing results
res.dir <- "7 CSV"
dir.create(file.path(res.dir), showWarnings = FALSE)

# color palette (grayscale)
pal <- grDevices::gray(seq(
  from = 0,
  to = 1,
  length.out = 256
), alpha = NULL)

# ###################### DATA TO ANALYZE ######################

# video (.mp4) file names
input <- "input_video.mp4"

# ###################### SIMULATION PARAMETERS ######################

info <- av::av_media_info(inputfile)
output <- "sim_video.mp4"

# to speed up the process, decrease the image size
if (!is.na(match("batch.simulation", ls(all.names = TRUE, envir = .GlobalEnv)))) {
  info$video$width <- 200
  info$video$height <- 200
}

# Primary matrix (background)
image.sim <- matrix(
  data = 0,
  nrow = info$video$width,
  ncol = info$video$height
)

if (!is.na(match("batch.simulation", ls(all.names = TRUE, envir = .GlobalEnv)))) {
  # secondary matrix side size (object), odd size only
  kernel <- as.numeric(sim.config[1])
  noise.bg <- as.numeric(sim.config[2])
  noise.obj <- as.numeric(sim.config[3])
  noise.path <- as.numeric(sim.config[4])
  filter.type <- sim.config[5]
  filter.size <- as.numeric(sim.config[6])
  param <-
    as.data.frame(matrix(
      c(
        kernel,
        noise.bg,
        noise.obj,
        noise.path,
        filter.type,
        filter.size
      ),
      nrow = 1,
      dimnames = list(
        c(),
        c(
          "Kernel",
          "Noise background (0-255)",
          "Noise object (0-255)",
          "Noise trajectory (pixels)",
          "Filter type",
          "Filter size"
        )
      )
    ))
} else {
  form <-
    list(
      `Kernel:NUM` = 25,
      `Noise background (0-255):NUM` = 0,
      `Noise object (0-255):NUM` = 0,
      `Noise trajectory (pixels):NUM` = 0,
      `Filter type:TEXT` = "none",
      `Filter size:NUM` = 7
    )
  param <- svDialogs::dlg_form(form, "Simulation parameters")$res
  # secondary matrix side size (object), odd size only
  kernel <- as.numeric(param$Kernel)
  noise.bg <-
    as.numeric(param$"Noise background (0-255)") # (min = 0, max = 255)
  noise.obj <-
    as.numeric(param$"Noise object (0-255)") # (min = 0, max = 255)
  noise.path <-
    as.numeric(param$"Noise trajectory (pixels)") # (pixels)
  filter.size <-
    as.numeric(param$"Filter size") # (pixels, odd values only)
  filter.type <- as.character(param$"Filter type")
  # start point, in px
}
x0 <- round(info$video$width / 2)
y0 <- round(info$video$height / 2)

x0 <- 50
y0 <- 50

# ###################### FUNCTION ######################

sim.us <- function(x0, y0) {
  step.x <- 2
  step.y <- 1
  
  # start path
  trajectory <- matrix(NA, ncol = 2, nrow = 0)
  pb = utils::txtProgressBar(min = 0,
                      max = info$video$frames,
                      initial = 0)
  grDevices::png(
    file.path(raw.dir, "image_%06d.png"),
    width = info$video$width,
    height = info$video$height,
    res = 72
  )
  
  # loop for images creation
  for (i in 1:(info$video$frames)) {
    # update status bar
    utils::setTxtProgressBar(pb, i)
    
    # generate and update background noise
    noise <-
      matrix(round(
        runif(
          info$video$width * info$video$height,
          min = 0,
          max = min(noise.bg, 255)
        ),
        0
      ),
      nrow = info$video$width,
      ncol = info$video$height)
    matriz <- image.sim + noise
    
    # moving the pixel
    # setting a linear movement from the margin
    xi <-
      x0 + (i - 1) * step.x + round(runif(1, min = 0, max = noise.path), 0)
    yi <-
      y0 + (i - 1) * step.y + round(runif(1, min = 0, max = noise.path), 0)
    
    # check/set boundaries
    xi <- max(floor(kernel / 2), xi)
    xi <- min(xi, info$video$width - floor(kernel / 2))
    yi <- max(floor(kernel / 2), yi)
    yi <- min(yi, info$video$height - floor(kernel / 2))
    
    # generate and update object noise
    trajectory <- rbind(trajectory, c(xi, yi))
    
    x <- (xi - floor(kernel / 2)):(xi + floor(kernel / 2))
    y <- (yi - floor(kernel / 2)):(yi + floor(kernel / 2))
    matriz[x, y] <-
      matrix(round(runif(
        (kernel) * kernel,
        min = max(255 - noise.obj, 0),
        max = 255
      ), 0), nrow = kernel, ncol = kernel)
    
    # PRE-PROCESSING #2 FILTERING
    if (filter.type != "none") {
      if (filter.size != 0) {
        for (row in 1:dim(matriz)[1]) {
          for (col in 1:dim(matriz)[2]) {
            matriz[row, col] <-
              apply(matrix(matriz[max(1, (row - floor(filter.size / 2))):min((row + floor(filter.size /
                                                                                            2)), dim(matriz)[1]), max(1, (col - floor(filter.size / 2))):min((col +
                                                                                                                                                                floor(filter.size / 2)), dim(matriz)[2])], nrow = 1),
                    MARGIN = 1,
                    FUN = filter.type)
            # image(matriz[, ncol(matriz):1], useRaster = TRUE, axes = FALSE, col = pal)
          }
        }
      }
    }
    
    matriz <- MinMax(data = matriz,
                     min = 0,
                     max = 255)
    
    # signal-to-noise ratio (formjla by Pedrini & Schwartz, 2008)
    SNR <-
      10 * log(sum(matriz[x, y] ^ 2) / sum(((matriz[x, y]) - (noise[x, y])) ^
                                             2), base = 10)
    
    # plotting the image
    par(mar = rep(0, 4), oma = rep(0, 4))
    image(matriz, axes = F, col = pal)
    legend(
      "topleft",
      legend = as.character(i),
      cex = 2,
      text.col = "white",
      box.col = NA,
      bg = NA
    )
    
    # restarting the original data
    matriz[x, y] <- noise[x, y]
  }
  dev.off()
  # write trajectory data to CSV file
  colnames(trajectory) <- c("X", "Y")
  write.csv(
    trajectory,
    file.path(res.dir, "trajectory_simulated.csv"),
    row.names = FALSE,
    col.names = TRUE
  )
  print("", quote = FALSE)
  print("", quote = FALSE)
  return("SNR" = SNR)
}

# Runs the expression and captures all plots into a video
sim.us(x0, y0)

# Read all images and record a video
png_files <- list.files(file.path(raw.dir),
                        pattern = "png",
                        full.names = TRUE)
av::av_encode_video(
  input = png_files,
  output = file.path(output),
  framerate = info$video$frames/info$duration,
  vfilter = paste0(
    "fps=",
    as.character(info$video$frames/info$duration),
    ", setpts=",
    as.character(info$video$frames),
    "*",
    as.character(info$duration),
    "*PTS"
  )
)

if (!is.na(match("batch.simulation", ls(all.names = TRUE, envir = .GlobalEnv)))) {
  # do nothing on batch run
} else {
  # Open output video
  # utils::browseURL(output)
  
  # read and plot trajectory data from CSV file
  dev.new()
  trajectory.sim <-
    read.csv(file.path(res.dir, "trajectory_simulated.csv"),
             sep = ",")
  plot(
    trajectory.sim,
    xlim = c(0, info$video$width),
    ylim = c(0, info$video$height),
    asp = NA,
    col = "blue",
    type = "b",
    lty = 1,
    lwd = 1,
    pch = 0
  )
}
