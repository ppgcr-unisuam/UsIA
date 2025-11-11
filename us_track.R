us_track <- function(center.ini = NULL,
                     inputfile = NULL,
                     filtertype = "none",
                     filtersize = 5,
                     overlap = 50,
                     jump = 0,
                     kernel = 31) {
  
  # ============================================================
  # 1) GLOBAL SETUP
  # ============================================================
  
  rm(list = ls(all.names = TRUE, envir = .GlobalEnv))
  options(warn = -1)

  # Helper: round number to next odd
  round_2_odd <- function(x) 2 * floor(x / 2) + 1
  
  # Helper: create clean directory
  make_clean_dir <- function(path) {
    if (dir.exists(path)) unlink(path, recursive = TRUE)
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    shiny::addResourcePath(prefix = "www", directoryPath = path)
  }
  
  # ============================================================
  # 2) DIRECTORY SETUP
  # ============================================================
  
  raw.dir    <- "www/0 raw"
  gray.dir   <- "www/2 gray"
  detrend.dir <- "www/3 detrend"
  hist.dir   <- "www/4 equaliz"
  filter.dir <- "www/5 filter"
  bin.dir    <- "www/6 bin"
  morph.dir  <- "www/7 morph"
  out.dir    <- "www/8 output"
  res.dir    <- "www/CSV"
  
  lapply(c(raw.dir, gray.dir, detrend.dir, hist.dir,
           filter.dir, bin.dir, morph.dir,
           out.dir, res.dir), make_clean_dir)
  
  # grayscale palette
  pal <- grDevices::gray(seq(0, 1, length.out = 256))
  
  # ============================================================
  # 3) INPUT VIDEO
  # ============================================================
  
  info <- av::av_media_info(inputfile)
  
  # ============================================================
  # 4) TRACKING PARAMETERS
  # ============================================================
  
  kernel <- round_2_odd(kernel)
  
  param <- list(
    `Filter type:TEXT` = filtertype,
    `Filter size:NUM` = filtersize,
    `Overlap %:NUM`   = overlap,
    `Jump:NUM`        = jump
  )
  
  roi <- round_2_odd(kernel * (1 + as.numeric(param$`Overlap %`) / 100))
  
  # ============================================================
  # 5) LOAD TRACKING FUNCTIONS
  # ============================================================
  
  source("f_track_first.R", local = TRUE)
  source("f_track_all.R", local = TRUE)
  
  # ============================================================
  # 6) RUN TRACKING
  # ============================================================
  
  run_all <- f_all_frames(
    inputfile = inputfile,
    info = info,
    param = param,
    track.by = "cross-correlation",
    raw.dir = raw.dir,
    gray.dir = gray.dir,
    detrend.dir = detrend.dir,
    hist.dir = hist.dir,
    filter.dir = filter.dir,
    bin.dir = bin.dir,
    morph.dir = morph.dir,
    out.dir = out.dir,
    res.dir = res.dir,
    pal = pal,
    kernel = kernel,
    roi = roi,
    center.ini = center.ini,
    plot.border = FALSE,
    dsp = c("gray", "equalize", "mean", "threshold", "morphologic")
  )
  
  # ============================================================
  # 7) OUTPUT VIDEO
  # ============================================================
  
  output_filename <- "www/outputvideo.mp4"
}
