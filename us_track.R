us_track <- function(center.ini = NULL,
                     inputfile = NULL,
                     filtertype = "none",
                     filtersize = 5,
                     overlap = 50,
                     jump = 0,
                     kernel = 31) {
  # ###################### SET UP ######################
  if (!is.na(match("batch.simulation", ls(
    all.names = TRUE, envir = .GlobalEnv
  )))) {
    # do nothing on batch run
  } else {
    # restart all variables
    rm(list = ls(all.names = TRUE, envir = .GlobalEnv))
    options(warn = -1)
  }
  
  # custom functions
  round_2_odd <- function(x) {
    2 * floor(x / 2) + 1
  }
  
  # dir for storing images
  raw.dir <- file.path("www", "0 raw")
  # delete folder if it already exists
  if (dir.exists(raw.dir)) {
    unlink(file.path(raw.dir), recursive = TRUE)
  }
  dir.create(file.path(raw.dir), showWarnings = FALSE)
  shiny::addResourcePath(prefix = "www", directoryPath = raw.dir)
  
  gray.dir <- file.path("www", "2 gray")
  # delete folder if it already exists
  if (dir.exists(gray.dir)) {
    unlink(file.path(gray.dir), recursive = TRUE)
  }
  dir.create(file.path(gray.dir), showWarnings = FALSE)
  shiny::addResourcePath(prefix = "www", directoryPath = gray.dir)
  
  detrend.dir <- file.path("www", "3 detrend")
  # delete folder if it already exists
  if (dir.exists(detrend.dir)) {
    unlink(file.path(detrend.dir), recursive = TRUE)
  }
  dir.create(file.path(detrend.dir), showWarnings = FALSE)
  shiny::addResourcePath(prefix = "www", directoryPath = detrend.dir)
  
  hist.dir <- file.path("www", "4 equaliz")
  # delete folder if it already exists
  if (dir.exists(hist.dir)) {
    unlink(file.path(hist.dir), recursive = TRUE)
  }
  dir.create(file.path(hist.dir), showWarnings = FALSE)
  shiny::addResourcePath(prefix = "www", directoryPath = hist.dir)
  
  filter.dir <- file.path("www", "5 filter")
  # delete folder if it already exists
  if (dir.exists(filter.dir)) {
    unlink(file.path(filter.dir), recursive = TRUE)
  }
  dir.create(file.path(filter.dir), showWarnings = FALSE)
  shiny::addResourcePath(prefix = "www", directoryPath = filter.dir)
  
  bin.dir <- file.path("www", "6 bin")
  # delete folder if it already exists
  if (dir.exists(bin.dir)) {
    unlink(file.path(bin.dir), recursive = TRUE)
  }
  dir.create(file.path(bin.dir), showWarnings = FALSE)
  shiny::addResourcePath(prefix = "www", directoryPath = bin.dir)
  
  morph.dir <- file.path("www", "7 morph")
  # delete folder if it already exists
  if (dir.exists(morph.dir)) {
    unlink(file.path(morph.dir), recursive = TRUE)
  }
  dir.create(file.path(morph.dir), showWarnings = FALSE)
  shiny::addResourcePath(prefix = "www", directoryPath = morph.dir)
  
  # dir for storing images AFTER prrocess
  out.dir <- file.path("www", "8 output")
  # delete folder if it already exists
  if (dir.exists(out.dir)) {
    unlink(file.path(out.dir), recursive = TRUE)
  }
  dir.create(file.path(out.dir), showWarnings = FALSE)
  shiny::addResourcePath(prefix = "www", directoryPath = out.dir)
  
  # dir for storing results
  res.dir <- file.path("www", "CSV")
  dir.create(file.path(res.dir), showWarnings = FALSE)
  shiny::addResourcePath(prefix = "www", directoryPath = res.dir)
  
  # color palette (grayscale)
  pal <- grDevices::gray(seq(
    from = 0,
    to = 1,
    length.out = 256
  ), alpha = NULL)
  
  ###################### DATA TO ANALYZE ######################
  
  # video (.mp4) file names
  if (!is.na(match("batch.simulation", ls(
    all.names = TRUE, envir = .GlobalEnv
  )))) {
    inputfile <- "sim_video.mp4"
  } else {
    inputfile <- inputfile
  }
  
  # Get video info such as width, height, format, duration and framerate
  info <- av::av_media_info(inputfile)
  
  # ###################### TRACKING PARAMETERS ######################
  
  # object size
  kernel <- round_2_odd(kernel) # odd numbers only
  
  if (!is.na(match("batch.simulation", ls(
    all.names = TRUE, envir = .GlobalEnv
  )))) {
    filter.type <- track.config[1]
    filter.size <- as.numeric(track.config[2])
    overlap <- as.numeric(track.config[3])
    jump <- as.numeric(track.config[4])
    param <-
      as.data.frame(matrix(
        c(filter.type, filter.size, overlap, jump),
        nrow = 1,
        dimnames = list(c(), c(
          "Filter type", "Filter size", "Overlap %", "Jump"
        ))
      ))
  } else {
    param <- list(
      `Filter type:TEXT` = filtertype,
      `Filter size:NUM` = filtersize,
      `Overlap %:NUM` = overlap,
      `Jump:NUM` = jump
    )
  }
  
  roi <-
    round_2_odd(kernel * (1 + as.numeric(param$`Overlap %`) / 100)) # odd numbers only
  
  # ###################### CODE TO RUN ######################
  
  # read 1:N images and process all frames
  source("f_track_first.R", local = TRUE)
  source("f_track_all.R", local = TRUE)
  
  # show tracking
  run_all <-
    f_all_frames(
      inputfile = inputfile,
      info = info,
      param = param,
      track.by = c("cross-correlation", "blob")[1],
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
  
  # Set output file
  output_filename <- file.path("www", "outputvideo.mp4")
  
  # ###################### READ DATA FOR ANALYSIS ######################
  
  if (!is.na(match("batch.simulation", ls(all = TRUE, envir = .GlobalEnv)))) {
    # do nothing on batch run
  } else {
    # open output video
    # utils::browseURL(output_filename)
  }
  
  # ###################### END RUN ######################
}
