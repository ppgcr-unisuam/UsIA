f_all_frames <- function(inputfile,
                         param,
                         track.by,
                         raw.dir,
                         gray.dir,
                         detrend.dir,
                         hist.dir,
                         filter.dir,
                         bin.dir,
                         morph.dir,
                         out.dir,
                         res.dir,
                         pal,
                         kernel,
                         roi,
                         center.ini,
                         plot.border,
                         dsp,
                         show.plot,
                         save.plot,
                         info) {
  
  # ---------------------------------------------------------------
  # Pre-load external processing functions (MUCH faster)
  # ---------------------------------------------------------------
  source("f_process_color.R", local = TRUE)
  source("f_process_detrend.R", local = TRUE)
  source("f_process_equalization.R", local = TRUE)
  source("f_process_filter.R", local = TRUE)
  source("f_process_threshold.R", local = TRUE)
  source("f_process_morphological.R", local = TRUE)
  source("f_process_track.R", local = TRUE)
  
  # ---------------------------------------------------------------
  # Helper 1: Load frame from disk
  # ---------------------------------------------------------------
  load_frame <- function(i) {
    file <- file.path(raw.dir, sprintf("image_%06d.png", i))
    grDevices::as.raster(magick::image_read(file))
  }
  
  # ---------------------------------------------------------------
  # Helper 2: Apply preprocessing pipeline
  # ---------------------------------------------------------------
  apply_preprocessing <- function(frame) {
    f <- frame
    if ("gray" %in% dsp) {
      f <- f_color_conversion(f, pal, gray.dir, show.plot = FALSE, save.plot = FALSE, info)
    }
    if ("detrend" %in% dsp) {
      f <- f_spatial_detrend(f, pal, detrend.dir, show.plot = FALSE, save.plot = FALSE, info)
    }
    if ("equalize" %in% dsp) {
      f <- f_equalize(f, pal, hist.dir, show.plot = FALSE, save.plot = FALSE, info)
    }
    if ("filter" %in% dsp) {
      f <- f_spatial_filter(f, param, pal, filter.dir, show.plot = FALSE, save.plot = FALSE, info)
    }
    if ("threshold" %in% dsp) {
      f <- f_threshold(f, pal, bin.dir, show.plot = FALSE, save.plot = FALSE, info)
    }
    if ("morphologic" %in% dsp) {
      f <- f_morphologic(f, pal, morph.dir, show.plot = FALSE, save.plot = FALSE, info)
    }
    return(f)
  }
  
  # ---------------------------------------------------------------
  # Helper 3: Extract ROI around a center
  # ---------------------------------------------------------------
  extract_ROI <- function(frame, center, size) {
    half <- floor(size / 2)
    y1 <- info$video$height - (center$y + half)
    y2 <- info$video$height - (center$y - half)
    x1 <- center$x - half
    x2 <- center$x + half
    frame[y1:y2, x1:x2]
  }
  
  # ---------------------------------------------------------------
  # Helper 4: Save PNG of the current frame (kept as requested)
  # ---------------------------------------------------------------
  save_png_frame <- function(i, width, height) {
    grDevices::png(
      filename = file.path(out.dir, sprintf("image_%06d.png", i)),
      width = width,
      height = height,
      units = "px",
      res = 1,
      type = "cairo"
    )
  }
  
  # ---------------------------------------------------------------
  # SPLIT VIDEO INTO FRAMES (only once)
  # ---------------------------------------------------------------
  av::av_video_images(inputfile, destdir = raw.dir, format = "png", fps = NULL)
  
  # ---------------------------------------------------------------
  # Initialize vectors
  # ---------------------------------------------------------------
  trajectory <- matrix(NA, ncol = 2, nrow = 0)
  max.crosscorrel <- numeric(0)
  shifts.all <- numeric(0)
  past.objects <- list()
  
  iter <- seq(1, info$video$frames, by = as.numeric(param$Jump) + 1)
  index <- 1
  
  # ---------------------------------------------------------------
  # MAIN LOOP
  # ---------------------------------------------------------------
  for (i in 1:(info$video$frames - param$Jump)) {
    
    save_png_frame(i, info$video$width, info$video$height)
    
    if (i == iter[index]) {
      
      # -----------------------------------------------------------
      # First frame — special handling
      # -----------------------------------------------------------
      if (i == 1) {
        
        run_1st <- f_first_frame(
          inputfile = inputfile,
          raw.dir = raw.dir,
          gray.dir = gray.dir,
          detrend.dir = detrend.dir,
          hist.dir = hist.dir,
          filter.dir = filter.dir,
          bin.dir = bin.dir,
          morph.dir = morph.dir,
          out.dir = out.dir,
          param = param,
          pal = pal,
          kernel = kernel,
          roi = roi,
          center.ini = center.ini,
          plot.border = plot.border,
          dsp = dsp,
          show.plot = show.plot,
          save.plot = save.plot,
          info = info
        )
        
        center <- run_1st$center
        
        past.objects[[i]] <- run_1st$object.ini
        trajectory <- rbind(trajectory, c(center$x, center$y))
        
        index <- index + 1
        next
      }
      
      # -----------------------------------------------------------
      # Read and preprocess frame
      # -----------------------------------------------------------
      cur.frame.raw <- load_frame(i)
      
      # Set basic plot
      par(mar = rep(0,4), oma = rep(0,4), omi = rep(0,4), mai = rep(0,4))
      plot(NULL, xlim = c(0, info$video$width), ylim = c(0, info$video$height),
           asp = 1, col = pal)
      par(new = TRUE)
      plot(cur.frame.raw, asp = 1, col = pal)
      
      # -----------------------------------------------------------
      # Update ROI from previous center
      # -----------------------------------------------------------
      prev_center <- list(x = trajectory[index-1,1],
                          y = trajectory[index-1,2])
      
      ROI.raw <- extract_ROI(cur.frame.raw, prev_center, roi)
      
      if (plot.border) {
        rect(prev_center$x - roi/2, prev_center$y - roi/2,
             prev_center$x + roi/2, prev_center$y + roi/2,
             border = "blue", lwd = 2)
      }
      
      # Boundaries check
      if (any(c(prev_center$x - roi/2 <= 0,
                prev_center$y - roi/2 <= 0,
                prev_center$x + roi/2 >= info$video$width,
                prev_center$y + roi/2 >= info$video$height))) {
        stop("ROI is touching borders.")
      }
      
      # -----------------------------------------------------------
      # Preprocess frame, object and ROI together
      # -----------------------------------------------------------
      cur.frame <- apply_preprocessing(cur.frame.raw)
      object     <- apply_preprocessing(past.objects[[i-1]])
      ROI        <- apply_preprocessing(ROI.raw)
      
      # -----------------------------------------------------------
      # TRACK OBJECT
      # -----------------------------------------------------------
      track <- f_process_track(
        cur.frame.bin = cur.frame,
        track.by = track.by,
        object.bin = object,
        ROI.bin = ROI,
        center = prev_center
      )
      
      center$x <- prev_center$x + track$shifts.max.shifts[2]
      center$y <- prev_center$y - track$shifts.max.shifts[1]
      
      # -----------------------------------------------------------
      # Update trajectory and store data
      # -----------------------------------------------------------
      trajectory <- rbind(trajectory, c(center$x, center$y))
      max.crosscorrel <- c(max.crosscorrel, track$max.correl.max.corr)
      shifts.all <- c(shifts.all,
                      round(sqrt(sum(track$shifts.max.shifts^2)), 0))
      
      # store object for next iteration
      past.objects[[i]] <- extract_ROI(cur.frame.raw, center, kernel)
      
      # Draw movement arrow and trajectory
      arrows(prev_center$x, prev_center$y, center$x, center$y,
             col="yellow", lwd=2.5, length=0.1)
      lines(trajectory[,1], trajectory[,2], col="yellow", lwd=2.5)
      
      if (plot.border) {
        rect(prev_center$x - kernel/2, prev_center$y - kernel/2,
             prev_center$x + kernel/2, prev_center$y + kernel/2,
             border = "red", lwd = 2)
      }
      
      index <- index + 1
    }
    dev.off()
  }
  
  # ---------------------------------------------------------------
  # SAVE RESULTS
  # ---------------------------------------------------------------
  colnames(trajectory) <- c("X", "Y")
  
  # calibrate trajectory to real-world units
  trajectory[,1] <- trajectory[,1]
  trajectory[,2] <- trajectory[,2]
  shifts <- shifts.all
  
  write.csv(trajectory,
            file.path(res.dir, "trajectory_measured.csv"),
            row.names = FALSE)
  
  write.csv(max.crosscorrel,
            file.path(res.dir, "max_cross_correlation.csv"),
            row.names = FALSE)
  
  write.csv(shifts.all,
            file.path(res.dir, "displacement.csv"),
            row.names = FALSE)
  
  return(list(
    center = center,
    shifts = shifts.all,
    trajectory = trajectory,
    max.crosscorrelation = max.crosscorrel
  ))
}
