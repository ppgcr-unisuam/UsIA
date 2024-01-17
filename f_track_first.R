# function to image reading, processing, and locating of the object within the ROI
# ###################### FIRST FRAME (manual input) ######################
f_first_frame <-
  function(info,
           inputfile,
           raw.dir,
           gray.dir,
           detrend.dir,
           hist.dir,
           filter.dir,
           bin.dir,
           morph.dir,
           out.dir,
           param,
           pal,
           kernel,
           roi,
           center.ini,
           plot.border,
           dsp) {
    # read video and plot frame 1
    j <- 1
    file <-
      file.path(raw.dir, paste("image_", paste(rep("0", 6 - nchar(
        j
      )), collapse = ""), j, ".png", sep = ""))
    # raw frame
    cur.frame.raw <- grDevices::as.raster(magick::image_read(file))
    
    # config and initialize plot
    par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
    plot(
      NULL,
      xlim = c(0, info$video$width),
      ylim = c(0, info$video$height),
      asp = 1,
      col = pal
    )
    par(new = TRUE)
    # show raw frame
    plot(cur.frame.raw, asp = 1, col = pal)
    
    if (!is.na(match("batch.simulation", ls(
      all.names = TRUE, envir = .GlobalEnv
    )))) {
      # use simulation config
      center <- list(x = x0, y = y0)
    } else {
      # plot will not be shown (use previous selection)
      center <-
        list(x = center.ini$x, y = center.ini$y)
    }
    center$x <- round(center$x)
    center$y <- round(center$y)
    
    # object coordinates
    object.ini <-
      cur.frame.raw[(info$video$height - (center$y + floor(kernel / 2))):(info$video$height - (center$y - floor(kernel / 2))),
                    ((center$x - floor(kernel / 2))):((center$x + floor(kernel / 2)))]
    # show object area?
    if (plot.border) {
      rect(
        xleft = (center$x - floor(kernel / 2)),
        ybottom = (center$y - floor(kernel / 2)),
        xright = (center$x + floor(kernel / 2)),
        ytop = (center$y + floor(kernel / 2)),
        border = "red",
        lty = "solid",
        lwd = 2
      )
    }
    
    # region of interest coordinates
    ROI.ini <-
      cur.frame.raw[(info$video$height - (center$y + floor(roi / 2))):(info$video$height - (center$y - floor(roi / 2))),
                    ((center$x - floor(roi / 2))):((center$x + floor(roi / 2)))]
    # show ROI area?
    if (plot.border) {
      rect(
        xleft = (center$x - floor(roi / 2)),
        ybottom = (center$y - floor(roi / 2)),
        xright = (center$x + floor(roi / 2)),
        ytop = (center$y + floor(roi / 2)),
        border = "blue",
        lty = "solid",
        lwd = 2
      )
    }
    
    cur.frame <- cur.frame.raw
    object <- object.ini
    ROI <- ROI.ini
    
    # PRE-PROCESSING #1 COLOR CONVERSION
    if (!is.na(match("gray", dsp))) {
      # PRE-PROCESSING #1 COLOR CONVERSION
      source("f_process_color.R", local = TRUE)
      cur.frame <-
        f_color_conversion(
          frame.raw = cur.frame,
          pal = pal,
          gray.dir = gray.dir,
          show.plot = FALSE,
          save.plot = FALSE
        )
    }
    
    # PRE-PROCESSING #2 DETRENDING
    if (!is.na(match("detrend", dsp))) {
      source('f_process_detrend.R', local = TRUE)
      cur.frame <-
        f_spatial_detrend(
          frame.gray = cur.frame,
          pal = pal,
          detrend.dir = detrend.dir,
          show.plot = FALSE,
          save.plot = FALSE
        )
    }
    
    # PRE-PROCESSING #3 HISTOGRAM EQUALIZATION
    if (!is.na(match("equalize", dsp))) {
      source('f_process_equalization.R', local = TRUE)
      cur.frame <-
        f_equalize(
          frame.detrend = cur.frame,
          pal = pal,
          hist.dir = hist.dir,
          show.plot = FALSE,
          save.plot = FALSE
        )
    }
    
    # PRE-PROCESSING #4 FILTERING
    if (!is.na(match("filter", dsp))) {
      source('f_process_filter.R', local = TRUE)
      cur.frame <-
        f_spatial_filter(
          frame.equalize = cur.frame,
          param = param,
          pal = pal,
          filter.dir = filter.dir,
          show.plot = FALSE,
          save.plot = FALSE
        )
    }
    
    # PRE-PROCESSING #5 INTENSITY THRESHOLD
    if (!is.na(match("threshold", dsp))) {
      source("f_process_threshold.R", local = TRUE)
      cur.frame <-
        f_threshold(
          frame.filter = cur.frame,
          pal = pal,
          bin.dir = bin.dir,
          show.plot = FALSE,
          save.plot = FALSE
        )
    }
    
    # PRE-PROCESSING #6 MORPHOLOGIC OPERATION
    if (!is.na(match("morphologic", dsp))) {
      source("f_process_morphological.R", local = TRUE)
      cur.frame <-
        f_morphologic(
          frame.bin = cur.frame,
          pal = pal,
          morph.dir = morph.dir,
          show.plot = FALSE,
          save.plot = FALSE
        )
    }
    
    # store image as PNG file
    dev.off()
    
    # reset plot
    grDevices::quartz.options(reset = TRUE)
    
    return(
      list(
        'center' = center,
        'cur.frame.raw' = cur.frame.raw,
        'object.ini' = object.ini,
        'ROI.ini' = ROI.ini
      )
    )
  }
