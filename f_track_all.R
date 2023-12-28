# function to image reading, processing, and locating of the object within the ROI
# ###################### ALL FRAMES (frame 1 repeated, no plot window) ######################
f_all_frames <-
  function(inputfile,
           info,
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
           dsp) {
    # Splits a video file in a set of image files. Use format = "png" for losless images
    av::av_video_images(inputfile,
                        destdir = raw.dir,
                        format = "png",
                        fps = NULL)
    
    # initialize output vectors
    trajectory <- matrix(NA, ncol = 2, nrow = 0)
    max.crosscorrel <- matrix(NA, ncol = 1, nrow = 0)
    shifts.all <- matrix(NA, ncol = 1, nrow = 0)
    past.objects <- list()
    
    # generating sequence of frames for possible jump
    iter <- seq(
      from = 1,
      to = info$video$frames,
      by = as.numeric(param$Jump) + 1
    )
    index <- 1
    
    # for each frame in dir
    for (i in 1:(info$video$frames - as.numeric(param$Jump))) {
      # initialize plot
      grDevices::png(
        file.path(out.dir, paste(
          "image_", paste(rep("0", 6 - nchar(i)), collapse = ""), i, ".png", sep = ""
        )),
        width = (info$video$width),
        height = (info$video$height),
        units = "px",
        res = 1,
        type = "cairo"
      )
      
      # check jump
      if (i == iter[index]) {
        # check iteration
        if (i == 1) {
          # read and process 1st frame
          run_1st <- f_first_frame(
            info = info,
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
            dsp = dsp
          )
          # use first object & ROI
          cur.frame.raw <- run_1st$cur.frame.raw
          object.raw <- run_1st$object.ini
          ROI.raw <- run_1st$ROI.ini
          past.objects[[i]] <- object.raw
          
          # update vectors
          center <- run_1st$center
          trajectory <- rbind(trajectory, c(center$x, center$y))
        } else {
          # read all (i > 1) images, process image and record a video
          file <-
            file.path(raw.dir,
                      paste("image_", paste(rep(
                        "0", 6 - nchar(i)
                      ), collapse = ""), i, ".png", sep = ""))
          # raw frame
          cur.frame.raw <-
            grDevices::as.raster(magick::image_read(file))
          
          # config and initialize plot
          par(mar = rep(0, 4), oma = rep(0, 4))
          plot(
            NULL,
            xlim = c(0, info$video$width),
            ylim = c(0, info$video$height),
            asp = NA,
            col = pal
          )
          par(new = TRUE)
          # SHOW CURRENT FRAME RAW
          plot(cur.frame.raw, asp = NA, col = pal)
          
          # update ROI based on the previous frame
          ROI.raw <- cur.frame.raw[(info$video$height - (trajectory[index - 1, 2] + floor(roi / 2))):(info$video$height - (trajectory[index - 1, 2] - floor(roi / 2))),
                                   ((trajectory[index - 1, 1] - floor(roi / 2))):((trajectory[index - 1, 1] + floor(roi / 2)))]
          # show ROI area?
          if (plot.border) {
            rect(
              xleft = (trajectory[index - 1, 1] - floor(roi / 2)),
              ybottom = (trajectory[index - 1, 2] - floor(roi / 2)),
              xright = (trajectory[index - 1, 1] + floor(roi / 2)),
              ytop = (trajectory[index - 1, 2] + floor(roi / 2)),
              border = "blue",
              lty = "solid",
              lwd = 2
            )
          }
          # CHECK WHETER ROI TOUCHES ANY BORDERS
          if (any(
            center$x - floor(roi / 2) <= 0,
            center$y - floor(roi / 2) <= 0,
            center$x + floor(roi / 2) >= info$video$width,
            center$y + floor(roi / 2) >= info$video$height
          )) {
            stop("ROI is touching the image borders. The function was interrupted.")
          }
          
          # define input data
          cur.frame <- cur.frame.raw # current frame
          object <- past.objects[[index - 1]] # previous frame
          ROI <- ROI.raw # current frame
          
          # PRE-PROCESSING #1 COLOR CONVERSION
          if (!is.na(match("gray", dsp))) {
            source("f_process_color.R", local = TRUE)
            cur.frame <-
              f_color_conversion(
                frame.raw = cur.frame,
                pal = pal,
                gray.dir = gray.dir,
                show.plot = FALSE,
                save.plot = FALSE
              )
            object <-
              f_color_conversion(
                frame.raw = object,
                pal = pal,
                gray.dir = gray.dir,
                show.plot = FALSE,
                save.plot = FALSE
              )
            ROI <-
              f_color_conversion(
                frame.raw = ROI,
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
            object <-
              f_spatial_detrend(
                frame.gray = object,
                pal = pal,
                detrend.dir = detrend.dir,
                show.plot = FALSE,
                save.plot = FALSE
              )
            ROI <-
              f_spatial_detrend(
                frame.gray = ROI,
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
            object <-
              f_equalize(
                frame.detrend = object,
                pal = pal,
                hist.dir = hist.dir,
                show.plot = FALSE,
                save.plot = FALSE
              )
            ROI <-
              f_equalize(
                frame.detrend = ROI,
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
            object <-
              f_spatial_filter(
                frame.equalize = object,
                param = param,
                pal = pal,
                filter.dir = filter.dir,
                show.plot = FALSE,
                save.plot = FALSE
              )
            ROI <-
              f_spatial_filter(
                frame.equalize = ROI,
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
            object <-
              f_threshold(
                frame.filter = object,
                pal = pal,
                bin.dir = bin.dir,
                show.plot = FALSE,
                save.plot = FALSE
              )
            ROI <-
              f_threshold(
                frame.filter = ROI,
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
            object <-
              f_morphologic(
                frame.bin = object,
                pal = pal,
                morph.dir = morph.dir,
                show.plot = FALSE,
                save.plot = FALSE
              )
            ROI <-
              f_morphologic(
                frame.bin = ROI,
                pal = pal,
                morph.dir = morph.dir,
                show.plot = FALSE,
                save.plot = FALSE
              )
          }
          
          # TRACK OBJECT IN ROI
          source("f_process_track.R", local = TRUE)
          track <-
            f_process_track(
              cur.frame.bin = cur.frame,
              track.by = track.by,
              object.bin = object,
              ROI.bin = ROI,
              center = center
            )
          center$x <-
            trajectory[index - 1, 1] + track$shifts.max.shifts[2]
          center$y <-
            trajectory[index - 1, 2] - track$shifts.max.shifts[1]
          
          # update vectors
          trajectory <- rbind(trajectory, c(center$x, center$y))
          max.crosscorrel <-
            c(max.crosscorrel, track$max.correl.max.corr)
          shifts.all <-
            c(shifts.all, round(
              sqrt(
                track$shifts.max.shifts[1] ^ 2 + track$shifts.max.shifts[2] ^ 2
              ),
              0
            ))
          # object coordinates
          past.objects[[i]] <- cur.frame.raw[(info$video$height - (center$y + floor(kernel / 2))):(info$video$height - (center$y - floor(kernel / 2))),
                                             ((center$x - floor(kernel / 2))):((center$x + floor(kernel / 2)))]
          
          # add arrows indicating how the image shifted
          graphics::arrows(
            trajectory[index - 1, 1],
            trajectory[index - 1, 2],
            center$x,
            center$y,
            col = "yellow",
            lwd = 2.5,
            lty = "solid",
            length = 0.1 # inches
          )
          graphics::lines(
            trajectory[, 1],
            trajectory[, 2],
            col = "yellow",
            lwd = 2.5,
            lty = "solid"
          )
          # show object area?
          if (plot.border) {
            graphics::rect(
              xleft = (trajectory[index - 1, 1] - floor(kernel / 2)),
              ybottom = (trajectory[index - 1, 2] - floor(kernel / 2)),
              xright = (trajectory[index - 1, 1] + floor(kernel / 2)),
              ytop = (trajectory[index - 1, 2] + floor(kernel / 2)),
              border = "red",
              lty = "solid",
              lwd = 2
            )
          }
          
          # store image as PNG file
          grDevices::dev.off()
        }
        index <- index + 1
      }
    }
    
    # store data
    colnames(trajectory) <- c("X", "Y")
    write.csv(
      trajectory,
      file.path(res.dir, "trajectory_measured.csv"),
      row.names = FALSE,
      col.names = TRUE
    )
    names(max.crosscorrel) <- c("CCmax")
    write.csv(
      max.crosscorrel,
      file.path(res.dir, "max_cross_correlation.csv"),
      row.names = FALSE,
      col.names = TRUE
    )
    names(shifts.all) <- c("D")
    write.csv(
      shifts.all,
      file.path(res.dir, "displacement.csv"),
      row.names = FALSE,
      col.names = TRUE
    )
    return(list('center' = run_1st$center))
  }
