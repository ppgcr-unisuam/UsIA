f_spatial_detrend <-
  function(frame.gray,
           pal,
           detrend.dir = NULL,
           show.plot = FALSE,
           save.plot = FALSE) {
    # subtract the mean from each color channel matrix
    frame.min <- min(frame.gray)
    frame.mean <- mean(frame.gray)
    frame.max <- max(frame.gray)
    
    img.gray.dmean <- frame.gray - frame.mean
    
    # Find the best fit plane (trend) in each color channel matrix
    img.gray.trend <- imagefx::fit3d(img.gray.dmean)
    
    ## subtract the fitted plane from each color channel (i.e. detrend) and add the mean back
    frame.detrend <- img.gray.dmean - img.gray.trend + frame.mean
    
    # rescale to fit min-max of original image
    frame.detrend  <-
      scales::rescale(frame.detrend, to = c(frame.min, frame.max))
    
    if (show.plot == TRUE) {
      par(mar = rep(0, 4), oma = rep(0, 4))
      # rotate 90 degrees clockwise to plot with "image"
      rotate <- function(x)
        t(apply(x, 2, rev))
      image(rotate(frame.detrend), axes = F, col = pal)
    }
    if (save.plot == TRUE) {
      frame <-
        length(list.files(file.path(detrend.dir), pattern = ".png")) + 1
      file.out <-
        file.path(detrend.dir,
                  paste("image_", paste(rep(
                    "0", 6 - nchar(frame)
                  ), collapse = ""), frame, ".png", sep = ""))
      par(mar = rep(0, 4), oma = rep(0, 4))
      # rotate 90 degrees clockwise to plot with "image"
      rotate <- function(x)
        t(apply(x, 2, rev))
      image(rotate(frame.detrend), axes = F, col = pal)
      dev.copy(
        device = png,
        filename = file.out,
        width = info$video$width,
        height = info$video$height
      )
      dev.off()
    }
    return('frame.detrend' = frame.detrend)
  }
