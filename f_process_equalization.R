f_equalize <-
  function(frame.detrend,
           pal,
           hist.dir = NULL,
           show.plot = FALSE,
           save.plot = FALSE) {
    # convert graycale images to binary by threshold
    frame.equalize <-
      round(EBImage::equalize(frame.detrend, range = c(0, 255), levels = 256), 0)
    
    if (show.plot == TRUE) {
      par(mar = rep(0, 4), oma = rep(0, 4))
      # rotate 90 degrees clockwise to plot with "image"
      rotate <- function(x)
        t(apply(x, 2, rev))
      image(rotate(frame.equalize), axes = F, col = pal)
    }
    
    if (save.plot == TRUE) {
      frame <-
        length(list.files(file.path(hist.dir), pattern = ".png")) + 1
      file.out <-
        file.path(hist.dir,
                  paste("image_", paste(rep(
                    "0", 6 - nchar(frame)
                  ), collapse = ""), frame, ".png", sep = ""))
      par(mar = rep(0, 4), oma = rep(0, 4))
      # rotate 90 degrees clockwise to plot with "image"
      rotate <- function(x)
        t(apply(x, 2, rev))
      image(rotate(frame.equalize), axes = F, col = pal)
      dev.copy(
        device = png,
        filename = file.out,
        width = info$video$width,
        height = info$video$height
      )
      dev.off()
    }
    return('frame.equalize' = frame.equalize)
  }
