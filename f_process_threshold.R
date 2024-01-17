f_threshold <-
  function(frame.filter,
           pal,
           bin.dir = NULL,
           show.plot = FALSE,
           save.plot = FALSE) {
    # convert graycale images to binary by threshold
    frame.bin <- frame.filter
    threshold <-
      autothresholdr::auto_thresh(
        frame.filter,
        method = "Otsu",
        ignore_black = FALSE,
        ignore_white = FALSE,
        ignore_na = FALSE
      )[1]
    
    frame.bin[frame.filter > threshold] <- 255
    frame.bin[frame.filter <= threshold] <- 0
    
    if (show.plot == TRUE) {
      par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
      # rotate 90 degrees clockwise to plot with "image"
      rotate <- function(x)
        t(apply(x, 2, rev))
      image(rotate(frame.bin), axes = F, col = pal)
    }
    
    if (save.plot == TRUE) {
      frame <-
        length(list.files(file.path(bin.dir), pattern = ".png")) + 1
      file.out <-
        file.path(bin.dir,
                  paste("image_", paste(rep(
                    "0", 6 - nchar(frame)
                  ), collapse = ""), frame, ".png", sep = ""))
      par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
      # rotate 90 degrees clockwise to plot with "image"
      rotate <- function(x)
        t(apply(x, 2, rev))
      image(rotate(frame.bin), axes = F, col = pal)
      dev.copy(
        device = png,
        filename = file.out,
        width = info$video$width,
        height = info$video$height
      )
      dev.off()
    }
    return('frame.bin' = frame.bin)
  }
