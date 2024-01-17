f_morphologic <-
  function(frame.bin,
           pal,
           morph.dir = NULL,
           show.plot = FALSE,
           save.plot = FALSE) {
    # perform opening (erosion -> dilation) then closing (dilation -> closing) operations
    k <- mmand::shapeKernel(c(3, 3), type = "diamond")
    frame.morph <- mmand::closing(mmand::opening(frame.bin, k), k)
    
    if (show.plot == TRUE) {
      par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
      # rotate 90 degrees clockwise to plot with "image"
      rotate <- function(x)
        t(apply(x, 2, rev))
      image(rotate(frame.morph), axes = F, col = pal)
    }
    
    if (save.plot == TRUE) {
      frame <-
        length(list.files(file.path(morph.dir), pattern = ".png")) + 1
      file.out <-
        file.path(morph.dir,
                  paste("image_", paste(rep(
                    "0", 6 - nchar(frame)
                  ), collapse = ""), frame, ".png", sep = ""))
      par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
      # rotate 90 degrees clockwise to plot with "image"
      rotate <- function(x)
        t(apply(x, 2, rev))
      image(rotate(frame.morph), axes = F, col = pal)
      dev.copy(
        device = png,
        filename = file.out,
        width = info$video$width,
        height = info$video$height
      )
      dev.off()
    }
    return('frame.morph' = frame.morph)
  }
