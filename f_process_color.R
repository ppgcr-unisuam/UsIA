f_color_conversion <-
  function(frame.raw = NULL,
           pal = NULL,
           gray.dir = NULL,
           show.plot = FALSE,
           save.plot = FALSE) {
    ix <- rownames(grDevices::col2rgb("#000000"))
    # convert images from RGB to grayscale by averaging channels
    R <- grDevices::col2rgb(frame.raw)[ix == "red", ]
    G <- grDevices::col2rgb(frame.raw)[ix == "green", ]
    B <- grDevices::col2rgb(frame.raw)[ix == "blue", ]
    
    # grayscale by luminosity method
    frame.gray <-
      round(0.2989 * R + 0.5870 * G + 0.1140 * B, digits = 0)
    frame.gray <-
      matrix(
        frame.gray,
        nrow = dim(frame.raw)[1],
        ncol = dim(frame.raw)[2],
        byrow = TRUE
      )
    
    if (show.plot == TRUE) {
      par(mar = rep(0, 4), oma = rep(0, 4))
      # rotate 90 degrees clockwise to plot with "image"
      rotate <- function(x)
        t(apply(x, 2, rev))
      image(rotate(frame.gray), axes = F, col = pal)
    }
    if (save.plot == TRUE) {
      frame <-
        length(list.files(file.path(gray.dir), pattern = ".png")) + 1
      file.out <-
        file.path(gray.dir,
                  paste("image_", paste(rep(
                    "0", 6 - nchar(frame)
                  ), collapse = ""), frame, ".png", sep = ""))
      par(mar = rep(0, 4), oma = rep(0, 4))
      # rotate 90 degrees clockwise to plot with "image"
      rotate <- function(x)
        t(apply(x, 2, rev))
      image(rotate(frame.gray), axes = F, col = pal)
      dev.copy(
        device = png,
        filename = file.out,
        width = info$video$width,
        height = info$video$height
      )
      dev.off()
    }
    return('frame.gray' = frame.gray)
  }
