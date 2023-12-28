f_spatial_filter <-
  function(frame.equalize,
           param,
           pal,
           filter.dir = NULL,
           show.plot = FALSE,
           save.plot = FALSE) {
    if (param$"Filter type" == "none") {
      # initialize matrix and pass value if no filtering is applied
      frame.filter <- frame.equalize
    }
    # apply spatial filter
    if (param$"Filter type" != "mean") {
      filter <- as.character(param$"Filter type")
      size <-
        as.numeric(param$"Filter size") # (pixels, odd values only)
      if (size != 0) {
        k <- mmand::shapeKernel(c(size, size), type = "box")
        frame.filter <- mmand::meanFilter(frame.equalize, k)
      }
    }
    if (param$"Filter type" != "median") {
      filter <- as.character(param$"Filter type")
      size <-
        as.numeric(param$"Filter size") # (pixels, odd values only)
      if (size != 0) {
        k <- mmand::shapeKernel(c(size, size), type = "box")
        frame.filter <- mmand::medianFilter(frame.equalize, k)
      }
    }
    if (show.plot == TRUE) {
      par(mar = rep(0, 4), oma = rep(0, 4))
      # rotate 90 degrees clockwise to plot with "image"
      rotate <- function(x)
        t(apply(x, 2, rev))
      image(rotate(frame.filter), axes = F, col = pal)
    }
    if (save.plot == TRUE) {
      frame <-
        length(list.files(file.path(filter.dir), pattern = ".png")) + 1
      file.out <-
        file.path(filter.dir,
                  paste("image_", paste(rep(
                    "0", 6 - nchar(frame)
                  ), collapse = ""), frame, ".png", sep = ""))
      par(mar = rep(0, 4), oma = rep(0, 4))
      # rotate 90 degrees clockwise to plot with "image"
      rotate <- function(x)
        t(apply(x, 2, rev))
      image(rotate(frame.filter), axes = F, col = pal)
      dev.copy(
        device = png,
        filename = file.out,
        width = info$video$width,
        height = info$video$height
      )
      dev.off()
    }
    return('frame.filter' = frame.filter)
  }
