f_equalize <- function(
    frame.detrend,
    pal,
    hist.dir = NULL,
    show.plot = FALSE,
    save.plot = FALSE,
    info = NULL
) {
  
  # ---- Safety checks ----
  if (is.null(frame.detrend))
    stop("frame.detrend cannot be NULL.")
  
  if (!is.matrix(frame.detrend) && !is.array(frame.detrend))
    stop("frame.detrend must be a matrix or array.")
  
  if (is.null(pal))
    stop("A color palette 'pal' must be provided for plotting.")
  
  if (save.plot && is.null(hist.dir))
    stop("hist.dir must be provided when save.plot = TRUE.")
  
  if (save.plot && is.null(info))
    stop("info (with video dimensions) must be provided for saving images.")
  
  if (save.plot && !dir.exists(hist.dir))
    dir.create(hist.dir, recursive = TRUE)
  
  # ---- Rotation helper ----
  rotate90 <- function(x) t(apply(x, 2, rev))
  
  # ---- Histogram equalization ----
  frame.equalize <- round(
    EBImage::equalize(
      frame.detrend,
      range  = c(0, 255),
      levels = 256
    ),
    0
  )
  
  # ---- Plot ----
  if (show.plot) {
    par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
    image(rotate90(frame.equalize), axes = FALSE, col = pal)
  }
  
  # ---- Save plot ----
  if (save.plot) {
    
    # sequential numbering
    n <- length(list.files(hist.dir, pattern = "\\.png$")) + 1
    file.out <- file.path(hist.dir, sprintf("image_%06d.png", n))
    
    par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
    image(rotate90(frame.equalize), axes = FALSE, col = pal)
    
    dev.copy(
      device = png,
      filename = file.out,
      width  = info$video$width,
      height = info$video$height
    )
    dev.off()
  }
  
  return(frame.equalize)
}
