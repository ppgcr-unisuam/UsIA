f_threshold <- function(
    frame.filter,
    pal,
    bin.dir = NULL,
    show.plot = FALSE,
    save.plot = FALSE,
    info = NULL
) {
  
  # ---- Safety checks ----
  if (is.null(frame.filter))
    stop("frame.filter cannot be NULL.")
  
  if (!is.matrix(frame.filter) && !is.array(frame.filter))
    stop("frame.filter must be a matrix or array.")
  
  if (is.null(pal))
    stop("A color palette 'pal' must be provided.")
  
  if (save.plot && is.null(bin.dir))
    stop("bin.dir must be provided when save.plot = TRUE.")
  
  if (save.plot && is.null(info))
    stop("info (with video dimensions) must be provided for saving images.")
  
  if (save.plot && !dir.exists(bin.dir))
    dir.create(bin.dir, recursive = TRUE)
  
  
  # ---- Rotation helper ----
  rotate90 <- function(x) t(apply(x, 2, rev))
  
  
  # ---- Compute threshold (Otsu) ----
  threshold <- autothresholdr::auto_thresh(
    frame.filter,
    method       = "Otsu",
    ignore_black = FALSE,
    ignore_white = FALSE,
    ignore_na    = FALSE
  )[1]
  
  # ---- Apply threshold ----
  frame.bin <- ifelse(frame.filter > threshold, 255, 0)
  
  # ---- Plot ----
  if (show.plot) {
    par(mar = rep(0,4), oma = rep(0,4), omi = rep(0,4), mai = rep(0,4))
    image(rotate90(frame.bin), axes = FALSE, col = pal)
  }

  # ---- Save plot ----
  if (save.plot) {
    n <- length(list.files(bin.dir, pattern = "\\.png$")) + 1
    file.out <- file.path(bin.dir, sprintf("image_%06d.png", n))
    
    par(mar = rep(0,4), oma = rep(0,4), omi = rep(0,4), mai = rep(0,4))
    image(rotate90(frame.bin), axes = FALSE, col = pal)
    
    dev.copy(
      device   = png,
      filename = file.out,
      width    = info$video$width,
      height   = info$video$height
    )
    dev.off()
  }
  
  return(frame.bin)
}
