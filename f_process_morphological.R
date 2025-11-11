f_morphologic <- function(
    frame.bin,
    pal,
    morph.dir = NULL,
    show.plot = FALSE,
    save.plot = FALSE,
    info = NULL
) {
  
  # ---- Safety checks ----
  if (is.null(frame.bin))
    stop("frame.bin cannot be NULL.")
  
  if (!is.matrix(frame.bin) && !is.array(frame.bin))
    stop("frame.bin must be a matrix or array (binary/grayscale image).")
  
  if (is.null(pal))
    stop("A color palette 'pal' must be provided for plotting.")
  
  if (save.plot && is.null(morph.dir))
    stop("morph.dir must be provided when save.plot = TRUE.")
  
  if (save.plot && is.null(info))
    stop("info (with video dimensions) must be provided for saving images.")
  
  if (save.plot && !dir.exists(morph.dir))
    dir.create(morph.dir, recursive = TRUE)
  
  # ---- Rotation helper ----
  rotate90 <- function(x) t(apply(x, 2, rev))
  
  # ---- Morphological operations ----
  # Opening = erosion → dilation
  # Closing = dilation → erosion
  k <- mmand::shapeKernel(c(3, 3), type = "diamond")
  frame.morph <- mmand::closing(
    mmand::opening(frame.bin, k),
    k
  )
  
  # ---- Plot ----
  if (show.plot) {
    par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
    image(rotate90(frame.morph), axes = FALSE, col = pal)
  }
  
  # ---- Save plot ----
  if (save.plot) {
    
    n <- length(list.files(morph.dir, pattern = "\\.png$")) + 1
    file.out <- file.path(morph.dir, sprintf("image_%06d.png", n))
    
    par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
    image(rotate90(frame.morph), axes = FALSE, col = pal)
    
    dev.copy(
      device   = png,
      filename = file.out,
      width    = info$video$width,
      height   = info$video$height
    )
    dev.off()
  }
  
  return(frame.morph)
}
