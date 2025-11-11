f_color_conversion <- function(
    frame.raw = NULL,
    pal = NULL,
    gray.dir = NULL,
    show.plot = FALSE,
    save.plot = FALSE,
    info = NULL
) {
  
  # ---- Safety checks ----
  if (is.null(frame.raw))
    stop("frame.raw cannot be NULL.")
  
  if (!is.array(frame.raw) && !is.matrix(frame.raw))
    stop("frame.raw must be an array or matrix of RGB values.")
  
  if (is.null(pal))
    stop("A color palette 'pal' must be provided for plotting.")
  
  if (save.plot && is.null(gray.dir))
    stop("gray.dir must be provided when save.plot = TRUE.")
  
  if (save.plot && is.null(info))
    stop("info (with video dimensions) must be provided when saving plots.")
  
  # ---- Extract RGB channels ----
  rgb_vals <- grDevices::col2rgb(frame.raw)
  
  R <- rgb_vals["red", ]
  G <- rgb_vals["green", ]
  B <- rgb_vals["blue", ]
  
  # ---- Grayscale conversion (luminosity method) ----
  frame.gray <- round(
    0.2989 * R + 0.5870 * G + 0.1140 * B,
    digits = 0
  )
  
  frame.gray <- matrix(
    frame.gray,
    nrow = dim(frame.raw)[1],
    ncol = dim(frame.raw)[2],
    byrow = TRUE
  )
  
  # ---- Rotation helper ----
  rotate90 <- function(x) t(apply(x, 2, rev))
  
  # ---- Plot grayscale image ----
  if (show.plot) {
    par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
    image(rotate90(frame.gray), axes = FALSE, col = pal)
  }
  
  # ---- Save grayscale image ----
  if (save.plot) {
    if (!dir.exists(gray.dir))
      dir.create(gray.dir, recursive = TRUE)
    
    n <- length(list.files(gray.dir, pattern = "\\.png$")) + 1
    file.out <- file.path(gray.dir, sprintf("image_%06d.png", n))
    
    par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
    image(rotate90(frame.gray), axes = FALSE, col = pal)
    
    dev.copy(
      device = png,
      filename = file.out,
      width = info$video$width,
      height = info$video$height
    )
    dev.off()
  }
  
  return(frame.gray)
}
