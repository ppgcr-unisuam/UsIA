f_spatial_detrend <- function(
    frame.gray,
    pal,
    detrend.dir = NULL,
    show.plot = FALSE,
    save.plot = FALSE,
    info = NULL
) {
  
  # ---- Safety checks ----
  if (is.null(frame.gray))
    stop("frame.gray cannot be NULL.")
  
  if (!is.matrix(frame.gray) && !is.array(frame.gray))
    stop("frame.gray must be a matrix or array.")
  
  if (is.null(pal))
    stop("A color palette 'pal' must be provided for plotting.")
  
  if (save.plot && is.null(detrend.dir))
    stop("detrend.dir must be provided when save.plot = TRUE.")
  
  if (save.plot && is.null(info))
    stop("info (with video dimensions) must be provided for saving images.")
  
  if (save.plot && !dir.exists(detrend.dir))
    dir.create(detrend.dir, recursive = TRUE)
  
  # ---- Rotation helper ----
  rotate90 <- function(x) t(apply(x, 2, rev))
  
  # ---- Summary stats ----
  frame.min  <- min(frame.gray)
  frame.mean <- mean(frame.gray)
  frame.max  <- max(frame.gray)
  
  # ---- Mean-center the image ----
  img.gray.dmean <- frame.gray - frame.mean
  
  # ---- Fit trend plane using imagefx::fit3d ----
  img.gray.trend <- imagefx::fit3d(img.gray.dmean)
  
  # ---- Remove trend (detrend) and add mean back ----
  frame.detrend <- img.gray.dmean - img.gray.trend + frame.mean
  
  # ---- Rescale to original dynamic range ----
  frame.detrend <- scales::rescale(frame.detrend, to = c(frame.min, frame.max))
  
  # ---- Plot ----
  if (show.plot) {
    par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
    image(rotate90(frame.detrend), axes = FALSE, col = pal)
  }
  
  # ---- Save plot ----
  if (save.plot) {
    
    # sequential file numbering
    n <- length(list.files(detrend.dir, pattern = "\\.png$")) + 1
    file.out <- file.path(detrend.dir, sprintf("image_%06d.png", n))
    
    par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
    image(rotate90(frame.detrend), axes = FALSE, col = pal)
    
    dev.copy(
      device = png,
      filename = file.out,
      width  = info$video$width,
      height = info$video$height
    )
    dev.off()
  }
  
  return(frame.detrend)
}
