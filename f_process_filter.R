f_spatial_filter <- function(
    frame.equalize,
    param,
    pal,
    filter.dir = NULL,
    show.plot = FALSE,
    save.plot = FALSE,
    info = NULL
) {
  
  # ---- Safety checks ----
  if (is.null(frame.equalize))
    stop("frame.equalize cannot be NULL.")
  
  if (is.null(param))
    stop("param list must be provided.")
  
  if (is.null(param$`Filter type`))
    stop("param$'Filter type' must exist.")
  
  if (is.null(param$`Filter size`))
    stop("param$'Filter size' must exist.")
  
  if (is.null(pal))
    stop("A color palette 'pal' must be provided for plotting.")
  
  if (save.plot && is.null(filter.dir))
    stop("filter.dir must be provided when save.plot = TRUE.")
  
  if (save.plot && is.null(info))
    stop("info (with video dimensions) must be provided when saving images.")
  
  if (save.plot && !dir.exists(filter.dir))
    dir.create(filter.dir, recursive = TRUE)
  
  # ---- Rotation helper ----
  rotate90 <- function(x) t(apply(x, 2, rev))
  
  # ---- Extract parameters ----
  filter.type <- as.character(param$`Filter type`)
  size        <- as.numeric(param$`Filter size`)
  
  # ---- No filtering ----
  if (filter.type == "none" || size == 0) {
    frame.filter <- frame.equalize
    
  } else {
    
    # Ensure size is odd (common requirement for kernels)
    if (size %% 2 == 0)
      stop("Filter size must be an odd integer.")
    
    # Create kernel
    k <- mmand::shapeKernel(c(size, size), type = "box")
    
    # ---- Apply filter selected ----
    frame.filter <- switch(
      filter.type,
      "mean"   = mmand::meanFilter(frame.equalize, k),
      "median" = mmand::medianFilter(frame.equalize, k),
      stop("Unknown Filter type: must be 'none', 'mean', or 'median'")
    )
  }
  
  # ---- Plot ----
  if (show.plot) {
    par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
    image(rotate90(frame.filter), axes = FALSE, col = pal)
  }
  
  # ---- Save plot ----
  if (save.plot) {
    n <- length(list.files(filter.dir, pattern = "\\.png$")) + 1
    file.out <- file.path(filter.dir, sprintf("image_%06d.png", n))
    
    par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
    image(rotate90(frame.filter), axes = FALSE, col = pal)
    
    dev.copy(
      device = png,
      filename = file.out,
      width  = info$video$width,
      height = info$video$height
    )
    dev.off()
  }
  
  return(frame.filter)
}
