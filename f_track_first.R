f_first_frame <- function(info,
                          inputfile,
                          raw.dir,
                          gray.dir,
                          detrend.dir,
                          hist.dir,
                          filter.dir,
                          bin.dir,
                          morph.dir,
                          out.dir,
                          param,
                          pal,
                          kernel,
                          roi,
                          center.ini,
                          plot.border,
                          dsp,
                          show.plot,
                          save.plot) {
  
  # ===============================================================
  # Helpers internos
  # ===============================================================
  
  # Carrega um frame RAW a partir do disco
  load_frame <- function(i) {
    file.path(raw.dir, sprintf("image_%06d.png", i)) |>
      magick::image_read() |>
      grDevices::as.raster()
  }
  
  # Extrai ROI/object a partir de um centro
  extract_square <- function(frame, center, size) {
    half <- floor(size / 2)
    y1 <- info$video$height - (center$y + half)
    y2 <- info$video$height - (center$y - half)
    x1 <- center$x - half
    x2 <- center$x + half
    frame[y1:y2, x1:x2]
  }
  
  # Pipeline unificado de pré-processamento
  preprocess <- function(f) {
    if ("gray"       %in% dsp) f <- f_color_conversion(f, pal, gray.dir, FALSE, FALSE, info)
    if ("detrend"    %in% dsp) f <- f_spatial_detrend(f, pal, detrend.dir, FALSE, FALSE, info)
    if ("equalize"   %in% dsp) f <- f_equalize(f, pal, hist.dir, FALSE, FALSE, info)
    if ("filter"     %in% dsp) f <- f_spatial_filter(f, param, pal, filter.dir, FALSE, FALSE, info)
    if ("threshold"  %in% dsp) f <- f_threshold(f, pal, bin.dir, FALSE, FALSE, info)
    if ("morphologic"%in% dsp) f <- f_morphologic(f, pal, morph.dir, FALSE, FALSE, info)
    f
  }
  
  # Pequeno utilitário para saber se estamos em modo simulado
  batch_mode <- "batch.simulation" %in% ls(all.names = TRUE, envir = .GlobalEnv)
  
  # ===============================================================
  # Leitura do frame 1
  # ===============================================================
  
  j <- 1
  cur.frame.raw <- load_frame(j)
  
  # ===============================================================
  # Setup do plot
  # ===============================================================
  
  par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
  plot(NULL, xlim = c(0, info$video$width), ylim = c(0, info$video$height),
       asp = 1, col = pal)
  par(new = TRUE)
  plot(cur.frame.raw, asp = 1, col = pal)
  
  # ===============================================================
  # Determinação do centro (manual ou simulado)
  # ===============================================================
  
  if (batch_mode) {
    center <- list(x = x0, y = y0)
  } else {
    center <- list(x = center.ini$x, y = center.ini$y)
  }
  
  center$x <- round(center$x)
  center$y <- round(center$y)
  
  # ===============================================================
  # Object e ROI iniciais
  # ===============================================================
  
  object.ini <- extract_square(cur.frame.raw, center, kernel)
  ROI.ini    <- extract_square(cur.frame.raw, center, roi)
  
  if (plot.border) {
    rect(center$x - kernel/2, center$y - kernel/2,
         center$x + kernel/2, center$y + kernel/2,
         border = "red", lwd = 2)
    rect(center$x - roi/2, center$y - roi/2,
         center$x + roi/2, center$y + roi/2,
         border = "blue", lwd = 2)
  }
  
  # ===============================================================
  # Pré-processamento do frame inicial (objeto e ROI não processam)
  # ===============================================================
  
  cur.frame <- preprocess(cur.frame.raw)
  
  # ===============================================================
  # Finaliza plot
  # ===============================================================
  
  dev.off()
  grDevices::quartz.options(reset = TRUE)
  
  # ===============================================================
  # Retorno padronizado
  # ===============================================================
  
  list(
    center        = center,
    cur.frame.raw = cur.frame.raw,
    object.ini    = object.ini,
    ROI.ini       = ROI.ini
  )
}
