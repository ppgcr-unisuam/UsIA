f_process_track <-
  function(cur.frame.bin,
           track.by,
           object.bin,
           ROI.bin,
           center) {
    # select the tracking strategy
    if (track.by == "cross-correlation") {
      # create object (centered) with the size of ROI
      new.object.bin <-
        matrix(
          0,
          nrow = dim(ROI.bin)[1],
          ncol = dim(ROI.bin)[2],
          byrow = FALSE
        )
      new.object.bin[
        (floor(dim(ROI.bin)[1] / 2) - floor(dim(object.bin)[1] / 2) + 1):(floor(dim(ROI.bin)[1] / 2) + floor(dim(object.bin)[1] / 2) + 1),
        (floor(dim(ROI.bin)[2] / 2) - floor(dim(object.bin)[2] / 2) + 1):(floor(dim(ROI.bin)[2] / 2) + floor(dim(object.bin)[2] / 2) + 1)
      ] <-
        object.bin
      
      # track movement between ROI and object (matrices with the same dimensions)
      cc <- imagefx::xcorr3d(new.object.bin, ROI.bin)
      shifts <- list()
      shifts$max.shifts[1] <- cc$max.shifts[1] # cent_x
      shifts$max.shifts[2] <- cc$max.shifts[2] # cent_y
      max.correl <- list()
      max.correl$max.corr <- cc$max.corr # max cross-correl
    }
    if (track.by == "blob") {
      # using blob analysis from imagefx package
      
      ## find the pixel value location in each channel that deviates most from the mean.
      max.gray = which(abs(ROI.bin) == max(abs(ROI.bin)), arr.ind = TRUE)
      
      ## define a window size used in the connected component algorithm
      win.size = 5
      
      ## define a sigma value for the Gaussian filter
      sig = 25
      
      ## build a Gaussian mask based on this sigma and whose dimensions match the image
      gaus <-
        imagefx::build.gaus(xdim = nrow(ROI.bin),
                   ydim = ncol(ROI.bin),
                   sig.x = sig)

      ## extract the blob from each channel
      blob <-
        imagefx::blob.extract(
          img = ROI.bin,
          blob.point = max.gray,
          win.size = win.size,
          gaus = gaus
        )
      
      ## note the blob points (blob$xy.coords) must be adjusted according to
      ## where the origin (0,0) is located in R image plots
      blob.coords  <- blob$xy.coords
      blob.coords[, 1] <- blob$xy.coords[, 1]
      blob.coords[, 2] <- (blob$xy.coords[, 2] - nrow(cur.frame.bin)) * -1
      
      # output blob coords
      blob.stats <- imagefx::calc.blob.stats(cur.frame.bin, blob.coords)
      print(blob.stats)
      
      shifts <- list()
      shifts$max.shifts[1] <- round(blob.stats[7]) # cent_x
      shifts$max.shifts[2] <- round(blob.stats[8]) # cent_y
      max.correl <- NA
    }
    return(c('shifts' = shifts, 'max.correl' = max.correl))
  }