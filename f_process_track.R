f_process_track <-
  function(cur.frame.bin,
           track.by,
           object.bin,
           ROI.bin,
           center) {
    
    # -------------------------------------------------------------
    # TRACKING STRATEGY: CROSS-CORRELATION
    # -------------------------------------------------------------
    if (track.by == "cross-correlation") {
      
      # create object (centered) with the size of ROI
      new.object.bin <-
        matrix(
          0,
          nrow = dim(ROI.bin)[1],
          ncol = dim(ROI.bin)[2],
          byrow = FALSE
        )
      
      # ---- ORIGINAL INDEXING PRESERVED EXACTLY ----
      new.object.bin[
        (floor(dim(ROI.bin)[1] / 2) - floor(dim(object.bin)[1] / 2) + 1):
          (floor(dim(ROI.bin)[1] / 2) + floor(dim(object.bin)[1] / 2) + 1),
        
        (floor(dim(ROI.bin)[2] / 2) - floor(dim(object.bin)[2] / 2) + 1):
          (floor(dim(ROI.bin)[2] / 2) + floor(dim(object.bin)[2] / 2) + 1)
      ] <- object.bin
      
      # track movement
      cc <- imagefx::xcorr3d(new.object.bin, ROI.bin)
      
      # fixed: initialize list before indexing
      shifts <- list()
      shifts$max.shifts <- c(cc$max.shifts[1], cc$max.shifts[2])
      
      max.correl <- list()
      max.correl$max.corr <- cc$max.corr
    }
    
    # -------------------------------------------------------------
    # TRACKING STRATEGY: BLOB ANALYSIS
    # -------------------------------------------------------------
    else if (track.by == "blob") {
      
      max.gray <- which(abs(ROI.bin) == max(abs(ROI.bin)), arr.ind = TRUE)
      
      win.size <- 5
      sig <- 25
      
      gaus <- imagefx::build.gaus(
        xdim = nrow(ROI.bin),
        ydim = ncol(ROI.bin),
        sig.x = sig
      )
      
      blob <- imagefx::blob.extract(
        img        = ROI.bin,
        blob.point = max.gray,
        win.size   = win.size,
        gaus       = gaus
      )
      
      blob.coords <- blob$xy.coords
      blob.coords[, 2] <- (blob.coords[, 2] - nrow(cur.frame.bin)) * -1
      
      blob.stats <- imagefx::calc.blob.stats(cur.frame.bin, blob.coords)
      print(blob.stats)
      
      shifts <- list()
      shifts$max.shifts <- c(round(blob.stats[7]), round(blob.stats[8]))
      
      max.correl <- NA
    }
    
    # -------------------------------------------------------------
    # RETURN
    # -------------------------------------------------------------
    return(c('shifts' = shifts, 'max.correl' = max.correl))
  }
