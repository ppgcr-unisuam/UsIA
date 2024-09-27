f_measurement <- function(
    R = NULL,
    G = NULL,
    B = NULL,
    poly = NULL
) {
  # initialize
  threshold <- NA
  ecogenicidade <- NA
  
  # grayscale by luminosity method
  frame.gray <-
    round(0.2989 * R + 0.5870 * G + 0.1140 * B, digits = 0)

  # ecogenicidade (proportion of white pixels)
  try(threshold <-
    autothresholdr::auto_thresh(
      frame.gray,
      method = "Otsu",
      ignore_black = FALSE,
      ignore_white = FALSE,
      ignore_na = FALSE
    )[1], silent = TRUE)
  
  # custom functions
  # extract convex polygon
  source("f_border.R", local = TRUE)
  mask <- fill_border(poly)
  contour <- mask$contour
  frame.gray <- frame.gray * t(mask$output)
  
  frame.bin <- matrix(0, nrow = dim(frame.gray)[1], ncol = dim(frame.gray)[2])
  frame.bin[frame.gray > threshold] <- 1
  frame.bin[frame.gray <= threshold] <- 0
  img_object <- na.omit(as.vector(frame.bin))
  
  # count B&W
  ecogenicidade_bw <- sum(img_object) / length(img_object) * 100
  
  # mean grayscale
  ecogenicidade_gray <- mean(frame.gray, na.rm = TRUE) / 255 * 100
  
  return(list("threshold" = threshold, "ecogenicidade_bw" = ecogenicidade_bw, "ecogenicidade_gray" = ecogenicidade_gray, "mask" = mask$output, "contour" = contour))
}
