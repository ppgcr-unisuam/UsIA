f_measurement <- function(
    R = NULL,
    G = NULL,
    B = NULL
) {
  
  # grayscale by luminosity method
  frame.gray <-
    round(0.2989 * R + 0.5870 * G + 0.1140 * B, digits = 0)
  
  # initialize
  threshold <- NA
  ecogenicidade <- NA
  
  # ecogenicidade (proportion of white pixels)
  try(threshold <-
    autothresholdr::auto_thresh(
      frame.gray,
      method = "Otsu",
      ignore_black = FALSE,
      ignore_white = FALSE,
      ignore_na = FALSE
    )[1], silent = TRUE)
  
  frame.bin <- matrix(0, nrow = dim(frame.gray)[1], ncol = dim(frame.gray)[2])
  frame.bin[frame.gray > threshold] <- 1
  frame.bin[frame.gray <= threshold] <- 0
  img_object <- as.vector(frame.bin)
  
  # count
  ecogenicidade <- sum(img_object) / length(img_object) * 100
  
  # mean
  ecogenicidade <- mean(frame.gray) / 255 * 100
  
  return(list("threshold" = threshold, "ecogenicidade" = ecogenicidade))
}
