fill_border <- function(poly) {
  # rename poly with x, y
  input <- data.frame(x = poly[, 1], y = poly[, 2])
  
  # input <- data.frame(x = c(6, 8, 9, 15, 10, 12, 10, 5),
  #                     y = c(5, 2, 5, 9, 10, 18, 20, 19))
  
  # generate random data for a polygon
  # input <- data.frame(x = round(runif(8, 0, 20)),
  #                     y = round(runif(8, 0, 20)))
  # get convex hull
  # ch <- chull(input)
  # input <- input[ch,]
  
  input <- rbind(input, input[1,])

  # interpolate input
  output <-
    akima::interp(
      x = input$y,
      y = input$x,
      z = runif(dim(input)[1]),
      linear = TRUE,
      extrap = FALSE,
      duplicate = "strip",
      nx = max(input$y) - min(input$y) + 1,
      ny = max(input$x) - min(input$x) + 1
    )
  
  xy <- c()
  for (i in 1:dim(input)[1]) {
    x0y0 <- c(input$x[i], input$y[i])
    if (i < dim(input)[1]) {
      x1y1 <- c(input$x[i + 1], input$y[i + 1])
      # testa se a trajetória é nula
      if (all(x0y0 == x1y1)) {
        xy <- rbind(xy, x0y0)
      }
      # testa se a trajetória é horizontal
      else if (x0y0[2] == x1y1[2]) {
        new_x <- seq(x0y0[1], x1y1[1], by = 1 * sign(x1y1[1] - x0y0[1]))
        new_y <- rep(x0y0[2], length(new_x))
        xy <- rbind(xy, cbind(new_x, new_y))
      }
      # testa se a trajetória é vertical
      else if (x0y0[1] == x1y1[1]) {
        new_y <- seq(x0y0[2], x1y1[2], by = 1 * sign(x1y1[2] - x0y0[2]))
        new_x <- rep(x0y0[1], length(new_y))
        xy <- rbind(xy, cbind(new_x, new_y))
      }
      # a trajetória é inclinada, crie pontos para incluir o maior deslocamento
      else {
        slope <- (x1y1[2] - x0y0[2]) / (x1y1[1] - x0y0[1])
        if (abs(slope) > 1) {
          new_y <- seq(x0y0[2], x1y1[2], by = 1 * sign(x1y1[2] - x0y0[2]))
          new_x <- x0y0[1] + (new_y - x0y0[2]) / slope
        } else {
          new_x <- seq(x0y0[1], x1y1[1], by = 1 * sign(x1y1[1] - x0y0[1]))
          new_y <- x0y0[2] + (new_x - x0y0[1]) * slope
        }
        xy <- rbind(xy, cbind(new_x, new_y))
      }
    }
  }
  xy <- round(xy) %>%
    data.frame()
  
  # remove duplicate points in sequence
  for(i in 2:(dim(xy)[1])) {
    if(all(xy[i,] == xy[i - 1,])){
      xy[i,] <- -1
    }
  }
  xy <- xy[apply(xy != -1, MARGIN = 1, FUN = all), ]
  
  # add 1 to indexes to match output matrix
  xy <- xy + 1
  for (i in 1:dim(xy)[1]) {
    output$z[xy[i, 2] - min(output$x), xy[i, 1] - min(output$y)] <- 1
  }
  
  # subtract 1
  xy <- xy - 1
  
  # # fill output$z matrix
  output$z <- EBImage::fillHull(output$z)
  
  # replace 0 with NA
  output$z[output$z == 0] <- NA
  
  # # overlay plots with output in the background
  # plot(input$x, input$y, type = "l", xlim = c(0, 50), ylim = c(0, 50))
  # # add vertical grid lines
  # abline(v = seq(min(xy), max(xy), by = 1), col = "gray", lty = 2)
  # # add horizontal grid lines
  # abline(h = seq(min(xy), max(xy), by = 1), col = "gray", lty = 2)
  # image.default(output$y + 0.0, output$x + 0.0, t(output$z), add = TRUE, col = c(NA, "grey"))
  # points(xy, col = "red", pch = 19)
  # lines(input$x, input$y, type = "l")
  
  return(list("output" = output$z, "contour" = xy))
}
