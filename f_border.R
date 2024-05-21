input <- data.frame(x = c(0, 5, 10, 15, 0),
                    y = c(0, 0, 2, 10, 10))

z <- runif(dim(input)[1])

# interpolate input
output <-
  akima::interp(
    x = input$y,
    y = input$x,
    z = z,
    linear = TRUE,
    extrap = FALSE,
    duplicate = "strip",
    nx = max(input$y) - min(input$y) + 1,
    ny = max(input$x) - min(input$x) + 1
  )

output

# #######

input <- rbind(input, input[1,])

plot(input$x, input$y, type = "l")
# add vertical grid lines
abline(v = seq(0, 15, by = 1), col = "gray", lty = 2)
# add horizontal grid lines
abline(h = seq(0, 10, by = 1), col = "gray", lty = 2)

# #######

xy <- c()
for (i in 1:dim(input)[1]) {
  x0y0 <- c(input$x[i], input$y[i])
  if (i < dim(input)[1]) {
    x1y1 <- c(input$x[i + 1], input$y[i + 1])
    print(rbind(x0y0, x1y1))
    if (x0y0[1] == x1y1[1]) {
      new_y <- seq(x0y0[2], x1y1[2], by = 1 * sign(x1y1[2] - x0y0[2]))
      new_x <- rep(x0y0[1], length(new_y))
      xy <- rbind(xy, cbind(new_x, new_y))
    } else {
      delta.y <- abs(x1y1[1] - x0y0[1])
      delta.x <- abs(x1y1[2] - x0y0[2])
      new_y <- seq(x0y0[2], x1y1[2], by = 1 * sign(delta.x))
      new_x <- seq(x0y0[1], x1y1[1], by = 1 * sign(delta.y))
      xy <- rbind(xy, cbind(new_x, new_y))
    }
  }
  points(xy, col = "red")
  Sys.sleep(2)
}

for(i in 2:dim(xy)[1]) {
  if(all(xy[i,] == xy[i - 1,])){
    xy[i,] <- -1
  }
}
xy <- xy[apply(xy != -1, MARGIN = 1, FUN = all), ]
print(xy)
