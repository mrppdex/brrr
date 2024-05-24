# using axis$axis_function, and other elements returned from plot_axis
# create a function that does forest plot

plot_horizontal_seg <- function(box, x, n1, n2=1, N2=1, lty=1, lwd=3, col='black') {

  xvec <- sapply(x, function(x) box$axis$axis_function(x))
  yvec <- box$y_pos[n1] + (box$y_pos[n1+1] - box$y_pos[n1])*n2/(N2+1)

  grid.lines(x = xvec, y = rep(yvec, 2), 
             gp = gpar(lty = lty, lwd = lwd, col = col))
    
}

plot_dot <- function(box, x, n1, n2=1, N2=1, pch=19, size=unit(1, 'char'), col='black') {

  xvec <- sapply(x, function(x) box$axis$axis_function(x))
  yvec <- box$y_pos[n1] + (box$y_pos[n1+1] - box$y_pos[n1])*n2/(N2+1)
  
  if (N2>1) {
    pch <- 15 + (n2-1) %% 6
  }

  grid.points(x = unit(xvec, 'npc'), y = unit(yvec, 'npc'),
        pch=pch, size=size, gp = gpar(col = col))
  
}

# using horizontal segment and dot, plot a forest plot
plot_forest_tree <- function(box, x_lower, x_upper, x_dot, n1, n2=1, N2=1, col='purple') {
  
  plot_horizontal_seg(box, c(x_lower, x_upper), n1, n2, N2, col=col)
  plot_dot(box, x_dot, n1, n2, N2, col=col)
  
}

# test

# grid.newpage()

# # create a box
# box2 <- plot_box(0.5, 0.5, 1, 3, 0.02, 3, 5, 1, -2, 'value', FALSE, show_axis=TRUE)

# from_ <- min(box2$axis$from, box2$axis$to)
# to_ <- max(box2$axis$from, box2$axis$to)

# x_lower <- runif(3, from_, to_)
# x_upper <- runif(3, x_lower, to_)

# # test the function
# plot_horizontal_seg(box2, c(x_lower[1], x_upper[1]), 1, col='purple')
# plot_horizontal_seg(box2, c(x_lower[2], x_upper[2]), 2, col='purple')
# plot_horizontal_seg(box2, c(x_lower[3], x_upper[3]), 3, col='purple')

# # add dot
# plot_dot(box2, runif(1, x_lower[1], x_upper[1]), 1, col='purple')
# plot_dot(box2, runif(1, x_lower[2], x_upper[2]), 2, col='purple')
# plot_dot(box2, runif(1, x_lower[3], x_upper[3]), 3, col='purple')

