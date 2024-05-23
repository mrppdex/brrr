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
  
  grid.points(x = unit(xvec, 'npc'), y = unit(yvec, 'npc'),
        pch=pch, size=size, gp = gpar(col = col))
  
}


# grid.newpage()

# # create a box
# box2 <- plot_box(0.5, 0.5, 1, 3, 0.05, 3, 5, 0, 1, expression(x[1]^2), FALSE)

# # test the function
# plot_horizontal_seg(box2, c(0.1, 0.9), 1, 1, 3, col='red')
# plot_horizontal_seg(box2, c(0.1, 0.9), 1, 2, 3, col='red')
# plot_horizontal_seg(box2, c(0.1, 0.9), 1, 3, 3, col='red')

# # add dot
# plot_dot(box2, 0.5, 1, pch=17, col='red')