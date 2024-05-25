

#' Plot a horizontal segment on a box plot
#'
#' This function plots a horizontal segment on a box plot.
#'
#' @param box The box plot object.
#' @param x The x-coordinate of the segment.
#' @param n1 The y-coordinate of the segment start.
#' @param n2 The y-coordinate of the segment end (default is 1).
#' @param N2 The total number of segments (default is 1).
#' @param lty The line type (default is 1).
#' @param lwd The line width (default is 3).
#' @param col The line color (default is 'black').
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' # Plot a horizontal segment on the box plot
#' plot_horizontal_seg(box, x = 1, n1 = 20, n2 = 30, N2 = 2, lty = 2, lwd = 2, col = 'red')
#' }
#'
plot_horizontal_seg <- function(box, x, n1, n2=1, N2=1, lty=1, lwd=3, col='black') {

  xvec <- sapply(x, function(x) box$axis$axis_function(x))
  yvec <- box$y_pos[n1] + (box$y_pos[n1+1] - box$y_pos[n1])*n2/(N2+1)

  grid.lines(x = xvec, y = rep(yvec, 2), 
             gp = gpar(lty = lty, lwd = lwd, col = col))
    
}

#' Plot a long rectangle on a given plot
#'
#' This function plots a long rectangle on a given plot using the specified parameters.
#'
#' @param box The plot box object containing axis information
#' @param x The x-coordinates of the rectangle
#' @param n1 The index of the y-axis position to use for the rectangle
#' @param n2 The position of the rectangle along the y-axis (default is 1)
#' @param N2 The total number of positions along the y-axis (default is 1)
#' @param height The height of the rectangle (default is 0.015)
#' @param lty The line type of the rectangle (default is 1)
#' @param lwd The line width of the rectangle (default is 3)
#' @param col The color of the rectangle (default is 'black')
#'
#' @return None
#'
#' @examples
#' # Create a plot box object
#' \dontrun{
#' # Plot a long rectangle
#' plot_long_rectangle(box, x = c(0.2, 0.8), n1 = 1)
#' }
plot_long_rectangle <- function(box, x, n1, n2=1, N2=1, height=0.015, lty=1, lwd=3, col='black') {

  xvec <- sapply(x, function(x) box$axis$axis_function(x))
  yvec <- box$y_pos[n1] + (box$y_pos[n1+1] - box$y_pos[n1])*n2/(N2+1)

  height <- ifelse(is.null(height), (box$y_pos[n1+1] - box$y_pos[n1])/5, height)

  grid.rect(x = unit(xvec[1], 'npc'), y = unit(yvec, 'npc'), 
            width = unit(diff(xvec), 'npc'), height = unit(height, 'npc'),
            just=c('left', 'center'),
            gp = gpar(lty = lty, lwd = lwd, col = col))
    
}

#' Plot dots
#'
#' This function plots dots on a box.
#'
#' @param box The box to plot the dots on.
#' @param x The x-coordinate of the dots.
#' @param n1 The number of dots to plot.
#' @param n2 The number of dots to plot (default is 1).
#' @param N2 The total number of dots (default is 1).
#' @param pch The symbol to use for the dots (default is 19).
#' @param size The size of the dots (default is unit(1, 'char')).
#' @param col The color of the dots (default is 'black').
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' plot_dot(box = my_box, x = 0.5, n1 = 10, n2 = 2, N2 = 20, pch = 19, size = unit(1, 'char'), 
#'          col = 'black')
#' }
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
#' Plot Forest Tree
#'
#' This function plots a forest tree based on the given parameters.
#'
#' @param box The box parameter.
#' @param x_lower The lower x-coordinate.
#' @param x_upper The upper x-coordinate.
#' @param x_dot The x-dot parameter.
#' @param n1 The n1 parameter.
#' @param n2 The n2 parameter (default is 1).
#' @param N2 The N2 parameter (default is 1).
#' @param col The color of the forest tree (default is '#663399').
#' @param br_palette The color palette to use (default is page_options$get_palette()).
#' @param userect The userect parameter (default is FALSE).
#' @param height The height of the rectangle, used when userect is TRUE. When NULL, 
#'        the height is set to 1/5 of the box height.
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' # Create a box
#' box2 <- plot_box(0.5, 0.5, 1, 3, 0.02, 3, 5, 1, -2, 'value', FALSE, show_axis=TRUE)
#'
#' # Generate random x coordinates
#' from_ <- min(box2$axis$from, box2$axis$to)
#' to_ <- max(box2$axis$from, box2$axis$to)
#' x_lower <- runif(3, from_, to_)
#' x_upper <- runif(3, x_lower, to_)
#'
#' # Plot horizontal segments and dots
#' plot_horizontal_seg(box2, c(x_lower[1], x_upper[1]), 1, col='purple')
#' plot_horizontal_seg(box2, c(x_lower[2], x_upper[2]), 2, col='purple')
#' plot_horizontal_seg(box2, c(x_lower[3], x_upper[3]), 3, col='purple')
#' plot_dot(box2, runif(1, x_lower[1], x_upper[1]), 1, col='purple')
#' plot_dot(box2, runif(1, x_lower[2], x_upper[2]), 2, col='purple')
#' plot_dot(box2, runif(1, x_lower[3], x_upper[3]), 3, col='purple')
#' }
#'
plot_forest_tree <- function(box, x_lower, x_upper, x_dot, n1, n2=1, N2=1, 
                             col='#663399', userect=FALSE, height=NULL,
                             br_palette=page_options$new()$get_palette()) {
  
  if (is.null(col) & N2>1) {
    col <- br_palette[n2]
  } else if(!is.null(col) & N2>1 & length(col) == N2) {
    col <- col[n2]
  } else if(is.null(col) & N2==1) {
    col <- br_palette[1]
  }

  if(!userect) {
    plot_horizontal_seg(box, c(x_lower, x_upper), n1, n2, N2, col=col)
  } else {
    plot_long_rectangle(box, c(x_lower, x_upper), n1, n2, N2, col=col, height=height)
  }

  plot_dot(box, x_dot, n1, n2, N2, col=col)
  
}
