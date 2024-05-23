# function to plot a box with xaxis created by axis.R
# the box will present categorical data. It will consist
# of a box with a horizontal axis. The box will be divided
# into n parts, where n is the number of categories. The
# height of the box will be determined by single category height 
# argument and number of categories. There will be a dashed vertical
# line starting at y position on the xaxis neutral position
# and will ho up to the top of the box.

plot_box <- function(xpos, ypos, xlength, n_categories, single_category_height, 
                     neutral_pos, n_ticks, from, to, label=NULL, logscale=FALSE, b=10, 
                     show_axis=TRUE) {

    # plot the xaxis
    axis <- plot_axis(xlength, xpos, ypos, from, to, n_ticks, neutral_pos, label, logscale, b, show_axis)
    
    # plot the box
    grid.rect(x = xpos, y = ypos, width = axis$length, height = n_categories * single_category_height, 
              just = c('left', 'bottom'), gp = gpar(fill = "#ded8db43", lty = 1, lwd = 1))

    # separate each category with a faint vertical line across the whole
    # width of the box
    for (i in 1:(n_categories-1)) {
        grid.lines(x = c(xpos, xpos + axis$length), 
                   y = c(ypos + i * single_category_height, ypos + i * single_category_height), 
                   gp = gpar(lty = 3, lwd = 1))
    }
    
    # plot the vertical line
    axis_transform_fun <- axis$axis_function

    grid.lines(x = rep(axis_transform_fun(ifelse(logscale, 1, 0)), 2),
               y = c(ypos, ypos + n_categories * single_category_height), 
               gp = gpar(lty = 2, lwd = 1))

    # return axis and the y positions of the categories
    return(list(axis = axis, 
                y_pos = seq(ypos, ypos + n_categories * single_category_height, single_category_height)))
}

grid.newpage()

# test the function
box1 <- plot_box(0.5, 0.5, 1, 3, 0.05, 3, 5, 0, 1, expression(x[1]^2), FALSE)

# test it again, with overlapping box
box2 <- plot_box(0.4, 0.4, 1, 3, 0.05, 3, 5, 0, 1, expression(x[1]^2), FALSE)

box3 <- plot_box(0.6, 0.6, 1, 3, 0.01, 3, 5, 0, 1, NULL, FALSE)

# test it again, but hide axis
box4 <- plot_box(0.5, 0.7, 1, 3, 0.05, 3, 5, 0, 1, expression(x[1]^2), FALSE, show_axis=FALSE)