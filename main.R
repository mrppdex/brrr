grid.newpage()

# create header
breaks_widths <- c(0.2, 0.1, 0.1)
my_header <- create_header(breaks_widths, c('Endpoint', 'Treatment', 'Placebo'))

# function to generate a header, add benefit arrows and plot and boxex.
# The function calculates the bottom edge of all plotted elements and
# returns it as a y coordinate. This way, the next element can be plotted
# below the previous one. Also, one of the arguments is the spacing
# between the elements. This way, the function can be used to plot.

add_benefits_box <- function(header, spacing, n_categories, single_category_height, 
                             neutral_pos, n_ticks, from, to, label=NULL, logscale=FALSE, b=10, 
                             show_axis=TRUE, direction='up') {
    current_y <- 1 - (header$HEADER_HEIGHT + header$PAGE_TOP_MARGIN) - spacing

    global_box_x <- rev(header$breaks_positions)[2]
    global_box_width <- rev(header$breaks_positions)[1] - global_box_x

    # create a box
    box1 <- plot_box(global_box_x, current_y, global_box_width, n_categories, single_category_height, 
                     neutral_pos, n_ticks, from, to, label, logscale, b, show_axis)

    # add benefit arrows
    add_benefit_arrows(header, neutral_pos/n_ticks, direction=direction)

    return(list(box = box1, y_pos = min(box1$y_pos)))
}


# test the function
box1_data <- add_benefits_box(my_header, 0, 3, 0.05, 3, 5, 0, 1, expression(x[1]^2), 
                              FALSE, direction='down')
box1 <- box1_data$box

# function to simulate and plot forest tree

simulate_forest_plot <- function(box, N1=1, col='purple') {

    from_ <- min(box$axis$from, box$axis$to)
    to_ <- max(box$axis$from, box$axis$to)

    for (i in 1:(length(box$y_pos-1))) {
        for (j in 1:N1) {
            x_lower <- runif(1, from_, to_)
            x_upper <- runif(1, x_lower, to_)

            plot_forest_tree(box, x_lower, x_upper, runif(1, x_lower, x_upper), i, j, N1, col=col)
        }
    }
}

simulate_forest_plot(box1, 3, col='purple')
