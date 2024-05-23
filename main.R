grid.newpage()

# create header
breaks_widths <- c(0.2, 0.1, 0.1)
my_header <- create_header(breaks_widths, c('Endpoint', 'Treatment', 'Placebo'))
#add_benefit_arrows(my_header, 0.7, 'up')

current_y <- 1 - (HEADER_HEIGHT + PAGE_TOP_MARGIN)

global_box_x <- rev(my_header$breaks_positions)[2]
global_box_width <- rev(my_header$breaks_positions)[1] - global_box_x

# create a box
box1 <- plot_box(global_box_x, current_y, global_box_width, 3, 0.05, 3, 5, 1, -2, expression(x[1]^2), FALSE)

# add benefit arrows
add_benefit_arrows(my_header, 3/5)

from_ <- min(box1$axis$from, box1$axis$to)
to_ <- max(box1$axis$from, box1$axis$to)

x_lower <- runif(3, from_, to_)
x_upper <- runif(3, x_lower, to_)

# test the function
plot_horizontal_seg(box1, c(x_lower[1], x_upper[1]), 1, col='purple')
plot_horizontal_seg(box1, c(x_lower[2], x_upper[2]), 2, col='purple')
plot_horizontal_seg(box1, c(x_lower[3], x_upper[3]), 3, col='purple')

# add dot
plot_dot(box1, runif(1, x_lower[1], x_upper[1]), 1, col='purple')
plot_dot(box1, runif(1, x_lower[2], x_upper[2]), 2, col='purple')
plot_dot(box1, runif(1, x_lower[3], x_upper[3]), 3, col='purple')