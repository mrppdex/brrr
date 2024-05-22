
# use grid to plot horizontal axis. It can be either linear or logaritmic
# with base b. The axis has ticks determined by outputs of function 'pretty'
# however the argument 'n' is used to determine the number of ticks. 
# The axis is plotted in the rectangle determined by the parameters. 
# The range of the axis is specified in arguments 'from' and 'to'.
# The axis is labeled with the string 'label'. The argument 'side' is used

# set option pp_tick_len

plot_axis <- function(xlength, xpos, ypos, from, to, b, n_ticks, neutral_pos) {

    ratio_rhs <- abs(ifelse(to>from, to/(n_ticks-neutral_pos), from/(n_ticks-neutral_pos)))
    ratio_lhs <- abs(ifelse(to>from, from/neutral_pos, to/neutral_pos))

    ratio <- max(ratio_rhs, ratio_lhs)

    from <- ifelse(to>from, -ratio*neutral_pos, ratio*(n_ticks-neutral_pos))
    to   <- ifelse(to>from, ratio*(n_ticks-neutral_pos), -ratio*neutral_pos)

    # debug print from and to
    cat("from = ", from, " to = ", to, "\n")

    axis_range <- round(seq(from, to, length = n_ticks), 2)

    # plot the line of length xlength at x = xpos, y = ypos
    # when length + xpos > 1, the line is cut at PAGE_RIGHT_MARGIN
    if (xpos + xlength > 1) {
        xlength <- 1 - xpos - PAGE_RIGHT_MARGIN
    }

    # set the length of the ticks
    tick_len <- 0.02 * xlength

    grid.lines(x = c(xpos, xpos + xlength), y = c(ypos, ypos), gp = gpar(lwd = 1))

    # plot the ticks
    for (i in 1:length(axis_range)) {
        tick_pos <- xpos + xlength * (axis_range[i] - from) / (to - from)
        grid.lines(x = c(tick_pos, tick_pos), y = c(ypos - tick_len, ypos), gp = gpar(lwd = 1))
    }

    # add the labels
    for (i in 1:length(axis_range)) {
        tick_pos <- xpos + xlength * (axis_range[i] - from) / (to - from)
        grid.text(label = axis_range[i], 
                  x = tick_pos, y = ypos - 2 * tick_len, just = "top", 
                  gp = gpar(fontsize = 12))
    }
}

grid.newpage()
plot_axis(xlength = 0.8, xpos = 0.5, ypos = 0.5, 
          from = 0, to = 10, b = 10, n_ticks = 6, neutral_pos = 2)