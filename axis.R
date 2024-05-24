
# use grid to plot horizontal axis. It can be either linear or logaritmic
# with base b. The axis has ticks determined by outputs of function 'pretty'
# however the argument 'n' is used to determine the number of ticks. 
# The axis is plotted in the rectangle determined by the parameters. 
# The range of the axis is specified in arguments 'from' and 'to'.
# The axis is labeled with the string 'label'. The argument 'side' is used

# set option pp_tick_len

plot_axis <- function(xlength, xpos, ypos, from, to, n_ticks, neutral_pos, 
                      label=NULL, logscale=FALSE, b = 10, show_axis=TRUE) {
    
    # when logscale is TRUE stop if from or to is <= 0
    if (logscale && (from <= 0 || to <= 0)) {
        stop("from and to must be > 0 when logscale is TRUE")
    }

    if (logscale) {
        from <- log(from, base = b)
        to <- log(to, base = b)
    }

    neutral_pos <- ifelse(from > to, n_ticks - neutral_pos, neutral_pos)

    ratio_rhs <- abs(ifelse(to>from, to/(n_ticks-neutral_pos), from/(n_ticks-neutral_pos)))
    ratio_lhs <- abs(ifelse(to>from, from/neutral_pos, to/neutral_pos))

    
    ratio <- max(ratio_rhs, ratio_lhs)
    ratio <- ifelse(logscale, ceiling(abs(ratio))*sign(ratio), ratio)

    from <- ifelse(to>from, -ratio*neutral_pos, ratio*(n_ticks-neutral_pos))
    to   <- ifelse(to>from, ratio*(n_ticks-neutral_pos), -ratio*neutral_pos)

    # debug print from and to
    cat("from = ", from, " to = ", to, "\n")

    axis_range <- round(seq(from, to, length = n_ticks + 1), 2)

    # plot the line of length xlength at x = xpos, y = ypos
    # when length + xpos > 1, the line is cut at PAGE_RIGHT_MARGIN
    if (xpos + xlength > 1) {
        xlength <- 1 - xpos - PAGE_RIGHT_MARGIN
    }

    # set the length of the ticks
    tick_len <- 0.02 * xlength


    if (show_axis) {
        grid.lines(x = c(xpos, xpos + xlength), y = c(ypos, ypos), gp = gpar(lwd = 1))

        # plot the ticks
        for (i in 1:length(axis_range)) {
            tick_pos <- xpos + xlength * (axis_range[i] - from) / (to - from)
            grid.lines(x = c(tick_pos, tick_pos), y = c(ypos - tick_len, ypos), gp = gpar(lwd = 1))
        }

        format_log_ticks <- function(x, b) {
            if(b^abs(x)>=1e3) {
                eval_txt <- sprintf('expression(%g^%g)', b, x)
                return(eval(parse(text = eval_txt)))
            } else {
                if(x<0) {
                    return(sprintf('1/%g', b^(-x)))
                } else {
                    return(sprintf('%g', b^x))
                }
            }
        }

        # add the labels
        for (i in 1:length(axis_range)) {
            tick_pos <- xpos + xlength * (axis_range[i] - from) / (to - from)
            grid.text(label = ifelse(logscale, 
                                     format_log_ticks(axis_range[i], b), 
                                     as.character(axis_range[i])),
                      x = tick_pos, y = ypos - 2 * tick_len, just = "top", 
                      gp = gpar(fontsize = 12))
        }

        # add the label
        if (!is.null(label)) {
            grid.text(label = label, x = xpos + xlength / 2, y = ypos - 4 * tick_len, just = "top", 
                    gp = gpar(fontsize = 12, fontface = "bold"))
        }
    }

    # return a function that scales the value x to the position on the axis.
    # when logscale is TRUE, the function does the log transformation. 
    # 

    return(list(
      axis_function = function(x) {
        if (logscale) {
            return(x = xpos + xlength * (log(x, base = b) - from) / (to - from))
        } else {
            return(x = xpos + xlength * (x - from) / (to - from))
        }
      },
      from = from,
      to = to,
      length = xlength
    ))
}

grid.newpage()
# pp_lin_axis <- plot_axis(xlength = 0.8, xpos = 0.5, ypos = 0.5, 
#           from = 0, to = 10, b = 10, n_ticks = 6, neutral_pos = 1)

# # reversed axis
# pp_rev_axis <- plot_axis(xlength = 0.8, xpos = 0.5, ypos = 0.4, 
#           from = 10, to = 0, b = 10, n_ticks = 6, neutral_pos = 1)

# # logaritmic axis
# pp_log_axis <- plot_axis(xlength = 0.8, xpos = 0.5, ypos = 0.3, 
#           from = 0.1, to = 1000, b = 10, n_ticks = 6, neutral_pos = 1, 
#           logscale = TRUE, label = "from=0.1 to 1000")

# logarithmic reversed axis with label that has math expression
pp_log_rev_axis <- plot_axis(xlength = 0.8, xpos = 0.5, ypos = 0.2, 
          from = 1000, to = 0.1, b = 10, n_ticks = 6, neutral_pos = 1, 
          logscale = TRUE, label = 'from=1000 to 0.1', show_axis=TRUE)

# # another logaritmic axis
# pp_log_axis2 <- plot_axis(xlength = 0.8, xpos = 0.5, ypos = 0.1, 
#           from = 1.12, to = 99, b = 10, n_ticks = 6, neutral_pos = 1, 
#           logscale = TRUE, label = "from=1.12 to 99")
