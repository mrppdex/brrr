#' Plot Axis
#'
#' This function plots an axis with ticks and labels on a given plot.
#'
#' @param xlength The length of the axis line.
#' @param xpos The x-coordinate of the starting position of the axis line (0 to 1).
#' @param ypos The y-coordinate of the starting position of the axis line (0 to 1).
#' @param from The starting value of the axis range.
#' @param to The ending value of the axis range.
#' @param n_ticks The number of ticks on the axis.
#' @param neutral_pos The position of the neutral tick (0 on linear scale, 1 on logarithmic scale).
#' @param label The label for the axis.
#' @param logscale Logical value indicating whether to use logarithmic scale.
#' @param b The base for logarithmic scale.
#' @param show_axis Logical value indicating whether to show the axis.
#' @param options The page options object.
#'
#' @return A list containing the following elements:
#'   - axis_function: A function that scales a value to the position on the axis.
#'   - from: The transformed starting value of the axis range.
#'   - to: The transformed ending value of the axis range.
#'   - length: The length of the axis line.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' grid.newpage()
#' pp_lin_axis <- plot_axis(xlength = 0.8, xpos = 0.5, ypos = 0.5, 
#'          from = 0, to = 10, b = 10, n_ticks = 6, neutral_pos = 1)
#'
#' # reversed axis
#' pp_rev_axis <- plot_axis(xlength = 0.8, xpos = 0.5, ypos = 0.4, 
#'          from = 10, to = 0, b = 10, n_ticks = 6, neutral_pos = 1)
#' 
#' # logaritmic axis
#' pp_log_axis <- plot_axis(xlength = 0.8, xpos = 0.5, ypos = 0.3, 
#'           from = 0.1, to = 1000, b = 10, n_ticks = 6, neutral_pos = 1, 
#'           logscale = TRUE, label = "from=0.1 to 1000")
#' 
#' # logarithmic reversed axis with label that has math expression
#' pp_log_rev_axis <- plot_axis(xlength = 0.8, xpos = 0.5, ypos = 0.2, 
#'           from = 1000, to = 0.1, b = 10, n_ticks = 6, neutral_pos = 1, 
#'           logscale = TRUE, label = 'from=1000 to 0.1', show_axis=TRUE)
#' 
#' # another logaritmic axis
#' pp_log_axis2 <- plot_axis(xlength = 0.8, xpos = 0.5, ypos = 0.1, 
#'           from = 1.12, to = 99, b = 10, n_ticks = 6, neutral_pos = 1, 
#'           logscale = TRUE, label = "from=1.12 to 99")
#' }
#'
#' @export
plot_axis <- function(xlength, xpos, ypos, from, to, n_ticks, neutral_pos, 
                      label=NULL, logscale=FALSE, b = 10, show_axis=TRUE, 
                      options=page_options$new()) {
    
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

    axis_range <- round(seq(from, to, length = n_ticks + 1), 2)

    scale_function <- function(x) {
        if (logscale) {
            return(x = xpos + xlength * (log(x, base = b) - from) / (to - from))
        } else {
            return(x = xpos + xlength * (x - from) / (to - from))
        }
    }
    
    # set the length of the ticks
    tick_len <- 0.02 * xlength


    if (show_axis) {
        # plot the axis
        grid.lines(x = c(xpos, xpos + xlength), y = c(ypos, ypos), gp = gpar(lwd = 1))

        # plot the ticks
        for (i in 1:length(axis_range)) {
            tick_pos <- xpos + xlength * (axis_range[i] - from) / (to - from)
            grid.lines(x = c(tick_pos, tick_pos), y = c(ypos - tick_len, ypos), gp = gpar(lwd = 1))
        }

        # plot small ticks if logscale
        if(logscale & b==10) {
            seq_direction <- ifelse(from < to, 1, -1)

            ticks_small <- NULL
            if (seq_direction==1) {
                ticks_small <- b^from + cumsum(rep(diff(b^axis_range)/10, each=10))
            } else {
                ticks_small <- rev(b^from + cumsum(rep(diff(b^axis_range)/10, each=10)))
            }

            for (i in 1:length(ticks_small)) {
                tick_pos <- scale_function(ticks_small[i])
                grid.lines(x = c(tick_pos, tick_pos), y = c(ypos - tick_len/2, ypos), gp = gpar(lwd = 1))
            }
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

        axis_label_font_size <- options$get_axis_label_font_size()
        axis_label_height <- convertHeight(unit(axis_label_font_size, 'points'),
                                           unitTo='npc', valueOnly = TRUE)

        axis_ticks_font_size <- options$get_axis_ticks_font_size()
        axis_ticks_label_height <- convertHeight(unit(axis_ticks_font_size, 'points'),
                                          unitTo='npc', valueOnly = TRUE)

        # add the ticks labels
        for (i in 1:length(axis_range)) {
            tick_pos <- xpos + xlength * (axis_range[i] - from) / (to - from)
            grid.text(label = ifelse(logscale, 
                                     format_log_ticks(axis_range[i], b), 
                                     as.character(axis_range[i])),
                      x = tick_pos, 
                      y = unit(ypos - 1.5*tick_len - axis_ticks_label_height, 'npc'),
                      just = "bottom", 
                      gp = gpar(fontsize = axis_label_font_size))
        }

        # add the axis label
        if (!is.null(label)) {
            grid.text(label = label, x = xpos + xlength / 2, 
                    y =  unit(ypos - 2*tick_len - axis_ticks_label_height - axis_label_height, 'npc'), 
                    just = "bottom", gp = gpar(fontsize = axis_label_font_size, fontface = "bold"))
        }
    }

    # return a function that scales the value x to the position on the axis.
    # when logscale is TRUE, the function does the log transformation. 
    # 

    return(list(
      axis_function = scale_function,
      from = from,
      to = to,
      length = xlength
    ))
}

# grid.newpage()
# pp_lin_axis <- plot_axis(xlength = 0.8, xpos = 0.1, ypos = 0.5, 
#          from = 10^-5, to = 10, b = 10, n_ticks = 6, neutral_pos = 2, logscale = TRUE)
