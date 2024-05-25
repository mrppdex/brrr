

#' Plot Box
#'
#' This function plots a box at the specified position with the specified dimensions.
#'
#' @param xpos The x-coordinate of the box's position.
#' @param ypos The y-coordinate of the box's position.
#' @param xlength The length of the box along the x-axis.
#' @param n_categories The number of categories in the box.
#' @param single_category_height The height of each category within the box.
#' @param neutral_pos The position of the neutral line within the box.
#' @param n_ticks The number of ticks on the axis.
#' @param from The starting value of the axis range.
#' @param to The ending value of the axis range.
#' @param label The label for the axis.
#' @param logscale Logical value indicating whether to use logarithmic scale.
#' @param b The base for logarithmic scale.
#' @param show_axis Logical value indicating whether to show the axis.
#'
#' @return A list containing the following elements:
#'  - axis: The axis of the box.
#'  - name: The name of the box.
#'  - height: The height of the box.
#'  - y_pos: The y-positions of the categories in the box.
#' 
#'
#' @examples
#' \dontrun{
#' grid.newpage()
#' box1 <- plot_box(0.5, 0.5, 1, 3, 0.05, 3, 5, 0, 1, expression(x[1]^2), FALSE)
#' box2 <- plot_box(0.4, 0.4, 1, 3, 0.05, 3, 5, 0, 1, expression(x[1]^2), FALSE)
#' box3 <- plot_box(0.6, 0.6, 1, 3, 0.01, 3, 5, 0, 1, NULL, FALSE)
#' box4 <- plot_box(0.5, 0.7, 1, 3, 0.05, 3, 5, 0, 1, expression(x[1]^2), FALSE, show_axis=FALSE)
#' }
plot_box <- function(xpos, ypos, xlength, n_categories, single_category_height, 
                     neutral_pos, n_ticks, from, to, label=NULL, logscale=FALSE, b=10, 
                     show_axis=TRUE) {

    box_height <- n_categories * single_category_height

    # plot the xaxis
    axis <- plot_axis(xlength, xpos, ypos - box_height, from, to, n_ticks, neutral_pos, label, logscale, b, show_axis)

    # plot the box
    grid.rect(x = xpos, y = ypos, width = axis$length, height = box_height, 
              just = c('left', 'top'), gp = gpar(fill = "#ded8db43", lty = 1, lwd = 1))

    # separate each category with a faint vertical line across the whole
    # width of the box
    for (i in 1:(n_categories-1)) {
        grid.lines(x = c(xpos, xpos + axis$length), 
                   y = c(ypos - i * single_category_height, ypos - i * single_category_height), 
                   gp = gpar(lty = 3, lwd = 1))
    }
    
    # plot the vertical line
    axis_transform_fun <- axis$axis_function

    grid.lines(x = rep(axis_transform_fun(ifelse(logscale, 1, 0)), 2),
               y = c(ypos, ypos - n_categories * single_category_height), 
               gp = gpar(lty = 2, lwd = 1))

    # return axis and the y positions of the categories
    return(list(axis = axis,
                name = 'box', 
                height = box_height,
                y_pos = seq(ypos, ypos - n_categories * single_category_height, -single_category_height)))
}


#' add_box function
#'
#' This function adds a box to a plot. The box can be either a header or a regular box.
#'
#' @param obj The object to which the box will be added.
#' @param spacing The spacing between the box and the object above it.
#' @param n_categories The number of categories in the box.
#' @param single_category_height The height of each category in the box.
#' @param neutral_pos The position of the neutral point in the box.
#' @param n_ticks The number of ticks in the box.
#' @param from The starting point of the box.
#' @param to The ending point of the box.
#' @param label The label for the box (optional).
#' @param logscale Whether to use a logarithmic scale for the box (default is FALSE).
#' @param b The base for the logarithmic scale (default is 10).
#' @param show_axis Whether to show the axis for the box (default is TRUE).
#' @param direction The direction of the benefit arrows (default is 'up').
#' @param userect Whether to use a rectangle for the box (default is FALSE).
#'
#' @return A list containing the label function, the box object, the options, the name, the header, and the y position.
#'
#' @examples
#' \dontrun{
#' grid.newpage()
#' # create header
#' breaks_widths <- c(0.2, -0.1, 0.1, 0.2)
#' h_labels <- c('Benefits', 'Treatment\n(N=100)', 'Placebo\n(N=100)',
#'               'Comparison\nHR or Odds Ratio\n(95% CI)')
#' 
#' stopifnot(length(breaks_widths)==length(h_labels))
#' 
#' my_header <- create_header(breaks_widths, h_labels)
#' 
#' box1_data <- add_box(my_header, 0, 3, 0.05, 3, 5, 0, 1, expression(x[1]^2), 
#'                      FALSE, direction='down')
#' 
#' for (i in 1:3) {
#'     box1_data$label_fun(sprintf('Efficacy endpoint %g', i), 1, i)
#'     box1_data$label_fun('LY 10mg', 2, i, 1, 3)
#'     box1_data$label_fun('LY 20mg', 2, i, 2, 3)
#'     box1_data$label_fun('LY 40mg', 2, i, 3, 3)
#'     box1_data$label_fun('Placebo', 3, i)
#' }
#' 
#' box2_data <- add_box(box1_data, 0.07, 3, 0.02, 3, 5, 0, 1, expression(x[1]^2), 
#'                      FALSE, direction='down', show_axis=FALSE)
#' 
#' # add labels to box2
#' for (i in 1:3) {
#'     box2_data$label_fun(sprintf('Efficacy endpoint 1%s', LETTERS[i]), 1, i)
#'     box2_data$label_fun('LY 100mg', 2, i)
#'     box2_data$label_fun('Placebo', 3, i)
#' }
#' 
#' box3_data <- add_box(box2_data, 0, 3, 0.04, 3, 5, 0, 1, "Efficacy endpoint", 
#'                      FALSE, direction='down')
#' 
#' # add labels to box3
#' for (i in 1:3) {
#'     box3_data$label_fun(sprintf('Efficacy endpoint 2%s', LETTERS[i]), 1, i)
#'     box3_data$label_fun('LY 100mg', 2, i, 1, 2)
#'     box3_data$label_fun('LY 200mg', 2, i, 2, 2)
#'     box3_data$label_fun('Placebo', 3, i)
#' }
#' 
#' # function to simulate and plot forest tree
#' 
#' simulate_forest_plot <- function(box, N1=1, col='purple') {
#' 
#'     from_ <- min(box$axis$from, box$axis$to)
#'     to_ <- max(box$axis$from, box$axis$to)
#' 
#'     for (i in 1:(length(box$y_pos-1))) {
#'         for (j in 1:N1) {
#'             x_lower <- runif(1, from_, to_)
#'             x_upper <- runif(1, x_lower, to_)
#' 
#'             plot_forest_tree(box, x_lower, x_upper, runif(1, x_lower, x_upper), i, j, N1, col=col)
#'         }
#'     }
#' }
#' 
#' simulate_forest_plot(box1_data$box, 3, col='purple')
#' simulate_forest_plot(box2_data$box, 1, col='purple')
#' simulate_forest_plot(box3_data$box, 2, col='forestgreen')
#' }
#' @export
add_box <- function(obj, spacing, n_categories, single_category_height, 
                    neutral_pos, n_ticks, from, to, label=NULL, logscale=FALSE, b=10, 
                    show_axis=TRUE, direction='up', userect=FALSE) {
    
    current_y <- NULL

    options <- NULL
    if (obj$name=='header') {
        options <- obj$options
    } else if (obj$name=='box') {
        options <- obj$header$options
    } else {
        stop('The object is not unknown')
    }

    HEADER_HEIGHT <- options$get_page_parameter('HEADER_HEIGHT')
    PAGE_TOP_MARGIN <- options$get_page_parameter('PAGE_TOP_MARGIN')

    if(!is.null(obj$name) & obj$name == 'header') {
        header <- obj
        current_y <- 1 - (HEADER_HEIGHT + PAGE_TOP_MARGIN) - spacing
        
        # add benefit arrows
        add_benefit_arrows(header, neutral_pos/n_ticks, direction=direction)

    } else if (!is.null(obj$name) & obj$name == 'box') {
        header <- obj$header
        current_y <- obj$y_pos - spacing
    } else {
        stop('The object is not a header')
    } 

    global_box_x <- rev(header$breaks_positions)[2]
    global_box_width <- rev(header$breaks_positions)[1] - global_box_x

    # create a box
    box1 <- plot_box(global_box_x, current_y, global_box_width, n_categories, single_category_height, 
                     neutral_pos, n_ticks, from, to, label, logscale, b, show_axis)


    # read header$breaks_position discard two last elements
    # and calculate the mean of each pair of elements

    breaks_positions <- header$breaks_positions
    breaks_positions <- breaks_positions[1:(length(breaks_positions)-1)]
    breaks_positions <- breaks_positions[-length(breaks_positions)] + diff(breaks_positions)/2

    # all labels
    add_label <- function(label, collevel, rowlevel, n=1, N=1, 
                          fontsize=10, fontface='bold', col='black', isglobal=FALSE) 
    {

        y_pos <- box1$y_pos

        xvec <- breaks_positions[collevel]
        
        yvec <- y_pos[rowlevel] + (y_pos[rowlevel+1] - y_pos[rowlevel])*n/(N+1)
        yvec <- ifelse(isglobal, (y_pos[1] + y_pos[length(y_pos)])/2, yvec)

        grid.text(label = label, 
                  x = unit(xvec, 'npc'), y = unit(yvec, 'npc'), just = c('center', 'center'),
                  gp = gpar(fontsize = fontsize, fontface = fontface, col=col))
    }

    return(list(label_fun=add_label, 
                box = box1, name='box', userect=userect,
                header=header, y_pos = min(box1$y_pos)))
}

