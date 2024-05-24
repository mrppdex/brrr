grid.newpage()

# create header
breaks_widths <- c(0.2, -0.1, 0.1, 0.2)
h_labels <- c('Benefits', 'Treatment\n(N=100)', 'Placebo\n(N=100)',
              'Comparison\nHR or Odds Ratio\n(95% CI)')

stopifnot(length(breaks_widths)==length(h_labels))

my_header <- create_header(breaks_widths, h_labels)

# function to generate a header, add benefit arrows and plot and boxex.
# The function calculates the bottom edge of all plotted elements and
# returns it as a y coordinate. This way, the next element can be plotted
# below the previous one. Also, one of the arguments is the spacing
# between the elements. This way, the function can be used to plot.

add_benefits_box <- function(obj, spacing, n_categories, single_category_height, 
                             neutral_pos, n_ticks, from, to, label=NULL, logscale=FALSE, b=10, 
                             show_axis=TRUE, direction='up') {
    current_y <- NULL
    if(!is.null(obj$name) & obj$name == 'header') {
        header <- obj
        current_y <- 1 - (header$HEADER_HEIGHT + header$PAGE_TOP_MARGIN) - spacing
        
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

    # create a function with arguments level=1 to the length of breaks_positions
    # that gets the x position of the breaks_positions, and y in similar manner to
    # yvec <- box$y_pos[n1] + (box$y_pos[n1+1] - box$y_pos[n1])*n2/(N2+1)
    # and plots a text at that position

    add_label <- function(label, leveln, subleveln, n=1, N=1, 
                          fontsize=10, fontface='bold', col='black', isglobal=FALSE) 
    {

        y_pos <- box1$y_pos

        xvec <- breaks_positions[leveln]
        
        yvec <- y_pos[subleveln] + (y_pos[subleveln+1] - y_pos[subleveln])*n/(N+1)
        yvec <- ifelse(isglobal, (y_pos[1] + y_pos[length(y_pos)])/2, yvec)

        grid.text(label = label, 
                  x = unit(xvec, 'npc'), y = unit(yvec, 'npc'), just = c('center', 'center'),
                  gp = gpar(fontsize = fontsize, fontface = fontface, col=col))
    }

    return(list(label_fun=add_label, box = box1, name='box', header=header, y_pos = min(box1$y_pos)))
}


# test the function
box1_data <- add_benefits_box(my_header, 0, 3, 0.05, 3, 5, 0, 1, expression(x[1]^2), 
                              FALSE, direction='down')

for (i in 1:3) {
    box1_data$label_fun(sprintf('Efficacy endpoint %g', i), 1, i)
    box1_data$label_fun('LY 10mg', 2, i, 1, 3)
    box1_data$label_fun('LY 20mg', 2, i, 2, 3)
    box1_data$label_fun('LY 40mg', 2, i, 3, 3)
    box1_data$label_fun('Placebo', 3, i)
}

box2_data <- add_benefits_box(box1_data, 0.07, 3, 0.02, 3, 5, 0, 1, expression(x[1]^2), 
                              FALSE, direction='down', show_axis=FALSE)

# add labels to box2
for (i in 1:3) {
    box2_data$label_fun(sprintf('Efficacy endpoint 1%s', LETTERS[i]), 1, i)
    box2_data$label_fun('LY 100mg', 2, i)
    box2_data$label_fun('Placebo', 3, i)
}

box3_data <- add_benefits_box(box2_data, 0, 3, 0.04, 3, 5, 0, 1, "Efficacy endpoint", 
                              FALSE, direction='down')

# add labels to box3
for (i in 1:3) {
    box3_data$label_fun(sprintf('Efficacy endpoint 2%s', LETTERS[i]), 1, i)
    box3_data$label_fun('LY 100mg', 2, i, 1, 2)
    box3_data$label_fun('LY 200mg', 2, i, 2, 2)
    box3_data$label_fun('Placebo', 3, i)
}

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

simulate_forest_plot(box1_data$box, 3, col='purple')
simulate_forest_plot(box2_data$box, 1, col='purple')
simulate_forest_plot(box3_data$box, 2, col='forestgreen')