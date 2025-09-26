# create header
#' Create a header for a page
#'
#' This function creates a header for a page, using the specified breaks widths,
#' labels, and options.
#'
#' @param breaks_widths A numeric vector specifying the widths of the breaks relative to its total width.
#' @param labels A character vector specifying the labels for the breaks. Default is NULL. 
#'        If provided, the length of the labels must be the same as the length of breaks_widths.
#' @param header_text_size A numeric vector specifying the relative font size of the header labels.
#' @param options An instance of the page_options class specifying the options for the page. 
#'        Default is a new instance of the page_options class.
#'
#' @return The created header.
#'
#' @examples
#' \dontrun{
#' grid.newpage()
#' create_header(c(1, 2, 3), c("A", "B", "C"))
#' create_header(c(1, 2, 3), options = page_options$new())
#' }
#' 
#' @import grid
#' 
#' @export
create_header <- function(breaks_widths, labels=NULL, header_text_size=NULL, options=page_options$new()) {

  # stop when labels is not null and has different length than breaks_widths
  if (!is.null(labels) && length(labels) != length(breaks_widths)) {
    stop("Length of labels must be the same as length of breaks_widths")
  }

  breaks <- c(cumsum(abs(breaks_widths)))

  PAGE_BOTTOM_MARGIN <- options$get_option('PAGE_BOTTOM_MARGIN')
  PAGE_TOP_MARGIN <- options$get_option('PAGE_TOP_MARGIN')
  PAGE_LEFT_MARGIN <- options$get_option('PAGE_LEFT_MARGIN')
  PAGE_RIGHT_MARGIN <- options$get_option('PAGE_RIGHT_MARGIN')
  HEADER_HEIGHT <- options$get_option('HEADER_HEIGHT')
  HEADER_WIDTH <- options$get_option('HEADER_WIDTH')

  # use grid to plot a rectangle with specified margins
  grid.rect(x = PAGE_LEFT_MARGIN, y = 1 - PAGE_TOP_MARGIN,
            width = 1 - PAGE_LEFT_MARGIN - PAGE_RIGHT_MARGIN,
            height = HEADER_HEIGHT, 
            gp = gpar(fill = "lightblue"), just=c('left', 'top'))
  
  # plot vertical segments within the rectangle, with specified breaks
  # as the ratio of the width of the rectangle
  
  for (i in seq_along(breaks)) {
    if (breaks_widths[i] < 0) next
    grid.lines(x = c(breaks[i], breaks[i])*HEADER_WIDTH+PAGE_LEFT_MARGIN, 
               y = c(1 - PAGE_TOP_MARGIN, 1 - PAGE_TOP_MARGIN - HEADER_HEIGHT),
               gp = gpar(col = "black"))
  }
  
  actual_breaks <- c(0, breaks, 1)
  # add labels to the segments
  if (!is.null(labels)) {
    for (i in 1:(length(actual_breaks) - 2)) {
      text_size_ <- options$get_option('header.label.font.size')
      if (!is.null(header_text_size) & !is.null(header_text_size[i])) {
        text_size_ <- text_size_*header_text_size[i]
      }
      
      grid.text(label = labels[i], 
                x = PAGE_LEFT_MARGIN+HEADER_WIDTH*(actual_breaks[i] + actual_breaks[i+1])/2, 
                y = 1 - PAGE_TOP_MARGIN - HEADER_HEIGHT/2,
                just = c('center', 'center'),
                gp = gpar(fontsize = text_size_, fontface = "bold", col=options$header.label.color))
    }
  }

  # return all used parameters including margins
    return(list(options = options,
                breaks = actual_breaks,
                breaks_positions = actual_breaks*HEADER_WIDTH + PAGE_LEFT_MARGIN,
                name = 'header'))
}

#' Add benefit arrows to the header
#'
#' This function adds benefit arrows to the header of a plot. The arrows indicate the direction
#' of benefit, either favoring the last year (LY) or favoring the placebo. The arrows are drawn
#' using the grid package.
#'
#' @param obj An object containing the plot parameters and options.
#' @param neutral_relative_x The relative x-coordinate of the neutral position of the arrow.
#' @param direction The direction of the arrows. Default is 'up'.
#' @param labels The labels for the arrows. Default is c('Favors LY', 'Favors Placebo').
#' @param col The color of the arrows. Default is '#043099'.
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' # Create a plot object
#' obj <- create_header(c(1, 2, 3))
#'
#' # Add benefit arrows to the header
#' add_benefit_arrows(obj, neutral_relative_x = 0.5, direction = 'up',
#'                    labels = c('Favors LY', 'Favors Placebo'), col = '#043099')
#' }
#'
#' @export
add_benefit_arrows <- function(obj, neutral_relative_x, direction='up',
                               labels=c('Favors LY', 'Favors Placebo'), 
                               col='#043099') {
    breaks <- obj$breaks

    HEADER_HEIGHT <- obj$options$get_option('HEADER_HEIGHT')
    PAGE_TOP_MARGIN <- obj$options$get_option('PAGE_TOP_MARGIN')
    PAGE_LEFT_MARGIN <- obj$options$get_option('PAGE_LEFT_MARGIN')
    HEADER_WIDTH <- obj$options$get_option('HEADER_WIDTH')

    label_font_size <- obj$options$get_option('header.label.font.size')

    # last two elements of breaks
    last_breaks <- breaks[(length(breaks)-1):length(breaks)]

    header_relative_x <- last_breaks[2]*neutral_relative_x + last_breaks[1]*(1-neutral_relative_x)

    # transform header_relative_x to actual x
    x <- PAGE_LEFT_MARGIN + HEADER_WIDTH*header_relative_x

    # calculate the shortest distance from header_relative_x to one of the last_breaks
    min_distance_to_breaks <- min(abs(header_relative_x - last_breaks))

    # transform to actual distance
    distance_to_breaks <- min_distance_to_breaks*HEADER_WIDTH - 0.01

    # use grid to draw an arrow from x with length distance_to_breaks to the right and left
    # y is 90% of the header height

    b_arrow <- arrow(type="closed", angle=10, length=unit(0.02, "npc"))

    # left arrow
    grid.lines(x = c(x + 0.01, x + distance_to_breaks), 
               y = c(1 - PAGE_TOP_MARGIN - HEADER_HEIGHT*0.9, 1 - PAGE_TOP_MARGIN - HEADER_HEIGHT*0.9),
               arrow=b_arrow, 
               gp = gpar(fill =  col, col = col)) 

    # draw text above the arrow
    grid.text(label = ifelse(direction=='up', labels[1], labels[2]), 
              x = unit(x + distance_to_breaks/2, 'npc'), 
              y = unit(1 - PAGE_TOP_MARGIN - HEADER_HEIGHT*0.5, 'npc'),
              just = c('center', 'center'), 
              gp = gpar(fontsize = label_font_size, fontface="bold", col=obj$options$header.label.color))

    # right arrow
    grid.lines(x = c(x - 0.01, x - distance_to_breaks),
                y = c(1 - PAGE_TOP_MARGIN - HEADER_HEIGHT*0.9, 1 - PAGE_TOP_MARGIN - HEADER_HEIGHT*0.9),
                arrow=b_arrow, 
                gp = gpar(fill = col, col = col)) 

    grid.text(label = ifelse(direction=='up', labels[2], labels[1]),
              x = x - distance_to_breaks/2, 
              y = 1 - PAGE_TOP_MARGIN - HEADER_HEIGHT*0.5,
              just = c('center', 'center'),  gp = gpar(fontsize = label_font_size, fontface="bold", col=obj$options$header.label.color))
}

