library(grid)

# define page margins that will be used in visualization

PAGE_TOP_MARGIN <- 0.05
PAGE_BOTTOM_MARGIN <- 0.05
PAGE_LEFT_MARGIN <- 0.05
PAGE_RIGHT_MARGIN <- 0.05

HEADER_HEIGHT <- 0.05
HEADER_WIDTH  <- 1 - PAGE_LEFT_MARGIN - PAGE_RIGHT_MARGIN

# reset grid page
grid.newpage()

breaks_widths <- c(0.2, -0.1, 0.1)

# create header
create_header <- function(breaks_widths, labels=NULL) {
  # stop when labels is not null and has different length than breaks_widths
  if (!is.null(labels) && length(labels) != length(breaks_widths)) {
    stop("Length of labels must be the same as length of breaks_widths")
  }

  breaks <- c(cumsum(abs(breaks_widths)))
  
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
      grid.text(label = labels[i], 
                x = PAGE_LEFT_MARGIN+HEADER_WIDTH*(actual_breaks[i] + actual_breaks[i+1])/2, 
                y = 1 - PAGE_TOP_MARGIN - HEADER_HEIGHT/2,
                just = c('center', 'center'))
    }
  }

  # return all used parameters including margins
    return(list(PAGE_TOP_MARGIN = PAGE_TOP_MARGIN,
                PAGE_BOTTOM_MARGIN = PAGE_BOTTOM_MARGIN,
                PAGE_LEFT_MARGIN = PAGE_LEFT_MARGIN,
                PAGE_RIGHT_MARGIN = PAGE_RIGHT_MARGIN,
                HEADER_HEIGHT = HEADER_HEIGHT,
                HEADER_WIDTH = HEADER_WIDTH,
                breaks = actual_breaks,
                breaks_positions = actual_breaks*HEADER_WIDTH + PAGE_LEFT_MARGIN,
                name = 'header'))
}

add_benefit_arrows <- function(obj, neutral_relative_x, direction='up') {
    breaks <- obj$breaks

    # last two elements of breaks
    last_breaks <- breaks[(length(breaks)-1):length(breaks)]

    header_relative_x <- last_breaks[2]*neutral_relative_x + last_breaks[1]*(1-neutral_relative_x)

    # transform header_relative_x to actual x
    x <- obj$PAGE_LEFT_MARGIN + obj$HEADER_WIDTH*header_relative_x

    # calculate the shortest distance from header_relative_x to one of the last_breaks
    min_distance_to_breaks <- min(abs(header_relative_x - last_breaks))

    # transform to actual distance
    distance_to_breaks <- min_distance_to_breaks*obj$HEADER_WIDTH - 0.01

    # use grid to draw an arrow from x with length distance_to_breaks to the right and left
    # y is 90% of the header height

    grid.lines(x = c(x + 0.01, x + distance_to_breaks), 
               y = c(1 - obj$PAGE_TOP_MARGIN - obj$HEADER_HEIGHT*0.9, 1 - obj$PAGE_TOP_MARGIN - obj$HEADER_HEIGHT*0.9),
               arrow=arrow(type="closed", length=unit(0.1, "inches")), 
               gp = gpar(fill =  "#043099", col = "#043099")) 

    # draw text above the arrow
    grid.text(label = ifelse(direction=='up', 'Prefers LY', 'Prefers Placebo'), 
              x = x + distance_to_breaks/2, 
              y = 1 - obj$PAGE_TOP_MARGIN - obj$HEADER_HEIGHT*0.5,
              just = c('center', 'center'))

    grid.lines(x = c(x - 0.01, x - distance_to_breaks),
                y = c(1 - obj$PAGE_TOP_MARGIN - obj$HEADER_HEIGHT*0.9, 1 - obj$PAGE_TOP_MARGIN - obj$HEADER_HEIGHT*0.9),
                arrow=arrow(type="closed", length=unit(0.1, "inches")), 
                gp = gpar(fill = "#043099", col = "#043099")) 

    grid.text(label = ifelse(direction=='up', 'Prefers Placebo', 'Prefers LY'),
              x = x - distance_to_breaks/2, 
              y = 1 - obj$PAGE_TOP_MARGIN - obj$HEADER_HEIGHT*0.5,
              just = c('center', 'center'))
}

my_header <- create_header(breaks_widths, c('Endpoint', 'Treatment', 'Dose'))
add_benefit_arrows(my_header, 0.7, 'up')
