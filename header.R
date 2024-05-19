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

breaks_widths <- c(0.2, 0.1, 0.1)

# create header
create_header <- function(breaks_widths) {
  breaks <- c(cumsum(breaks_widths))
  
  # use grid to plot a rectangle with specified margins
  grid.rect(x = PAGE_LEFT_MARGIN, y = 1 - PAGE_TOP_MARGIN,
            width = 1 - PAGE_LEFT_MARGIN - PAGE_RIGHT_MARGIN,
            height = HEADER_HEIGHT, 
            gp = gpar(fill = "lightblue"), just=c('left', 'top'))
  
  # plot vertical segments within the rectangle, with specified breaks
  # as the ratio of the width of the rectangle
  
  for (i in seq_along(breaks)) {
    grid.lines(x = c(breaks[i], breaks[i])*HEADER_WIDTH+PAGE_LEFT_MARGIN, 
               y = c(1 - PAGE_TOP_MARGIN, 1 - PAGE_TOP_MARGIN - HEADER_HEIGHT),
               gp = gpar(col = "black"))
  }
  
  actual_breaks <- c(0, 
                     breaks, 
                     1)
  # add a label to each of the rectangle segments
  for (i in 1:(length(actual_breaks) - 1)) {
    cat("Segment", i, "starts at", actual_breaks[i], "and ends at", actual_breaks[i+1], "\n")
    grid.text(label = paste("Segment", i), 
              x = PAGE_LEFT_MARGIN+HEADER_WIDTH*(actual_breaks[i] + actual_breaks[i+1])/2, 
              y = 1 - PAGE_TOP_MARGIN - HEADER_HEIGHT/2,
              just = c('center', 'center'))
  }

  # return all used parameters including margins
    return(list(PAGE_TOP_MARGIN = PAGE_TOP_MARGIN,
                PAGE_BOTTOM_MARGIN = PAGE_BOTTOM_MARGIN,
                PAGE_LEFT_MARGIN = PAGE_LEFT_MARGIN,
                PAGE_RIGHT_MARGIN = PAGE_RIGHT_MARGIN,
                HEADER_HEIGHT = HEADER_HEIGHT,
                HEADER_WIDTH = HEADER_WIDTH,
                breaks = actual_breaks))
}

res <- create_header(breaks_widths)

