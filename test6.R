library(testthat)
library(ggplot2)
library(grid)
library(dplyr)


NUMBER_OF_TICKS <- 6 # number of ticks on the axis
NEUTRAL_POSITION <- 2 # position of the neutral value on the axis
AXIS_START <- 0.5 # the start of the axis
AXIS_END <- 0.9 # the end of the axis
MARGIN_TOP_BEN <- 0.1 # margin top for benefits
MARGIN_BOTTOM_BEN <- 0.3 # margin bottom for benefits
FONT_SIZE_LABELS <- 14 # font size for labels

# simulate data
# columns: group, type (linear or log), base (used by log), label, value, lower, upper
data <- tibble::tribble(
  ~group, ~type,   ~base, ~label, ~value, ~lower, ~upper,
      1L, "linear",    NA,   "A",    10,      5,     11,
      1L, "linear",    NA,   "B",    20,     19,     30,
      1L, "linear",    NA,   "C",    30,     29,     31,
      2L,     "log",     2,   "D",  1/8,   1/16,    1/4,
      2L,     "log",     2,   "E",    1,    1/2,      2,
      2L,     "log",     2,   "F",   64,     32,    128,
      3L, "linear",    NA,   "G",    15,     10,     20,
      3L, "linear",    NA,   "H",    25,     20,     30,
      3L, "linear",    NA,   "I",    35,     30,     40,
      3L, "linear",    NA,   "M",    45,     40,     50,
      4L,     "log",     3,   "J",  1/4,   1/8,    1/2,
      4L,     "log",     3,   "K",    2,    1,      4,
      4L,     "log",     3,   "L",   128,     64,    256,
      5L,     "log",    10,  "N",  1/10,  1/100,   1/5,
      5L,     "log",    10,  "O",    1,    1/10,     10,
      5L,     "log",    10,  "P",   100,     10,    1000,
      6L, "linear",    NA,  "Q",    20,     15,     25,
      6L, "linear",    NA,  "R",    30,     25,     35,
)

augment_data <- function(data) {
  data <- data %>%
    group_by(group) %>% # nolint
    mutate(
      min_value = min(value, lower, upper),
      max_value = max(value, lower, upper)
    ) %>%
    mutate(
      min_value = ifelse(type == "linear", 
                         floor(min_value)-1, 
                         base^(floor(log(min_value, base))-1)),
      max_value = ifelse(type == "linear", 
                         ceiling(max_value)+1, 
                         base^ceiling(log(max_value, base)+1))
    ) %>%
    mutate(
      min_value = case_when(
        type == "linear" & min_value > 0 ~ -1,
        type == "log" & min_value > 1 ~ 1/base,
        TRUE ~ min_value
      ),
      max_value = case_when(
        type == "linear" & max_value < 0 ~ 1,
        type == "log" & max_value < 0 ~ base,
        TRUE ~ max_value
      )
    ) %>% 
    ungroup()
  
  return(data)
}

data <- augment_data(data)

linear_scale <- function(x, start_x, end_x, a, b) {
  return ((x - start_x) / (end_x - start_x) * (b - a) + a)
}

log_scale <- function(x, start_x, end_x, a, b, base=2) {
  return ((log(x, base) - log(start_x, base)) / (log(end_x, base) - log(start_x, base)) * (b - a) + a)
}

format_number <- function(n) {
  if (n >= 1e9) {
    sprintf("%.1fg", n / 1e9)
  } else if (n >= 1e6) {
    sprintf("%.1fm", n / 1e6)
  } else if (n >= 1e3) {
    sprintf("%.0fk", n / 1e3)
  } else {
    as.character(n)
  }
}

# create a low level code to plot only an axis using grid package
plot_axis <- function(y_origin, start_x, end_x, a, b, ticks_number=10, scale_type = "linear", base=2) {

  if(scale_type=='linear') {
    start_x <- floor(start_x)
    end_x <- ceiling(end_x)
  } else {
    start_x <- base^floor(log(start_x, base))
    end_x <- base^ceiling(log(end_x, base))
  }

  unit_size <- ifelse(scale_type=="linear", 
    #(floor(end_x) - floor(start_x)) / ticks_number, 
    as.integer((floor(end_x) - floor(start_x)) / NUMBER_OF_TICKS),
    (log(end_x, base) - log(start_x, base)) / NUMBER_OF_TICKS)

  x <- NULL
  
  if (scale_type == "log") {
    x <- base^seq(log(start_x, base), log(end_x, base), by=unit_size)
    cat('x: ', x, '\n')
  } else {
    step <- (ceiling(end_x)-floor(start_x))/NUMBER_OF_TICKS
    x <- seq(start_x, end_x, by=unit_size)
  }
  
  y <- y_origin

  if (scale_type == "linear") {
    x_scaled <- linear_scale(x, start_x, end_x, a, b)
  } else if (scale_type == "log") {
    x_scaled <- log_scale(x, start_x, end_x, a, b, base)
  } else {
    stop("Invalid scale_type. Must be either 'linear' or 'log'.")
  }

  tick_labels_text <- NULL
  if (scale_type == "log") {
    tick_labels_text <- log(x, base)
    tick_labels_text <- sapply(tick_labels_text, 
      function(x) ifelse(x <0, paste0('1/',base^abs(x)), format_number(base^x)))
    cat('tick_labels_text: ', tick_labels_text, '\n')
  } else {
    tick_labels_text <- round(x, 2)
  }

  axis_line <- grid::segmentsGrob(x0=a, x1=b, y0=y, y1=y, gp=grid::gpar(lwd=2))
  axis_ticks <- grid::segmentsGrob(x0=x_scaled, x1=x_scaled, y0=y, y1=y-0.01, gp=grid::gpar(lwd=1))
  tick_labels <- grid::textGrob(label=tick_labels_text, x=x_scaled, y=y-0.02, gp=grid::gpar(fontsize=16))
  
  grid::grid.draw(axis_line)
  grid::grid.draw(axis_ticks)
  grid::grid.draw(tick_labels)
}

# generate forest plot for each of rows in data grouped by group
plot_forest <- function(ref_y, ref_x, axis_number, data, weights, margin_bottom=MARGIN_BOTTOM_BEN, 
                        margin_top=MARGIN_TOP_BEN, single_height=0.03, distance_from_axis=0.02) {

  #total_height <- nrow(data)*single_height
  #weights <- c(0, weights)[1:(length(weights)-1)]
  total_height <- (1-(margin_top+margin_bottom))*weights[axis_number]
  

  # calculate the y position of each row
  y <- ref_y + distance_from_axis + seq(0, nrow(data)-1) * min(single_height, (total_height - 2.5*distance_from_axis) / nrow(data))
  cat('y: ', y, '\n')

  # plot the forest plot
  for (i in 1:nrow(data)) {

    value <- data$scaled_value[i]
    lower <- data$scaled_lower[i]
    upper <- data$scaled_upper[i]

    grid.circle(x=value, y=y[i], r=0.005, gp=grid::gpar(fill="black"))
    grid.segments(x0=lower, x1=upper, y0=y[i], y1=y[i], gp=grid::gpar(lwd=2))
    grid.text(label=data$label[i], x=ref_x-0.02, y=y[i], just="right", gp=grid::gpar(fontsize=FONT_SIZE_LABELS))
    # add the name of the group to the left of the plot, in the middle of the y position
    grid.text(label=paste0('Group ', data$group[i]), x=ref_x-0.2, y=mean(y), just="right", gp=grid::gpar(fontsize=20))

    # for each row, draw faing horizontal line
    grid.segments(x0=AXIS_START, x1=AXIS_END, y0=y[i], y1=y[i], gp=grid::gpar(lty=2))
  }

  # calculate the x position of the neutral value
  neutral_x <- ifelse(data$type[1] == "linear", 
    linear_scale(0, data$min_value[1], data$max_value[1], AXIS_START, AXIS_END), 
    log_scale(1, data$min_value[1], data$max_value[1], AXIS_START, AXIS_END))

  # draw a vertical dashed line at neutral_x that goes through all rows
  grid.segments(x0=neutral_x, x1=neutral_x, y0=ref_y, y1=max(y), gp=grid::gpar(lty=2, lwd=0.7))
}

axis_y_position <- function(total_axes, axis_number, weights,  margin_top=0.1, margin_bottom=0.3) {
  weights <- c(0, weights)
  return((1-(margin_top+margin_bottom))*sum(weights[1:axis_number]) + margin_bottom)
}

plot_benefits <- function(data, margin_top=0.1, margin_bottom=0.3, newpage=TRUE) {
  
  if(newpage) grid.newpage()

  number_of_axes <- data %>% distinct(group) %>% nrow()
  axes_specs <- data %>% distinct(group, type, base, min_value, max_value) %>% as.data.frame()

  for (i in 1:number_of_axes) {
    axis_data <- axes_specs %>% filter(group == i)

    group_min_value <- axis_data$min_value
    group_max_value <- axis_data$max_value
    
    if (axis_data$type == "linear") {

      while((group_max_value) %% (NUMBER_OF_TICKS-NEUTRAL_POSITION) != 0) {
        group_max_value <- group_max_value + 1
      }
      
      group_min_value <- -group_max_value*NEUTRAL_POSITION/(NUMBER_OF_TICKS-NEUTRAL_POSITION)

    } else if (axis_data$type == "log") {
      log_base <- axis_data$base

      log_group_min_value <- log(group_min_value, log_base)
      log_group_max_value <- log(group_max_value, log_base)

      while((log_group_max_value) %% (NUMBER_OF_TICKS-NEUTRAL_POSITION) != 0) {
        log_group_max_value <- log_group_max_value + 1
      }

      log_group_min_value <- -log_group_max_value*NEUTRAL_POSITION/(NUMBER_OF_TICKS-NEUTRAL_POSITION)

      group_min_value <- log_base^log_group_min_value
      group_max_value <- log_base^log_group_max_value
    } else {
      stop("Invalid axis type. Must be either 'linear' or 'log'.")
    }

    group_weights <- data %>% 
      mutate(N=n()) %>% 
      group_by(N, group) %>% 
      summarize(n=n()) %>% 
      ungroup() %>% 
      mutate(weights=n/N) %>%
      arrange(group) %>%
      pull(weights)

    cat('group_weights: ', group_weights, '\n')

    plot_axis(axis_y_position(number_of_axes, i, group_weights, margin_top, margin_bottom), 
      group_min_value, 
      group_max_value, AXIS_START, AXIS_END, NUMBER_OF_TICKS,
      axis_data$type, axis_data$base)

    # update original data with new group_min_value and group_max_value
    group_data <- data %>%
      filter(group == i) %>% 
      mutate(min_value=ifelse(group==i, group_min_value, min_value),
          max_value=ifelse(group==i, group_max_value, max_value)
      ) %>%
      mutate(
        scaled_lower = ifelse(type == "linear", linear_scale(lower, min_value, max_value, AXIS_START, AXIS_END), log_scale(lower, min(min_value), max(max_value), AXIS_START, AXIS_END)),
        scaled_upper = ifelse(type == "linear", linear_scale(upper, min_value, max_value, AXIS_START, AXIS_END), log_scale(upper, min(min_value), max(max_value), AXIS_START, AXIS_END)),
        scaled_value = ifelse(type == "linear", linear_scale(value, min_value, max_value, AXIS_START, AXIS_END), log_scale(value, min(min_value), max(max_value), AXIS_START, AXIS_END))
      )

    axis_y_pos_vec <- sapply(1:number_of_axes, function(x) axis_y_position(number_of_axes, x, group_weights))

    plot_forest(axis_y_position(number_of_axes, i, group_weights, margin_top, margin_bottom), 
                AXIS_START, i, group_data, group_weights, margin_bottom, margin_top)
  }
}

plot_benefits(data, 0.1, 0.1, newpage=TRUE)
#plot_benefits(data, 0.55, 0.1, newpage=FALSE)
