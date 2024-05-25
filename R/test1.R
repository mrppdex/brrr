# library(dplyr)

# source("R/header.R")
# source("R/plot_box.R")
# source("R/plot_constructs.R")

library(tidyr)
library(dplyr)
library(grid)

# create header
breaks_widths <- c(0.2, -0.1, 0.1, 0.2)
columns_specs <- c('Benefits'='endpoint', 
              'Treatment\n(N=100)'='treatment', 
              'Placebo\n(N=100)'='placebo',
              'Comparison\nHR or Odds Ratio\n(95% CI)'='col3')
value_collapse <- c(TRUE, FALSE, FALSE, FALSE)

split_axis_by_col <- 'estimator'
split_box_by_col <- 'endpoint'

# load data and add column
data(mock_data)
part1_data <- plot_br(mock_data, columns_specs, breaks_widths, split_axis_by_col, split_box_by_col, top_margin=NULL, value_collapse=value_collapse)

