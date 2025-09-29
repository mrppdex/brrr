library(brrr)
library(dplyr)

# create header
columns_specs <- c('Benefits'='endpoint', 
              'Treatment\n(N=100)'='treatment', 
              'Placebo\n(N=100)'='placebo',
              'Comparison\nHR or Odds Ratio\n(95% CI)'='col3')
value_collapse <- c(TRUE, FALSE, FALSE, FALSE)

split_axis_by_col <- 'estimator'
split_box_by_col <- 'endpoint'

# load data and add column
data(mock_data)

# create a subdirectory for the output
dir.create("output")

plot_br(mock_data, columns_specs, c(0.2, -0.1, 0.1, 0.2), 
        split_axis_by_col, split_box_by_col, 'endpoint', 
        filename = "output/plot.svg", options_br = page_options$new(plot.width = 10, plot.height = 12))
