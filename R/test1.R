# library(dplyr)

# source("R/header.R")
# source("R/plot_box.R")
# source("R/plot_constructs.R")

library(tidyr)
library(dplyr)
library(grid)

data(mock_data)

# create benefits header
breaks_widths <- c(0.2, -0.1, 0.1, 0.2)
columns_specs <- c('Benefit'='endpoint', 
              'Treatment\n(N=100)'='treatment', 
              'Placebo\n(N=100)'='placebo',
              'Comparison\nHR or Odds Ratio\n(95% CI)'='col3')
value_collapse <- c(TRUE, FALSE, FALSE, FALSE)

split_axis_by_col <- 'estimator'
split_box_by_col <- 'endpoint'

part1_data <- plot_br(mock_data, columns_specs, breaks_widths, 
                      split_axis_by_col, split_box_by_col, 
                      neutral_pos = 2, num_ticks = 6, 
                      top_margin=NULL, value_collapse=value_collapse)

# risks

data(mock_data_risks)

breaks_width_risks <- c(0.2, -0.1, 0.1, 0.2)

columns_specs_risks <- c('Risk'='endpoint', 
              'Treatment\n(N=100)'='treatment', 
              'Placebo\n(N=100)'='placebo',
              'Comparison\nRisk Ratio\n(95% CI)'='txt_val')

value_collapse_risks <- c(FALSE, FALSE, FALSE, FALSE)

split_axis_by_col <- 'estimator'
split_box_by_col <- 'endpoint'

risks_options <- page_options$new()
risks_options$set_label_font_size(12)

part2_data <- plot_br(mock_data_risks, columns_specs_risks, breaks_width_risks,
                      split_axis_by_col, split_box_by_col, userect=TRUE,
                      neutral_pos = 2, num_ticks = 6, 
                      top_margin=1-part1_data$last_y+0.05, value_collapse=value_collapse_risks,
                      options_br=risks_options)
