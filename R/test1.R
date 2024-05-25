# library(dplyr)

# source("R/header.R")
# source("R/plot_box.R")
# source("R/plot_constructs.R")

library(tidyr)
library(dplyr)
library(grid)

# create benefits header
breaks_widths <- c(0.2, -0.1, 0.1, 0.2)
columns_specs <- c('Benefit'='endpoint', 
              'Treatment\n(N=100)'='treatment', 
              'Placebo\n(N=100)'='placebo',
              'Comparison\nHR or Odds Ratio\n(95% CI)'='col3')
value_collapse <- c(TRUE, FALSE, FALSE, FALSE)

split_axis_by_col <- 'estimator'
split_box_by_col <- 'endpoint'

# load data and add column
data(mock_data)
part1_data <- plot_br(mock_data, columns_specs, breaks_widths, split_axis_by_col, split_box_by_col, 
                      neutral_pos = 2, num_ticks = 6, top_margin=NULL, value_collapse=value_collapse)

# function to calculate risk ratio and its confidence interval
mock_data_risks <- data.frame(
  endpoint = c("Risk 1", "Risk 2"),
  treatment = c(0.1, 0.2),
  placebo = c(0.2, 0.3)
) %>% 
  mutate(value = treatment/placebo) %>%
  mutate(se = sqrt(1/treatment + 1/placebo)) %>%
  mutate(lower = exp(log(value) - 1.96 * se),
         upper = exp(log(value) + 1.96 * se)) %>%
  select(-se) %>%
  mutate(logscale = TRUE, logbase = 10) %>%
  mutate(estimator = 'Relative Risk (95% CI)') %>%
  mutate(txt_val = sprintf("%.2f (%.2f, %.2f)", value, lower, upper))


breaks_width_risks <- c(0.2, -0.1, 0.1, 0.2)

columns_specs_risks <- c('Risk'='endpoint', 
              'Treatment\n(N=100)'='treatment', 
              'Placebo\n(N=100)'='placebo',
              'Comparison\nRisk Ratio\n(95% CI)'='txt_val')

value_collapse_risks <- c(FALSE, FALSE, FALSE, FALSE)

split_axis_by_col <- 'estimator'
split_box_by_col <- 'endpoint'

part2_data <- plot_br(mock_data_risks, columns_specs_risks, breaks_width_risks, 
                      split_axis_by_col, split_box_by_col, neutral_pos = 2, userect=TRUE,
                      num_ticks = 6, top_margin=1-part1_data$last_y+0.05, value_collapse=value_collapse_risks)
