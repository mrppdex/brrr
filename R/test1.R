# library(dplyr)

# source("R/header.R")
# source("R/plot_box.R")
# source("R/plot_constructs.R")

library(tidyr)
library(dplyr)
library(grid)

mock_data <- tribble(
  ~benefit, ~treatment, ~placebo, ~estimator, ~value, ~lower, ~upper, ~reversed,
  "End Point 1A", 0.8, 0.6, "Hazard Ratio (95% CI)", 1.1, 0.5, 1.2, FALSE,
  "End Point 1A", 0.8, 0.6, "Hazard Ratio (95% CI)", 1.6, 0.9, 2.0, FALSE, 
  "End Point 1B", 0.7, 0.5, "Hazard Ratio (95% CI)",  0.7, 0.4, 1.1, FALSE,
  "End Point 2A", 0.7, 0.5, "Odds Ratio (95% CI)",  1, 0.4, 1.1, TRUE, 
  "End Point 2B", 0.7, 0.5, "Odds Ratio (95% CI)",  0.45, 0.4, 0.5, TRUE
)

favor_direction <- 'up'

grid.newpage()

# create header
breaks_widths <- c(0.2, -0.1, 0.1, 0.2)
h_labels <- c('Benefits', 'Treatment\n(N=100)', 'Placebo\n(N=100)',
              'Comparison\nHR or Odds Ratio\n(95% CI)')

stopifnot(length(breaks_widths)==length(h_labels))

header2 <- create_header(breaks_widths, h_labels)

mock_data <- mutate(mock_data, col3 = sprintf('%.2f (%.2f, %.2f)', value, lower, upper))

mock_data_meta <-
  mock_data %>% group_by(estimator) %>% 
  mutate(ncats=length(unique(benefit))) %>%
  mutate(minval=min(value, lower, upper), maxval=max(value, lower, upper)) %>%
  mutate(reversed=any(reversed)) %>%
  group_by(benefit, estimator) %>% mutate(nsubcats=n()) %>%
  ungroup() %>%
  distinct(benefit, estimator, ncats, nsubcats, minval, maxval, reversed)

last_graph_part <- header2

boxes <- list()

for (est in unique(mock_data_meta$estimator)) {
  mock_data_meta_subset <- mock_data_meta %>% filter(estimator==est)

  minval <- min(mock_data_meta_subset$minval)
  maxval <- max(mock_data_meta_subset$maxval)

  ncats <- unique(mock_data_meta_subset$ncats)

  spacing <- ifelse(last_graph_part$name=='header', 0, 0.05)

  is_reversed <- any(mock_data_meta_subset$reversed)

  last_graph_part <- add_box(last_graph_part, spacing, ncats, 0.03, 3, 6, 
                                      ifelse(is_reversed, maxval, minval), 
                                      ifelse(is_reversed, minval, maxval), label=est, logscale=TRUE, b=2, 
                                      show_axis=TRUE, direction=favor_direction)

  boxes[[est]] <- list(box=last_graph_part)
}

for (est in unique(mock_data_meta$estimator)) {

  column_names   <- c('benefit', 'treatment', 'placebo', 'col3')
  value_collapse <- c(TRUE, FALSE, FALSE, FALSE)
  
  mock_data_subset <- mock_data %>% filter(estimator==est)

  box <- boxes[[est]]
  add_label <- box$box$label_fun
  ncats <- box$ncats

  unique_benefits <- unique(mock_data_subset$benefit)

  # plot data
  for (ben_idx in seq_along(unique_benefits)) {
    ben <- unique_benefits[ben_idx]
    data_sub_subset <- mock_data_subset %>% filter(benefit==ben)

    # add labels to each box
    for (cn_idx in seq_along(column_names)) {
      column_name <- column_names[cn_idx]
      unique_column_names <- data_sub_subset[[column_name]]
      if(value_collapse[cn_idx]) {
        unique_column_names <- unique(unique_column_names)
      }
      
      for (j in 1:length(unique_column_names)) {
        add_label(unique_column_names[j], cn_idx, ben_idx, n=j, N=length(unique_column_names), 
                  col=box$box$header$options$get_palette()[j])
      }
      
    }

    cat('so far so good\n')
    # plot the forest plot
    for(k in 1:nrow(data_sub_subset)) {
      plot_forest_tree(box$box$box, data_sub_subset[k, 'lower'], data_sub_subset[k, 'upper'], 
                       data_sub_subset[k, 'value'], ben_idx, k, nrow(data_sub_subset), 
                       col=NULL, br_palette=box$box$header$options$get_palette())
    }
  }  

}

new_options <- page_options$new()
new_options$set_page_parameter('PAGE_TOP_MARGIN', 1-boxes[[2]]$box$y_pos+0.05)

# risk header labels
risk_labels <- c('Risk', h_labels[2:length(h_labels)])
new_header <- create_header(c(0.1, -0.1, 0.1, 0.2), risk_labels, options=new_options)

# add box to new header
last_graph_part <- add_box(new_header, 0, 1, 0.03, 3, 6, 0, 1, label='New Box', logscale=FALSE, b=2, 
                           show_axis=TRUE, direction=favor_direction, userect=TRUE)
last_graph_part$label_fun(expression(Omega), 1, 1, fontsize=20)
last_graph_part$label_fun(expression(alpha), 2, 1, fontsize=20)
last_graph_part$label_fun(expression(beta), 3, 1, fontsize=20)
last_graph_part$label_fun(expression(gamma), 4, 1, fontsize=20) 

plot_forest_tree(last_graph_part$box, 0.2, 0.8, 0.5, 1, 1, 1, col='black', height=NULL, userect=TRUE)