# library(dplyr)

# source("R/header.R")
# source("R/plot_box.R")
# source("R/plot_constructs.R")

library(tidyr)
library(dplyr)
library(grid)

# load data and add column
data(mock_data)

get_metadata <- function(data, split_axis_by_col, split_box_by_col) {
  mock_data_meta <- data %>%
    group_by_at({{split_axis_by_col}}) %>%
    mutate(across({{split_box_by_col}}, ~ length(unique(.)), .names="ncats")) %>%
    mutate(minval = min(value, lower, upper), maxval = max(value, lower, upper)) %>%
    mutate(reversed = any(reversed)) %>%
    group_by_at(c({{split_box_by_col}}, {{split_axis_by_col}})) %>%
    mutate(nsubcats = n()) %>%
    ungroup() %>%
    select(any_of(c(split_axis_by_col, split_box_by_col, 'ncats', 'nsubcats', 'minval', 'maxval', 'reversed', 'logscale', 'logbase'))) %>%
    distinct()
  
  return(mock_data_meta)
}

# create header
breaks_widths <- c(0.2, -0.1, 0.1, 0.2)
h_labels <- c('Benefits'='endpoint', 
              'Treatment\n(N=100)'='treatment', 
              'Placebo\n(N=100)'='placebo',
              'Comparison\nHR or Odds Ratio\n(95% CI)'='col3')
value_collapse <- c(TRUE, FALSE, FALSE, FALSE)

# make sure lengths are ok
stopifnot(length(breaks_widths)==length(h_labels))


grid.newpage()


# 1. CREATE HEADER
header2 <- create_header(breaks_widths, names(h_labels))

# 2. ADD BOXES

split_axis_by_col <- 'estimator'
split_box_by_col <- 'endpoint'

mock_data_meta <- get_metadata(mock_data, split_axis_by_col, split_box_by_col)

# remember last added graph part
last_graph_part <- header2
boxes <- list()

for (est in unique(mock_data_meta[[split_axis_by_col]])) {
  mock_data_meta_subset <- mock_data_meta %>% 
    filter(if_all(split_axis_by_col, ~ .x==est))

  minval <- min(mock_data_meta_subset$minval)
  maxval <- max(mock_data_meta_subset$maxval)

  ncats <- unique(mock_data_meta_subset$ncats)

  logscale <- any(mock_data_meta_subset$logscale, na.rm = TRUE)
  logbase  <- unique(mock_data_meta_subset$logbase, na.rm = TRUE)[1]

  spacing <- ifelse(last_graph_part$name=='header', 0, 0.05)

  is_reversed <- any(mock_data_meta_subset$reversed)

  last_graph_part <- add_box(last_graph_part, spacing, ncats, 0.03, 3, 6, 
                                      ifelse(is_reversed, maxval, minval), 
                                      ifelse(is_reversed, minval, maxval), label=est, 
                                      logscale=(!is.na(logscale) & logscale), 
                                      b=ifelse(is.na(logbase), 2, logbase), 
                                      show_axis=TRUE)

  boxes[[est]] <- list(box=last_graph_part)
}

for (est in unique(mock_data_meta[[split_axis_by_col]])) {
  column_names   <- names(h_labels)
  
  mock_data_subset <- mock_data %>% 
    filter(if_all(split_axis_by_col, ~ .x==est))

  box <- boxes[[est]]
  add_label <- box$box$label_fun
  ncats <- box$ncats

  unique_benefits <- unique(mock_data_subset[[split_box_by_col]])

  # plot data
  for (ben_idx in seq_along(unique_benefits)) {

    ben <- unique_benefits[ben_idx]
    data_sub_subset <- mock_data_subset %>% 
        filter(if_all(split_box_by_col, ~ .x==ben)) 

    # add labels to each box
    for (cn_idx in seq_along(column_names)) {
      column_name <- h_labels[cn_idx]
      unique_column_names <- data_sub_subset[[column_name]]
      if(value_collapse[cn_idx]) {
        unique_column_names <- unique(unique_column_names)
      }
      
      for (j in 1:length(unique_column_names)) {
        add_label(unique_column_names[j], cn_idx, ben_idx, n=j, N=length(unique_column_names), 
                  col=box$box$header$options$get_palette()[j])
      }
      
    }

    # plot the forest plot
    for(k in 1:nrow(data_sub_subset)) {
      plot_forest_tree(box$box$box, data_sub_subset[k, 'lower'], data_sub_subset[k, 'upper'], 
                       data_sub_subset[k, 'value'], ben_idx, k, nrow(data_sub_subset), 
                       col=NULL, br_palette=box$box$header$options$get_palette())
    }
  }  

}

# new_options <- page_options$new()
# new_options$set_page_parameter('PAGE_TOP_MARGIN', 1-boxes[[2]]$box$y_pos+0.05)

# # risk header labels
# risk_labels <- c('Risk', names(h_labels)[2:length(h_labels)])
# new_header <- create_header(c(0.1, -0.1, 0.1, 0.2), risk_labels, options=new_options)

# # add box to new header
# last_graph_part <- add_box(new_header, 0, 1, 0.03, 3, 6, 0, 1, label='New Box', logscale=FALSE, b=2, 
#                            show_axis=TRUE, direction=favor_direction, userect=TRUE)
# last_graph_part$label_fun(expression(Omega), 1, 1, fontsize=20)
# last_graph_part$label_fun(expression(alpha), 2, 1, fontsize=20)
# last_graph_part$label_fun(expression(beta), 3, 1, fontsize=20)
# last_graph_part$label_fun(expression(gamma), 4, 1, fontsize=20) 

# plot_forest_tree(last_graph_part$box, 0.2, 0.8, 0.5, 1, 1, 1, col='black', height=NULL, userect=TRUE)