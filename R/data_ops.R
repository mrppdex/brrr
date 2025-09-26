#' Get Metadata for a Dataset
#'
#' @description
#' This function calculates metadata for a given dataset. The metadata includes
#' the number of unique categories, minimum and maximum values, whether the data
#' is reversed, and the number of subcategories. This information is crucial
#' for constructing the plot layout.
#'
#' @param data The dataset to calculate metadata for.
#' @param split_axis_by_col The column to split the data by on the y-axis.
#' @param axis_labels_col The column containing the axis labels.
#' @param split_box_by_col The column to split the data by in each box.
#' @param vline_col The column for vertical lines.
#'
#' @return A data frame containing the calculated metadata.
#'
#' @import dplyr
#' @import tidyr
#' @export
get_metadata <- function(data, split_axis_by_col, axis_labels_col, split_box_by_col, vline_col) {

    if (nrow(data) == 0) {
        return(data.frame())
    }

    if (!'reversed' %in% colnames(data)) {
        data$reversed <- FALSE
    }
  
    safe_max <- function(x) {
      if(all(is.na(unique(x)))) return(NA)
      max(x[!is.na(x)])
    }
    
    mock_data_meta <- data %>%
        group_by_at({{split_axis_by_col}}) %>%
        mutate(across({{split_box_by_col}}, ~ length(unique(.)), .names="ncats")) %>%
        mutate(across(any_of(vline_col), ~ safe_max(.), .names="vertical_line")) %>%
        mutate(vertical_line = ifelse('vertical_line' %in% colnames(.), vertical_line, NA)) %>%
        mutate(minval = min(c(value, lower, upper, as.numeric(vertical_line)), na.rm=TRUE), 
               maxval = max(c(value, lower, upper, as.numeric(vertical_line)), na.rm=TRUE)) %>%
        mutate(reversed = any(reversed)) %>%
        group_by_at(c({{split_box_by_col}}, {{split_axis_by_col}})) %>%
        mutate(nsubcats = n()) %>%
        ungroup() %>%
        select(any_of(c(split_axis_by_col, axis_labels_col, split_box_by_col, 'vertical_line', 'ncats', 'nsubcats', 'minval', 'maxval', 'reversed', 'logscale', 'logbase'))) %>%
        distinct()
    
    return(mock_data_meta)
}