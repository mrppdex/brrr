#' Get metadata for a given dataset
#'
#' This function calculates metadata for a given dataset, including the number of unique categories,
#' minimum and maximum values, whether the data is reversed, and the number of subcategories.
#'
#' @param data The dataset to calculate metadata for.
#' @param split_axis_by_col The column to split the data by on the x-axis.
#' @param axis_labels_col The column containing the axis labels.
#' @param split_box_by_col The column to split the data by in each box.
#'
#' @return A data frame containing the calculated metadata.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   x = c(1, 2, 3, 1, 2, 3),
#'   y = c(4, 5, 6, 4, 5, 6),
#'   category = c("A", "A", "B", "B", "C", "C")
#' )
#' get_metadata(data, x, category)
#' }
#'
#' @export
get_metadata <- function(data, split_axis_by_col, axis_labels_col, split_box_by_col) {

    if (!'reversed' %in% colnames(data)) {
        data$reversed <- FALSE
    }
    
    mock_data_meta <- data %>%
        group_by_at({{split_axis_by_col}}) %>%
        mutate(across({{split_box_by_col}}, ~ length(unique(.)), .names="ncats")) %>%
        mutate(minval = min(value, lower, upper), maxval = max(value, lower, upper)) %>%
        mutate(reversed = any(reversed)) %>%
        group_by_at(c({{split_box_by_col}}, {{split_axis_by_col}})) %>%
        mutate(nsubcats = n()) %>%
        ungroup() %>%
        select(any_of(c(split_axis_by_col, axis_labels_col, split_box_by_col, 'ncats', 'nsubcats', 'minval', 'maxval', 'reversed', 'logscale', 'logbase'))) %>%
        distinct()
    
    return(mock_data_meta)
}