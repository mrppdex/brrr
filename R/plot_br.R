#' Plot BR
#'
#' This function plots BR (Bayesian Regression) data using a forest plot. It takes in various parameters to customize the plot.
#'
#' @param data The data frame containing the BR data.
#' @param columns_specs A character named vector specifying the column names in the data frame that correspond to the different columns in the plot.
#' @param breaks_widths A numeric vector specifying the widths of the breaks between the columns in the plot.
#' @param split_axis_by_col A character string specifying the column name in the data frame to split the plot axis by.
#' @param axis_labels_col A character string specifying the column name in the data frame to use as the axis labels.
#' @param split_box_by_col A character string specifying the column name in the data frame to split the boxes within each axis by.
#' @param neutral_pos A numeric value specifying the neutral position of the plot.
#' @param num_ticks An optional numeric value specifying the number of ticks on the plot axis.
#' @param top_margin An optional numeric value specifying the top margin of the plot.
#' @param userect A logical value specifying whether to use rectangles in the plot.
#' @param arrow_labels A character vector specifying the labels for the arrows in the plot.
#' @param value_collapse A logical vector specifying whether to collapse the values within each column/row.
#' @param options_br An optional page_options object specifying the options for the plot.
#'
#' @return A list containing the boxes in the plot and the last y-position of the plot.
#'
#' @examples
#' \dontrun{
#' #create header
#' breaks_widths <- c(0.2, -0.1, 0.1, 0.2)
#' columns_specs <- c('Benefits'='endpoint', 
#'               'Treatment\n(N=100)'='treatment', 
#'               'Placebo\n(N=100)'='placebo',
#'               'Comparison\nHR or Odds Ratio\n(95% CI)'='col3')
#' value_collapse <- c(TRUE, FALSE, FALSE, FALSE)
#' 
#' split_axis_by_col <- 'estimator'
#' split_box_by_col <- 'endpoint'
#' 
#' # load data and add column
#' data(mock_data)
#' part1_data <- plot_br(mock_data, columns_specs, breaks_widths, 
#' split_axis_by_col, split_box_by_col, top_margin=NULL, value_collapse=value_collapse)
#' }
#' 
#' @export
plot_br <- function(data, columns_specs, breaks_widths, 
                    split_axis_by_col, axis_labels_col, split_box_by_col, # data splitting
                    neutral_pos = 3, num_ticks = 6,
                    top_margin = NULL, userect = FALSE,
                    arrow_labels = c('Favors\nTreatment', 'Favors\nPlacebo'),
                    value_collapse=rep(FALSE, length(columns_specs)),
                    options_br = page_options$new()) {

  # make sure lengths are ok
  stopifnot(length(breaks_widths)==length(columns_specs))

  # new page
  if (is.null(top_margin)) grid.newpage()

  # 1. CREATE HEADER
  if (!is.null(top_margin)) {
    options_br$set_page_parameter('PAGE_TOP_MARGIN', top_margin)
  }
  header_br <- create_header(breaks_widths, names(columns_specs), options=options_br)

  # 2. ADD BOXES
  data_meta <- get_metadata(data, split_axis_by_col, axis_labels_col, split_box_by_col)

  # remember last added graph part
  last_graph_part <- header_br
  boxes <- list()

  for (est in unique(data_meta[[split_axis_by_col]])) {
    data_meta_subset <- data_meta %>% 
      filter(if_all(split_axis_by_col, ~ .x==est))

    minval <- min(data_meta_subset$minval)
    maxval <- max(data_meta_subset$maxval)

    ncats <- unique(data_meta_subset$ncats)

    logscale <- ifelse('logscale' %in% colnames(data_meta_subset),
                       any(data_meta_subset$logscale, na.rm = TRUE), FALSE)

    logbase  <- ifelse('logbase' %in% colnames(data_meta_subset),
                       unique(data_meta_subset$logbase, na.rm = TRUE)[1],
                       2)

    spacing <- ifelse(last_graph_part$name=='header', 0, 
                      options_br$get_box_spacing())

    is_reversed <- ifelse('reversed' %in% colnames(data_meta_subset),
                          any(data_meta_subset$reversed), FALSE)

    axis_label <- data_meta_subset[[axis_labels_col]][1]

    last_graph_part <- add_box( last_graph_part, spacing, ncats, 
                                unit(10, 'mm'), neutral_pos, num_ticks, 
                                ifelse(is_reversed, maxval, minval), 
                                ifelse(is_reversed, minval, maxval), label=axis_label, 
                                logscale=(!is.na(logscale) & logscale), 
                                b=ifelse(is.na(logbase), 2, logbase), 
                                arrow_labels = arrow_labels,
                                show_axis=TRUE)

    boxes[[est]] <- list(box=last_graph_part)
  }

  for (est in unique(data_meta[[split_axis_by_col]])) {
    column_names   <- names(columns_specs)
    
    data_subset <- data %>% 
      filter(if_all(split_axis_by_col, ~ .x==est))

    box <- boxes[[est]]
    add_label <- box$box$label_fun
    ncats <- box$ncats

    unique_endpoints <- unique(data_subset[[split_box_by_col]])

    # plot data
    for (ben_idx in seq_along(unique_endpoints)) {

      ben <- unique_endpoints[ben_idx]
      data_sub_subset <- data_subset %>% 
          filter(if_all(split_box_by_col, ~ .x==!!ben)) 

      # add labels to each box
      for (cn_idx in seq_along(column_names)) {
        column_name <- columns_specs[cn_idx]
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
                        userect=userect, col=NULL, br_palette=box$box$header$options$get_palette())
      }
    }  

  }

  return(list(boxes = boxes, last_y = min(last_graph_part$y_pos)))
}


