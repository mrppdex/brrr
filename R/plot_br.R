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
#' @param box_group An optional header object specifying the `plot_br` object to group with.
#' @param options_br An optional page_options object specifying the options for the plot.
#'
#' @return A list containing the boxes in the plot, used options and the last y-position of the plot.
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
                    vline_col=NULL, # e.g. Non-Inferiority Margin
                    neutral_pos = 3, num_ticks = 6,
                    top_margin = NULL, userect = FALSE,
                    arrow_labels = c('Favors\nTreatment', 'Favors\nPlacebo'),
                    value_collapse=rep(FALSE, length(columns_specs)),
                    label_text_size = NULL,
                    header_text_size = NULL,
                    box_group=NULL, colors_by=NULL,
                    options_br = page_options$new()) {
  
  
  if (is.null(colors_by))  {
    colors_by <- 'tmpcolorsby'
    data <- data %>% mutate(!!colors_by:='x')
  }
  
  get_color <- function(group, shift=0) {
    which((data %>% select(all_of(!!colors_by)) %>% distinct() %>% pull(!!colors_by)) == as.character(group))+shift
  }

  # make sure lengths are ok
  stopifnot(length(breaks_widths)==length(columns_specs))

  # new page
  if (is.null(top_margin) & is.null(box_group)) grid.newpage()

  # 1. CREATE HEADER
  if (!is.null(top_margin)) {
    options_br$set_page_parameter('PAGE_TOP_MARGIN', top_margin)
  } else if (!is.null(box_group) ) {
    if (box_group$name!='box_group') {
      stop('box_group must be a header object')
    }
    options_br <- box_group$options
    new_top_margin <- 1 - box_group$last_y + options_br$get_box_spacing()
    options_br$set_page_parameter('PAGE_TOP_MARGIN', new_top_margin)
  }

  header_br <- create_header(breaks_widths, names(columns_specs), header_text_size, options=options_br)

  # 2. ADD BOXES
  
  data_meta <- get_metadata(data, split_axis_by_col, axis_labels_col, split_box_by_col, vline_col)

  # remember last added graph part
  last_graph_part <- header_br
  boxes <- list()

  
  data_meta <- data_meta %>% arrange(across(any_of(split_axis_by_col)))

  for (est in unique(data_meta[[split_axis_by_col]])) {
    data_meta_subset <- data_meta %>% 
      filter(if_all(split_axis_by_col, ~ .x==est))
    
    minval <- min(data_meta_subset$minval)
    maxval <- max(data_meta_subset$maxval)
    
    expand_factor <- 0.01*abs(maxval-minval)

    
    minval <- ifelse('logscale' %in% colnames(data_meta_subset), minval*0.99, minval - sign(minval)*expand_factor)
    maxval <- ifelse('logscale' %in% colnames(data_meta_subset), maxval*1.01, maxval + sign(maxval)*expand_factor)

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
    vline_val <- NULL 
    if(!is.null(vline_col)) {
      
      vline_val <- max(data_meta_subset$vertical_line)
      vline_val <- ifelse(is.infinite(vline_val), NULL, vline_val)
    } 

    last_graph_part <- add_box( last_graph_part, spacing, ncats, 
                                options_br$box.category.height, 
                                neutral_pos, num_ticks, 
                                ifelse(is_reversed, maxval, minval), 
                                ifelse(is_reversed, minval, maxval), label=axis_label, 
                                logscale=(!is.na(logscale) & logscale),
                                b=ifelse(is.na(logbase), 2, logbase), 
                                arrow_labels = arrow_labels,
                                show_axis=TRUE, vline=vline_val, colbreaks=breaks_widths)

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
        
        text_size_ <- options_br$label.font.size
        if (!is.null(label_text_size) & !is.null(label_text_size[cn_idx])) {
          text_size_ <- text_size_*label_text_size[cn_idx]
        }
        
        for (j in 1:max(length(unique_column_names), 1)) {
          
          header_options_ <- box$box$header$options
          col_ <- ifelse(!is.null(colors_by), 
                         header_options_$get_palette()[get_color(data_sub_subset[j, colors_by])], 
                         header_options_$get_palette()[j])
          
          
          add_label(unique_column_names[j], cn_idx, ben_idx, n=j, N=length(unique_column_names), 
                    col=ifelse(cn_idx==1 | !header_options_$label.font.usecolors, 'black', col_), fontsize=text_size_)
        }
        
      }

      # plot the forest plot
      for(k in 1:nrow(data_sub_subset)) {
        
        header_options_ <- box$box$header$options
        
        col_ <- ifelse(!is.null(colors_by), header_options_$get_palette()[get_color(data_sub_subset[k, colors_by])], 'black')
        
        pch_idx_ <- ifelse(!is.null(colors_by), get_color(data_sub_subset[k, colors_by]), NULL) + options_br$forest.pch.shift
        
        plot_forest_tree(box$box$box, data_sub_subset[k, 'lower'], data_sub_subset[k, 'upper'], 
                        data_sub_subset[k, 'value'], ben_idx, k, nrow(data_sub_subset), 
                        userect=userect, col=col_, pch=pch_idx_, options=header_options_)
      }
    }  

  }

  return(list(name='box_group', boxes = boxes, options=options_br, last_y = min(last_graph_part$y_pos)))
}


