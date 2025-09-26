
#' Add a legend to a plot
#'
#' This function adds a legend to a plot based on the provided legend items.
#'
#' @param legend_items A list of legend items, where each item is a list with the following properties:
#'   \describe{
#'     \item{label}{The label for the legend item}
#'     \item{type}{The type of the legend item (e.g., "line")}
#'     \item{pch}{The point character for the legend item}
#'     \item{col}{The color of the legend item}
#'     \item{lty}{The line type of the legend item}
#'     \item{lwd}{The line width of the legend item}
#'   }
#' @param xpos The x position of the legend box
#' @param ypos The y position of the legend box
#' @param width The width of the legend box
#' @param height The height of the legend box
#' @param n_rows The number of rows in the legend box (optional)
#' @param n_cols The number of columns in the legend box (optional)
#' @param byrow A logical value indicating whether the legend items should be arranged by row (TRUE) or by column (FALSE)
#' @param label The label for the legend box (optional). Default is 'Legend'. When provided, the label is displayed at the top of the legend box.
#' @param options An object of class "page_options" containing additional options for the legend
#' @return None
#' @export
add_legend <- function(legend_items, xpos, ypos, width, height,
                       n_rows=NULL, n_cols=NULL, byrow=TRUE, label='Legend',
                       options=page_options$new()) 
{
    # convert xpos, ypos, and width to npc
    # xpos <- convertX(xpos, unitTo='npc', valueOnly = TRUE)
    # ypos <- convertY(ypos, unitTo='npc', valueOnly = TRUE)
    # legend_width <- convertX(width, unitTo='npc', valueOnly = TRUE)

    if(xor(is.null(n_rows), is.null(n_cols))) {
        if(is.null(n_rows)) {
            n_rows <- ceiling(length(legend_items) / n_cols)
        } else {
            n_cols <- ceiling(length(legend_items) / n_rows)
        }
    } else if(is.null(n_rows) & is.null(n_cols)) {
        if(!byrow) {
            n_rows <- length(legend_items)
            n_cols <- 1
        } else {
            n_rows <- 1
            n_cols <- length(legend_items)
        }
    }

    legend_width <- width / n_cols
    legend_height <- height / n_rows
    
    if(!is.null(label)) {
        legend_label_font_size <- options$get_option('legend.header.font.size')
        legend_label_font_size <- unit(legend_label_font_size, 'points')

        # convert legend_label_font_size to npc
        legend_label_font_size_npc <- convertHeight(legend_label_font_size, 'npc')

        # draw a box with the label
        grid.rect(x = convertX(xpos, 'npc'), y = ypos, width = width, height = legend_label_font_size_npc*1.5, 
              just = c('left', 'top'),
              gp = gpar(fill = "transparent", lty = 1, lwd = 1))

        # draw the label
        grid.text(label, x = xpos+convertX(unit(1.5, 'mm'), 'npc'), y = ypos - legend_label_font_size_npc*0.75, 
                  just = c('left', 'center'), 
                  gp = gpar(fontsize = legend_label_font_size, 
                            fontface='bold', col = 'black')) # col = options$br_palette[1]))

        # shift the box down by the label height
        ypos <- ypos - legend_label_font_size_npc*1.5
        ypos <- unit(ypos, 'npc')
    }

    # draw box 
    grid.rect(x = xpos, y = ypos, width = width, height = height, 
              just = c('left', 'top'),
              gp = gpar(fill = options$get_option('legend.fill.color'), lty = 1, lwd = 1))

    for (item_idx in seq_along(legend_items)) {
        col_num <- NULL
        row_num <- NULL

        if(byrow) {
            col_num <- (item_idx - 1) %% n_cols
            row_num <- (item_idx - 1) %/% n_cols
        } else {
            col_num <- (item_idx - 1) %/% n_rows
            row_num <- (item_idx - 1) %% n_rows
        }

        legend_item <- legend_items[[item_idx]]
        legend_label <- legend_item$label
        legend_type <- legend_item$type
        legend_point <- legend_item$pch
        legend_lwd <- legend_item$lwd
        legend_lty <- legend_item$lty
        legend_color <- legend_item$col
        legend_text_color <- legend_item$col_txt

        get_default <- function(x, default) {
            if(is.null(x)) {
                return(default)
            } else {
                return(x)
            }
        }

        legend_lwd <- get_default(legend_lwd, options$get_option('forest.line.width'))
        legend_lty <- get_default(legend_lty, options$get_option('forest.line.type'))

        get_default_color <- function(x) {
            if(is.null(x)) {
                return(options$get_option('br_palette')[1])
            } else {
                return(options$get_option('br_palette')[x])
            }
        }

        legend_color <- get_default_color(legend_color)
        legend_text_color <- ifelse(options$get_option('label.font.usecolors'), get_default_color(legend_text_color), 'black')

        max_width <- convertWidth(unit(20, 'mm'), 'npc', valueOnly=TRUE)
        max_width <- unit(min(max_width, 0.25*legend_width), 'npc')

        if(!is.null(legend_type)) {
            if(legend_type == 'line') {
                grid.lines(x = c(xpos + col_num * legend_width + legend_width*0.05, 
                                 xpos + col_num * legend_width + max_width),
                           y = rep(ypos - row_num * legend_height - legend_height / 2, 2),
                           gp = gpar(col = legend_color, lty = legend_lty, lwd = legend_lwd))
            } else if (legend_type == 'rectangle') {
                grid.rect(x = xpos + col_num * legend_width + legend_width*0.05, 
                          y = ypos - row_num * legend_height - legend_height / 2, 
                          width = max_width - legend_width*0.05, height = 0.015, 
                          just = c('left', 'center'),
                          gp = gpar(col = legend_color, lty = legend_lty, lwd = legend_lwd))
            }
        }

        if(!is.null(legend_point)) {
            legend_point <- 21 + (legend_point-1) %% 6
            grid.points(x = xpos + col_num * legend_width + (legend_width*0.05+max_width) / 2, 
                        y = ypos - row_num * legend_height - legend_height / 2, 
                        pch = legend_point, 
                        gp = gpar(col = legend_color, fill = legend_color))
        }

        if(!is.null(legend_label)) {
            txt_pos <- ifelse(!is.null(legend_type) & legend_type=='text', 0.05, 1.2)
            grid.text(legend_label, x = xpos + col_num * legend_width + max_width*txt_pos, 
                      y = ypos - row_num * legend_height - legend_height / 2, just = 'left', 
                      gp = gpar(fontsize = options$get_option('legend.label.font.size'), 
                                col=legend_text_color))
        }
    }

}


