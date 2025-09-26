#' Page Options Class
#'
#' @description
#' This R6 class defines and manages options for page layout and plotting.
#' It provides a centralized way to control various aspects of the plot,
#' such as margins, fonts, colors, and other graphical parameters.
#'
#' @details
#' The class is initialized with a set of default values for all options.
#' These options can be modified using the `set_option` method and
#' retrieved using the `get_option` method.
#'
#' @field PAGE_TOP_MARGIN Top margin of the page.
#' @field PAGE_BOTTOM_MARGIN Bottom margin of the page.
#' @field PAGE_LEFT_MARGIN Left margin of the page.
#' @field PAGE_RIGHT_MARGIN Right margin of the page.
#' @field HEADER_HEIGHT Height of the header.
#' @field HEADER_WIDTH Width of the header.
#' @field br_palette Color palette for the plot.
#' @field row.label.font.size Font size for row labels.
#' @field label.use.separation.line Logical, whether to use a separation line for labels.
#' @field label.font.size Font size for labels.
#' @field label.font.usecolors Logical, whether to use colors for label fonts.
#' @field header.label.font.size Font size for header labels.
#' @field header.label.color Color for header labels.
#' @field axis.label.font.size Font size for axis labels.
#' @field axis.ticks.font.size Font size for axis ticks.
#' @field axis.ticks.font.rotation Rotation for axis ticks.
#' @field axis.ticks.label.nice Logical, whether to use nice labels for axis ticks.
#' @field box.spacing Spacing between boxes.
#' @field box.category.height Height of a single category in a box.
#' @field box.fill.color Fill color for boxes.
#' @field legend.label.font.size Font size for legend labels.
#' @field legend.header.font.size Font size for legend header.
#' @field legend.fill.color Fill color for the legend.
#' @field forest.line.type Line type for the forest plot.
#' @field forest.line.width Line width for the forest plot.
#' @field forest.pch.shift Shift for the pch in the forest plot.
#' @field plot.width Width of the plot in inches.
#' @field plot.height Height of the plot in inches.
#'
#' @import R6
#' @import grid
#' @export
page_options <- R6Class("page_options",
    public = list(
        PAGE_TOP_MARGIN = 0.05,
        PAGE_BOTTOM_MARGIN = 0.05,
        PAGE_LEFT_MARGIN = 0.05,
        PAGE_RIGHT_MARGIN = 0.05,
        HEADER_HEIGHT = 0.05,
        HEADER_WIDTH = 0.9,
        br_palette = c("black", "rebeccapurple" = "#663399", "cornflowerblue" = "#6495ED",
                       "mediumseagreen" = "#3CB371", "tomato" = "#FF6347",
                       "peachpuff" = "#FFDAB9", "lightsalmon" = "#FFA07A"),
        row.label.font.size = convertUnit(unit(3, 'mm'), 'points', valueOnly=TRUE),
        label.use.separation.line = FALSE,
        label.font.size = convertUnit(unit(3, 'mm'), 'points', valueOnly=TRUE),
        label.font.usecolors = TRUE,
        header.label.font.size = convertUnit(unit(3, 'mm'), 'points', valueOnly=TRUE),
        header.label.color = "#043099",
        axis.label.font.size = convertUnit(unit(4, 'mm'), 'points', valueOnly=TRUE),
        axis.ticks.font.size = convertUnit(unit(3, 'mm'), 'points', valueOnly=TRUE),
        axis.ticks.font.rotation = 0,
        axis.ticks.label.nice = TRUE,
        box.spacing = convertUnit(unit(20, 'mm'), 'npc', valueOnly=TRUE),
        box.category.height = unit(15, 'mm'),
        box.fill.color = "#ded8db43",
        legend.label.font.size = convertUnit(unit(3, 'mm'), 'points', valueOnly=TRUE),
        legend.header.font.size = convertUnit(unit(3, 'mm'), 'points', valueOnly=TRUE),
        legend.fill.color = "#fffdd0",
        forest.line.type = 1,
        forest.line.width = 3,
        forest.pch.shift = 0,
        plot.width = 8.5,
        plot.height = 11,

        #' @description
        #' Initialize the page options.
        #' @param ... A list of options to override the default values.
        initialize = function(...) {
            args <- list(...)
            for (name in names(args)) {
                self$set_option(name, args[[name]])
            }
            self$HEADER_WIDTH <- 1 - self$PAGE_LEFT_MARGIN - self$PAGE_RIGHT_MARGIN
        },

        #' @description
        #' Get an option value.
        #' @param name The name of the option to get.
        #' @return The value of the option.
        get_option = function(name) {
            return(self[[name]])
        },

        #' @description
        #' Set an option value.
        #' @param name The name of the option to set.
        #' @param value The new value for the option.
        set_option = function(name, value) {
            if (name %in% names(self)) {
                self[[name]] <- value
            } else {
                stop(paste0("Option '", name, "' is not defined."))
            }
        }
    )
)
