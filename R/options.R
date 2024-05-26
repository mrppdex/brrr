#' Define a class for page options
#'
#' This class represents the options for a page layout.
#' It includes properties for the page margins, header height, and color palette.
#' 
#' @field PAGE_TOP_MARGIN The top margin of the page.
#' @field PAGE_BOTTOM_MARGIN The bottom margin of the page.
#' @field PAGE_LEFT_MARGIN The left margin of the page.
#' @field PAGE_RIGHT_MARGIN The right margin of the page.
#' @field HEADER_HEIGHT The height of the header.
#' @field HEADER_WIDTH The width of the header.
#' @field row.label.font.size The font size of the row labels.
#' @field br_palette The color palette for the page.
#' 
#' @format An object of class \code{page_options}
#' @import R6
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
                            row.label.font.size = 10,
                            #' @description Initialize the page options.
                            #' @param PAGE_TOP_MARGIN The top margin of the page.
                            #' @param PAGE_BOTTOM_MARGIN The bottom margin of the page.
                            #' @param PAGE_LEFT_MARGIN The left margin of the page.
                            #' @param PAGE_RIGHT_MARGIN The right margin of the page.
                            #' @param HEADER_HEIGHT The height of the header.
                            initialize = function(PAGE_TOP_MARGIN = 0.05,
                                                  PAGE_BOTTOM_MARGIN = 0.05,
                                                  PAGE_LEFT_MARGIN = 0.05,
                                                  PAGE_RIGHT_MARGIN = 0.05,
                                                  HEADER_HEIGHT = 0.05) {
                                self$PAGE_TOP_MARGIN <- PAGE_TOP_MARGIN
                                self$PAGE_BOTTOM_MARGIN <- PAGE_BOTTOM_MARGIN
                                self$PAGE_LEFT_MARGIN <- PAGE_LEFT_MARGIN
                                self$PAGE_RIGHT_MARGIN <- PAGE_RIGHT_MARGIN
                                self$HEADER_HEIGHT <- HEADER_HEIGHT
                                self$HEADER_WIDTH <- 1 - PAGE_LEFT_MARGIN - PAGE_RIGHT_MARGIN
                            },
                            #' @description Get the color palette for the page.
                            #' @return The color palette for the page.
                            get_palette = function() {
                                return(self$br_palette)
                            },
                            #' @description Set the color palette for the page.
                            #' @param palette The color palette to set.
                            set_palette = function(palette) {
                                self$br_palette <- palette
                            },
                            #' @description Get the page parameters.
                            #' @return A list of the page parameters.
                            get_page_parameters = function() {
                                return(list(PAGE_TOP_MARGIN = self$PAGE_TOP_MARGIN,
                                             PAGE_BOTTOM_MARGIN = self$PAGE_BOTTOM_MARGIN,
                                             PAGE_LEFT_MARGIN = self$PAGE_LEFT_MARGIN,
                                             PAGE_RIGHT_MARGIN = self$PAGE_RIGHT_MARGIN,
                                             HEADER_HEIGHT = self$HEADER_HEIGHT,
                                             HEADER_WIDTH = self$HEADER_WIDTH))
                            },
                            #' @description Get a specific page parameter.
                            #' @param parameter The parameter to get.
                            #' @return The value of the parameter.
                            get_page_parameter = function(parameter) {
                                return(self$get_page_parameters()[[parameter]])
                            },
                            #' @description Set a specific page parameter.
                            #' @param parameter The parameter to set.
                            #' @param value The value to set.
                            set_page_parameter = function(parameter, value) {
                                if (parameter %in% names(self$get_page_parameters())) {
                                    self[[parameter]] <- value
                                } else {
                                    stop(paste0('Parameter ', parameter, ' is not defined.'))
                                }
                            },
                            #' @description Get the font size of the row labels.
                            #' @return The font size of the row labels.
                            get_label_font_size = function() {
                                return(self$row.label.font.size)
                            },
                            #' @description Set the font size of the row labels.
                            #' @param size The font size to set.
                            #' @return None
                            set_label_font_size = function(size) {
                                self$row.label.font.size <- size
                            }
                        )
)

# test the class
# test <- page_options$new()
# test$get_page_parameters()
# test$get_page_parameter('PAGE_TOP_MARGIN')
# test$set_page_parameter('PAGE_TOP_MARGIN', 0.1)
# test$get_page_parameter('PAGE_TOP_MARGIN')
