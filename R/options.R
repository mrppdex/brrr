library(R6)

page_options <- R6Class("page_options",
                        public = list(
                            PAGE_TOP_MARGIN = 0.05,
                            PAGE_BOTTOM_MARGIN = 0.05,
                            PAGE_LEFT_MARGIN = 0.05,
                            PAGE_RIGHT_MARGIN = 0.05,
                            HEADER_HEIGHT = 0.05,
                            HEADER_WIDTH = 1 - PAGE_LEFT_MARGIN - PAGE_RIGHT_MARGIN,
                            br_palette = c("black", "rebeccapurple" = "#663399", "cornflowerblue" = "#6495ED", 
                                           "mediumseagreen" = "#3CB371", "tomato" = "#FF6347", 
                                           "peachpuff" = "#FFDAB9", "lightsalmon" = "#FFA07A"),
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
                            get_palette = function() {
                                return(self$br_palette)
                            },
                            set_palette = function(palette) {
                                self$br_palette <- palette
                            },
                            get_page_parameters = function() {
                                return(list(PAGE_TOP_MARGIN = self$PAGE_TOP_MARGIN,
                                             PAGE_BOTTOM_MARGIN = self$PAGE_BOTTOM_MARGIN,
                                             PAGE_LEFT_MARGIN = self$PAGE_LEFT_MARGIN,
                                             PAGE_RIGHT_MARGIN = self$PAGE_RIGHT_MARGIN,
                                             HEADER_HEIGHT = self$HEADER_HEIGHT,
                                             HEADER_WIDTH = self$HEADER_WIDTH))
                            },
                            get_page_parameter = function(parameter) {
                                return(self$get_page_parameters()[[parameter]])
                            },
                            set_page_parameter = function(parameter, value) {
                                if (parameter %in% names(self$get_page_parameters())) {
                                    self[[parameter]] <- value
                                } else {
                                    stop(paste0('Parameter ', parameter, ' is not defined.'))
                                }
                            }
                        )
)

# test the class
# test <- page_options$new()
# test$get_page_parameters()
# test$get_page_parameter('PAGE_TOP_MARGIN')
# test$set_page_parameter('PAGE_TOP_MARGIN', 0.1)
# test$get_page_parameter('PAGE_TOP_MARGIN')
