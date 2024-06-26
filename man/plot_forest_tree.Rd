% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_constructs.R
\name{plot_forest_tree}
\alias{plot_forest_tree}
\title{Plot Forest Tree}
\usage{
plot_forest_tree(
  box,
  x_lower,
  x_upper,
  x_dot,
  n1,
  n2 = 1,
  N2 = 1,
  col = "#663399",
  userect = FALSE,
  height = NULL,
  options = page_options$new()
)
}
\arguments{
\item{box}{The box parameter.}

\item{x_lower}{The lower x-coordinate.}

\item{x_upper}{The upper x-coordinate.}

\item{x_dot}{The x-dot parameter.}

\item{n1}{The n1 parameter.}

\item{n2}{The n2 parameter (default is 1).}

\item{N2}{The N2 parameter (default is 1).}

\item{col}{The color of the forest tree (default is '#663399').}

\item{userect}{The userect parameter (default is FALSE).}

\item{height}{The height of the rectangle, used when userect is TRUE. When NULL,
the height is set to 1/5 of the box height.}

\item{options}{The color palette to use (default is page_options$new()).}
}
\value{
None
}
\description{
This function plots a forest tree based on the given parameters.
}
\examples{
\dontrun{
# Create a box
box2 <- plot_box(0.5, 0.5, 1, 3, 0.02, 3, 5, 1, -2, 'value', FALSE, show_axis=TRUE)

# Generate random x coordinates
from_ <- min(box2$axis$from, box2$axis$to)
to_ <- max(box2$axis$from, box2$axis$to)
x_lower <- runif(3, from_, to_)
x_upper <- runif(3, x_lower, to_)

# Plot horizontal segments and dots
plot_horizontal_seg(box2, c(x_lower[1], x_upper[1]), 1, col='purple')
plot_horizontal_seg(box2, c(x_lower[2], x_upper[2]), 2, col='purple')
plot_horizontal_seg(box2, c(x_lower[3], x_upper[3]), 3, col='purple')
plot_dot(box2, runif(1, x_lower[1], x_upper[1]), 1, col='purple')
plot_dot(box2, runif(1, x_lower[2], x_upper[2]), 2, col='purple')
plot_dot(box2, runif(1, x_lower[3], x_upper[3]), 3, col='purple')
}

}
