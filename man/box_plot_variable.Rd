% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_helper.R
\name{box_plot_variable}
\alias{box_plot_variable}
\title{Box Plot Variable}
\usage{
box_plot_variable(data = NULL, y.var = NULL, x.var = NULL,
  coloring = NULL, y.name = "", x.name = "", group.label = NULL,
  legend.name = "", legend.label = NULL)
}
\arguments{
\item{data}{Data points to be plotted.}

\item{y.var}{The variable to be plotted along the y-axis.}

\item{x.var}{The variable to be plotted along the x-axis.}

\item{coloring}{The colors for subgroups.}

\item{y.name}{Name of the y variable in the plot.}

\item{x.name}{Name of the x variable in the plot.}

\item{legend.name}{Title of the legend.}

\item{legend.label}{New labels for the legend (optional).}
}
\value{
A table with session ids and durations.
}
\description{
This function provides a box plot of a variable.
}
\details{
It can be separated by scenes and controling factors.
}
\examples{
#Box plot without variable subgroupings
box_plot_variable(test,
                  y.var = "path_length",
                  x.var = "scene_name",
                  y.name = "Path Length",
                  x.name = "Scene",
                  group.label = c("Training","City"))
#Box plot with variable subgroupings
box_plot_variable(test,
                  y.var = "path_length",
                  x.var = "scene_name",
                  coloring = "sex",
                  y.name = "Path Length",
                  x.name = "Scene",
                  group.label = c("Training","City"),
                  legend.name = "Sex",
                  legend.label = c("TRUE"="Female", "FALSE"="Male"))

}
\keyword{EVE,}
\keyword{evaluation,}
\keyword{path}
