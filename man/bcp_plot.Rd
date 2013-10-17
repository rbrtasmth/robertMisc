\name{bcp_plot}
\alias{bcp_plot}
\title{Bayesian Change Point Plot}
\usage{
  bcp_plot(y_name, x_name = NULL, data_arg,
    threshold = 0.9, plot_breaks = "10 weeks",
    plot_title = paste0("Average Weekly ", ylabel, "\nWith Bayesian Change Point Trends"),
    ...)
}
\arguments{
  \item{y_name}{is the variable you're predicting.}

  \item{x_name}{(optional) is for establishing order (e.g.
  a date)}

  \item{data_arg}{is the data.frame which contains columns
  that can be pointed to by y_name and x_name.}

  \item{threshold}{is the probability of change needed to
  determine if a significant enough change has occured to
  create a new cluster}

  \item{plot_breaks}{can be changed when x_name refers to a
  date variable and you want the tick marks / scale of the
  plot to change.}

  \item{plot_title}{= plot title (sensible default
  provided).}

  \item{...}{additional arguments passed to bcp.}
}
\description{
  Runs the BCP model, then simplifies the results in a
  ggplot object.
}

