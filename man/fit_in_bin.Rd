\name{fit_in_bin}
\alias{fit_in_bin}
\title{fit_in_bin is used when predicting on a new data set when the trained model contains binned version of variables.}
\usage{
  fit_in_bin(var, bin_labels, input_df, output_df,
    scale = 1, quants = NULL)
}
\arguments{
  \item{var}{the name (as a string) of the continuous
  variable you want to bin}

  \item{bin_labels}{a character vector of the binned labels
  (e.g. bin_labels =
  levels(my_training_data$my_binned_predictor)}

  \item{input_df}{the data set used to create the original
  binned version of the variable (var must be in this
  data)}

  \item{output_df}{the data set containing var you want to
  find the bin for.}

  \item{scale}{if there was a scale argument used in
  creating the bin.}

  \item{quants}{sometimes when binning a variable you
  specify a quants argument but because of the data you may
  end up with less that quants bins, use this agument for
  when this happens.}
}
\description{
  I.E. use when you want to predict on a new data set and
  the variables need to be binned the same way they were to
  create the model.
}

