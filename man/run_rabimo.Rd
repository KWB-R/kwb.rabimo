% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_rabimo.R
\name{run_rabimo}
\alias{run_rabimo}
\title{Run R-Abimo, the R-implementation of Water Balance Model Abimo}
\usage{
run_rabimo(data, config, controls = define_controls())
}
\arguments{
\item{data}{data frame similar to
\code{\link{rabimo_inputs_2025}$data}}

\item{config}{configuration object (list) similar to
\code{\link{rabimo_inputs_2025}$config}}

\item{controls}{list of settings that control how the function should behave.
Use \code{\link{define_controls}} to define such a list. The default is
the list returned by \code{define_controls()}.}
}
\value{
data frame with columns as returned by Abimo
}
\description{
Run R-Abimo, the R-implementation of Water Balance Model Abimo
}
\examples{
# Get input data and config for Berlin (version 2020)
inputs_2020 <- kwb.rabimo::rabimo_inputs_2020

# Randomly select 1000 blocks (to reduce runtime)
data <- inputs_2020$data
data <- data[sample(seq_len(nrow(data)), size = 1000L), ]

# Run R-Abimo
results_2020 <- kwb.rabimo::run_rabimo(data, inputs_2020$config)

# Get input data and config for Berlin (version 2025)
inputs_2025 <- kwb.rabimo::rabimo_inputs_2025

# Randomly select 1000 blocks (to reduce runtime)
data <- inputs_2025$data
data <- data[sample(seq_len(nrow(data)), size = 1000L), ]

# Run R-Abimo
results_2025 <- kwb.rabimo::run_rabimo(data, inputs_2025$config)
}
