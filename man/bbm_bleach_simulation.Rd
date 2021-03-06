% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bleach.R
\name{bbm_bleach_simulation}
\alias{bbm_bleach_simulation}
\title{Bleach a simulation.}
\usage{
bbm_bleach_simulation(sim, rate)
}
\arguments{
\item{sim}{An object of class \link[=bbm_sim_class]{bbm_sim}: the output of a
\code{\link[=bbm_simulate]{bbm_simulate()}}.}

\item{rate}{The rate at which the bleaching should occur. Each particle has
probability \code{rate} of being removed for the next frame. I.e. \code{rate} must be
between 0 aand 1 and a low \code{rate} means not much bleaching, whereas a high
\code{rate} means a lot of bleaching.}
}
\value{
An object of class \link[=bbm_sim_class]{bbm_sim}.
}
\description{
Remove particles from the simulation at a specified rate. If a particle is
removed at a given time-point, it never returns.
}
