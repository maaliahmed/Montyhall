% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/last_monty-hall-problem (2).R
\name{play_n_games}
\alias{play_n_games}
\title{run a simulation loop}
\usage{
play_n_games(n = 100)
}
\arguments{
\item{no}{argument}
}
\value{
The function will return the number of wins and losses for each strategy:
staying with the initial door selected or switching after a goat door is
opened.
}
\description{
play_n_games function will run a simulation for a specified number of loops.
}
\details{
This function will run a simulation to establish the best strategy for
winning. If ran 10,000 times, the simulated statistic will converge close
to the actual theoretical value.
}
\examples{
play_n_games


}
