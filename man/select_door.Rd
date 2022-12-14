% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{select_door}
\alias{select_door}
\title{The contestant selects a door offered by the host.}
\usage{
select_door()
}
\arguments{
\item{no}{params}
}
\value{
Returns a numeric value between 1 and 3 that correlates with door choice and
the number chosen will be matched to a door from create_game()
}
\description{
\code{select_door()} allows the contestant to select a random door between
door 1, door 2, and door 3.
}
\details{
This function allows the contestant to select a door that will be used
in the next step when the host will open the door selected by the contestant.
}
\examples{
select_door() = 1
}
