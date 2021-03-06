% Generated by roxygen2 (4.0.2): do not edit by hand
\name{ActionList}
\alias{ActionList}
\title{Construct an actionList object}
\usage{
ActionList(actions = NULL, ID = NULL, rmVol = NULL, i = NULL,
  solutions = NULL)
}
\description{
An actionList is simply a list with \code{names} equal
to the ID of each \code{action} object.
}
\details{
The most straightforward way to construct an actionList is
to supply a list of action objects to the \code{actions} argument.
Alternatively, several vectors can be supplied to describe multiple
actions. The \code{ID}, \code{rmVol}, and \code{i} arguments
must be vectors that are the same length \emph{n} that describe
\emph{n} actions. One or more of them can be \code{NULL}, but then
an empty action will be created for the actionList. A list
of \emph{n} \code{Solution} objects can aso be supplied to the
\code{solutions} argument. If no Solution object are provided,
empty object are constructed and added to the action objects.

If no ID's are provided, the the actions are given numbers
as their ID's (numbers change with \code{as.character}). If any
of the actions have NA as their ID, these actions are named
similarly.
}
\examples{
# Empty actionList
ActionList()

# Making an action list from actions
actions = list(Action(ID="a"), Action())
ActionList(actions)

# Making an action list with other arguments
ActionList(ID="a")
ActionList(i=33)
ActionList(ID="a",i=1,rmVol=30,solutions=list(Solution(volume=90)))
}

