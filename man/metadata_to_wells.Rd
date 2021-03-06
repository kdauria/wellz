% Generated by roxygen2 (4.0.2): do not edit by hand
\name{metadata_to_wells}
\alias{metadata_to_wells}
\title{Convert metadata data.frame to wellList}
\usage{
metadata_to_wells(meta.df)
}
\arguments{
\item{meta.df}{the metadata in a \code{data.frame} (see \code{parse_metadata})}
}
\description{
This is the large wrapper function that does most of the
grunt work for converting metadata to \code{Solution},
\code{action}, \code{actionList}, \code{well}, and \code{wellList}
wellList. This should eventually be refactored so that there
are constructors for each of these objects.
}

