% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/APIPackage.R
\name{APIpackage}
\alias{APIpackage}
\alias{get_mem}
\alias{data_fetch}
\title{API to get data from Swedish Parliment open Data site}
\usage{
get_mem(county)
}
\arguments{
\item{county}{county of the members}

\item{id}{member id}
}
\value{
this function will return the members of parliment. if specific country is given then it will return member only from this country otherwise all parliment member

It will return member appoinments data according to the county or member is or both

this function will return the gender presence in each county
}
\description{
API to get data from Swedish Parliment open Data site
}
\examples{
\dontrun{
data_fetch()
}

\dontrun{
get_mem("")
}

\dontrun{
get_memdata("","")
}

\dontrun{
get_genderdata("")
}

}
