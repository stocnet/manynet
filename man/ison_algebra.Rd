% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_ison.R
\docType{data}
\name{ison_algebra}
\alias{ison_algebra}
\title{Multiplex graph object of friends, social, and task ties (McFarland 2001)}
\format{
\if{html}{\out{<div class="sourceCode">}}\preformatted{#> -- # M182 Algebra Class --------------------------------------------------------
#> # A multiplex, weighted, directed network of 16 nodes and 279 social, tasks,
#> and friends arcs
#> 
#> -- Ties
#> # A tibble: 279 x 4
#>    from    to type   weight
#>   <int> <int> <chr>   <dbl>
#> 1     1     5 social   1.2 
#> 2     1     5 tasks    0.3 
#> 3     1     8 social   0.15
#> 4     1     9 social   2.85
#> 5     1     9 tasks    0.3 
#> 6     1    10 social   6.45
#> # i 273 more rows
#> 
}\if{html}{\out{</div>}}
}
\source{
See also \code{data(studentnets.M182, package = "NetData")}

Larger comprehensive data set publicly available, contact Daniel A. McFarland for details.
}
\usage{
data(ison_algebra)
}
\description{
Multiplex graph object of friends, social, and task ties
between 16 anonymous students in an honors algebra class (M182).
Each type of tie is weighted:
the \code{friends} ties are weighted
\code{2} = best friends, \code{1} = friend, and \code{0} is not a friend;
\code{social} consists of social interactions per hour;
and \code{tasks} consists of task interactions per hour.
}
\references{
McFarland, Daniel A. (2001) “Student Resistance.”
\emph{American Journal of Sociology} 107(3): 612-78.
\doi{10.1086/338779}.
}
\keyword{datasets}
