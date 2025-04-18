% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_ison.R
\docType{data}
\name{ison_brandes}
\alias{ison_brandes}
\title{One-mode and two-mode centrality demonstration networks}
\format{
\if{html}{\out{<div class="sourceCode">}}\preformatted{#> # A undirected network of 11 nodes and 12 ties
#> 
#> -- Nodes
#> # A tibble: 11 x 1
#>   twomode_type
#>   <lgl>       
#> 1 FALSE       
#> 2 FALSE       
#> 3 TRUE        
#> 4 FALSE       
#> 5 TRUE        
#> 6 TRUE        
#> # i 5 more rows
#> 
#> -- Ties
#> # A tibble: 12 x 2
#>    from    to
#>   <int> <int>
#> 1     1     3
#> 2     2     3
#> 3     3     4
#> 4     4     5
#> 5     4     6
#> 6     5     7
#> # i 6 more rows
#> 
}\if{html}{\out{</div>}}
}
\usage{
data(ison_brandes)
}
\description{
This network should solely be used
for demonstration purposes as it does not describe a real network.
To convert into the two-mode version,
assign \code{ison_brandes \%>\% rename(type = twomode_type)}.
}
\keyword{datasets}
