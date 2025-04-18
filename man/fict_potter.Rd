% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_ison.R
\docType{data}
\name{fict_potter}
\alias{fict_potter}
\title{Six complex one-mode support data in Harry Potter books (Bossaert and Meidert 2013)}
\format{
\if{html}{\out{<div class="sourceCode">}}\preformatted{#> -- # Harry Potter support network ----------------------------------------------
#> # A longitudinal, labelled, complex, directed network of 64 students and 544
#> support arcs over 6 waves
#> 
#> -- Nodes
#> # A tibble: 64 x 5
#>   name              schoolyear gender house      active
#>   <chr>                  <int> <chr>  <chr>      <logi>
#> 1 Adrian Pucey            1989 male   Slytherin  TRUE  
#> 2 Alicia Spinnet          1989 female Gryffindor TRUE  
#> 3 Angelina Johnson        1989 female Gryffindor TRUE  
#> 4 Anthony Goldstein       1991 male   Ravenclaw  TRUE  
#> # i 60 more rows
#> 
#> -- Changes
#> # A tibble: 81 x 4
#>    time  node var    value
#>   <int> <int> <chr>  <lgl>
#> 1     2     9 active TRUE 
#> 2     2    21 active TRUE 
#> 3     2    35 active TRUE 
#> 4     2    39 active FALSE
#> # i 77 more rows
#> 
#> -- Ties
#> # A tibble: 544 x 3
#>    from    to  wave
#>   <int> <int> <dbl>
#> 1    11    11     1
#> 2    11    25     1
#> 3    11    26     1
#> 4    11    44     1
#> # i 540 more rows
#> 
}\if{html}{\out{</div>}}
}
\usage{
data(fict_potter)
}
\description{
Goele Bossaert and Nadine Meidert coded peer support ties among 64 characters
in the Harry Potter books.
Each author coded four of seven books using NVivo,
with the seventh book coded by both and serving to assess inter-rater reliability.
The first six books concentrated on adolescent interactions,
were studied in their paper, and are made available here.
The peer support ties mean voluntary emotional, instrumental, or informational support,
or praise from one living, adolescent character to another within the book's pages.
In addition, nodal attributes name, schoolyear (which doubles as their age),
gender, and their house assigned by the sorting hat are included.
}
\references{
Bossaert, Goele and Nadine Meidert (2013).
"'We are only as strong as we are united, as weak as we are divided'. A dynamic analysis of the peer support networks in the Harry Potter books."
\emph{Open Journal of Applied Sciences}, 3(2): 174-185.
\doi{10.4236/ojapps.2013.32024}
}
\keyword{datasets}
