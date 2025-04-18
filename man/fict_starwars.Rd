% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_ison.R
\docType{data}
\name{fict_starwars}
\alias{fict_starwars}
\title{Seven one-mode Star Wars character interactions (Gabasova 2016)}
\format{
\if{html}{\out{<div class="sourceCode">}}\preformatted{#> -- # Star Wars network data ----------------------------------------------------
#> # A longitudinal, labelled, complex, weighted, directed network of 110
#> characters and 563 interaction arcs over 7 waves
#> 
#> -- Nodes
#> # A tibble: 110 x 12
#>   name            species homeworld sex   height hair_color eye_color skin_color
#>   <chr>           <chr>   <chr>     <chr>  <int> <chr>      <chr>     <chr>     
#> 1 Admiral Ackbar  Mon Ca~ Mon Cala  male     180 none       orange    brown mot~
#> 2 Admiral Statura Human   Garel     male     172 black      brown     light     
#> 3 Anakin          Human   Tatooine  male     188 blond      blue      fair      
#> 4 Bail Organa     Human   Alderaan  male     191 black      brown     tan       
#> # i 106 more rows
#> # i 4 more variables: birth_year <dbl>, mass <dbl>, faction <chr>, active <lgl>
#> 
#> -- Changes
#> # A tibble: 184 x 4
#>    time  node var    value
#>   <int> <int> <chr>  <chr>
#> 1     2     7 active TRUE 
#> 2     2    10 active TRUE 
#> 3     2    11 active FALSE
#> 4     2    13 active FALSE
#> # i 180 more rows
#> 
#> -- Ties
#> # A tibble: 563 x 4
#>    from    to weight  wave
#>   <int> <int>  <int> <int>
#> 1    80    73     11     1
#> 2    80    79     14     1
#> 3    80     3     16     1
#> 4    80   106      3     1
#> # i 559 more rows
#> 
}\if{html}{\out{</div>}}
}
\usage{
data(fict_starwars)
}
\description{
One-mode network dataset collected by Gabasova (2016)
on the interactions between Star Wars characters in each movie from
Episode 1 ("The Phantom Menace") to Episode 7 ("The Force Awakens").

Characters are named (eg. R2-D2, Anakin, Chewbacca)
and the following node attributes are provided where available:
height, mass, hair color, skin color, eye color, birth year, sex, homeworld, and species.
The node attribute 'faction' has also been added,
denoting the faction (eg. Jedi, Rebel Alliance, etc)
that Star Wars characters belong to in each episode
(coding completed by Yichen Shen, Tiphaine Aeby, and James Hollway).

Weighted ties represent the number of times characters speak
within the same scene of each film, indicated by the wave (1-7).

Change in the composition of the network is tracked by the variable 'active',
though several other variables also change
(mostly as Anakin becomes \emph{spoiler alert}).
}
\details{
The network for each episode may be extracted and used separately,
eg. \code{to_time(fict_starwars, 1)} for Episode 1.
}
\references{
Gabasova, E. (2016).
\emph{Star Wars social network.}.
\doi{10.5281/zenodo.1411479}
}
\keyword{datasets}
