# Fictitious ####

## Marvel ####

#' Multilevel two-mode affiliation, signed one-mode networks of Marvel comic
#' book characters (Yuksel 2017)
#'
#' @description
#' This package includes two datasets related to the Marvel _comic book_ universe.
#' The first, `ison_marvel_teams`,  is a two-mode affiliation network of 53
#' Marvel comic book characters and their affiliations to 141 different teams.
#' This network includes only information about nodes' names and nodeset,
#' but additional nodal data can be taken from the other Marvel dataset here.
#'
#' The second network, `ison_marvel_relationships`, is a one-mode signed network
#' of friendships and enmities between the 53 Marvel comic book characters.
#' Friendships are indicated by a positive sign in the tie `sign` attribute,
#' whereas enmities are indicated by a negative sign in this edge attribute.
#' @details
#' Additional nodal variables have been coded and included by Dr Umut Yuksel:
#'
#' - **Gender**: binary character, 43 "Male" and 10 "Female"
#' - **PowerOrigin**: binary character, 2 "Alien", 1 "Cyborg", 5 "God/Eternal",
#' 22 "Human", 1 "Infection", 16 "Mutant", 5 "Radiation", 1 "Robot"
#' - **Appearances**: integer, in how many comic book issues they appeared in
#' - **Attractive**: binary integer, 41 1 (yes) and 12 0 (no)
#' - **Rich**: binary integer, 11 1 (yes) and 42 0 (no)
#' - **Intellect**: binary integer, 39 1 (yes) and 14 0 (no)
#' - **Omnilingual**: binary integer, 8 1 (yes) and 45 0 (no)
#' - **UnarmedCombat**: binary integer, 51 1 (yes) and 2 0 (no)
#' - **ArmedCombat**: binary integer, 25 1 (yes) and 28 0 (no)
#' 
#' See also https://graphics.straitstimes.com/STI/STIMEDIA/Interactives/2018/04/marvel-cinematic-universe-whos-who-interactive/index.html.
#' @docType data
#' @keywords datasets
#' @name ison_marvel
#' @usage data(ison_marvel_teams)
#' @source Umut Yuksel, 31 March 2017
#' @format
#'   ```{r, echo = FALSE}
#'   ison_marvel_teams
#'   ```
"ison_marvel_teams"

#' @rdname ison_marvel
#' @usage data(ison_marvel_relationships)
#' @format
#'   ```{r, echo = FALSE}
#'   ison_marvel_relationships
#'   ```
"ison_marvel_relationships"

## Lord of the Rings ####

#' One-mode network of Lord of the Rings character interactions
#'
#' @description
#'   The Lord of the Rings is a beloved, epic high fantasy novel written by
#'   J.R.R. Tolkien.
#'   This is a network of 36 Lord of the Rings book characters and 
#'   66 interactional relationships.
#'   
#'   The ties are unweighted and concern only interaction.
#'   Interaction can be cooperative or conflictual.
#'   
#'   In addition, the race of these characters has been coded,
#'   though not without debate.
#'   The most contentious is the coding of Tom Bombadil and Goldberry as Maiar,
#'   presumably coded as such to avoid having categories of one.
#' @docType data
#' @keywords datasets
#' @name fict_lotr
#' @usage data(fict_lotr)
#' @format
#'   ```{r, echo = FALSE}
#'   fict_lotr
#'   ```
"fict_lotr"

## Harry Potter ####

#' Six complex one-mode support data in Harry Potter books (Bossaert and Meidert 2013)
#'
#' @description
#'   Goele Bossaert and Nadine Meidert coded peer support ties among 64 characters
#'   in the Harry Potter books.
#'   Each author coded four of seven books using NVivo, 
#'   with the seventh book coded by both and serving to assess inter-rater reliability.
#'   The first six books concentrated on adolescent interactions,
#'   were studied in their paper, and are made available here.
#'   The peer support ties mean voluntary emotional, instrumental, or informational support,
#'   or praise from one living, adolescent character to another within the book's pages.
#'   In addition, nodal attributes name, schoolyear (which doubles as their age),
#'   gender, and their house assigned by the sorting hat are included.
#' @docType data
#' @keywords datasets
#' @name fict_potter
#' @usage data(fict_potter)
#' @references
#'   Bossaert, Goele and Nadine Meidert (2013). 
#'   "'We are only as strong as we are united, as weak as we are divided'. A dynamic analysis of the peer support networks in the Harry Potter books." 
#'   _Open Journal of Applied Sciences_, 3(2): 174-185.
#'   \doi{10.4236/ojapps.2013.32024}
#' @format
#'   ```{r, echo = FALSE}
#'   fict_potter
#'   ```
"fict_potter"

## Game of Thrones ####

#' One-mode Game of Thrones kinship (Glander 2017)
#'
#' @description
#'   The original dataset was put together by Erin Pierce and Ben Kahle for an
#'   assignment for a course on Bayesian statistics.
#'   The data included information on when characters died in the Song of Ice
#'   and Fire books,
#'   and some predictive factors such as whether they were nobles, married, etc.
#'   Shirin Glander extended this data set on character deaths in the TV series 
#'   Game of Thrones with the kinship relationships between the characters, 
#'   by scraping "A Wiki of Ice and Fire" and adding missing information by hand.
#'   There is certainly more that can be done here.
#' @docType data
#' @keywords datasets
#' @name fict_thrones
#' @usage data(fict_thrones)
#' @references
#'   Pierce, Erin, and Ben Kahle. 2015.
#'   "\href{http://allendowney.blogspot.com/2015/03/bayesian-survival-analysis-for-game-of.html}{Bayesian Survival Analysis in A Song of Ice and Fire}".
#'   
#'   Glander, Shirin. 2017. 
#'   "\href{https://datascienceplus.com/network-analysis-of-game-of-thrones/}{Network analysis of Game of Thrones}".
#' @format
#'   ```{r, echo = FALSE}
#'   fict_thrones
#'   ```
"fict_thrones"

## Star Wars ####

#' Seven one-mode Star Wars character interactions (Gabasova 2016)
#'
#' @description
#'   One-mode network dataset collected by Gabasova (2016)
#'   on the interactions between Star Wars characters in each movie from
#'   Episode 1 ("The Phantom Menace") to Episode 7 ("The Force Awakens").
#'   The data was constructed by parsing the scripts,
#'   as described in 
#'   https://evelinag.com/blog/2015/12-15-star-wars-social-network/index.html.
#'   
#'   Characters are named (eg. R2-D2, Anakin, Chewbacca)
#'   and the following node attributes are provided where available:
#'   height, mass, hair color, skin color, eye color, birth year, sex, homeworld, and species.
#'   The node attribute 'faction' has also been added,
#'   denoting the faction (eg. Jedi, Rebel Alliance, etc) 
#'   that Star Wars characters belong to in each episode
#'   (coding completed by Yichen Shen, Tiphaine Aeby, and James Hollway).
#'   
#'   Weighted ties represent the number of times characters speak 
#'   within the same scene of each film, indicated by the wave (1-7).
#'   
#'   Change in the composition of the network is tracked by the variable 'active',
#'   though several other variables also change 
#'   (mostly as Anakin becomes *spoiler alert*).
#' @details
#' The network for each episode may be extracted and used separately,
#' eg. `to_time(fict_starwars, 1)` for Episode 1.
#' @docType data
#' @keywords datasets
#' @name fict_starwars
#' @usage data(fict_starwars)
#' @references
#'   Gabasova, Evelina. 2016.
#'   \emph{Star Wars social network (Version 1.0.1)}.
#'   _Zenodo_.
#'   \doi{10.5281/zenodo.1411479}
#'   
#' @format
#'   ```{r, echo = FALSE}
#'   fict_starwars
#'   ```
"fict_starwars"

## Friends ####

#' One-mode undirected Friends character scene co-appearances (McNulty, 2020)
#'
#' @description
#' One-mode network collected by \href{https://github.com/keithmcnulty/friends_analysis/}{McNulty (2020)}
#' on the connections between the Friends TV series characters
#' from Seasons 1 to 10.
#' The `fict_friends` is an undirected network
#' containing connections between characters organised by season number,
#' which is reflected in the tie attribute 'wave'.
#' The network contains 650 nodes
#' Each tie represents the connection between a character pair (appear in the same scene),
#' and the 'weight' of the tie is the number of scenes the character pair appears in together.
#' For all networks, characters are named (eg. Phoebe, Ross, Rachel).
#' @docType data
#' @keywords datasets
#' @name fict_friends
#' @usage data(fict_friends)
#' @references
#'   McNulty, K. (2020).
#'   \emph{Network analysis of Friends scripts.}.
#' @format
#'   ```{r, echo = FALSE}
#'   fict_friends
#'   ```
"fict_friends"

## Greys Anatomy ####

#' One-mode undirected network of characters hook-ups on Grey's Anatomy TV show
#'
#' @description
#'   Grey's Anatomy is an American medical drama television series running on ABC since 2005.
#'   It focuses on the personal and professional lives of surgical interns, residents, and attendings
#'   at Seattle Grace Hospital, later renamed as the Grey Sloan Memorial Hospital. 
#'   \href{https://gweissman.github.io/post/grey-s-anatomy-network-of-sexual-relations/}{Gary Weissman} 
#'   collected data on the sexual contacts between characters on the television show
#'   through observation of the story lines in the episodes and fan pages,
#'   and this data was extended by 
#'   \href{http://badhessian.org/2012/09/lessons-on-exponential-random-graph-modeling-from-greys-anatomy-hook-ups/}{Benjamin Lind}
#'   including nodal attributes:
#'   
#'   - 'name': first and, where available, surname
#'   - 'sex': `F` for female and `M` for male
#'   - 'race': `White`, `Black`, or `Other`
#'   - 'birthyear': year born (some missing data)
#'   - 'position': `"Chief"`, `"Attending"`, `"Resident"`, `"Intern"`, `"Nurse"`, `"Non-Staff"`, `"Other"`
#'   - 'season': season that the character joined the show
#'   - 'sign': character's astrological starsign, if known
#'   
#' The data is current up to (I think?) season 10?
#'   
#' @docType data
#' @keywords datasets
#' @name fict_greys
#' @author Gary Weissman and Benjamin Lind
#' @usage data(fict_greys)
#' @format 
#'   ```{r, echo = FALSE}
#'   fict_greys
#'   ```
"fict_greys"

## Love Actually ####

#' Two-mode network of Love Actually characters and their scene appearances (Robinson 2015)
#'
#' @description
#'   Love Actually is a 2003 British romantic comedy film.
#'   This is a two-mode network of 20 characters and their appearances 
#'   across 76 scenes.
#'   The data was collected by David Robinson as parsed from the script. 
#'   
#' @docType data
#' @keywords datasets
#' @name fict_actually
#' @author David Robinson
#' @usage data(fict_actually)
#' @references
#' Robinson, David. 2015.
#' "Analyzing networks of characters in 'Love Actually'".
#' http://varianceexplained.org/r/love-actually-network/
#' @format 
#'   ```{r, echo = FALSE}
#'   fict_actually
#'   ```
"fict_actually"
