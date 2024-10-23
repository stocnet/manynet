# Classics ####

## Adolescents ####

#' One-mode subset of the adolescent society network (Coleman 1961)
#'
#' @description
#'  One-mode subset of Coleman's adolescent society network (Coleman 1961),
#'  as used in Feld's (1991) "Why your friends have more friends than you do".
#'  Coleman collected data on friendships among students in 12 U.S. high schools.
#'  Feld explored a subset of 8 girls from one of these schools, "Marketville",
#'  and gave them fictitious names, which are retained here.
#' @docType data
#' @keywords datasets
#' @name ison_adolescents
#' @usage data(ison_adolescents)
#' @references
#'   Coleman, James S. 1961. _The Adolescent Society_.
#'   New York: Free Press.
#'
#'   Feld, Scott. 1991. “Why your friends have more friends than you do”
#'   _American Journal of Sociology_ 96(6): 1464-1477.
#'   \doi{10.1086/229693}.
#' @format
#'   ```{r, echo = FALSE}
#'   ison_adolescents
#'   ```
"ison_adolescents"

## Algebra ####

#' Multiplex graph object of friends, social, and task ties (McFarland 2001)
#'
#' @description
#'  Multiplex graph object of friends, social, and task ties between 16 anonymous students.
#'  M182 was an honors algebra class where researchers
#'  collected friendship, social, and task ties between 16 students.
#'  The edge attribute `friends` contains friendship ties,
#'  where `2` = best friends, `1` = friend, and `0` is not a friend.
#'  `social` consists of social interactions per hour,
#'  and `tasks` consists of task interactions per hour.
#' @docType data
#' @keywords datasets
#' @name ison_algebra
#' @usage data(ison_algebra)
#' @references
#' McFarland, Daniel A. (2001) “Student Resistance.”
#' _American Journal of Sociology_ 107(3): 612-78.
#' \doi{10.1086/338779}.
#' @source See also `data(studentnets.M182, package = "NetData")`
#' Larger comprehensive data set publicly available, contact Daniel A. McFarland for details.
#' @format
#'   ```{r, echo = FALSE}
#'   ison_algebra
#'   ```
"ison_algebra"

## Karateka ####

#' One-mode karateka network (Zachary 1977)
#'
#' @description
#'   The network was observed in a university Karate club in 1977.
#'   The network describes association patterns among 34 members
#'   and maps out allegiance patterns between members and either Mr. Hi,
#'   the instructor, or the John A. the club president
#'   after an argument about hiking the price for lessons.
#'   The allegiance of each node is listed in the `obc` argument
#'   which takes the value 1 if the individual sided with Mr. Hi after the fight
#'   and 2 if the individual sided with John A.
#' @docType data
#' @keywords datasets
#' @name ison_karateka
#' @usage data(ison_karateka)
#' @references
#'   Zachary, Wayne W. 1977. “An Information Flow Model for Conflict and Fission in Small Groups.”
#'   _Journal of Anthropological Research_ 33(4):452–73.
#'   \doi{10.1086/jar.33.4.3629752}.
#' @format
#'   ```{r, echo = FALSE}
#'   ison_karateka
#'   ```
"ison_karateka"

## Koenigsberg ####

#' One-mode Seven Bridges of Koenigsberg network (Euler 1741)
#'
#' @description
#'   The Seven Bridges of Koenigsberg is a notable historical problem in mathematics and laid the foundations of graph theory. 
#'   The city of Koenigsberg in Prussia (now Kaliningrad, Russia) was set on both sides of the Pregel River, 
#'   and included two large islands which were connected to each other and the mainland by seven bridges.
#'   A weekend diversion for inhabitants was to find a walk through the city that would cross each bridge once and only once. 
#'   The islands could not be reached by any route other than the bridges, 
#'   and every bridge must have been crossed completely every time 
#'   (one could not walk half way onto the bridge and then turn around and later cross the other half from the other side).
#'   In 1735, Leonard Euler proved that the problem has no solution.
#' @docType data
#' @keywords datasets
#' @name ison_koenigsberg
#' @usage data(ison_koenigsberg)
#' @references
#'   Euler, Leonard. 1741. “Solutio problematis ad geometriam situs pertinentis.”
#'   _Commentarii academiae scientiarum Petropolitanae_.
#' @source `{igraphdata}`
#' @format
#'   ```{r, echo = FALSE}
#'   ison_koenigsberg
#'   ```
"ison_koenigsberg"

## Laterals ####

#' Two-mode projection examples (Hollway 2021)
#'
#' @description
#'  These networks are for demonstration purposes and do not describe any real world network.
#'  All examples contain named nodes.
#'  The networks are gathered together as a list and can be retrieved simply by plucking 
#'  the desired network.
#' @docType data
#' @keywords datasets
#' @name ison_laterals
#' @usage data(ison_laterals)
#' @format
#'   ```{r, echo = FALSE}
#'   ison_laterals
#'   ```
"ison_laterals"

## Networkers ####

#' One-mode EIES dataset (Freeman and Freeman 1979)
#'
#' @description A directed, simple, named, weighted graph with 32 nodes and 440
#' edges. Nodes are academics and edges illustrate the communication patterns
#' on an Electronic Information Exchange System among them. Node attributes
#' include the number of citations (`Citations`) and the discipline of the
#' researchers (`Discipline`). Edge weights illustrate the number of emails
#' sent from one academic to another over the studied time period.
#' @docType data
#' @keywords datasets
#' @name ison_networkers
#' @usage data(ison_networkers)
#' @source networkdata package
#' @references
#' Freeman, Sue C. and Linton C. Freeman. 1979.
#' \emph{The networkers network: A study of the impact of a new communications medium on sociometric structure}.
#' Social Science Research Reports No 46. Irvine CA, University of California.
#'
#' Wasserman Stanley and Katherine Faust. 1994.
#' \emph{Social Network Analysis: Methods and Applications}.
#' Cambridge University Press, Cambridge.
#' @format
#'   ```{r, echo = FALSE}
#'   ison_networkers
#'   ```
"ison_networkers"

## Brandes ####

#' One-mode and two-mode centrality demonstration networks
#'
#' This network should solely be used
#' for demonstration purposes as it does not describe a real network.
#' To convert into the two-mode version, 
#' assign `ison_brandes %>% rename(type = twomode_type)`.
#' @docType data
#' @keywords datasets
#' @name ison_brandes
#' @usage data(ison_brandes)
#' @format
#'   ```{r, echo = FALSE}
#'   ison_brandes
#'   ```
"ison_brandes"

## Southern Women ####

#' Two-mode southern women (Davis, Gardner and Gardner 1941)
#'
#' @description
#'   Two-mode network dataset collected by Davis, Gardner and Gardner (1941)
#'   about the pattern of a group of women's participation 
#'   at informal social events in Old City during a 9 month period,
#'   as reported in the \emph{Old City Herald} in 1936.
#'   By convention, the nodes are named by the women's first names 
#'   and the code numbers of the events,
#'   but the women's surnames and titles (Miss, Mrs.) are recorded here too.
#'   The events' dates are recorded in place of the Surname,
#'   and these dates are also offered as a tie attribute.
#' @docType data
#' @keywords datasets
#' @name ison_southern_women
#' @usage data(ison_southern_women)
#' @references
#'   Davis, Allison, Burleigh B. Gardner, and Mary R. Gardner. 1941.
#'   \emph{Deep South}.
#'   Chicago: University of Chicago Press.
#' @format
#'   ```{r, echo = FALSE}
#'   ison_southern_women
#'   ```
"ison_southern_women"

## Lawfirm ####

#' One-mode lawfirm (Lazega 2001)
#'
#' @description
#' One-mode network dataset collected by Lazega (2001)
#' on the relations between partners in a corporate law firm called SG&R in New England 1988-1991.
#' This particular subset includes the 36 partners among the 71 attorneys of this firm.
#' Nodal attributes include seniority, formal status, office in which they work, gender, lawschool they attended,
#' their age, and how many years they had been at the firm.
#' @details
#' The larger data from which this subset comes includes also individual performance measurements (hours worked, fees brought in)
#' and attitudes concerning various management policy options (see also `{sand}`),
#' their strong-coworker network, advice network, friendship network, and indirect control network.
#' @docType data
#' @keywords datasets
#' @name ison_lawfirm
#' @usage data(ison_lawfirm)
#' @source `{networkdata}`
#' @references
#'   Lazega, Emmanuel. 2001.
#'   \emph{The Collegial Phenomenon: The Social Mechanisms of Cooperation Among Peers in a Corporate Law Partnership}.
#'   Oxford: Oxford University Press.
#' @format
#'   ```{r, echo = FALSE}
#'   ison_lawfirm
#'   ```
"ison_lawfirm"

## Physicians ####

#' Four multiplex one-mode physician diffusion data (Coleman, Katz, and Menzel, 1966)
#'
#' @description
#'   Ron Burt prepared this data from 
#'   Coleman, Katz and Menzel's 1966 study on medical innovation. 
#'   They had collected data from physicians in four towns in Illinois: 
#'   Peoria, Bloomington, Quincy and Galesburg.
#'   These four networks are held as separate networks in a list.
#'   
#'   Coleman, Katz and Menzel were concerned with the impact of network ties 
#'   on the physicians' adoption of a new drug, tetracycline. 
#'   Data on three types of ties were collected in response to three questions:
#'   
#'   - advice: "When you need information or advice about questions of therapy 
#'   where do you usually turn?" 
#'   - discussion: "And who are the three or four physicians with whom you most often find yourself 
#'   discussing cases or therapy in the course of an ordinary week – last week for instance?" 
#'   - friendship: "Would you tell me the first names of your three friends 
#'   whom you see most often socially?"
#'   
#'   Additional questions and records of prescriptions provided additional information: 
#'   - recorded date of tetracycline `adoption` date
#'   - years in `practice` 
#'   (note that these are `{messydates}`-compatible dates)
#'   - `conferences` attended 
#'   (those that attended "Specialty" conferences presumably also attended "General" conferences)
#'   - regular subscriptions to medical `journals`
#'   - `free_time` spent associating with doctors
#'   - `discussions` on medical matters when with other doctors sociallyy
#'   - memberships in `clubs` with other doctores
#'   - number of top 3 `friends` that are doctors
#'   - time practicing in current `community`
#'   - `patients` load (ordinal)
#'   - physical `proximity` to other physicians (in building/sharing office)
#'   - medical `specialty` (GP/Internist/Pediatrician/Other)
#' @docType data
#' @keywords datasets
#' @name ison_physicians
#' @usage data(ison_physicians)
#' @references
#'   Coleman, James, Elihu Katz, and Herbert Menzel. 1966.
#'   \emph{Medical innovation: A diffusion study}.
#'   Indianapolis: The Bobbs-Merrill Company.
#' @source `{networkdata}`
#' @format
#'   ```{r, echo = FALSE}
#'   ison_physicians
#'   ```
"ison_physicians"

## High-tech ####

#' One-mode multiplex, directed network of managers of a high-tech company (Krackhardt 1987)
#'
#' @description
#'   21 managers of a company of just over 100 employees manufactured high-tech equipment 
#'   on the west coast of the United States.
#'   Three types of ties were collected:
#'   
#'   - _friends_: managers' answers to the question "Who is your friend?"
#'   - _advice_: managers' answers to the question "To whom do you go to for advice?"
#'   - _reports_: "To whom do you report?" based on company reports
#'   
#'   The data is anonymised, but four nodal attributes are included:
#'   
#'   - _age_: the manager's age in years
#'   - _tenure_: the manager's length of service
#'   - _level_: the manager's level in the corporate hierarchy,
#'   where 3 = CEO, 2 = Vice President, and 1 = manager
#'   - _dept_: one of four departments, B, C, D, E,
#'   with the CEO alone in A
#' @docType data
#' @keywords datasets
#' @name ison_hightech
#' @usage data(ison_hightech)
#' @references
#'   Krackhardt, David. 1987. "Cognitive social structures". _Social Networks_ 9: 104-134.
#' @format
#'   ```{r, echo = FALSE}
#'   ison_hightech
#'   ```
"ison_hightech"

## Monks ####

#' Multiplex network of three one-mode signed, weighted networks and a three-wave longitudinal network of monks (Sampson 1969)
#'
#' @description
#'   The data were collected for an ethnographic study of community structure in a New England monastery. 
#'   Various sociometric data was collected of the novices attending the minor seminary of 'Cloisterville' 
#'   preparing to join the monastic order.
#'   
#'   - `type = "like"` records whom novices said they liked most at three time points/waves
#'   - `type = "esteem"` records whom novices said they held in esteem (sign > 0) and disesteem (sign < 0)
#'   - `type = "praise"` records whom novices said they praised (sign > 0) and blamed (sign < 0)
#'   - `type = "influence"` records whom novices said were a positive influence (sign > 0) and negative influence (sign < 0)
#'   
#'   All networks are weighted.
#'   Novices' first choices are weighted 3, the second 2, and third choices 1.
#'   Some subjects offered tied ranks for their top four choices.
#'   
#'   In addition to node names,
#'   a 'groups' variable records the four groups that Sampson observed during his time there:
#'   
#'   - The _Loyal_ Opposition consists of novices who entered the monastery first and defended existing practices
#'   - The _Young Turks_ arrived later during a period of change and questioned practices in the monastery
#'   - The _Interstitial_ did not take sides in the debate
#'   - The _Outcasts_ were novices that were not accepted in the group
#'   
#'   Information about senior monks was not included.

#'   While `type = "like"` is observed over three waves,
#'   the rest of the data was recorded retrospectively from the end of the study,
#'   after the network fragmented.
#'   The waves in which the novitiates were expelled (1), voluntarily departed (2 and 3),
#'   or remained (4) are given in the nodal attribute "left".
#' @docType data
#' @keywords datasets
#' @name ison_monks
#' @references
#'   Sampson, Samuel F. 1969. _Crisis in a cloister_. 
#'   Unpublished doctoral dissertation, Cornell University.
#'   
#'   Breiger R., Boorman S. and Arabie P. 1975. 
#'   "An algorithm for clustering relational data with applications to social network analysis and comparison with multidimensional scaling". 
#'   _Journal of Mathematical Psychology_, 12: 328-383.
#' @usage data(ison_monks)
#' @format 
#'   ```{r, echo = FALSE}
#'   ison_monks
#'   ```
"ison_monks"

## Dolphins ####

#' One-mode, undirected network of frequent associations in a dolphin pod (Lusseau et al. 2003)
#'
#' @description
#'   These data contain the frequent associations between the 62 dolphins of a
#'   pod of dolphins living off Doubtful Sound, New Zealand.
#'   Additional information can be found in the literature cited below.
#' @docType data
#' @keywords datasets
#' @name ison_dolphins
#' @references
#'   Lusseau, David, K. Schneider, O. J. Boisseau, P. Haase, E. Slooten, and S. M. Dawson. 2003. 
#'   "The bottlenose dolphin community of Doubtful Sound features a large proportion of long-lasting associations", 
#'   _Behavioral Ecology and Sociobiology_ 54, 396-405.
#'   
#'   Lusseau, David. 2003.
#'   "The emergent properties of a dolphin social network",
#'   _Proc. R. Soc. London B_ 270(S): S186-S188.
#'   \doi{10.1098/rsbl.2003.0057}
#'   
#'   Lusseau, David. 2007. 
#'   "Evidence for social role in a dolphin social network". 
#'   _Evolutionary Ecology_ 21: 357–366. 
#'   \doi{10.1007/s10682-006-9105-0}
#' @usage data(ison_dolphins)
#' @format 
#'   ```{r, echo = FALSE}
#'   ison_dolphins
#'   ```
"ison_dolphins"

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
#'   A network of 36 Lord of the Rings book characters and 66 interactional relationships.
#'   The ties are unweighted and concern only interaction.
#'   Interaction can be cooperative or conflictual.
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
#' One-mode network dataset collected by Gabasova (2016)
#' on the interactions between Star Wars characters in each movie from
#' Episode 1 (The Phantom Menace) to Episode 7 (The Force Awakens).
#' There is a separate network for each episode,
#' and the data is listed in order from episode 1 to 7.
#' The network for each episode varies in the number of nodes and ties.
#' For all networks, characters are named (eg. R2-D2, Anakin, Chewbacca)
#' and the following node attributes are provided where available:
#' height, mass, hair color, skin color, eye color, birth year, sex, homeworld, and species.
#' The node attribute 'faction' has also been added,
#' denoting the faction (eg. Jedi, Rebel Alliance, etc) 
#' that Star Wars characters belong to in each episode
#' (coding completed with help of Yichen Shen and Tiphaine Aeby).
#' Weighted ties represent the number of times characters speak 
#' within the same scene of the film.
#' @details
#' The network for each episode may be extracted and used separately,
#' eg. `ison_starwars[[1]]` or `ison_starwars$Episode I` for Episode 1.
#' @docType data
#' @keywords datasets
#' @name ison_starwars
#' @usage data(ison_starwars)
#' @references
#'   Gabasova, E. (2016).
#'   \emph{Star Wars social network.}.
#'   \doi{10.5281/zenodo.1411479}
#' @format
#'   ```{r, echo = FALSE}
#'   ison_starwars
#'   ```
"ison_starwars"

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

## Greys ####

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
#'   The data is current up to (I think?) season 10?
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

# Political ####

## Books ####

#' One-mode undirected network of co-purchased books about US politics on Amazon
#'
#' @description
#'   This network consists of books about US politics sold by Amazon.com.
#'   Ties represent books that are often purchased together,
#'   as revealed by Amazon's 'customers who bought this book also bought these other
#'   books' section on those books' pages on the website.
#'   
#'   Information about the book's leaning "Liberal", "Neutral", or "Conservative"
#'   were added separately by Mark Newman based on the abstracts, descriptions,
#'   and reviews posted on Amazon.
#'   
#'   These data should be cited as V. Krebs, unpublished, http://www.orgnet.com/.
#'   
#' @docType data
#' @keywords datasets
#' @name irps_books
#' @author Valdis Krebs, Mark Newman
#' @usage data(irps_books)
#' @format 
#'   ```{r, echo = FALSE}
#'   irps_books
#'   ```
"irps_books"

## Blogs ####

#' One-mode directed network of links between US political blogs (Adamic and Glance 2005)
#'
#' @description
#'   This network consists of the blogosphere around the time of the 2004
#'   US presidential election until February 2005.
#'   The 2004 election was the first in which blogging played a significant role.
#'   Ties were constructed from a crawl of the front page of each blog.
#'   
#'   Political leaning is indicated as "Liberal" (or left leaning) or 
#'   "Conservative" (or right leaning), sourced from blog directories.
#'   Some blogs were labelled manually, 
#'   based on incoming and outgoing links and posts.
#' @docType data
#' @keywords datasets
#' @name irps_blogs
#' @references
#' Adamic, Lada, and Natalie Glance. 2005.
#' "The political blogosphere and the 2004 US Election: Divided they blog". 
#' _LinkKDD '05: Proceedings of the 3rd international workshop on Link discovery_, 36-43.
#' \doi{10.1145/1134271.1134277}
#' @usage data(irps_blogs)
#' @format 
#'   ```{r, echo = FALSE}
#'   irps_blogs
#'   ```
"irps_blogs"

## WWI ####

#' One-mode signed network of relationships between European major powers (Antal et al. 2006)
#'
#' @description
#'   This network records the evolution of the major relationship changes
#'   between the protagonists of World War I (WWI) from 1872 to 1907.
#'   It is incomplete both in terms of (eventual) parties to the war as well
#'   as some other relations, but gives a good overview of the main alliances
#'   and enmities.
#'   
#'   The data series begins with the Three Emperors' League (1872, revived in 1881)
#'   between Germany, Austria-Hungary, and Russia.
#'   The Triple Alliance in 1882 joined Germany, Austria-Hungary, and Italy into
#'   a bloc that lasted until WWI.
#'   A bilateral alliance between Germany and Russia lapsed in 1890,
#'   and a French-Russian alliance developed between 1891-1894.
#'   The Entente Cordiale thawed and then fostered relations between Great Britain
#'   and France in 1904, and a British-Russian agreement in 1907 bound
#'   Great Britain, France, and Russia into the Triple Entente.
#' @docType data
#' @keywords datasets
#' @name irps_wwi
#' @references
#' Antal, Tibor, Pavel Krapivsky, and Sidney Redner. 2006.
#' "Social balance on networks: The dynamics of friendship and enmity". 
#' _Physica D_ 224: 130-136.
#' \doi{10.1016/j.physd.2006.09.028}
#' @usage data(irps_wwi)
#' @format 
#'   ```{r, echo = FALSE}
#'   irps_wwi
#'   ```
"irps_wwi"

## US States ####

#' One-mode undirected network of US state contiguity (Meghanathan 2017)
#'
#' @description
#'   This network is of contiguity between US states.
#'   States that share a border are connected by a tie in the network.
#'   The data is a network of 107 ties among 50 US states (nodes).
#'   States are named by their two-letter ISO-3166 code.
#'   This data includes also the names of the capitol cities of each state,
#'   which are listed in the node attribute 'capitol'.
#' @docType data
#' @keywords datasets
#' @name irps_usgeo
#' @usage data(irps_usgeo)
#' @references
#'   Meghanathan, Natarajan. 2017. 
#'   "Complex network analysis of the contiguous United States graph." 
#'   _Computer and Information Science_, 10(1): 54-76.
#'   \doi{10.5539/cis.v10n1p54}
#' @format
#'   ```{r, echo = FALSE}
#'   irps_usgeo
#'   ```
"irps_usgeo"
