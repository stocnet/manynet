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
#'  Multiplex graph object of friends, social, and task ties 
#'  between 16 anonymous students in an honors algebra class (M182).
#'  Each type of tie is weighted:
#'  the `friends` ties are weighted 
#'  `2` = best friends, `1` = friend, and `0` is not a friend;
#'  `social` consists of social interactions per hour;
#'  and `tasks` consists of task interactions per hour.
#' @docType data
#' @keywords datasets
#' @name ison_algebra
#' @usage data(ison_algebra)
#' @references
#' McFarland, Daniel A. (2001) “Student Resistance.”
#' _American Journal of Sociology_ 107(3): 612-78.
#' \doi{10.1086/338779}.
#' @source 
#' See also `data(studentnets.M182, package = "NetData")`
#' 
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

## Judo moves ####

#' One-mode judo moves network (Bastazini 2025)
#'
#' @description
#'   Judo is a martial art with a long history and many different techniques.
#'   It involves a dynamic 'chess match' of throws, holds, locks, 
#'   submission techniques, and other maneuvers.
#'   The techniques are often combined in sequences to create fluid and 
#'   effective combinations to score points or achieve victory.
#'   As the author of this network describes,
#'   "While individual techniques (called waza) are foundational,
#'   the real artistry lies in how they are chained together --
#'   through renraku-waza (combination techniques) and 
#'   renzoku-waza (continuous combination techniques)"
#'   This network describes the relationships between 33 individual judo moves,
#'   as recognised by the Kodokan (the official international governing body of judo),
#'   where an arc indicates that one move can be followed by another.
#' @docType data
#' @keywords datasets
#' @name ison_judo_moves
#' @usage data(ison_judo_moves)
#' @references
#'   Bastazini, Vinicius. 2025. 
#'   "The Dynamics of the “Gentle Way”: Exploring Judo Attack Combinations as Networks in R",
#'   https://geekcologist.wordpress.com/2025/05/27/the-dynamics-of-the-gentle-way-exploring-judo-attack-combinations-as-networks-in-r/
#'   
#'   Kashiwazaki, Katsuhiko, and Hidetoshi Nakanishi. 1995. 
#'   _Attacking Judo: A Guide to Combinations and Counters_. 
#'   Ippon Books.
#'   
#'   Kawaishi, Mikinosuke. 1963. 
#'   _Standing judo: The combinations and counter-attacks_. 
#'   Budoworks.
#'   
#'   van Haesendonck, F.M. 1968. 
#'   _Judo: Ecyclopédie par l’Image_. 
#'   Éditions Erasme: Anvers-Bruxelles.
#' @format
#'   ```{r, echo = FALSE}
#'   ison_judo_moves
#'   ```
"ison_judo_moves"

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

## Emotions ####

#' One-mode, weighted network of emotional transitions (Trampe et al. 2015)
#'
#' @description
#'   Emotions are highly interconnected, and one emotion often follows another.
#'   This network describes the transitions between 18 different emotions as 
#'   experienced in everyday life.
#'   The data is collected from 11,000 participants who completed daily 
#'   questionnaires on the emotions they felt at a given moment.
#'   While Trampe et al. (2015) created and analysed an undirected network 
#'   in their paper, the directed network constructed by Will Hipson 
#'   is shared here.
#' @docType data
#' @keywords datasets
#' @name ison_emotions
#' @references
#'   Trampe, Debra, Jordi Quoidbach, and Maxime Taquet. 2015. 
#'   "Emotions in everyday life". 
#'   _PLOS ONE_. 
#'   \doi{10.1371/journal.pone.0145450}
#'   
#'   Hipson, Will. 2019.
#'   https://www.r-bloggers.com/2019/03/network-analysis-of-emotions/
#' @usage data(ison_emotions)
#' @format 
#'   ```{r, echo = FALSE}
#'   ison_emotions
#'   ```
"ison_emotions"

