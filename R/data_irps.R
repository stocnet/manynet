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

## Hijackers ####

#' One-mode multiplex network of relationships between 9/11 hijackers (Krebs 2002)
#'
#' @description
#'   This network records two different types of relationships between and
#'   surrounding the hijackers of four planes in the United States 
#'   on September 11, 2001, culminating in those planes crashing into four
#'   locations: New York's World Trade Center (North and South buildings),
#'   as well as the Pentagon and a location in Somerset County, Pennsylvania.
#'   
#'   The hijackers were members of al-Qaeda.
#'   Valdis Krebs collected further information from newspapers on the
#'   broader network of associates of these hijackers,
#'   reflecting on the challenges of collecting this information even
#'   after the fact.
#'   
#'   The data includes two types of ties:
#'   "trust"ed prior contacts among the hijackers,
#'   and "association" ties among the hijackers but also their broader associates.
#'   All associates are named, along with a logical vector about whether they
#'   were a hijacker or not, and if so which their (eventual) target was.
#' @docType data
#' @keywords datasets
#' @name irps_911
#' @references
#' Krebs, Valdis. 2002.
#' "Mapping networks of terrorist cells". 
#' _Connections_ 24(3): 43-52.
#' @usage data(irps_911)
#' @format 
#'   ```{r, echo = FALSE}
#'   irps_911
#'   ```
"irps_911"

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

## Revere ####

#' Two-mode network of Paul Revere's (Fischer 1995)
#'
#' @description
#'   This network is of Paul Revere and 253 of his contemporary's overlapping 
#'   memberships in seven colonial organisations.
#'   The data has been collected by Kieran Healy from the appendix to
#'   David Hackett Fischer's "Paul Revere's Ride".
#'   It highlights Paul Revere's centrality in this network, and thus his
#'   ability to mobilise the towns he rode through on horseback north
#'   from Boston on the night of April 18, 1775.
#'   This is in contrast to William Dawes, who set out the same night,
#'   but south. 
#'   Despite both men coming from similar class and backgrounds,
#'   and riding through towns with similar demography and political leanings,
#'   only Paul Revere was able to mobilise those he encountered,
#'   and his social network was thought key to this.
#' @docType data
#' @keywords datasets
#' @name irps_revere
#' @usage data(irps_revere)
#' @references
#'   Fischer, David Hackett. 1995.
#'   "Paul Revere's Ride".
#'   Oxford: Oxford University Press.
#' 
#'   Han, Shin-Kap. 2009.
#'   "The Other Ride of Paul Revere: The Brokerage Role in the Making of the American Revolution".
#'   _Mobilization: An International Quarterly_, 14(2): 143-162.
#'   \doi{10.17813/maiq.14.2.g360870167085210}
#'   
#'   Healy, Kieran. 2013.
#'   "Using Metadata to find Paul Revere".
#' @format
#'   ```{r, echo = FALSE}
#'   irps_revere
#'   ```
"irps_revere"

## Nuclear Discourse ####

#' Two-mode dynamic discourse network of Germany's nuclear energy phase-out (Haunss and Hollway 2023)
#'
#' @description
#'   Following the 11 March 2011 Fukushima nuclear disaster in Japan,
#'   there was a vigorous public debate in Germany about the future of nuclear energy.
#'   This network captures the discourse established by 337 actors,
#'   including individual politicians, experts, parties, and the media,
#'   and their claims about nuclear energy and German nuclear energy policy.
#'   These claims were with respect to 54 concepts coded,
#'   and could be supportive or critical, and could also be repeated.
#' @docType data
#' @keywords datasets
#' @name irps_nuclear
#' @usage data(irps_nuclear)
#' @references
#'   Haunss Sebastian, James Hollway. 2023.
#'   "Multimodal mechanisms of political discourse dynamics and the case of Germany’s nuclear energy phase-out". 
#'   _Network Science_, 11(2):205-223. 
#'   \doi{10.1017/nws.2022.31}
#' @format
#'   ```{r, echo = FALSE}
#'   irps_nuclear
#'   ```
"irps_nuclear"

