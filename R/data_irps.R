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
#' @details
#'   Adamic and Glance gathered the blog URLs from the eTalkingHead,
#'   BlogCatalog, CampaignLine, and Blogarama directories, which the 'Source'
#'   nodal attribute records. They retrieved a single front page for each blog
#'   on 8 February 2005, added the blogs those pages cited 17 or more times,
#'   and retrieved a front page for each of those on 22 February 2005.
#'   Libertarian, independent, and moderate blogs were not gathered.
#'   
#'   A tie is any link from one blog's front page to another's, whether it
#'   appeared in a post or in the blogroll in the sidebar, and the two are not
#'   distinguished.
#'   
#'   Note that 65 of the 19090 arcs repeat an arc already in the network,
#'   with nothing recorded to tell the two apart, so `tie_is_parallel()`
#'   marks 130 ties and `as_matrix()` reports two rather than one in those
#'   cells.
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
#' @details
#'   Each tie is one claim by one speaker about one concept on one day, so the
#'   network records a stream of events and not a panel.
#'   The day is held in a 'time' column, and whether the claim was supportive
#'   or critical in a 'weight' column of 1 or -1.
#'   
#'   A speaker may claim the same concept on more than one day, and 152 of the
#'   speaker-concept pairs do, one of them 15 times. Such claims follow one
#'   another rather than coexist, so they are repetitions and not parallel
#'   ties.
#'   Eight speaker-concept pairs make more than one claim on a single day,
#'   which `tie_is_parallel()` marks, covering 16 of the 1164 ties.
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

## Supreme Court ####

#' Two-mode network of votes on the Rehnquist court (Spaeth 1990)
#'
#' @description
#'   A two-mode network of 376 US Supreme Court cases and the nine justices
#'   who sat together on the Rehnquist court between the 1995 and 2004 terms.
#'   This was the longest period in the court's history without a change in
#'   its membership, which makes the whole run comparable.
#'
#'   A tie indicates that a justice voted with the majority on a case, and is
#'   weighted `1` for a full vote with the majority and `0.5` for a partial
#'   concurrence.
#'
#'   One nodal attribute is included:
#'
#'   - _term_: for cases, the court term in which the case was decided,
#'   from 1995 to 2004. This is `NA` for the justices.
#' @details
#'   Cases are labelled by an index and an abbreviation of the case name
#'   followed by the two-digit term, so that `E001_Ada95` is the first case of
#'   the 1995 term. Justice names have been corrected to their standard
#'   spelling.
#' @docType data
#' @keywords datasets
#' @name irps_supremecourt
#' @references
#'   Spaeth, Harold J. 1990.
#'   _United States Supreme Court Judicial Database_.
#'   Ann Arbor MI: Inter-university Consortium for Political and Social Research.
#'   \doi{10.3886/icpsr09422}
#'
#'   Doreian, Patrick, Paulette Lloyd, and Andrej Mrvar. 2013.
#'   "Partitioning large signed two-mode networks: Problems and prospects".
#'   _Social Networks_ 35(2): 212-230.
#'   \doi{10.1016/j.socnet.2012.01.002}
#' @source
#'   The UCINET standard dataset collection derives it from the Supreme Court Database.
#' @usage data(irps_supremecourt)
#' @format
#'   ```{r, echo = FALSE}
#'   irps_supremecourt
#'   ```
"irps_supremecourt"

## Tribes ####

#' One-mode signed network of Gahuku-Gama sub-tribes (Read 1954)
#'
#' @description
#'   Political relations among 16 Gahuku-Gama sub-tribes of the central
#'   highlands of New Guinea, as reported in Read's (1954) ethnography.
#'   This is one of the canonical datasets for the study of structural balance
#'   and of networks containing negative ties, since the sub-tribes stand in
#'   one of two mutually exclusive relations:
#'
#'   - `weight > 0` records _rova_, a relation of political alliance
#'   - `weight < 0` records _hina_, a relation of political opposition
#'
#'   Each relation holds between 29 pairs of sub-tribes.
#'   No pair of sub-tribes stands in both relations, and pairs with no
#'   recorded political relation are simply absent.
#' @details
#'   The network is held as a 'stocnet' object, so that the metadata reported
#'   in the GRAND statement can be recorded in its info component,
#'   including where and when the relations were observed and by what method.
#'   As is the convention for 'stocnet' objects, the sign of each relation is
#'   held as a negative or positive weight rather than in a separate 'sign'
#'   column.
#' @docType data
#' @keywords datasets
#' @name irps_tribes
#' @references
#'   Read, Kenneth E. 1954.
#'   "Cultures of the Central Highlands, New Guinea".
#'   _Southwestern Journal of Anthropology_ 10(1): 1-43.
#'   \doi{10.1086/soutjanth.10.1.3629074}
#'
#'   Everett, Martin G., and Stephen P. Borgatti. 2014.
#'   "Networks containing negative ties".
#'   _Social Networks_ 38: 111-120.
#'   \doi{10.1016/j.socnet.2014.03.005}
#' @source
#'   Sub-tribe names have been title-cased.
#'   The date recorded is that of Read's publication;
#'   his fieldwork in the Asaro valley preceded it by some years.
#' @usage data(irps_tribes)
#' @format
#'   ```{r, echo = FALSE}
#'   irps_tribes
#'   ```
"irps_tribes"

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
