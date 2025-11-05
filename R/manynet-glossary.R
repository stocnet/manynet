# Glossary ####

#' Adding network glossary items
#' 
#' @description 
#'   This function adds a glossary item, useful in tutorials.
#'   
#' @param text The text to appear.
#' @param ref The name of the glossary item to index.
#'   If NULL, then the function will search the glossary for 'text' instead.
#' @name glossary
NULL

#' @rdname glossary
#' @export
gloss <- function(text, ref = NULL){
  if(is.null(ref)) ref <- tolower(text)
  if(!ref %in% names(glossies)) 
    snet_abort("No glossary entry for '{text}' exists.") else {
      defn <- glossies[which(names(glossies)==ref)]
      options(mnet_glossary = unique(c(ref, getOption("mnet_glossary", default = ""))))
      paste(paste0("<abbr><dfn title='", defn, "'>"), text, "</dfn></abbr>")
    }
}

#' @rdname glossary
#' @export
print_glossary <- function(){
  defns <- getOption("mnet_glossary", default = "")
  if(length(defns)!=0){
    glossd <- glossies[names(glossies) %in% defns]
    glossn <- gsub("([[:alpha:]])([[:alpha:]]+)", "\\U\\1\\L\\2", names(glossd), perl=TRUE)
    glosst <- data.frame(term = paste("<dt>",glossn,"</dt>"), 
                         defn = paste("<dd>    ",glossd,"</dd>"))
    paste("<dl>",paste(paste(glosst$term, glosst$defn), collapse = " "),"</dl>")
  }
}

#' @rdname glossary
#' @export
clear_glossary <- function(){
  options(mnet_glossary = vector())
}

## Definitions ####

glossies <- list(
  acyclic = "An acyclic network is a network without any cycles.",
  aperiodic = "An aperiodic network is a network where the greatest common divisor of the lengths of its cycles is one.",
  adhesion = "The minimum number of ties to remove to increase the number of components.",
  agglomerative = "An agglomerative algorithm is one that starts with each node in its own cluster and then merges them.",
  alter = "An alter is a node connected to a focal node (ego).",
  anti = "An anti-tie, -edge, or antigraph is the complement of a network.",
  arc = "An ordered pair of nodes indicating a directed tie or edge from a tail to a head.",
  authority = "An authority is a node pointed to by many hubs.",
  automorphiceq = "Two or more nodes are automorphically equivalent if they can be interchanged without changing the structure of the network.",
  betweenness = "The betweenness centrality of a node is the proportion of shortest paths between all pairs of nodes that pass through that node.",
  blockmodel = "A blockmodel reduces a network to a smaller comprehensible structure of the roles positions take with respect to one another.",
  bridge = "A bridge or isthmus is a tie whose deletion increases the number of components.",
  centralization = "A measure of how unequal the centralities of the nodes in a network are.",
  circumference = "A network's circumference is the length of its longest simple cycle.",
  clique = "A clique is a set of mutually adjacent nodes.",
  closeness = "The closeness centrality of a node is the reciprocal of the sum of its distances to all other nodes.",
  cohesion = "The minimum number of nodes to remove to increase the number of components.",
  community = "A community is a set of nodes more densely connected to one another than to other nodes in the network.",
  complete = "A complete network is one where all ties that could exist are present.",
  complex = "A complex network is one that includes or can include loops or self-ties.",
  component = "A component is a connected subgraph not part of a larger connected subgraph.",
  configuration = "A configuration network is one where the degree sequence is fixed.",
  connected = "A connected network is one with a single (strong) component.",
  constraint = "The constraint of a node is a measure of how much its connections are also connected to one another.",
  core = "A core-periphery is a bipartition of nodes into maximally dense and sparse blocks.",
  cutpoint = "A cutpoint or articulation point is a node whose deletion increases the number of components.",
  cug = "A conditional uniform graph (CUG) is a random graph generated under constraints defined by some property of the observed network.",
  cycle = "A simple cycle is a closed walk without any repeated nodes.",
  degree = "The degree of a node is the number of connections it has.",
  dendrogram = "A dendrogram is a tree diagram that records the sequences of merges or splits from some, say, hierarchical clustering.",
  density = "The density of a network is the proportion of possible ties that are present.",
  diameter = "A network's diameter is the maximum length of any shortest path.",
  distribution = "A degree distribution is the frequency distribution of the degrees of the nodes in a network.",
  distance = "The distance between two nodes is the length of the shortest path between them.",
  divisive = "A divisive algorithm is one that starts with all nodes in one cluster and then splits them.",
  dominating = "A dominating set is the set of nodes that includes or is adjacent to every node in the network.",
  dyad = "A dyad is a pair of nodes and the ties between them.",
  eccentricity = "A node's eccentricity is the maximum distance from that node to any other node.",
  ego = "An ego is a focal node in a network.",
  eigenvector = "The eigenvector centrality of a node is the corresponding value in the dominant eigenvector of the adjacency matrix.",
  empty = "An empty network is a network without any ties.",
  Eulerian = "An Eulerian path is a walk that uses each tie in a network exactly once.",
  geodesic = "A geodesic is a shortest path between two nodes.",
  giant = "The giant component is the component that includes the most nodes in the network.",
  girth = "A network's girth is the length of its shortest cycle.",
  graphlet = "A graphlet is a small, connected, induced, non-isomorphic subgraphs.",
  height = "The height of a network is the maximum length of any directed path.",
  heterogeneity = "A measure of how varied the nodes in a network are.",
  heterophily = "A tendency for nodes to connect to dissimilar nodes.",
  hierclust = "Hierarchical clustering is a method of cluster analysis that seeks to build a hierarchy of clusters.",
  hit = "A herd immunity threshold is the proportion of the population that would need to be immune for a disease to cease being endemic.",
  homophily = "A tendency for nodes to connect to similar nodes.",
  hub = "A hub is a node connected to many authorities.",
  independent = "An independent set is a set of nodes that induces an empty subgraph.",
  induced = "An induced subgraph comprises all ties in a subset of the nodes in a network.",
  internal = "An internal node is a node with degree larger than one.",
  intersection = "The intersection of two networks is their largest common subgraph.",
  intervention = "A network intervention is a process to accelerate behavioural change or improve performance in a network.",
  isolate = "An isolate is a node with degree equal to zero.",
  kcoreness = "A k-core is the induced subgraph formed by removing all nodes of degree less than k following earlier removals.",
  label = "A labelled network includes unique labels for each node (or ties) in the network.",
  lattice = "A network that can be drawn as a regular tiling.",
  loop = "A loop is a self-tie with a single node as both endpoints, forming a cycle of length 1.",
  LTM = "A linear threshold model is a diffusion model where nodes are resistant to activation up to some threshold.",
  man = "A random network conditional on the number of mutual, asymmetric, and null dyads.",
  matching = "A matching is a set of ties that do not share any nodes.",
  mixed = "A mixed network is a network that includes both directed and undirected ties.",
  modularity = "The modularity of a membership assignment is the difference of the cross-cluster ties from the expected value.",
  motif = "A subgraph that is exceptional or significant compared to a null model.",
  mrqap = "A multiple regression quadratic assignment procedure (MRQAP) is a regression model for network data using a permutation test.",
  multilevel = "A network of more than one set of nodes that includes ties both between and within the different node sets.",
  multimodal = "A network that includes more than one set of nodes.",
  multiplex = "A network that includes multiple types of tie.",
  neighborhood = "A node's neighborhood is the set of other nodes to which that node is connected.",
  network = "A network comprises a set of nodes/vertices, a set of ties/edges among them, and usually some node or tie attributes.",
  orbit = "An orbit is a unique position in a subgraph.",
  order = "The order of a network is the number of its nodes.",
  path = "A path is a walk without repeated nodes.",
  pendant = "A pendant or leaf node has a degree of 1.",
  permutation = "A permuted network is one where the nodes/ties have been rearranged or resequenced.",
  planar = "A planar graph has an embedding onto a Euclidean plane.",
  power = "An eigenvector-style centrality that allows being connected to more peripheral nodes to contribute centrality.",
  property = "A network property depends only on the structure and not on any labelled or attribute information.",
  qap = "A quadratic assignment procedure (QAP) is a permutation test for network data.",
  radius = "A network's radius is the minimum eccentricity of any node.",
  random = "A random network is one where ties are placed among nodes according to some probability distribution.",
  reachability = "The ability of reach one node from another in a network.",
  reciprocity = "A measure of how often nodes in a directed network are mutually linked.",
  reduced = "A reduced graph is a contraction of a network into the ties within and between blocks.",
  regular = "A k-regular network includes only nodes with degree of k.",
  regulareq = "Two or more nodes are regularly equivalent if they have similar motif patterns.",
  richness = "A measure of how many different types of nodes are in a network.",
  scalefree = "A scale-free network is a type of network whose degree distribution asymptotically follows a power law.",
  simplex = "A simplex network is one without loops or multiple adjacencies.",
  sink = "A sink is a node without outgoing ties.",
  size = "The size of a network is its number of ties.",
  smallworld = "A small-world network is a network where most nodes are not neighbours but can reach each other in a small number of steps.",
  source = "A source is a node without incoming ties.",
  spanning = "A spanning subgraph includes all nodes from the original network.",
  sparse = "A sparse network has relatively few ties for its number of nodes.",
  star = "A star network has one internal, dominating, universal node.",
  strength = "A network's strength is the minimum ratio of removed ties to components created.",
  structeq = "Two or more nodes are structurally equivalent if they have similar ties to and from all other nodes in the network.",
  structfold = "A structural fold is an overlap between two or more cohesive groups in a network.",
  structhole = "A structural hole is a gap between two parts of a network that would benefit from being connected.",
  subgraph = "A subgraph comprises a subset of the nodes and ties in a network.",
  supergraph = "A supergraph is formed by adding nodes, ties, or both to a network.",
  toughness = "A network's toughness is the minimum ratio of removed nodes to components created.",
  transitivity = "Triadic closure is where if the connections A-B and A-C exist among three nodes, there is a tendency for B-C also to be formed.",
  threshold = "A threshold is a limit over which behaviour is expected to vary.",
  tour = "A closed trail, a walk without repeated ties.",
  trail = "A walk without repeated ties.",
  transitive = "The transitive closure of a network is the proportion of nodes that are connected by a path of length two that are also connected by a directed tie.",
  triangle = "A cycle of length three in a network.",
  undirected = "An undirected or line network is one in which tie direction is undefined.",
  universal = "A universal, dominating, or apex node is adjacent to every other node in the network.",
  unweighted = "An unweighted network is where the ties have not been assigned weights.",
  volume = "The sum of the degrees of a set of nodes.",
  walk = "A walk is a sequence of ties that joins a sequence of nodes.",
  weighted = "A weighted network is where the ties have been assigned weights."
)
