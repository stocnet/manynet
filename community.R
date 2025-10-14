## ----setup, include=FALSE------------------------------------------------------------
library(learnr)
library(manynet)
library(autograph)
knitr::opts_chunk$set(echo = FALSE)
clear_glossary()

friends <- to_uniplex(ison_algebra, "friends")
social <- to_uniplex(ison_algebra, "social")
tasks <- to_uniplex(ison_algebra, "tasks")








## ----data-solution-------------------------------------------------------------------
data("ison_algebra", package = "manynet")
?manynet::ison_algebra
# If you want to see the network object, you can run the name of the object
ison_algebra
# or print the code with brackets at the front and end of the code
(ison_algebra <- manynet::ison_algebra)








## ----addingnames-solution------------------------------------------------------------
ison_algebra <- to_named(ison_algebra)
graphr(ison_algebra)












## ----separatingnets-solution---------------------------------------------------------
friends <- to_uniplex(ison_algebra, "friends")
gfriend <- graphr(friends) + ggtitle("Friendship")

social <- to_uniplex(ison_algebra, "social")
gsocial <- graphr(social) + ggtitle("Social")

tasks <- to_uniplex(ison_algebra, "tasks")
gtask <- graphr(tasks) + ggtitle("Task")

# We now have three separate networks depicting each type of tie from the ison_algebra network:
gfriend + gsocial + gtask






## ----dens-explicit-solution----------------------------------------------------------
# calculating network density manually according to equation
net_ties(tasks)/(net_nodes(tasks)*(net_nodes(tasks)-1))




## ----dens-solution-------------------------------------------------------------------
net_density(tasks)






## ----recip-solution------------------------------------------------------------------
net_reciprocity(tasks)
# this function calculates the amount of reciprocity in the whole network


## ----recip-explanation, exercise = TRUE----------------------------------------------
tasks %>% mutate_ties(rec = tie_is_reciprocated(tasks)) %>% graphr(edge_color = "rec")
net_indegree(tasks)




## ----trans-solution------------------------------------------------------------------
net_transitivity(tasks)
# this function calculates the amount of transitivity in the whole network










## ----setup-women-solution------------------------------------------------------------
data("ison_southern_women")
ison_southern_women
graphr(ison_southern_women, node_color = "type")




## ----hardway-solution----------------------------------------------------------------
twomode_matrix <- as_matrix(ison_southern_women)
women_matrix <- twomode_matrix %*% t(twomode_matrix)
event_matrix <- t(twomode_matrix) %*% twomode_matrix


## ----easyway, exercise=TRUE, exercise.setup = "setup-women"--------------------------
# women-graph
# to_mode1(): Results in a weighted one-mode object that retains the row nodes from
# a two-mode object, and weights the ties between them on the basis of their joint
# ties to nodes in the second mode (columns)

women_graph <- to_mode1(ison_southern_women)
graphr(women_graph)

# note that projection `to_mode1` involves keeping one type of nodes
# this is different from to_uniplex above, which keeps one type of ties in the network

# event-graph
# to_mode2(): Results in a weighted one-mode object that retains the column nodes from
# a two-mode object, and weights the ties between them on the basis of their joint ties
# to nodes in the first mode (rows)

event_graph <- to_mode2(ison_southern_women)
graphr(event_graph)




## ----otherway-solution---------------------------------------------------------------
tie_weights(to_mode1(ison_southern_women, similarity = "count"))
tie_weights(to_mode1(ison_southern_women, similarity = "jaccard"))
tie_weights(to_mode1(ison_southern_women, similarity = "rand"))
tie_weights(to_mode1(ison_southern_women, similarity = "pearson"))
tie_weights(to_mode1(ison_southern_women, similarity = "yule"))








## ----twomode-cohesion-solution-------------------------------------------------------
net_transitivity(women_graph)
net_transitivity(event_graph)
net_equivalency(ison_southern_women)












## ----comp-no-solution----------------------------------------------------------------
# note that friends is a directed network
net_components(friends)
net_components(to_undirected(friends))










## ----comp-memb-solution--------------------------------------------------------------
friends <- friends %>% 
  mutate(weak_comp = node_components(to_undirected(friends)),
         strong_comp = node_components(friends))
graphr(friends, node_color = "weak_comp") + ggtitle("Weak components") +
graphr(friends, node_color = "strong_comp") + ggtitle("Strong components")




## ----blogsize, exercise = TRUE-------------------------------------------------------
# This is a large network
net_nodes(irps_blogs)
# Let's concentrate on just a sample of 240 
# (by deleting a random sample of 1250)
blogs <- delete_nodes(irps_blogs, sample(1:1490, 1250))
graphr(blogs)


## ----blogisolates, exercise = TRUE, exercise.setup = "blogsize"----------------------
sum(node_is_isolate(blogs))


## ----blogcomp, exercise = TRUE, exercise.setup = "blogsize"--------------------------
net_components(blogs)
net_components(to_undirected(blogs))


## ----blogtogiant, exercise=TRUE, warning=FALSE, fig.width=9, exercise.setup = "blogsize"----
blogs <- blogs %>% to_giant()
sum(node_is_isolate(blogs))
graphr(blogs)


## ----bloggraph, exercise=TRUE, exercise.setup = "blogtogiant", warning=FALSE, fig.width=9----
blogs %>% mutate_nodes(part = node_in_partition()) %>% 
  graphr(node_color = "part")


## ----blogmod, exercise=TRUE, exercise.setup = "blogtogiant"--------------------------
net_modularity(blogs, membership = node_in_partition(blogs))




## ----blogmodassign, exercise=TRUE, exercise.setup = "blogtogiant", warning=FALSE, fig.width=9----
graphr(blogs, node_color = "Leaning")
net_modularity(blogs, membership = node_attribute(blogs, "Leaning"))




## ----walk, exercise=TRUE, exercise.setup = "separatingnets"--------------------------
friend_wt <- node_in_walktrap(friends, times=50)








## ----walk-solution-------------------------------------------------------------------
friend_wt <- node_in_walktrap(friends, times=50)
# results in a modularity of 
net_modularity(friends, friend_wt)










## ----walkplot-solution---------------------------------------------------------------
friends <- friends %>% 
  mutate(walk_comm = friend_wt)
graphr(friends, node_color = "walk_comm")
# to be fancy, we could even draw the group borders around the nodes using the node_group argument
graphr(friends, node_group = "walk_comm")
# or both!
graphr(friends,
       node_color = "walk_comm",
       node_group = "walk_comm") +
  ggtitle("Walktrap",
    subtitle = round(net_modularity(friends, friend_wt), 3))


## ----eb, exercise=TRUE, exercise.setup = "separatingnets"----------------------------
friend_eb <- node_in_betweenness(friends)
friend_eb








## ----ebplot-solution-----------------------------------------------------------------
friends <- friends %>% 
  mutate(eb_comm = friend_eb)
graphr(friends,
       node_color = "eb_comm",
       node_group = "eb_comm") +
  ggtitle("Edge-betweenness",
    subtitle = round(net_modularity(friends, friend_eb), 3))








## ----fg-solution---------------------------------------------------------------------
friend_fg <- node_in_greedy(friends)
friend_fg # Does this result in a different community partition?
net_modularity(friends, friend_fg) # Compare this to the edge betweenness procedure

# Again, we can visualise these communities in different ways:
friends <- friends %>% 
  mutate(fg_comm = friend_fg)
graphr(friends,
       node_color = "fg_comm",
       node_group = "fg_comm") +
  ggtitle("Fast-greedy",
    subtitle = round(net_modularity(friends, friend_fg), 3))




## ----incomm, exercise = TRUE, exercise.setup = "separatingnets"----------------------
node_in_community(friends)




## ----freeplay, exercise = TRUE, fig.width=9------------------------------------------
irps_books

