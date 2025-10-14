## ----setup, include = FALSE----------------------------------------------------------
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
# ison_algebra
# or print the code with brackets at the front and end of the code
# (ison_algebra <- manynet::ison_algebra)








## ----separatingnets-solution---------------------------------------------------------
friends <- to_uniplex(ison_algebra, "friends")
graphr(friends) + ggtitle("Friendship")

social <- to_uniplex(ison_algebra, "social")
graphr(social) + ggtitle("Social")

tasks <- to_uniplex(ison_algebra, "tasks")
graphr(tasks) + ggtitle("Task")








## ----bridges, exercise = TRUE, exercise.setup = "objects-setup"----------------------
sum(tie_is_bridge(friends))
any(node_bridges(friends)>0)








## ----constraint-solution-------------------------------------------------------------
node_constraint(tasks)










## ----constraintplot-solution---------------------------------------------------------
tasks <- tasks %>% 
  mutate(constraint = node_constraint(tasks), 
         low_constraint = node_is_min(node_constraint(tasks)))
graphr(tasks, node_size = "constraint", node_color = "low_constraint")




## ----find-se, exercise = TRUE, exercise.setup = "data"-------------------------------
node_in_structural(ison_algebra)

ison_algebra %>% 
  mutate(se = node_in_structural(ison_algebra)) %>% 
  graphr(node_color = "se")










## ----construct-cor-solution----------------------------------------------------------
node_by_tie(ison_algebra)
dim(node_by_tie(ison_algebra))






## ----construct-binary-solution-------------------------------------------------------
# But it's easier to simplify the network by removing the classification into different types of ties.
# Note that this also reduces the total number of possible paths between nodes
ison_algebra %>%
  select_ties(-type) %>%
  node_by_tie()


## ----varyclust, exercise = TRUE, exercise.setup = "data"-----------------------------
alge <- to_named(ison_algebra) # fake names to make comparison clearer
plot(node_in_structural(alge, cluster = "hier", distance = "euclidean"))

# changing the type of distance used
plot(node_in_structural(alge, cluster = "hier", distance = "manhattan"))

# changing the clustering algorithm
plot(node_in_structural(alge, cluster = "concor", distance = "euclidean"))




## ----k-discrete, exercise = TRUE, exercise.setup = "varyclust"-----------------------
plot(node_in_structural(alge, k = 2))


## ----elbowsil, exercise = TRUE, exercise.setup = "varyclust"-------------------------
plot(node_in_structural(alge, k = "elbow"))
plot(node_in_structural(alge, k = "silhouette"))


## ----strict, exercise = TRUE, exercise.setup = "varyclust"---------------------------
plot(node_in_structural(alge, k = "strict"))


## ----strplot, exercise = TRUE, exercise.setup = "varyclust"--------------------------
alge %>% 
  mutate(se = node_in_structural(alge)) %>% 
  graphr(node_color = "se")






## ----summ-solution-------------------------------------------------------------------
summary(node_by_tie(alge),
        membership = node_in_structural(alge))






## ----block-solution------------------------------------------------------------------
# plot the blockmodel for the whole network
plot(as_matrix(alge),
     membership = node_in_structural(alge))

# plot the blockmodel for the friends, tasks, and social networks separately
plot(as_matrix(to_uniplex(alge, "friends")),
     membership = node_in_structural(alge))
plot(as_matrix(to_uniplex(alge, "tasks")),
     membership = node_in_structural(alge))
plot(as_matrix(to_uniplex(alge, "social")),
     membership = node_in_structural(alge))


## ----structblock, exercise = TRUE, exercise.setup = "varyclust", warning=FALSE-------
(bm <- to_blocks(alge, node_in_structural(alge)))

bm <- bm %>% as_tidygraph %>% 
  mutate(name = c("Freaks", "Squares", "Nerds", "Geek"))
graphr(bm)


## ----freeplay, exercise = TRUE-------------------------------------------------------


