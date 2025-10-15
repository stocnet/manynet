## ----pkgs, include = FALSE-----------------------------------------------------------
library(manynet)
library(autograph)










## ----coercion-solution---------------------------------------------------------------
# plot the one-mode version
graphr(ison_brandes)
ison_brandes2 <- ison_brandes %>% rename(type = twomode_type)
# plot the two-mode version
graphr(ison_brandes2, layout = "bipartite")






## ----addingnames-solution------------------------------------------------------------
ison_brandes <- to_named(ison_brandes)
# plot network with names
graphr(ison_brandes)








## ----degreesum-solution--------------------------------------------------------------
# manually calculate degree centrality
mat <- as_matrix(ison_brandes)
degrees <- rowSums(mat)
rowSums(mat) == colSums(mat)
# You can also just use a built in command in manynet though:
node_degree(ison_brandes, normalized = FALSE)








## ----distrib-solution----------------------------------------------------------------
# distribution of degree centrality scores of nodes
plot(node_degree(ison_brandes))














## ----micent-solution-----------------------------------------------------------------
node_betweenness(ison_brandes)
node_closeness(ison_brandes)
node_eigenvector(ison_brandes)










## ----otherdist-solution--------------------------------------------------------------
plot(node_betweenness(ison_brandes))
plot(node_closeness(ison_brandes))
plot(node_eigenvector(ison_brandes))




## ----ggid-solution-------------------------------------------------------------------
# plot the network, highlighting the node with the highest centrality score with a different colour
ison_brandes %>%
  mutate_nodes(color = node_is_max(node_degree())) %>%
  graphr(node_color = "color")

ison_brandes %>%
  mutate_nodes(color = node_is_max(node_betweenness())) %>%
  graphr(node_color = "color")

ison_brandes %>%
  mutate_nodes(color = node_is_max(node_closeness())) %>%
  graphr(node_color = "color")

ison_brandes %>%
  mutate_nodes(color = node_is_max(node_eigenvector())) %>%
  graphr(node_color = "color")




## ----ggid_twomode-solution-----------------------------------------------------------
ison_brandes2 %>%
  add_node_attribute("color", node_is_max(node_degree(ison_brandes2))) %>%
  graphr(node_color = "color", layout = "bipartite")

ison_brandes2 %>%
  add_node_attribute("color", node_is_max(node_betweenness(ison_brandes2))) %>%
  graphr(node_color = "color", layout = "bipartite")

ison_brandes2 %>%
  add_node_attribute("color", node_is_max(node_closeness(ison_brandes2))) %>%
  graphr(node_color = "color", layout = "bipartite")

ison_brandes2 %>%
  add_node_attribute("color", node_is_max(node_eigenvector(ison_brandes2))) %>%
  graphr(node_color = "color", layout = "bipartite")






## ----centzn-solution-----------------------------------------------------------------
net_degree(ison_brandes)
net_betweenness(ison_brandes)
net_closeness(ison_brandes)
print(net_eigenvector(ison_brandes), digits = 5)




## ----multiplot-solution--------------------------------------------------------------
ison_brandes <- ison_brandes %>%
  add_node_attribute("degree", node_is_max(node_degree(ison_brandes))) %>%
  add_node_attribute("betweenness", node_is_max(node_betweenness(ison_brandes))) %>%
  add_node_attribute("closeness", node_is_max(node_closeness(ison_brandes))) %>%
  add_node_attribute("eigenvector", node_is_max(node_eigenvector(ison_brandes)))
gd <- graphr(ison_brandes, node_color = "degree") + 
  ggtitle("Degree", subtitle = round(net_degree(ison_brandes), 2))
gc <- graphr(ison_brandes, node_color = "closeness") + 
  ggtitle("Closeness", subtitle = round(net_closeness(ison_brandes), 2))
gb <- graphr(ison_brandes, node_color = "betweenness") + 
  ggtitle("Betweenness", subtitle = round(net_betweenness(ison_brandes), 2))
ge <- graphr(ison_brandes, node_color = "eigenvector") + 
  ggtitle("Eigenvector", subtitle = round(net_eigenvector(ison_brandes), 2))
(gd | gb) / (gc | ge)
# ggsave("brandes-centralities.pdf")

