## ----pkgs, include = FALSE-----------------------------------------------------------
library(manynet)
library(autograph)






## ----empty-solution------------------------------------------------------------------
(graphr(create_empty(50), "circle") + ggtitle("Empty graph"))
(graphr(create_filled(50)) + ggtitle("Complete graph"))
(graphr(create_filled(50/2)) + ggtitle("Complete graph (smaller)"))




## ----star-solution-------------------------------------------------------------------
(graphr(create_star(50)) + ggtitle("Star graph"))
(graphr(create_star(50, directed = TRUE)) + ggtitle("Star out"))
(graphr(to_redirected(create_star(50, directed = TRUE))) + ggtitle("Star in"))




## ----tree-solution-------------------------------------------------------------------
# width argument specifies the breadth of the branches
(graphr(create_tree(50, width = 2)) + ggtitle("Tree graph"))
(graphr(create_tree(50, width = 2, directed = TRUE)) + ggtitle("Tree out"))
(graphr(create_tree(50, width = 2, directed = TRUE), "tree") + ggtitle("Tree layout"))






## ----lattices-solution---------------------------------------------------------------
(graphr(create_lattice(50)) + ggtitle("One-mode lattice graph"))
(graphr(create_lattice(50/2)) + ggtitle("Smaller lattice graph"))




## ----rings-solution------------------------------------------------------------------
(graphr(create_ring(50)) + ggtitle("Ring graph", subtitle = "Starring Naomi Watts"))
# width argument specifies the width of the ring
(graphr(create_ring(50, width = 2), "circle") + ggtitle("The Ring Two", subtitle = "No different?"))
(graphr(create_ring(50, width = 2), "stress") + ggtitle("The Ring Two v2.0"))




## ----random-solution-----------------------------------------------------------------
(graphr(generate_random(50, 0.08)) + ggtitle("Random 1 graph"))
(graphr(generate_random(50, 0.08)) + ggtitle("Random 2 graph"))
(graphr(generate_random(50, 0.08)) + ggtitle("Random 3 graph"))




## ----randomno-solution---------------------------------------------------------------
(erdren4 <- graphr(generate_random(50, 200)) + ggtitle("Random 1 graph"))




## ----smallw-solution-----------------------------------------------------------------
(graphr(generate_smallworld(50, 0.025)) + ggtitle("Smallworld 1 graph"))
(graphr(generate_smallworld(50, 0.025)) + ggtitle("Smallworld 2 graph"))
(graphr(generate_smallworld(50, 0.025)) + ggtitle("Smallworld 3 graph"))




## ----smallwtest-solution-------------------------------------------------------------
net_smallworld(generate_smallworld(50, 0.25))




## ----scalef-solution-----------------------------------------------------------------
(graphr(generate_scalefree(50, 0.5)) +
    ggtitle("Scalefree 1 graph", subtitle = "Power = .5"))
(graphr(generate_scalefree(50, 1)) +
    ggtitle("Scalefree 2 graph", subtitle = "Power = 1"))
(graphr(generate_scalefree(50, 1.5)) +
    ggtitle("Scalefree 3 graph", subtitle = "Power = 1.5"))




## ----scaleftest-solution-------------------------------------------------------------
net_scalefree(generate_scalefree(50, 2))






## ----core-solution-------------------------------------------------------------------
(graphr(create_core(50)) + ggtitle("Core"))




## ----gnet-solution-------------------------------------------------------------------
lawfirm <- ison_lawfirm |> to_uniplex("friends") |> to_undirected()
graphr(lawfirm, node_color = "school", edge_color = "darkgray")
graphr(lawfirm, node_color = "gender", edge_color = "darkgray")
graphr(lawfirm, node_color = "office", edge_color = "darkgray")
graphr(lawfirm, node_color = "practice", edge_color = "darkgray")




## ----nodecore-solution---------------------------------------------------------------
lawfirm %>% 
  mutate(nc = node_is_core()) %>% 
  graphr(node_color = "nc", edge_color = "gray")




## ----netcore-solution----------------------------------------------------------------
net_core(lawfirm, node_is_core(lawfirm))






## ----chisq-solution------------------------------------------------------------------
chisq.test(as.factor(node_is_core(lawfirm)), 
           as.factor(node_attribute(lawfirm, "gender")))
chisq.test(as.factor(node_is_core(lawfirm)), 
           as.factor(node_attribute(lawfirm, "office")))
chisq.test(as.factor(node_is_core(lawfirm)), 
           as.factor(node_attribute(lawfirm, "school")))
chisq.test(as.factor(node_is_core(lawfirm)), 
           as.factor(node_attribute(lawfirm, "practice")))














## ----hierarchy, exercise = TRUE, warning=FALSE---------------------------------------
graphr(ison_emotions)
graphr(fict_thrones)


## ----hierarchy-solution--------------------------------------------------------------
graphr(ison_emotions)
net_by_hierarchy(ison_emotions)
graphr(fict_thrones)
net_by_hierarchy(fict_thrones)






## ----connected-solution--------------------------------------------------------------
net_connectedness(ison_adolescents)






## ----cohesion-solution---------------------------------------------------------------
net_cohesion(ison_adolescents)








## ----idcuts-solution-----------------------------------------------------------------
node_is_cutpoint(ison_adolescents)




## ----closerlook-solution-------------------------------------------------------------
ison_adolescents |> mutate(cut = node_is_cutpoint(ison_adolescents)) |> 
  graphr(node_color = "cut")




## ----tieside-solution----------------------------------------------------------------
net_adhesion(ison_adolescents)
ison_adolescents |> mutate_ties(cut = tie_is_bridge(ison_adolescents)) |> 
  graphr(edge_color = "cut")




## ----tiecoh-solution-----------------------------------------------------------------
ison_adolescents |> mutate_ties(coh = tie_cohesion(ison_adolescents)) |> 
  graphr(edge_size = "coh")


## ----freeplay, exercise = TRUE-------------------------------------------------------


