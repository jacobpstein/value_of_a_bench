library(dagitty)
library(ggdag)
library(tidyverse)

our_dag <- dagitty('dag {
  "Bench Performance" [exposure,pos="-1.823,-0.308"]
  "Starter Performance" [adjusted,pos="-2.200,-1.520"]
  "Team Win Pct" [outcome,pos="-0.300,-0.082"]
  Injuries [adjusted,pos="-2.117,0.929"]
  "Bench Performance" -> "Team Win Pct" [pos="-0.750,0.047"]
  "Starter Performance" -> "Team Win Pct" [pos="-0.791,-1.045"]
  Injuries -> "Bench Performance"
  Injuries -> "Starter Performance"
  Injuries -> "Team Win Pct"
}') %>% 
  tidy_dagitty() 

ggdag(our_dag, text_size = 1.2, node_size = 18) +
  theme_dag() 

# DAG with some Wiz colors

p1 <- our_dag %>% 
  ggplot(aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
  geom_dag_point(aes(colour = name)) +
  geom_dag_edges() +
  geom_dag_label_repel(aes(label = name, fill = name), seed = 05082023,
                       color = "white", fontface = "bold") +
  theme_dag() +
  scale_color_manual(values = c("#E41134", "#C4CED4", "#00265B", "#ffb7c5")) +
  scale_fill_manual(values = c("#E41134", "#C4CED4", "#00265B", "#ffb7c5")) +
  theme(legend.position = "NA") +
  labs(title = "Model Graph of Bench and Starter Relationship with Winning"
       , caption = "wizardspoints.substack.com")

p1

ggsave("02 Output/DAG.png", p1, w = 14, h = 12, dpi = 300)

