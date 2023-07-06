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
}')

ggdag(our_dag, text_size = 1.2, node_size = 18) +
  theme_dag() 
