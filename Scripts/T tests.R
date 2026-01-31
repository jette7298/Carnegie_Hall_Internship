rm(list = ls())
library(tidyverse)

# Load the data
synchrony_idea_1 <- readRDS("~/Downloads/sync_results_parent_child_idea_1.rds")

synchrony_idea_2 <- readRDS("~/Downloads/sync_results_parent_child_idea_2.rds")

synchrony_idea_3 <- readRDS("~/Downloads/sync_results_parent_child_idea_3.rds")

# Calculate the differences and significance

results_1 <- t.test(sync_overall ~ Condition, data = synchrony_idea_1)
results_1

results_2 <- t.test(overall_strength ~ Condition, data = synchrony_idea_2)
results_2

results_3 <- t.test(rr_overall ~ Condition, data = synchrony_idea_3)
results_3


# Explanation for the difference between 1 and 3, and 2

# WCC / CRQA
## Capture local, time-varying, and nonlinear coordination
## Sensitive to intermittent, phase-locked, or pattern-based coupling
## Robust when synchrony is bursty or switches leader/follower
### Exactly the kind of coordination social interaction produces

# CCF metric
## Global, linear, and single-lag
## Penalizes role switching and intermittent coupling
## Axis-wise calculation and averaging further weakens effects

#  Hence: lower strength, and even reversed group differences

# Key interpretation
## CCF peak strength is a measure of consistency, not strength, of coordination.
## The concert group likely coordinates more often but less consistently at one fixed lag
## The playtime group may show weaker interaction but more mechanically stable timing, which CCF favours






