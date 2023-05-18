
#Load Data
library(tidyverse)
statcast <- read_csv("statcast2022_pitch_by_pitch.csv")
fields <- read_csv("fields.csv")
retro <- read_csv("all2022.csv", col_names = pull(fields, Header), na = character())

# Hard coded run expectancies from Fangraphs
exp_runs_0100 <- 1.068
exp_runs_0001 <- 0.243
exp_runs_1000_end <- 0.831

# Probabilities of baserunning success
# For now I just completely made up values
# once we get data we can calculate functions for these based on lead distance, pickoffs, and count
P_safe_on_steal <- function(d, p, b, s) {
  0.75
}

P_safe_on_pickoff <- function(d, p, b, s) {
  0.99
}

P_steal_attempt <- function(d, p, b, s) {
  0.1
}


# Calculate Count Transition Probabilities

# Give each half inning a unique ID
statcast %>% 
  mutate(HALF.INNING = paste(game_pk, inning, inning_topbot)) -> statcast2022

#Convert baserunners from NA
statcast2022$on_1b <- ifelse(is.na(statcast2022$on_1b), "", statcast2022$on_1b)
statcast2022$on_2b <- ifelse(is.na(statcast2022$on_2b), "", statcast2022$on_2b)
statcast2022$on_3b <- ifelse(is.na(statcast2022$on_3b), "", statcast2022$on_3b)

#Create STATE Variable
statcast2022 %>% 
  arrange(game_pk, at_bat_number, pitch_number) %>% 
  mutate(BASES = paste(ifelse(on_1b > "", 1, 0),
                       ifelse(on_2b > "", 1, 0),
                       ifelse(on_3b > "", 1, 0), sep = ""), 
         STATE = paste0(BASES, balls, strikes, outs_when_up)) -> statcaststates

#Add next state
statcaststates %>% 
  mutate(NEW.STATE = lead(STATE)) -> statcaststates

#Fix End State Errors
statcaststates %>% 
  mutate(NEW.STATE = ifelse((lead(inning) != inning) | inning_topbot != lead(inning_topbot), 3, NEW.STATE)) -> statcaststates
statcaststates[708540, 96] <- "3"

# Count outs on the play
statcaststates$event_outs <- as.numeric(substr(statcaststates$NEW.STATE, nchar(statcaststates$NEW.STATE), nchar(statcaststates$NEW.STATE))) -
  as.numeric(substr(statcaststates$STATE, nchar(statcaststates$STATE), nchar(statcaststates$STATE)))

#Fix walk-offs
statcaststates$event_outs <- ifelse(statcaststates$events != "triple_play" & statcaststates$event_outs == 3 , 0, statcaststates$event_outs)


#Count outs in an inning - including incomplete innings (walkoffs) messed up transition probabilities
statcast2 <- statcaststates %>% 
  group_by(HALF.INNING) %>% 
  summarize(Innings_Outs = sum(event_outs))
statcast_merged <- statcaststates %>% 
  inner_join(statcast2)

#Filter out incomplete innings and skipped ABs
statcast_merged %>% filter(Innings_Outs == 3) -> statcast_merged
statcast_merged %>% filter(lead(at_bat_number) - at_bat_number < 2) -> statcast_final



# Filter out count errors and create transition matrix
T_matrix <- statcast_final %>% 
  filter(STATE != "00042 0", STATE != "01042 2",
         NEW.STATE != "00042 0", NEW.STATE != "01042 2",
         STATE != "00013 0", NEW.STATE != "00013 0") %>% 
  select(STATE, NEW.STATE) %>% 
  table()

# Weird impossible transition due to data error
T_matrix[1,11] <- 0
view(T_matrix)
#Compute P(Transition State)
P_matrix <- 
  prop.table(T_matrix, 1)

# Probability Matrix Complete
P_mat <- as.data.frame(P_matrix)


# Solving Problem with Backwards Induction

d <- 10 # Pick a random value to start, doesn't matter yet

# Start state: 3-2 count, 0 outs, 2 pickoffs
start_state <- P_mat %>% filter(STATE == "100320")
P_Foul <- start_state %>% filter(NEW.STATE == "100320") # Only way for PA to continue is a foul ball
P_Foul <- P_Foul$Freq

# Run Value of pickoff attempt
f1_d <- P_safe_on_pickoff(d, 2, 3, 2) * exp_runs_0100 + (1 - P_safe_on_pickoff(d, 2, 3, 2)) * exp_runs_0001

# Solve for X
X <- (f1_d - P_steal_attempt(d, 2, 3, 2) * P_safe_on_steal(d, 2, 3, 2) * exp_runs_0100 - P_steal_attempt(d, 2, 3, 2) * (1 - P_safe_on_steal(d, 2, 3, 2)) * exp_runs_0001 - (1 - P_steal_attempt(d, 2, 3, 2)) * (1 - P_Foul) * exp_runs_1000_end) / ((1 - P_steal_attempt(d, 2, 3, 2)) * P_Foul)

# Make sure f0 = f1
f0_d <- P_steal_attempt(d, 2, 3, 2) * P_safe_on_steal(d, 2, 3, 2) * exp_runs_0100 +  P_steal_attempt(d, 2, 3, 2) * (1 - P_safe_on_steal(d, 2, 3, 2)) * exp_runs_0001 + (1 - P_steal_attempt(d, 2, 3, 2)) * (1 - P_Foul) * exp_runs_1000_end + (1 - P_steal_attempt(d, 2, 3, 2)) * (P_Foul) * f1_d

Exp_runs_1000322 <- X # This seems way too high


# One Step Away States

# Start State: 3-2 count, 0 outs, 1 pickoff
start_state <- P_mat %>% filter(STATE == "100320")
P_Foul <- start_state %>% filter(NEW.STATE == "100320") # Only way for PA to continue is a foul ball
P_Foul <- P_Foul$Freq

# Run Value of pickoff attempt - defintely too high - why would it be higher with 1 pickoff as opposed to 2?
f1_d <- P_safe_on_pickoff(d, 1, 3, 2) * Exp_runs_1000322 + (1 - P_safe_on_pickoff(d, 1, 3, 2)) * exp_runs_0001

# Solve for X
X <- (f1_d - P_steal_attempt(d, 1, 3, 2) * P_safe_on_steal(d, 1, 3, 2) * exp_runs_0100 - P_steal_attempt(d, 1, 3, 2) * (1 - P_safe_on_steal(d, 1, 3, 2)) * exp_runs_0001 - (1 - P_steal_attempt(d, 1, 3, 2)) * (1 - P_Foul) * exp_runs_1000_end) / ((1 - P_steal_attempt(d, 1, 3, 2)) * (P_Foul))

# Make sure f0 = f1 - not true, defintely a mistake somewhere
f0_d <- P_steal_attempt(d, 1, 3, 2) * P_safe_on_steal(d, 1, 3, 2) * exp_runs_0100 +  P_steal_attempt(d, 1, 3, 2) * (1 - P_safe_on_steal(d, 1, 3, 2)) * exp_runs_0001 + (1 - P_steal_attempt(d, 1, 3, 2)) * (1 - P_Foul) * exp_runs_1000_end + (1 - P_steal_attempt(d, 1, 3, 2)) * (P_Foul) * f1_d

Exp_runs_1000321 <- X # Obviously way too high


# Start State: 2-2 count, 0 outs, 2 pickoffs
start_state <- P_mat %>% filter(STATE == "100220")
P_Foul <- start_state %>% filter(NEW.STATE == "100220") # PA Continues on Foul or Ball
P_Ball <- start_state %>% filter(NEW.STATE == "100320") # PA Continues on Foul or Ball
P_Foul <- P_Foul$Freq 
P_Ball <- P_Ball$Freq 

# Run value of a pickoff attempt - for now same regardless of count
f1_d <- P_safe_on_pickoff(d,2,2,2) * exp_runs_0100 + (1 - P_safe_on_pickoff(d,2,2,2)) * exp_runs_0001

# Solve for x
X <- (f1_d - P_steal_attempt(d,2,2,2) * P_safe_on_steal(d,2,2,2) * exp_runs_0100 - P_steal_attempt(d,2,2,2) * (1 - P_safe_on_steal(d,2,2,2)) * exp_runs_0001 - (1 - P_steal_attempt(d,2,2,2)) * (1 - P_Foul - P_Ball) * exp_runs_1000_end - (1 - P_steal_attempt(d,2,2,2)) * (P_Ball) * Exp_runs_1000322) / ((1 - P_steal_attempt(d,2,2,2)) * (P_Foul))

# Make sure f1 = f0 - it does not
f0_d <- P_steal_attempt(d,2,2,2) * P_safe_on_steal(d,2,2,2) * exp_runs_0100 +  P_steal_attempt(d,2,2,2) * (1 - P_safe_on_steal(d,2,2,2)) * exp_runs_0001 + (1 - P_steal_attempt(d,2,2,2)) * (1 - P_Foul) * exp_runs_1000_end + (1 - P_steal_attempt(d,2,2,2)) * (P_Ball) * Exp_runs_1000322 + (1 - P_steal_attempt(d,2,2,2)) * (P_Foul) * X
Exp_runs_1000222 <- X # X value seems realistic

# Start State: 3-1 count, 0 outs, 2 pickoffs
start_state <- P_mat %>% filter(STATE == "100310")
P_Strike <- start_state %>% filter(NEW.STATE == "100320") # PA Continues on Strike
P_Strike <- P_Strike$Freq 

# Run value of a pickoff attempt - for now same regardless of count
f1_d <- P_safe_on_pickoff(d,2,3,1) * exp_runs_0100 + (1 - P_safe_on_pickoff(d,2,3,1)) * exp_runs_0001
# There's no X to solve for - calculate f0 - does not equal f1
f0_d <- P_steal_attempt(d,2,3,1) * P_safe_on_steal(d,2,3,1) * exp_runs_0100 +  P_steal_attempt(d,2,3,1) * (1 - P_safe_on_steal(d,2,3,1)) * exp_runs_0001 + (1 - P_steal_attempt(d,2,3,1)) * (1 - P_Foul) * exp_runs_1000_end + (1 - P_steal_attempt(d,2,3,1)) * (P_Strike) * Exp_runs_1000322 
# No X to solve for here?
Exp_runs_1000312 <- ???

