# Script Name:  NBA_Lineups.R
#      Author:  Nathan Hogg
#  Orig. Date:  1/29/2016
#    Overview:  This script seeks to examine lineup information for an NBA game
#
# NEED A WAY TO DETERMINE WHO CAME INTO THE GAME AT THE START OF EACH QUARTER
# OR THIS CODE IS USELESS
#
# Inputs:
#   1) gameID - number string for a certain game used by ESPN
#
# Outputs:
#   TBD - code is incomplete until solution to start of quarter lineups is found

rm(list = ls())

# Load the necessary packages for this program
library(rvest)
library(plyr)
library(stringr)
library(dplyr)
library(tidyr)
library(DT)
library(reshape2)
library(ggplot2)

# Input the game ID of the game to be analyzed
gameID = 400828579;
url <- read_html(paste0("http://espn.go.com/nba/playbyplay?gameId=", gameID, "&period=0"))

# Setting up constants for while loop
i <- 1;
test_table <- data.frame(x = "1", y = "2")

# Loop until the table with the play-by-play is found
while (nrow(test_table) < 50) {
    test_table <- html_table(html_nodes(url, "table")[[i]], fill = TRUE)
    i <- i + 1
}

# Make the test table the actual table and update column names
plays <- test_table
colnames(plays) <- plays[2, ]
remove(test_table)
i <- i - 1

# Get box score and pull out starters, bench players, and positions
url <- read_html(paste0("http://espn.go.com/nba/boxscore?gameId=", gameID))
box_score <- html_table(html_nodes(url,"table")[[i]], fill = TRUE)

# Create an empty list to store where the starters and bench start
index <- list(0, 0, 0, 0)
j <- 1

# Fill the index list with row numbers where starters/bench start
for (i in 1:nrow(box_score)) {
  if (box_score[i, 1] == "STARTERS" || box_score[i, 1] == "BENCH") {
    index[[j]] <- i
    j <- j + 1
  }
}

# Optional - make data frames of the home/away starters/bench
# away_starters <- data.frame(Players = box_score[(index[[1]] + 1):(index[[2]] - 1), 1])
# away_bench    <- data.frame(Players = box_score[(index[[2]] + 1):(index[[3]] - 6), 1])
# home_starters <- data.frame(Players = box_score[(index[[3]] + 1):(index[[4]] - 1), 1])
# home_bench    <- data.frame(Players = box_score[(index[[4]] + 1):(nrow(box_score) - 4), 1])

# Create a list for each of the team's starters and bench players
away_starters <- box_score[(index[[1]] + 1):(index[[2]] - 1), 1]
away_bench    <- box_score[(index[[2]] + 1):(index[[3]] - 6), 1]
home_starters <- box_score[(index[[3]] + 1):(index[[4]] - 1), 1]
home_bench    <- box_score[(index[[4]] + 1):(nrow(box_score) - 4), 1]

# Create a list for all players on each team
away_players <- c(away_starters, away_bench)
home_players <- c(home_starters, home_bench)

# Add a column to plays with the time left in the game

# Create an empty index for where quarters are designated
index <- list(0, 0, 0, 0)
j <- 1

# Loop through the plays table to find where quarters are designated
for (i in 1:nrow(plays)) {
  if (substr(plays[i, 1], 5, 19) == "Quarter Summary") {
    index[[j]] <- i
    j <- j + 1
  }
}

plays$quarter <- 0

# Add the quarter each play took place in using the quarter index
for (i in 1:nrow(plays)) {
  if (i < index[[2]]) {
    plays[i, 5] <- 1
    } else {
      if (i < index[[3]]) {
        plays[i, 5] <- 2
        } else {
          if (i < index[[4]]) {
            plays[i, 5] <- 3
            } else {
              plays[i, 5] <- 4
            }
        }
    }
}

# Remove the lines in the plays table that refer to quarters starts/stops
plays <- plays[-c(index[[1]], (index[[1]] + 1), index[[2]], (index[[2]] + 1),
                  index[[3]], (index[[3]] + 1), index[[4]], (index[[4]] + 1)), ]

# Create blank columns to be filled in later
plays$Time_Left <- 0
plays$Away_Score <- 0
plays$Home_Score <- 0
plays$Away_Sub_Out <- ""
plays$Away_Sub_In <- ""

# Loop through plays and tidily move data into the recently created blank columns
for (i in 1:nrow(plays)) {
  plays[i, 6] <- 36 - 12 * (plays[i, 5] - 1) + 
                 as.numeric(substr(plays[i, 1], 1, nchar(plays[i, 1]) - 3)) +
                 as.numeric(substr(plays[i, 1], nchar(plays[i, 1]) - 1, nchar(plays[i, 1]))) / 60
  
  plays[i, 7] <- as.numeric(substr(plays[i, 3], 1, regexpr("-", plays[i, 3])[1] - 1))
  plays[i, 8] <- as.numeric(substr(plays[i, 3], regexpr("-", plays [i, 3])[1] + 1, nchar(plays[i, 3])))
   
  if (grepl("enters the game for", plays[i, 2]) == TRUE) {
    plays[i, 9] <- substr(plays[i, 2], regexpr("enters the game for", plays[i, 2])[1] + 20, nchar(plays[i, 2]))
    plays[i, 10] <- substr(plays[i, 2], 1, regexpr("enters the game for", plays[i, 2])[1] - 2)
  }
}

# Create a column for every away player
for (i in 1:length(away_players)) {
  temp <- data.frame(X1 = NA)
  colnames(temp) <- substr(away_players[i], 1, regexpr(",", away_players[i]) - 1)
  plays <- cbind(plays, temp) 
}

# Separate starters names and positions
away_starters_names <- away_starters
for (i in 1:length(away_starters)) {
  away_starters_names[i] <- substr(away_starters[i], 1, regexpr(",", away_starters[i]) - 1)
}

# Set starters into the first row
for (i in 11:ncol(plays)) {
  if (colnames(plays)[i] %in% away_starters_names) {
    plays[1, i] <- 1
  } else {
    plays[1, i] <- 0
  }
}

# Update players in the game
for (i in 11:ncol(plays)) {
  for (j in 2:nrow(plays)) {
    if (plays[j, 10] == colnames(plays)[i]) {
      plays[j, i] <- 1
    } else {
      if (plays[j, 9] == colnames(plays)[i]) {
        plays[j, i] <- 0
      } else {
        plays[j, i] <- plays[j - 1, i]
      }
    }
  }
}

away_lineups <- rbind(plays[1, c(5:8, 11:ncol(plays))], plays[nchar(plays$Away_Sub_In) > 1, c(5:8, 11:ncol(plays))])

away_score_final <- as.numeric(substr(plays[nrow(plays) - 2, 3], 1, 
                                      regexpr("-", plays[nrow(plays) - 2, 3])[1] - 1))
home_score_final <- as.numeric(substr(plays[nrow(plays) - 2, 3], 
                                      regexpr("-", plays[nrow(plays) - 2, 3])[1] + 1,
                                      nchar(plays[nrow(plays) - 2, 3])))

# Create a column for the time played, pts scored, and pts allowed by each lineup
away_lineups$Time_Played <- 0
away_lineups$Pts_Scored <- 0
away_lineups$Pts_Allowed <- 0

for (i in 1:(nrow(away_lineups) - 1)) {
  away_lineups$Time_Played[i] <- away_lineups$Time_Left[i] - away_lineups$Time_Left[i + 1]
  away_lineups$Pts_Scored[i] <- away_lineups$Away_Score[i + 1] - away_lineups$Away_Score[i]
  away_lineups$Pts_Allowed[i] <- away_lineups$Home_Score[i + 1] - away_lineups$Home_Score[i]
}

# Fill in the time played and pts scored/allowed for the last lineup
away_lineups$Time_Played[nrow(away_lineups)] <- away_lineups$Time_Left[nrow(away_lineups)]
away_lineups$Pts_Scored[nrow(away_lineups)] <- away_score_final - away_lineups$Away_Score[nrow(away_lineups)]
away_lineups$Pts_Allowed[nrow(away_lineups)] <- home_score_final - away_lineups$Home_Score[nrow(away_lineups)]

# Remove lineups with no time played (double substitutions most likely)
away_lineups <- away_lineups[away_lineups$Time_Played > 0, ]

# Create column for the net points the lineup allowed
away_lineups$Net_Pts <- away_lineups$Pts_Scored - away_lineups$Pts_Allowed

# What to do next:
# - Find a way to determine who came into the game at the start of a new quarter!!!
# - Combine same lineups
# - Find a way to get possession counts (then calculate ORtg and DRtg)
# - Add position information
