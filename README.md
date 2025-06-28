# Comebacks in Sports: Baseball and the Home Team Advantage 

## Overview
This repository contains the process code and results of my summer research as a part of a sports psychology study on comebacks in sports. I chose to focus on home team advantage, because in the literature review part of this study, I read that the home team advantage doesn't have as great of an effect on baseball as it does other team sports, due to baseball being made up of more individual actions than those sports. The goal of this project was to examine if a home team advantage could be observed in a sample of games, in terms of making comebacks (ie, are home teams less likely to be in a position to need to make the comeback? when in that position, do they complete the comeback more frequently than the away team?). 

Project uses Google Sheets and R (dplyr, tidyr, lubridate and stringr packages specfically. Rselenium, rvest, wdman attempted but not used in final product)

## Data Collection, Wrangling
The original plan for data collection was to collect straight from the MLB website via webscraping, and just filter out the games that didn't meet the criteria for a possible comeback (in this case we're looking for games where a team was down at least five runs at some point in the first five innings). I was unable to scrape the data in a way that would be useful for this project *(for code attempts see 01-Webscraping-Attempt.R)*, and was recommended to download data from [stathead](https://stathead.com/baseball/team-batting-game-finder.cgi?request=1&order_by=date&timeframe=seasons&year_min=2020&year_max=2025&score_since=thru&score_period=5&score_margin_comp=lt&score_margin_val=-5)
 instead. 

 To get the data from stathead into R, I opened each download in Google Sheets as a way to easily compile all rows into one sheet per inning, and then imported each inning sheet into R. From there each dataframe got home/away indicator columns, reversal indicator columns, win proportion difference columns, and a gameID column. Once each inning was cleaned and formatted, I found all matches that were represented across multiple innings, and kept only the inning with the largest deficit for each game. *(full code in 02-First-Five-Innings.R)*

 The second sample of data I needed to collect was for games in the last five seasons where a team was down at least three runs in innings 1-3, and then tied the game in innings 4-6. The steps for cleaning each set of three innings is the same as the First Five Innings data, but after removing duplicate recordings of the same game in each set of three, I inner joined the two sets of innings to keep only games where the team was both down three in the first set and tied in the second set. *(full code in 03-Down-The-Tied.R)*

## Statistical Analysis
*(starting at line 268 in 02-First-Five-Innings.R or line 324 in 03-Down-The-Tied.R, continued in 04-Results.Rmd)*

Fit a logistic regression model to find which explanatory variables were statistically significant in whether a team won (completed the comeback) or lost. *(starting at line 268 in 02-First-Five-Innings.R or line 324 in 03-Down-The-Tied.R)* I then took the statistically significant variables, and created tables the showed the number of occurances and number of wins that the comeback team had, to compare the rates that the comeback was made for home/away teams. There were some interesting observed differences, so I did pairwise comparison tests to see how significant the differences were, and after adjusting the p-values, found that none of the pairs of proportions were statistically significant. *(code and analysis in 04-Results.Rmd)*


