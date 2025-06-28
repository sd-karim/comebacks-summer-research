library(tidyverse)
library(stringr)
library(readxl)

winpropall <- read.csv("~/DWD2/SUMMER25/mlb win prop.csv")

# innings 1-3, team down 3 pts -------------------------------------------------

## inning 01

TTT_inning01 <- read_excel("SUMMER25/TTT_inning01.xlsx", col_types = c("numeric", "text", "date", "numeric", "numeric", 
                                               "numeric", "numeric", "text", "text", "text", 
                                               "numeric"))

# select relevant columns
TTT_inning01 <- TTT_inning01 |>
  rename(Inning = "Thru Inn.", Scored = RS, Allowed = RA, At = ...8, DownTeam = Team, UpTeam = Opp) |>
  mutate(Date = as.Date(Date))

# make home and away columns
TTT_inning01 <- TTT_inning01 |>
  mutate(Home = ifelse(is.na(At), DownTeam, UpTeam), 
         Away = ifelse(is.na(At), UpTeam, DownTeam))

# change up and down teams to be home/away, add reversal column
TTT_inning01 <- TTT_inning01 |>
  mutate(DownTeam = ifelse(DownTeam == Home, "Home", "Away"), 
         UpTeam = ifelse(UpTeam == Home, "Home", "Away")) |>
  mutate(Reversal = ifelse(str_detect(Result, "^W"), TRUE, FALSE))

# add win proportions/difference
TTT_inning01 <- TTT_inning01 |>
  mutate(DownWin = ifelse(DownTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))), 
         UpWin = ifelse(UpTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))))

TTT_inning01 <- TTT_inning01 |>
  mutate(DownWin = winpropall$WinProp[match(DownWin, winpropall$TeamCode)],
         UpWin = winpropall$WinProp[match(UpWin, winpropall$TeamCode)], 
         WinDiff = (DownWin - UpWin)) 

# add game ID for later 
TTT_inning01 <- TTT_inning01 |>
  mutate(gameID = ifelse(is.na(Doubleheader), paste0(Home, Away, Date), paste0(Home, Away, Date, "-", Doubleheader))) 

## inning 02

TTT_inning02 <- read_excel("SUMMER25/TTT_inning02.xlsx", col_types = c("numeric", "text", "date", 
                                               "numeric", "numeric", "numeric", "numeric", 
                                               "text", "text", "text", "numeric"))

# select relevant columns
TTT_inning02 <- TTT_inning02 |>
  rename(Inning = "Thru Inn.", Scored = RS, Allowed = RA, At = ...8, DownTeam = Team, UpTeam = Opp) |>
  mutate(Date = as.Date(Date))

# make home and away columns
TTT_inning02 <- TTT_inning02 |>
  mutate(Home = ifelse(is.na(At), DownTeam, UpTeam), 
         Away = ifelse(is.na(At), UpTeam, DownTeam))

# change up and down teams to be home/away, add reversal column
TTT_inning02 <- TTT_inning02 |>
  mutate(DownTeam = ifelse(DownTeam == Home, "Home", "Away"), 
         UpTeam = ifelse(UpTeam == Home, "Home", "Away")) |>
  mutate(Reversal = ifelse(str_detect(Result, "^W"), TRUE, FALSE))

# add win proportions/difference
TTT_inning02 <- TTT_inning02 |>
  mutate(DownWin = ifelse(DownTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))), 
         UpWin = ifelse(UpTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))))

TTT_inning02 <- TTT_inning02 |>
  mutate(DownWin = winpropall$WinProp[match(DownWin, winpropall$TeamCode)],
         UpWin = winpropall$WinProp[match(UpWin, winpropall$TeamCode)], 
         WinDiff = (DownWin - UpWin)) 

# add game ID for later 
TTT_inning02 <- TTT_inning02 |>
  mutate(gameID = ifelse(is.na(Doubleheader), paste0(Home, Away, Date), paste0(Home, Away, Date, "-", Doubleheader))) 

## inning 03

TTT_inning03 <- read_excel("SUMMER25/TTT_inning03.xlsx", col_types = c("numeric", "text", "date", 
                                               "numeric", "numeric", "numeric", "numeric", 
                                               "text", "text", "text", "numeric"))

# select relevant columns
TTT_inning03 <- TTT_inning03 |>
  rename(Inning = "Thru Inn.", Scored = RS, Allowed = RA, At = ...8, DownTeam = Team, UpTeam = Opp) |>
  mutate(Date = as.Date(Date))

# make home and away columns
TTT_inning03 <- TTT_inning03 |>
  mutate(Home = ifelse(is.na(At), DownTeam, UpTeam), 
         Away = ifelse(is.na(At), UpTeam, DownTeam))

# change up and down teams to be home/away, add reversal column
TTT_inning03 <- TTT_inning03 |>
  mutate(DownTeam = ifelse(DownTeam == Home, "Home", "Away"), 
         UpTeam = ifelse(UpTeam == Home, "Home", "Away")) |>
  mutate(Reversal = ifelse(str_detect(Result, "^W"), TRUE, FALSE))

# add win proportions/difference
TTT_inning03 <- TTT_inning03 |>
  mutate(DownWin = ifelse(DownTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))), 
         UpWin = ifelse(UpTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))))

TTT_inning03 <- TTT_inning03 |>
  mutate(DownWin = winpropall$WinProp[match(DownWin, winpropall$TeamCode)],
         UpWin = winpropall$WinProp[match(UpWin, winpropall$TeamCode)], 
         WinDiff = (DownWin - UpWin)) 

# add game ID for later 
TTT_inning03 <- TTT_inning03 |>
  mutate(gameID = ifelse(is.na(Doubleheader), paste0(Home, Away, Date), paste0(Home, Away, Date, "-", Doubleheader))) 

# get rid of doubles -----------------------------------------------------------

game_ids2 <- table(c(TTT_inning01$gameID, TTT_inning02$gameID, TTT_inning03$gameID))
duplicateID2 <- names(game_ids2[game_ids2 >1])

TTTdowninnings <- list(TTT_inning01, TTT_inning02, TTT_inning03)

matched_rows2 <- lapply(TTTdowninnings, function(df) {
  df %>% filter(gameID %in% duplicateID2)
})

result_df2 <- bind_rows(matched_rows2) 

largest_def2 <- result_df2 %>%
  group_by(gameID) %>%
  arrange(Diff, desc(Inning), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup()

# create copies of each inning df so that only largest deficit inning is kept for that game

# Create lookup table of the "best" duplicated gameID + Inning pairs
best_dupes2 <- largest_def2 %>% select(gameID, Inning)

# For each inning dataframe, remove bad duplicates and keep only correct matches
filtered_innings2 <- lapply(TTTdowninnings, function(df) {
  df %>%
    filter(
      !(gameID %in% duplicateID2) | 
        paste(gameID, Inning) %in% paste(best_dupes2$gameID, best_dupes2$Inning)
    )
})

flt_TTT_inning01  <- filtered_innings2[[1]]
flt_TTT_inning02 <- filtered_innings2[[2]]
flt_TTT_inning03  <- filtered_innings2[[3]]

TTT_inning123 <- bind_rows(filtered_innings2)


# innings 4-6, tied game -------------------------------------------------

## inning 04

TTT_inning04 <- read_excel("SUMMER25/TTT_inning04.xlsx", col_types = c("numeric", "text", "date", "numeric", "numeric", 
                                                                       "numeric", "numeric", "text", "text", "text", 
                                                                       "numeric"))

# select relevant columns
TTT_inning04 <- TTT_inning04 |>
  rename(Inning = "Thru Inn.", Scored = RS, Allowed = RA, At = ...8, DownTeam = Team, UpTeam = Opp) |>
  mutate(Date = as.Date(Date))

# make home and away columns
TTT_inning04 <- TTT_inning04 |>
  mutate(Home = ifelse(is.na(At), DownTeam, UpTeam), 
         Away = ifelse(is.na(At), UpTeam, DownTeam))

# change up and down teams to be home/away, add reversal column
TTT_inning04 <- TTT_inning04 |>
  mutate(DownTeam = ifelse(DownTeam == Home, "Home", "Away"), 
         UpTeam = ifelse(UpTeam == Home, "Home", "Away")) |>
  mutate(Reversal = ifelse(str_detect(Result, "^W"), TRUE, FALSE))

# add win proportions/difference
TTT_inning04 <- TTT_inning04 |>
  mutate(DownWin = ifelse(DownTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))), 
         UpWin = ifelse(UpTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))))

TTT_inning04 <- TTT_inning04 |>
  mutate(DownWin = winpropall$WinProp[match(DownWin, winpropall$TeamCode)],
         UpWin = winpropall$WinProp[match(UpWin, winpropall$TeamCode)], 
         WinDiff = (DownWin - UpWin)) 

# add game ID for later 
TTT_inning04 <- TTT_inning04 |>
  mutate(gameID = ifelse(is.na(Doubleheader), paste0(Home, Away, Date), paste0(Home, Away, Date, "-", Doubleheader))) 


## inning 05

TTT_inning05 <- read_excel("SUMMER25/TTT_inning05.xlsx", col_types = c("numeric", "text", "date", "numeric", "numeric", 
                                                                       "numeric", "numeric", "text", "text", "text", 
                                                                       "numeric"))

# select relevant columns
TTT_inning05 <- TTT_inning05 |>
  rename(Inning = "Thru Inn.", Scored = RS, Allowed = RA, At = ...8, DownTeam = Team, UpTeam = Opp) |>
  mutate(Date = as.Date(Date))

# make home and away columns
TTT_inning05 <- TTT_inning05 |>
  mutate(Home = ifelse(is.na(At), DownTeam, UpTeam), 
         Away = ifelse(is.na(At), UpTeam, DownTeam))

# change up and down teams to be home/away, add reversal column
TTT_inning05 <- TTT_inning05 |>
  mutate(DownTeam = ifelse(DownTeam == Home, "Home", "Away"), 
         UpTeam = ifelse(UpTeam == Home, "Home", "Away")) |>
  mutate(Reversal = ifelse(str_detect(Result, "^W"), TRUE, FALSE))

# add win proportions/difference
TTT_inning05 <- TTT_inning05 |>
  mutate(DownWin = ifelse(DownTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))), 
         UpWin = ifelse(UpTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))))

TTT_inning05 <- TTT_inning05 |>
  mutate(DownWin = winpropall$WinProp[match(DownWin, winpropall$TeamCode)],
         UpWin = winpropall$WinProp[match(UpWin, winpropall$TeamCode)], 
         WinDiff = (DownWin - UpWin)) 

# add game ID for later 
TTT_inning05 <- TTT_inning05 |>
  mutate(gameID = ifelse(is.na(Doubleheader), paste0(Home, Away, Date), paste0(Home, Away, Date, "-", Doubleheader))) 

## inning 06

TTT_inning06 <- read_excel("SUMMER25/TTT_inning06.xlsx", col_types = c("numeric", "text", "date", "numeric", "numeric", 
                                                                       "numeric", "numeric", "text", "text", "text", 
                                                                       "numeric"))

# select relevant columns
TTT_inning06 <- TTT_inning06 |>
  rename(Inning = "Thru Inn.", Scored = RS, Allowed = RA, At = ...8, DownTeam = Team, UpTeam = Opp) |>
  mutate(Date = as.Date(Date))

# make home and away columns
TTT_inning06 <- TTT_inning06 |>
  mutate(Home = ifelse(is.na(At), DownTeam, UpTeam), 
         Away = ifelse(is.na(At), UpTeam, DownTeam))

# change up and down teams to be home/away, add reversal column
TTT_inning06 <- TTT_inning06 |>
  mutate(DownTeam = ifelse(DownTeam == Home, "Home", "Away"), 
         UpTeam = ifelse(UpTeam == Home, "Home", "Away")) |>
  mutate(Reversal = ifelse(str_detect(Result, "^W"), TRUE, FALSE))

# add win proportions/difference
TTT_inning06 <- TTT_inning06 |>
  mutate(DownWin = ifelse(DownTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))), 
         UpWin = ifelse(UpTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))))

TTT_inning06 <- TTT_inning06 |>
  mutate(DownWin = winpropall$WinProp[match(DownWin, winpropall$TeamCode)],
         UpWin = winpropall$WinProp[match(UpWin, winpropall$TeamCode)], 
         WinDiff = (DownWin - UpWin)) 

# add game ID for later 
TTT_inning06 <- TTT_inning06 |>
  mutate(gameID = ifelse(is.na(Doubleheader), paste0(Home, Away, Date), paste0(Home, Away, Date, "-", Doubleheader)))

# get rid of doubles -----------------------------------------------------------

game_ids3 <- table(c(TTT_inning04$gameID, TTT_inning05$gameID, TTT_inning06$gameID))
duplicateID3 <- names(game_ids3[game_ids3 >1])

TTTtieinnings <- list(TTT_inning04, TTT_inning05, TTT_inning06)

matched_rows3 <- lapply(TTTtieinnings, function(df) {
  df %>% filter(gameID %in% duplicateID3)
})

result_df3 <- bind_rows(matched_rows3) 

largest_def3 <- result_df3 %>%
  group_by(gameID) %>%
  arrange(desc(Inning), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup()

# create copies of each inning df so that only largest deficit inning is kept for that game

# Create lookup table of the "best" duplicated gameID + Inning pairs
best_dupes3 <- largest_def3 %>% select(gameID, Inning)

# For each inning dataframe, remove bad duplicates and keep only correct matches
filtered_innings3 <- lapply(TTTtieinnings, function(df) {
  df %>%
    filter(
      !(gameID %in% duplicateID3) | 
        paste(gameID, Inning) %in% paste(best_dupes3$gameID, best_dupes3$Inning)
    )
})

flt_TTT_inning04  <- filtered_innings3[[1]]
flt_TTT_inning05 <- filtered_innings3[[2]]
flt_TTT_inning06  <- filtered_innings3[[3]]

TTT_inning456 <- bind_rows(filtered_innings3)


# inner join innings 123 with 456 -----------------------------------------------

TTT_inning456 <- TTT_inning456 |>
  select(DownTeam, Inning, gameID) |>
  rename(TiedIn = Inning)

TTT_inning123 <- TTT_inning123 |>
  rename(MaxDef = Diff, MaxDefIn = Inning)

TTT_sixinnings <- inner_join(TTT_inning123, TTT_inning456, by = c("DownTeam", "gameID")) # 272 rows

TTT_sixinnings <- TTT_sixinnings |>
  filter(!(str_detect(Result, "7\\)$"))) # to get rid of games that only had 7 innings

# 265 rows

# logistic regression ------------------------------------------------------------

TTT_sixinnings <- TTT_sixinnings |>
  mutate(DownTeamBin = recode(DownTeam, Away = 0, Home = 1))

cor(TTT_sixinnings[, c("DownTeamBin", "MaxDefIn", "MaxDef", "TiedIn", "WinDiff")])

model_TTT <- glm(Reversal ~ DownTeamBin + MaxDefIn + MaxDef + TiedIn + WinDiff, data = TTT_sixinnings, family = "binomial")

summary(model_TTT)


# add windiff categories

TTT_sixinnings <- TTT_sixinnings |>
  mutate(WinDiffCat = case_when(
    WinDiff < -0.084 ~ "Disadvantage",
    WinDiff >= -0.084 & WinDiff < 0.012 ~ "Balanced",
    WinDiff >= 0.012 & WinDiff < 0.093 ~ "Advantage",
    WinDiff >= 0.093 ~ "Strong Advantage"
  ))

HOME_TTT <- TTT_sixinnings |>
  filter(DownTeam == "Home")

AWAY_TTT <- TTT_sixinnings |>
  filter(DownTeam == "Away")