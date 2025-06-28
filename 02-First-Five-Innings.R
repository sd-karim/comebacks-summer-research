library(tidyverse)
library(stringr)
library(readxl)

test_fifthinning <- read_excel("SUMMER25/sportsref_fifthinning01G.xlsx")

test_fifthinning$Date <- as.Date(as.numeric(test_fifthinning$Date), origin = "1899-12-30")
head(test_fifthinning) # fix date col

# select relevant columns
test_fifthinning <- test_fifthinning |>
  select(Rk, Team, Date, "Thru Inn.", Diff, ...8, Opp, Result, RS, RA) |>
  rename(Inning = "Thru Inn.", Scored = RS, Allowed = RA, At = ...8, DownTeam = Team, UpTeam = Opp)

# fix NA dates - all are doubleheader games, so in stathead date is listed as "2025-05-08(1)"
test_fifthinning |>
  filter(is.na(Date))

test_fifthinning[60, 3] <- as.Date("2025-05-09") 
test_fifthinning[61, 3] <- as.Date("2025-05-08")
test_fifthinning[74, 3] <- as.Date("2025-04-30") 
test_fifthinning[82, 3] <- as.Date("2025-04-27") 
test_fifthinning[93, 3] <- as.Date("2025-04-24") 
test_fifthinning[124, 3] <- as.Date("2025-04-06") 
test_fifthinning[158, 3] <- as.Date("2025-09-22") 

# will probably use stringr to remove the (1) from end of match for the rest of the innings?

# make home and away columns
test_fifthinning <- test_fifthinning |>
  mutate(Home = ifelse(is.na(At), DownTeam, UpTeam), 
         Away = ifelse(is.na(At), UpTeam, DownTeam))

# change up and down teams to be home/away, add reversal column
test_fifthinning <- test_fifthinning |>
  mutate(DownTeam = ifelse(DownTeam == Home, "Home", "Away"), 
         UpTeam = ifelse(UpTeam == Home, "Home", "Away")) |>
  mutate(Reversal = ifelse(str_detect(Result, "^W"), "Yes", "No"))



## upload full fifth inning data and repeat these steps: -------------------------------------------------

fifthinning <- read_excel("SUMMER25/sportsref_fifthinning01G.xlsx", sheet = "fifthinning ALL", 
                          col_types = c("numeric", "text", "date", "numeric", "numeric", 
                          "numeric", "numeric", "text", "text", "text", "numeric"))

# rename relevant columns
fifthinning <- fifthinning |>
  rename(Inning = "Thru Inn.", Scored = RS, Allowed = RA, At = ...8, DownTeam = Team, UpTeam = Opp) |>
  mutate(Date = as.Date(Date))

# make home and away columns
fifthinning <- fifthinning |>
  mutate(Home = ifelse(is.na(At), DownTeam, UpTeam), 
         Away = ifelse(is.na(At), UpTeam, DownTeam))

# change up and down teams to be home/away, add reversal column
fifthinning <- fifthinning |>
  mutate(DownTeam = ifelse(DownTeam == Home, "Home", "Away"), 
         UpTeam = ifelse(UpTeam == Home, "Home", "Away")) |>
  mutate(Reversal = ifelse(str_detect(Result, "^W"), TRUE, FALSE))

# add win proportions/difference
fifthinning <- fifthinning |>
  mutate(DownWin = ifelse(DownTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))), 
         UpWin = ifelse(UpTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date)))
         )

winpropall <- read.csv("~/DWD2/SUMMER25/mlb win prop.csv")

fifthinning <- fifthinning |>
  mutate(DownWin = winpropall$WinProp[match(DownWin, winpropall$TeamCode)],
    UpWin = winpropall$WinProp[match(UpWin, winpropall$TeamCode)], 
    WinDiff = (DownWin - UpWin)) 

# add game ID for later 
fifthinning <- fifthinning |>
  mutate(gameID = ifelse(is.na(Doubleheader), paste0(Home, Away, Date), paste0(Home, Away, Date, "-", Doubleheader))) 


# fourth inning: ------------------------------------------------------------------------------------------

fourthinning <- read_excel("SUMMER25/sportsref_fourthinning.xlsx", col_types = c("numeric", "text", "date", 
                                              "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "numeric"))

# select relevant columns
fourthinning <- fourthinning |>
  rename(Inning = "Thru Inn.", Scored = RS, Allowed = RA, At = ...8, DownTeam = Team, UpTeam = Opp) |>
  mutate(Date = as.Date(Date))

# NA dates were taken care of in google sheets for simplicity, couldn't use stringr like I thought I could

# make home and away columns
fourthinning <- fourthinning |>
  mutate(Home = ifelse(is.na(At), DownTeam, UpTeam), 
         Away = ifelse(is.na(At), UpTeam, DownTeam))

# change up and down teams to be home/away, add reversal column
fourthinning <- fourthinning |>
  mutate(DownTeam = ifelse(DownTeam == Home, "Home", "Away"), 
         UpTeam = ifelse(UpTeam == Home, "Home", "Away")) |>
  mutate(Reversal = ifelse(str_detect(Result, "^W"), TRUE, FALSE))

# add win proportions/difference
fourthinning <- fourthinning |>
  mutate(DownWin = ifelse(DownTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))), 
         UpWin = ifelse(UpTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))))

fourthinning <- fourthinning |>
  mutate(DownWin = winpropall$WinProp[match(DownWin, winpropall$TeamCode)],
         UpWin = winpropall$WinProp[match(UpWin, winpropall$TeamCode)], 
         WinDiff = (DownWin - UpWin)) 

# add game ID for later 
fourthinning <- fourthinning |>
  mutate(gameID = ifelse(is.na(Doubleheader), paste0(Home, Away, Date), paste0(Home, Away, Date, "-", Doubleheader))) 

# third inning: ------------------------------------------------------------------------------------------

thirdinning <- read_excel("SUMMER25/sportsref_thirdinning.xlsx", col_types = c("numeric", "text", "date", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "text", "text", "text", "numeric"))

# select relevant columns
thirdinning <- thirdinning |>
  rename(Inning = "Thru Inn.", Scored = RS, Allowed = RA, At = ...8, DownTeam = Team, UpTeam = Opp) |>
  mutate(Date = as.Date(Date))

# make home and away columns
thirdinning <- thirdinning |>
  mutate(Home = ifelse(is.na(At), DownTeam, UpTeam), 
         Away = ifelse(is.na(At), UpTeam, DownTeam))

# change up and down teams to be home/away, add reversal column
thirdinning <- thirdinning |>
  mutate(DownTeam = ifelse(DownTeam == Home, "Home", "Away"), 
         UpTeam = ifelse(UpTeam == Home, "Home", "Away")) |>
  mutate(Reversal = ifelse(str_detect(Result, "^W"), TRUE, FALSE))

# add win proportions/difference
thirdinning <- thirdinning |>
  mutate(DownWin = ifelse(DownTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))), 
         UpWin = ifelse(UpTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))))

thirdinning <- thirdinning |>
  mutate(DownWin = winpropall$WinProp[match(DownWin, winpropall$TeamCode)],
         UpWin = winpropall$WinProp[match(UpWin, winpropall$TeamCode)], 
         WinDiff = (DownWin - UpWin)) 

# add game ID for later 
thirdinning <- thirdinning |>
  mutate(gameID = ifelse(is.na(Doubleheader), paste0(Home, Away, Date), paste0(Home, Away, Date, "-", Doubleheader))) 

# second inning: ------------------------------------------------------------------------------------------

secondinning <- read_excel("SUMMER25/sportsref_secondinning.xlsx", col_types = c("numeric", "text", "date", 
                                               "numeric", "numeric", "numeric", "numeric", "text", "text", "text", 
                                               "numeric"))
# select relevant columns
secondinning <- secondinning |>
  rename(Inning = "Thru Inn.", Scored = RS, Allowed = RA, At = ...8, DownTeam = Team, UpTeam = Opp) |>
  mutate(Date = as.Date(Date))

# make home and away columns
secondinning <- secondinning |>
  mutate(Home = ifelse(is.na(At), DownTeam, UpTeam), 
         Away = ifelse(is.na(At), UpTeam, DownTeam))

# change up and down teams to be home/away, add reversal column
secondinning <- secondinning |>
  mutate(DownTeam = ifelse(DownTeam == Home, "Home", "Away"), 
         UpTeam = ifelse(UpTeam == Home, "Home", "Away")) |>
  mutate(Reversal = ifelse(str_detect(Result, "^W"), TRUE, FALSE))

# add win proportions/difference
secondinning <- secondinning |>
  mutate(DownWin = ifelse(DownTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))), 
         UpWin = ifelse(UpTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))))

secondinning <- secondinning |>
  mutate(DownWin = winpropall$WinProp[match(DownWin, winpropall$TeamCode)],
         UpWin = winpropall$WinProp[match(UpWin, winpropall$TeamCode)], 
         WinDiff = (DownWin - UpWin)) 

# add game ID for later 
secondinning <- secondinning |>
  mutate(gameID = ifelse(is.na(Doubleheader), paste0(Home, Away, Date), paste0(Home, Away, Date, "-", Doubleheader))) 

# first inning: ------------------------------------------------------------------------------------------

firstinning <- read_excel("SUMMER25/sportsref_firstinning.xlsx", col_types = c("numeric", "text", "date", 
                                              "numeric", "numeric", "numeric", "numeric", "text", "text", "text", 
                                              "numeric"))

# select relevant columns
firstinning <- firstinning |>
  rename(Inning = "Thru Inn.", Scored = RS, Allowed = RA, At = ...8, DownTeam = Team, UpTeam = Opp) |>
  mutate(Date = as.Date(Date))

# make home and away columns
firstinning <- firstinning |>
  mutate(Home = ifelse(is.na(At), DownTeam, UpTeam), 
         Away = ifelse(is.na(At), UpTeam, DownTeam))

# change up and down teams to be home/away, add reversal column
firstinning <- firstinning |>
  mutate(DownTeam = ifelse(DownTeam == Home, "Home", "Away"), 
         UpTeam = ifelse(UpTeam == Home, "Home", "Away")) |>
  mutate(Reversal = ifelse(str_detect(Result, "^W"), TRUE, FALSE))

# add win proportions/difference
firstinning <- firstinning |>
  mutate(DownWin = ifelse(DownTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))), 
         UpWin = ifelse(UpTeam == "Home", paste0(Home, year(Date)), paste0(Away, year(Date))))

firstinning <- firstinning |>
  mutate(DownWin = winpropall$WinProp[match(DownWin, winpropall$TeamCode)],
         UpWin = winpropall$WinProp[match(UpWin, winpropall$TeamCode)], 
         WinDiff = (DownWin - UpWin)) 

# add game ID for later 
firstinning <- firstinning |>
  mutate(gameID = ifelse(is.na(Doubleheader), paste0(Home, Away, Date), paste0(Home, Away, Date, "-", Doubleheader))) 


# find all games that have deficit in multiple innings (used chatgpt) -------------------------------------

game_ids <- table(c(firstinning$gameID, secondinning$gameID, thirdinning$gameID, fourthinning$gameID, fifthinning$gameID))
duplicateID <- names(game_ids[game_ids >1])

allinnings <- list(firstinning, secondinning, thirdinning, fourthinning, fifthinning)

matched_rows <- lapply(allinnings, function(df) {
  df %>% filter(gameID %in% duplicateID)
})

result_df <- bind_rows(matched_rows) # 2077 rows

# narrow down to games with largest deficit

largest_def <- result_df %>%
  group_by(gameID) %>%
  arrange(Diff, desc(Inning), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup()

# create copies of each inning df so that only largest deficit inning is kept for that game

# Create lookup table of the "best" duplicated gameID + Inning pairs
best_dupes <- largest_def %>% select(gameID, Inning)

# For each inning dataframe, remove bad duplicates and keep only correct matches
filtered_innings <- lapply(allinnings, function(df) {
  df %>%
    filter(
      !(gameID %in% duplicateID) | 
        paste(gameID, Inning) %in% paste(best_dupes$gameID, best_dupes$Inning)
    )
})

flt_firstinning  <- filtered_innings[[1]]
flt_secondinning <- filtered_innings[[2]]
flt_thirdinning  <- filtered_innings[[3]]
flt_fourthinning <- filtered_innings[[4]]
flt_fifthinging  <- filtered_innings[[5]]

# logistic regression ------------------------------------------------------------------------

full_innings <- bind_rows(allinnings) # 5145 rows, 44.6% home team is down

flt_innings <- bind_rows(filtered_innings) # 2352 rows, 44.5% home team is down

# response variable: reversal
# explanatory variables: DownTeam, Inning*, Diff, WinDiff
# make downteam binary

full_innings <- full_innings |>
  mutate(DownTeamBin = recode(DownTeam, Away = 0, Home = 1))

flt_innings <- flt_innings |>
  mutate(DownTeamBin = recode(DownTeam, Away = 0, Home = 1))

cor(full_innings[, c("DownTeamBin", "Inning", "Diff", "WinDiff")])

cor(flt_innings[, c("DownTeamBin", "Inning", "Diff", "WinDiff")])

# no concerning correlation for either version, no colinearity 
# fit glm model

model_full <- glm(Reversal ~ DownTeamBin + Inning + Diff + WinDiff, data = full_innings, family = "binomial")


model_flt <- glm(Reversal ~ DownTeamBin + Inning + Diff + WinDiff, data = flt_innings, family = "binomial")

summary(model_full)

summary(model_flt)

summary(glm(Reversal ~ DownTeamBin, data = full_innings)) # confirming that any significance of this isn't being hidden by other, more significant variables
summary(glm(Reversal ~ DownTeamBin, data = flt_innings))

# remove games that ended in fewer than 9 innings

flt_innings <- flt_innings |>
  filter(!(str_detect(Result, "7\\)$") | str_detect(Result, "8\\)$") | str_detect(Result, "6\\)$") | str_detect(Result, "5\\)$")))

# make a windiff category to look at proportions of specific scenarios

flt_innings <- flt_innings |>
  mutate(WinDiffCat = case_when(
    WinDiff < -0.118 ~ "Strong Disadvantage",
    WinDiff >= -0.118 & WinDiff < -0.043 ~ "Moderate Disadvantage",
    WinDiff >= -0.043 & WinDiff < 0.037 ~ "Balanced",
    WinDiff >= 0.037 ~ "Advantage"
  ))

SDHOME_innings <- flt_innings |>
  filter(WinDiffCat == "Strong Disadvantage" & DownTeam == "Home")

SDAWAY_innings <- flt_innings |>
  filter(WinDiffCat == "Strong Disadvantage" & DownTeam == "Away")

MDHOME_innings <- flt_innings |>
  filter(WinDiffCat == "Moderate Disadvantage" & DownTeam == "Home")

MDAWAY_innings <- flt_innings |>
  filter(WinDiffCat == "Moderate Disadvantage" & DownTeam == "Away")

BALHOME_innings <- flt_innings |>
  filter(WinDiffCat == "Balanced" & DownTeam == "Home")

BALAWAY_innings <- flt_innings |>
  filter(WinDiffCat == "Balanced" & DownTeam == "Away")

ADVHOME_innings <- flt_innings |>
  filter(WinDiffCat == "Advantage" & DownTeam == "Home")

ADVAWAY_innings <- flt_innings |>
  filter(WinDiffCat == "Advantage" & DownTeam == "Away")

home_down <- flt_innings |> filter(DownTeam == "Home") 
away_down <- flt_innings |> filter(DownTeam == "Away")
