
# from gpt, prompt: https://www.mlb.com/scores/2025-06-08 using rvest and rselenium ONLY IF NEEDED, write r code to webscrape box scores into either a google sheet or into r directly
library(RSelenium)
library(rvest)
library(dplyr)
library(lubridate)
library(wdman)

# Start Selenium with Firefox
selServ <- selenium(retcommand = TRUE, browser = "firefox")
system(selServ, wait = FALSE)
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444L, browserName = "firefox")
remDr$open()



# Navigate to the MLB scores page
remDr$navigate("https://www.mlb.com/scores/2025-06-08")
Sys.sleep(5)  # wait for JS to load

# Get the full page source after JS renders
page_source <- remDr$getPageSource()[[1]]
page <- read_html(page_source)

# Example: Extract team names and scores (you may need to adapt this based on actual structure)
games <- page %>% html_nodes(".scoring-summary")  # check structure with html_nodes("*")

game_data <- lapply(games, function(g) {
  teams <- g %>% html_nodes(".team-name") %>% html_text(trim = TRUE)
  scores <- g %>% html_nodes(".total") %>% html_text(trim = TRUE) %>% as.numeric()
  
  if (length(teams) == 2 && length(scores) == 2) {
    data.frame(
      Date = as.Date("2025-06-08"),
      Home_Team = teams[2],
      Away_Team = teams[1],
      Home_Score = scores[2],
      Away_Score = scores[1],
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }
})

df <- bind_rows(game_data)
print(df)

# Cleanup
remDr$close()
rD$server$stop()


library(httr)
library(jsonlite)
library(dplyr)

url <- "https://statsapi.mlb.com/api/v1/schedule?sportId=1&date=2025-06-08&hydrate=team,linescore"

res <- GET(url)

if (status_code(res) != 200) stop("Failed to get data")

json_data <- content(res, as = "text", encoding = "UTF-8")
data <- fromJSON(json_data, flatten = TRUE)

games <- data$dates$games[[1]]

# Now extract fields carefully
box_scores <- games %>%
  transmute(
    gamePk,
    gameDate = substr(gameDate, 1, 10),
    status = status.detailedState,
    away_team = teams.away.team.name,
    away_score = teams.away.score,
    home_team = teams.home.team.name,
    home_score = teams.home.score,
    venue = venue.name
  )

print(box_scores) # gets final scores but not box scores






# try again for box scores

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

url <- "https://statsapi.mlb.com/api/v1/schedule?sportId=1&date=2025-06-08&hydrate=team,linescore"

res <- GET(url)
stopifnot(status_code(res) == 200)

json_data <- content(res, as = "text", encoding = "UTF-8")
data <- fromJSON(json_data, flatten = TRUE)

games_df <- data$dates$games[[1]]

# Convert each row to a list (to access nested lists properly)
games_list <- split(games_df, seq(nrow(games_df)))

box_scores_innings <- lapply(games_list, function(game) {
  game <- as.list(game)
  innings <- game$linescore.innings
  
  if(is.null(innings) || length(innings) == 0) return(NULL)
  
  inning_scores <- tibble(
    inning = sapply(innings, function(x) x$num),
    away_runs = sapply(innings, function(x) ifelse(is.null(x$away), NA, x$away)),
    home_runs = sapply(innings, function(x) ifelse(is.null(x$home), NA, x$home))
  )
  
  inning_scores$gamePk <- game$gamePk
  inning_scores$away_team <- game$teams.away.team.name
  inning_scores$home_team <- game$teams.home.team.name
  inning_scores$venue <- game$venue.name
  inning_scores$status <- game$status.detailedState
  
  inning_scores
})


box_scores_innings <- do.call(rbind, box_scores_innings)

print(box_scores_innings) # away innings and home innings are null






# Inspect first game's innings data
first_game <- games_list[[1]]
first_game_innings <- first_game$linescore.innings

print(first_game_innings)

# check structure of first_game_innings

if (!is.null(first_game_innings) && length(first_game_innings) > 0) {
  print(first_game_innings[[1]])
}

str(first_game_innings)

# fixed code to extract per-inning runs

# API request for June 8, 2025
url <- "https://statsapi.mlb.com/api/v1/schedule?sportId=1&date=2025-06-08&hydrate=team,linescore"
res <- GET(url)
stopifnot(status_code(res) == 200)

json_data <- content(res, as = "text", encoding = "UTF-8")
data <- fromJSON(json_data, flatten = TRUE)

games_df <- data$dates$games[[1]]
games_list <- split(games_df, seq(nrow(games_df)))

box_scores_innings <- lapply(games_list, function(game_row) {
  game <- as.list(game_row)
  innings_df <- game$linescore.innings
  
  # Skip if innings data is missing
  if (is.null(innings_df) || !is.data.frame(innings_df)) return(NULL)
  
  tibble(
    gamePk = game$gamePk,
    away_team = game$teams.away.team.name,
    home_team = game$teams.home.team.name,
    inning = innings_df$num,
    away_runs = innings_df$away.runs,
    home_runs = innings_df$home.runs,
    venue = game$venue.name,
    status = game$status.detailedState
  )
})

# Combine all games
box_scores_innings_df <- bind_rows(box_scores_innings)

print(box_scores_innings_df)

# check if its even able to pull innings data from this url

sum(sapply(games_list, function(game_row) {
  game <- as.list(game_row)
  !is.null(game$linescore.innings) && is.data.frame(game$linescore.innings)
}))




## try with hover? 

# Get all gamePk values from the date
schedule_url <- "https://statsapi.mlb.com/api/v1/schedule?sportId=1&date=2025-06-08"
sched_res <- GET(schedule_url)
sched_data <- fromJSON(content(sched_res, "text"), flatten = TRUE)

games <- sched_data$dates$games[[1]]

# Now for each gamePk, get the full linescore
linescores <- lapply(games$gamePk, function(pk) {
  linescore_url <- paste0("https://statsapi.mlb.com/api/v1/game/", pk, "/linescore")
  res <- GET(linescore_url)
  if (status_code(res) != 200) return(NULL)
  
  ls_data <- fromJSON(content(res, "text"), flatten = TRUE)
  
  # Make sure innings exist
  if (is.null(ls_data$innings) || length(ls_data$innings) == 0) return(NULL)
  
  tibble(
    gamePk = pk,
    inning = sapply(ls_data$innings, function(x) x$num),
    away_team = ls_data$teams$away$team$name,
    home_team = ls_data$teams$home$team$name,
    away_runs = sapply(ls_data$innings, function(x) ifelse(is.null(x$away$runs), NA, x$away$runs)),
    home_runs = sapply(ls_data$innings, function(x) ifelse(is.null(x$home$runs), NA, x$home$runs))
  )
})

# Combine
box_scores_df <- bind_rows(linescores)

print(box_scores_df)





## use individual game url


# Game ID for the Marlins vs. Rays game on June 8, 2025
game_id <- 777592

# Construct the URL to fetch the linescore data
url <- paste0("https://statsapi.mlb.com/api/v1/game/", game_id, "/linescore")

# Fetch and parse the JSON data
linescore_data <- fromJSON(url)

# Extract relevant information
date <- linescore_data$gameDate
away_team <- linescore_data$teams$away$team$name
home_team <- linescore_data$teams$home$team$name
innings <- linescore_data$innings
away_score <- linescore_data$teams$away$score
home_score <- linescore_data$teams$home$score

# Print the extracted information
cat("Date:", date, "\n")
cat("Away Team:", away_team, "\n")
cat("Home Team:", home_team, "\n")
cat("Away Score:", away_score, "\n")
cat("Home Score:", home_score, "\n")
cat("Inning-by-Inning Scores:\n")
for (i in seq_along(innings)) {
  cat("Inning", i, "- Away:", innings[[i]]$away$runs, "Home:", innings[[i]]$home$runs, "\n")
}






