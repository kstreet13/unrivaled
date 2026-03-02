# play-by-play data
source('~/funstuff/unrivaled/code/pbp/utils.R')

# plays <- getPlays('https://www.unrivaled.basketball/game/24w1j54rlgk9/play-by-play')
plays <- readRDS('../unriv_pbpData/2025/xagwq6rm8vw7.rds')

players <- IDplayers(plays)

plays <- possBeforeAfter(plays, players)

poss <- getPossessions(plays, players)

poss <- possessionData(poss)









