# play-by-play data
source('~/funstuff/unrivaled/code/pbp/utils.R')

# plays <- getPlays('https://www.unrivaled.basketball/game/24w1j54rlgk9/play-by-play')
plays <- readRDS('../unriv_pbpData/2025/zvudhzbxaaiq.rds')

players <- IDplayers(plays)

plays <- determinePossession(plays, players)

ps <- possessionStats(plays)









