# get all 2025 play-by-play data

require(rvest)
p <- read_html('https://www.unrivaled.basketball/schedule?games=All&season=2025')
t <- as.character(p)
t <- unlist(strsplit(t, split='\n'))
t <- grep('/game/[a-z0-9]*/box-score', t, value = TRUE)
t <- t[-grep('footer', t)]
gameIDs <- gsub('^.*/game/','', t)
gameIDs <- gsub('/box-score.*$','', gameIDs)


for(gi in gameIDs){
    plays <- getPlays(paste0('https://www.unrivaled.basketball/game/',gi,'/play-by-play'))
    saveRDS(plays, file = paste0('../unriv_pbpData/2025/',gi,'.rds'))
}



