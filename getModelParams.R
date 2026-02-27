
# parameters I want for model
# - distribution of lengths for possessions
# probability of:
# - miss/block (-> rebound)
# - turnover/off.foul (-> other team)
# - make (2/3/4 points and -> other team)
# - defensive/offensive rebound

source('~/funstuff/unrivaled/code/pbp/utils.R')

files <- list.files('../unriv_pbpData', recursive = TRUE, full.names = TRUE)

reboundStats <- t(sapply(files, function(f){
    #print(f)
    plays <- readRDS(f)
    defReb <- length(grep('defensive rebound', plays$play))
    offReb <- length(grep('offensive rebound', plays$play))
    return(c(defReb = defReb, offReb = offReb))
}))
###
offRebRate <- sum(reboundStats[,'offReb']) / sum(reboundStats)
defRebRate <- sum(reboundStats[,'defReb']) / sum(reboundStats)
###
rm(rebound)

allPossData <- lapply(files, function(f){
    print(f)

    plays <- readRDS(f)
    
    players <- IDplayers(plays)
    
    plays <- possBeforeAfter(plays, players)
    
    poss <- getPossessions(plays, players)
    
    poss <- possessionData(poss)
    
    if(any(poss$points == 1 & poss$end == 'make')){
        stop('found it')
    }
    
    return(poss)
})
allPossData <- do.call(rbind, allPossData)

table(allPossData$points, allPossData$end)
# how do we have (valid) possessions ending in a make for 1 point?

# some clear path fouls are apparently just labelled as common fouls


