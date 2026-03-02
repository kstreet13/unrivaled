
# parameters I want for model
# - distribution of lengths for possessions
# probability of:
# - miss/block (-> rebound)
# - turnover/off.foul (-> other team)
# - make (2/3/4 points and -> other team)
# - defensive/offensive rebound

source('~/funstuff/unrivaled/code/pbp/utils.R')

files <- list.files('../unriv_pbpData', recursive = TRUE, full.names = TRUE)

getModelParams <- function(files){
    reboundStats <- t(sapply(files, function(f){
        #print(f)
        plays <- readRDS(f)
        defReb <- length(grep('defensive rebound', plays$play))
        offReb <- length(grep('offensive rebound', plays$play))
        return(c(defReb = defReb, offReb = offReb))
    }))
    ###
    rebProbs <- colSums(reboundStats) / sum(reboundStats)
    ###
    rm(reboundStats)
    
    allPossData <- lapply(files, function(f){
        print(f)
        plays <- readRDS(f)
        players <- IDplayers(plays)
        plays <- possBeforeAfter(plays, players)
        poss <- getPossessions(plays, players)
        poss <- possessionData(poss)
        return(poss)
    })
    allPossData <- do.call(rbind, allPossData)
    
    table(allPossData$points, allPossData$end)
    # how do we have (valid) possessions ending in a make for 1 point?
    
    # some clear path fouls are apparently just labelled as common fouls
    # there's a really weird one where the inserted a rebound for the shooting team between two technical FTs
    # just remove it
    allPossData <- allPossData[-which(allPossData$points == 1 &
                                          allPossData$end == 'make'), ]
    table(allPossData$points, allPossData$end)
    
    # four implausibly long possessions (420 105? 48? 34?)
    allPossData <- allPossData[which(allPossData$length < 30), ]
    
    lengths <- table(allPossData$length)
    
    possProbs <- table(allPossData$end)
    possProbs <- possProbs[-which(names(possProbs) == 'stoppage')]
    possProbs <- possProbs / sum(possProbs)
    
    makePoints <- table(allPossData$points[which(allPossData$end == 'make')])
    makePoints <- makePoints / sum(makePoints)
    missPoints <- table(allPossData$points[which(allPossData$end == 'miss')])
    missPoints <- missPoints / sum(missPoints)
    
    return(list(lengths = lengths,
                outcome = possProbs,
                makePoints = makePoints,
                missPoints = missPoints,
                reb = rebProbs))
}

params <- getModelParams(files)










