getPlays <- function(url){
    require(rvest)
    p <- read_html(url)
    plays <- html_table(p)[[1]]
    names(plays) <- tolower(names(plays))
    plays$quarter <- substr(plays$play, 1,2)
    plays$play <- substr(plays$play, start = 3, stop = 999999)
    scores <- do.call(rbind, strsplit(plays$score, split='-'))
    plays$score <- NULL
    plays$score1 <- as.numeric(scores[,1])
    plays$score2 <- as.numeric(scores[,2])  
    plays <- plays[,c('time','quarter','play','score1','score2')]
    return(plays)
}

# bug: could be someone who started the game but never subbed in (eg. early injury)
IDplayers <- function(plays){
    # teams
    x <- plays$play[grep('lineup change', plays$play)]
    y <- gsub(' lineup change.+', '', x)
    teams <- unique(y)
    
    # players (ID players when they sub in)
    z <- gsub('.+lineup change [(]', '', x)
    z <- gsub('[)]', '', z)
    z <- strsplit(z, split=', ')
    names(z) <- y
    players <- unique(data.frame(team = rep(y, times=lengths(z)), player = unlist(z)))
    rm(x,y,z)
    
    # check for all players 
    require(stringr)
    allplayers <- unique(unlist(str_extract_all(plays$play, "([A-Z][A-Za-z-']+ ?){2,3}")))
    allplayers <- gsub(' $','', allplayers)
    allplayers <- unique(gsub("'s",'', allplayers))
    if(!all(allplayers %in% players$player)){
        stop(paste('Players not subbed in:', paste(allplayers[!allplayers %in% players$player], collapse = ', ')))
    }else{
        rm(allplayers)
    }
    
    # order teams to align with score
    team1ind <- which(diff(plays$score1) > 0) + 1
    count1 <- sapply(players$player, function(pl){
        length(grep(paste(pl,'makes'), plays$play[team1ind]))
    })
    team2ind <- which(diff(plays$score2) > 0) + 1
    count2 <- sapply(players$player, function(pl){
        length(grep(paste(pl,'makes'), plays$play[team2ind]))
    })
    if(any(count1 > 0 & count2 > 0)){
        stop(paste('Players scoring for both teams:',
                   paste(names(count1)[count1 > 0 & count2 > 0], collapse = ', ')))
    }
    team1 <- players$team[count1[players$player] > 0][1]
    team2 <- players$team[count2[players$player] > 0][1]
    players <- rbind(players[players$team == team1, ], 
                     players[players$team == team2, ])
    
    return(players)
}

# possBefore, possAfter
determinePossession <- function(plays, players){
    teams <- unique(players$team)
    
    # check for jump ball, split into two events (force tie-up / jump ball, gain possession)
    if(length(grep('Jump ball', plays$play)) > 0){
        for(i in 1:length(grep('Jump ball', plays$play))){
            idx <- grep('Jump ball', plays$play)[i]
            # duplicate the jump ball event
            plays <- rbind(plays[1:idx,], plays[idx:nrow(plays),])
            # make first event a "Tie up"
            plays$play[idx] <- gsub('Jump ball','Tie up', plays$play[idx])
            plays$play[idx] <- gsub(' [(].* gains possession[)]','', plays$play[idx])
        }
    }
    
    # determine possession (before and after play)
    plays$possAfter <- plays$possBefore <- NA
    
    # make version of each play with just team names, no player names
    plays$teamplay <- plays$play
    for(i in 1:nrow(players)){
        plays$teamplay <- gsub(players$player[i], players$team[i], plays$teamplay)
    }
    
    ### DETERMINE POSSESSION ###
    # (events sorted roughly by frequency)
    for(i in 1:nrow(plays)){
        if(grepl('rebound', plays$play[i])){
            team <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0(teams,' [a-z]* rebound'))))]
            plays$possBefore[i] <- 'unknown'
            plays$possAfter[i] <- team
            next
        }
        if(grepl('misses', plays$play[i])){
            team <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0(teams,' misses'))))]
            plays$possBefore[i] <- team
            plays$possAfter[i] <- 'unknown'
            next
        }
        if(grepl('makes', plays$play[i])){
            team <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0(teams,' makes'))))]
            otherteam <- teams[teams != team]
            plays$possBefore[i] <- team
            # check for and-1, flagrant/technical
            if(i < nrow(plays) && plays$time[i] == plays$time[i+1] &
               grepl(paste(otherteam,'shooting foul'), plays$teamplay[i+1])){
                plays$possAfter[i] <- team
            }else{
                plays$possAfter[i] <- otherteam
            }
            next
        }
        if(grepl('turnover', plays$play[i])){
            team <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0(teams,' turnover'))))]
            otherteam <- teams[teams != team]
            plays$possBefore[i] <- team
            plays$possAfter[i] <- otherteam
            next
        }
        if(grepl('lineup change', plays$play[i]) ||
           grepl('timeout', plays$play[i]) ||
           grepl('ejected', plays$play[i])){
            # non-basketball plays, possession doesn't change
            plays$possBefore[i] <- plays$possAfter[i] <- plays$possAfter[i-1]
            next
        }
        if(grepl('personal foul', plays$play[i])){
            team <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0(teams,' personal foul'))))]
            otherteam <- teams[teams != team]
            plays$possAfter[i] <- otherteam
            plays$possBefore[i] <- plays$possAfter[i-1]
            next
        }
        if(grepl('shooting foul', plays$play[i])){
            otherteam <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0(teams,' draws the foul'))))]
            team <- teams[teams != otherteam]
            plays$possAfter[i] <- otherteam
            plays$possBefore[i] <- otherteam
            next
        }
        if(grepl('offensive foul', plays$play[i])){
            team <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0(teams,' offensive foul'))))]
            otherteam <- teams[teams != team]
            plays$possAfter[i] <- team # don't change possession here, because the next play is always "turnover" at the same timestamp
            plays$possBefore[i] <- team
            next
        }
        if(grepl('technical foul', plays$play[i])){
            team <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0(teams,' technical foul'))))]
            otherteam <- teams[teams != team]
            plays$possBefore[i] <- plays$possAfter[i-1]
            plays$possAfter[i] <- otherteam
            next
        }
        if(grepl('flagrant', plays$play[i])){
            team <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0(teams,' flagrant'))))]
            otherteam <- teams[teams != team]
            plays$possBefore[i] <- plays$possAfter[i-1]
            plays$possAfter[i] <- otherteam
            next
        }
        if(grepl('blocks', plays$play[i])){
            team <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0(teams,' blocks'))))]
            otherteam <- teams[teams != team]
            plays$possBefore[i] <- otherteam
            plays$possAfter[i] <- 'unknown'
            next
        }
        if(grepl('End of', plays$play[i])){
            plays$possAfter[i] <- 'stoppage'
            plays$possBefore[i] <- plays$possAfter[i-1]
            next
        }
        if(grepl('Open inbound', plays$play[i])){
            team <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0('Open inbound ',teams))))]
            plays$possAfter[i] <- team
            plays$possBefore[i] <- 'stoppage'
            next
        }
        if(grepl('Tie up', plays$play[i])){
            plays$possBefore[i] <- plays$possAfter[i-1]
            plays$possAfter[i] <- 'unknown'
            next
        }
        if(grepl('gains possession', plays$play[i])){
            team <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0(teams,' gains possession'))))]
            plays$possBefore[i] <- 'unknown'
            plays$possAfter[i] <- team
        }
    }
    ###
    
    # replays are tricky, just take before and after from prev/next play
    for(i in grep('replay', plays$play)){
        plays$possBefore[i] <- plays$possAfter[i-1]
        plays$possAfter[i] <- plays$possBefore[i+1]
    }
    
    # check for missing "open inbound"
    if(length(grep('Open inbound', plays$play)) != 3){
        missing <- c('Q2','Q3','Q4')
        missing <- missing[! missing %in% plays$quarter[grep('Open inbound', plays$play)]]
        # add appropriate inbound play
        for(m in missing){
            zerotime <- ifelse(m == 'Q4', "+0:00", "7:00")
            idx <- which.max(plays$quarter == m & plays$time != zerotime)
            team <- plays$possBefore[idx]
            # duplicate the row before "open inbound" (score won't change)
            plays <- rbind(plays[1:(idx-1),], plays[(idx-1):nrow(plays),]) 
            # set values we know
            plays$time[idx] <- zerotime
            plays$quarter[idx] <- m
            plays$play[idx] <- plays$teamplay[idx] <- paste('Open inbound', team)
            plays$possBefore[idx] <- 'stoppage'
            plays$possAfter[idx] <- team
        }
    }
    
    # technical fouls result in one shot, outside of any possession, unless offsetting
    if(any(grepl('technical foul', plays$play))){

        # while any technical fouls not accounted for
        # - find first tech
        # - step forward
        # - if 1-pt free throw: mark accounted for
        # - if another tech, other team: offsetting, mark both accounted for
        # - if another tech, same team: need 2 free throws
        
    })
    
    # flagrant fouls are free throw + ball
    if(any(grepl('flagrant', plays$play))){
        for(i in grep('flagrant', plays$play)){
            # check the next 5 lines for a free throw
            for(j in 1:5){
                if(grepl('one point free throw', plays$play[i+j])){
                    plays$possAfter[i+j] <- plays$possAfter[i]
                    break
                }
            }
        }
    }

    
    # final check
    stopifnot(all(plays$possAfter[-nrow(plays)] == plays$possBefore[-1]))
    
    plays$teamplay <- NULL
    return(plays)
}

# points, time
possessionStats <- function(plays){
    # count possessions (1,2,3...)
    plays$poss <- NA
    p <- 1
    curr <- plays$possAfter[1]
    for(i in 1:nrow(plays)){
        plays$poss[i] <- p
        if(! plays$possAfter[i] %in% c(curr, 'unknown')){
            p <- p+1
            curr <- plays$possAfter[i]
        }
    }
    
    # get time in seconds
    sec <- rep(NA, nrow(plays))
    idx13 <- which(plays$quarter %in% c('Q1','Q2','Q3'))
    sec[idx13] <- sapply(strsplit(plays$time[idx13], split=':'), function(t){
        t <- as.numeric(t)
        t <- 60*t[1]+t[2]
        return(7*60 - t)
    })
    idx4 <- which(plays$quarter == 'Q4')
    sec[idx4] <- sapply(strsplit(plays$time[idx4], split=':'), function(t){
        t <- as.numeric(t)
        t <- 60*t[1]+t[2]
        return(t)
    })
    plays$sec <- sec
    rm(idx13,idx4)
    
    # only keep valid possessions for stats
    possessions <- lapply(unique(plays$poss), function(i){
        idx <- which(plays$poss == i)
        if(min(idx) > 1){
            idx <- c(min(idx)-1, idx)
        }
        return(plays[idx,])
    })
    keep <- !sapply(possessions, function(p){
        any(c(p$possAfter) == 'stoppage')
    })
    possessions <- possessions[keep]
    
    # get points and time for each possession
    points <- sapply(possessions, function(p){
        length(grep('makes one point', p$play[-1])) + 
            2*length(grep('makes two point', p$play[-1])) +
            3*length(grep('makes three point', p$play[-1]))
    })
    time <- sapply(possessions, function(p){
        p[nrow(p),]$sec - p[1,]$sec
    })
    
    return(data.frame(points, time))
}




