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
    allplayers <- allplayers[!allplayers %in% teams]
    if(!all(allplayers %in% players$player)){
        # don't count "players" who only show up in technical fouls or get ejected, could be coaches
        problems <- allplayers[! allplayers %in% players$player]
        actualProblem <- sapply(problems, function(p){
            idx1 <- grep(p, plays$play)
            idx2 <- c(grep('technical foul', plays$play),
                      grep('ejected', plays$play))
            if(all(idx1 %in% idx2)){
                return(FALSE)
            }else{
                return(TRUE)
            }
        })
        if(any(actualProblem)){
            stop(paste('Players not subbed in:', paste(allplayers[!allplayers %in% players$player], collapse = ', ')))
        }else{
            for(prob in problems){
                players <- rbind(players, c(NA, prob))
            }
        }
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
    # make sure players only score for their team
    if(any(count1 > 0 & count2 > 0)){
        stop(paste('Players scoring for both teams:',
                   paste(names(count1)[count1 > 0 & count2 > 0], collapse = ', ')))
    }
    team1 <- players$team[count1[players$player] > 0][1]
    team2 <- players$team[count2[players$player] > 0][1]
    players <- rbind(players[which(players$team == team1), ], 
                     players[which(players$team == team2), ],
                     players[which(is.na(players$team)), ])
    
    return(players)
}

# possBefore, possAfter
possBeforeAfter <- function(plays, players){
    teams <- unique(players$team)
    teams <- teams[!is.na(teams)]
    
    # check for jump ball, split into two events (force tie-up / jump ball, gain possession)
    # not doing this, they use possession arrow

    # determine possession (before and after play)
    plays$possAfter <- plays$possBefore <- NA
    
    # make version of each play with just team names, no player names
    plays$teamplay <- plays$play
    for(i in 1:nrow(players)){
        plays$teamplay <- gsub(players$player[i], players$team[i], plays$teamplay)
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
    sec[plays$quarter == 'Q2'] <- sec[plays$quarter == 'Q2'] + 7*60
    sec[plays$quarter == 'Q3'] <- sec[plays$quarter == 'Q3'] + 14*60
    sec[plays$quarter == 'Q4'] <- sec[plays$quarter == 'Q4'] + 21*60
    plays$sec <- sec
    rm(idx13,idx4)
    
    # make sure plays are sorted by time
    if(any(diff(plays$sec) < 0)){
        # make sure "End of [quarter]" is actually the last play
        q1 <- which(plays$quarter == 'Q1')
        q1end <- which(plays$play == 'End of 1st Quarter.')
        q2 <- which(plays$quarter == 'Q2')
        q2end <- which(plays$play == 'End of 1st Half.')
        q3 <- which(plays$quarter == 'Q3')
        q3end <- which(plays$play == 'End of 3rd Quarter.')
        q4 <- which(plays$quarter == 'Q4')
        q4end <- which(plays$play == 'End of 4th Quarter.')
        plays <- plays[c(q1[which(q1 != q1end)],q1end,
                         q2[which(q2 != q2end)],q2end,
                         q3[which(q3 != q3end)],q3end,
                         q4[which(q4 != q4end)],q4end), ]
        if(any(diff(plays$sec) < 0)){
            stop('Plays out of order')
        }
    }
    
    # a one-point FT that isn't after a shooting foul is probably missing a clear path label
    for(i in grep('one point free throw', plays$play)){
        for(j in 1:3){
            # search backwards for shooting foul
            if(grepl('shooting foul',plays$play[i-j])){
                break
            }
            if(j==3){
                # only reach this part if there wasn't a shooting foul
                for(j2 in 1:3){
                    if(grepl('foul',plays$play[i-j2])){
                        plays$play[i-j2] <- sub('foul','clear path foul',plays$play[i-j2])
                        plays$teamplay[i-j2] <- sub('foul','clear path foul',plays$teamplay[i-j2])
                        break
                    }
                }
            }
        }
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
            # before: check for technical/flagrant/clear path
            if(i > 1 & plays$time[i] == plays$time[i-1] &
               plays$possAfter[i-1] %in% c('technical','flagrant','clear path')){
                plays$possBefore[i] <- plays$possAfter[i-1]
            }else{
                plays$possBefore[i] <- team
            }
            # after: check for and-1
            if(i < nrow(plays) & plays$time[i] == plays$time[i+1] &
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
            team <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0(teams,' shooting foul'))))]
            otherteam <- teams[teams != team]
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
            plays$possBefore[i] <- plays$possAfter[i-1]
            plays$possAfter[i] <- 'technical'
            next
        }
        if(grepl('flagrant', plays$play[i])){
            team <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0(teams,' flagrant'))))]
            otherteam <- teams[teams != team]
            plays$possBefore[i] <- plays$possAfter[i-1]
            plays$possAfter[i] <- otherteam
            next
        }
        if(grepl('clear path', plays$play[i])){
            team <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0(teams,' [a-z]* clear path'))))]
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
        if(grepl('Jump ball', plays$play[i])){
            plays$possBefore[i] <- plays$possAfter[i-1]
            team <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0(teams,' gains possession'))))]
            plays$possAfter[i] <- team
            next
        }
        if(grepl('kicked ball', plays$play[i])){
            team <- teams[which(!is.na(str_extract(plays$teamplay[i], paste0(teams,' kicked ball'))))]
            otherteam <- teams[teams != team]
            plays$possBefore[i] <- plays$possAfter[i-1]
            plays$possAfter[i] <- otherteam
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
        # go up to 5 plays ahead to figure out who got possession
        for(j in 1:5){
            if(!is.na(plays$possBefore[i+j])){
                plays$possAfter[i:(i+j-1)] <- plays$possBefore[i+j]
                plays$possBefore[(i+1):(i+j)] <- plays$possBefore[i+j]
                break
            }
        }
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
    
    # they insert a turnover event after a jump ball if the defense gains possession
    # it's just for bookkeeping, don't need the event
    if(any(grepl('Jump ball', plays$play))){
        for(i in rev(grep('Jump ball', plays$play))){
            gainedPoss <- plays$possAfter[i]
            if(plays$possBefore[i+1] != gainedPoss & grepl('turnover', plays$play[i+1])){
                plays <- plays[-(i+1),]
            }
        }
    }
    
    
    # flagrant fouls and clear path fouls are free throw + ball
    for(foultype in c('flagrant','clear path')){
        if(any(grepl(foultype, plays$play))){
            for(i in grep(foultype, plays$play)){
                getsBall <- plays$possAfter[i]
                plays$possAfter[i] <- getsBall
                # check the next 5 lines for a free throw
                for(j in 1:5){
                    if(grepl('free throw', plays$play[i+j])){
                        plays$possBefore[(i+1):(i+j)] <- getsBall
                        plays$possAfter[i:(i+j-1)] <- getsBall
                        plays$possAfter[i+j] <- getsBall # team that got fouled gets ball back
                        break
                    }
                }
            }
        }
    }

    # final checks
    # stopifnot(!any(is.na(plays$possBefore)))
    # stopifnot(!any(is.na(plays$possAfter)))
    # stopifnot(all(plays$possAfter[-nrow(plays)] == plays$possBefore[-1]))
    
    plays$teamplay <- NULL
    return(plays)
}

getPossessions <- function(plays, players){
    teams <- unique(players$team)
    teams <- teams[!is.na(teams)]
    
    # make version of each play with just team names, no player names
    plays$teamplay <- plays$play
    for(i in 1:nrow(players)){
        plays$teamplay <- gsub(players$player[i], players$team[i], plays$teamplay)
    }
    
    possessions <- list()
    for(team in teams){
        posses <- list()
        start.idx <- which(plays$possBefore != team & plays$possAfter == team)
        for(i in start.idx){
            #print(i)
            j <- 1
            while(plays$possAfter[i+j] == team){
                j <- j+1
            }
            out <- plays[i:(i+j),]
            out$team <- team
            out$length <- out$sec[nrow(out)] - out$sec[1]
            posses[[length(posses)+1]] <- out
        }
        possessions <- c(possessions, posses)
    }
    
    # check validity of each possession
    validPoss <- rep(FALSE, length(possessions))
    possStart <- possEnd <- points <- rep(NA, length(possessions))
    for(i in 1:length(possessions)){
        p <- possessions[[i]]
        
        team <- p$team[1]
        teamInd <- 1 + (team == teams[2])
        # count points
        pts <- p[[paste0('score',teamInd)]]
        points[i] <- pts[length(pts)] - pts[1]
        
        otherteam <- teams[teams != team]
        countPoss <- TRUE
        if(any(p$possBefore %in% c('technical','flagrant'))){
            countPoss <- FALSE
        }
        if(any(p$possAfter %in% c('technical','flagrant'))){
            countPoss <- FALSE
        }
        
        # check start
        startset <- c(paste(team,'gains possession'),
                      paste(team,'defensive rebound'),
                      paste(team,'offensive rebound'),
                      paste('Open inbound', team),
                      paste(otherteam,'turnover'),
                      paste(otherteam,'makes'))
        startTF <- sapply(startset, function(phrase){
            grepl(phrase, p$teamplay[1])
        })
        validStart <- sum(startTF) == 1
        if(validStart){
            possStart[i] <- c('jump','defReb','offReb','inbound','turnover','make')[startTF]
        }
        
        # check end
        endset <- c(paste(team,'makes'),
                    paste(team,'misses'),
                    paste(team,'offensive foul'),
                    paste(team,'turnover'),
                    paste(otherteam,'gains possession'),
                    paste(otherteam,'blocks'),
                    'End of')
        endTF <- sapply(endset, function(phrase){
            grepl(phrase, p$teamplay[nrow(p)])
        })
        validEnd <- sum(endTF) == 1
        if(endTF['End of'] & p$length[1] <= 5){ # don't count if 5 sec or less remaining
            validEnd <- FALSE
        }
        if(validEnd){
            possEnd[i] <- c('make','miss','offFoul','turnover','jump','block')[endTF]
        }
        validPoss[i] <- countPoss & validStart & validEnd
    }
    
    out <- possessions
    for(i in 1:length(out)){
        out[[i]]$start <- possStart[i]
        out[[i]]$end <- possEnd[i]
        out[[i]]$valid <- validPoss[i]
        out[[i]]$points <- points[i]
    }
    
    #out <- out[validPoss]
    return(out)
}

# takes output of getPossessions()
# returns: team, points, length, firstplay, lastplay
possessionData <- function(poss){
    poss <- lapply(poss, function(p){
        p[1,]
    })
    poss <- do.call(rbind, poss)
    poss <- poss[poss$valid, ]
    poss <- poss[,c('team','points','length','start','end')]
    return(poss)
}




