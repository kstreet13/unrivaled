prevElo <- function(team, date, sched){
    ytd <- sched[which(sched$date < date &
                              (sched$team1 == team | sched$team2 == team)), ]
    if(nrow(ytd) == 0){
        # first game of new season
        prevElo <- 1500
    }else{
        # take post-game Elo from last game played
        last <- ytd[nrow(ytd), ]
        if(last$team2 == team){
            prevElo <- last$elo_post2
        }else if(last$team1 == team){
            prevElo <- last$elo_post1
        }
    }
    return(prevElo)
}
team2_winProb <- function(game){
    # game should have pre-game Elo ratings
    if(is.na(game$elo_pre1) | is.na(game$elo_pre2)){
        stop('need pre-game Elo for both teams before calculating win probs')
    }
    elodiff <- game$elo_pre2 - game$elo_pre1
    return(1/(10^(-elodiff/400)+1))
}
eloShift <- function(game){
    # should have everything except post-game elo
    # higher K to account for shorter schedule, yearly reset
    K <- 28
    elodiff <- game$elo_pre2 - game$elo_pre1
    if(game$score2 > game$score1){
        if(elodiff > 0){ # home team was expected to win
            MoVmult <- (abs(game$score2 - game$score1)+3)^.8 / (7.5 + .006*elodiff)
        }else{
            MoVmult <- (abs(game$score2 - game$score1)+3)^.8 / (7.5 + .006*(-elodiff))
        }
        pregameFavMult <- 1 - game$team2_winProb # actual - expected
    }else if(game$score1 > game$score2){
        if(elodiff < 0){ # away team was expected to win
            MoVmult <- (abs(game$score2 - game$score1)+3)^.8 / (7.5 + .006*(-elodiff))
        }else{
            MoVmult <- (abs(game$score2 - game$score1)+3)^.8 / (7.5 + .006*elodiff)
        }
        pregameFavMult <- 0 - game$team2_winProb # actual - expected
    }
    return(abs(K * MoVmult * pregameFavMult))
}


calcElo <- function(sched){
    sched$elo_pre1 <- sched$elo_pre2 <- sched$elo_post1 <- sched$elo_post2 <- sched$team2_winProb <- NA
    teams <- unique(c(sched$team1, sched$team2))
    
    # QC stuff. missing values, ties
    if(any(!is.na(sched$score2) & (sched$score2==sched$score1))){
        stop('ties detected')
    }
    # remove unplayed games
    sched <- sched[!is.na(sched$score1), ]
    sched <- sched[!is.na(sched$score2), ]
    
    for(gi in 1:nrow(sched)){
        game <- sched[gi,]
        
        # search for previous Elo ratings
        game$elo_pre1 <- prevElo(game$team1, game$date, sched)
        game$elo_pre2 <- prevElo(game$team2, game$date, sched)
        
        # calculate win prob
        game$team2_winProb <- team2_winProb(game)
        
        # calculate update
        shift <- eloShift(game)
        if(game$score1 > game$score2){
            shift <- -shift
        }
        # update
        game$elo_post1 <- game$elo_pre1 - shift
        game$elo_post2 <- game$elo_pre2 + shift
        sched[gi,] <- game
    }
    
    return(sched)
}
