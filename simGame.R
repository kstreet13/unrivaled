
simGame <- function(params){
    quarter_length <- 7 # minutes
    
    score <- c(0,0)
    arrow <- sample(1:2, 1)
    poss <- 3 - arrow
    # game <- data.frame(time = numeric(0),
    #                    poss = numeric(0),
    #                    score1 = numeric(0),
    #                    score2 = numeric(0))
    
    for(Q in 1:3){
        poss <- arrow
        arrow <- 3 - arrow
        t <- quarter_length*60
        while(t > 0){
            # determine length and outcome
            pt <- sample(as.numeric(names(params$lengths)), 1, prob = params$lengths)
            if(t < pt){
                pt <- sample(as.numeric(names(params$lengths)), 1, prob = params$lengths)
                if(t < pt){
                    # end quarter
                    t <- t - pt
                    next
                }
            }
            outcome <- sample(names(params$outcome), 1, prob = params$outcome)
            t <- t - pt
            #game <- rbind(game, c(pt, poss, 0, 0))
            
            if(outcome == 'make'){
                score[poss] <- score[poss] + 
                    sample(as.numeric(names(params$makePoints)), 1, prob = params$makePoints)
                poss <- 3 - poss # change possession
                next
            }
            if(outcome == 'miss'){
                score[poss] <- score[poss] + 
                    sample(as.numeric(names(params$missPoints)), 1, prob = params$missPoints)
                if(runif(1) < params$reb['defReb']){
                    poss <- 3 - poss # defensive rebound
                }
                next
            }
            if(outcome == 'turnover'){
                poss <- 3 - poss # change possession
                next
            }
            if(outcome == 'block'){
                if(runif(1) < params$reb['defReb']){
                    poss <- 3 - poss # defensive rebound
                }
            }
            if(outcome == 'jump'){
                if(runif(1) < 0.5){
                    poss <- arrow
                    arrow <- 3 - arrow
                }
            }
        }
    }
    
    Wscore <- max(score) + 11
    while(max(score) < Wscore){
        # determine outcome
        outcome <- sample(names(params$outcome), 1, prob = params$outcome)
        
        if(outcome == 'make'){
            score[poss] <- score[poss] + 
                sample(as.numeric(names(params$makePoints)), 1, prob = params$makePoints)
            poss <- 3 - poss # change possession
            next
        }
        if(outcome == 'miss'){
            score[poss] <- score[poss] + 
                sample(as.numeric(names(params$missPoints)), 1, prob = params$missPoints)
            if(runif(1) < params$reb['defReb']){
                poss <- 3 - poss # defensive rebound
            }
            next
        }
        if(outcome == 'turnover'){
            poss <- 3 - poss # change possession
            next
        }
        if(outcome == 'block'){
            if(runif(1) < params$reb['defReb']){
                poss <- 3 - poss # defensive rebound
            }
        }
        if(outcome == 'jump'){
            if(runif(1) < 0.5){
                poss <- arrow
                arrow <- 3 - arrow
            }
        }
    }
    
    print(paste0(score[1],' - ',score[2],' (WS: ',Wscore,')'))
    
    return(score)
}
