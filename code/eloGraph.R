
# single-year plot, similar to standings
makeUnrivaledEloGraph <- function(year, sched, mode = c('light','dark')){
    mode <- match.arg(mode)
    
    games <- sched
    
    elo1 <- games[,c('date','team1','elo_pre1','elo_post1')]
    elo2 <- games[,c('date','team2','elo_pre2','elo_post2')]
    names(elo1) <- c('date','team','elo_pre','elo_post')
    names(elo2) <- names(elo1)
    elo <- rbind(elo1, elo2)
    elo <- elo[order(elo$date), ]
    rm(elo1,elo2)
    
    teams <- unique(elo$team)
    
    # check
    stopifnot(all(teams %in% teamcolors$team))
    # stopifnot(all(teamcolors$team %in% teams)) # teams can be added/removed
    
    # QC stuff. missing values, etc.
    
    # add initial value
    opening <- min(elo$date)
    for(team in teams){
        toAdd <- elo[which.max(elo$team == team), ]
        toAdd$date <- opening - 1
        toAdd$elo_post <- toAdd$elo_pre
        elo <- rbind(toAdd, elo)
    }
    finale <- max(games$date)
    
    curves <- lapply(teams, function(team){
        idx <- which(elo$team == team)
        curve <- data.frame(date = elo$date[idx],
                            elo = elo$elo_post[idx])
        # already added starting value, but want intermediate values, so it's clear when games happen
        supp <- data.frame(date = curve$date-1,
                           elo = c(0,curve$elo[-nrow(curve)]))
        supp <- supp[-1, ]
        supp <- supp[!supp$date %in% curve$date, ]
        curve <- rbind(supp,curve)
        curve <- curve[order(curve$date), ]
        # curve <- curve[!is.na(curve$margin), ] # only count played games
        # if(is.na(curve$WL[nrow(curve)])){
        #     curve <- curve[-nrow(curve), ]
        # }
        # 
        return(curve)
    })
    names(curves) <- teams
    
    
    # remove NAs (only relevant for current season)
    curves <- lapply(curves, function(x){ x[!is.na(x$elo), ]})
    for(i in seq_along(curves)){
        curve <- curves[[i]]
        curve <- curve[order(curve$date), ]
        curves[[i]] <- curve
    }
    # remove points after "today" (only relevant for current season)
    today <- Sys.Date() - 1
    curves <- lapply(curves, function(x){ x[x$date <= today, ]})
    
    # sort by final elo
    curves <- curves[order(sapply(curves, function(x){ x$elo[nrow(x)] }))]
    teamdata <- teamcolors[match(names(curves), teamcolors$team), ]
    teamdata$elo <- sapply(curves, function(x){ x$elo[nrow(x)] })
    # all(names(curves) == teamdata$name) # check
    
    # build legend df
    lgnd <- teamdata[nrow(teamdata):1, ]
    lgnd$nameElo <- paste0(1:nrow(lgnd), '. ', lgnd$team,' (',round(lgnd$elo),')')
    
    
    layout(matrix(c(1,1,1,2), nrow=1))
    par(mar = c(5,5,4,0)+.1, cex.lab = 1.25, cex.main = 2, bg = '#fffaf6')
    if(mode == 'dark'){
        par(fg = 'white', bg = '#272935', col.axis = 'white', col.lab = 'white', col.main = 'white', col.sub = 'white')
    }
    
    require(scales)
    xl <- range(do.call(rbind, curves)$date)
    yl <- range(sapply(curves,function(x){x$elo}))
    plot(xl, yl, col = 'transparent',
         xlab = 'Date', ylab='Elo Rating', main = paste(year, 'Elo Ratings'), las = 1)
    abline(h = 1500, lty = 2)
    
    #rect(min(sched$date)-9999,-9999,max(sched$date)+9999,9999, col='grey95')
    for(i in 1:length(curves)){
        cc <- as.character(teamdata[teamdata$team == names(curves)[i], ][,c('color1','color2')])
        #points(curves[[i]]$date, curves[[i]]$elo, col=cc[2], pch=16, cex=.5)
        #lines(curves[[i]]$date, curves[[i]]$elo, col=cc[2], lwd=4)
        lines(curves[[i]]$date, curves[[i]]$elo, col=cc[1], lwd=6)
        lines(curves[[i]]$date, curves[[i]]$elo, col=cc[2], lwd=2)
        points(curves[[i]]$date[nrow(curves[[i]])], curves[[i]]$elo[nrow(curves[[i]])], col=cc[2], pch=16, cex=3)
        points(curves[[i]]$date[nrow(curves[[i]])], curves[[i]]$elo[nrow(curves[[i]])], col=cc[1], pch=16, cex = 2)
    }
    
    par(mar = c(5,0,4,1)+.1)
    plot.new()
    legend('left', legend = rep('', nrow(lgnd)), bty='n', lwd=6, 
           col = lgnd$color1, cex = 1.6)
    legend('left', legend = lgnd$nameElo, bty='n', lwd=2, 
           col = lgnd$color2, cex = 1.6)
    
}
