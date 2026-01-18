require(rvest)
require(stringr)
require(lubridate)

getSchedule <- function(year){
    p <- read_html(paste0('https://www.unrivaled.basketball/schedule?season=',year,'&games=All'))
    t <- as.character(p)
    t <- unlist(strsplit(t, split='\n'))
    
    # vector of dates
    dates <- sapply(t, function(r){
        pattern <- paste0('(Sun|Mon|Tue|Wed|Thu|Fri|Sat),[[:space:]]+(Jan|Feb|Mar)[[:space:]]+[0-9]+,[[:space:]]+',year)
        if(grepl(pattern, r)){
            return(str_extract(r, pattern))
        }else{
            return(NA)
        }
    }, USE.NAMES = FALSE)
    for(ii in 1:length(dates)){
        if(is.na(dates[ii])){
            if(any(!is.na(dates[1:(ii-1)]))){
                opts <- dates[1:(ii-1)][!is.na(dates[1:(ii-1)])]
                dates[ii] <- opts[length(opts)]
            }
        }
    }
    
    # count open and close <div> elements, so we can isolate the ones we're interested in (representing records)
    nclose <- sapply(gregexec('</div', t), function(x){ ifelse(is.matrix(x), ncol(x), 0) })
    nopen <- sapply(gregexec('<div', t), function(x){ ifelse(is.matrix(x), ncol(x), 0) })
    
    # find starting line for (potential) game elements
    ind <- which(grepl('div class="flex w-100 radius-8"', t, fixed=TRUE))
    
    # extract <div> elements for (potential) records
    records <- lapply(ind, function(i){
        divcount <- nopen[i]
        open <- TRUE
        j <- i
        while(open){
            j <- j+1
            divcount <- divcount + nopen[j] - nclose[j]
            if(divcount <= 0 | j %in% ind){
                open <- FALSE
            }
        }
        return(t[i:j])
    })
    
    dates <- dates[ind] # shouldn't select any NA's, length should be equivalent to records
    stopifnot(length(records)==length(dates))
    stopifnot(all(!is.na(dates)))
    
    # remove non-game events (all-star/1-on-1 tournament)
    keep <- grepl('background:var(--color-white-5)', records, fixed=TRUE)
    records <- records[keep]
    dates <- dates[keep]
    stopifnot(length(records)==length(dates))
    
    # completed records
    completed <- grepl('Final', records)
    
    games <- t(sapply(1:length(records), function(gi){
        if(completed[gi]){
            # get scores
            scores <- str_extract(records[[gi]], '>[0-9]+<')
            scores <- scores[!is.na(scores)]
            scores <- as.numeric(gsub('[<>]','', scores))
            stopifnot(length(scores)==2)
            # get teams
            teams <- str_extract(records[[gi]], '>[[:alpha:]]+[[:space:]]?[[:alpha:]]+<')
            teams <- teams[!is.na(teams)]
            teams <- gsub('[<>]','', teams)
            teams <- teams[! teams %in% c("Final","Box Score")]
            stopifnot(length(teams)==2)
            return(c(date = dates[gi],
                     team1 = teams[1],
                     score1 = scores[1],
                     team2 = teams[2],
                     score2 = scores[2]))
        }else{
            # get teams
            teams <- unlist(str_extract_all(records[[gi]], '[[:alpha:]]+[[:space:]]?[[:alpha:]]+ Logo'))
            teams <- gsub(' Logo','', teams)
            stopifnot(length(teams)==2)
            return(c(date = dates[gi],
                     team1 = teams[1],
                     score1 = NA,
                     team2 = teams[2],
                     score2 = NA))
        }
    }))
    
    games <- as.data.frame(games)
    games$score1 <- as.numeric(games$score1)
    games$score2 <- as.numeric(games$score2)
    games$date <- parse_date_time(games$date, c("abdY"))
    games$date <- as.Date(games$date)
    # remove games with undetermined teams
    games <- games[games$team1!='Unrivaled', ]
    
    return(games)
}

#sched <- getSchedule(2025)

teamcolors <- read.csv(text = 'team,color1,color2
Breeze,#B52F72,#4B3970
Hive,#FFC728,#17181C
Laces,#76B1A1,#FFFFFF
Lunar Owls,#40347D,#FCFFFE
Mist,#063860,#A3D3E7
Phantom,#000000,#FFFFFF
Rose,#1B5750,#DDA493
Vinyl,#820234,#1E9CBF')

# source('calcElo.R')

# makeUnrivaledStandingsGraph(2025, sched)

# sched <- calcElo(sched)


