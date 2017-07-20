cumtime <- function(x) {
  
  for (i in 1:(length(x)-1)) {
    
    x[i+1] <- x[i] + x[i+1] 
    
  }
  
  x
  
}

#' Function adds or subtracts cells of two list data frames

diffFrame <- function(dat1, operator = "-", dat2) {

  #' Use first df as dummy frame
  
  diffFrame <- dat1
  
  #' Generate matrix that repeats column numbers of dat1 in all the rows of that column,
  #' to use for repeat-iteration  
  
  RowRepCol <- sapply(1:ncol(dat1), function(x) {rep(x, nrow(dat1))})
  
  #' Save list of sums/differences between cells in dat1 and cells in dat2
  
  L <- mapply(
    
    function(x,y) {
      
      if (operator == "-") {
        
        dat1[[x,y]] - dat2[[x,y]] 
        
      } else {
        
        dat1[[x,y]] + dat2[[x,y]]
        
      }
      
    }, 
    
    x = 1:nrow(dat1), y = RowRepCol, SIMPLIFY = FALSE
    
  )
  
  #' Loop over column numbers of dat1, breaking big list into smaller lists the size of the intended columns in the output
  #' data frame
  
  listOflists <- lapply(1:ncol(dat1), function(i) {L[(i*nrow(dat1) - (nrow(dat1) - 1)):(i*nrow(dat1))]})
  
  #' Replace dummy frame interior with new list of lists
  
  diffFrame[] <- listOflists
  
  #' Express result
  
  diffFrame
  
}

VectorizeString <- function(x) {
    
  #' Break string at every comma, store as vector
  
  vec <- strsplit(x, ",") %>% unlist
  
  #' For each string element of vector, generate vector of individual spaces and words; store as list
  
  list <- strsplit(vec, " ")
  
  #' Generate new list, removing any spaces at the front and end of the vectors in previous list
  
  newlist <- lapply(list, function(y) {
    
    #' Remove first space in vector
    
    if (y[1] == "") {
      
      y <- y[-1]
      
    }
    
    #' Remove last space in vector
    
    if (y[length(y)] == "") {
      
      y <- y[-length(y)]
      
    }
    
    #' Convert vector to character string, separating words by spaces
    
    paste(y, collapse = " ")
    
  })
  
  #' Unlist output
  
  output <- newlist %>% unlist
  
  #' Call output
  
  output
  
}

#' methods: c-add, c-cum,

lframe <- function(dat, selector = NULL, selectorRow = NULL, method = "c-add", colName = "X") {
  
  #' Subset specific rows if specified
  
  if (is.null(selectorRow)) {
    
    Dat <- dat
    
  } else {
    
    Dat <- dat[selectorRow,]
    
  }
  
  #' Change main selector variable to useful subsetting vector
  
  if (is.null(selector)) {
    
    S <- Dat %>% ncol %>% seq_len
    
  } else {
    
    S <- selector
    
  }
  
  #' Add all selected columns together
  #' 
  #'  ---------------
  
  #' Create initial empty vector 
  
  total <- Dat %>% nrow %>% rep(0, . )
  
  for (i in S) {
    
    col <- Dat[[i]] %>% do.call("c", .)
    total <- col + total
    
  }
  #' ----------------
  #' 
  
  #' Change function to be applied to 'total' vector prior to transformation to list. If cumulative output desired, use 'cumtime' function; if otherwise,
  #' use the identity function
  #' 
  #' -------------
  
  if (method == "c-cum") {
    
    varfunc <- cumtime
    
  } else if (method == "c-add") {
    
    varfunc <- identity
    
  }
  #' -------------
  #' 
  
  #' Transform 'total' vector to list, controlling for 'difftime' class, by applying previous function
  #'
  #' -------------
  
  C <- total %>% class
  
  if (C == "difftime") {
    
    U <- units(total)
    
    total %<>% varfunc %>% lapply( . , function(x) {as.difftime(x, units = U)})
    
  } else {
    
    total %<>% varfunc %>% as.list
    
  }
  #' -------------
  #' 
  
  #' Express final result
  
  total <- tibble(X = total)
  colnames(total) <- colName
  total
  
}

BuildOpCurve <- function(dat, dim_extent = "time", dim_metricS = "time", units_extent = "hours", metricJump = "days", start = NULL, end = NULL) {
  
  #' ---------------------------------------| ERROR HANDLING |-----------------------------------------
  
  extentClassVec <- lapply(dat$Extent, function(x) {class(x)})
  startendClassVec <- lapply(dat$Start, function(x) {class(x)})
  
  if ( !isTRUE(all.equal(extentClassVec, rep(extentClassVec[1], length(extentClassVec)))) ) {
    
    stop("Units of 'Extent' not homogeneous")
    
  } else if ( !isTRUE(all.equal(startendClassVec, rep(startendClassVec[1], length(startendClassVec)))) ) {
    
    stop("Units of 'Start' and 'End' not homogenous")
    
  } 
  
  #' ------------------------------------------| OPCURVE |---------------------------------------------
  
  
  #' Create 'Start' and 'End' lists for atoms and use them for iterating to make a list of ordered sequences of 
  #' metric points for each atom, making sure to process with the time dimension of metric points if they have it
  #' 
  #' ------------
  
  if (dim_metricS == "time") {
    
    a <- dat$Start %>% unlist %>% as.POSIXct(., origin = "1970-01-01 00:00.00 UTC")
    b <- dat$End %>% unlist %>% as.POSIXct(., origin = "1970-01-01 00:00.00 UTC") 
    
  } else {
    
    a <- dat$Start %>% unlist
    b <- dat$End %>% unlist
    
  }
  
  taskperiods <- lapply(seq(length(a)), function(x) {
    
    seq(from = a[x], to =  b[x], by = metricJump)
    
  })
  #' ------------
  #'
  
  #' Produce list of metric points from smallest to largest available, from points within tasks
  #' 
  #' ------------
  
  if (is.null(start)) {
    
    Min <- dat$Start %>% do.call("c", .) %>% min
    
  } else {
    
    if(dim_metricS == "time") {
      
      Min <- as.POSIXct(start)
      
    } else {
      
      Min <- start
      
    }
    
  }
  
  if (is.null(end)) {
    
    Max <- dat$End %>% do.call("c", . ) %>% max
    
  } else {
    
    if(dim_metricS == "time") {
      
      Max <- as.POSIXct(end)
      
    } else {
      
      Max <- end
      
    }
    
  }
  
  LongestSeq <- seq(Min, Max, by = metricJump)
  
  #' ------------
  #'
  
  #' Create list of True/False index values specifying which metric points in every task are in the full set of metric points for the longest
  #' task; needed to replace unused metric points with '0's
  
  periodsIndex <- lapply(seq(length(a)), function(x) {
    
    if (dim_metricS == "time") {
      
      Max <- dat$End[[x]] %>% max
      Min <- dat$Start[[x]] %>% min
      
      LongestSeq %within% as.interval(Max-Min, Min)
      
    } else {
    
    LongestSeq %in% taskperiods[[x]]
    
    }
      
  })
  
  #' Make list of lists of work-required-per-point, for each atom, making sure to process with the time dimension of 'Extent', if it has one.
  
  opCurve <- lapply(seq(length(a)), function(x) {
    
    est <- dat$Extent[[x]]
    
    if (dim_extent == "time") {
      
      units(est) <- units_extent
      
    }
     
    if (dim_metricS == "time") {
     
      a <- dat$Start[[x]] %>% unlist %>% as.POSIXct(., origin = "1970-01-01 00:00.00 UTC")
      b <- dat$End[[x]] %>% unlist %>% as.POSIXct(., origin = "1970-01-01 00:00.00 UTC")
      
      tasklength <- as.numeric(difftime(b, a, units = metricJump))
      
    } else {
      
      a <- dat$Start[[x]] %>% unlist
      b <- dat$End[[x]] %>% unlist
      
      tasklength <- ((b - a)+1)/metricJump
      
    }
    
    slope <- est/tasklength
    
    optC <- slope*rep(1, length(LongestSeq))
    optC[periodsIndex[[x]] == FALSE] <- 0
    optC
    
  })
  
  #' Turn list of lists into data frame with atom IDs as column names.
  #'
  #' ------------
  
  names(opCurve) <- as.character(unlist(dat$ID))
  opCurve <- data.frame(opCurve)
  names(opCurve) <- as.character(unlist(dat$ID))
  
  opCurve[] <- lapply(1:length(opCurve), function(x) {
    
    as.numeric(opCurve[,x])
    
  })
  #' ------------
  #'    
  
  #' Express reult
  
  cbind(fp = LongestSeq, opCurve) %>% as_tibble
  
}

###################################################

BuildProcLog <- function(dat, starter = TRUE, ender = TRUE, start = NULL, end = NULL) {
  
  diffs <- diffFrame(dat$End, "-", dat$Start)
  dum <- diffs
  
  #' Create index data frame of True/False values, indicating which cells contain
  #' non-empty blocks, for use later.
  
  dum[] <- lapply(1:ncol(dum), function(x) {
    
    lapply(1:nrow(dum), function(y) {
      
        C <- dum[[y,x]] %>% class
        
        if (C == "difftime") {U <- dum[[y,x]] %>% units} else {U <- "hours"}
        
        dum[[y,x]] <- (dum[[y,x]] != as.difftime(0, units = U)) | (dum[[y,x]] != 0)
      
    })
    
  })
  
  #' Convert 'dum' and dat$Start/End to regular data frames (technicality to do next step)
  
  datS <- dat$Start %>% as.data.frame; datE <- dat$End %>% as.data.frame; datT <- dat$LogTime %>% as.data.frame
  dum2 <- dum %>% as.data.frame  
  
  #' Create list of start and end points then transform to vector
  
  startPoints <- datS[dum2 == TRUE] %>% do.call("c", .) 
  
  endPoints <- datE[dum2 == TRUE] %>% do.call("c", .) 
  
  entryTimes <- datT[dum == TRUE] %>% do.call("c", .)
  
  #' Generate order index for start points; order both start points and end points by that index
  
  startOrder <- order(startPoints)
  startPoints <- startPoints[startOrder]; endPoints <- endPoints[startOrder]; entryTimes <- entryTimes[startOrder]
  
  #' Order start and end data frames
  
  datSo <- diffs[startOrder,]
  datEo <-  diffs[startOrder,]
  
  #' Remove points from datEo and replace with unit-appropriate zeros
  
  datSo[] <- lapply(1:ncol(datSo), function(i) {
    
    lapply(1:nrow(datSo), function(j) {
      
      C <- datSo[[j,i]] %>% class
      
      if (C == "difftime") {
        
        U <- datSo[[j,i]] %>% units
        
        as.difftime(0, units = U)
        
      } else {
        
        0
        
      }
      
    })
    
  })
  
  #' Create index vectors to later append  to both data frames, containing the final positions of every row in the
  #' eventual combined data frame
  
  startIndex <- seq(length.out = nrow(datSo), by = 2)
  endIndex <- seq(length.out =  nrow(datEo), by = 2)+1 # offset rows by 1
  
  #' Append index vectors as columns to cumulative data frames, then name new column "index"
  
  datSo <- cbind(datSo, index = list(startIndex)) %>% as_tibble; colnames(datSo)[ncol(datSo)] <- "index"
  datEo <- cbind(datEo, index = list(endIndex)) %>% as_tibble; colnames(datEo)[ncol(datEo)] <- "index"
  
  #' Append the entry points in front, for both data frames
  
  datSo <- cbind(et = entryTimes, fp = startPoints, datSo) %>% as_tibble
  datEo <- cbind(et = entryTimes, fp = endPoints, datEo) %>% as_tibble
  
  #' Stack one data frame on top of the other, then order by index and remove 
  #' index column
  
  Logged <- rbind(datSo, datEo)
  Logged <- Logged[order(Logged$index),1:(ncol(Logged)-1)]
  
  #' Add starting row; makes graph of logged effort start when optimal curve does, instead of 
  #' only at the first block.
  
  if (starter == TRUE) {
    
    #' take first row of Logged
    
    Starter <- Logged[1,]
    
    startTime <- A$fps[[1]][1] %>% as.POSIXct
    
    C <- (Logged$'fp'[[1]] %>% class)[1]
    
    Starter[[1,1]] <- startTime
    
    if (C == "POSIXct") {
     
      Starter[[1,2]] <- startTime
        
    } else {
      
      Starter[[1,2]] <- A$iH[[length(A$iH)]]$Start %>% do.call("c", .) %>% min
      
    }
    
    Logged <- rbind(Starter, Logged)
        
  }
  
  if (ender == TRUE) {
    
    Ender <- Logged[nrow(Logged),]
    
    endTime <- Sys.time()
    
    Ender[[1,1]] <- endTime
    
    C <- (Logged$'fp'[[1]] %>% class)[1]
    
    if (C == "POSIXct") {
      
      Ender[[1,2]] <- endTime
      
    } 
    
    Logged <- rbind(Logged, Ender)
    
  }
  
  #' Express result
  
  Logged
  
}

##### Assemble

assemble <- function(framelist) {
  
  # Looks to see how many columns first data frame has, stores, then last data frame
  NumberOfBasicCols <- ncol(framelist[[1]])
  NumberOfColsTot <- ncol(framelist[[length(framelist)]])
  
  AncillaryCols <- framelist[[length(framelist)]][,(NumberOfBasicCols+1):NumberOfColsTot]
  
  AncillaryCols[] <- lapply(1:ncol(AncillaryCols), function(x) {
    
    0
    
  })
  
  for(i in 1:(length(framelist)-1)) {
    
    NewAncillaryCols <- AncillaryCols[1:nrow(framelist[[i]]),]
    AncillaryColsToReplace <- NewAncillaryCols[,i:ncol(NewAncillaryCols), drop = FALSE]
    
    framelist[[i]] <- cbind(framelist[[i]], AncillaryColsToReplace)
    
  }
  
  framelist
}
























