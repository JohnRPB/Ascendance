################################################################################################################################
#############------------------------------------------------------------------------------------------------------#############
#############---------------------------------- Ascendance "Assess" Module ----------------------------------------#############
#############------------------------------------------------------------------------------------------------------#############
################################################################################################################################

#' This module generates the optimal curves overall and for individual focus areas.

coltime <- function(dat, names, colName = "X") {
  
  if (length(names) == 0) {
    
    colSum <- data.frame(X = as.difftime(rep(0, nrow(dat)), units = "secs"))
    names(colSum) <- colName
    colSum
    
  } else {
    
    colSum <- dat[,names[1], drop = FALSE]
    
    for (i in 1:nrow(colSum)) {
      
      colSum[i,] <- as.difftime(0, units = "secs")
      
    }
    
    for (i in names) {
      
      colSum <- colSum + dat[,i, drop = FALSE]  
      
    }
    
    names(colSum) <- colName
    colSum
    
  }
  
}

#' This module presents the global optimal curve along with various specific focus area curves

curveboxUI <- function(id, ID) {
  
  ns <- NS(id)
  
  box(width = 6, title = ID, fluidRow(column(12, plotOutput(ns("curve")))),
      fluidRow(column(3, offset = 2, h4("General")), column(5, h4("VirtualGPA"))))
  
  
}

# the piebox server must be passed the focus area of the box in the argument 'ID'

curvebox <- function(input, output, session, ID, Path) {
  
  load(Path)
  
  rv <- reactiveValues()
  
  rv$A <- A
  
  dat <- A$opC[, (colnames(A$opC) == ID) | (colnames(A$opC) == "fp")]
  Dat <- dat
  
  dayafter <- dat$fp[dat$fp == max(dat$fp)] + as.difftime(1, units = "days")
  
  x <- c(dat$fp[1], dat$fp[dat$fp > dat$fp[1]], dayafter, dayafter, dat$fp[1])
  
  y <- c(as.difftime(0, units = "hours"), dat[[ID]][dat$fp >= dat$fp[1]],
         as.difftime(0, units = "hours"), as.difftime(0, units = "hours"))
  
  shade <- data.frame(x, y)
  
  #####
  
  faIndex <- colnames(A$tentr$start) %in% A$tdat$ID[A$tdat$fa == ID]
  
  if (all(faIndex == FALSE)) {
    
  } else {
    
    dat <- A$Logged[,c("fp", ID)]
    print(dat); print(ID)
    colnames(dat) <- c("fp", "bcggplot")
    
    pointdat <- dat[nrow(dat),]
    
    output$curve <- renderPlot({
      
      ggplot(data = Dat, aes(x = fp, y = ID)) +  geom_polygon(data = shade, aes(x, y), alpha = .80) + 
        theme(axis.text.x = element_text(angle = 90, vjust = .5), panel.grid.minor.x = element_blank()) +
        scale_x_datetime(breaks = Dat$fp, labels = format(Dat$fp,  "%a %d"), name = NULL) + 
        scale_y_continuous(name = NULL) + geom_line(data = dat, aes(x = fp, y = bcggplot), color = "steelblue", size = 1.5) + 
        geom_point(data = pointdat, aes(x = fp, y = bcggplot), color = "steelblue", size = 4)
      
    })
    
  }
  
}

assessUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    
    column(11, offset = 1, 
           
           tabBox(title = "Global Assessment", width = 11,
             
             tabPanel("Priority", 
                      fluidRow(column(12, plotOutput(ns("priorGlobal")))),
                      fluidRow(column(12, offset = 1, h4("General"), h4("VirtualGPA")))
                      ),
             tabPanel("Efficiency",
                      fluidRow(column(12, plotOutput(ns("effGlobal")))),
                      fluidRow(column(12, offset = 1, h4("General"), h4("VirtualGPA")))
                      ),
             tabPanel("Hours", br(),
                      fluidRow(column(12, plotOutput(ns("hoursGlobal")))),
                      fluidRow(column(5, offset = 2, h4("General")), column(5, h4("VirtualGPA")))
                      )
             
           )
    ),
    
    column(10, offset = 1, 
           
           lapply(unlist(A$fdat$fa), function(id) {curveboxUI(ns(id), id)})
           
    )
    
  )
  
  
}

assess <- function(input, output, session, Path) {
  
  load(Path)
  
  ns <- session$ns
  
  dayafter <- A$opC$fp[A$opC$fp == max(A$opC$fp)] + as.difftime(1, units = "days")
  
  x <- c(A$opC$fp[1], A$opC$fp[A$opC$fp > A$opC$fp[1]], dayafter, dayafter, A$opC$fp[1])
  
  y <- c(as.difftime(0, units = "hours"), A$opC$full[A$opC$fp >= A$opC$fp[1]],
         as.difftime(0, units = "hours"), as.difftime(0, units = "hours"))
  
  shade <- data.frame(x, y)
  
  if (5 > 8) {
    
    tentr <- as.list(rep(Sys.time(), length(A$tdat$ID)))
    names(tentr) <- colnames(A$opC)[colnames(A$opC) %in% A$tdat$ID]
    tentr <- data.frame(tentr, stringsAsFactors = FALSE)
    colnames(tentr) <- colnames(A$opC)[colnames(A$opC) %in% A$tdat$ID]
    
    tentr <- lapply(1:2, function(x) { tentr })
    
    names(tentr) <- c("start", "end")
    
    auto <- as.list(rep("", length(A$tdat$tasks)))
    names(auto) <- colnames(A$opC)[colnames(A$opC) %in% A$tdat$ID]
    auto <- data.frame(auto, stringsAsFactors = FALSE)
    colnames(auto) <- colnames(A$opC)[colnames(A$opC) %in% A$tdat$ID]
    
    eff <- as.list(rep(0, length(A$tdat$tasks)))
    names(eff) <- colnames(A$opC)[colnames(A$opC) %in% A$tdat$ID]
    eff <- data.frame(eff, stringsAsFactors = FALSE)
    colnames(eff) <- colnames(A$opC)[colnames(A$opC) %in% A$tdat$ID]
    
    dummy <- list()
    
    dummy$auto <- auto; dummy$start <- tentr$start; dummy$end <- tentr$end; dummy$eff <- eff
    
    A$tentr <- dummy
    
    timediff <- (A$fps[[1]][2] - A$fps[[1]][1])/2
    
    latemail <- function(N, st=A$fps[[1]][1], et= as.POSIXct(A$fps[[1]][2])+ as.difftime(7, units = "hours")-timediff) {
      st <- as.POSIXct(as.Date(st)) + as.difftime(7, units = "hours")
      et <- as.POSIXct(as.Date(et)) + as.difftime(7, units = "hours")
      dt <- as.numeric(difftime(et,st,unit="sec"))
      ev <- sort(runif(N, 0, dt))
      rt <- st + ev
    }
    
    simulate_data <- function(N, meanHour = .4, stdHour = .05, meanEff = .5, stdEff = .1) {
      
      lapply(1:N, function(x) {
        
        A$tentr$auto <- rbind(A$tentr$auto, as.list(rep("", ncol(A$tentr$start))))
        A$tentr$start <- rbind(A$tentr$start, as.list(rep(Sys.time(), ncol(A$tentr$start))))
        A$tentr$end <- rbind(A$tentr$end, as.list(rep(Sys.time(), ncol(A$tentr$start))))
        A$tentr$eff <- rbind(A$tentr$eff, as.list(rep(0, ncol(A$tentr$start))))
        
        start <- latemail(length(A$tdat$ID))
        end <- start + as.difftime(rnorm(length(A$tdat$ID), meanHour, stdHour), units = "hours")
        effEst <- rnorm(length(A$tdat$ID), meanEff, stdEff)
        
        start <- as.list(start)
        names(start) <- colnames(A$tentr$start)
        end <- as.list(end)
        names(end) <- colnames(A$tentr$end)
        effEst <- as.list(effEst)
        names(effEst) <- colnames(A$tentr$eff)
        
        A$tentr$start[x,] <- as.data.frame(start, optional = TRUE)
        A$tentr$end[x,] <- as.data.frame(end, optional = TRUE)
        A$tentr$eff[x,] <- as.data.frame(effEst, optional = TRUE)
        
      })
      
    }
    
    simulate_data(10)
    
  }
  
  now <- Sys.time()
  
  Logged <- A$tentr$end - A$tentr$start
  
  difftime_start <- Logged; difftime_end <- Logged
  
  dum <- difftime_start
  
  dum[] <- lapply(1:ncol(difftime_start), function(x) {
    
    lapply(1:nrow(difftime_start), function(y) {
      
      dum[y,x] <- dum[y,x] != as.difftime(0, units = "hours")   
      
    })
    
  })
    
  if (nrow(Logged) == 1 | nrow(Logged) == 2) {
    
    for (i in as.character(rev(unlist(A$cdat$ID)))) {
      
      focusIncat <- A$cdat$fa[A$cdat$ID == i]
      catIncat <- A$cdat$cat[A$cdat$ID == i]
      
      taskIDsInCat <- as.character(A$tdat$ID[(A$tdat$fa %in% focusIncat) & (A$tdat$cat %in% catIncat)])
      
      Logged <- cbind(coltime(Logged, taskIDsInCat, i), Logged)
      
    }
    
    for (i in rev(unlist(A$fdat$fa))) {
      
      taskIDsInFa <- as.character(A$tdat$ID[A$tdat$fa %in% i])
      
      Logged <- cbind(coltime(Logged, taskIDsInFa, i), Logged)
      
    }
    
    Logged <- cbind(coltime(Logged, as.character(unlist(A$tdat$ID)), colName = "full"), Logged)
    
    Logged[] <- lapply(1:ncol(Logged), function(x) {
      
      lapply(1:nrow(Logged), function(y) {
        
        a <- Logged[y,x]
        units(a) <- "hours"
        a
        
      })
      
    })
    
    Logged[] <- lapply(1:ncol(Logged), function(x) {
      
      Logged[,x] <- unlist(Logged[,x])
      
    })
    
    Logged <- cbind(fp = as.POSIXct(A$fps[[1]][1]) + as.difftime(7, units = "hours"), Logged)
    
    Ender <- Logged[nrow(Logged),]; Ender$fp <- now
    
    Logged <- rbind(Logged, Ender)
    
    A$Logged <- Logged; save(A, file = Path)
    
    pointdat <- Logged[nrow(Logged),c("fp", "full")]
    
  } else {
    
    Starter <- difftime_start[1,]
    difftime_start <- difftime_start[2:nrow(difftime_start),]
    Ender <- difftime_end[1,]
    difftime_end <- difftime_end[2:nrow(difftime_end),]
    
    Logged <- Logged[2:nrow(Logged),]
    
    startTimes <- as.POSIXct(A$tentr$start[dum == TRUE]); endTimes <- as.POSIXct(A$tentr$end[dum == TRUE])
    
    startOrder <- order(startTimes)
    
    startTimes <- startTimes[startOrder]; endTimes <- endTimes[startOrder]
    
    difftime_start <- difftime_start[startOrder,]
    difftime_end <-  difftime_end[startOrder,]
    
    startIndex <- seq(length.out = nrow(difftime_start), by = 2)
    difftime_startC <- difftime_start
    endIndex <- seq(length.out =  nrow(difftime_end), by = 2)+1
    difftime_endC <- difftime_end
    
    cumtime <- function(x) {
      
      for (i in 1:(length(x)-1)) {
        
        x[i+1] <- x[i] + x[i+1] 
        
      }
      
      x
      
    }
    
    difftime_startC[] <- lapply(1:ncol(difftime_startC), function(x) {
      
      cumtime(unlist(difftime_startC[,x]))
      
    })
    
    difftime_endC[] <- lapply(1:ncol(difftime_endC), function(x) {
      
      cumtime(unlist(difftime_endC[,x]))
      
    })
    
    difftime_startC <- cbind(difftime_startC, index = startIndex)
    difftime_endC <- cbind(difftime_endC, index = endIndex)
    
    difftime_startC <- cbind(fp = startTimes, difftime_startC)
    difftime_endC <- cbind(fp = endTimes, difftime_endC)
    
    Logged <- rbind(difftime_startC, difftime_endC)
    
    Logged <- Logged[order(Logged$index),1:(ncol(Logged)-1)]
    
    startDate <- as.POSIXct(A$fps[[1]][1]) + as.difftime(7, units = "hours")
    
    Starter <- cbind(fp = startDate, Starter)
    Ender <- cbind(fp = now, Logged[nrow(Logged), 2:ncol(Logged)])
    
    Logged <- rbind(Starter, Logged)
    Logged <- rbind(Logged, Ender)
    
    loggedTimes <- Logged$fp
    Logged <- Logged[,2:ncol(Logged)]
    
    for (i in as.character(rev(unlist(A$cdat$ID)))) {
      
      focusIncat <- A$cdat$fa[A$cdat$ID == i]
      catIncat <- A$cdat$cat[A$cdat$ID == i]
      
      taskIDsInCat <- as.character(A$tdat$ID[(A$tdat$fa %in% focusIncat) & (A$tdat$cat %in% catIncat)])
      
      Logged <- cbind(coltime(Logged, taskIDsInCat, i), Logged)
      
    }
    
    for (i in rev(unlist(A$fdat$fa))) {
      
      taskIDsInFa <- as.character(A$tdat$ID[A$tdat$fa %in% i])
      
      Logged <- cbind(coltime(Logged, taskIDsInFa, i), Logged)
      
    }
    
    Logged <- cbind(coltime(Logged, as.character(unlist(A$tdat$ID)), colName = "full"), Logged)
    
    Logged[] <- lapply(1:ncol(Logged), function(x) {
      
      lapply(1:nrow(Logged), function(y) {
        
        a <- Logged[y,x]
        units(a) <- "hours"
        a
        
      })
      
    })
    
    Logged[] <- lapply(1:ncol(Logged), function(x) {
      
      Logged[,x] <- unlist(Logged[,x])
      
    })
    
    Logged <- cbind(fp = loggedTimes, Logged)
    
    A$Logged <- Logged; save(A, file = Path)
    
    
    pointdat <- Logged[nrow(Logged),c("fp", "full")]

     
  }
  
  output$hoursGlobal <- renderPlot({
    
    ggplot(data = A$opC, aes(x = fp, y = full)) +  
      geom_polygon(data = shade, aes(x, y), alpha = .80) + 
      theme(axis.text.x = element_text(angle = 90, vjust = .5), panel.grid.minor.x = element_blank()) +
      scale_x_datetime(breaks = A$opC$fp, labels = format(A$opC$fp,  "%a %d"), name = NULL) + 
      scale_y_continuous(name = NULL) + 
      geom_line(data = Logged, aes(x = fp, y = full), color = "steelblue", size = 1.5) + 
      geom_point(data = pointdat, aes(x = fp, y = full), color = "steelblue", size = 4)
    
  })
  
  lapply(unlist(A$fdat$fa), function(id) {
    
    callModule(curvebox, id, ID = id, Path = Path)
    
  })
  
  
}