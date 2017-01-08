################################################################################################################################
#############------------------------------------------------------------------------------------------------------#############
#############---------------------------------- Ascendance "Assess" Module ----------------------------------------#############
#############------------------------------------------------------------------------------------------------------#############
################################################################################################################################

#' This module generates the optimal curves overall and for individual focus areas.
#' 
#' Piping features:
#' 
#' %T>% makes available output from left hand side to console, but continues pipe
#' 
#' %<>% starts with an existing object, runs it through a pipe, and then replaces the original object with the result.
#' 
#' %$% is an awesome operator that makes the *arguments* of the left-hand side (wherever they are) available to the right-hand 
#' side, by their names.
#' 

############ ------------------------------------------------------| CurveBox Module |------------------------------------------------------- ############
#'
#' Following ui and server functions produce a single box with an optimal curve, if passed any node in the n-hierarchy. Will be repeatedly called from
#' the "Assess" module to produce a cascading list of plot outputs, two to a row, going from the highest levels to the lowest.

#' Special argument 'ID' is equivalent to the name of the node

curveboxUI <- function(id, ID) {
  
  ns <- NS(id)
  
  box(width = 6, title = ID, fluidRow(column(12, plotOutput(ns("curve")))),
      fluidRow(column(3, offset = 2, h4("General")), column(5, h4("VirtualGPA"))))

}

# 'ID' is the name of the n-hierarchy node; 'Path' is just the filepath to reload AGE

curvebox <- function(input, output, session, node, node_level, Path) {
  
  #' Reload AGE
  
  load(Path)
  
  atoms_in_node <- A$atoms$ID[A$atoms[[8 + node_level]] == node] %>% unlist %>% as.character
  
  node_opCurve <- rowSums(A$opCurve[atoms_in_node]) %>% data_frame(fp = A$opCurve$fp, tot = . )
  
  node_Logged <- coltime(A$Logged, names = atoms_in_node, colName = "tot") %>% cbind(fp = A$Logged$fp, tot = . )
  
  dayafter <- max(node_opCurve$fp) + as.difftime(1, units = "days")
  
  x <- c(node_opCurve$fp[1], node_opCurve$fp[node_opCurve$fp > node_opCurve$fp[1]], dayafter, dayafter, node_opCurve$fp[1])
  
  y <- c(as.difftime(0, units = "hours"), node_opCurve$tot[node_opCurve$fp >= node_opCurve$fp[1]],
         as.difftime(0, units = "hours"), as.difftime(0, units = "hours"))
  
  shade <- data.frame(x, y)
  
  #####
  
  if (length(atoms_in_node) == 0) {
    
  } else {
    
    node_Logged_2 <- node_Logged
    
    colnames(node_Logged_2) <- c("fp", "bcggplot")
    
    fulldat <- node_Logged_2[nrow(node_Logged_2),]
    
    output$curve <- renderPlot({
      
      ggplot(data = node_Logged_2, aes(x = fp, y = tot)) +  geom_polygon(data = shade, aes(x, y), alpha = .80) + 
        theme(axis.text.x = element_text(angle = 90, vjust = .5), panel.grid.minor.x = element_blank()) +
        scale_x_datetime(breaks = node_Logged$fp, labels = format(node_Logged$fp,  "%a %d"), name = NULL) + 
        scale_y_continuous(name = NULL) + geom_line(data = node_Logged_2, aes(x = fp, y = bcggplot), color = "steelblue", size = 1.5) + 
        geom_point(data = fulldat, aes(x = fp, y = bcggplot), color = "steelblue", size = 4)
      
    })
    
  }
  
}

############ ------------------------------------------------------| Assessment Module |------------------------------------------------------- ############
#'
#' ui and server produce Ascendance "Assess" module, enclosing the global productive time optimal curve at the top, within a tabBox (other optimal curves 
#' produced by CurveBox module, listed above)

assessUI <- function(id) {
  
  ns <- NS(id)
  
  node_levels <- A$iH %>% length %>% seq_len()
  
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
           
           {
             
             lapply(node_levels, function(i) {
               
               nodes <- A$iH[[i]]$ID %>% unlist
               
               lapply(nodes, function(j) {
                 
                 id <- paste0(i, "_", j, "_opCurve")
                 
                 curveboxUI(ns(id), id)
                 
               })
               
             })
             
           }
           
    )
    
  )
  
}

assess <- function(input, output, session, Path) {
  
  load(Path)
  
  ns <- session$ns
  
  opMod <- BuildOpCurve(A$iH[[length(A$iH)]], dim_extent = "time", dim_metricS = "time", units_extent = "hours", metricJump = "days")
  
  opMod[,-1] %>% rowSums
  
  dayafter <- max(A$opCurve$fp) + as.difftime(1, units = "days")
  
  x <- c(A$opCurve$fp[1], A$opCurve$fp[A$opCurve$fp > A$opCurve$fp[1]], dayafter, dayafter, A$opCurve$fp[1])
  
  opCurve_tot <- A$opCurve[,colnames(A$opCurve) != 'fp'] %>% rowSums %>% data_frame(fp = A$opCurve$fp, full = . )
  
  y <- c(as.difftime(0, units = "hours"), opCurve_tot$full[opCurve_tot$fp >= opCurve_tot$fp[1]],
         as.difftime(0, units = "hours"), as.difftime(0, units = "hours"))
  
  shade <- data.frame(x, y)
  
  #' Bind entry times back onto Logged, store in AGE, save AGE, then make data frame 
  #' of total time blocks.
  
  dat <- BuildProcLog(A$RawLog, starter = FALSE)
  
  fulldat <- lframe(dat[,-c(1,2)], colName = "full", method = "c-cum") %>% cbind(dat[,2], . ) %>% as_tibble
  
  output$hoursGlobal <- renderPlot({
    
    ggplot(data = opCurve_tot, aes(x = fp, y = full)) +  
      geom_polygon(data = shade, aes(x, y), alpha = .80) + 
      theme(axis.text.x = element_text(angle = 90, vjust = .5), panel.grid.minor.x = element_blank()) +
      scale_x_datetime(breaks = opCurve_tot$fp, labels = format(opCurve_tot$fp,  "%a %d"), name = NULL) + 
      scale_y_continuous(name = NULL) + 
      geom_line(data = Logged, aes(x = fp, y = full), color = "steelblue", size = 1.5) + 
      geom_point(data = fulldat, aes(x = fp, y = full), color = "steelblue", size = 4)
    
  })
  
  #' Calling curvebox module on each node in n-hierarchy
  
  node_levels <- A$iH %>% length %>% seq_len()
  
  for (i in node_levels) {
    
    nodes <- A$iH[[i]]$ID %>% unlist
    
    for (j in nodes) {
      
      callModule(curvebox, id = paste0(i, "_", j, "_opCurve"), node_level = i, node = j, Path = Path)
      
    }
    
  }
  
}