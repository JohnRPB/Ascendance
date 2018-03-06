

### Must be passed object of type 'expression'; the [[1]] next to "module" retrieves the function name from the expression list

####### The ultimate dynamic module caller
dynamicModuleCaller <- function(module, id, ...) {
  
  argList <- list(id = id, module = module[[1]]) %>% append(., list(...))
  
  {
    
    substitute(
      
      do.call(callModule, args = argList),
      
      list(argList = argList)
      
    )
    
  }
  
}
#######
dynamicModuleCaller(module = expression(setup_trackers), id = "0", proceed = "yes")

####### Simple makeModuleList

replicateModules <- function(module, ids, ...) {
  
  lapply(ids, function(i) {
    
    argList <- list(module, i) %>% append(. , list(...))
    
    do.call(dynamicModuleCaller, args = argList)
    
  })
  
}
replicateModules(expression(setup_trackers), ids = list("0", "1"), proceed = "yes")

####### 
makeModuleList <- function(moduleList, idList, argList) {
  
  IndexVector <- moduleList %>% length %>% seq_len
  
  lapply(IndexVector, function(i) {
    
    argList <- list(moduleList[[i]], idList[[i]]) %>% append(., argList[[i]])
    
    do.call(dynamicModuleCaller, args = argList)
    
  })
}

moduleList <- list(expression(setup_trackers), expression(setup_goals))
argList <- list(
  list(proceed = "yes", no = 0), list()
)

makeModuleList(moduleList, idList, argList)


