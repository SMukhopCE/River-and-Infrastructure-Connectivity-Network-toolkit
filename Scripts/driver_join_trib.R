#' Title
#'
#' @param initial_list 
#' @param NHDdata 
#' @param Flowline.df 
#' @param points.df 
#' @param tag 
#'
#' @return
#' @export
#'
#' @examples
driver_join_trib <- function(initial_list , NHDdata, Flowline.df,points.df,tag = "major")
{
  # calls join_tributary
  # by default moves along mainsteam
  # change tag for checking minor connections 
  # returns updated "nodes", nextdf 
  
  dupl_line <- initial_list$dupl_line
  initial_df  <- initial_list$initial_df
  initial_nodes  <- initial_list$initial_nodes
  new.index.list <- initial_list$index_list
  
  node_names <- dimnames(initial_nodes)[[1]] 
  nextdf <- data.frame()
  tmp.df <- data.frame()

 # cat("Moving downstream across ", length(dupl_line)," levepaths along ", tag, "flow path \n")
    for (i in 1:length(dupl_line)) {
      # sort points on this river by distance from head water in KM 
      tmp <- initial_df %>% 
                 filter(LEVELPATHI %in% names(dupl_line)[i]) %>% 
                 arrange(DIST_FROMHWKM) 
      
      # Downstream most point in this river  
      tmp_ds <- tmp %>% group_by(LEVELPATHI) %>% top_n(1,DIST_FROMHWKM)
      
      tmp.index.list <- match(tmp$NODENAME, node_names)  # indices to fill 
      
      ds.index.list <- match(tmp_ds$NODENAME, node_names) # FromNode index to start  
      new.index.list <- c(new.index.list,tmp.index.list) # Update 
      
      # First find out the nhdflowline COMID to start downstream tracing 
      if(length(tmp.index.list)>1) # Have more than 1 point ? 
      { 
        for (j in 2:length(tmp.index.list))
        {
          initial_nodes[tmp.index.list[j], tmp.index.list[(j-1)]] <- 1 # Populate nodes 
          
          # at the downstream mode node, check for connection / divergence 
          if (j == length(tmp.index.list))
            # move downstream 
          {
            tmp.nhd <- Flowline.df %>% filter( COMID %in% tmp$COMID[length(tmp$COMID)]) %>% 
              select(everything())   }
        }
      } else  { 
        # Have just one point on this river ? 
        # move downstream 
        tmp.nhd <- Flowline.df %>% filter( COMID %in% tmp$COMID[length(tmp$COMID)]) %>% 
          select(everything()) 
        }
      
      
      if (tmp.nhd$DNDRAINCOU > 0) # call join_tributary 
      { 
        trib.df <- join_tributary(this.nhd = tmp.nhd, 
                                  PlusFlow = NHDdata$flowlines,
                                  PlusFlowlineVAA = NHDdata$flownodes,
                                  flowlines.df = Flowline.df,
                                  out = c(),tag)
      } 
      
      if (isTRUE(exists("trib.df"))) {
        
        # Update if tributaries exist 
        trib <- points.df %>% filter(NEAREST_LINE_ID %in% trib.df$COMID)
        trib.index.list <- match(trib$NODENAME, node_names) 
        new.index.list <- c(new.index.list,trib.index.list)
        if(length(trib.index.list) > 1 )
        {
          trib.index.list <- c(ds.index.list,trib.index.list)
          for (k in 2:length(trib.index.list))
          {
           # cat("From Node :",trib.index.list[k-1],"To Node :",trib.index.list[k],'\n')
            initial_nodes[trib.index.list[k], trib.index.list[(k-1)]] <- 1 
          }
          
        } else {
         # cat("From Node :",ds.index.list,"To Node :",trib.index.list,'\n')
          initial_nodes[trib.index.list,ds.index.list] <- 1 
        }
        tmp.df <- trib %>% group_by(LEVELPATHI) %>% top_n(1,DIST_FROMHWKM)    
      }
     print(paste("Completed i = ",i))
    } # End of loop i 
  
  
    nextdf <- bind_rows(nextdf,tmp.df)
    nextdf <- nextdf %>% group_by(LEVELPATHI) %>% arrange(DIST_FROMHWKM) 
    dline <- table(nextdf$LEVELPATHI) 
    
  outlist <-   list(dupl_line = dline,
                initial_nodes = initial_nodes,
                initial_df = nextdf, 
                index_list = new.index.list)
   
  return(outlist)
 
} 



#' Title
#'
#' @param this.nhd 
#' @param PlusFlow 
#' @param PlusFlowlineVAA 
#' @param flowlines.df 
#' @param out 
#' @param tag 
#'
#' @return
#' @export
#'
#' @examples
join_tributary <- function(this.nhd,PlusFlow,PlusFlowlineVAA,flowlines.df,out,tag){
  # moves downstream till a node is found 
  print(this.nhd$COMID)
  FROMNODE <- this.nhd$FROMNODE
  TONODE <- this.nhd$TONODE
  
  FROMNODEAtr <- PlusFlowlineVAA %>% filter(NODENUMBER %in% FROMNODE) %>% select(everything())
  TONODEAtr <- PlusFlowlineVAA %>% filter(NODENUMBER %in% TONODE) %>% select(everything())
 # print(TONODEAtr) 
  # check for junction nodes ---------------------------------------------------   
  if(dim(TONODEAtr)[1] > 1) { # junction : divergent/complex 
    if (tag == "major") {
      # Must move along main flowline 
      ro <-   which(TONODEAtr$FROMLVLPAT != TONODEAtr$TOLVLPAT) # only keeps main flow line
      TONODEAtr <- TONODEAtr[-ro,]
    } else if (tag == "minor") {
      # remove main stem only if current flowline is a minor flowpath 
      # meanling moving from minor path to major 
      if((dim(TONODEAtr)[1] > 1)) {
        r1 <- which(this.nhd$LEVELPATHI == TONODEAtr$FROMLVLPAT) # is current path the main flowline? 
        ro <- which(TONODEAtr$FROMLVLPAT == TONODEAtr$TOLVLPAT) #  main flow line
        
        print(paste0("r1 = ",r1))
        print(paste0("ro = ",ro))
          
        if(length(r1)>0 & length(ro)> 0 & r1!=ro){
            TONODEAtr <- TONODEAtr[-ro,]
          }else{
            TONODEAtr <- TONODEAtr
          }
        }
        
      downstr.levelpathIds <- TONODEAtr$TOLVLPAT 
     
      lines.with.pts <- flowlines.df %>% filter(LEVELPATHI %in% downstr.levelpathIds) %>% 
                        filter( ! is.na(POINTTYPE )) %>% 
                        filter(ARBOLATESU > this.nhd$ARBOLATESU) 
      
      
      
      if(dim(lines.with.pts)[1] > 1){ 
       # print("picking current line ") 
        
         # print(TONODEAtr) 
         # print(downstr.levelpathIds)
         # 
        TONODEAtr <- TONODEAtr %>% 
          filter(FROMCOMID %in% this.nhd$COMID) %>% 
          select(everything()) 
      } else { 
      #  print("picking minor flowpath ")
        # print(TONODEAtr) 
        # print(downstr.levelpathIds)
        # 
        TONODEAtr <- TONODEAtr %>% 
          filter(FROMCOMID %in% lines.with.pts$COMID) %>% 
          select(everything()) 
       
      } 
    }
      
  }
  #-----------------------------------------------------------------------------
  if  (dim(TONODEAtr)[1]==0) {   
    print("stop if data not found")
    return(out)} else if(dim(TONODEAtr)[1] == 1 && TONODEAtr$DIRECTION == 713)  { # Stop if reached an isolated network or coastline 
     print("Have reached network end")
    return(out) } else if (is.null(TONODEAtr$DIRECTION) | 
                           (TONODEAtr$DIRECTION == 714 | #& 
                            TONODEAtr$TOCOMID == 0))   {  # Fix here!  
    print("Have reached coastline ")
    return(out)
      }  else if (is.null(this.nhd$DNDRAINCOU)) {  
        # stop if DNDRAINCOUnt == 0 
    print("reached end of this stream")
    return(out) 
    }   else  { 
          # move downstream 
    #  print("compare HydroSeq of TONODE")
              if(TONODEAtr$FROMHYDSEQ > TONODEAtr$TOHYDSEQ) {
                downstreamCOMID <- TONODEAtr$TOCOMID
              } else {
                downstreamCOMID <- TONODEAtr$FROMCOMID
              }
    
    downstream.Flowline <- PlusFlow %>% 
      filter(COMID %in% downstreamCOMID) %>% 
      select(everything())
    
    out <- flowlines.df %>% filter(COMID %in% downstreamCOMID) 
    }  # 
    #If flowline with a node is found, STOP 
  if ((dim(out)[1] > 0 ) & !all(is.na(out$POINTTYPE))){
           # if ((dim(out)[1] > 0 ) & (tag == "major") & !all(is.na(out$POINTTYPE)) ){
    print(paste0("stopping at ",out$COMID))
              return(out) 
            } else 
            { # else move downstream 
#              print(".. recursion ..")
              out <- join_tributary(downstream.Flowline,PlusFlow,PlusFlowlineVAA,flowlines.df,out,tag)  
              
            }
    
   
  
  
  return(out)
}
