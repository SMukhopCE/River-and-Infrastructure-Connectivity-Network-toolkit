#' @title creates edges from connectivity matrix 
#'
#' @param df 
#' @param nodeinfo 
#'
#' @return to from edge matrix with edge lengths in KM 
#' 
#' @import dplyr 
#' 
#' @export
#'
#' @examples
create_edges <- function(df, nodeinfo,NHDdata, 
                         Flowline.df,it.max = 200){ 
  
  if(is.data.frame(df)){
    from_node <- df[1] %>% mutate_if(is.factor,as.character)
    to_node <- df[2] %>% mutate_if(is.factor,as.character)
  } else {
    from_node <- df[1]
    to_node <- df[2] 
  }
 
  
  PlusFlowlineVAA <- NHDdata$flowlines  %>% 
                     dplyr::rename_all(stringr::str_to_upper)
  PlusFlow <- NHDdata$flownodes %>% 
                      dplyr::rename_all(stringr::str_to_upper)
  
  # # formula # Edge length = from_node_dist + to_node_dist + <length of
  # flowlines in between> 
  
  from_node_attr <- nodeinfo %>% 
                      dplyr::filter(NODENAME %in% c(from_node)) %>% 
                      dplyr::left_join(PlusFlowlineVAA,by = "COMID") %>% 
                      dplyr::select(-ends_with(".y")) %>% 
                      dplyr::rename_at(vars(ends_with(".x")),funs(str_remove(.,".x")))
                    
  to_node_attr <- nodeinfo %>% 
                      dplyr::filter(NODENAME %in% c(to_node)) %>% 
                      dplyr::left_join(PlusFlowlineVAA,by = "COMID") %>% 
                      dplyr::select(-ends_with(".y")) %>% 
                      dplyr::rename_at(vars(ends_with(".x")),funs(str_remove(.,".x")))
       
  
  if(from_node_attr$COMID == to_node_attr$COMID){
    # On same flowline : just substract distance from one end 
    
    from_node_dist <- nodeinfo %>% dplyr::filter(NODENAME %in% c(from_node)) %>% 
      dplyr::select(DIST_FROMHWKM)
    
    to_node_dist <- nodeinfo %>% dplyr::filter(NODENAME %in% c(to_node)) %>% 
      dplyr::select(DIST_FROMHWKM) 
    
    EdgeLengthKM <-  unlist(to_node_dist) - unlist(from_node_dist)
    
    flowlines_btw <- PlusFlowlineVAA %>% 
      dplyr::filter(COMID %in% from_node_attr$COMID)  
    
  }else if ( (from_node_attr$COMID != to_node_attr$COMID) & 
             (from_node_attr$LEVELPATHI == to_node_attr$LEVELPATHI) ) {
    # different flowline but same river 
    # find lines with this levelpathid and hydroseq in between from/to lines 
    
    flowlines_btw <- PlusFlowlineVAA %>% 
                     dplyr::filter(LEVELPATHI %in% from_node_attr$LEVELPATHI) %>% 
                     dplyr::filter( (HYDROSEQ < from_node_attr$HYDROSEQ) & 
                                    (HYDROSEQ > to_node_attr$HYDROSEQ)) 
    
    len_lines_bw <- 1000*sum(flowlines_btw$LENGTHKM)  # in meter
    
    from_node_dist <- nodeinfo %>% dplyr::filter(NODENAME %in% c(from_node)) %>% 
      dplyr::select(DISTTONODE) # in meter 
    
    to_node_dist <- nodeinfo %>% dplyr::filter(NODENAME %in% c(to_node)) %>% 
      dplyr::select(DISTFROMNODE)  # in meter
    
    # edge length in km 
    EdgeLengthKM <-  0.001*(unlist(to_node_dist) + len_lines_bw + unlist(from_node_dist))
    
    
  } else if ( (from_node_attr$COMID != to_node_attr$COMID) & 
              (from_node_attr$LEVELPATHI != to_node_attr$LEVELPATHI) ) {
    
    
    # if( abs( to_node_attr$STREAMORDE - from_node_attr$STREAMORDE) <= 1 ) {
    #   
    #   flowlines_btw1 <- PlusFlowlineVAA %>% 
    #     dplyr::filter(LEVELPATHI %in% from_node_attr$LEVELPATHI) %>% 
    #     dplyr::filter( (HYDROSEQ < from_node_attr$HYDROSEQ)  & 
    #                      (HYDROSEQ > to_node_attr$HYDROSEQ)) %>% 
    #     dplyr::arrange(desc(HYDROSEQ))
    #   
    #   
    #   last_hyseq <- flowlines_btw1$DNHYDROSEQ[dim(flowlines_btw1)[1]] 
    #   
    #   flowlines_btw2 <- PlusFlowlineVAA %>% 
    #     dplyr::filter(LEVELPATHI %in% to_node_attr$LEVELPATHI) %>% 
    #     dplyr::filter( (HYDROSEQ <= last_hyseq) & 
    #                      (HYDROSEQ > to_node_attr$HYDROSEQ)) 
    #   
    #   
    #   flowlines_btw <- dplyr::bind_rows(flowlines_btw1, flowlines_btw2) 
    #   
    #   
    #   
    # } else{
      # Keep moving till downstream upto it.max till to_node_attr$HYDROSEQ is found
      
      # Original two lines 
        flowlines_btw <-  PlusFlowlineVAA %>%  
                          dplyr::filter(HYDROSEQ %in% c(to_node_attr$HYDROSEQ,
                                                        from_node_attr$HYDROSEQ)) 
      
        to_hyseq <- to_node_attr$HYDROSEQ 
        from_hyseq <- from_node_attr$HYDROSEQ 
        
        from_l <-  PlusFlowlineVAA %>% 
          dplyr::filter(LEVELPATHI %in% c(from_node_attr$LEVELPATHI, 
                                          from_node_attr$UPLEVELPAT, 
                                          to_node_attr$LEVELPATHI, 
                                          to_node_attr$DNLEVELPAT) ) %>% 
          dplyr::filter( (HYDROSEQ <= from_hyseq)  & 
                           (HYDROSEQ >= to_hyseq)) %>% 
          dplyr::arrange(desc(HYDROSEQ))
   
    
      
     #  while ((it <= it.max) | to_hyseq != from_hyseq ) {
       
       
       tmp <- which(from_l$UPHYDROSEQ == from_hyseq) 
       
       if ( length(tmp) <= 1){ 
         tmp1 <-   which(from_l$HYDROSEQ == from_node_attr$DNHYDROSEQ)
         if (length(tmp1) > 0 ){
           check_lines <-  which(from_l$HYDROSEQ <= from_l$HYDROSEQ[tmp1]) 
           if (length(check_lines) > 1  ) {
             to_l <- from_l[check_lines, ]
             if(any(to_l$COMID == to_node_attr$COMID)){
               flowlines_btw <- bind_rows(flowlines_btw, to_l) %>% dplyr::distinct()
             } 
           }
         }
       } 
        
        
  #  } # Compare Stream Order 
    
    flowlines_btw <- flowlines_btw %>% dplyr::filter(!COMID %in% c(from_node_attr$COMID,
                                                            to_node_attr$COMID))
   
    len_lines_bw <- 1000*sum(flowlines_btw$LENGTHKM)  # in meter
    
    from_node_dist <- nodeinfo %>% dplyr::filter(NODENAME %in% c(from_node)) %>% 
      dplyr::select(DISTTONODE) # in meter 
    
    to_node_dist <- nodeinfo %>% dplyr::filter(NODENAME %in% c(to_node)) %>% 
      dplyr::select(DISTFROMNODE)  # in meter
    
    # edge length in km 
    EdgeLengthKM <-  0.001*(unlist(to_node_dist) + len_lines_bw + unlist(from_node_dist))
    
    
  }          
  
  return( list ( EDGELENGTH =  as.numeric(unlist(EdgeLengthKM)) ,
                 NHDflowlines_btw = flowlines_btw)    )
} 

#' @title find_distance 
#' @description Calculates distance between points using edge list of the network. 
#' 
#' @param edges edge list 
#' @param point1 ToNode 
#' @param point2 FromNode 
#' @param direction upstream or downstream 
#'
#' @return
#' @export
#'
#' @examples
find_distance <- function(edges, point1, point2, direction="downstream"){ 
  # Check if points belong to the edge list 
  
  
  edges <- edges %>% dplyr::rename_all(stringr::str_to_upper)
  
  direction <- tolower(direction)
  
  dist_p1_p2 <- list() #numeric(length(point2))
  for(i in 1: length(point2)) { 
    
    thisNode <- point2[i] # 
    print(paste0("Starting from ",thisNode)) 
    
    len <- dist_nodes(point1,edges,thisNode, 
                      direction,c())
    
    dist_p1_p2[[i]] <- len
    
  }
  return(dist_p1_p2)
}

#' Title
#'
#' @param pt # TO_NODE
#' @param e 
#' @param p1 # FROM_NODE
#' @param direction 
#' @param out 
#'
#' @return
#' @export
#'
#' @examples
dist_nodes <- function(pt,e,p1,direction,out){
# Always move towards downstream 
  
  direction <- tolower(direction)
  e <- e %>% dplyr::rename_all(stringr::str_to_upper) 
  
  #print(paste0(p1," --> "))
  
  if (direction == "upstream")  
  {
    tmp1 = p1 
    tmp2 = pt 
    # flip points 
    pt = tmp1 
    p1 = tmp2 
    direction = "downstream"
  } 
  
  
  node_attr <- e %>% 
    filter(FROM_NODE %in% p1) %>% mutate(NODE = TO_NODE) %>% 
    select(NODE,EDGE_LENGTHKM) %>% 
    mutate_if(is.factor,as.character) 
  
  
  check_pt <- node_attr %>% filter(NODE %in% pt) 
  # print(check_pt)
  
  if(dim(check_pt)[1] == 0){
    # no match : update 
    
    new_p1 <- node_attr$NODE 
    add <- node_attr$EDGE_LENGTHKM
    names(add) <- new_p1
    out <- c(out,add)
    # names(out) <- c(names(out), node_attr$NODE)
    # recursion 
    dist_nodes(pt,e,new_p1,direction,out)
    
  } else if (dim(check_pt)[1] == 1){ 
    # exact match found 
    add <- node_attr$EDGE_LENGTHKM
    names(add) <- node_attr$NODE 
    print(paste0("reached ",names(add)))
    out <- c(out,add)
    return(out)
  } else{
    # stop 
    return(out)
  }
}
