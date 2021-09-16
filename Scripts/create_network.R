#' Creates a river network with point data
#'
#' @param points 
#' @param nodes 
#' @param NHDdata 
#' @param Flowline.df 
#'
#' @return
#' @export
#'
#' @examples
create_network <- function(points, nodes = NULL, NHDdata, Flowline.df = NULL ){
  # Creates a complete network with all data 
  ## INPUT 
  # points : list of point data frames 
  # nodes : n x n matrix with margin names for points names. Initial
  # value is NA 
  # NHDdata : list of 'flowlines' (PlusFlowlineVAA) and 'flownodes' (PlusFlow)
  # Flowline.df : output of "join_flow_data" function 
  ## OUTPUT 
  # connectivity = n x n matrix with margin names for points names.Rows [i] : child node, 
  # Columns [j] : Parent node, so that elemnt[i,j] : 1/0 (j is a parent node of i or not)
  # edges : n x n matrix same as connectivity. For each '1' in connectivity, edges has length 
  # of the edge connecting i and j 
  


  visited_index.list <- c()
  

 
  ########## data prep ########################################
  if (is.list(points)){
   
    points.df <- dplyr::bind_rows(points) %>% 
                  dplyr::select(-GEOMETRY)  %>% 
                  dplyr::mutate(NODENAME = IDS)
               
   } else{
     points.df <- as.data.frame(points)
   }
  
  if(is.null(nodes)){
    
    nodes <- matrix(NA,nrow = dim(points.df)[1], ncol = dim(points.df)[1],
                    dimnames = list(points.df$IDS, points.df$IDS))
    node_names <- dimnames(nodes)[[1]] 
    
  } else {
    # Combined data.frame 
    if(dim(points.df)[1] != dim(nodes)[1]) {stop("Dimension mismatch")} 
  }
  
  NHDdata <- lapply(NHDdata, function(x) rename_all(x,str_to_upper))
  
  Flowline.df <- join_flow_data(points[[1]],points[[2]],NHDdata$flowlines,"NEAREST_LINE_ID") 
   
  tmp <- Flowline.df %>% select(COMID,LEVELPATHI)
  
  # final point df 
  points.df <- points.df  %>% dplyr::left_join(tmp,by='COMID')  %>% dplyr::select(-ends_with(".y")) %>% 
    dplyr::rename_at(vars(ends_with(".x")),funs(str_remove(.,".x")))
  
  
  ###########################  step 1 ##########################################
  # #  headwater lines with nodes
  hwlines_with_nodes <- Flowline.df %>%  
    dplyr::filter((COMID %in% points.df$NEAREST_LINE_ID) & (STARTFLAG == 1)) %>% 
    dplyr::select(everything()) 
  
  hw_nodes <- points.df %>% dplyr::filter(NEAREST_LINE_ID %in% hwlines_with_nodes$COMID) %>% 
    dplyr::group_by(COMID) %>% dplyr::arrange(DIST_FROMHWKM)
  
 dupl_line0 <- table(hw_nodes$NEAREST_LINE_ID) 
 
# Populate nodes matrix 
 for (i in 1:length(dupl_line0))
   {
   tmp <- hw_nodes %>% dplyr::filter(COMID %in% names(dupl_line0)[i]) %>% 
     dplyr::arrange(DIST_FROMHWKM)
   
   tmp.index.list <- match(tmp$NODENAME, node_names) 
   
   visited_index.list <- c(visited_index.list, tmp.index.list)
   
   if (length(tmp.index.list)>1) { # if more than one point
     for (j in 2:length(tmp.index.list)){
       nodes[tmp.index.list[j], tmp.index.list[1:(j-1)]] <- 1 
     }
   } 
   # else {
   #   nodes[trib.index.list,ds.index.list] <- 1 
   # }
   # print(tmp.index.list)
   } 
rm(i,tmp,tmp.index.list)



# # first part of initial df 
dn_hw_nods <- hw_nodes %>%
                    dplyr::group_by(COMID) %>% 
                    dplyr::top_n(1,DIST_FROMHWKM)
# farthest from head water for each COMID 

    
 ########### Step 2  #####################################################   
  # nodes NOT visited in step 1  : Non head water nodes  
 #subset.point.df <- missed_nodes(visited_index.list) 

  # ALL HEAD WATER LINES 
  hwflowlines <- NHDdata$flowlines %>% 
                 dplyr::filter(STARTFLAG == 1) %>% 
                dplyr::select(everything())
    
  # downstream nodes AFTER head water lines with NO PARENT NODE   
  ds_line_wo_parent <-  Flowline.df  %>% 
                  filter(LEVELPATHI %in% hwflowlines$LEVELPATHI) %>% 
                  filter(STARTFLAG != 1) %>% 
                   select(everything()) 
  
  # Note : Grouping by LEVELPATHI 
  sub_df <- points.df %>% filter(NEAREST_LINE_ID %in% ds_line_wo_parent$COMID) %>% 
  group_by(LEVELPATHI) %>% arrange(DIST_FROMHWKM)
  
  #join with downstream points of head water lines 
  # these two will be updated after every downstream trace 
  sub_df <- bind_rows(sub_df, dn_hw_nods) 
  # points to start moving downstream at 
  sub_df <- sub_df %>% group_by(LEVELPATHI) %>% arrange(DIST_FROMHWKM) 
  
  duplline <- table(sub_df$LEVELPATHI) 
  
  ########## Step 3 ###########################################
  print("moving along main stem" )
  
  L0 <- driver_join_trib(initial_list = list(dupl_line = duplline,
                              initial_df = sub_df, 
                              initial_nodes = nodes,
                              index_list = unique(visited_index.list)), 
                         NHDdata = NHDdata, 
                         Flowline.df = Flowline.df,
                         points.df = points.df, tag = "major")
  

  ########## Step 4 check initial result ####################################
  # # other connections / divergence/ convergences / minor paths 
  print("checking for complex connections ")
  
  length(unique(L0$index_list)) # 
  # visited nodes 
  visited_index.list <- unique(L0$index_list)
  visited.nodes <- node_names[visited_index.list]
  missed.nodes <- node_names[-visited_index.list]
  
  
   tmp.nodes <- L0$initial_nodes 
   tmp.nodes[is.na(tmp.nodes)] <- 0

   missing_FromNode <- which(colSums(tmp.nodes) == 0)
   
   

  
  ######## Step 5 check for minor connections ##################################
   print("check for minor connections")
  # points to start moving downstream at 
  sub_df <- points.df %>% filter(NODENAME %in% missed.nodes) %>% 
              group_by(LEVELPATHI) %>% arrange(DIST_FROMHWKM) 
  
  dn_nodes <- points.df %>% filter(NODENAME %in% visited.nodes) %>% 
    group_by(LEVELPATHI) %>% top_n(1,DIST_FROMHWKM)
  
  missed_nodes <- points.df %>% filter(NODENAME %in% missing_FromNode) %>% 
    group_by(LEVELPATHI) %>% arrange(DIST_FROMHWKM) 
  
  sub_df <- bind_rows(sub_df, dn_nodes, missed_nodes) 
  # points to start moving downstream at 
  sub_df <- sub_df %>% group_by(LEVELPATHI) %>% arrange(DIST_FROMHWKM) 
  
  
  duplline <- table(sub_df$LEVELPATHI) 
  
  
  # # move along MINOR stem 
 
  L1 <- driver_join_trib(list(dupl_line = duplline,
                              initial_df = sub_df, 
                              initial_nodes = L0$initial_nodes,
                              index_list = visited_index.list),
                             NHDdata, Flowline.df,points.df,tag = "minor")
  
 
  
  
  # visited nodes 
  visited_index.list <- unique(L1$index_list)
  visited.nodes <- unique(node_names[visited_index.list])
  missed.nodes <- node_names[-visited_index.list] # empty 
 
  # Update nodes 
  nodes <- L1$initial_nodes
  
  nodes[is.na(nodes)] <- 0
  
  print("Creating edge list ...")
  
  check <- melt(nodes,id.vars = 1:ncol(nodes)) 
  check <- check %>% dplyr::filter(value != 0) # 
  
  
  
  Edge_List <- data.frame(FROM_NODE = check[,2], 
                          TO_NODE = check[,1]) #, 
                        #  Edgeval = check[,3]) equals 1 
  
  Edge_List <- Edge_List %>% dplyr::mutate(EDGE_LENGTHKM = NA) 
  NHDFlowlines_Visited <- list()
  

  
  
  for(i in 1 : dim(Edge_List)[1]){
    
    x <- create_edges(df = as.matrix(Edge_List[i,1:2]), 
                      nodeinfo = points.df, 
                      NHDdata = NHDdata, 
                      Flowline.df = Flowline.df)
    
    Edge_List$EDGE_LENGTHKM[i] <- x$EDGELENGTH 
    NHDFlowlines_Visited[[i]] <- x$NHDflowlines_btw
    
  #  print(i)
    rm(x)
  }
  
  
  check_edge <- which(Edge_List$EDGE_LENGTHKM < 0)
  
  if(length(check_edge) > 0){Edge_List <- Edge_List[- check_edge,]}
  # Remove edge with <0 length ! these are few duplicate edges 
  
  
  # add more attributes from points.df  
  # Pointtype 
  tmp1 <- match(Edge_List$FROM_NODE,points.df$NODENAME)
  Edge_List$FROM_NODETYPE <- points.df$POINTTYPE[tmp1] 
  
  tmp2 <- match(Edge_List$TO_NODE,points.df$NODENAME)
  Edge_List$TO_NODETYPE <- points.df$POINTTYPE[tmp2]
  
  # COMID 
  tmp1 <- match(Edge_List$FROM_NODE,points.df$NODENAME)
  Edge_List$FROM_COMID <- points.df$NEAREST_LINE_ID[tmp1] 
  
  tmp2 <- match(Edge_List$TO_NODE,points.df$NODENAME)
  Edge_List$TO_COMID <- points.df$NEAREST_LINE_ID[tmp2] 
  

  # LevelPathID 
  tmp1 <- match(Edge_List$FROM_NODE,points.df$NODENAME)
  Edge_List$FROM_LEVELPATHI <- points.df$LEVELPATHI[tmp1] 
  
  tmp2 <- match(Edge_List$TO_NODE,points.df$NODENAME)
  Edge_List$TO_LEVELPATHI <- points.df$LEVELPATHI[tmp2] 
  
  
  # Node Name 
  tmp1 <- match(Edge_List$FROM_NODE,points.df$NODENAME)
  Edge_List$FROM_NODE_NAME <- points.df$NAME[tmp1]
  
  
  tmp2 <- match(Edge_List$TO_NODE,points.df$NODENAME)
  Edge_List$TO_NODE_NAME <- points.df$NAME[tmp2]
  
  
  
  # RETURN 
  return(list(edge_list = Edge_List, 
              connectivity = nodes,
              combined_Points = points.df,
              joined_flowline = Flowline.df, 
              visited_flowline = NHDFlowlines_Visited, # for each edge 
              visited_index.list = visited_index.list, 
              visited.nodes = visited.nodes, 
              missed.nodes = missed.nodes))
}
