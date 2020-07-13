# Reproduce the Colorado River Basin network using River and Infrastructure Connectivity Network (RICON) dataset
Contains vignettes and functions for creating River and Infrastructure Connectivity Network (RICON) data base.  To access the latest version of the dataset for the Colorado River Basin, please visit 
https://figshare.com/articles/River_and_Infrastructure_Connectivity_Network_RICON_/11849157  

River and Infrastructure Connectivity Network (RICON) – shows the dependency between streamgages and reservoirs as a concise edge list for the Colorado River Basin (CRB). The RICON dataset provides (1) a complete list of edges for the unidirectional network connecting streamgages, reservoirs and NHDPlusflowline features for CRB and (2) attributes of all nodes (reservoirs and streamgages) including their geospatial locations, unique identifiers, immediate upstream and downstream nodes, etc. The RICON data archive available at figshare contains the dataset tables in comma-separated values (CSV) format. Input files are 'sites_df.csv', 'nid_df.csv' and 'NHD_data.RData'. Output file is 'Edge_List.csv' 


Using the following steps one can reproduce the River and Infrastructure Connectivity Network (RICON) dataset for Colorado River Basin :  


## Download RICON data : 
Once you have downloaded the  [RICON](https://figshare.com/articles/dataset/River_and_Infrastructure_Connectivity_Network_RICON_/11849157) data in your local directory, you have the following files :  
1. nid_df.csv, 2. sites_df.csv, 3. NHD_data.RData and 4. Edge_list.csv 

The input data set already contains all the information neccessary to create the edge list of dams and streamgages for Colorado River Basin. Please update "ricon_data_path" argument before running each code chunk in the demo file. Data files must be downloaded from figshare and saved in a local directory. 

```{r load ricon data, echo=TRUE, eval=TRUE}
nid_df <- read.csv(paste0(ricon_data_path,"nid_df.csv"),stringsAsFactors = F)
sites_df <- read.csv(paste0(ricon_data_path,"sites_df.csv"),stringsAsFactors = F)

load(paste0(ricon_data_path,"NHD_data.RData"))

# For comparison 
edge_list_ricon <- read.csv(paste0(ricon_data_path,"Edge_List.csv"),stringsAsFactors = F)

```

##  Check input data sets : 
The point data must have these attributes : (i) "IDS", (ii) "NAME", (iii) "LONGITUDE", (iv) "LATITUDE", (v)"NEAREST_LINE_ID", (vi) "COMID", (vii) "ARBOLATESU", (viii)      "SNAP_DIST", (ix) "DISTFROMNODE", (x) "DISTTONODE", (xi) "DIST_FROMHWKM", (xii)  "GEOMETRY", (xiii) "POINTTYPE". "IDS" are the unique identifier for each point and  should be of character class. "NAME" is the "STATION_NAME" and "DAM NAME" of respective selected points, though this can be updated by users to any character. "DISTFROMNODE" and "DISTTONODE"  are distances, in meters, from starting nodes and terminal nodes of each NHDFlowline onto which a point is snapped. "NEAREST_LINE_ID" is the "COMID" of the nearest NHDFlowline (same as "COMID"), *__always measured along the flowline__*. "SNAP_DIST" is the distance in meter, by which true location of the points needed to be adjusted to make them perfectly intersect the nearest nhdflowline. "ARBOLATESU" is another [NHDPlusV2](https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data#v2datamap) attribute that is defined, for each NHDFlowline as the summation of its own length and lengths of every upstream features. "DIST_FROMHWKM" for a point is calculated based on its distance to the terminal nodes and ARBOLATESUM of the nearest nhdflowline. Geographical locations of each point is stored as LONGITUDE and LATITUDE (in degrees). The "GEOMETRY" column contains the planner coordinates (saved as characters) of each point that is used to for all distance calculations discussed before. For Colorado River Basin, UTM zone 12 is used when projecting the points to planner coordinates. 

NHD_data.RData contains NHDPlusV2 data used in the analysis, combined for water resources regions 14 and 15. 

## Create network for Colorado River Basin 

Because of the recursive nature of network traversal,  it may take long time to run this part, depending on the size of input data. 

```{r create network, echo=TRUE, eval=FALSE, include=TRUE}
# this takes ~ 20 min or more depending on the machine. 
tic()
 
result <- create_network(points = list(dams = nid_df,sites = sites_df), 
                          nodes = NULL,
                          NHDdata = list(flowlines = PlusFlowlineVAA, 
                                         flownodes = PlusFlow),
                          Flowline.df = NULL)

toc()  # 
``` 

The "result_network" is a list containing the following elements:  

1.  "edge_list" contains the columns - 
    - FROM_NODE 
    - TO_NODE 
    - EDGE_LENGTHKM 
    - FROM_NODETYPE
    - TO_NODETYPE
    - FROM_COMID 
    - TO_COMID 
    - FROM_LEVELPATHI
    - TO_LEVELPATHI
    - FROM_NODE_NAME 
    - TO_NODE_NAME 
2.  "connectivity" 
3.  "combined_Points" 
4.  "joined_flowline"
5.  "visited_flowline"
4.  "visited_index.list"
5.  "visited.nodes"
6.  "missed.nodes"

The first item is a list of edges in the river network for the Colorado River Basin, with dams and gages represented as *node*s of the network. The column names are self explanatory. Second item "connectivity" is a sparse connectivity matrix of the network - from which edge list is constructed. Combined points is a dataframe containing all point data. "joined_flowline" is a large list which is generated while calculating length of edges for the raw edge list. Each element of the list "joined_flowline" contains a dataframe of the flowlines (from PlusflowlineVAA) that are traversed to reach "TO_NODE" from "FROM_NODE" of each each. Items 4 through 6 of the output are checks for missed points etc.

## Usage of Edge List 
# Find distance between any two points in the river network 

This is not the geographic distance between points, rather the river kilometers between the selected points. Using the edge list, one can easily calculate this distance using the starting node and target node information provided by users. As an example, distance calculation between Glen Caynon (*NID ID AZ10307*) and Hoover (*NID ID NV10122*) dams is shown here. The output is a list of edges traversed to reach Hoover from Glen Canyon. Distance between these two nodes are simply the summation of the vector elements of the output. 

```{r usage 1 find distance, echo=TRUE, eval=TRUE, include=TRUE}

hoover <- nid_df %>% filter(NAME %in% "NV10122 HOOVER") %>% select(IDS)
glencanyon <- nid_df %>% filter(NAME %in% "AZ10307 GLEN CANYON") %>% select(IDS)

dist_hoover_glencanyon <- find_distance(edges = edge_list_ricon, 
                                        point1 = as.character(hoover), # TO_NODE
                                        point2 = as.character(glencanyon), # FROM_NODE
                                        direction = "downstream") 

# Distance between points (Kilometers): 
sum(unlist(dist_hoover_glencanyon)) 

```

For any question, readers are encouraged to contact [Sudarshana Mukhopadhyay](https://sudarshanamukhopadhyay.com/). 
