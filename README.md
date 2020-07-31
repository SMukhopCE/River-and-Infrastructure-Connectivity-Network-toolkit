# Overview

Contains vignettes and functions for creating River and Infrastructure Connectivity Network (RICON) data base.  To access the latest version of the dataset for the Colorado River Basin, please visit 
https://figshare.com/articles/River_and_Infrastructure_Connectivity_Network_RICON_/11849157  

River and Infrastructure Connectivity Network (RICON) – shows the dependency between streamgages and reservoirs as a concise edge list for the Colorado River Basin (CRB). The RICON dataset provides (1) a complete list of edges for the unidirectional network connecting streamgages, reservoirs and NHDPlusflowline features for CRB and (2) attributes of all nodes (reservoirs and streamgages) including their geospatial locations, unique identifiers, immediate upstream and downstream nodes, etc. The RICON data archive available at figshare contains the dataset tables in comma-separated values (CSV) format. Input files are 'sites_df.csv', 'nid_df.csv' and 'NHD_data.RData'. Output file is 'Edge_List.csv' 

Please see "vignettes/dataprep_CRB.Rmd" for input data preparation. The input data already is available as a part of RICON dataset for Colorado River Basin. 

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
Before proceeding with network creation, let’s look at the data.
```{r}
head(nid_df) 
#>       IDS                     NAME LONGITUDE LATITUDE NEAREST_LINE_ID   COMID
#> 1 Point.1           CO00361 BARREN -107.9522  39.0399         3230589 3230589
#> 2 Point.2 CO00641 UPPER HOTEL LAKE -107.9588  39.0499         3230397 3230397
#> 3 Point.3     CO00634 TWIN LAKE #1 -107.8422  39.0649         3230613 3230613
#> 4 Point.4      CO00334 ARCH SLOUGH -107.9755  39.0449         3230597 3230597
#> 5 Point.5   CO00331 ALEXANDER LAKE -107.9755  39.0383         3230577 3230577
#> 6 Point.6          CO00665 SAWMILL -106.0588  39.4733         1314643 1314643
#>   ARBOLATESU SNAP_DIST DISTFROMNODE DISTTONODE DIST_FROMHWKM
#> 1      0.744 133.81255     186.6774   562.9378     0.1810622
#> 2      3.209 131.67295     103.1490   451.1597     2.7578403
#> 3      0.939  52.88639     197.5622   128.1986     0.8108014
#> 4      0.603 138.94570     480.1914   129.1254     0.4738746
#> 5      8.743  83.45528      45.3045   948.7204     7.7942796
#> 6      5.476  95.59380    3971.3790  1534.9318     3.9410682
#>                                GEOMETRY POINTTYPE
#> 1 c(763751.486600966, 4325498.30305245)       dam
#> 2 c(763299.763531855, 4326654.90331456)       dam
#> 3 c(773230.747418582, 4328777.51969633)       dam
#> 4   c(761786.0943976, 4325977.70804057)       dam
#> 5 c(761795.097034096, 4325298.60253278)       dam
#> 6 c(925010.291645439, 4380990.26819213)       dam

head(sites_df)  
#>              IDS                                                        NAME
#> 1 Point.09010500 09010500 COLORADO RIVER BELOW BAKER GULCH NR GRAND LAKE, CO
#> 2 Point.09010501            09010501 COLO R BL BAKER GUL PLUS GRAND R D COLO
#> 3 Point.09010600                    09010600 ONAHU CREEK NEAR GRAND LAKE, CO
#> 4 Point.09011000                09011000 COLORADO RIVER NEAR GRAND LAKE, CO.
#> 5 Point.09011500   09011500 L COLUMBINE C AB SHADOW MTN LK, AT GRAND LK, CO.
#> 6 Point.09012400              09012400 TONAHUTU CREEK NEAR GRAND LAKE, COLO.
#>   LONGITUDE LATITUDE NEAREST_LINE_ID   COMID ARBOLATESU    SNAP_DIST
#> 1 -105.8570 40.32567         1234461 1234461     96.025 2.957439e-05
#> 2 -105.8570 40.32567         1234461 1234461     96.025 2.937379e-05
#> 3 -105.8448 40.32145         1234011 1234011     11.814 3.352742e-04
#> 4 -105.8576 40.21880         1234087 1234087    148.407 3.588026e-06
#> 5 -105.8339 40.25208         1234483 1234483      2.538 1.871722e-05
#> 6 -105.8195 40.26866         1234039 1234039     17.734 2.707803e-06
#>   DISTFROMNODE DISTTONODE DIST_FROMHWKM                              GEOMETRY
#> 1     110.6035 4734.02955     91.290970 c(937009.076588949, 4476615.67601351)
#> 2     110.7828 4733.85028     91.291150 c(937009.101223712, 4476615.49843755)
#> 3    7874.3745 3735.17458      8.078825 c(938076.500295248, 4476207.98145532)
#> 4    1305.9727  101.67942    148.305321 c(937644.012012138, 4464742.30066102)
#> 5    1604.7658  326.59674      2.211403 c(939446.566163658, 4468556.41768432)
#> 6    9260.2575   56.06928     17.677931  c(940570.18881963, 4470470.12967801)
#>   POINTTYPE
#> 1      gage
#> 2      gage
#> 3      gage
#> 4      gage
#> 5      gage
#> 6      gage
```
## Create network for Colorado River Basin 

Because of the recursive nature of network traversal,  it may take a long time to run this part, depending on the size of input data. 

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
### Find distance between any two points in the river network 

This is not the geographic distance between points, rather the river kilometers between the selected points. Using the edge list, one can easily calculate this distance using the starting node and target node information provided by users. As an example, distance calculation between Glen Caynon (*NID ID AZ10307*) and Hoover (*NID ID NV10122*) dams is shown here. The output is a list of edges traversed to reach Hoover from Glen Canyon. Distance between these two nodes are simply the summation of the vector elements of the output. 

```{r usage 1 find distance, echo=TRUE, eval=TRUE, include=TRUE}

hoover <- nid_df %>% filter(NAME %in% "NV10122 HOOVER") %>% select(IDS)
glencanyon <- nid_df %>% filter(NAME %in% "AZ10307 GLEN CANYON") %>% select(IDS)

dist_hoover_glencanyon <- find_distance(edges = edge_list_ricon, 
                                        point1 = as.character(hoover), # TO_NODE
                                        point2 = as.character(glencanyon), # FROM_NODE
                                        direction = "downstream") 

#> [1] "Starting from Point.820"
#> [1] "reached Point.339"


# Distance between points (Kilometers): 
sum(unlist(dist_hoover_glencanyon)) 
#> [1] 592.9389
```

For any question, readers are encouraged to contact [Sudarshana Mukhopadhyay](https://sudarshanamukhopadhyay.com/). 
