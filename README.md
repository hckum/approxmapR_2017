#Approxmap Shiny App

A GUI implementation of the Approxmap algorithm.

##User Guide

The left pane of the app is where you give the inputs (upload data, set the cutoff, etc.) and the right pane is where the results are displayed.
The steps for using the applet are discussed below (The points correspond to the input steps):

###Input Panel

1. **File Upload**: This is the data file that the user wants to analyze. The input must be a csv file with 3 columns: 
  - ID 
  - Date 
  - Item
2. **Data Aggregation**: This panel deals with aggregating the data into sets. For instance, if the aggregation level is "Calendar Month", all items in January are grouped as a set (and so on for Feb, Mar, Apr..). The start date is a panel that will appear for certain kinds of periods where you can specify when the program can start aggregating.
3. **Hierarchy File Upload**: This is an optional panel. You can upload a file for further aggregation at the item level. You can choose to, for example, group treatments into physiological, mental and others. Once the file is uploaded, the aggregation levels are populated according to the number of columns. The default aggregation is 1 which means, the data will be analyzed at the most granular level. There are certain standards that the hierarchical data file must adhere to:
  - It must be a csv file
  - The *first* column of the file must contain the elements in the "Item" column of the original data.
  - *All the elements* in the item column of the original data *must* be present in the hierarchical table
  - The agrregation moves from left to right
4. **Clustering**: The clustering algorithm used is kNN clustering. The k (number of nearest neighbours to look at) can be specified here. k defaults to 2.
5. **Cutoffs**: Cutoffs can be specified here. After the approxmap button is clicked, these are the only inputs that work simultaneously as they are changed  There are 3 cutoffs used:
  1. Noise cutoff: Given in terms of actual frequencies. Affects the frequency chart that will be displayed. It basically clears all the points that do not meet the cutoff in terms of te number of occurences.
  2. Variation cutoff: Mines the variation pattern (a superset of the consensus pattern). Since it is not as stringent as the consensus pattern, by definition the cutoff for variation pattern must be lower than the consensus pattern. This is automatically taken care in the program.
  3. Consensus cutoff: Mines the consensus pattern for the clusters. Specifies the consensus pattern threshold
6. **Get Approxmap**: Once the parameters are specified, you can click on this to get the patterns. You can play with the cutoffs after clicking on them but *for any other change in input (aggregation, hierarchy or knn) the button has to be clicked again to get the new approxmap*. This is because, the patterns are calcualted from the weighted sequence and hence do need a lot of computation whereas making the other inputs interactive would have slowed the program down as the user is just determining the parameters. 

###Output panel

1. **Data Tab**: Displays the input data before (left) and after (right) date and hierarchy aggregation. 
2. **Cluster Tab**: Empty for now.
3. **Consensus Patterns Tab**: 
  - Has c tabs (where c is the number of clusters)
  - For each cluster, the consensus and variation patterns, the frequency plot (to reevaulate the thresholds) and the weighted sequence are displayed.
  - All the patterns are color-coded (and size-coded) according to the strength of the signal. This means that items that have occured many times appear more prominently than the ones that haven't.
  
###Demo
1. Go to [http://ilangurudev.shinyapps.io/ApproxMap_Shiny/]
2. For the input data file, use "demo1.csv" found in the data folder of this github project [http://github.com/hckum/approxmapR/tree/master/ and navigate to the "data" folder] (Right-click and click on save link/target as)
3. For the hierarchical data, use "Tree.csv" in the same folder using the same method. Please ensure that you do not select "Tree - Copy.csv" as it is a hierarchical file with a few entries missing (to be used for testing).
4. Use any input parameters that you desire.
 
Feel free to leave your suggestions at [kum@tamu.edu] or [ilan50_guru@tamu.edu]
