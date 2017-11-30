# GarbageOperation
> data.Kitchen.3
$nbTrucks
[1] 2

$truckCapacity
[1] 80

$nbNodes
[1] 13

$nbCustomers
[1] 12

$demands
 [1] 12  7  6  6  2  3 12  9  1 12  1  3

$distanceMatrix
      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
 [1,]    0 2353 2618 2256 2376 2308 2098 2121 2126  2317  2407  2423  2119
 [2,] 2443    0 1621  600 1108 1448 1054 1107 1370  1763  1603  1515  1966
 [3,] 2620 1613    0 1516  873 1137 1146 1200 1059   513   357   270   834
 [4,] 2367  468 1640    0 1033 1372  978 1032 1294  1687  1598  1559  1890
 [5,] 2429 1144  900 1048    0  359  571  561  281   869   779   740  1013
 [6,] 2340 1292 1062 1195  349    0  482  472  192  1030   940   902  1174
 [7,] 2192  956 1141  859  534  609    0   92  427  1188  1098  1060  1391
 [8,] 2210 1012 1197  916  590  627   88    0  445  1244  1155  1116  1447
 [9,] 2148 1192  962 1096  250  182  290  280    0   930   841   802  1074
[10,] 2305 1698  437 1602  817 1070 1231 1272  992     0    89   285   477
[11,] 2439 1613  347 1516  732  995 1146 1200  917   461     0   130   653
[12,] 2425 1564  323 1467  718  982 1097 1151  904   319   106     0   639
[13,] 2153 1774  641 1677  919 1134 1307 1336 1057   254   343   387     0

function model(){
  routeSequences[k in 0..nbTrucks-1] <- list(nbCustomers);      // decision variable
  constraint partition[k in 0..nbTrucks-1](routeSequences[k]);  // visited by exaclty one route
  trucksUsed[k in 0..nbTrucks-1] <- count(routeSequences[k]) > 0;  
  nbTrucksUsed <- sum[k in 0..nbTrucks-1](trucksUsed[k]);
  for [k in 0..nbTrucks-1] {
  local sequence <- routeSequences[k];
  local c <- count(sequence);
  // The quantity needed in each route must not exceed the truck capacity
  routeQuantity[k] <- sum(0..c-1, i => demands[sequence[i]]);
  constraint routeQuantity[k] <= truckCapacity;
  // Distance travelled by truck k
  routeDistances[k] <- sum(1..c-1, i => distanceMatrix[sequence[i-1]+2][sequence[i]+2]) +
  (c > 0 ? (distanceMatrix[0][sequence[0]+2] + distanceMatrix[sequence[c-1]+2][1]) : 0);
  }
  // Total distance travelled
  totalDistance <- sum[k in 0..nbTrucks-1](routeDistances[k]);
  // Objective: minimize the number of trucks used, then minimize the distance travelled
  minimize nbTrucksUsed;   
  minimize totalDistance;
}
