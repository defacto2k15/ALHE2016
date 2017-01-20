require("lattice")

run = function()
{
  XMIN <- 0
  XMAX <- 1000
  YMIN <- 0
  YMAX <- 1000
  
  ITERATIONS <- 2000
  
  XSTART <- 1000
  YSTART <- 1000
  
  annealing <- function(costFunc, initialPos, infoConsumerFunc, tempFunc )
  {
    bestPos <- initialPos
    lastTemp <- NA;
    temperature <- NA;
    
    for(k in 1:ITERATIONS)
    {
      lastTemp <- temperature
      temperature <- tempFunc(k)
      
      randomNeighbour <-  rnorm( 2, bestPos, 10)
      randomNeighbour[1] <- min(XMAX, max(XMIN, randomNeighbour[1]))
      randomNeighbour[2] <- min(YMAX, max(YMIN, randomNeighbour[2]))
      
      costNew <- costFunc(randomNeighbour)
      costOld <- costFunc(bestPos)
      
      distances[k] <- abs(costNew-costOld);
      
      if( costNew < costOld ){
        bestPos <- randomNeighbour;
        propabilities[k] <- NA;
      }else {
        propability <- exp(-abs(costNew-costOld)/temperature);
        propabilities[k] <- propability;
        
        if( runif(1,0,1) < propability) {
          bestPos <- randomNeighbour;
        }
      }
      propabilities <<- propabilities;
      distances <<- distances;
      infoConsumerFunc(bestPos, randomNeighbour)    }
  }
  
  colorFromRange = function(value, min, max)
  {
    # min value = green
    # max value = red
    # yellow's in the middle
    
    strip = function(v)
    {
      return(max(0, min(1, v)))
    }
    
    middle = (min+max)/2
    
    #if(value < middle)
    #  return(rgb(1, strip(2/(max-min)*(value-min)), 0))
    #return(rgb(strip(2/(min-max)*(value-max)), 1, 0))
    
    if(value < middle)
      return(rgb(strip(2/(min-max)*(-value+min)), 1, 0))
    return(rgb(1, strip(2/(max-min)*(-value+max)), 0))
    
  }
  
  # ======================================================
  # ======================================================
  # ======================================================
  
  resources = read.csv("resources.csv", stringsAsFactors=FALSE)
  sources = read.csv("sources.csv", stringsAsFactors=FALSE)
  #  -- do oblicze?:
  # jednostkowy koszt zakupu + jednostkowy koszt przetransportowania do fabryki
  sources["unitcost"] = vector(mode="double", length=nrow(sources))
  # na tej podstawie jest wyliczane ile z tego ?r?d?a kupujemy jednostek
  sources["used"] = vector(mode="double", length=nrow(sources))
  
  # ustawienie dla ka?dego ?r?d?a indexu zasobu
  sources["resourcesIndex"] <- vector( mode="double", length=nrow(sources))
  for( resourceIndex in 1:nrow(resources)){
    sources[ sources$resource == resources$id[resourceIndex], ]$resourcesIndex <-resourceIndex 
  }
  # zapisanie indeksu ?r?d?a w dataframe
  sources["sourceIndex"] <- 1:nrow(sources) 
  
  # ======================================================
  # =============== WALIDACJA DANYCH =====================
  # ======================================================
  
  for(id in resources$id)
  {
    available = sum(sources[sources$resource == id,]$limit)
    if(resources[resources$id == id,]$required > available)
    {
      cat("Ilosci zasobu o nazwie: ", resources[resources$id == id,]$name, " jest za malo w zrodlach.\n")
      return(FALSE)
    }
  }
  
  #TODO:
  #* czy wsp??rz?dne s? w ramach 0-1000
  #* czy ID produkowanego zasobu istnieje
  
  # ======================================================
  # ======================================================
  # ======================================================
  
  for(id in resources$id)
  {
    available = sum(sources[sources$resource == id,]$limit)
    if(resources[resources$id == id,]$required > available)
    {
      cat("Ilosci zasobu o nazwie: ", resources[resources$id == id,]$name, " jest za malo w zrodlach.\n")
      return(FALSE)
    }
  }
  
  # --------
  
  minCost <- .Machine$integer.max
  maxCost <- 0
  
  costFunction <- function(pos) {
    distance = function(pos1, pos2) {
      norm( pos1- pos2, type="2")
    }
    
    # --- oblicz koszt jednostkowy dla ka?dego ?r?d?a
    for( index in  1:nrow(sources) ){
      resource <- resources[sources$resourcesIndex[index],]
      sources$unitcost[index] <-
        (sources$cost[index] 
         + distance( c(sources$x[index], sources$y[index] ), pos) * resource$transport)
    }
    
    sum <- 0
    # --- dla ka?dego zasobu
    for( index in  1:nrow(resources)  ){
      # znajd? ?r?d?a kt?re tworz? dany zas?b
      sourcesMatching = sources[ sources$resourcesIndex == index,]
      required <- resources$required[index]
      orderedSourcesByCheapest <- sourcesMatching[with(sourcesMatching, order(unitcost, decreasing = FALSE)),];
      # przegl?daj ?r?d?a w kolejno?ci rozn?cego kosztu jednostkowego
      for( orderedIndex in 1:nrow(orderedSourcesByCheapest)){
        used <- min(orderedSourcesByCheapest[orderedIndex,]$limit, required)
        sum <- sum + used * sources[orderedSourcesByCheapest$sourceIndex[orderedIndex],]$unitcost;
        sources[orderedSourcesByCheapest$sourceIndex[orderedIndex],]$used <- used
        required <- required - used
      }
      
    }
    return (sum)  
  }
  
  infoFunction <- function(pos1, pos2) {
    drawPointOnContour(pos1[1], pos1[2], col="black", pch=5)
  }
  
  plot(c(), c(), xlim=c(0, 1000), ylim=c(0, 1000))
  
  #deklaracje globalnych zmiennych używanych w f.wyrzarzającej
  propabilities <- vector( mode="double", length=ITERATIONS);
  distances <- vector( mode="double", length=ITERATIONS);
  sampledSteps <- vector( mode="double", length=((1000/50)^2));
  
  # --- warstwice ---
  # wyliczanie warto?ci funkcji w pr?bkowanych punktach
  contourValuesMatrix <- matrix( nrow=21, ncol=21)
  for(x in seq(from=0, to=1000, by=50)){
    for(y in seq(from=0, to=1000, by=50)){
      #contourValuesMatrix[x/50+1,y/50+1] = costFunction(c(x, y))
      # points(c(x), c(y), pch=20, col=colorFromRange(costFunction(c(x, y)), 50000000, 170000000))
      
      firstPointVal <- costFunction(c(x, y));
      randomNeighbour <-  rnorm( 2, c(x,y), 10); #todo function to extract
      secondPointVal <- costFunction(randomNeighbour);
      sampledSteps[(x/50)*(1000/50) + (y/50) + 1] <- abs(firstPointVal - secondPointVal);
    }
  }
  
  expectedValueOfValueDifference <- mean(sampledSteps);
  expectedFirstT0Value <- 0.8;
  T0 <- -(expectedValueOfValueDifference)/log(expectedFirstT0Value);
  
  # # wysowanie konturu
  # contourX <- 50 * 1:nrow(contourValuesMatrix);
  # contourY <- 50 * 1:ncol(contourValuesMatrix);
  # filled.contour(x = contourX -50 , y=contourY - 50, z=contourValuesMatrix)
  # # --- ?r?d?a ---
  # 
  # drawPointOnContour(sources$x, sources$y, pch=8, col="#FF0000")
  # 
  # # --- wy?arzanie ---
  
  
  propabilitiesSum <-  vector( mode="double", length=ITERATIONS);
  propabilitiesCount <-  vector( mode="double", length=ITERATIONS);
  distancesSum <-  vector( mode="double", length=ITERATIONS);
  for( i in 1:10 ){
    annealing(costFunction, c(XSTART, YSTART), function(un1, un2){}, function (iteration){
      T0 * ((0.9942)^iteration)
    });
    message("asdas\n");
    for( j in 1:1000){
      if(!is.na(propabilities[j]) ){
        propabilitiesCount[j] <- propabilitiesCount[j]+1;
      }
    }
    propabilitiesSum <- propabilitiesSum +  sapply( propabilities, FUN=function(x) ifelse(is.na(x),0,x) );
    distancesSum <- distancesSum + distances;
  }
  
  plot( propabilitiesSum/propabilitiesCount);
  
  # plot(exp( -(distancesSum/10)/ sapply( seq(1:1000), function (iteration){
  #   T0 * (0.9942)*iteration
  # })))
  
  # annealing(costFunction, c(XSTART, YSTART), infoFunction)
  # 
  # cat('Min Cost detected: ', minCost, '\n')
  # cat('Max Cost detected: ', maxCost, '\n')
  
  return(TRUE)
}

drawPointOnContour <- function (x,y, col='#FFFFFF', pch){
  xs <- -30 + x/1000 * 810;
  ys <- y;
  points(x=xs,y=y, col = col, pch=pch )
  
  # oznaczanie ?r?de? labelkami: kompletnie tymczasowe
  #text(x=xs, y=y+25, labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"), cex= 0.7)
}

#run()

