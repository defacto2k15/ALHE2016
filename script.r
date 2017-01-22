require("lattice")

run = function()
{
  XMIN <- 0
  XMAX <- 1000
  YMIN <- 0
  YMAX <- 1000
  
  ITERATIONS <- 500
  
  XSTART <- 1000
  YSTART <- 1000
  

  
  annealing <- function(costFunc, initialPos, infoConsumerFunc, tempFunc, randomNeighbourGenerator )
  {
    bestPos <- initialPos
    lastTemp <- NA;
    temperature <- NA;
    
    for(k in 1:ITERATIONS)
    {
      lastTemp <- temperature
      temperature <- tempFunc(k)
      
      randomNeighbour <-  randomNeighbourGenerator(bestPos)
      
      costNew <- costFunc(randomNeighbour)
      costOld <- costFunc(bestPos)
      
      distances[k] <- abs(costNew-costOld);
      
      if( costNew < costOld ){
        bestPos <- randomNeighbour;
        propabilities[k] <- NA;
        if( minCost > costNew){
          minCost <<- costNew;
        }
        
      }else {
        propability <- exp(-abs(costNew-costOld)/temperature);
        propabilities[k] <- propability;
        
        if( runif(1,0,1) < propability) {
          bestPos <- randomNeighbour;
        }
      }
      propabilities <<- propabilities;
      distances <<- distances;
      infoConsumerFunc(bestPos, randomNeighbour)   
      
      
    }
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
  
  randomNeighbourGenerator <- function( bestPos ){
    randomNeighbour <-  pmax(pmin(rnorm( 2, bestPos, 30), c(XMAX, YMAX)), c(XMIN, YMIN))
    randomNeighbour;
  }
  
  # ======================================================
  # ======================================================
  # ======================================================
  
  loadResources <- function( resourcesFileName, sourcesFileName ){
    if( !file.exists(resourcesFileName)){
      cat('Błąd. Plik z zasobami ',resourcesFileName,' nie istnieje. Kończenie\n');
      quit(status = -1);
    }
    resources <- read.csv(resourcesFileName, stringsAsFactors=FALSE)
    if( !file.exists(sourcesFileName)){
      cat('Błąd. Plik z źródłami ',sourcesFileName,' nie istnieje. Kończenie\n');
      quit(status = -1);
    }
    sources <- read.csv(sourcesFileName, stringsAsFactors=FALSE)
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
    
    # czy współrzędne są w ramach 0-1000
    for( i in 1:nrow(sources)){
      if( sources$x[i] < 0 || sources$x[i] > 1000 ){
        cat('Błąd. Zasób o id ',sources$id[i],' ma mieć współrzędną x między 0 a 1000, a ma ', sources$x[i] )
      }
      if( sources$y[i] < 0 || sources$y[i] > 1000 ){
        cat('Błąd. Zasób o id ',sources$id[i],' ma mieć współrzędną y między 0 a 1000, a ma ', sources$y[i] )
      }
    }
    
    # czy id produkowanego zasobu istnieje
    for( i in 1:nrow(sources)){
      if( nrow( resources[resources$id == sources$resource[i],]) == 0){
        cat('Błąd. W pliku ze źródłami jest zasób o id ',sources$id[i],' który ma kolumne zasób z wartością ',sources$resource[i],' a takiego zasobu nie ma');
        quit(status = -1);
      }
    }   
    
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
    
    minCost <<- .Machine$integer.max
    maxCost <<- 0
    resources <<- resources
    sources <<- sources
  }
  
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
  
  infoFunction <- function(bestPos, currPos) {
    if( currentPointIndex > ITERATIONS ){
      currentPointIndex <- 1;
    }
    pointsXVec[currentPointIndex] <<- currPos[1];
    pointsYVec[currentPointIndex] <<- currPos[2];
    currentPointIndex <<- currentPointIndex+1;
    #drawPointOnContour(pos1[1], pos1[2], col="black", pch=5)
  }
  
  resourcesFileName <- 'resources4.csv';
  sourcesFileName <-'sources4.csv';
  contourImageFileName <- NULL;
  propabilitiesImageFileName <- NULL;
  startPoint <- c(XSTART, YSTART);
  
  #runAlgorithm <- function ( resourcesFileName, sourcesFileName, startPoint=c(XSTART, YSTART), contourImageFileName= NULL, propabilitiesImageFileName = NULL ){
    # do rysowania śladu
    pointsXVec <<- vector(mode="double", length=ITERATIONS); 
    pointsYVec <<- vector(mode="double", length=ITERATIONS); 
    currentPointIndex <<- 1;
    
    plot(c(), c(), xlim=c(XMIN, XMAX), ylim=c(YMIN, YMAX))
    
    #deklaracje globalnych zmiennych używanych w f.wyrzarzającej
    propabilities <<- vector( mode="double", length=ITERATIONS);
    distances <<- vector( mode="double", length=ITERATIONS);
    sampledSteps <<- vector( mode="double", length=((1000/50)^2));
    
    loadResources(resourcesFileName, sourcesFileName ); 
    
    # --- warstwice ---
    # wyliczanie warto?ci funkcji w pr?bkowanych punktach
    contourValuesMatrix <- matrix( nrow=21, ncol=21)
    for(x in seq(from=0, to=1000, by=50)){
      for(y in seq(from=0, to=1000, by=50)){
        contourValuesMatrix[x/50+1,y/50+1] = costFunction(c(x, y))
        # points(c(x), c(y), pch=20, col=colorFromRange(costFunction(c(x, y)), 50000000, 170000000))
        
        firstPointVal <- costFunction(c(x, y));
        randomNeighbour <- randomNeighbourGenerator(c(x,y))
        secondPointVal <- costFunction(randomNeighbour);
        sampledSteps[(x/50)*(1000/50) + (y/50) + 1] <- abs(firstPointVal - secondPointVal);
      }
    }
  
    # --- wyliczanie parametrów funkcji 
    expectedValueOfValueDifference <- mean(sampledSteps);
    expectedFirstT0Value <- 0.8;
    T0 <- -(expectedValueOfValueDifference)/log(expectedFirstT0Value);
    
    RFunc <- function ( mean, T0, Y ){
      f1 <- mean/T0;
      tan( ((f1/log(Y))+0.5)*pi )
    }
    
    temperatureXPointSetting <- c( ITERATIONS/5, ITERATIONS*3/5)
    temperatureYPointSetting <- c(0.75, 0.3)
    
    R1 <- RFunc(mean(sampledSteps), T0, temperatureYPointSetting[1]);
    R2 <- RFunc(mean(sampledSteps), T0, temperatureYPointSetting[2]);
    aParam <- (temperatureXPointSetting[2]  - temperatureXPointSetting[1] /(R1/R2) )/(1 - R2/R1)
    bParam <- (temperatureXPointSetting[1] - aParam)/R1
    
    tempFunc <- function(iteration){
      T0 * (-atan((iteration - aParam)/bParam)/(pi) + 0.5)
    }
    
    if( !is.null(contourImageFileName)){
      # wysowanie konturu
      contourX <- 50 * 1:nrow(contourValuesMatrix);
      contourY <- 50 * 1:ncol(contourValuesMatrix);
      filled.contour(x = contourX -50 , y=contourY - 50, z=contourValuesMatrix, plot.axes = { points(pointsXVec, pointsYVec); axis(1); axis(2) });
    }
    
    # obiekty do zbierania statystyk z wykonania
    propabilitiesSum <-  vector( mode="double", length=ITERATIONS);
    propabilitiesCount <-  vector( mode="double", length=ITERATIONS);
    
    # wyszukanie wyniku!!!    
    annealing(costFunction, startPoint, infoFunction, tempFunc , randomNeighbourGenerator);
    
    # statystyki do prawdopodobieństwa
    for( j in 1:ITERATIONS){
      if(!is.na(propabilities[j]) ){
        propabilitiesCount[j] <- propabilitiesCount[j]+1;
      }
    }
    propabilitiesSum <- propabilitiesSum +  sapply( propabilities, FUN=function(x) ifelse(is.na(x),0,x) );
    
    if( !is.null(contourImageFileName)){
      jpeg(contourImageFileName);
      filled.contour(x = contourX -50 , y=contourY - 50, z=contourValuesMatrix, plot.axes = { points(pointsXVec, pointsYVec); axis(1); axis(2) });
      dev.off();      
    }
    
    if( !is.null(propabilitiesImageFileName) ){
      jpeg(propabilitiesImageFileName );
      #remove zeros
      propabilitiesCount <- propabilitiesCount +  sapply( propabilitiesCount, FUN=function(x) ifelse(x==0,1,x) );
      plot( propabilitiesSum/propabilitiesCount, xlim=c(0,ITERATIONS));
      dev.off();
    }
    
    cat('Min Cost detected: ', minCost, '\n')
    
    return(TRUE)
  #}
  
  
  #runAlgorithm('resources4.csv', 'sources4.csv', contourImageFileName = 'con1.jpeg');
}


# T0*(-atan((iteration - 300)/100)/(pi) + 0.5))
#plot(sapply(FUN=function(iteration){ exp(-distancesSum[iteration] /( T0*(-atan((iteration - 500)/300)/(pi) + 0.5)))}, X=1:1000))

#run()

