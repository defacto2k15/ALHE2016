run = function()
{
  XMIN <- 0
  XMAX <- 1000
  YMIN <- 0
  YMAX <- 1000
  
  annealing <- function(costFunc, initialPos, infoConsumerFunc )
  {
    tempFunc <- function( interation )
    {
      (1 - 0.1)^interation
    }
    
    bestPos <- initialPos
    
    for(k in 1:200)
    {
      temperature <- tempFunc(k)
      #rand <-  rnorm( 2, bestPos, 10)
      #randomNeighbour <- c( max(0,  rand[1]), max(0,  rand[2]))
      
      randomNeighbour <-  rnorm( 2, bestPos, 10)
      randomNeighbour[1] <- min(XMAX, max(XMIN, randomNeighbour[1]))
      randomNeighbour[2] <- min(YMAX, max(YMIN, randomNeighbour[2]))
      
      #if( costFunc (randomNeighbour) > costFunc(bestPos) ){
      #  bestPos <- randomNeighbour;
      #} else if ( runif(1,0,1) < exp( - abs(  costFunc (randomNeighbour)-costFunc(bestPos)  )/temperature  )){
      #  bestPos <- randomNeighbour;
      #}
      
      costNew <- costFunc(randomNeighbour)
      costOld <- costFunc(bestPos)
      
      if( costNew < costOld ){
        bestPos <- randomNeighbour;
      } else if ( runif(1,0,1) < exp(-abs(costNew-costOld)/temperature)){
        bestPos <- randomNeighbour;
      }
      
      infoConsumerFunc(bestPos, randomNeighbour)
      #message( sprintf("%i\t%.6f\t%.6f\t%.6ft%.6f\t%.6f", k, temperature,costFunc(bestPos), costFunc(randomNeighbour), bestPos[1], bestPos[2]))
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
  
  # ======================================================
  # ======================================================
  # ======================================================
  
  resources = read.csv("resources.csv", stringsAsFactors=FALSE)
  sources = read.csv("sources.csv", stringsAsFactors=FALSE)
  #  -- do obliczeń:
  # jednostkowy koszt zakupu + jednostkowy koszt przetransportowania do fabryki
  sources["unitcost"] = vector(mode="double", length=nrow(sources))
  # na tej podstawie jest wyliczane ile z tego źródła kupujemy jednostek
  sources["used"] = vector(mode="double", length=nrow(sources))
  
  # ustawienie dla każdego źródła indexu zasobu
  sources["resourcesIndex"] <- vector( mode="double", length=nrow(sources))
  for( resourceIndex in 1:nrow(resources)){
    sources[ sources$resource == resources$id[resourceIndex], ]$resourcesIndex <-resourceIndex 
  }
  # zapisanie indeksu źródła w dataframe
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
  #* czy współrzędne są w ramach 0-1000
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
    
    # --- oblicz koszt jednostkowy dla każdego źródła
    for( index in  1:nrow(sources) ){
      resource <- resources[sources$resourcesIndex[index],]
      sources$unitcost[index] <-
          (sources$cost[index] 
          + distance( c(sources$x[index], sources$y[index] ), pos) * resource$transport)
    }
    
    sum <- 0
    # --- dla każdego zasobu
    for( index in  1:nrow(resources)  ){
      # znajdź źródła które tworzą dany zasób
      sourcesMatching = sources[ sources$resourcesIndex == index,]
      required <- resources$required[index]
      orderedSourcesByCheapest <- sourcesMatching[with(sourcesMatching, order(unitcost, decreasing = FALSE)),];
      # przeglądaj źródła w kolejności roznącego kosztu jednostkowego
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
    points(pos1[1], pos1[2], col="black", pch=7)
  }
  
  plot(c(), c(), xlim=c(0, 1000), ylim=c(0, 1000))
  
  # --- poziomice ---
  
  for(x in seq(from=0, to=1000, by=50))
    for(y in seq(from=0, to=1000, by=50))
      points(c(x), c(y), pch=20, col=colorFromRange(costFunction(c(x, y)), 50000000, 170000000))
  
  # --- źródła ---
  
  points(sources$x, sources$y, pch=8, col="#FF0000")
  
  # --- wyżarzanie ---
  
  annealing(costFunction, c(0,0), infoFunction)
  
  cat('Min Cost detected: ', minCost, '\n')
  cat('Max Cost detected: ', maxCost, '\n')
  
  return(TRUE)
}

run()


























perlin_noise <- function( 
  n = 5,   m = 7,    # Size of the grid for the vector field
  N = 100, M = 100   # Dimension of the image
) {
  # For each point on this n*m grid, choose a unit 1 vector
  vector_field <- apply(
    array( rnorm( 2 * n * m ), dim = c(2,n,m) ),
    2:3,
    function(u) u / sqrt(sum(u^2))
  )
  f <- function(x,y) {
    # Find the grid cell in which the point (x,y) is
    i <- floor(x)
    j <- floor(y)
    stopifnot( i >= 1 || j >= 1 || i < n || j < m )
    # The 4 vectors, from the vector field, at the vertices of the square
    v1 <- vector_field[,i,j]
    v2 <- vector_field[,i+1,j]
    v3 <- vector_field[,i,j+1]
    v4 <- vector_field[,i+1,j+1]
    # Vectors from the point to the vertices
    u1 <- c(x,y) - c(i,j)
    u2 <- c(x,y) - c(i+1,j)
    u3 <- c(x,y) - c(i,j+1)
    u4 <- c(x,y) - c(i+1,j+1)
    # Scalar products
    a1 <- sum( v1 * u1 )
    a2 <- sum( v2 * u2 )
    a3 <- sum( v3 * u3 )
    a4 <- sum( v4 * u4 )
    # Weighted average of the scalar products
    s <- function(p) 3 * p^2 - 2 * p^3
    p <- s( x - i )
    q <- s( y - j )
    b1 <- (1-p)*a1 + p*a2
    b2 <- (1-p)*a3 + p*a4
    (1-q) * b1 + q * b2
  }
  xs <- seq(from = 1, to = n, length = N+1)[-(N+1)]
  ys <- seq(from = 1, to = m, length = M+1)[-(M+1)]
  outer( xs, ys, Vectorize(f) )
}

matx <- perlin_noise()

funcX <- function( pos ){
  
  matx[ pos[1], pos[2]]
}

funcY <- function( pos ){
  x1Val <- funcX( c( floor(pos[1]), pos[2]) )
  x2Val <- funcX( c( ceiling(pos[1]), pos[2]) )
  y1Val <- funcX( c( pos[1], floor(pos[2]) ))
  y2Val <- funcX( c( pos[1], ceiling(pos[2]) ))
  
  x <- pos[1] - floor(pos[1])
  y <- pos[2] - floor(pos[2])
  
  ( x + y)/2
}

plotTest <- function(){
  library(lattice)
  
  #Build the horizontal and vertical axis information
  hor <- seq(0,100,10)
  ver <- paste("DM1-", hor, sep="")
  
  #Build the fake correlation matrix
  nrowcol <- length(ver)
  cor <- matrix(runif(nrowcol*nrowcol, min=0.4), nrow=nrowcol, ncol=nrowcol)
  for (i in 1:nrowcol) cor[i,i] = 1
  
  #Build the plot
  rgb.palette <- colorRampPalette(c("blue", "yellow"), space = "rgb")
  levelplot(cor, main="stage 12-14 array correlation matrix", xlab="", ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01)) 
}

#annealing(costFunction, c(0,0), tempFunc, infoFunction)