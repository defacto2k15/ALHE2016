require('fields')

annealing <- function(costFunc, initialPos, temperatureFunc, infoConsumerFunc, stoppingFunc, neighbourGenerator ){
  currentPos <- bestPos <- initialPos
  message( sprintf("Iterac\tTemp\t\tFKosztu\t\tBestPos\t\t\tCurrVal\t\tCurrPos"))
  k <- 0
  while( !stoppingFunc(k)){
    temperature <- temperatureFunc(k)
    randomNeighbour <- neighbourGenerator(currentPos)

    if( costFunc (randomNeighbour) > costFunc(bestPos) ){
      currentPos <- bestPos <- randomNeighbour;
    } else if (costFunc (randomNeighbour) > costFunc(currentPos) ){
      currentPos  <- randomNeighbour;
    } else if ( runif(1,0,1) < exp( - abs(  costFunc (randomNeighbour)-costFunc(currentPos)  )/temperature  )){
      currentPos <- randomNeighbour;
    }
    infoConsumerFunc(bestPos, currentPos, randomNeighbour)
    message( sprintf("%i\t%.6f\t%.6f\t[%.6f:%.6f]\t%.6f\t[%.6f:%.6f]",
                     k, temperature,costFunc(bestPos), bestPos[1], bestPos[2],costFunc(currentPos), currentPos[1], currentPos[2]))
    k <- k + 1
  }
   bestPos
}

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

#matx <- perlin_noise()

funcY <- function( pos ){
  fourCorners = list( 
      c( floor(pos[1]), floor(pos[2]) ),  
      c( ceiling(pos[1]), floor(pos[2]) ), 
      c( floor(pos[1]), ceiling(pos[2]) ),
      c( ceiling(pos[1]), ceiling(pos[2]) ))
  
  s <- norm(pos-c( ceiling(pos[1]), ceiling(pos[2])), type="2")
  
  weights <- mapply( function(arg){ sqrt(2) - norm(pos-arg, type="2")  },fourCorners)
  values <- mapply( function(arg){ matx[ arg[1], arg[2]]},fourCorners)
  
  weighted.mean(values, weights)
}

mainFunc <- function( temparatureSpeed = 0.01, 
                      maxIterations=1000, 
                      neighbourGeneratingStdDeviation = 1,
                      initialPos=c(50,50)){
  if( !exists("matx")){
    matx <- perlin_noise()
  }
  
  plot.new()
  plot.window( c(0,100), c(0,100))
  #image(x = 0:100, y = 0:100, matx)
  filled.contour(x = 1:100, y = 1:100, matx)
  
  title(main="main title", sub="sub-title", 
        xlab="x-axis label", ylab="y-axis label")
  res <- annealing ( costFunc = funcY, 
              initialPos = initialPos, 
              temperatureFunc = function(iteration){
                (1 - temparatureSpeed)^iteration
              },
              infoConsumerFunc = function(bestPos, currentPos, generatedPos ){
                #Sys.sleep(1)
                if( identical(bestPos,generatedPos) ){
                  pch <-3
                } else if( identical(currentPos, generatedPos)){
                  pch <-2
                }else{
                  pch <-1
                }
                fixedPos <- getPointFixedPosition(generatedPos)
                points( fixedPos[1], fixedPos[2], pch=pch)
                  
              },
              stoppingFunc = function(iteration ){
                iteration > maxIterations
              },
              neighbourGenerator = function( currentPos ){
                rand <-  rnorm( 2, currentPos, neighbourGeneratingStdDeviation)
                randomNeighbour <- pmin( pmax(rand, 1 ), 100 ) 
              }
  )
  message( sprintf("Najlepszy wynik to %.6f osiągnięty w punkcie[%.6f %.6f]", funcY(res), res[1], res[2]))
  fixedBestPos <- getPointFixedPosition(res)
  points( fixedBestPos[1], fixedBestPos[2], pch=20, col=15)
}

getPointFixedPosition <- function( pos ){
  c(  (-2) + (pos[1])*0.8, pos[2])
}