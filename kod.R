annealing <- function(costFunc, initialPos, temperatureFunc, infoConsumerFunc ){
  bestPos <- initialPos
  
  for( k in 1:100){
    temperature <- temperatureFunc(k)
    rand <-  rnorm( 2, bestPos, 1)
    randomNeighbour <- c( max(0,  rand[0]), max(0,  rand[1]))
    if( costFunc (randomNeighbour) > costFunc(bestPos) ){
      bestPos <- randomNeighbour;
    } else if ( runif(1,0,1) < exp( - abs(  costFunc (randomNeighbour)-costFunc(bestPos)  )/temperature  )){
      bestPos <- randomNeighbour;
    }
    infoConsumerFunc(bestPos, randomNeighbour)
    message( sprintf("%i\t%.6f\t%.6f\t%.6f\t%.6f", k, temperature,costFunc(bestPos), bestPos[1], bestPos[2]))
  }
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

tempFunc <- function( interation ){
  (1 - 0.1)^interation
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
