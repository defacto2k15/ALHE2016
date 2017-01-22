source(file="script.r")

args<-commandArgs(TRUE)


assertThereIsOneMoreArg <- function (argPosition, argName ){
  if( argPosition > length(args)){
    cat('Błąd. Po argumencie ',argName,' powinien być kolejny, ale go nie ma\n');
  }
}

if(length(args) == 0  ){
  cat('Użycie programu: Rscript runAnnealing.r -rf [nazwaPlikuZZasobami] -sf [nazwaPlikuZeŹródłami] -sp [startowyX] [startowyY] (opcjonalny) -cf [nazwaPlikuZKonturemIŚladem]\n')
} else {
  resourcesFileName <- NA;
  sourcesFileName <- NA;
  contourImageFileName <- NA;
  startPoint <- NA;
  for( i in 1:length(args)){
    if( args[i] == '-rf'){
      assertThereIsOneMoreArg(i+1, '-rf');
      resourcesFileName <- args[i+1];
    }
    if( args[i] == '-sf'){
      assertThereIsOneMoreArg(i, '-sf');
      sourcesFileName <- args[i+1];
    }  
    if( args[i] == '-cf'){
      assertThereIsOneMoreArg(i+1, '-cf');
      contourImageFileName <- args[i+1];
    }   
    if( args[i] == '-sp'){
      assertThereIsOneMoreArg(i+1, '-sp');
      assertThereIsOneMoreArg(i+2, '-sp xPos');
      startPoint <- c(as.integer(args[i+1]), as.integer(args[i+2]));
    }   
  }
  
  if( is.na(resourcesFileName) ){
    cat('Błąd. Nie zdefiniowano resourcesFileName\n');
    quit(statue=-1);
  }
  if( is.na(sourcesFileName) ){
    cat('Błąd. Nie zdefiniowano sourcesFileName\n');
    quit(statue=-1);
  }
  if( is.na(startPoint[1]) ){
    cat('Błąd. Nie zdefiniowano startPoint\n');
    quit(statue=-1);
  }    
  
  run(resourcesFileName = resourcesFileName, sourcesFileName = sourcesFileName, startPoint = startPoint, contourImageFileName = contourImageFileName)
}


