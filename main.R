epoka = 0
znak<-matrix();
fenotyp<-matrix();
f<-matrix();
finalZnak <- matrix()
finalFenotyp <- matrix()
sumyMiar <- vector();
notBest = 0;
bestSumaMiar = 0;
bezPoprawy = 3;
bestSol<- matrix(1, nrow = 1, ncol = 4)
funkcjaPrzystosowania <- function(X) {
    return(0.2 * X^3 + 0.1 * X^2 - 8*X)
}
pop<-matrix(round(runif(40,0,1)),10,4)
while (epoka < 15) {
    if (epoka > 0) {
        pop<-mutacja
    }
    epoka = epoka + 1
    for(i in 1:10) {
        znak[i]<-pop[i,1]*-1;
        if (znak[i] == -1) {
            fenotyp[i]<- (2^2*pop[i,2] + 2^1*pop[i,3] + 2^0*pop[i,4]) * znak[i];
        } else {
            fenotyp[i]<- (2^2*pop[i,2] + 2^1*pop[i,3] + 2^0*pop[i,4]);
        }
    }
    wartFunkcjiPrzystosowania <- vector(length=10)
    wartFunkcjiPrzystosowania <- funkcjaPrzystosowania(fenotyp)
    maximum <- max(wartFunkcjiPrzystosowania)
    miaraPrzystosowania <- vector(length=10)
    miaraPrzystosowania <- maximum - wartFunkcjiPrzystosowania
    sumaMiar <- sum(miaraPrzystosowania)

    koloRuletki <- vector(length=10) + 0
    for(i in 1:10) {
        for(j in 1:i) {
            koloRuletki[i] <- koloRuletki[i] + miaraPrzystosowania[j]
        }
    }
    losowanie <- runif(10, 0, sumaMiar)
    potomkowie <- vector(length=10)
    for(i in 1:10) {
        for(j in 1:10) {
            if(losowanie[i] <= koloRuletki[j]){
                potomkowie[i] <- j
                break
            }
        }
    }
    potomkowieBin <- matrix(nrow = 10, ncol = 4)
    for(i in 1:10) {
        potomkowieBin[i, 1:4] <- pop[potomkowie[i], 1:4]
    }
    szansaKrzyzowania <- 1
    krzyzowanie <- potomkowieBin
    for(i in seq(from = 1, to = 10, by = 2)) {
        if(runif(1,0.0,1.0) <= szansaKrzyzowania) {
            krzyzowanie[i, 1:2] <- potomkowieBin[i, 1:2]
        }
        if(runif(1,0.0,1.0) <= szansaKrzyzowania) {
            krzyzowanie[i, 3:4] <- potomkowieBin[i+1, 3:4]
        }
        if(runif(1,0.0,1.0) <= szansaKrzyzowania) {
            krzyzowanie[i+1, 1:2] <- potomkowieBin[i+1, 1:2]
        }
        if(runif(1,0.0,1.0) <= szansaKrzyzowania)  {
            krzyzowanie[i+1, 3:4] <- potomkowieBin[i, 3:4]
        }
    }
    mutacja <- krzyzowanie
    szansaMutacji <- 0.1
    for (i in 1:10) {
        for(j in 1:4) {
            if(runif(1,0.0,1.0) <= szansaMutacji) {
                mutacja[i,j] <- (mutacja[i,j] +1) %% 2
            }
        }
    }
    for(i in 1:10) {
        finalZnak[i]<-mutacja[i,1]*-1;
        if (finalZnak[i] == -1) {
            finalFenotyp[i]<- (2^2*mutacja[i,2] + 2^1*mutacja[i,3] + 2^0*mutacja[i,4]) * finalZnak[i];
        } else {
            finalFenotyp[i]<- (2^2*mutacja[i,2] + 2^1*mutacja[i,3] + 2^0*mutacja[i,4]);
        }
    }
    finalWartFunkcjiPrzystosowania <- vector(length=10)
    finalWartFunkcjiPrzystosowania <- funkcjaPrzystosowania(finalFenotyp)
    finalMaximum <- max(finalWartFunkcjiPrzystosowania)
    finalMiaraPrzystosowania <- vector(length=10)
    finalMiaraPrzystosowania <- finalMaximum - finalWartFunkcjiPrzystosowania
    finalSumaMiar <- sum(finalMiaraPrzystosowania)
    sumyMiar <- append(sumyMiar, finalSumaMiar)
    if (finalSumaMiar > bestSumaMiar) {
        bestSumaMiar = finalSumaMiar
        notBest = 0
    } else {
        notBest = notBest + 1
    }
    if (notBest == bezPoprawy) {
        cat("Nie ma poprawy przez", notBest, "populacje")
        break
    }
    if (all(mutacja[1, 1:4] == bestSol) &&
        all(mutacja[2, 1:4] == bestSol) &&
        all(mutacja[3, 1:4] == bestSol) &&
        all(mutacja[4, 1:4] == bestSol) &&
        all(mutacja[5, 1:4] == bestSol) &&
        all(mutacja[6, 1:4] == bestSol) &&
        all(mutacja[7, 1:4] == bestSol) &&
        all(mutacja[8, 1:4] == bestSol) &&
        all(mutacja[9, 1:4] == bestSol) &&
        all(mutacja[10, 1:4] == bestSol)) {

        print("Znaleziono najlepsze rozwiązanie")
        break
    }
    if (all(mutacja[2, 1:4] == mutacja[1, 1:4]) &&
        all(mutacja[3, 1:4] == mutacja[1, 1:4]) &&
        all(mutacja[4, 1:4] == mutacja[1, 1:4]) &&
        all(mutacja[5, 1:4] == mutacja[1, 1:4]) &&
        all(mutacja[6, 1:4] == mutacja[1, 1:4]) &&
        all(mutacja[7, 1:4] == mutacja[1, 1:4]) &&
        all(mutacja[8, 1:4] == mutacja[1, 1:4]) &&
        all(mutacja[9, 1:4] == mutacja[1, 1:4]) &&
        all(mutacja[10, 1:4] == mutacja[1, 1:4])){
        print("Populacja sklada sie z tego samego chromosomu")
        break
    }
}
sredniaWartPrzystosowania<-(sum(sumyMiar)/(epoka*10))
show(pop);
show(krzyzowanie);
show(mutacja);
show(sredniaWartPrzystosowania);
show(epoka);