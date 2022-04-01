

#### Create Function for Life Tables ##### 

      # This is an adaptation of EDDIE HUNSINGER's R script 
      # so we can compute multiple Period Life Tables for 
      # different groups with one single function





# Define Age groups
x <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)


#### National Life Table Decile ##### ----------------------------------------------------------------



    # create function for National Life Table
life_table_nat_decile <- function( x, dataset, nMx){
  ## simple lifetable using Keyfitz and Flieger separation factors and 
  ## exponential tail of death distribution (to close out life table)
  tableid <- dataset$tableid # ******************** 
  sex <- dataset$sex # ********************
  decileBR <- dataset$decileBR # ********************
  b0 <- 0.07;   b1<- 1.7;      
  nmax <- length(x)
  #nMx = nDx/nKx   
  n <- c(diff(x),999)          		#width of the intervals
  nax <- n / 2;		            	# default to .5 of interval
  nax[1] <- b0 + b1 *nMx[1]    		# from Keyfitz & Flieger(1968)
  nax[2] <- 1.5  ;              
  nax[nmax] <- 1/nMx[nmax] 	  	# e_x at open age interval
  nqx <- (n*nMx) / (1 + (n-nax)*nMx)
  nqx<-ifelse( nqx > 1, 1, nqx);		# necessary for high nMx
  nqx[nmax] <- 1.0
  lx <- c(1,cumprod(1-nqx)) ;  		# survivorship lx
  lx <- lx[1:length(nMx)]
  ndx <- lx * nqx ;
  nLx <- n*lx - nax*ndx;       		# equivalent to n*l(x+n) + (n-nax)*ndx
  nLx[nmax] <- lx[nmax]*nax[nmax]
  Tx <- rev(cumsum(rev(nLx)))
  ex <- ifelse( lx[1:nmax] > 0, Tx/lx[1:nmax] , NA);
  lt <- data.frame(tableid=tableid,sex=sex,decileBR=decileBR,x=x,nax=nax,nmx=nMx,nqx=nqx,lx=lx,ndx=ndx,nLx=nLx,Tx=Tx,ex=ex)
  return(lt)
}



#### National Life Table QuINTILE ##### ----------------------------------------------------------------

# create function for National Life Table

life_table_nat_quintile <- function( x, dataset, nMx){
  ## simple lifetable using Keyfitz and Flieger separation factors and 
  ## exponential tail of death distribution (to close out life table)
  tableid <- dataset$tableid # ********************
  
  sex <- dataset$sex # ********************
  quintileBR <- dataset$quintileBR # ********************
  b0 <- 0.07;   b1<- 1.7;
  nmax <- length(x)
  #nMx = nDx/nKx   
  n <- c(diff(x),999)          		#width of the intervals
  nax <- n / 2;		            	# default to .5 of interval
  nax[1] <- b0 + b1 *nMx[1]    		# from Keyfitz & Flieger(1968)
  nax[2] <- 1.5  ;              
  nax[nmax] <- 1/nMx[nmax] 	  	# e_x at open age interval
  nqx <- (n*nMx) / (1 + (n-nax)*nMx)
  nqx<-ifelse( nqx > 1, 1, nqx);		# necessary for high nMx
  nqx[nmax] <- 1.0
  lx <- c(1,cumprod(1-nqx)) ;  		# survivorship lx
  lx <- lx[1:length(nMx)]
  ndx <- lx * nqx ;
  nLx <- n*lx - nax*ndx;       		# equivalent to n*l(x+n) + (n-nax)*ndx
  nLx[nmax] <- lx[nmax]*nax[nmax]
  Tx <- rev(cumsum(rev(nLx)))
  ex <- ifelse( lx[1:nmax] > 0, Tx/lx[1:nmax] , NA);
  lt <- data.frame(tableid=tableid,sex=sex,quintileBR=quintileBR,x=x,nax=nax,nmx=nMx,nqx=nqx,lx=lx,ndx=ndx,nLx=nLx,Tx=Tx,ex=ex)
  return(lt)
}



#### Regional Life Table Quintile ##### ----------------------------------------------------------------

life_table_region_quintile <- function( x, dataset, nMx){
  ## simple lifetable using Keyfitz and Flieger separation factors and 
  ## exponential tail of death distribution (to close out life table)
  tableid <- dataset$tableid # ********************
  region <- dataset$region # ********************
  sex <- dataset$sex # ********************
  quintileRegion <- dataset$quintileRegion # ********************
  b0 <- 0.07;   b1<- 1.7;      
  nmax <- length(x)
  #nMx = nDx/nKx   
  n <- c(diff(x),999)          		#width of the intervals
  nax <- n / 2;		            	# default to .5 of interval
  nax[1] <- b0 + b1 *nMx[1]    		# from Keyfitz & Flieger(1968)
  nax[2] <- 1.5  ;              
  nax[nmax] <- 1/nMx[nmax] 	  	# e_x at open age interval
  nqx <- (n*nMx) / (1 + (n-nax)*nMx)
  nqx<-ifelse( nqx > 1, 1, nqx);		# necessary for high nMx
  nqx[nmax] <- 1.0
  lx <- c(1,cumprod(1-nqx)) ;  		# survivorship lx
  lx <- lx[1:length(nMx)]
  ndx <- lx * nqx ;
  nLx <- n*lx - nax*ndx;       		# equivalent to n*l(x+n) + (n-nax)*ndx
  nLx[nmax] <- lx[nmax]*nax[nmax]
  Tx <- rev(cumsum(rev(nLx)))
  ex <- ifelse( lx[1:nmax] > 0, Tx/lx[1:nmax] , NA);
  lt <- data.frame(tableid=tableid,region=region,sex=sex,quintileRegion=quintileRegion,x=x,nax=nax,nmx=nMx,nqx=nqx,lx=lx,ndx=ndx,nLx=nLx,Tx=Tx,ex=ex)
  return(lt)
}









#### State Life Table Quintile ##### ----------------------------------------------------------------

life_table_state_quintile <- function( x, dataset, nMx){
  ## simple lifetable using Keyfitz and Flieger separation factors and 
  ## exponential tail of death distribution (to close out life table)
  tableid <- dataset$tableid # ********************
  state <- dataset$state # ********************
  sex <- dataset$sex # ********************
  quintileState <- dataset$quintileState # ********************
  b0 <- 0.07;   b1<- 1.7;      
  nmax <- length(x)
  #nMx = nDx/nKx   
  n <- c(diff(x),999)          		#width of the intervals
  nax <- n / 2;		            	# default to .5 of interval
  nax[1] <- b0 + b1 *nMx[1]    		# from Keyfitz & Flieger(1968)
  nax[2] <- 1.5  ;              
  nax[nmax] <- 1/nMx[nmax] 	  	# e_x at open age interval
  nqx <- (n*nMx) / (1 + (n-nax)*nMx)
  nqx<-ifelse( nqx > 1, 1, nqx);		# necessary for high nMx
  nqx[nmax] <- 1.0
  lx <- c(1,cumprod(1-nqx)) ;  		# survivorship lx
  lx <- lx[1:length(nMx)]
  ndx <- lx * nqx ;
  nLx <- n*lx - nax*ndx;       		# equivalent to n*l(x+n) + (n-nax)*ndx
  nLx[nmax] <- lx[nmax]*nax[nmax]
  Tx <- rev(cumsum(rev(nLx)))
  ex <- ifelse( lx[1:nmax] > 0, Tx/lx[1:nmax] , NA);
  lt <- data.frame(tableid=tableid,state=state,sex=sex,quintileState=quintileState,x=x,nax=nax,nmx=nMx,nqx=nqx,lx=lx,ndx=ndx,nLx=nLx,Tx=Tx,ex=ex)
  return(lt)
}
