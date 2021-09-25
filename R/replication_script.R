###############################################################################################################################
# Title: replication_script.R
# creator: Georg Leitner
# Description: Runs analysis and creates all final figures used in the final version of the working paper:
# "How risky is Monetary Policy? The Effect of Monetary Policy on Systemic Risk in the Euro Area"
# Figures are created one by one and have are put togehter in the end for a final figure
# In case of questions, please contact georg.leitner380@gmail.com
###############################################################################################################################
library(dplyr)
library(readr)
library(ecb)
library(xts)
library(tseries)
library(stringr)
library(vars)
library(MCMCpack)
library(magic)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggplotify)
library(seasonal)
library(grid)



###############################################################################################################################
## preperation:

#load in the factors of Altavilla et al.
#to be found at http://refet.bilkent.edu.tr/research.html
instruments <- read_csv(here::here("input/ois_factors.csv")) %>% as.data.frame()
instruments<-xts(cbind(instruments$RateFactor1, instruments$ConfFactor1, instruments$ConfFactor2, instruments$ConfFactor3), order.by = as.Date(instruments$Date, format = "%Y-%m-%d"))
instruments[is.na(instruments)] <- 0
instruments<-to.monthly(instruments)
colnames(instruments)<- c("Target", "Timing", "FG", "QE")

par(mfrow=(c(4,1)))
plot(instruments$Target)
plot(instruments$Timing)
plot(instruments$FG)
plot(instruments$QE)
par(mfrow=(c(1,1)))

#euribor 1 month 
euribor1m <- get_data("FM.M.U2.EUR.RT.MM.EURIBOR1MD_.HSTA")
euribor1m <- subset(euribor1m, select = c(obstime, obsvalue))
euribor1m$obsvalue<-as.numeric(euribor1m$obsvalue)
euribor1m <- ts(euribor1m$obsvalue, start=c(1994, 1), frequency=12) %>% as.xts()

#euribor 3 month 
euribor3m <- get_data("FM.M.U2.EUR.RT.MM.EURIBOR3MD_.HSTA")
euribor3m <- subset(euribor3m, select = c(obstime, obsvalue))
euribor3m$obsvalue<-as.numeric(euribor3m$obsvalue)
euribor3m <- ts(euribor3m$obsvalue, start=c(1994, 1), frequency=12) %>% as.xts()

#euribor 6 month 
euribor6m <- get_data("FM.M.U2.EUR.RT.MM.EURIBOR6MD_.HSTA")
euribor6m <- subset(euribor6m, select = c(obstime, obsvalue))
euribor6m$obsvalue<-as.numeric(euribor6m$obsvalue)
euribor6m <- ts(euribor6m$obsvalue, start=c(1994, 1), frequency=12) %>% as.xts()

#euribor 12 month 
euribor12m <- get_data("FM.M.U2.EUR.RT.MM.EURIBOR1YD_.HSTA")
euribor12m <- subset(euribor12m, select = c(obstime, obsvalue))
euribor12m$obsvalue<-as.numeric(euribor12m$obsvalue)
euribor12m <- ts(euribor12m$obsvalue, start=c(1994, 1), frequency=12) %>% as.xts() 

#euro area government bond 2 year benchmark yield
byield2y <- get_data("FM.M.U2.EUR.4F.BB.U2_2Y.YLD")
byield2y <- subset(byield2y, select = c(obstime, obsvalue))
byield2y$obsvalue<-as.numeric(byield2y$obsvalue)
byield2y <- ts(byield2y$obsvalue, start=c(1970, 1), frequency=12) %>% as.xts()

#euro area government bond 3 year benchmark yield
byield3y <- get_data("FM.M.U2.EUR.4F.BB.U2_3Y.YLD")
byield3y <- subset(byield3y, select = c(obstime, obsvalue))
byield3y$obsvalue<-as.numeric(byield3y$obsvalue)
byield3y <- ts(byield3y$obsvalue, start=c(1970, 1), frequency=12) %>% as.xts()

#euro area government bond 5 year benchmark yield
byield5y <- get_data("FM.M.U2.EUR.4F.BB.U2_5Y.YLD")
byield5y <- subset(byield5y, select = c(obstime, obsvalue))
byield5y$obsvalue<-as.numeric(byield5y$obsvalue)
byield5y <- ts(byield5y$obsvalue, start=c(1970, 1), frequency=12) %>% as.xts()

#euro area government bond 10 year benchmark yield
byield10y <- get_data("FM.M.U2.EUR.4F.BB.U2_10Y.YLD")
byield10y <- subset(byield10y, select = c(obstime, obsvalue))
byield10y$obsvalue<-as.numeric(byield10y$obsvalue)
byield10y <- ts(byield10y$obsvalue, start=c(1970, 1), frequency=12) %>% as.xts()



e.yields <- cbind(euribor1m, euribor3m, euribor6m, euribor12m)

b.yields <- cbind(byield2y, byield3y, byield5y, byield10y)



#production 1 month 
prod <- get_data("STS.M.I8.Y.PROD.NS0020.4.000")
prod <- subset(prod, select = c(obstime, obsvalue))
prod$obsvalue<-as.numeric(prod$obsvalue)
prod <- ts(prod$obsvalue, start=c(1991, 1), frequency=12) 
prod <- final(seas(prod)) %>% as.xts()

#hicp 1 month index, not changes
hicp <- get_data("ICP.M.U2.N.000000.4.INX")
hicp <- subset(hicp, select = c(obstime, obsvalue))
hicp$obsvalue<-as.numeric(hicp$obsvalue)
hicp <- ts(hicp$obsvalue, start=c(1996, 1), frequency=12) 
hicp <- final(seas(hicp)) %>% as.xts()



#ciss bondsmarket subindex 
ciss.bond <- get_data("CISS.D.U2.Z0Z.4F.EC.SS_BM.CON")
ciss.bond <- subset(ciss.bond, select = c(obstime, obsvalue))
ciss.bond$obsvalue<-as.numeric(ciss.bond$obsvalue)
ciss.bond<-xts(ciss.bond$obsvalue, order.by = as.Date(ciss.bond$obstime, format = "%Y-%m-%d"))
ciss.bond.m<-period.apply(ciss.bond, INDEX = endpoints(ciss.bond, on = "months"), FUN = mean) #create montly data
ciss.bond.m<-ts(ciss.bond.m, start=c(1999, 1), frequency=12) %>% as.xts() 

#ciss equity subindex
ciss.equity <- get_data("CISS.D.U2.Z0Z.4F.EC.SS_EM.CON")
ciss.equity <- subset(ciss.equity, select = c(obstime, obsvalue))
ciss.equity$obsvalue<-as.numeric(ciss.equity$obsvalue)
ciss.equity<-xts(ciss.equity$obsvalue, order.by = as.Date(ciss.equity$obstime, format = "%Y-%m-%d"))
ciss.equity.m<-period.apply(ciss.equity, INDEX = endpoints(ciss.equity, on = "months"), FUN = mean) #create montly data
ciss.equity.m<-ts(ciss.equity.m, start=c(1999, 1), frequency=12) %>% as.xts() 

#ciss financial intermediers subindex
ciss.fincint <- get_data("CISS.D.U2.Z0Z.4F.EC.SS_FI.CON")
ciss.fincint <- subset(ciss.fincint, select = c(obstime, obsvalue))
ciss.fincint$obsvalue<-as.numeric(ciss.fincint$obsvalue)
ciss.fincint<-xts(ciss.fincint$obsvalue, order.by = as.Date(ciss.fincint$obstime, format = "%Y-%m-%d"))
ciss.fincint.m<-period.apply(ciss.fincint, INDEX = endpoints(ciss.fincint, on = "months"), FUN = mean) #create montly data
ciss.fincint.m<-ts(ciss.fincint.m, start=c(1999, 1), frequency=12) %>% as.xts() 

#ciss foreign exchange subindex
ciss.fx <- get_data("CISS.D.U2.Z0Z.4F.EC.SS_FX.CON")
ciss.fx <- subset(ciss.fx, select = c(obstime, obsvalue))
ciss.fx$obsvalue<-as.numeric(ciss.fx$obsvalue)
ciss.fx<-xts(ciss.fx$obsvalue, order.by = as.Date(ciss.fx$obstime, format = "%Y-%m-%d"))
ciss.fx.m<-period.apply(ciss.fx, INDEX = endpoints(ciss.fx, on = "months"), FUN = mean) #create montly data
ciss.fx.m<-ts(ciss.fx.m, start=c(1999, 1), frequency=12) %>% as.xts() 

#ciss cmoneymarket subindex
ciss.mon <- get_data("CISS.D.U2.Z0Z.4F.EC.SS_MM.CON")
ciss.mon <- subset(ciss.mon, select = c(obstime, obsvalue))
ciss.mon$obsvalue<-as.numeric(ciss.mon$obsvalue)
ciss.mon<-xts(ciss.mon$obsvalue, order.by = as.Date(ciss.mon$obstime, format = "%Y-%m-%d"))
ciss.mon.m<-period.apply(ciss.mon, INDEX = endpoints(ciss.mon, on = "months"), FUN = mean) #create montly data
ciss.mon.m<-ts(ciss.mon.m, start=c(1999, 1), frequency=12) %>% as.xts() 

#ciss composit indicator
ciss <- get_data("CISS.D.U2.Z0Z.4F.EC.SS_CIN.IDX")
ciss <- subset(ciss, select = c(obstime, obsvalue))
ciss$obsvalue<-as.numeric(ciss$obsvalue)
ciss<-xts(ciss$obsvalue, order.by = as.Date(ciss$obstime, format = "%Y-%m-%d"))
ciss.m<-period.apply(ciss, INDEX = endpoints(ciss, on = "months"), FUN = mean) #create montly data
ciss.m<-ts(ciss.m, start=c(1999, 1), frequency=12) %>% as.xts() 



#create mathing tseries and combine time series
data<-merge(instruments$Target, instruments$Timing, instruments$FG, instruments$QE, e.yields, b.yields, 
            prod, hicp, ciss.bond.m, ciss.equity.m, ciss.fincint.m, ciss.fx.m, ciss.mon.m, ciss.m)
#cut window according to the availability of the instruments
data<-data["2002-01/2018-11"]
#set instruments to 0 for those months where now MP action was taken
data[is.na(data)] <- 0
colnames(data)<-c("Target", "Timing", "FG", "QE", "euribor1m", "euribor3m", "euribor6m", "euribor12m", "byield2y", 
                  "byield3y", "byield5y", "byield10y", "prod", "hicp", "ciss.bond", "ciss.equity", "ciss.fincint", "ciss.fx", "ciss.mon", "ciss")

#get some plots of the data

#hicp and prod
plot(cbind(data$prod, data$hicp))

#ciss indicators
plot(cbind(data$ciss.bond, data$ciss.equity, data$ciss.fincint, data$ciss.fx, data$ciss.mon))

#euribors
plot(e.yields)

#bond yields
plot(b.yields)


###############################################################################################################################
## BAVR LOOP:

#Loop preperation

specification <- c("composite", "subs")

#only post crises sample
samples <- c("2007-01/2018-11")
samplenames <- c("post-crisis")

indicators <- data[,c(5,7,11,12)]
indicatornames <- colnames(indicators)

#number of overall loop reps
totrep<-1

#storage place for all plots
allplots <- list()


for (spec in 1:length(specification)) {
  
  for (s in 1:length(samplenames)) {#selects each of the subsamples
    
    for (i in 1:length(indicatornames)) {#Whole Loop once for each indicator
      
      #select either ciss composite or all subindices
      if (spec==1) {
        #log diff gpd and prod, diff ciss
        fin <- cbind(diff(log(data[,c(13,14)])), diff(data[,20]))
        fin <- cbind(diff(indicators[,i]), fin) # first diff for the indicators (yield curve)
        fin<-na.omit(fin)
        
      } else{
        #log diff gpd and prod, diff ciss
        fin <- cbind(diff(log(data[,c(13,14)])), diff(data[,c(15:19)]))
        fin <- cbind(diff(indicators[,i]), fin) # first diff for the indicators (yield curve)
        fin<-na.omit(fin)
      }
      

      instruments <- data[,c(1:4)]
      instruments <- instruments[-1,] # exclude first observation to fit endogenous data (loses one by log-diff) 
      instrumentsnames <- colnames(instruments)
      
      #select the sample
      fin<-fin[paste(samples[s])] 
      instruments<-instruments[paste(samples[s])]
      
      
      ###----------------------------------- Load data ---------------------------------------------
      
      #Here the data is insertet
      fin <- as.ts(fin)
      Traw <- nrow(fin)
      Yraw <- fin
      
      plot.ts(Yraw)
      #------------------------------------------------------------------------------------
      # useful function
      mlag <- function(X,lag){
        p <- lag
        X <- as.matrix(X)
        Traw <- nrow(X)
        N <- ncol(X)
        Xlag <- matrix(0,Traw,p*N)
        for (ii in 1:p){
          Xlag[(p+1):Traw,(N*(ii-1)+1):(N*ii)]=X[(p+1-ii):(Traw-ii),(1:N)]
        }
        return(Xlag)  
      }
      #######################################################################################
      ### Independent normal-Wishart prior for the VAR
      ######################################################################################
      # needed libraries
      library(MCMCpack) # has the inverse wishart riwish()
      library(magic)
      # specifications
      p <- 2              # number of lags
      
      # Create data matrices
      Y <- as.matrix(Yraw)
      X <- cbind(mlag(Y,p),1)
      
      # look at size of data
      dim(Y)
      dim(X)
      
      #first p rows are zero | conditioning on the first p observations
      Y <- Y[(p+1):nrow(Y),]
      X <- X[(p+1):nrow(X),]
      y <- as.vector(Y)
      
      #the p lags also need to be deterred from the instruments
      instruments <- instruments[(p+1):nrow(instruments),]             # cut of p observations in front, 
      # to fit endognenous data only neccesary 
      # once so done in first j
      # and for j of QE since instruments are new defined there (could be done smarter i guess)
      
      
      # outside loop calculations
      XX <- crossprod(X)
      
      # get useful dimensions
      M    <- ncol(Y)           # number of endogenous variables in the VAR
      bigT <- nrow(Y)           # sample size, do not use T!
      K    <- ncol(X)           # number of parameters per equation
      k    <- K*M               # total number of coefficients
      #------------------------------------------------------------------------------------
      # Initial Values, OLS preliminaries
      #------------------------------------------------------------------------------------
      A_OLS <- solve(crossprod(X))%*%crossprod(X,Y)
      S_OLS <- crossprod(Y-X%*%A_OLS)/(bigT-K)
      
      # initialize draws with OLS estimators
      A_draw <- A_OLS
      S_draw <- S_OLS
      
      # get AR variances
      sigs <- numeric(length=M)
      for(mm in 1:M){
        yuse <- Y[,mm,drop=FALSE]
        xuse <- cbind(X[,seq(mm,M*p,by=M),drop=FALSE],1)
        b    <- solve(crossprod(xuse))%*%crossprod(xuse,yuse)
        sigs[mm] <- crossprod(yuse-xuse%*%b)/(bigT-p-1)
      }
      #------------------------------------------------------------------------------------
      # PRIORS
      #------------------------------------------------------------------------------------
      A_prior <- matrix(0,K,M)
      diag(A_prior) <- 1
      a_prior <- as.vector(A_prior)
      
      # Minnesota prior
      # own lags:       (lambda1/k)^2   # k == lag
      # cross lags:     (sig_i^2/sig_j^2)(lambda2/k)^2
      # deterministics: lambda3*sig_i^2
      lambda1 <- 0.1; lambda2 <- 0.2; lambda3 <- 100
      V_prior <- array(0,c(K,K,M))
      for(mm in 1:M){ # over all equations
        for(pp in 1:p){ # over all lags
          for(mmm in 1:M){ # over all coefficients
            # own lag
            if(mm==mmm){
              V_prior[(pp-1)*M+mmm,(pp-1)*M+mmm,mm] <- (lambda1/pp)^2
            }else{
              V_prior[(pp-1)*M+mmm,(pp-1)*M+mmm,mm] <- (sigs[mm]/sigs[mmm])*(lambda2/pp)^2
            }
          }
        }
        V_prior[K,K,mm] <- lambda3*sigs[mm]
      }
      V_prior    <- lapply(seq(dim(V_prior)[3]), function(x) V_prior[,,x]) # array to list
      V_prior    <- Reduce(magic::adiag,V_prior)
      V_priorinv <- diag(1/diag(V_prior))
      
      # hyperparamter for inverse Wishart
      s0 <- M + 2
      S0 <- (s0 - M - 1)*diag(sigs)
      
      # outside loop calculations
      s_post    <- bigT + s0
      #------------------------------------------------------------------------------------
      # MCMC setup
      #------------------------------------------------------------------------------------
      nsave <- 5000            # number of saved draws
      nburn <- 5000              # number of burned draws
      ntot  <- nsave + nburn      # number of total draws
      nhor  <- 13                 # horizon for IRFs
      fhorz <- 8                  # forecasting horizon
      
      # Container for MCMC draws
      A_store <- array(NA, c(nsave, K, M))
      S_store <- array(NA, c(nsave, M, M))
      
      eig_store <- numeric(length=nsave)
      cou_store <- numeric(length=nsave)  ###For repetitions of sign restrictions 
      
      # Predictions -- dimensions: number of draws x number of variables x forecasting horizon
      yf_store <- array(NA, c(nsave,M,fhorz))
      
      # IRFs -- dimensions: Number of draws x Number of responses x Number of structural shocks x horizon
      IRFproxy_store <- array(NA,c(nsave,M,M,nhor))
      
      
      set.seed(1)
      for(irep in 1:ntot){
        # Step 1: Draw S_draw | Y, A_draw from IW
        # s_overbar = T + s_underbar (s0)
        # S_overbar = (Y-XA)'(Y-XA) + S_underbar (S0)
        # SIGMA | Y ~ iW(s_overbar,S_overbar)
        
        S_post    <- crossprod(Y-X%*%A_draw)
        S_drawinv <- matrix(rWishart(1,s_post,solve(S_post)),M,M) # note that we can also draw from the Wishart
        S_draw    <- solve(S_drawinv)                            # and inverting leads to the inverse-Wishart
        # or using the MCMCpack
        S_draw    <- MCMCpack::riwish(s_post, S_post)
        
        # Step 2: Draw A_draw | Y, S_draw from MVN
        # V_overbar = (SIGMAinv otimes X'X + Vinv_underbar)^{-1}
        # a_overbar = V_overbar (Vinv_underbar a_underbar + (SIGMAinv otimes X')y)
        V_post    <- solve(kronecker(S_drawinv, XX) + V_priorinv) #Voverbar
        A_post    <- V_post %*% (kronecker(S_drawinv, t(X))%*%y + V_priorinv%*%a_prior) #aoverbar
        
        A_draw     <- matrix(A_post + t(chol(V_post))%*%rnorm(k),K,M)
        
        
        # Step 3: Storage/Predictions/IRFs
        if(irep > nburn){
          # Step 3a: Save draws
          A_store[irep-nburn,,] <- A_draw
          S_store[irep-nburn,,] <- S_draw
          
          # Step 3b: Companion Matrix
          Cm <- matrix(0,M*p,M*p)
          Cm[1:M,] <- t(A_draw[1:(M*p),])
          diag(Cm[(M+1):(M*p),1:(M*(p-1))]) <- 1
          Jm <- matrix(0,M*p,M)
          diag(Jm) <- 1
          
          # Step 3d: Check stability
          eig_store[irep-nburn] <- max(Re(eigen(Cm)$values))
          
          # Step 3e: Impulse response functions
          cond.overall <- TRUE
          counter      <- 0
          MaxTries     <- 1000
          
          ########################################################
          # Identification proxy VAR
          
          # calculation of reduced form residuals
          Emat <- Y-X%*%A_draw
          
          # set the proxy
          proxy <- instruments[,i] #here each of the j instruments is used to instrument i indicators
          
          # get the dimension of S_draw
          sigK <- ncol(S_draw)
          # identification step
          fitted.err <- lm(Emat[,1] ~ proxy)$fitted
          b21ib11    <- t(lm(Emat[,-1] ~ fitted.err-1)$coef) # => s21/s11
          Sig11      <- matrix(S_draw[1,1], 1, 1)
          Sig21      <- matrix(S_draw[2:sigK,1],sigK-1,1)
          Sig12      <- matrix(S_draw[1,2:sigK],1,sigK-1)
          Sig22      <- matrix(S_draw[2:sigK,2:sigK],sigK-1,sigK-1) # => Sig11 - Sig22 Partitioning ofVarCov
          ZZp        <- b21ib11%*%Sig11%*%t(b21ib11) - Sig21%*%t(b21ib11)+b21ib11%*%t(Sig21)+Sig22 # => Q
          b12b12p    <- t(Sig21-b21ib11%*%Sig11)%*%solve(ZZp)%*%(Sig21-b21ib11%*%Sig11) # => (s12*s21')
          b11b11p    <- Sig11 - b12b12p  # => (s^p^2)
          b11        <- sqrt(b11b11p) # => (s^p)
          if(is.nan(b11)) b11 <- 1e-10
          shock      <- c(b11, b21ib11*c(b11)) # => the final shock, stacked together again
          # shock      <- shock/shock[1] # normalize to unit shock
          
          #here random data for the other shocks is inserted
          randMat <- matrix(rnorm(sigK*sigK,0,1),sigK,sigK)
          Q <- qr(randMat)
          Q <- qr.Q(Q)
          Q[,1] <- shock
          
          #the final shock
          shock.proxy<-Q
          
          # Temporary objects for state space representation
          irf.mat.proxy <- array(NA,c(M,M,nhor))
          
          #Normalise shocks to 1 units
          shock.proxy <- shock.proxy%*%diag(-1/diag(shock.proxy)) #also normalizes the random shocks
          
          # Impulse --> shock at t = 0:
          irf.mat.proxy[,,1] <- shock.proxy                                   
          
          Cmi <- Cm
          for(ihorz in 2:nhor){
            irf.mat.proxy[,,ihorz] <- t(Jm)%*%Cmi%*%Jm%*%shock.proxy         
            Cmi <- Cmi%*%Cm
          }
          IRFproxy_store[irep-nburn,,,] <- irf.mat.proxy
          
        }
        if(irep%%50==0) print(paste0("Round: ", irep,"/",ntot))
      }
      
      
      #Quantiles over the first dimension (number of saved draws)
      IRF_low    <- apply(IRFproxy_store, c(2,3,4), quantile, 0.1,na.rm=TRUE)
      IRF_high   <- apply(IRFproxy_store, c(2,3,4), quantile, 0.9,na.rm=TRUE)
      IRF_median <- apply(IRFproxy_store, c(2,3,4), median, na.rm=TRUE)
      
      IRF_low.2    <- apply(IRFproxy_store, c(2,3,4), quantile, 0.16,na.rm=TRUE)
      IRF_high.2   <- apply(IRFproxy_store, c(2,3,4), quantile, 0.84,na.rm=TRUE)
      


###############################################################################################################################
## create final plots to be found in the paper
      
      #ggplotting
      ####################################################################
      plots<-list()
      indicatorfullnames <- c("Euribor 1M", "Euribor 6M", "5Y Bond Yield", "10Y Bond Yield" )
      if (spec==1) {
        itemnames <- c(paste0(indicatorfullnames[i]),"Production", "HICP", "CISS")
      }else{
        itemnames <- c(paste0(indicatorfullnames[i]),"Production", "HICP",  "Bond Market", "Equity Market", "Financial Intermediaries", "FX Market", "Money Market")
      }
      for (plotn in 1:M) {
        plotdat<-cbind(c(1:13), 
                       IRF_median[plotn,1,], IRF_low[plotn,1,], IRF_high[plotn,1,],
                       IRF_low.2[plotn,1,], IRF_high.2[plotn,1,]) %>% as.data.frame()
        colnames(plotdat) <- c("time", "med", "low", "high", "low2", "high2")
        min1 <- min(plotdat$low)
        max1 <- max(plotdat$high)
        plots[[plotn]] <-ggplot(plotdat) + 
          geom_hline(yintercept = 0, linetype="dashed", color = "black", size = 0.2) +
          geom_line(aes(time, med), size = 0.2) +
          geom_line(aes(time, high), colour = "lightblue", size = 0.2) +
          geom_line(aes(time, low), colour = "lightblue", size = 0.2) + 
          geom_ribbon(aes(x=time, ymax=high, ymin=low), fill="lightblue", alpha=.25) +
          #geom_ribbon(aes(x=time, ymax=high2, ymin=low2), fill="#3498DB", alpha=.25) +
          xlab(paste0(itemnames[plotn])) + ylab("")  +
          ylim(min1, max1) +
          theme_classic(base_size = 5) +
          #theme_classic()+
          theme(legend.position="none",
                plot.margin=unit(c(0.1,0,0.15,0), "cm"))+
          theme(axis.title.x=element_text(size=rel(0.85)),
                axis.text.y=element_text(size=rel(0.85)),
                axis.text.x=element_text(size=rel(0.85)))
        names(plots)[plotn] <- paste0(itemnames[plotn])
        
      }
      

      
      #save plots in allplots
      allplots[[totrep]] <- plots
      names(allplots)[totrep] <- paste0(instrumentsnames[i],"_", specification[spec])
      totrep<-totrep+1
    }
    
  }
  
}

###############################################################################################################################
## Create Final Plots for Paper

output <- here::here("output")

###Composit Indicator

#Target
pdf(paste0(output, "/Target_Comp.pdf"), 
    width = 2.5,
    height = 0.6)
grid.arrange(allplots$Target_composite$`Euribor 1M`, 
             allplots$Target_composite$Production,
             allplots$Target_composite$HICP,
             allplots$Target_composite$CISS,
             nrow = 1)
dev.off()

#Timing
pdf(paste0(output, "/Timing_Comp.pdf"), 
    width = 2.5,
    height = 0.6)
grid.arrange(allplots$Timing_composite$`Euribor 6M`,
             allplots$Timing_composite$Production,
             allplots$Timing_composite$HICP,
             allplots$Timing_composite$CISS,
             nrow = 1)
dev.off()

#FG
pdf(paste0(output, "/FG_Comp.pdf"), 
    width = 2.5,
    height = 0.6)
grid.arrange(allplots$FG_composite$`5Y Bond Yield`,
             allplots$FG_composite$Production,
             allplots$FG_composite$HICP,
             allplots$FG_composite$CISS,
             nrow = 1)
dev.off()

#QE
pdf(paste0(output, "/QE_Comp.pdf"), 
    width = 2.5,
    height = 0.6)
grid.arrange(allplots$QE_composite$`10Y Bond Yield`, 
             allplots$QE_composite$Production,
             allplots$QE_composite$HICP,
             allplots$QE_composite$CISS,
             nrow = 1)
dev.off()

###Sub Indicators
################################################################################
#Target
pdf(paste0(output, "/Target_Sub.pdf"), 
    width = 5,
    height = 0.8)
grid.arrange(allplots$Target_subs$`Bond Market`, 
             allplots$Target_subs$`Equity Market`, 
             allplots$Target_subs$`Financial Intermediaries`, 
             allplots$Target_subs$`FX Market`, 
             allplots$Target_subs$`Money Market`, 
             nrow = 1)
dev.off()

pdf(paste0(output, "/Timing_Sub.pdf"), 
    width = 5,
    height = 0.8)
grid.arrange(allplots$Timing_subs$`Bond Market`, 
             allplots$Timing_subs$`Equity Market`, 
             allplots$Timing_subs$`Financial Intermediaries`, 
             allplots$Timing_subs$`FX Market`, 
             allplots$Timing_subs$`Money Market`, 
             nrow = 1)
dev.off()

pdf(paste0(output, "/FG_Sub.pdf"), 
    width = 5,
    height = 0.8)
grid.arrange(allplots$FG_subs$`Bond Market`, 
             allplots$FG_subs$`Equity Market`, 
             allplots$FG_subs$`Financial Intermediaries`, 
             allplots$FG_subs$`FX Market`, 
             allplots$FG_subs$`Money Market`, 
             nrow = 1)
dev.off()


pdf(paste0(output, "/QE_Sub.pdf"), 
    width = 5,
    height = 0.8)
grid.arrange(allplots$QE_subs$`Bond Market`, 
             allplots$QE_subs$`Equity Market`, 
             allplots$QE_subs$`Financial Intermediaries`, 
             allplots$QE_subs$`FX Market`, 
             allplots$QE_subs$`Money Market`, 
             nrow = 1)
dev.off()








