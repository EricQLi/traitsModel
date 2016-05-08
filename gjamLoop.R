.gibbsLoop <- function(formula, xdata, ydata, modelList){
  
  holdoutN      <-  0
  holdoutIndex  <- numeric(0)
  betaPrior     <- NULL
  pgPrior       <- NULL
  specByTrait   <- traitTypes <- NULL
  modelSummary  <- NULL
  censor <- censorCA <- censorDA <- NULL
  CCgroups <- FCgroups <- NULL
  effort        <- NULL
  breakList     <- NULL
  notStandard   <- NULL
  traitList     <- NULL
  pg            <- NULL
  ng            <- 2000
  burnin        <- 500
  flist         <- NULL
  ZEROINFL      <- F
  REDUCT        <- F
  PREDICTX      <- T
  y0 <- N  <- r <- otherpar <- NULL
  alpha.DP <- ncol(ydata)          # large values give more variation
  
  for(k in 1:length(modelList))assign( names(modelList)[k], modelList[[k]] )
  if(!is.null(traitList))for(k in 1:length(traitList))
    assign( names(traitList)[k], traitList[[k]] )
  
  if(burnin >= ng)           stop( 'burnin must be > no. MCMC steps, ng' )
  if('censor' %in% names(modelList)){
    for(k in 1:length(censor)){
      if(!names(censor)[[k]] %in% c('CA','DA'))stop('censor name must be CA or DA')
      if( nrow(censor[[k]]$partition) != 3 )stop('censor matrix: 3 rows for value, lo, hi')
      rownames(censor[[k]]$partition) <- c('value','lo','hi')
    }
  }
  
  S <- ncol(ydata)
  if(length(typeNames) == 1)typeNames <- rep(typeNames,S)
  if(length(typeNames) != S) stop('typeNames must be one value or no. columns in y')
  
  tmp <- .buildYdata(ydata, typeNames, CCgroups=NULL, FCgroups=NULL)
  y   <- tmp$y
  typeNames <- tmp$typeNames
  CCgroups  <- tmp$CCgroups
  FCgroups  <- tmp$FCgroups
  CATgroups <- tmp$CATgroups
  
  S <- ncol(y)
  n <- nrow(y)
  
  tmp      <- .gjamGetTypes(typeNames)
  typeCols <- tmp$typeCols
  typeFull <- tmp$typeFull
  typeCode <- tmp$TYPES[typeCols]
  allTypes <- sort(unique(typeCols))
  # typeNames <- tmp$typeNames
  
  tmp <- .gjamXY(formula, xdata, y, typeNames, notStandard)
  x <- tmp$x; y <- tmp$y;            snames <- tmp$snames;      xnames <- tmp$xnames
  isInt         <- tmp$isInt;        intMat <- tmp$intMat;    isSquare <- tmp$isSquare
  factorList    <- tmp$factorList; isFactor <- tmp$isFactor; isNonLinX <- tmp$isNonLinX
  designTable   <- tmp$designTable;  xscale <- tmp$xscale;   predXcols <- tmp$predXcols
  standMat      <- tmp$standMat;    standMu <- tmp$standMu
  standRows     <- tmp$standRows;  facNames <- tmp$facNames
  contrast      <- tmp$contrast
  if(length(factorList) == 0)factorList <- NULL
  
  modelSummary  <- append(modelSummary,
                          list(designTable = designTable, isFactor = isFactor, 
                               factorList = factorList, contrasts = contrast))
  Q <- ncol(x)
  
  tmp <- .gjamMissingValues(x,y)
  xmiss  <- tmp$xmiss; xbound <- tmp$xbound; missX <- tmp$missX
  missX2 <- tmp$missX2; ymiss <- tmp$ymiss; missY <- tmp$missY
  xprior <- tmp$xprior
  yprior <- tmp$yprior
  nmiss  <- length(xmiss)
  mmiss  <- length(ymiss)
  if(nmiss > 0)x[xmiss] <- xprior
  if(mmiss > 0)y[ymiss] <- yprior
  
  npar  <- S*(S + 1)/2 + Q*S
  nobs  <- S*n
  ratio <- 1/5
  Smax  <- floor( 2*(n*ratio - Q) - 1 )
  Nmax  <- min( floor( S*(n*ratio - Q)/3 ), 20)    # r  = 3
  
  OVERRIDE <- F
  if( 'REDUCT' %in% names(modelSummary) ){
    if( !modelSummary$REDUCT ) OVERRIDE <- T
  }
  if( !'reductList' %in% names(modelList) & S > min(Smax,100) & !OVERRIDE ){
    reductList <- list(r = 3, N = Nmax, alpha.DP = alpha.DP)
    for(k in 1:length(reductList))assign( names(reductList)[k], reductList[[k]] )
    REDUCT <- T
    message( paste('for n =',n,'dimension reduction from',Smax,'in Sigma to',Nmax) )
  }
  if('reductList' %in% names(modelList) & !OVERRIDE){
    for(k in 1:length(reductList))assign( names(reductList)[k], reductList[[k]] )
    REDUCT <- T
  }
  
  updateBeta <- .gjamUpdateBetaNoPrior
  loBeta <- hiBeta <- NULL
  
  if(!is.null(betaPrior)){
    loBeta     <- as.vector(betaPrior$lo)
    hiBeta     <- as.vector(betaPrior$hi)
    updateBeta <- .gjamUpdateBetaPrior
  }                 
  
  tmp <- .gjamHoldoutSetup(holdoutIndex, holdoutN, n)
  holdoutIndex <- tmp$holdoutIndex; holdoutN <- tmp$holdoutN
  inSamples <- tmp$inSamples; nIn <- tmp$nIn
  
  tmp <- .gjamSetup(typeNames, x, y, breakList, holdoutN, holdoutIndex,
                    censor=censor, effort=effort) 
  w <- tmp$w; z <- tmp$z; y <- tmp$y; other <- tmp$other; cuts <- tmp$cuts
  cutLo       <- tmp$cutLo; cutHi <- tmp$cutHi; plo <- tmp$plo; phi <- tmp$phi
  ordCols     <- tmp$ordCols; disCols <- tmp$disCols; compCols <- tmp$compCols 
  classBySpec <- tmp$classBySpec; breakMat <- tmp$breakMat
  minOrd      <- tmp$minOrd; maxOrd <- tmp$maxOrd; censorCA <- tmp$censorCA
  censorDA    <- tmp$censorDA; ncut <- ncol(cuts); corCols <- tmp$corCols
  catCols     <- which(attr(typeNames,'CATgroups') > 0)
  sampleW     <- tmp$sampleW
  
  byCol <- byRow <- F
  if(attr(sampleW,'type') == 'cols')byCol <- T
  if(attr(sampleW,'type') == 'rows')byRow <- T
  indexW <- attr(sampleW,'index')
  
  notCorCols <- c(1:S)
  if(length(corCols) > 0)notCorCols <- notCorCols[-corCols]
  
  modelSummary <- append(modelSummary,list(classBySpec = classBySpec))
  
  tg       <- cutg <- cuts
  sigmaDf  <- nIn - Q + S - 1
  sg <- diag(.1,S)
  
  notOther <- c(1:S)
  sgOther  <- NULL
  if(length(other) > 0){                     
    notOther   <- notOther[!notOther %in% other]
    sg[other,] <- sg[,other] <- 0
    sgOther    <- matrix( cbind(other,other),ncol=2 )
    sg[sgOther] <- .0001
  }
  
  if(byCol){
    inw <- intersect( colnames(y)[indexW], colnames(y)[notOther] )
    indexW <- match(inw,colnames(y)[notOther])
  }
  
  ##############################
  .param.fn <- .paramWrapper(REDUCT,inSamples,SS=length(notOther),loBeta,
                             hiBeta,updateBeta=updateBeta)
  sigmaerror <- .1
  otherpar   <- list(S = S, Q = Q, sigmaerror = sigmaerror, Z = NA, K =rep(1,S),
                     sigmaDf = sigmaDf)
  kgibbs <- sigErrGibbs <- rndEff <- NULL
  
  if(REDUCT){
    message( paste('Dimension reduction from',S,'responses to',N,'X',r,'in covariance') )
    otherpar$N <- N
    otherpar$r <- r
    otherpar$Z <- .rmvnormArma(N,rep(0,r),1/S*diag(r))
    otherpar$D <- .riwish(df = (2 + r + N), 
                          S = (crossprod(otherpar$Z) +
                                 2*2*diag(rgamma(r,shape=1,rate=0.001))))
    otherpar$K <- sample(1:N,length(notOther),replace=T)
    otherpar$sigmaerror <- 0.1
    otherpar$alpha.DP <- alpha.DP
    otherpar$pvec     <- .sample.p(N=N, avec=rep(alpha.DP/N,(N-1)),
                                   bvec=((N-1):1)*alpha.DP/N, K=otherpar$K)
    kgibbs <- matrix(1,ng,S)
    sgibbs <- matrix(0,ng, N*r)
    nnames <- paste('N',1:N,sep='-')
    rnames <- paste('r',1:r,sep='-')
    colnames(sgibbs) <- .multivarChainNames(nnames,rnames)
    sigErrGibbs <- rep(0,ng)
  } else {
    Kindex <- which(as.vector(lower.tri(diag(S),diag=T)))
    nK     <- length(Kindex)
    sgibbs <- matrix(0,ng,nK)
    colnames(sgibbs) <- .multivarChainNames(snames,snames)[Kindex] # half matrix
  }
  
  bg  <- alpha <- matrix(0,Q,S)
  out <- .param.fn(x, beta = bg[,notOther], Y = w[,notOther], otherpar)
  sg[notOther,notOther] <- out$sg
  sg[other,other] <- 1
  bg[,notOther] <- alpha[,notOther] <- out$bg 
  otherpar      <- out$otherpar
  
  rownames(sg) <- colnames(sg) <- colnames(bg) <- snames
  colnames(x)  <- xnames
  
  
  bgibbs <- matrix(0,ng,S*Q)
  colnames(bgibbs) <- .multivarChainNames(xnames,snames)
  
  TRAITS <- F
  if(length(specByTrait) > 0){
    TRAITS <- T
    specByTrait <- specByTrait[colnames(y),]
    tnames   <- colnames(specByTrait)
    M        <- ncol(specByTrait)
    specByTrait <- t(specByTrait)
    
    agibbs <- matrix(0,ng,M*Q)
    mgibbs <- matrix(0,ng,M*M)
    tpred  <- tpred2 <- matrix(0,n,M)
    colnames(agibbs) <- .multivarChainNames(xnames,tnames)
    colnames(mgibbs) <- .multivarChainNames(tnames,tnames)
  }
  
  if('OC' %in% typeCode){
    cnames       <- paste('C',1:ncut,sep='-')
    nor          <- length(ordCols)
    cgibbs <- matrix(0,ng,(ncut-3)*nor)
    cutg <- cuts
    colnames(cgibbs) <- as.vector( outer(snames[ordCols],
                                         cnames[-c(1,2,ncut)],paste,sep='_') )
    tmp   <- .gjamGetCuts(z,ordCols)
    cutLo <- tmp$cutLo
    cutHi <- tmp$cutHi
    plo[,ordCols] <- tg[cutLo]                                        
    phi[,ordCols] <- tg[cutHi]
    #   lastOrd <- max(maxOrd) + 1
    lastOrd <- ncol(tg)
  }
  
  
  tmp <- .gjamGetTypes(typeNames)
  typeFull <- tmp$typeFull
  typeCols <- tmp$typeCols
  allTypes <- unique(typeCols)
  
  .updateW <- .wWrapper(REDUCT, x, y, n, S, effort, corCols, typeNames, 
                        typeFull, typeCols, 
                        allTypes, holdoutN, holdoutIndex, 
                        censorCA, censorDA, notOther, sampleW, byRow, byCol,
                        indexW)
  
  mm <- apply(z,2,range)
  rownames(mm) <- c('lo bin','hi bin')
  
  ycount <- rowSums(y)
  if('CC' %in% typeCode)ycount <- rowSums(y[,compCols])
  
  priorXIV <- diag(1e-5,ncol(x))
  priorX   <- colMeans(x)
  predx    <- predx2 <- x*0
  xpred    <- x
  xpred[,isNonLinX] <- 0            #all nonlinear terms (e.g., interactions)
  px <- 1:Q
  if(!is.null(isNonLinX))px <- px[-isNonLinX]
  px <- px[!xnames[px] %in% isFactor]
  px       <- px[px != 1]
  
  ypred  <- ypred2 <- wpred  <- wpred2 <- sumb <- ymissPred <- ymissPred2 <- y*0
  sumDev <- 0   #for DIC
  sMean  <- sg*0
  ntot   <- 0
  
  pbar <- txtProgressBar(min=1,max=ng,style=3)
  
  if(is.null(pgPrior)){
    pgMu <- .97
    p1 <- n*S
    p2 <- p1*(1/pgMu - 1)
    pgPrior <- c(p1,p2)
  }
  rndEff <- w*0
  
  richness <- NULL
  RICHNESS <- F
  
  notRichness <- which(!typeNames %in% c('CON','CAT'))
  notRichness <- sort(intersect(notRichness, notOther))
  if(length(notRichness) > 0)RICHNESS <- T
  
  kcluster <- matrix(0,S,S)
  kii  <- which(lower.tri(kcluster,diag=F))
  kcluster <- rep(0,length(kii))
  
  #  cg <- .cov2Cor(sg)
  
  for(g in 1:ng){ ########################################################
    
    tmp <- .param.fn(x, beta = bg[,notOther], Y = w[,notOther], otherpar)
    sg[notOther,notOther] <- tmp$sg
    bg[,notOther]         <- alpha[,notOther] <- tmp$bg 
    otherpar              <- tmp$otherpar
    
    if(REDUCT){
      rndEff[,notOther]   <- tmp$rndEff
      sigmaerror          <- otherpar$sigmaerror
      kgibbs[g,notOther]  <- otherpar$K
      sgibbs[g,]          <- as.vector(otherpar$Z)
      sigErrGibbs[g]      <- otherpar$sigmaerror
      sg[sgOther]         <- otherpar$sigmaerror
    } else {
      sgibbs[g,] <- sg[Kindex]
    }
    
    alpha <- .sqrtRootMatrix(bg,sg,DIVIDE=T)
    
    if( 'OC' %in% typeCode ){
      tg   <- .gjamUpdateTheta(w,tg,cutLo,cutHi,ordCols,
                               holdoutN,holdoutIndex,minOrd,maxOrd) # var scale
      cutg <- .gjamCuts2theta(tg,ss = sg[ordCols,ordCols]) # corr scale
      breakMat[ordCols,1:lastOrd] <- cutg
      cgibbs[g,] <- as.vector( cutg[,-c(1,2,ncut)] )
      
      plo[,ordCols] <- cutg[cutLo]
      phi[,ordCols] <- cutg[cutHi]
    }
    
    muw   <- x%*%bg
    
    tmp <- .updateW( w, muw, sg, alpha, cutg, plo, phi, rndEff, sigmaerror )
    w   <- tmp$w
    yp  <- tmp$yp
    plo <- tmp$plo
    phi <- tmp$phi
    
    setTxtProgressBar(pbar,g)
    
    if(mmiss > 0)y[ymiss] <- yp[ymiss]
    
    if(nmiss > 0){
      sinv     <- .invertSigma(sg,sigmaerror,otherpar,REDUCT)
      x[xmiss] <- .imputX_MVN(x,w,bg,xmiss,sinv,xprior=xprior,
                              xbound=xbound)[xmiss]
      XX  <- crossprod(x)
      IXX <- solve(XX)
      missX <- missX + x[xmiss]
      missX2 <- missX2 + x[xmiss]^2
    }
    
    bgs <- bg                        # unstandardize
    ags <- alpha
    if(length(standRows) > 0){
      bgs[standRows,] <- bg[standRows,]/standMat
      ags[standRows,] <- ags[standRows,]/standMat
    }
    
    bgibbs[g,] <- bgs
    
    if(TRAITS){
      Atrait <- bg%*%t(specByTrait[,colnames(yp)])
      Strait <- specByTrait[,colnames(yp)]%*%sg%*%t(specByTrait[,colnames(yp)])
      agibbs[g,] <- Atrait
      mgibbs[g,] <- Strait
    }
    
    if(PREDICTX & length(predXcols) > 0){
      if(!is.null(isNonLinX)){
        
        xpred <- .predictY2X_nonLinear(xpred, yy=w[,notOther],
                                       bb=bg[,notOther],ss=sg[notOther,notOther],
                                       priorIV = priorXIV,priorX=priorX,
                                       predCols=isNonLinX,isInt,intMat,
                                       isFactor,factorList, contrast = contrast)$x
      }
      
      if(length(px) > 0){
        sinv <- .invertSigma(sg[notOther,notOther],sigmaerror,otherpar,REDUCT)
        xpred[,px] <- .predictY2X_linear(xpred, yy=w[,notOther],
                                         bb=bg[,notOther],
                                         ss=sg[notOther,notOther], 
                                         sinv = sinv,
                                         priorIV = priorXIV, 
                                         priorX=priorX,predCols=px, REDUCT=REDUCT)[,px]
      }
    }
    
    if(g > burnin){
      
      ntot   <- ntot + 1
      ypred  <- ypred + yp
      ypred2 <- ypred2 + yp^2
      sumDev <- sumDev - 2*sum(.dMVN(w[,notOther],x%*%bg[,notOther],
                                     sg[notOther,notOther]) )
      sMean  <- sMean + sg
      
      wpred  <- wpred + w
      wpred2 <- wpred2 + w^2
      
      if(REDUCT){
        tmp <- dist(otherpar$K) 
        kcluster[tmp == 0] <- kcluster[tmp == 0] + 1
      }
      
      if(RICHNESS){
        yy <- yp[,notRichness]
        if(length(notRichness) == 0)yy <- yp
        yy[yy > 0] <- 1
        yy[yy < 0] <- 0
        richness <- .add2matrix(rowSums(yy),richness)
      }
      
      if(mmiss > 0){
        ymissPred[ymiss]  <- ymissPred[ymiss] + y[ymiss]
        ymissPred2[ymiss] <- ymissPred2[ymiss] + y[ymiss]^2
      }
      
      if(PREDICTX & length(predXcols) > 0){
        predx  <- predx + xpred
        predx2 <- predx2 + xpred^2
      }
      
      if(TRAITS){
        sumYp <- rowSums(yp)
        yw     <- sweep(yp,1,sumYp,'/')
        yw[sumYp==0,] <- 1/ncol(yw)
        Ttrait <- .gjamPredictTraits(yw,specByTrait[,colnames(yp)], traitTypes)
        tpred  <- tpred + Ttrait
        tpred2 <- tpred2 + Ttrait^2
      }
    }
  }     ################# end gibbs loop ####################
  
  otherpar$S <- S
  otherpar$Q <- Q
  otherpar$snames <- snames
  otherpar$xnames <- xnames
  kluster <- NULL
  
  if(REDUCT){
    kluster <- kcluster/ntot
    otherpar$cluster <- kluster
  }
  
  if(RICHNESS)richness <- richness/ntot
  
  if(mmiss > 0){
    ymissPred[ymiss]  <- ymissPred[ymiss]/ntot
    ymissPred2[ymiss] <- sqrt(ymissPred2[ymiss]/ntot - ymissPred[ymiss]^2)
  }
  
  x[xmiss] <- missX/ng
  xmissMu  <- missX/ng
  xmissSd  <- sqrt( missX2/ng - xmissMu^2 )
  
  sMean <- sMean/ntot
  
  chains <- list( sgibbs = sgibbs, bgibbs = bgibbs) 
  if(REDUCT) chains <- append(chains,list(kgibbs = kgibbs, sigErrGibbs = sigErrGibbs))
  
  xscore <- xpredMu <- xpredSd <- NULL
  if(PREDICTX){
    xpredMu <- predx/ntot
    xpredSd <- predx2/ntot - xpredMu^2
    xpredSd[xpredSd < 0] <- 0
    xpredSd <- sqrt(xpredSd)
    if(Q == 2)xscore <- mean( .getScoreNorm(x[,2],xpredMu[,2],xpredSd[,2]^2) )
    if(Q > 2) xscore <- colMeans( .getScoreNorm(x[,-1],xpredMu[,-1],xpredSd[,-1]^2) )
  }
  
  standX <- NULL
  if(length(standRows) > 0){                #unstandardize
    x[,standRows] <- x[,standRows]*
      matrix( standMat[,1], n, length(standRows),byrow=T ) + 
      matrix( standMu[,1], n, length(standRows),byrow=T )
    if(PREDICTX){
      xpredMu[,standRows] <- xpredMu[,standRows]*
        matrix( standMat[,1], n, length(standRows),byrow=T ) + 
        matrix( standMu[,1], n, length(standRows),byrow=T )
      xpredSd[,standRows] <- xpredSd[,standRows]*
        matrix( standMat[,1], n, length(standRows),byrow=T ) 
    }
    if(length(xmiss) > 0){
      wmm <- which( xnames[xmiss[,2]] %in% standRows )
      if(length(wmm) > 0){
        xmissMu <- x[xmiss]
        xmissSd[xmiss[wmm,]] <- xmissSd[xmiss[wmm,]]*standMat[xmiss[wmm,2],1]
      }
    }
    standX <- cbind(standMu[,1],standMat[,1])
    colnames(standX) <- c('xmean','xsd')
    rownames(standX) <- rownames(standMat)
  }
  
  tmp <- .processPars(bgibbs)$summary
  bMu <- matrix(tmp[,'estimate'], Q, S)
  bSe <- matrix(tmp[,'se'], Q, S)
  #  wz  <- which(tmp[,'0.025'] > 0 | tmp[,'0.975'] < 0)
  #  bwz <- matrix( unlist(strsplit(names(wz),'_')),ncol=2,byrow=T)[,1]
  
  # betaSens, sigma and R
  
  ns <- 500
  ji <- sample(burnin:ng,ns,replace=T)
  
  tmp <- .expandSigmaChains(snames,simIndex=ji, sgibbs, sigErrGibbs, kgibbs, 
                            bgibbs, otherpar, REDUCT, CHAINS=F)
  betaMu <- tmp$bMu; betaSe <- tmp$bSe; corMu <- tmp$rMu; corSe <- tmp$rSe
  sigMu  <- tmp$sMu; sigSe  <- tmp$sSe; xSensMu <- tmp$bsMu; xSensSe <- tmp$bsSe
  
  yMu <- ypred/ntot
  ySd <- sqrt(ypred2/ntot - yMu^2)
  cMu <- cuts
  cSe <- numeric(0)
  
  wMu <- wpred/ntot
  wpp <- pmax(0,wpred2/ntot - wMu^2)
  wSd <- sqrt(wpp)
  
  tMu <- tSd <- tMuOrd <- btMu <- btSe <- stMu <- stSe <- numeric(0)
  
  if(TRAITS){
    
    tMu <- tpred/ntot
    tSd <- sqrt(tpred2/ntot - tMu^2)
    wo  <- which(traitTypes == 'OC')    #predict ordinal scores
    M   <- ncol(tMu)
    
    if(length(wo) > 0){
      tMuOrd <- tMu*0
      for(j in wo)tMuOrd[,j] <- round(tMu[,j],0) - 1
      tMuOrd <- tMuOrd[,wo]
    }
    
    tmp <- .processPars(agibbs)$summary
    btMu <- matrix(tmp[,'estimate'], Q, M)
    btSe <- matrix(tmp[,'se'], Q, M)
    
    tmp <- .processPars(mgibbs)$summary
    stMu <- matrix(tmp[,'estimate'],M,M)
    stSe <- matrix(tmp[,'se'],M,M)
    
    rownames(btMu) <- rownames(btSe) <- colnames(x)
    colnames(btMu) <- colnames(btSe) <- rownames(stMu) <- colnames(stMu) <- 
      rownames(stSe) <- colnames(stSe) <- tnames
    
    chains <- append( chains,list('agibbs' = agibbs))
    chains <- append( chains, list('mgibbs' = mgibbs) ) 
  }
  
  # note: on latent w scale
  meanDev <- sumDev/ntot
  pd  <- meanDev + 2*sum(.dMVN(yMu[,notOther],x%*%bMu[,notOther],
                               sMean[notOther,notOther]) )
  DIC <- 2*pd + meanDev
  
  score <- mean( .getScoreNorm(y[,notOther],yMu[,notOther],
                               ySd[,notOther]^2),na.rm=T )  # gaussian w
  
  if('OC' %in% typeNames){
    nk  <- length(ordCols)
    tmp <- .processPars(cgibbs)$summary
    cMu <- matrix(tmp[,'estimate'],nk,ncut-3)
    cSe <- matrix(tmp[,'se'],nk,ncut-3)
    colnames(cMu) <- colnames(cSe) <- cnames[-c(1,2,ncut)]
    rownames(cMu) <- rownames(cSe) <- snames[ordCols]
    breakMat[ordCols,c(3:(3+(ncol(cMu))-1))] <- cMu
    chains <- c(chains,list(cgibbs = cgibbs))
  }
  
  if('PA' %in% typeNames){
    zMu <- yMu
    zSd <- ySd
  }
  
  prAbs <- betaZeroMu <- betaZeroSe <- numeric(0)
  
  colnames(bMu)   <- colnames(bSe) <- snames
  rownames(bMu)   <- rownames(bSe) <- xnames
  
  modelSummary <- append(modelSummary,
                         list(typeNames = typeNames,DIC = DIC, score = score, 
                              xscore = xscore,
                              tMuOrd = tMuOrd,tMu = tMu,tSd = tSd,
                              cutMu = cMu, cutSe = cSe, 
                              betaMu = bMu, betaSe = bSe, corMu = corMu, corSe = corSe, 
                              sigMu = sigMu, sigSe = sigSe, 
                              xSensMu = xSensMu, xSensSe = xSensSe,
                              betaTraitMu = btMu, betaTraitSe = btSe, 
                              sigmaTraitMu = stMu, sigmaTraitSe = stSe,
                              xpredMu = xpredMu, xpredSd = xpredSd,
                              yMu = yMu, ySd = ySd, wMu = wMu, wSd = wSd, 
                              prAbs = prAbs))
  
  list(burnin=burnin, ng = ng, REDUCT = REDUCT, otherpar = otherpar, 
       modelList = modelList, missingIndex = xmiss, missingX = xmissMu, 
       missingXSd = xmissSd, chains = chains, x = x, y = y, holdoutIndex = holdoutIndex,
       richness = richness, yMissMu = ymissPred, yMissSd = ymissPred2, ymiss = ymiss, 
       modelSummary = modelSummary, breakMat = breakMat, standX = standX,
       censor = censor, TRAITS = TRAITS, traitList = traitList)
}
