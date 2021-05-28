# source("DiffMenWomen.R")
suppressPackageStartupMessages({
  library('parallel')
})
## Sample from excess
sampfromexc = function (nsamp, exctotal, mfratio) {
  ## If x = M/F, y = F/(F+M)
  ## then 1/(1+x) = 1/(1 + M/F) = F/(M+F) = y
  p = 1/(1+mfratio)
  F = rbinom(nsamp, exctotal, p)  # Simulated amount of females who died
  (exctotal-F) / F
}

## Return the vector to par(mfrow=...) given the number of plot we want, suppose the canvas is an equilateral square.
lay = function (k) {
  xy = c(ceiling(sqrt(k)), ceiling(sqrt(k)))
  while ((xy[1]-1)*xy[2] >= k) xy[1] = xy[1]-1
  xy
}

##
## Histogram test.
## Return value: a vector of test results. "1" means ratio2compare is typical and "0" means it's too extreme.
##
## If popmfratio were provided then we normalise the ratio returned by sampfromexc() AFTER the call. Otherwise
## no population size normalisation is done.
## 
histtst = function (exc, mfratio, ratio2compare, alpha=0.05, xlim=c(0.5,1.9),
                    histplot=F, side=c("both","left","right"), pdfsize=35,
                    popmfratio = NULL, bonf.print = T) {
  if (is.character(histplot))  pdf(histplot, width=pdfsize, height=pdfsize)
  if (length(side)!=1)         stop("side= argument must be one of `both`,`left`,`right`")
  
  ## We've gotta remove all the NA and negative excess deaths. Bonferroni is
  ## done via dividing alpha with number of non-NA positive elements of the excess death vector.
  cntok   = sum((!is.na(exc)) & (exc>=0))
  cntfail = length(exc) - cntok
  
  if (is.character(histplot)) par(mfrow=lay(cntok))
  testres = rep(as.integer(NA), cntok)
  bonfres = rep(as.integer(NA), cntok)

  for (i in seq_along(exc)) {
    if ((!is.na(exc[i])) & (exc[i]>=0)) {
      S = sampfromexc(8000, round(exc[i]), mfratio)
      if (!is.null(popmfratio[i])) S = S/ popmfratio[i]
      if (side == "both") {
        quant      = quantile(S, c(alpha/2.0, 1-alpha/2.0))
        quant.bon  = quantile(S, c(alpha/cntok/2.0, 1-alpha/cntok/2.0))
        testres[i] = (quant[1] <= ratio2compare[i]) && (ratio2compare[i] <= quant[2])
        bonfres[i] = (quant.bon[1] <= ratio2compare[i]) && (ratio2compare[i] <= quant.bon[2])
        if (is.character(histplot)) {
          hist(S, main=sprintf("Week number=%s (%s)", names(exc)[i], ifelse(testres[i], "ACCEPT", "REJECT")),
               xlim = xlim)
          abline(v=ratio2compare[i], col=ifelse(testres[i], "blue", "red"))
          abline(v=quant[1], col="grey")
          abline(v=quant[2], col="grey")
        }
      } else if (side == "left") {
        quant      = quantile(S, alpha)
        quant.bon  = quantile(S, alpha/cntok/2.0)
        testres[i] = quant <= ratio2compare[i]
        bonfres[i] = quant.bon <= ratio2compare[i]
        if (is.character(histplot)) {
          hist(S, main=sprintf("Week number=%s (%s)", names(exc)[i], ifelse(testres[i], "ACCEPT", "REJECT")),
               xlim = xlim)
          abline(v=ratio2compare[i], col=ifelse(testres[i], "blue", "red"))
          abline(v=quant[1], col="grey")
        }
      } else if (side == "right") {
        quant      = quantile(S, 1-alpha)
        quant.bon  = quantile(S, 1-alpha/cntok/2.0)
        testres[i] = quant >= ratio2compare[i]
        bonfres[i] = quant.bon >= ratio2compare[i]
        if (is.character(histplot)) {
          hist(S, main=sprintf("Week number=%s (%s)", names(exc)[i], ifelse(testres[i], "ACCEPT", "REJECT")),
               xlim = xlim)
          abline(v=ratio2compare[i], col=ifelse(testres[i], "blue", "red"))
          abline(v=quant, col="grey")
        }
      }
    } else {
      cat(sprintf("WARNING: item %s skipped because of negativity.\n", names(exc)[i]))
    }
  }
  if (cntfail > 0) cat(sprintf("WARNING: %d weeks thrown away because of negativity\n", cntfail))
  if (is.character(histplot)) dev.off()
  if (bonf.print) cat(sprintf("Bonferroni: %s.\n", ifelse(all(bonfres == 1), "Not rejected", "Rejected")))
  testres
}

general_pearson_chi2_stat = function (x, y, m, n, theta0) {
  a = m+n
  b = -theta0*(m+y) - x - n
  c = theta0*(x+y)
  p1hat = (-b - sqrt(b^2 - 4*a*c))/(2*a)
  q1hat = 1 - p1hat
  U = ((x - m*p1hat)^2) / (m * p1hat * q1hat) * (1+ (m/n) * (theta0 - p1hat)/q1hat)
  U
}

general_pearson_chi2_test = function (x, y, m, n, theta0) {
  1 - pchisq(general_pearson_chi2_stat(x, y, m, n, theta0), df=1)
}


powerstudy_general_pearson_chi2_test = function (theta0=1, alpha=0.05) {
  lM=60; lN=60
  es = seq(from=1/3, to=3, length.out=50)
  cl = makeCluster(12, outfile="foo.txt")
  on.exit(stopCluster(cl))
  clusterExport(cl, "es", envir = environment())
  clusterExport(cl, "general_pearson_chi2_test")
  clusterExport(cl, "general_pearson_chi2_stat")
  clusterExport(cl, "lM", envir = environment())
  clusterExport(cl, "lN", envir = environment())
  parres = parLapply(cl, (1:lM)-1, function (l) {
    B = array(as.double(NA), dim = c(lN, length(es)))
    m = l*10 + 4
    for (k in 0:l) {
      n = k*10 + 4
      for (r in seq_along(es)) {
        true_theta=es[r]
        p = if (true_theta < 1) {
              p2 = 0.65
              p1 = p2 * true_theta
              c(p1= p1, p2= p2)
            } else {
              p1 = 0.65
              p2 = p1 / true_theta
              c(p1= p1, p2= p2)
            }
        x = rbinom(700, m, p=p[1])
        y = rbinom(700, n, p=p[2])
        B[k+1,r] = mean(na.omit(sapply(1:700, function (s) {
          ## print(c(true_theta, x[s], y[s], m, n))
          if ((x[s]==0 && y[s]==0) || (x[s]==m && y[s]==n)) {
            NA
          } else {
            pval = general_pearson_chi2_test(x[s],y[s],m,n,theta0)
            if (pval < 0.05) 1 else 0
          }
        })))
      }
    }
    B
  })
  A = array(as.double(NA), dim = c(lM, lN, length(es)))
  for (i in 1:lM) {
    A[i,,] = parres[[i]]
  }
  A
}
## powr = powerstudy_general_pearson_chi2_test()
## plot(powr[,,17],
##      main=paste("theta=",seq(from=1/3, to=3, length.out=50)[17]),
##      fmt.cell = "%.02f",
##      cex=0.88,
##      breaks=seq(0.05, 1, by=0.05),
##      na.cell=FALSE)

## ## X-axis = sample size diagonal, Y-axis = p, colour = power
## xs= dim(powr)[1];   ps= dim(powr)[3]
## zs= array(0, dim=c(xs,ps))
## for (j in 1:ps)
##   for (i in 1:xs)
##     zs[i,j] = powr[i,i,j]

## filled.contour(x=((1:xs)-1)*10+4,
##                y=seq(from=1/3, to=3, length.out=50)[1:ps],
##                z=zs,
##                color.palette = function(n) hcl.colors(n, "YlOrRd", rev = F),
##                levels=c(0, 0.6, 0.7, seq(0.75,1,by=0.05)),
##                xlab = "Sample size (m=n)",
##                ylab = "theta")
## ## Zoom in
## filled.contour(x=((15:30)-1)*10+4,
##                y=seq(from=1/3, to=3, length.out=50)[16:23],
##                z=zs[15:30,16:23],
##                color.palette = function(n) hcl.colors(n, "YlOrRd", rev = F),
##                levels=c(0, 0.6, 0.7, seq(0.75,1,by=0.05)),
##                xlab = "Sample size (m=n)",
##                ylab = "theta")

powerstudy_histtest = function () {
  lM=100
  es = seq(from=1/3, to=3, length.out=50)
  cl = makeCluster(12, outfile="foo.txt")
  on.exit(stopCluster(cl))
  clusterExport(cl, "es", envir = environment())
  clusterExport(cl, "histtst")
  clusterExport(cl, "lay")
  clusterExport(cl, "sampfromexc")
  clusterExport(cl, "lM", envir = environment())

  parres = parLapply(cl, (1:lM)-1, function (l) {
    B = array(as.double(NA), dim = length(es))
    m = l*10 + 4
    n = m
    for (r in seq_along(es)) {
      true_theta=es[r]
      p = if (true_theta < 1) {
            p2 = 0.65
            p1 = p2 * true_theta
            c(p1= p1, p2= p2)
          } else {
            p1 = 0.65
            p2 = p1 / true_theta
            c(p1= p1, p2= p2)
          }
      x = rbinom(500, m, p=p[1])
      y = rbinom(500, n, p=p[2])
      B[r] = mean(na.omit(sapply(1:500, function (s) {
        # print(c(true_theta, x[s], y[s], m, n))
        if ((x[s]==0 && y[s]==0) || (x[s]==m && y[s]==n)) {
          NA
        } else {
          res = histtst(n, x[s]/(m-x[s]), y[s]/(n-y[s]), histplot=F, bonf.print=F, side="both")
          if (res == 0) 1 else 0
        }
      })))
    }
    B
  })
  A = array(as.double(NA), dim = c(lM, length(es)))
  for (i in 1:lM) {
    A[i,] = parres[[i]]
  }
  A
}
## powr.histtst = powerstudy_histtest()

## xs= dim(powr.histtst)[1];   ps= dim(powr.histtst)[2]
## filled.contour(x = (1:xs)*10+4,
##                y = seq(from=1/3, to=3, length.out=50)[1:30],
##                z = powr.histtst[,1:30],
##                color.palette = function(n) hcl.colors(n, "YlOrRd", rev = F),
##                levels=c(0, 0.6, 0.7, seq(0.75,1,by=0.05)),
##                xlab = "Sample size (m=n)",
##                ylab = "theta")

## plot(powr[,,17],
##      main=paste("theta=",seq(from=1/3, to=3, length.out=50)[17]),
##      fmt.cell = "%.02f",
##      cex=0.88,
##      breaks=seq(0.05, 1, by=0.05),
##      na.cell=FALSE)
## ## X-axis = sample size diagonal, Y-axis = p, colour = power
## xs= dim(powr.histtst)[1];   ps= dim(powr.histtst)[3]
## zs= array(0, dim=c(xs,ps))
## for (j in 1:ps)
##   for (i in 1:xs)
##     zs[i,j] = powr.histtst[i,i,j]

## filled.contour(x=((1:xs)-1)*10+4,
##                y=seq(from=1/3, to=3, length.out=50)[1:ps],
##                z=zs,
##                color.palette = function(n) hcl.colors(n, "YlOrRd", rev = F),
##                levels=c(0, 0.6, 0.7, seq(0.75,1,by=0.05)),
##                xlab = "Sample size (m=n)",
##                ylab = "theta")
## ## Zoom in
## filled.contour(x=((15:30)-1)*10+4,
##                y=seq(from=1/3, to=3, length.out=50)[16:23],
##                z=zs[15:30,16:23],
##                color.palette = function(n) hcl.colors(n, "YlOrRd", rev = F),
##                levels=c(0, 0.6, 0.7, seq(0.75,1,by=0.05)),
##                xlab = "Sample size (m=n)",
##                ylab = "theta")


analytical_test = function (x, y, m, n, rG, alpha) {
  kappa = qnorm(1 - alpha/2, 0, 1)
  ytild = y + (kappa^2)/2
  ntild = n + (kappa^2)
  ptild = ytild/ntild
  qtild = 1 - ptild
  pA_interval = ptild + c(-1,1) * kappa * sqrt(ptild*qtild/ntild)
  psiA_interval = pA_interval / (1 - pA_interval)
  pG_interval = rG * psiA_interval / (1 + psiA_interval)
  x_interval = c(qbinom(alpha/2, m, pG_interval[1]), qbinom(1-alpha/2, m, pG_interval[2]))
  c(X=x, LOW = x_interval[1], HIGH = x_interval[2], REJECT = !(x_interval[1] < x && x < x_interval[2]))
}

powerstudy_analytical_test = function (alpha=0.05) {
  lM=80; lN=80
  es = seq(from=1/3, to=3, length.out=50)
  cl = makeCluster(12, outfile="foo.txt")
  on.exit(stopCluster(cl))
  clusterExport(cl, "es", envir = environment())
  clusterExport(cl, "analytical_test")
  clusterExport(cl, "lM", envir = environment())
  clusterExport(cl, "lN", envir = environment())
  parres = parLapply(cl, (1:lM)-1, function (l) {
    B = array(as.double(NA), dim = c(lN, length(es)))
    m = l*10 + 4
    for (k in 0:l) {
      n = k*10 + 4
      for (r in seq_along(es)) {
        true_theta=es[r]
        p = if (true_theta < 1) {
              p2 = 0.65
              p1 = p2 * true_theta
              c(p1= p1, p2= p2)
            } else {
              p1 = 0.65
              p2 = p1 / true_theta
              c(p1= p1, p2= p2)
            }
        x = rbinom(700, m, p=p[1])
        y = rbinom(700, n, p=p[2])
        B[k+1,r] = mean(na.omit(sapply(1:700, function (s) {
          ## print(c(true_theta, x[s], y[s], m, n))
          if ((x[s]==0 && y[s]==0) || (x[s]==m && y[s]==n)) {
            NA
          } else {
            pval = analytical_test(x[s],y[s],m,n,1.4, 0.05)
            if (pval < 0.05) 1 else 0
          }
        })))
      }
    }
    B
  })
  A = array(as.double(NA), dim = c(lM, lN, length(es)))
  for (i in 1:lM) {
    A[i,,] = parres[[i]]
  }
  A
}
## powr.ana = powerstudy_analytical_test()

