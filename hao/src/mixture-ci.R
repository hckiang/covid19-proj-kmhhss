suppressPackageStartupMessages({
  library(lattice)
  library(latticeExtra)
  library(RColorBrewer)
  library(parallel)
})
Lstat = Vectorize(function (p,alpha,nc=100,ns=130,nsamp=400000) {
  yc = rbinom(n=nsamp, size=nc, p=p)
  ys = rbinom(n=nsamp, size=ns, p=p)
  Tall = (yc+ys)/(nc+ns)
  logTall = log(yc+ys) - log(nc+ns)
  Tc = yc/nc
  logTc = log(yc)-log(nc)
  Ts = ys/ns
  logTs = log(ys)-log(ns)
  logL = yc * (logTc - logTall) + ys*(logTs - logTall) +
    (nc-yc)*(log(1-Tc) - log(1-Tall)) +
    (ns-ys)*(log(1-Ts) - log(1-Tall))
  k = quantile(logL, probs=alpha,na.rm=T)
  names(k) = NULL
  k
}, 'p')

# xs = seq(0.08, 0.92, length.out=200)
# Q = Lstat(xs,0.95)
# crit = max(Q)
# plot(y=Q, x=xs, type='l', xlab = expression(p_C), ylab='95th quantile of lik. ratio stat.')

pwrfn = (function (pn, pc, alpha, c, nc=100, ns=130, nsamp=100000) {
  yc = rbinom(n=nsamp, size=nc, p=pc)
  ys = rbinom(n=nsamp, size=ns, p=alpha*pn + (1-alpha)*pc)
  Tall = (yc+ys)/(nc+ns)
  logTall = log(yc+ys) - log(nc+ns)
  Tc = yc/nc
  logTc = log(yc)-log(nc)
  Ts = ys/ns
  logTs = log(ys)-log(ns)
  logL = yc * (logTc - logTall) + ys*(logTs - logTall) +
        (nc-yc)*(log(1-Tc) - log(1-Tall)) +
        (ns-ys)*(log(1-Ts) - log(1-Tall))
  sum(logL>c)/nsamp
})
cmapply <- function(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE,
                    USE.NAMES = TRUE)
{
  l <- expand.grid(..., stringsAsFactors=FALSE)
  r <- do.call(mapply, c(
    list(FUN=FUN, MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES),
    l
  ))
  if (is.matrix(r)) r <- t(r)
  cbind(l, r)
}

##pwr = cmapply(pn=seq(0.15, 0.85, by=0.05), pc=seq(0.15,0.85,by=0.04), alpha=seq(-0.5,1.5,by=0.1),
##         FUN= function (pn, pc, alpha) {
##            print(c(pn,pc,alpha))
##            pwrfn(pn, pc, alpha, c=1.98)
##          })
##colnames(pwr) = c('pN', 'pC', 'alpha', 'pwr')

# my.settings <- canonical.theme(color=FALSE)
# my.settings[['strip.background']]$col <- 'white'
# my.settings[['strip.border']]$col<- 'black'
# levelplot(pwr~pC*alpha|pN, data=pwr, panel = panel.2dsmoother, col.regions = colorRampPalette(brewer.pal(11,'Spectral'),2),
#           par.settings = my.settings,
#           strip = strip.custom(strip.levels = c(TRUE, TRUE)),
#           par.strip.text=list(col='black'))
#
#
#
# xs2 = seq(0.08, 0.92, length.out=200)
# Q2 = Lstat(xs2,0.95,nc=400,ns=600,nsamp=400000)
# crit2 = max(Q2)
# plot(y=Q2, x=xs2, type='l', xlab = expression(p_C), ylab='95th quantile of lik. ratio stat.')
# pwr2 = cmapply(pn=seq(0.15, 0.85, by=0.05), pc=seq(0.15,0.85,by=0.04), alpha=seq(-0.5,1.5,by=0.1),
#               FUN= function (pn, pc, alpha) {
#                 print(c(pn,pc,alpha))
#                 pwrfn(pn, pc, alpha, c=1.96, nc=400, ns=600)
#               })
# colnames(pwr2) = c('pN', 'pC', 'alpha', 'pwr')
# levelplot(pwr~pC*alpha|pN, data=pwr2, panel = panel.2dsmoother, col.regions = colorRampPalette(brewer.pal(11,'Spectral'),2),
#           par.settings = my.settings,
#           strip = strip.custom(strip.levels = c(TRUE, TRUE)),
#           par.strip.text=list(col='black'))

loglik_mainpart = function (pn,pc,a,yn,yc,ys,nn,nc,ns) {
  yn*log(pn) + (nn-yn)*log(1-pn) + yc*log(pc) + (nc-yc)*log(1-pc) + ys*log(a*pn+(1-a)*pc) +
      (ns-ys)*log(1-a*pn-(1-a)*pc)
}
mle_alpharestricted = function (a0,yn,yc,ys,nn,nc,ns) {
  obj = function (p) - loglik_mainpart(p[1],p[2],a0,yn,yc,ys,nn,nc,ns)
  res = simpleError('Encountered infinite value?')
  counter <- 1
  max_tries <- 1000
  while(inherits(res, 'error') & counter < max_tries) {
    res <- tryCatch({ optim(runif(2,min=0.001,max=0.999),
                            obj,
                            method='L-BFGS-B',
                            lower=c(0.001,0.001),upper=c(0.999,0.999)) },
                    error = function(e) e)
    counter <- counter + 1
  }
  if (inherits(res, 'error'))
    stop('Cannot optimise alpha-restricted likelihood')
  else
    res
}

logL_alpharestricted = function (a0,yn,yc,ys,nn,nc,ns) {
  mle_H0_optobj = tryCatch({
    mle_alpharestricted(a0,yn,yc,ys,nn,nc,ns)
  }, error = function (cond) { NULL })
  if (is.null(mle_H0_optobj)) return(NA)
  if (mle_H0_optobj[['convergence']] != 0) return(NA)
  mle    = loglik_mainpart(yn/nn, yc/nc, (nn*(ns*yc - nc*ys))/(ns*(nn*yc - nc*yn)), yn,yc,ys,nn,nc,ns)
  mle + mle_H0_optobj$value
}

critval_logL_alpharestricted = function (a0,nn,nc,ns, nsamp=3000) {
  ## Estimate of distribution of logL_alpharestricted under different values of the null hypothesis.
  ## For all possible pN and pC, simulate logL_alpharestricted and take quantiles.
  cmapply(pn=seq(0.08, 0.92, by=0.05), pc=seq(0.08, 0.92,by=0.04),
          FUN= function (pn, pc) {
            print(c(pn,pc))
            yn = rbinom(n=nsamp, size=nn, p=pn)
            yc = rbinom(n=nsamp, size=nc, p=pc)
            ys = rbinom(n=nsamp, size=ns, p=a0*pn+(1-a0)*pc)
            S = mapply(function (yn,yc,ys) {
              tryCatch({
                logL_alpharestricted(a0,yn,yc,ys,nn,nc,ns)
              }, error=function (cond) NA)
            },yn,yc,ys)
            quantile(S, prob = 0.95, na.rm=T)
          })
}

# nntest = 600;  yntest = 300
# nctest = 600;  yctest = 550
# nstest = 200;  ystest = 170

#levelplot(r~pn*pc, data=ct)


## Given an observed Y and n, compute an approximate
## confidence interval from the data.
alpha_ci = function (lvl, yn,yc,ys,nn,nc,ns, fineness=1000) {
  a0_candidates = seq(-0.3, 1.5, length.out = fineness)
  critval = qchisq(lvl, df=1)/2
  rejected = integer(fineness)
  for (i in seq_along(a0_candidates)) {
    LL = logL_alpharestricted(a0_candidates[i], yn,yc,ys,nn,nc,ns)
    rejected[i] = as.integer(LL > critval)
  }
  whichzero = which(rejected==0)
  lowerbidx = whichzero[1]
  upperbidx = whichzero[length(whichzero)]
  c(lower = a0_candidates[lowerbidx],
    upper = a0_candidates[upperbidx])
}
# rej = alpha_ci(0.95, yntest,yctest,ystest,nntest,nctest,nstest)

## Find coverage probability
covg_prob = function (lvl,pn,pc,a,nn,nc,ns, nsamp=600) {
  yn = rbinom(n=nsamp, size=nn, p=pn)
  yc = rbinom(n=nsamp, size=nc, p=pc)
  ys = rbinom(n=nsamp, size=ns, p=a*pn+(1-a)*pc)
  edgecases = (yn == 0) | (yn == nn) | (yc == 0) | (yc == nc) | (yn/nn == yc/nc)
  yn = yn[!edgecases]
  yc = yc[!edgecases]
  ys = ys[!edgecases]
  len_actual = sum(!edgecases)
  i = 0
  covg = mcmapply(function (yn,yc,ys) {
    print(c(finished=i, total=nsamp))
    i <<- i+1
    ci = alpha_ci(lvl, yn,yc,ys,nn,nc,ns)
    (a > ci[1]) || (a < ci[2])
  },yn,yc,ys, mc.cores=6)
  sum(covg,na.rm=T)/len_actual
}
# covg_prob(0.95, 0.55,0.85,0.9,nntest,nctest,nstest)
# cprob_all = sapply(seq(0,1,length.out=20), function (alpha) {
#   print(alpha)
#   covg_prob(0.95, 0.55,0.85,alpha,nntest,nctest,nstest)
# })
