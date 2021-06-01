suppressPackageStartupMessages({
  library(ggplot2)
})
check_iid = function (n, pA, rG) {
  qA = 1-pA
  k = rG*pA/qA
  pG = k/(1+k)
  qG = 1 - pG

  XG = rbinom(300000, n, pG)
  XA = rbinom(300000, n, pA)
  cat("Ratio means:\n")
  print(c(G = mean(XG/(n-XG)), A = mean(XA*rG/(n-XA))))
  cat("Ratio quantiles")
  print(c(G = quantile(XG/(n-XG),0.975), A = quantile(XA*rG/(n-XA), 0.975)))
  DG = data.frame(X = XG/(n-XG));   DA = data.frame(X = XA*rG/(n-XA))
  DG$typ = "G";                     DA$typ = "A"
  D = rbind(DG, DA)
  ggplot(D, aes(X, fill = typ)) + geom_density(alpha=0.2)
}
