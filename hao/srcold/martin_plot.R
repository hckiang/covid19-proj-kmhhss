
library(ggplot2)
library(dplyr)

# ratio distributions
ratiod <- sapply(res, function(i) i$ratio_distr)
ratiod.reg <- cbind(
  sapply(res.1, function(i) i$ratio_distr),
  sapply(res.2, function(i) i$ratio_distr),
  sapply(res.3, function(i) i$ratio_distr)
)

# construct p-value dataframe
create.pvals <- function(res, region) data.frame(
    region = region,
    age    = sapply(res, function(i) i$age),
    week   = sapply(res, function(i) as.numeric(i$wk)),
    pi     = sapply(res, pval)
  )

# construct data frames
pis <- create.pvals(se_bayes("SE", mfexc, mfexc), "SE")
pis.reg <- rbind(
  create.pvals(se_bayes("SE1", mfexc, mfexc.1), "SE1"),
  create.pvals(se_bayes("SE2", mfexc, mfexc.1), "SE2"),
  create.pvals(se_bayes("SE3", mfexc, mfexc.1), "SE3")
)

# pi values
pis.weeks <- pis %>%
  filter(age == "75_79")

# filter outliers out (IQR method)
q1 <- apply(ratiod, 2, function(v) quantile(v)[2])
q3 <- apply(ratiod, 2, function(v) quantile(v)[4])
thres1 <- q1 - 1.5 * (q3 - q1)
thres2 <- q3 + 1.5 * (q3 - q1)
# indices
idx <- which(pis$age == "75_79")
df <- data.frame()
for(i in idx) {
  data <- data.frame(ratiod = ratiod[,i]) %>%
    dplyr::filter(ratiod >= thres1[i] & ratiod <= thres2[i]) %>%
    as.matrix()
  df <- rbind(df, data.frame(week = pis[i,"week"], ratiod = data))
}

wk <- unique(df$week)[5]
df %>%
  dplyr::filter(week %in% wk) %>%
  ggplot(aes(x = ratiod)) +
  geom_histogram(binwidth = .05, alpha = .7) +
  #geom_vline(xintercept = (pis.weeks %>% filter(week %in% wk))$pi) +
  facet_grid(vars(week))
