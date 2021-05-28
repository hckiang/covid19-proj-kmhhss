par(mfrow = c(2,2))
plot(x=mfexc.1$value.death.f.mean,
     y=mfexc.1$var.death.f,
     xlim=c(0,100), ylim = c(0,100), main='Female death, all regions+weeks, Mean of (2014~2019) vs. Var.',
     xlab = 'xlim = ylim = c(0,100)', ylab = '')
plot(x=mfexc.1$value.death.f.mean,
     y=mfexc.1$var.death.f,
     xlim=c(0,900), ylim = c(0,900), main='Female death, all regions+weeks, Mean of (2014~2019) vs. Var.',
     xlab = 'xlim = ylim = c(0,900)', ylab = '')

SWEDMASK = which(mfexc.1$geo.time %in% paste0("SE",1:9))
plot(x=mfexc.1$value.death.f.mean[SWEDMASK],
     y=mfexc.1$var.death.f[SWEDMASK],
     xlim=c(0,100), ylim = c(0,100), main='Female death, Swedish regions+weeks, Mean of (2014~2019) vs. Var.',
     xlab = 'xlim = ylim = c(0,100)', ylab = '')
plot(x=mfexc.1$value.death.f.mean[SWEDMASK],
     y=mfexc.1$var.death.f[SWEDMASK],
     xlim=c(0,900), ylim = c(0,900), main='Female death, Swedish regions+weeks, Mean of (2014~2019) vs. Var.',
     xlab = 'xlim = ylim = c(0,900)', ylab = '')
