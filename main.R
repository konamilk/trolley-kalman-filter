# Title     : TODO
# Objective : TODO
# Created by: pinana
# Created on: 2021/05/01

set.seed(5534)

# EXP1
# TROLLEY <- read.csv("Car_202111011131135.csv");

# EXP2
TROLLEY <- read.csv("Car_202108023150827.csv");

TROLLEY$v <- TROLLEY$v_true + rnorm(nrow(TROLLEY), 0, 1)

par(mfrow=c(3,1))
plot(x=TROLLEY$t, y=TROLLEY$f, xlab = "t [s]",ylab = "f [N]", xaxs = "i", yaxs = "i", tcl=0.3, xlim=c(0,30),ylim=c(-10,30), type = "l")
plot(x=TROLLEY$t, y=TROLLEY$v_true, xlab = "t [s]",ylab = "v_true [m/s]", xaxs = "i", yaxs = "i", tcl=0.3, xlim=c(0,30),ylim=c(0,40), type = "l")
plot(x=TROLLEY$t, y=TROLLEY$v, xlab = "t [s]",ylab = "v [m/s]", xaxs = "i", yaxs = "i", tcl=0.3, xlim=c(0,30),ylim=c(0,40), type = "l")


library(dlm)
mod <- dlmModPoly(order=3)
# str(mod)

mod$GG[1,2] <- 0.02
mod$GG[2,1] <- 0
mod$GG[2,3] <- 0

mod$X <- matrix(TROLLEY$f * 0.02, nrow = nrow(TROLLEY), ncol = 1)
mod$JGG <- matrix(c(0,0,0,0,0,0,1,0,0),nrow=3, ncol=3)

build_dlm <- function(par) {
  mod$W[1,1] <- exp(par[1])
  mod$W[2,2] <- exp(par[2])
  mod$W[3,3] <- exp(par[3])
  mod$V[1,1] <- exp(par[4])
  mod$GG[1,1] <- 1 - 0.02 * exp(par[5])

  return(mod)
}

# ハイパーパラメータを適当にな初期値を与えて同じところに収束することを確認
# lapply(list(c(0,0,0,0,1), c(10,1,1,1,0),c(1,10,10,1,-1),c(10,1,1,5,-3)), function(params){
#   dlmMLE(y = TROLLEY$v, parm = params, build = build_dlm)
# })

fit_dlm <- dlmMLE(y = TROLLEY$v, parm = c(0,0,0,0,-1), build = build_dlm, hessian = TRUE)
mod <- build_dlm(fit_dlm$par)

mod

dlmFiltered_obj <- dlmFilter(y= TROLLEY$v, mod=mod)
str(dlmFiltered_obj, max.level = 1)

# 平均値
m <- dropFirst(dlmFiltered_obj$m)
m_dev <- dropFirst(
  dlmSvd2var(dlmFiltered_obj$U.C, dlmFiltered_obj$D.C)
)

par(mfrow=c(3,1))
# v vs t
plot(x=TROLLEY$t, y=TROLLEY$v, xlab = "t [s]",ylab = "v [m/s]", xaxs = "i", yaxs = "i", tcl=0.3, xlim=c(0,30), ylim=c(0,40), col=8, type = "l")
par(new = T)
plot(x=TROLLEY$t, y=m[,1], xaxs = "i", yaxs = "i",tcl=0.3, xlim=c(0,30), ylim=c(0,40), type = "l", lwd=3, col=2, ann=F)
# g vs t
plot(x=TROLLEY$t, y=m[,2], xlab = "t [s]",ylab = "g [m/s^2]", xaxs = "i", yaxs = "i", xlim=c(0,30), ylim=c(-10,20), type = "l")
# m vs t
plot(x=TROLLEY$t, y=m[,3], xlab = "t [s]",ylab = "m [kg]", xaxs = "i", yaxs = "i", xlim=c(0,30), ylim=c(0,1), type = "l")
