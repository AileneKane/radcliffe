#convex hulls on scatterplot matrix by factor

v1 <- seq(1:100)
v2 <- 3*v1 + rnorm(100,0,10)
v3 <- rnorm(100,0,10)

lev <- factor(rep(c("A", "B"), 50))

dum <- data.frame(lev, v1, v2, v3)

pairs(dum)

library(ggplot2)
library(GGally)

ggpairs(
	dum[,1:4],
	mapping = ggplot2::aes(color = lev)
)

