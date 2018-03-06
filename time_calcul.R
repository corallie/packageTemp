rm(list=ls())
library(microbenchmark)
library(packageTemp)

#====== Sys. time ====
system.time(mvnpdf(x=matrix(rep(1.96, 2), nrow=2, ncol=1), Log=FALSE))
system.time(mvtnorm::dmvnorm(rep(1.96, 2)))
#===== Test 1 ===== =====
Test 1

n <- 100
mb <- microbenchmark(mvtnorm::dmvnorm(rep(1.96, 2)),
                     mvnpdf(x=matrix(rep(1.96,2)), Log=FALSE),
                     times=1000L)
mb
mb <- microbenchmark(mvtnorm::dmvnorm(matrix(1.96, nrow = n, ncol = 2)),
                     mvnpdf(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
                     times=100L)
mb$time

ggplot2::ggplot(mb, aes(mb$expr, mb$time)) + geom_point() +  geom_boxplot()

#===== Test 2 =====

n <- 10e4

pdfval <- mvnpdf(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE)

pdfval <- mvnpdfsmart(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE)

n <- 1000

mb <- microbenchmark(mvtnorm::dmvnorm(matrix(1.96, nrow = n, ncol = 2)),
                     mvnpdf(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
                     mvnpdfsmart(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
                     times=100L)

ggplot2::ggplot(mb, aes(mb$expr, mb$time)) + geom_point() +  geom_boxplot()
# ggplot2::ggplot(mb, aes(mb$expr, mb$time)) + geom_point() +  geom_density_2d(geom = "point", aes(size = mb$time), n = 20, contour = FALSE)

#===== Test 3 =====
pdfval <- mvnpdf(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE)

pdfval <- mvnpdfsmart(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE)

pdfval <- mvnpdfoptim(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE)
n <- 1000

mb <- microbenchmark(mvtnorm::dmvnorm(matrix(1.96, nrow = n, ncol = 2)),
                     mvnpdf(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
                     mvnpdfsmart(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
                     mvnpdfoptim(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
                     times=100L)

ggplot2::ggplot(mb, aes(mb$expr, mb$time)) + geom_point() +  geom_boxplot()

