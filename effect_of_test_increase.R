# Simulation to observe the effect of increasing number of tests 
# on the Q-value of real signal

.qqplot <- function(P) {
    x <- runif(length(P))
    qqplot(x,P,cex=0.3)
    abline(0,1,col='red')
}

N <- 1600  # num tests
n <- 200   # num true signals
p1 <- rexp(n, rate=100) # signal

p2 <- runif(N-n) # noise
P <- c(p1,p2); qv1 <- p.adjust(P,method="BH")

par(mfrow=c(1,2))
.qqplot(P)
title("Num tests = 1600; signal = 200")

# now double the number of tests
N <- 1600 * 2
p2 <- runif(N-n)
P <- c(p1,p2); qv2 <- p.adjust(P,method="BH")
.qqplot(P)
title("Num tests = 3200 ; signal = 200")

# compare Q value for same loci before and after increasing num tests
plot(qv1[1:n],qv2[1:n], xlab="Q - N=1600",ylab="Q - N=3200", cex=0.5,
     bty='n',xlim=c(0,1),ylim=c(0,1))
abline(0,1,col='red')
title("Q value for same loci with N=1600 vs N=3200 tests",cex.main=0.7)
 
x <- summary(lm(qv2[1:n]~qv1[1:n]))$coef[2,1]
cat(sprintf("Slope of Q for N=3200 vs N=1600: %1.2f", x))
cat("")
