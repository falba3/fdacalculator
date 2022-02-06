

# MY LAB Exercises


# find rejection region (t value)
qt(.01, 7, lower.tail = TRUE)


# calculate the test statistic (mean / (s/√n)) (one n)
dif <- 2.5
n <- 8
s <- 3.4226
se <- s/(n^(1/2))

teststat <- dif / se


# confidence interval 99% (t student) (TWO TAILED BECAUSE CI, a/2)
a <- .005
t <- qt(a, (n-1))
error <- t*se

LL <- dif - error
UL <- dif + error
paste("LL:", LL, "and", "UL:", UL)



# confidence interval 99% (norm) (TWO TAILED BECAUSE CI, a/2)
conf <- .99
a <- (1-conf)/2 
z <- qnorm(a)
error <- abs(z)*se

LL <- dif - error
UL <- dif + error
paste("LL:", LL, "and", "UL:", UL)





# calculate the test statistic (2 sets)
x1 <- 34.7
x2 <- 30.9
dif <- x1 - x2

n1 <- 72
n2 <- 72
s1 <- 7.2
s2 <- 6.6

deno <- sqrt((s1^2/n1) + (s2^2/n2))


teststat <- dif / deno
2*(pt(tstat, (n1 + n2 -2), lower.tail = F))

pnorm(teststat, lower.tail = F)

qt(.01, 7, lower.tail = F)



# getting v = df
num <- (((s1^2/n1) + (s2^2/n2)))^2
den <- ((((s1^2/n1)^2)/(n1-1)) + (((s2^2/n2)^2)/(n2-1)))

v <- num / deno





# PROPORTIONS

# Confidence Interval
p1 <- .44
p2 <- .59

n1 <- 100
n2 <- 120

dif <- p1 - p2
se <- sqrt((((p1*(1-p1))/n1)+(p2*(1-p2))/n2))

conf <- .90
z <- qnorm(abs((1-conf)/2))

LL <- dif - (z*(se))
UL <- dif + (z*(se))


if (LL > UL) {
   temp <- LL
   LL <- UL
   UL <- temp
}

paste("LL:", LL, "UL:", UL)



# Proportion Hypothesis testing (norm)
p1 <- 27
p2 <- 39

n1 <- 174
n2 <- 139

p1 <- p1/n1
p2 <- p2/n2


# Proportion test statistic (norm)
p1 
p2

n1
n2

pz

qnorm(pz)

qnorm(.025)

1 - (pf(1.98, 24, 24))
df(2.46, 6, 27)

pnorm(-3.39)

qf()




# F DISTRIBUTION
qf(.025,6, 73)






# ANOVA
anova()


