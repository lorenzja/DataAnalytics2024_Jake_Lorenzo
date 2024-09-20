#Lab 1 Exercise 1

EPI_data <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Lab 1/epi2024results06022024.csv")

View(EPI_data)

attach(EPI_data)

EPI.new

tf <- is.na(EPI.new)
E <- EPI.new[!tf]

summary(EPI.new)
fivenum(EPI.new,na.rm=TRUE)
stem(EPI.new)
hist(EPI.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw=1.))
rug(EPI.new)
boxplot(EPI.new, APO.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw=1.)) 
rug(EPI.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw="SJ"))
rug(EPI.new)
x<-seq(20,80,1)
q<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
ln<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q)

#Exercise 2
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE)
qqnorm(EPI.new); qqline(EPI.new) 
qqplot(rt(ppoints(250), df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)

#Exercise 2a
#APO
APO.new <- EPI_data$APO.new
tf_APO <- is.na(APO.new)
APO_cleaned_data <- APO.new[!tf_APO]

summary(APO.new)
fivenum(APO.new, na.rm = TRUE)

stem(APO.new)
hist(APO.new, prob = TRUE)
lines(density(APO.new, na.rm = TRUE, bw = 1))
rug(APO.new)

boxplot(APO.new, EPI.new)
min_max_APO <- seq(min(APO.new, na.rm=TRUE), max(APO.new, na.rm=TRUE), 1)
density_APO <- dnorm(min_max_APO, mean = mean(APO.new, na.rm=TRUE), sd = sd(APO.new, na.rm=TRUE))
lines(min_max_APO, density_APO)

plot(ecdf(APO.new), do.points=FALSE, verticals=TRUE)
qqnorm(APO.new); qqline(APO.new)
qqplot(rt(ppoints(250), df = 5), APO.new, xlab = "Q-Q plot for t distribution")
qqline(APO.new)

#WRS
WRS.new <- EPI_data$WRS.new
tf_WRS <- is.na(WRS.new)
WRS_cleaned_data <- WRS.new[!tf_WRS]
summary(WRS.new)
fivenum(WRS.new, na.rm = TRUE)

stem(WRS.new)

hist(WRS.new, prob = TRUE)
lines(density(WRS.new, na.rm = TRUE, bw = 1))
rug(WRS.new)

boxplot(WRS.new, EPI.new)

min_max_WRS <- seq(min(WRS.new, na.rm=TRUE), max(WRS.new, na.rm=TRUE), 1)
density_WRS <- dnorm(min_max_WRS, mean = mean(WRS.new, na.rm=TRUE), sd = sd(WRS.new, na.rm=TRUE))
lines(min_max_WRS, density_WRS)

plot(ecdf(WRS.new), do.points=FALSE, verticals=TRUE)
qqnorm(WRS.new); qqline(WRS.new)
qqplot(rt(ppoints(250), df = 5), WRS.new, xlab = "Q-Q plot for t distribution")
qqline(WRS.new)




