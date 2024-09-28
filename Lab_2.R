#Lab 2 Exercise 1 

#Reading in the data
EPI_data <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Lab 2/epi2024results06022024.csv")


#Viewing the data
View(EPI_data)


#Attaching the column titles for callback
attach(EPI_data)

EPI_weights <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Lab 2/epi2024weights.csv")

View(EPI_weights)

attach(EPI_weights)

#Calling the "EPI.new" Column
EPI.new

#Cumulative Density Function
plot(ecdf(EPI.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(EPI.new))

#Q-Q Plot Against the Generating Distribution
x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)),EPI.new)
qqline(EPI.new)


#Calling the "Air.new" Column
AIR.new

tf <- is.na(AIR.new)
E <- AIR.new[!tf]

summary(AIR.new)
fivenum(AIR.new,na.rm=TRUE)
stem(AIR.new)
hist(AIR.new)


hist(AIR.new, xlim = c(0, 100), ylim = c(0.000, 0.030), prob = TRUE)
lines(density(AIR.new,na.rm=TRUE,bw=1.))
rug(AIR.new)
boxplot(AIR.new, AIR.old) 

AIR_mean <- mean(AIR.new)

plot(ecdf(rnorm(1000, AIR_mean, 10)), do.points=FALSE)
lines(ecdf(AIR.new))

plot(ecdf(AIR.old), do.points=FALSE, main="AIR.old vs. AIR.new ECDF")
lines(ecdf(AIR.new))

qqnorm(AIR.new); qqline(AIR.new) 
qqplot(rt(ppoints(250), df = 5), AIR.new, xlab = "Q-Q plot for t dsn") 
qqline(AIR.new)

#Calling the "WRS.new" Column
WRS.new

WRS_tf <- is.na(WRS.new)
WRS_E <- WRS.new[!tf]

summary(WRS.new)
fivenum(WRS.new,na.rm=TRUE)
stem(WRS.new)
hist(WRS.new)

x <- seq(0, 110, 5)
xl <- c(0, 120)

y <- seq(0.000, 0.030, 0.010)
yl <- c(0, 0.030)

hist(WRS.new, x, xlim = xl, ylim = yl, prob = TRUE)

lines(density(WRS.new,na.rm=TRUE,bw=1.))
rug(WRS.new)
boxplot(WRS.new, WRS.old) 

WRS_mean <- mean(WRS.new)

plot(ecdf(rnorm(1000, WRS_mean, 10)), do.points=FALSE)
lines(ecdf(WRS.new))

plot(ecdf(WRS.old), do.points=FALSE, main="WRS.old vs. WRS.new ECDF")
lines(ecdf(WRS.new))

qqnorm(WRS.new); qqline(WRS.new) 
qqplot(rt(ppoints(250), df = 5), WRS.new, xlab = "Q-Q plot for t dsn") 
qqline(WRS.new)

#########Boxplot Comparing Three Variables

EPI.new <- EPI_data$EPI.new
AIR.new <- EPI_data$AIR.new
WRS.new <- EPI_data$AIR.old

boxplot(EPI.new, AIR.new, WRS.new,
        names = c("EPI.new", "AIR.new", "WRS.new"),
        main = "Comparison of EPI.new, AIR.new, and WRS.new",
        ylab = "Data Points",
        col = c("blue", "green", "pink"))
        

#####Q-Q Plot Comparing Three Variables

install.packages("ggplot2")
install.packages("tidyr")

library(ggplot2)
library(tidyr)

data_comparison <- data.frame(
  EPI.new = EPI_data$EPI.new,
  AIR.new = EPI_data$AIR.new,
  WRS.new = EPI_data$WRS.new
)

comparison_long <- pivot_longer(data_comparison, cols = everything(), 
                                names_to = "Variable", values_to = "Value")

ggplot(comparison_long, aes(sample = Value)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Q-Q Plots for EPI.new, AIR.new, and WRS.new") +
  theme_minimal()

##ECDF Plot Comparing Three Variables

# Create the ECDF plot using ggplot2
ggplot(comparison_long, aes(x = Value, color = Variable)) +
  stat_ecdf(linewidth = 1) +
  labs(title = "ECDF Comparison of EPI.new, AIR.new, and WRS.new", 
       x = "Values", y = "ECDF") +
  theme_minimal() +
  theme(legend.position = "bottom")

#Exercise 2 (Examine the influence of population on various indexes (EPI, ECO, etc.))

population_data <- read.csv("C:/Users/Jake Lorenzo/Desktop/Data Analytics/Lab 2/countries_populations_2023.csv")

View(population_data)

attach(population_data)

# drop countries not in epi results
populations <- population_data[-which(!population_data$Country %in% EPI_data$country),]

# sort populations by country
populations <- populations[order(populations$Country),]

# drop countries not in populations
EPI_data.sub <- EPI_data[-which(!EPI_data$country %in% populations$Country),]

# sort epi results by country
EPI_data.sub <- EPI_data.sub[order(EPI_data.sub$country),]

# only keep necessary columns
EPI_data.sub <- EPI_data.sub[,c("country","EPI.old","EPI.new")]

# convert population to numeric
EPI_data.sub$population <- as.numeric(populations$Population)

# compute population log base 10
EPI_data.sub$population_log <- log10(EPI_data.sub$population)

#viewing the column titles of EPI_data.sub
head(EPI_data.sub)

#creating and plotting a linear model
lin.mod.epinew <- lm(EPI.new ~ population_log, data = EPI_data.sub)

plot(EPI.new ~ population_log, data = EPI_data.sub)
abline(lin.mod.epinew)

summary(lin.mod.epinew)
plot(lin.mod.epinew)

ggplot(EPI_data.sub, aes(x = population_log, y = EPI.new)) +
geom_point() +
stat_smooth(method = "lm")

ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
geom_point() +
geom_hline(yintercept = 0) +
labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

# Q-Q Plot
qqnorm(lin.mod.epinew$residuals)
qqline(lin.mod.epinew$residuals, col = "red")

