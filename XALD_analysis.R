#Fix install.packages
trace(utils:::unpackPkgZip, edit=TRUE)
#Line 142 = 2

# Install and Load Packages
library(openxlsx)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)

# Import xlsx data
df = read.xlsx("Jan2018-July2018.xlsx", colNames = TRUE)

# Data by sex
table(df$sex)

# Data by kit_type
table(unlist(df$kit_type))

# Remove Special Request and Known PKU kit types
df = df[!grepl("Known PKU", df$kit_type), ]
df = df[!grepl("Special Request", df$kit_type), ]

# Histogram of C24
hist(df$C24LPC, breaks= seq(0, 1.6, by=0.01), ylim=c(0,7000), xlab="[C24:0-LPC]", main="", col="grey50")
minor.tick(nx=1, tick.ratio=0.1)

ggplot(data=df, aes(df$C24LPC)) + geom_histogram(breaks= seq(0, 1.6, by=0.01), fill = "grey50", colour = "black") + 
  labs(x="[C24:0-LPC]", y="Frequency") + scale_x_continuous(breaks=seq(0,1.6,0.25), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

stat_bin(binwidth= 0.01, geom="text", size=1.5, aes(label=..count..) , 
         vjust = -0.5)

# Histogram of C26
hist(df$C26LPC, breaks= seq(0, 1, by=0.005), ylim=c(0,7000), xlab="[C26:0-LPC]", main="", col="grey50")
minor.tick(nx=1, tick.ratio=0.1)

ggplot(data=df, aes(df$C26LPC)) + geom_histogram(breaks= seq(0, 0.9, by=0.005), fill = "grey50", colour = "black") + 
  labs(x="[C26:0-LPC]", y="Frequency") + scale_x_continuous(breaks=seq(0,0.9,0.1), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Birthweight
df$WEIGHTGMS = as.numeric(as.character(df$WEIGHTGMS))
bw_breaks = c(0, 1000, 1500, 2500, 10000)
bw_labels = c("Extremely Low Birth Weight", "Very Low Birth Weight", "Low Birth Weight", "Normal Birth Weight")

data <- as.data.table(df)
setDT(data)[ , bwgroups := cut(df$WEIGHTGMS, breaks = bw_breaks, right = FALSE, labels = bw_labels)]

table(data$bwgroups)
ggplot(data, aes(x=bwgroups, fill=bwgroups)) + geom_bar(stat="count") +
  labs(title="Samples By Birth Weight") + labs(x= "Birth Weight Categories", y="Number of Samples") + 
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1))

# Collection Age
df$COLLECTIONAGE = as.numeric(as.character(df$COLLECTIONAGE))
ca_breaks = c(0, 24, 48, 72, 96, 120, 144, 168, 192, 216, 240, 264, 288, 312, 336, 1080, 1824, 2568, 3312, 4801)
ca_labels = c("Less than 24hr", "24-48hr", "49-72hr", "73-96hr", "4-5 days", "5-6 days", 
              "6-7 days", "7-8 days", "8-9 days", "9-10 days", "10-11 days", "11-12 days", 
              "12-13 days", "13-14 days", "14-45 days", "46-76 days", "77-107 days", "108-138 days", "Above 138 days")

data <- as.data.table(df)
setDT(data)[ , cagroups := cut(df$COLLECTIONAGE, breaks = ca_breaks, right = FALSE, labels = ca_labels)]

table(data$cagroups)
ggplot(na.omit(data), aes(x=cagroups, fill=cagroups)) + geom_bar(stat="count") +
  labs(title="Samples By Collection Age") + labs(x= "Collection Age", y="Number of Samples") + 
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1))

# C26 ratios
df1 = df %>% mutate("C26toC20" = round(C26LPC/C20LPC, 2), "C24toC20" = round(C24LPC/C20LPC, 2), "C26toC22" = round(C26LPC/C22LPC, 2),"C24toC22" = round(C24LPC/C22LPC, 2))
df1$C26toC20[which(df1$C26toC20=="Inf")] <- NA
df1$C26toC22[which(df1$C26toC22=="Inf")] <- NA

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplotRegression(lm(C26toC20 ~ C26LPC, data = df1))
summary(lm(C26toC20 ~ C26LPC, data = df1))

ggplotRegression(lm(C26toC22 ~ C26LPC, data = df1))
summary(lm(C26toC22 ~ C26LPC, data = df1))

p1 = ggplot(df1) + geom_point(aes(x=C26LPC, y=C26toC20, color = kit_type)) + 
  stat_smooth(aes(x=C26LPC, y=C26toC20), method = "lm", col = "red")

p2 = ggplot(df1) + geom_point(aes(x=C26LPC, y=C26toC22, color = kit_type))

ggarrange(p1, p2, ncol = 1, nrow = 2)

