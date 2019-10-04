dataNewau <- read.csv("/Statistics/sshpa/newau_year.csv", header = TRUE)
head(dataNewau)

boxplot(authors~pubyear, data=dataNewau, main="Year ~ Number of New Authors", 
   xlab="Year", ylab="# Authors")

dataNewau <- read.csv("/Statistics/sshpa/newau_year.csv", header = TRUE)
head(dataNewau)

ggplot(data=dataNewau,
       aes(x=pubyear, y=authors, colour=field)) +
       geom_line() +
       ylab("# Authors") +
       scale_color_discrete(name = "Fields", labels = c("Economics", "Education", "Medical", "Others")) +
       scale_x_continuous(name = "Year", breaks = seq(2008, max(dataNewau$pubyear), by = 1))

dataNewau <- read.csv("/Statistics/sshpa/newau_avgage.csv", header = TRUE)
head(dataNewau)

ggplot(data=dataNewau,
       aes(x=pubyear, y=avgage, color=field)) +
       geom_line() +
       xlab("Year") +
       ylab("Age") +
       scale_color_discrete(name = "Fields", labels = c("Economics", "Education", "Medical", "Others")) +
       scale_x_continuous(name = "Year", breaks = seq(2008, max(dataNewau$pubyear), by = 1))

##########
dataNewau <- read.csv("/Statistics/sshpa/newau_age.csv", header = TRUE)
head(dataNewau)

boxplot(age~pubyear, data=dataNewau[dataNewau$field=='eco',], main="Year ~ Age of New Authors", 
   xlab="Year", ylab="Age")

abline(h=mean(dataNewau[dataNewau$field=='eco',]$age), col="red", lwd=3, lty=2)

text(0.2, y = mean(dataNewau[dataNewau$field=='eco',]$age) + 1.1, labels = floor(mean(dataNewau[dataNewau$field=='eco',]$age)), col="red")


boxplot(age~pubyear, data=dataNewau[dataNewau$field=='edu',], main="Year ~ Age of New Authors", 
   xlab="Year", ylab="Age")

abline(h=mean(dataNewau[dataNewau$field=='edu',]$age), col="red", lwd=3, lty=2)

text(0.2, y = mean(dataNewau[dataNewau$field=='edu',]$age) + 1.1, labels = floor(mean(dataNewau[dataNewau$field=='edu',]$age)), col="red")


boxplot(age~pubyear, data=dataNewau[dataNewau$field=='med',], main="Year ~ Age of New Authors", 
   xlab="Year", ylab="Age")

abline(h=mean(dataNewau[dataNewau$field=='med',]$age), col="red", lwd=3, lty=2)

text(0.2, y = mean(dataNewau[dataNewau$field=='med',]$age) + 1.1, labels = floor(mean(dataNewau[dataNewau$field=='med',]$age)), col="red")


boxplot(age~pubyear, data=dataNewau[dataNewau$field=='other',], main="Year ~ Age of New Authors", 
   xlab="Year", ylab="Age")

abline(h=mean(dataNewau[dataNewau$field=='other',]$age), col="red", lwd=3, lty=2)

text(0.2, y = mean(dataNewau[dataNewau$field=='other',]$age) + 1.1, labels = floor(mean(dataNewau[dataNewau$field=='other',]$age)), col="red")


##########
dataNewau <- read.csv("/Statistics/sshpa/age_author_year.csv", header = TRUE)
head(dataNewau)

boxplot(age~pubyear, data=dataNewau[dataNewau$field=='eco',], main="Year ~ Age of Authors", 
   xlab="Year", ylab="Age")

abline(h=mean(dataNewau[dataNewau$field=='eco',]$age), col="red", lwd=3, lty=2)

text(0.2, y = mean(dataNewau[dataNewau$field=='eco',]$age) + 1.1, labels = floor(mean(dataNewau[dataNewau$field=='eco',]$age)), col="red")


boxplot(age~pubyear, data=dataNewau[dataNewau$field=='edu',], main="Year ~ Age of Authors", 
   xlab="Year", ylab="Age")

abline(h=mean(dataNewau[dataNewau$field=='edu',]$age), col="red", lwd=3, lty=2)

text(0.2, y = mean(dataNewau[dataNewau$field=='edu',]$age) + 1.1, labels = floor(mean(dataNewau[dataNewau$field=='edu',]$age)), col="red")


boxplot(age~pubyear, data=dataNewau[dataNewau$field=='med',], main="Year ~ Age of Authors", 
   xlab="Year", ylab="Age")

abline(h=mean(dataNewau[dataNewau$field=='med',]$age), col="red", lwd=3, lty=2)

text(0.2, y = mean(dataNewau[dataNewau$field=='med',]$age) + 1.1, labels = floor(mean(dataNewau[dataNewau$field=='med',]$age)), col="red")


boxplot(age~pubyear, data=dataNewau[dataNewau$field=='other',], main="Year ~ Age of Authors", 
   xlab="Year", ylab="Age")

abline(h=mean(dataNewau[dataNewau$field=='other',]$age), col="red", lwd=3, lty=2)

text(0.2, y = mean(dataNewau[dataNewau$field=='other',]$age) + 1.1, labels = floor(mean(dataNewau[dataNewau$field=='other',]$age)), col="red")

##########
dataNewau <- read.csv("/Statistics/sshpa/newau_avgage_sex.csv", header = TRUE)
head(dataNewau)

ggplot(data=dataNewau[dataNewau$field=='eco',],
       aes(x=pubyear, y=avgage, colour=as.factor(sex))) +
       geom_line() +
       ylab("Age") +
       scale_color_discrete(name = "Gender", labels = c("Male", "Female")) +
       scale_x_continuous(name = "Year", breaks = seq(2008, max(dataNewau$pubyear), by = 1))

ggplot(data=dataNewau[dataNewau$field=='edu',],
       aes(x=pubyear, y=avgage, colour=as.factor(sex))) +
       geom_line() +
       ylab("Age") +
       scale_color_discrete(name = "Gender", labels = c("Male", "Female")) +
       scale_x_continuous(name = "Year", breaks = seq(2008, max(dataNewau$pubyear), by = 1))

ggplot(data=dataNewau[dataNewau$field=='med',],
       aes(x=pubyear, y=avgage, colour=as.factor(sex))) +
       geom_line() +
       ylab("Age") +
       scale_color_discrete(name = "Gender", labels = c("Male", "Female")) +
       scale_x_continuous(name = "Year", breaks = seq(2008, max(dataNewau$pubyear), by = 1))

ggplot(data=dataNewau[dataNewau$field=='other',],
       aes(x=pubyear, y=avgage, colour=as.factor(sex))) +
       geom_line() +
       ylab("Age") +
       scale_color_discrete(name = "Gender", labels = c("Male", "Female")) +
       scale_x_continuous(name = "Year", breaks = seq(2008, max(dataNewau$pubyear), by = 1))
