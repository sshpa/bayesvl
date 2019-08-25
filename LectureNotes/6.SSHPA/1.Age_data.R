# do thi tong quan tren so bai
dataAge <- read.csv("/Statistics/sshpa/age_articles.csv", header = TRUE)
head(dataAge)

boxplot(article~age, data=dataAge, main="Age ~ Number of Article", 
   xlab="Age", ylab="# Articles")

# do thi so tac gia theo do tuoi
dataAge <- read.csv("/Statistics/sshpa/agegrp_authors.csv", header = TRUE)
head(dataAge)

age_labs = c("<25","25-29","30-34","35-39","40-44","45-49","50-54","55-59",">=60")

ggplot(data=dataAge,
       aes(x=age, y=authors, fill=field)) +
       geom_bar(stat="identity", position=position_dodge()) +
       ylab("# Authors") +
       xlab("Age") +
       scale_fill_discrete(name = "Fields", labels = c("Economics", "Education", "Medical", "Others")) +
       scale_x_discrete(limits=age_labs)

ggplot(data=dataAge,
       aes(x=age, y=authors, color=field)) +
       geom_line() +
       ylab("# Authors") +
       xlab("Age") +
       scale_color_discrete(name = "Fields", labels = c("Economics", "Education", "Medical", "Others")) +
       scale_x_discrete(limits=age_labs)

# do thi so bai theo do tuoi
dataAge <- read.csv("/Statistics/sshpa/agegrp_articles.csv", header = TRUE)
head(dataAge)

age_labs = c("<25","25-29","30-34","35-39","40-44","45-49","50-54","55-59",">=60")

ggplot(data=dataAge,
       aes(x=age, y=article, fill=field)) +
       geom_bar(stat="identity", position=position_dodge()) +
       ylab("# Articles") +
       xlab("Age") +
       scale_fill_discrete(name = "Fields", labels = c("Economics", "Education", "Medical", "Others")) +
       scale_x_discrete(limits=age_labs)

ggplot(data=dataAge,
       aes(x=age, y=article, color=field)) +
       geom_line() +
       ylab("# Articles") +
       xlab("Age") +
       scale_color_discrete(name = "Fields", labels = c("Economics", "Education", "Medical", "Others")) +
       scale_x_discrete(limits=age_labs)

# do thi so bai trung binh theo do tuoi
dataAge <- read.csv("/Statistics/sshpa/age_avgarticle.csv", header = TRUE)
head(dataAge)

age_labs = c("<25","25-29","30-34","35-39","40-44","45-49","50-54","55-59",">=60")

ggplot(data=dataAge,
       aes(x=age, y=article, colour=field)) +
       geom_line() +
       ylab("# Articles") +
       xlab("Age") +
       scale_color_discrete(name = "Fields", labels = c("Economics", "Education", "Medical", "Others")) +
       scale_x_discrete(limits=age_labs)

# do thi tuoi trung binh theo nam
dataAge <- read.csv("/Statistics/sshpa/age_year.csv", header = TRUE)
head(dataAge)

age_labs = c("<25","25-29","30-34","35-39","40-44","45-49","50-54","55-59",">=60")

ggplot(data=dataAge,
       aes(x=pubyear, y=avgage, colour=field)) +
       geom_line() +
       ylab("Age") +
       xlab("Year") +
       scale_color_discrete(name = "Fields", labels = c("Economics", "Education", "Medical", "Others")) +
       scale_x_continuous(name = "Year", breaks = seq(min(dataAge$pubyear), max(dataAge$pubyear), by = 1))
