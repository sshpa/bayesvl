dataSex <- read.csv("/Statistics/sshpa/sex_articles.csv", header = TRUE)
head(dataSex)

ggplot(data=dataSex,
       aes(x=field, y=article, fill=as.factor(sex))) +
       geom_bar(stat="identity", position=position_dodge()) +
       ylab("# Articles") +
       xlab("Fields") +
       scale_fill_discrete(name = "Gender", labels = c("Male", "Female"))
       #scale_x_discrete(limits=c("Economics", "Education", "Medical", "Others"))


dataSex <- read.csv("/Statistics/sshpa/sex_authors.csv", header = TRUE)
head(dataSex)

ggplot(data=dataSex,
       aes(x=field, y=authors, fill=as.factor(sex))) +
       geom_bar(stat="identity", position=position_dodge()) +
       ylab("# Authors") +
       xlab("Fields") +
       scale_fill_discrete(name = "Gender", labels = c("Male", "Female"))
       #scale_x_discrete(limits=c("Economics", "Education", "Medical", "Others"))

dataSex <- read.csv("/Statistics/sshpa/sex_avgage_year.csv", header = TRUE)
head(dataSex)

ggplot(data=dataSex[dataSex$field=='eco',],
       aes(x=createdyear, y=avgage, color=as.factor(sex))) +
       geom_line() +
       ylab("Age") +
       scale_color_discrete(name = "Gender", labels = c("Male", "Female")) +
       scale_x_continuous(name = "Year", breaks = seq(2008, 2018, by = 1))

ggplot(data=dataSex[dataSex$field=='edu',],
       aes(x=createdyear, y=avgage, color=as.factor(sex))) +
       geom_line() +
       ylab("Age") +
       scale_color_discrete(name = "Gender", labels = c("Male", "Female")) +
       scale_x_continuous(name = "Year", breaks = seq(2008, 2018, by = 1))

ggplot(data=dataSex[dataSex$field=='med',],
       aes(x=createdyear, y=avgage, color=as.factor(sex))) +
       geom_line() +
       ylab("Age") +
       scale_color_discrete(name = "Gender", labels = c("Male", "Female")) +
       scale_x_continuous(name = "Year", breaks = seq(2008, 2018, by = 1))


ggplot(data=dataSex[dataSex$field=='other',],
       aes(x=createdyear, y=avgage, color=as.factor(sex))) +
       geom_line() +
       ylab("Age") +
       scale_color_discrete(name = "Gender", labels = c("Male", "Female")) +
       scale_x_continuous(name = "Year", breaks = seq(2008, 2018, by = 1))

