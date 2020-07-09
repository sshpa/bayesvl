data1 <- read.csv("/Users/Shared/Previously Relocated Items/Security/Statistics/network/bayesvl/LectureNotes/4.Stem/Data/5000Toan.csv", header = TRUE)
head(data1)

library(tidyr)
data1 <- data1 %>% drop_na(TimeSoc, TimeSci)

data1$GradeNum <- as.factor(paste0(data1$Sex, "_", data1$Gradeid))

#data1$Sex_Grade <- factor(paste0(data1$Sex,"_",data1$Gradeid))

#data1$Sex <- as.numeric(data1$Sex)
#data1$Gradeid <- as.numeric(data1$Gradeid)

#Gradeid2Sex <- unique(data1[c("Gradeid","Sex")])[,"Sex"]

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "APS45ID", "norm")
model <- bvl_addNode(model, "TimeSci", "cat")
model <- bvl_addNode(model, "Sex", "cat")
model <- bvl_addNode(model, "GradeNum", "cat")

model <- bvl_addArc(model, "TimeSci",  "APS45ID", "slope")
model <- bvl_addArc(model, "Sex", "GradeNum", "varint")
model <- bvl_addArc(model, "GradeNum", "APS45ID", "varint")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 5000, iter = 10000, chains = 4, cores = 4)

bvl_trace(model)

###############
stem <- read.csv(file="/Users/Shared/Previously Relocated Items/Security/Statistics/STEM/STEM_model1.csv",header=T)

stem$school_grade <- factor(paste0(stem$school,"_",stem$gradeid))

stem$schoolid <- as.numeric(stem$school)
stem$gradenum <- as.numeric(stem$school_grade)

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "aps45id", "norm")
model <- bvl_addNode(model, "schoolid", "cat")
model <- bvl_addNode(model, "gradenum", "cat")
model <- bvl_addNode(model, "sex", "cat")

model <- bvl_addArc(model, "sex",  "aps45id", "slope")
model <- bvl_addArc(model, "schoolid", "gradenum", "varint")
model <- bvl_addArc(model, "gradenum", "aps45id", "varint")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, stem, warmup = 5000, iter = 10000, chains = 4, cores = 4)

###############
stem <- read.csv(file="/Users/Shared/Previously Relocated Items/Security/Statistics/STEM/STEM_model1.csv",header=T)

library(tidyr)
data1 <- data1 %>% drop_na(timesoc, timesci)

stem$sex_grade <- factor(paste0(stem$sex,"_",stem$gradeid))

stem$gradenum <- as.numeric(stem$sex_grade)

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "aps45id", "norm")
model <- bvl_addNode(model, "timesci", "cat")
model <- bvl_addNode(model, "sex", "cat")
model <- bvl_addNode(model, "gradenum", "cat")

model <- bvl_addArc(model, "timesci",  "aps45id", "slope")
model <- bvl_addArc(model, "sex", "gradenum", "varint")
model <- bvl_addArc(model, "gradenum", "aps45id", "varint")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, stem, warmup = 2000, iter = 5000, chains = 4, cores = 4)
