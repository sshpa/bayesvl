data1 <- read.csv("/Users/Shared/Previously Relocated Items/Security/Statistics/network/bayesvl/LectureNotes/4.Stem/Data/5000Toan.csv", header = TRUE)
head(data1)

library(tidyr)
data1 <- data1 %>% drop_na(TimeSoc, TimeSci)

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "APS45ID", "norm")
model <- bvl_addNode(model, "TimeSci", "cat")
model <- bvl_addNode(model, "Sex", "cat")
#model <- bvl_addNode(model, "Gradeid", "cat")

model <- bvl_addArc(model, "TimeSci",  "APS45ID", "slope")
model <- bvl_addArc(model, "Gradeid", "APS45ID", "varint")
#model <- bvl_addArc(model, "Sex", "APS45ID", "varint")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 5000, iter = 10000, chains = 4, cores = 4)

bvl_trace(model)


