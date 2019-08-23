data1 <- read.csv("/Statistics/sshpa/countByAge.csv", header = TRUE)
head(data1)

data1$region <- as.numeric(data1$region) + 1

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "article", "norm")
model <- bvl_addNode(model, "sex", "cat",test=c(1,2)) #Sex
model <- bvl_addNode(model, "region", "cat")
model <- bvl_addNode(model, "age", "norm") #age

model <- bvl_addArc(model, "sex",  "article", "slope")
model <- bvl_addArc(model, "age",  "article", "slope")

model <- bvl_addArc(model, "region", "article", "varint")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4, cores = 4)

bvl_plotTrace(model)

ggplot(data1, aes(x=age, y=article, fill=as.factor(sex))) +
geom_bar(stat="identity",position=position_dodge())
