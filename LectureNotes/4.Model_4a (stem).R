data1 <- read.csv("/Statistics/STEM/5000Toan.csv", header = TRUE)
head(data1)

#data1$Careers <- as.numeric(data1$CarMotGr) + as.numeric(data1$CarFatGr) - 1

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "APS45ID", "norm")
model <- bvl_addNode(model, "Sex", "cat")
model <- bvl_addNode(model, "EcoStt", "cat")
model <- bvl_addNode(model, "CarMotGr", "cat")
model <- bvl_addNode(model, "EduMot", "cat")
model <- bvl_addNode(model, "CarFatGr", "cat")
model <- bvl_addNode(model, "EduFat", "cat")
model <- bvl_addNode(model, "RankingF", "cat")
model <- bvl_addNode(model, "NumberofChi", "cat")

model <- bvl_addNode(model, "Careers",  "trans", fun="({0}-1)", out_type = "int")
model <- bvl_addArc(model, "CarMotGr",  "Careers", "+")
model <- bvl_addArc(model, "CarFatGr",  "Careers", "+")

model <- bvl_addArc(model, "EduMot",  "APS45ID", "slope")
model <- bvl_addArc(model, "EduFat",  "APS45ID", "slope")
model <- bvl_addArc(model, "RankingF",  "APS45ID", "slope")
model <- bvl_addArc(model, "NumberofChi",  "APS45ID", "slope")
model <- bvl_addArc(model, "EcoStt",   "APS45ID", "slope")
model <- bvl_addArc(model, "Sex", "APS45ID", "slope")

model <- bvl_addArc(model, "Careers", "APS45ID", "varint")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4, cores = 4)

bvl_trace(model)



###############################

data1$CarMotGr[data1$CarMotGr == "labor"] <- "Labor"
data1$CarMotGr <- droplevels(data1$CarMotGr)

data1$ParentJob <- as.numeric(data1$CarMotGr) + as.numeric(data1$CarFatGr) - 1

data1$SexPJ <- factor(paste0(data1$Sex,"_",data1$ParentJob))

data1$OnlyChi <- ifelse(data1$RankingF + data1$NumberofChi== 2, 1, 0)

# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "APS45ID", "norm")
model <- bvl_addNode(model, "EcoStt", "cat")
model <- bvl_addNode(model, "EduMot", "cat")
model <- bvl_addNode(model, "EduFat", "cat")
#model <- bvl_addNode(model, "RankingF", "cat")
model <- bvl_addNode(model, "OnlyChi", "binom")
model <- bvl_addNode(model, "NumberofChi", "cat")
model <- bvl_addNode(model, "Sex", "cat")
model <- bvl_addNode(model, "SexPJ", "cat")

#model <- bvl_addNode(model, "OnlyChi", "trans", fun = "({0} == 2 ? 1 : 0)")
#model <- bvl_addArc(model, "RankingF",  "OnlyChi", "+")
#model <- bvl_addArc(model, "NumberofChi",  "OnlyChi", "+")

model <- bvl_addArc(model, "EduMot",  "APS45ID", "slope")
model <- bvl_addArc(model, "EduFat",  "APS45ID", "slope")
model <- bvl_addArc(model, "OnlyChi",  "APS45ID", "slope")
model <- bvl_addArc(model, "NumberofChi",  "APS45ID", "slope")
model <- bvl_addArc(model, "EcoStt",   "APS45ID", "slope")

model <- bvl_addArc(model, "SexPJ", "APS45ID", "varint")

model <- bvl_addArc(model, "Sex", "SexPJ", "varint")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4, cores = 4)

bvl_trace(model)

