
##########################################Q2
library(readxl)
data1 <- read_excel("/Users/yuxichen/biostat620/hw4/ScreenTime-hw3Q3.xlsx")
data2 = read_excel("/Users/yuxichen/biostat620/hw4/data2.xlsx")
data =merge(data1, data2, by="pseudo_id")

#focus on only people with treatment B
dataB = data[data$Treatment == "B",]

#create dummy variable as required
dataB$X <- ifelse(dataB$Day == "Su" | dataB$Day == "Sa", 0, 1 )
dataB$B <- ifelse(dataB$Phase == "Baseline",  0, 1)


############################################Q2(a)
# create lag-1 variable for y
library(dplyr)
library(purrr)

dataB <- dataB %>%
  group_by(pseudo_id) %>%
  mutate(lagged_y = lag(Pickups, n = 1, default = NA)) %>%
  ungroup()  # Optionally ungroup data after creating the lagged variable

#remove row with NA value
data_model = na.omit(dataB)

#create model for each eight individidual 
p2 = data_model[data_model$pseudo_id == 2,]
p3 = data_model[data_model$pseudo_id == 3,]
p4 = data_model[data_model$pseudo_id == 4,]
p5 = data_model[data_model$pseudo_id == 5,]
p8 = data_model[data_model$pseudo_id == 8,]
p15 = data_model[data_model$pseudo_id == 15,]
p16 = data_model[data_model$pseudo_id == 16,]
p18 = data_model[data_model$pseudo_id == 18,]

model2 = glm(Pickups ~ lagged_y + B + X, 
            data = p2, 
            family = poisson(link = "log"), 
            offset = log(Tot.Scr.Time))

model3 = glm(Pickups ~ lagged_y + B + X, 
             data = p3, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model4 = glm(Pickups ~ lagged_y + B + X, 
             data = p4, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model5 = glm(Pickups ~ lagged_y + B + X, 
             data = p5, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model8 = glm(Pickups~ lagged_y + B + X, 
             data = p8, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model15 = glm(Pickups ~ lagged_y + B + X, 
             data = p15, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model16 = glm(Pickups ~ lagged_y + B + X, 
             data = p16, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model18 = glm(Pickups ~ lagged_y + B + X, 
             data = p18, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

############################################Q2(b)
meta0.var = 1/(1/(summary(model2)$coefficients[, "Std. Error"][1])^2 + 1/(summary(model3)$coefficients[, "Std. Error"][1])^2
               + 1/(summary(model4)$coefficients[, "Std. Error"][1])^2 + 1/(summary(model5)$coefficients[, "Std. Error"][1])^2
               + 1/(summary(model8)$coefficients[, "Std. Error"][1])^2 +  1/(summary(model15)$coefficients[, "Std. Error"][1])^2
               + 1/(summary(model16)$coefficients[, "Std. Error"][1])^2 + 1/(summary(model18)$coefficients[, "Std. Error"][1])^2)

meta0 =  (model2$coefficients[1]/(summary(model2)$coefficients[, "Std. Error"][1])^2 + model3$coefficients[1]/(summary(model3)$coefficients[, "Std. Error"][1])^2
          + model4$coefficients[1]/(summary(model4)$coefficients[, "Std. Error"][1])^2 + model5$coefficients[1]/(summary(model5)$coefficients[, "Std. Error"][1])^2
          + model8$coefficients[1]/(summary(model8)$coefficients[, "Std. Error"][1])^2 +  model15$coefficients[1]/(summary(model15)$coefficients[, "Std. Error"][1])^2
          + model16$coefficients[1]/(summary(model16)$coefficients[, "Std. Error"][1])^2 + model8$coefficients[1]/(summary(model18)$coefficients[, "Std. Error"][1])^2) * meta0.var
        
  
meta1.var = 1/(1/(summary(model2)$coefficients[, "Std. Error"][2])^2 + 1/(summary(model3)$coefficients[, "Std. Error"][2])^2
               + 1/(summary(model4)$coefficients[, "Std. Error"][2])^2 + 1/(summary(model5)$coefficients[, "Std. Error"][2])^2
               + 1/(summary(model8)$coefficients[, "Std. Error"][2])^2 +  1/(summary(model15)$coefficients[, "Std. Error"][2])^2
               + 1/(summary(model16)$coefficients[, "Std. Error"][2])^2 + 1/(summary(model18)$coefficients[, "Std. Error"][2])^2)

meta1 =  (model2$coefficients[2]/(summary(model2)$coefficients[, "Std. Error"][2])^2 + model3$coefficients[2]/(summary(model3)$coefficients[, "Std. Error"][2])^2
          + model4$coefficients[2]/(summary(model4)$coefficients[, "Std. Error"][2])^2 + model5$coefficients[2]/(summary(model5)$coefficients[, "Std. Error"][2])^2
          + model8$coefficients[2]/(summary(model8)$coefficients[, "Std. Error"][2])^2 +  model15$coefficients[2]/(summary(model15)$coefficients[, "Std. Error"][2])^2
          + model16$coefficients[2]/(summary(model16)$coefficients[, "Std. Error"][2])^2 + model8$coefficients[2]/(summary(model18)$coefficients[, "Std. Error"][2])^2) * meta1.var

meta2.var = 1/(1/(summary(model2)$coefficients[, "Std. Error"][3])^2 + 1/(summary(model3)$coefficients[, "Std. Error"][3])^2
               + 1/(summary(model4)$coefficients[, "Std. Error"][3])^2 + 1/(summary(model5)$coefficients[, "Std. Error"][3])^2
               + 1/(summary(model8)$coefficients[, "Std. Error"][3])^2 +  1/(summary(model15)$coefficients[, "Std. Error"][3])^2
               + 1/(summary(model16)$coefficients[, "Std. Error"][3])^2 + 1/(summary(model18)$coefficients[, "Std. Error"][3])^2)

meta2 =  (model2$coefficients[3]/(summary(model2)$coefficients[, "Std. Error"][3])^2 + model3$coefficients[3]/(summary(model3)$coefficients[, "Std. Error"][3])^2
          + model4$coefficients[3]/(summary(model4)$coefficients[, "Std. Error"][3])^2 + model5$coefficients[3]/(summary(model5)$coefficients[, "Std. Error"][3])^2
          + model8$coefficients[3]/(summary(model8)$coefficients[, "Std. Error"][3])^2 +  model15$coefficients[3]/(summary(model15)$coefficients[, "Std. Error"][3])^2
          + model16$coefficients[3]/(summary(model16)$coefficients[, "Std. Error"][3])^2 + model8$coefficients[3]/(summary(model18)$coefficients[, "Std. Error"][3])^2) * meta2.var

meta3.var = 1/(1/(summary(model2)$coefficients[, "Std. Error"][4])^2 + 1/(summary(model3)$coefficients[, "Std. Error"][4])^2
               + 1/(summary(model4)$coefficients[, "Std. Error"][4])^2 + 1/(summary(model5)$coefficients[, "Std. Error"][4])^2
               + 1/(summary(model8)$coefficients[, "Std. Error"][4])^2 +  1/(summary(model15)$coefficients[, "Std. Error"][4])^2
               + 1/(summary(model16)$coefficients[, "Std. Error"][4])^2 + 1/(summary(model18)$coefficients[, "Std. Error"][4])^2)

meta3 =  (model2$coefficients[4]/(summary(model2)$coefficients[, "Std. Error"][4])^2 + model3$coefficients[4]/(summary(model3)$coefficients[, "Std. Error"][4])^2
          + model4$coefficients[4]/(summary(model4)$coefficients[, "Std. Error"][4])^2 + model5$coefficients[4]/(summary(model5)$coefficients[, "Std. Error"][4])^2
          + model8$coefficients[4]/(summary(model8)$coefficients[, "Std. Error"][4])^2 +  model15$coefficients[4]/(summary(model15)$coefficients[, "Std. Error"][4])^2
          + model16$coefficients[4]/(summary(model16)$coefficients[, "Std. Error"][4])^2 + model8$coefficients[4]/(summary(model18)$coefficients[, "Std. Error"][4])^2) * meta3.var

############################################Q3(a)
#########################################################################################for intervention a
dataA = data[data$Treatment == "A",]

dataA$X <- ifelse(dataA$Day == "Su" | dataA$Day == "Sa", 0, 1 )
dataA$B <- ifelse(dataA$Phase == "Baseline",  0, 1)

dataA <- dataA %>%
  group_by(pseudo_id) %>%
  mutate(lagged_y = lag(Pickups, n = 1, default = NA)) %>%
  ungroup()  # Optionally ungroup data after creating the lagged variable

#remove row with NA value
data_model = na.omit(dataA)

p2 = data_model[data_model$pseudo_id == 1,]
p3 = data_model[data_model$pseudo_id == 6,]
p4 = data_model[data_model$pseudo_id == 7,]
p5 = data_model[data_model$pseudo_id == 10,]
p8 = data_model[data_model$pseudo_id == 13,]
p15 = data_model[data_model$pseudo_id == 14,]
p16 = data_model[data_model$pseudo_id == 24,]

model2 = glm(Pickups ~ lagged_y + B + X
             + sex + age + pets + siblings, 
             data = p2, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model3 = glm(Pickups ~ lagged_y + B + X
             + sex + age + pets + siblings, 
             data = p3, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model4 = glm(Pickups ~ lagged_y + B + X
             + sex + age + pets + siblings, 
             data = p4, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model5 = glm(Pickups ~ lagged_y + B + X
             + sex + age + pets + siblings, 
             data = p5, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model8 = glm(Pickups~ lagged_y + B + X
             + sex + age + pets + siblings, 
             data = p8, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model15 = glm(Pickups ~ lagged_y + B + X
              + sex + age + pets + siblings, 
              data = p15, 
              family = poisson(link = "log"), 
              offset = log(Tot.Scr.Time))

model16 = glm(Pickups ~ lagged_y + B + X
              + sex + age + pets + siblings, 
              data = p16, 
              family = poisson(link = "log"), 
              offset = log(Tot.Scr.Time))

meta0.var = 1/(1/(summary(model2)$coefficients[, "Std. Error"][1])^2 + 1/(summary(model3)$coefficients[, "Std. Error"][1])^2
               + 1/(summary(model4)$coefficients[, "Std. Error"][1])^2 + 1/(summary(model5)$coefficients[, "Std. Error"][1])^2
               + 1/(summary(model8)$coefficients[, "Std. Error"][1])^2 +  1/(summary(model15)$coefficients[, "Std. Error"][1])^2
               + 1/(summary(model16)$coefficients[, "Std. Error"][1])^2)

meta0 =  (model2$coefficients[1]/(summary(model2)$coefficients[, "Std. Error"][1])^2 + model3$coefficients[1]/(summary(model3)$coefficients[, "Std. Error"][1])^2
          + model4$coefficients[1]/(summary(model4)$coefficients[, "Std. Error"][1])^2 + model5$coefficients[1]/(summary(model5)$coefficients[, "Std. Error"][1])^2
          + model8$coefficients[1]/(summary(model8)$coefficients[, "Std. Error"][1])^2 +  model15$coefficients[1]/(summary(model15)$coefficients[, "Std. Error"][1])^2
          + model16$coefficients[1]/(summary(model16)$coefficients[, "Std. Error"][1])^2 ) * meta0.var


meta1.var = 1/(1/(summary(model2)$coefficients[, "Std. Error"][2])^2 + 1/(summary(model3)$coefficients[, "Std. Error"][2])^2
               + 1/(summary(model4)$coefficients[, "Std. Error"][2])^2 + 1/(summary(model5)$coefficients[, "Std. Error"][2])^2
               + 1/(summary(model8)$coefficients[, "Std. Error"][2])^2 +  1/(summary(model15)$coefficients[, "Std. Error"][2])^2
               + 1/(summary(model16)$coefficients[, "Std. Error"][2])^2 )

meta1 =  (model2$coefficients[2]/(summary(model2)$coefficients[, "Std. Error"][2])^2 + model3$coefficients[2]/(summary(model3)$coefficients[, "Std. Error"][2])^2
          + model4$coefficients[2]/(summary(model4)$coefficients[, "Std. Error"][2])^2 + model5$coefficients[2]/(summary(model5)$coefficients[, "Std. Error"][2])^2
          + model8$coefficients[2]/(summary(model8)$coefficients[, "Std. Error"][2])^2 +  model15$coefficients[2]/(summary(model15)$coefficients[, "Std. Error"][2])^2
          + model16$coefficients[2]/(summary(model16)$coefficients[, "Std. Error"][2])^2 ) * meta1.var

meta2.var = 1/(1/(summary(model2)$coefficients[, "Std. Error"][3])^2 + 1/(summary(model3)$coefficients[, "Std. Error"][3])^2
               + 1/(summary(model4)$coefficients[, "Std. Error"][3])^2 + 1/(summary(model5)$coefficients[, "Std. Error"][3])^2
               + 1/(summary(model8)$coefficients[, "Std. Error"][3])^2 +  1/(summary(model15)$coefficients[, "Std. Error"][3])^2
               + 1/(summary(model16)$coefficients[, "Std. Error"][3])^2 )

meta2 =  (model2$coefficients[3]/(summary(model2)$coefficients[, "Std. Error"][3])^2 + model3$coefficients[3]/(summary(model3)$coefficients[, "Std. Error"][3])^2
          + model4$coefficients[3]/(summary(model4)$coefficients[, "Std. Error"][3])^2 + model5$coefficients[3]/(summary(model5)$coefficients[, "Std. Error"][3])^2
          + model8$coefficients[3]/(summary(model8)$coefficients[, "Std. Error"][3])^2 +  model15$coefficients[3]/(summary(model15)$coefficients[, "Std. Error"][3])^2
          + model16$coefficients[3]/(summary(model16)$coefficients[, "Std. Error"][3])^2 ) * meta2.var

meta3.var = 1/(1/(summary(model2)$coefficients[, "Std. Error"][4])^2 + 1/(summary(model3)$coefficients[, "Std. Error"][4])^2
               + 1/(summary(model4)$coefficients[, "Std. Error"][4])^2 + 1/(summary(model5)$coefficients[, "Std. Error"][4])^2
               + 1/(summary(model8)$coefficients[, "Std. Error"][4])^2 +  1/(summary(model15)$coefficients[, "Std. Error"][4])^2
               + 1/(summary(model16)$coefficients[, "Std. Error"][4])^2 )

meta3 =  (model2$coefficients[4]/(summary(model2)$coefficients[, "Std. Error"][4])^2 + model3$coefficients[4]/(summary(model3)$coefficients[, "Std. Error"][4])^2
          + model4$coefficients[4]/(summary(model4)$coefficients[, "Std. Error"][4])^2 + model5$coefficients[4]/(summary(model5)$coefficients[, "Std. Error"][4])^2
          + model8$coefficients[4]/(summary(model8)$coefficients[, "Std. Error"][4])^2 +  model15$coefficients[4]/(summary(model15)$coefficients[, "Std. Error"][4])^2
          + model16$coefficients[4]/(summary(model16)$coefficients[, "Std. Error"][4])^2 ) * meta3.var

#########################################################################################for intervention b
dataB <- dataB %>%
  group_by(pseudo_id) %>%
  mutate(lagged_y = lag(Pickups, n = 1, default = NA)) %>%
  ungroup()  # Optionally ungroup data after creating the lagged variable

#remove row with NA value
data_model = na.omit(dataB)

#create model for each eight individidual 
p2 = data_model[data_model$pseudo_id == 2,]
p3 = data_model[data_model$pseudo_id == 3,]
p4 = data_model[data_model$pseudo_id == 4,]
p5 = data_model[data_model$pseudo_id == 5,]
p8 = data_model[data_model$pseudo_id == 8,]
p15 = data_model[data_model$pseudo_id == 15,]
p16 = data_model[data_model$pseudo_id == 16,]
p18 = data_model[data_model$pseudo_id == 18,]

model2 = glm(Pickups ~ lagged_y + B + X
             + sex + age + pets + siblings, 
             data = p2, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model3 = glm(Pickups ~ lagged_y + B + X
             + sex + age + pets + siblings,
             data = p3, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model4 = glm(Pickups ~ lagged_y + B + X
             + sex + age + pets + siblings,
             data = p4, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model5 = glm(Pickups ~ lagged_y + B + X
             + sex + age + pets + siblings,
             data = p5, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model8 = glm(Pickups~ lagged_y + B + X
             + sex + age + pets + siblings,
             data = p8, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model15 = glm(Pickups ~ lagged_y + B + X
              + sex + age + pets + siblings,
              data = p15, 
              family = poisson(link = "log"), 
              offset = log(Tot.Scr.Time))

model16 = glm(Pickups ~ lagged_y + B + X
              + sex + age + pets + siblings,
              data = p16, 
              family = poisson(link = "log"), 
              offset = log(Tot.Scr.Time))

model18 = glm(Pickups ~ lagged_y + B + X
              + sex + age + pets + siblings,
              data = p18, 
              family = poisson(link = "log"), 
              offset = log(Tot.Scr.Time))

############################################Q2(b)
meta0.var = 1/(1/(summary(model2)$coefficients[, "Std. Error"][1])^2 + 1/(summary(model3)$coefficients[, "Std. Error"][1])^2
               + 1/(summary(model4)$coefficients[, "Std. Error"][1])^2 + 1/(summary(model5)$coefficients[, "Std. Error"][1])^2
               + 1/(summary(model8)$coefficients[, "Std. Error"][1])^2 +  1/(summary(model15)$coefficients[, "Std. Error"][1])^2
               + 1/(summary(model16)$coefficients[, "Std. Error"][1])^2 + 1/(summary(model18)$coefficients[, "Std. Error"][1])^2)

meta0 =  (model2$coefficients[1]/(summary(model2)$coefficients[, "Std. Error"][1])^2 + model3$coefficients[1]/(summary(model3)$coefficients[, "Std. Error"][1])^2
          + model4$coefficients[1]/(summary(model4)$coefficients[, "Std. Error"][1])^2 + model5$coefficients[1]/(summary(model5)$coefficients[, "Std. Error"][1])^2
          + model8$coefficients[1]/(summary(model8)$coefficients[, "Std. Error"][1])^2 +  model15$coefficients[1]/(summary(model15)$coefficients[, "Std. Error"][1])^2
          + model16$coefficients[1]/(summary(model16)$coefficients[, "Std. Error"][1])^2 + model8$coefficients[1]/(summary(model18)$coefficients[, "Std. Error"][1])^2) * meta0.var


meta1.var = 1/(1/(summary(model2)$coefficients[, "Std. Error"][2])^2 + 1/(summary(model3)$coefficients[, "Std. Error"][2])^2
               + 1/(summary(model4)$coefficients[, "Std. Error"][2])^2 + 1/(summary(model5)$coefficients[, "Std. Error"][2])^2
               + 1/(summary(model8)$coefficients[, "Std. Error"][2])^2 +  1/(summary(model15)$coefficients[, "Std. Error"][2])^2
               + 1/(summary(model16)$coefficients[, "Std. Error"][2])^2 + 1/(summary(model18)$coefficients[, "Std. Error"][2])^2)

meta1 =  (model2$coefficients[2]/(summary(model2)$coefficients[, "Std. Error"][2])^2 + model3$coefficients[2]/(summary(model3)$coefficients[, "Std. Error"][2])^2
          + model4$coefficients[2]/(summary(model4)$coefficients[, "Std. Error"][2])^2 + model5$coefficients[2]/(summary(model5)$coefficients[, "Std. Error"][2])^2
          + model8$coefficients[2]/(summary(model8)$coefficients[, "Std. Error"][2])^2 +  model15$coefficients[2]/(summary(model15)$coefficients[, "Std. Error"][2])^2
          + model16$coefficients[2]/(summary(model16)$coefficients[, "Std. Error"][2])^2 + model8$coefficients[2]/(summary(model18)$coefficients[, "Std. Error"][2])^2) * meta1.var

meta2.var = 1/(1/(summary(model2)$coefficients[, "Std. Error"][3])^2 + 1/(summary(model3)$coefficients[, "Std. Error"][3])^2
               + 1/(summary(model4)$coefficients[, "Std. Error"][3])^2 + 1/(summary(model5)$coefficients[, "Std. Error"][3])^2
               + 1/(summary(model8)$coefficients[, "Std. Error"][3])^2 +  1/(summary(model15)$coefficients[, "Std. Error"][3])^2
               + 1/(summary(model16)$coefficients[, "Std. Error"][3])^2 + 1/(summary(model18)$coefficients[, "Std. Error"][3])^2)

meta2 =  (model2$coefficients[3]/(summary(model2)$coefficients[, "Std. Error"][3])^2 + model3$coefficients[3]/(summary(model3)$coefficients[, "Std. Error"][3])^2
          + model4$coefficients[3]/(summary(model4)$coefficients[, "Std. Error"][3])^2 + model5$coefficients[3]/(summary(model5)$coefficients[, "Std. Error"][3])^2
          + model8$coefficients[3]/(summary(model8)$coefficients[, "Std. Error"][3])^2 +  model15$coefficients[3]/(summary(model15)$coefficients[, "Std. Error"][3])^2
          + model16$coefficients[3]/(summary(model16)$coefficients[, "Std. Error"][3])^2 + model8$coefficients[3]/(summary(model18)$coefficients[, "Std. Error"][3])^2) * meta2.var

meta3.var = 1/(1/(summary(model2)$coefficients[, "Std. Error"][4])^2 + 1/(summary(model3)$coefficients[, "Std. Error"][4])^2
               + 1/(summary(model4)$coefficients[, "Std. Error"][4])^2 + 1/(summary(model5)$coefficients[, "Std. Error"][4])^2
               + 1/(summary(model8)$coefficients[, "Std. Error"][4])^2 +  1/(summary(model15)$coefficients[, "Std. Error"][4])^2
               + 1/(summary(model16)$coefficients[, "Std. Error"][4])^2 + 1/(summary(model18)$coefficients[, "Std. Error"][4])^2)

meta3 =  (model2$coefficients[4]/(summary(model2)$coefficients[, "Std. Error"][4])^2 + model3$coefficients[4]/(summary(model3)$coefficients[, "Std. Error"][4])^2
          + model4$coefficients[4]/(summary(model4)$coefficients[, "Std. Error"][4])^2 + model5$coefficients[4]/(summary(model5)$coefficients[, "Std. Error"][4])^2
          + model8$coefficients[4]/(summary(model8)$coefficients[, "Std. Error"][4])^2 +  model15$coefficients[4]/(summary(model15)$coefficients[, "Std. Error"][4])^2
          + model16$coefficients[4]/(summary(model16)$coefficients[, "Std. Error"][4])^2 + model8$coefficients[4]/(summary(model18)$coefficients[, "Std. Error"][4])^2) * meta3.var

#########################################################################################for intervention a or b
dataA = data[data$Treatment == "A" | data$Treatment == "B",]

dataA$X <- ifelse(dataA$Day == "Su" | dataA$Day == "Sa", 0, 1 )
dataA$B <- ifelse(dataA$Phase == "Baseline",  0, 1)

dataA <- dataA %>%
  group_by(pseudo_id) %>%
  mutate(lagged_y = lag(Pickups, n = 1, default = NA)) %>%
  ungroup()  # Optionally ungroup data after creating the lagged variable

#remove row with NA value
data_model = na.omit(dataA)

p1 = data_model[data_model$pseudo_id == 1,]
p2 = data_model[data_model$pseudo_id == 2,]
p3 = data_model[data_model$pseudo_id == 3,]
p4 = data_model[data_model$pseudo_id == 4,]
p5 = data_model[data_model$pseudo_id == 5,]
p6 = data_model[data_model$pseudo_id == 6,]
p7 = data_model[data_model$pseudo_id == 7,]
p8 = data_model[data_model$pseudo_id == 8,]
p10 = data_model[data_model$pseudo_id == 10,]
p13 = data_model[data_model$pseudo_id == 13,]
p14 = data_model[data_model$pseudo_id == 14,]
p15 = data_model[data_model$pseudo_id == 15,]
p16 = data_model[data_model$pseudo_id == 16,]
p18 = data_model[data_model$pseudo_id == 18,]
p24 = data_model[data_model$pseudo_id == 24,]

model1 = glm(Pickups ~ lagged_y + B + X
             + sex + age + pets + siblings, 
             data = p2, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model2 = glm(Pickups ~ lagged_y + B + X
             + sex + age + pets + siblings, 
             data = p2, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model3 = glm(Pickups ~ lagged_y + B + X
             + sex + age + pets + siblings, 
             data = p3, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model4 = glm(Pickups ~ lagged_y + B + X
             + sex + age + pets + siblings, 
             data = p4, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model5 = glm(Pickups ~ lagged_y + B + X
             + sex + age + pets + siblings, 
             data = p5, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model6 = glm(Pickups ~ lagged_y + B + X
             + sex + age + pets + siblings, 
             data = p6, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model7 = glm(Pickups ~ lagged_y + B + X
             + sex + age + pets + siblings, 
             data = p7, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model8 = glm(Pickups~ lagged_y + B + X
             + sex + age + pets + siblings, 
             data = p8, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model10 = glm(Pickups~ lagged_y + B + X
             + sex + age + pets + siblings, 
             data = p10, 
             family = poisson(link = "log"), 
             offset = log(Tot.Scr.Time))

model13 = glm(Pickups~ lagged_y + B + X
              + sex + age + pets + siblings, 
              data = p13, 
              family = poisson(link = "log"), 
              offset = log(Tot.Scr.Time))

model14 = glm(Pickups ~ lagged_y + B + X
              + sex + age + pets + siblings, 
              data = p14, 
              family = poisson(link = "log"), 
              offset = log(Tot.Scr.Time))

model15 = glm(Pickups ~ lagged_y + B + X
              + sex + age + pets + siblings, 
              data = p15, 
              family = poisson(link = "log"), 
              offset = log(Tot.Scr.Time))

model16 = glm(Pickups ~ lagged_y + B + X
              + sex + age + pets + siblings, 
              data = p16, 
              family = poisson(link = "log"), 
              offset = log(Tot.Scr.Time))

model18 = glm(Pickups ~ lagged_y + B + X
              + sex + age + pets + siblings, 
              data = p18, 
              family = poisson(link = "log"), 
              offset = log(Tot.Scr.Time))

model24 = glm(Pickups ~ lagged_y + B + X
              + sex + age + pets + siblings, 
              data = p24, 
              family = poisson(link = "log"), 
              offset = log(Tot.Scr.Time))

meta0.var = 1/(1/(summary(model1)$coefficients[, "Std. Error"][1])^2 + 1/(summary(model2)$coefficients[, "Std. Error"][1])^2
               + 1/(summary(model3)$coefficients[, "Std. Error"][1])^2 + 1/(summary(model4)$coefficients[, "Std. Error"][1])^2
               + 1/(summary(model5)$coefficients[, "Std. Error"][1])^2 +  1/(summary(model6)$coefficients[, "Std. Error"][1])^2
               + 1/(summary(model7)$coefficients[, "Std. Error"][1])^2 + 1/(summary(model8)$coefficients[, "Std. Error"][1])^2
               + 1/(summary(model10)$coefficients[, "Std. Error"][1])^2 + 1/(summary(model13)$coefficients[, "Std. Error"][1])^2
               + 1/(summary(model14)$coefficients[, "Std. Error"][1])^2 + 1/(summary(model15)$coefficients[, "Std. Error"][1])^2
               + 1/(summary(model18)$coefficients[, "Std. Error"][1])^2 + 1/(summary(model24)$coefficients[, "Std. Error"][1])^2)

meta0 =  (model2$coefficients[1]/(summary(model1)$coefficients[, "Std. Error"][1])^2 + model3$coefficients[1]/(summary(model2)$coefficients[, "Std. Error"][1])^2
          + model4$coefficients[1]/(summary(model3)$coefficients[, "Std. Error"][1])^2 + model5$coefficients[1]/(summary(model4)$coefficients[, "Std. Error"][1])^2
          + model8$coefficients[1]/(summary(model5)$coefficients[, "Std. Error"][1])^2 +  model15$coefficients[1]/(summary(model6)$coefficients[, "Std. Error"][1])^2
          + model16$coefficients[1]/(summary(model7)$coefficients[, "Std. Error"][1])^2 + model3$coefficients[1]/(summary(model8)$coefficients[, "Std. Error"][1])^2
          + model3$coefficients[1]/(summary(model10)$coefficients[, "Std. Error"][1])^2 + model3$coefficients[1]/(summary(model13)$coefficients[, "Std. Error"][1])^2
          + model3$coefficients[1]/(summary(model14)$coefficients[, "Std. Error"][1])^2 + model3$coefficients[1]/(summary(model15)$coefficients[, "Std. Error"][1])^2
          + model3$coefficients[1]/(summary(model18)$coefficients[, "Std. Error"][1])^2 + model3$coefficients[1]/(summary(model24)$coefficients[, "Std. Error"][1])^2) * meta0.var


meta1.var = 1/(1/(summary(model1)$coefficients[, "Std. Error"][2])^2 + 1/(summary(model2)$coefficients[, "Std. Error"][2])^2
               + 1/(summary(model3)$coefficients[, "Std. Error"][2])^2 + 1/(summary(model4)$coefficients[, "Std. Error"][2])^2
               + 1/(summary(model5)$coefficients[, "Std. Error"][2])^2 +  1/(summary(model6)$coefficients[, "Std. Error"][2])^2
               + 1/(summary(model7)$coefficients[, "Std. Error"][2])^2 + 1/(summary(model8)$coefficients[, "Std. Error"][2])^2
               + 1/(summary(model10)$coefficients[, "Std. Error"][2])^2 + 1/(summary(model13)$coefficients[, "Std. Error"][2])^2
               + 1/(summary(model14)$coefficients[, "Std. Error"][2])^2 + 1/(summary(model15)$coefficients[, "Std. Error"][2])^2
               + 1/(summary(model18)$coefficients[, "Std. Error"][2])^2 + 1/(summary(model24)$coefficients[, "Std. Error"][2])^2)

meta1 =  (model2$coefficients[2]/(summary(model1)$coefficients[, "Std. Error"][2])^2 + model3$coefficients[2]/(summary(model2)$coefficients[, "Std. Error"][2])^2
          + model4$coefficients[2]/(summary(model3)$coefficients[, "Std. Error"][2])^2 + model5$coefficients[2]/(summary(model4)$coefficients[, "Std. Error"][2])^2
          + model8$coefficients[2]/(summary(model5)$coefficients[, "Std. Error"][2])^2 +  model15$coefficients[2]/(summary(model6)$coefficients[, "Std. Error"][2])^2
          + model16$coefficients[2]/(summary(model7)$coefficients[, "Std. Error"][2])^2 + model3$coefficients[2]/(summary(model8)$coefficients[, "Std. Error"][2])^2
          + model3$coefficients[2]/(summary(model10)$coefficients[, "Std. Error"][2])^2 + model3$coefficients[2]/(summary(model13)$coefficients[, "Std. Error"][2])^2
          + model3$coefficients[2]/(summary(model14)$coefficients[, "Std. Error"][2])^2 + model3$coefficients[2]/(summary(model15)$coefficients[, "Std. Error"][2])^2
          + model3$coefficients[2]/(summary(model18)$coefficients[, "Std. Error"][2])^2 + model3$coefficients[2]/(summary(model24)$coefficients[, "Std. Error"][2])^2) * meta1.var

meta2.var = 1/(1/(summary(model1)$coefficients[, "Std. Error"][3])^2 + 1/(summary(model2)$coefficients[, "Std. Error"][3])^2
               + 1/(summary(model3)$coefficients[, "Std. Error"][3])^2 + 1/(summary(model4)$coefficients[, "Std. Error"][3])^2
               + 1/(summary(model5)$coefficients[, "Std. Error"][3])^2 +  1/(summary(model6)$coefficients[, "Std. Error"][3])^2
               + 1/(summary(model7)$coefficients[, "Std. Error"][3])^2 + 1/(summary(model8)$coefficients[, "Std. Error"][3])^2
               + 1/(summary(model10)$coefficients[, "Std. Error"][3])^2 + 1/(summary(model13)$coefficients[, "Std. Error"][3])^2
               + 1/(summary(model14)$coefficients[, "Std. Error"][3])^2 + 1/(summary(model15)$coefficients[, "Std. Error"][3])^2
               + 1/(summary(model18)$coefficients[, "Std. Error"][3])^2 + 1/(summary(model24)$coefficients[, "Std. Error"][3])^2)

meta2 =  (model2$coefficients[3]/(summary(model1)$coefficients[, "Std. Error"][3])^2 + model3$coefficients[3]/(summary(model2)$coefficients[, "Std. Error"][3])^2
          + model4$coefficients[3]/(summary(model3)$coefficients[, "Std. Error"][3])^2 + model5$coefficients[3]/(summary(model4)$coefficients[, "Std. Error"][3])^2
          + model8$coefficients[3]/(summary(model5)$coefficients[, "Std. Error"][3])^2 +  model15$coefficients[3]/(summary(model6)$coefficients[, "Std. Error"][3])^2
          + model16$coefficients[3]/(summary(model7)$coefficients[, "Std. Error"][3])^2 + model3$coefficients[3]/(summary(model8)$coefficients[, "Std. Error"][3])^2
          + model3$coefficients[3]/(summary(model10)$coefficients[, "Std. Error"][3])^2 + model3$coefficients[3]/(summary(model13)$coefficients[, "Std. Error"][3])^2
          + model3$coefficients[3]/(summary(model14)$coefficients[, "Std. Error"][3])^2 + model3$coefficients[3]/(summary(model15)$coefficients[, "Std. Error"][3])^2
          + model3$coefficients[3]/(summary(model18)$coefficients[, "Std. Error"][3])^2 + model3$coefficients[3]/(summary(model24)$coefficients[, "Std. Error"][3])^2) * meta2.var

meta3.var = 1/(1/(summary(model1)$coefficients[, "Std. Error"][4])^2 + 1/(summary(model2)$coefficients[, "Std. Error"][4])^2
               + 1/(summary(model3)$coefficients[, "Std. Error"][4])^2 + 1/(summary(model4)$coefficients[, "Std. Error"][4])^2
               + 1/(summary(model5)$coefficients[, "Std. Error"][4])^2 +  1/(summary(model6)$coefficients[, "Std. Error"][4])^2
               + 1/(summary(model7)$coefficients[, "Std. Error"][4])^2 + 1/(summary(model8)$coefficients[, "Std. Error"][4])^2
               + 1/(summary(model10)$coefficients[, "Std. Error"][4])^2 + 1/(summary(model13)$coefficients[, "Std. Error"][4])^2
               + 1/(summary(model14)$coefficients[, "Std. Error"][4])^2 + 1/(summary(model15)$coefficients[, "Std. Error"][4])^2
               + 1/(summary(model18)$coefficients[, "Std. Error"][4])^2 + 1/(summary(model24)$coefficients[, "Std. Error"][4])^2)

meta3 =  (model2$coefficients[4]/(summary(model1)$coefficients[, "Std. Error"][4])^2 + model3$coefficients[4]/(summary(model2)$coefficients[, "Std. Error"][4])^2
          + model4$coefficients[4]/(summary(model3)$coefficients[, "Std. Error"][4])^2 + model5$coefficients[4]/(summary(model4)$coefficients[, "Std. Error"][4])^2
          + model8$coefficients[4]/(summary(model5)$coefficients[, "Std. Error"][4])^2 +  model15$coefficients[4]/(summary(model6)$coefficients[, "Std. Error"][4])^2
          + model16$coefficients[4]/(summary(model7)$coefficients[, "Std. Error"][4])^2 + model3$coefficients[4]/(summary(model8)$coefficients[, "Std. Error"][4])^2
          + model3$coefficients[4]/(summary(model10)$coefficients[, "Std. Error"][4])^2 + model3$coefficients[4]/(summary(model13)$coefficients[, "Std. Error"][4])^2
          + model3$coefficients[4]/(summary(model14)$coefficients[, "Std. Error"][4])^2 + model3$coefficients[4]/(summary(model15)$coefficients[, "Std. Error"][4])^2
          + model3$coefficients[4]/(summary(model18)$coefficients[, "Std. Error"][4])^2 + model3$coefficients[4]/(summary(model24)$coefficients[, "Std. Error"][4])^2) * meta0.var

