library(dplyr)
library(ggplot2)
library(car)
library(plotly)
library(corrplot)
library(qgraph)
library(olsrr)
library(caret)
library(lmtest)
library(shiny)
library(tidyverse)
library(shinyjs)

fc24 <-FC24__PLAYER_RATINGS
View(fc24)


fc24<- fc24[grepl("ST", fc24$Position),]



sum(is.na(fc24)) # Kayip Deger Var mi ?
summary(fc24) # Ozet Istatistikler

##############################
# Kesifsel Veri Analizi (EDA)

ortalama_gruplar <- fc24 %>%
  group_by(Age) %>%
  summarise(Ortalama_OVR = mean(OVR))


ggplot(ortalama_gruplar, aes(x = Age, y = Ortalama_OVR)) +
  geom_line() +
  geom_point() +
  labs(title = "Yasa Gore Oyuncunun Guc'lerinin (OVR) Dagilimi",
       x = "Yas",
       y = "Ortalama OVR")

# Genc yaslarda yas ilerledikce oyuncu gelisimi fazla. yas cok ilerledikce oyuncu gelisimi yavasliyor.

############################

sonuc <- fc24 %>%
  group_by(Team) %>%
  summarize(Ortalama_OVR = mean(OVR)) %>%
  arrange(desc(Ortalama_OVR)) %>%
  slice(1:7) 


ggplot(sonuc, aes(x = reorder(Team, Ortalama_OVR), y = Ortalama_OVR)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "En Yuksek Ortalama OVR Degerine Sahip Ilk 7 Takim",
       x = "Team",
       y = "Ortalama OVR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

#############


sonuc <- fc24 %>%
  group_by(Nation) %>%
  summarize(Ortalama_OVR = mean(OVR)) %>%
  arrange(desc(Ortalama_OVR)) %>%
  slice(1:10)  


ggplot(sonuc, aes(x = reorder(Nation, Ortalama_OVR), y = Ortalama_OVR)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "En Yuksek Ortalama OVR Degerine Sahip Ilk 10 Ulke",
       x = "Nation",
       y = "Ortalama OVR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


###########

sonuc <- fc24 %>%
  group_by(League) %>%
  summarize(Ortalama_OVR = mean(OVR)) %>%
  arrange(desc(Ortalama_OVR)) %>%
  slice(1:10)  


ggplot(sonuc, aes(x = reorder(League, Ortalama_OVR), y = Ortalama_OVR)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "En Yuksek Ortalama OVR Degerine Sahip Ilk 10 Lig",
       x = "League",
       y = "Ortalama OVR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#########

#sonuc <- fc24 %>%
#  group_by(Position) %>%
#  summarize(Ortalama_OVR = mean(OVR)) %>%
#  arrange(desc(Ortalama_OVR)) %>%
#  slice(1:10)  


#ggplot(sonuc, aes(x = reorder(Position, Ortalama_OVR), y = Ortalama_OVR)) +
#  geom_bar(stat = "identity", fill = "purple") +
#  labs(title = "En Yuksek Ortalama OVR Degerine Sahip Ilk 10 Pozisyon",
#       x = "Position",
#       y = "Ortalama OVR") +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#######################

# OVR Degiskeninin Dagilimi

plot_ly(x = ~fc24$OVR, type = "histogram", 
        marker = list(color = "skyblue", line = list(color = "black", width = 1))) %>%
  layout(title = "Oyuncu Gucunun (OVR) Dagilimi", 
         xaxis = list(title = "OVR Degerleri"), 
         yaxis = list(title = "Oyuncu Sayisi"))

boxplot(fc24$OVR, main="OVR Boxplot", ylab="OVR Value", col="lightblue", border="black",horizontal = T)


shapiro.test(fc24$OVR)


plot_ly(x = ~log(fc24$OVR), type = "histogram", 
        marker = list(color = "skyblue", line = list(color = "black", width = 1))) %>%
  layout(title = "Oyuncu Gucunun (OVR) Dagilimi", 
         xaxis = list(title = "OVR Degerleri"), 
         yaxis = list(title = "Oyuncu Sayisi"))

shapiro.test(log(fc24$OVR))


# PACE Degiskeninin Dagilimi
plot_ly(x = ~fc24$PACE, type = "histogram", 
        marker = list(color = "pink", line = list(color = "black", width = 1))) %>%
  layout(title = "Oyuncunun Hizinin (PACE) Dagilimi", 
         xaxis = list(title = "PACE Degerleri"), 
         yaxis = list(title = "Oyuncu Sayisi"))

boxplot(fc24$PACE, main="PACE Boxplot", ylab="PACE Value", col="lightblue", border="black",horizontal = T)


# SHOOTING Degiskeninin Dagilimi
plot_ly(x = ~fc24$SHOOTING, type = "histogram", 
        marker = list(color = "pink", line = list(color = "black", width = 1))) %>%
  layout(title = "Oyuncunun Sut Gucunun (SHOOTING) Dagilimi", 
         xaxis = list(title = "SHOOTING Degerleri"), 
         yaxis = list(title = "Oyuncu Sayisi"))

boxplot(fc24$SHOOTING, main="SHOOTING Boxplot", ylab="SHOOTING Value", col="lightblue", border="black",horizontal = T)


# PASSING Degiskeninin Dagilimi
plot_ly(x = ~fc24$PASSING, type = "histogram", 
        marker = list(color = "pink", line = list(color = "black", width = 1))) %>%
  layout(title = "Oyuncunun Pas Gucunun (PASSING) Dagilimi", 
         xaxis = list(title = "PASSING Degerleri"), 
         yaxis = list(title = "Oyuncu Sayisi"))

boxplot(fc24$PASSING, main="PASSING Boxplot", ylab="PASSING Value", col="lightblue", border="black",horizontal = T)



# DRIBBLING Degiskeninin Dagilimi
plot_ly(x = ~fc24$DRIBBLING, type = "histogram", 
        marker = list(color = "pink", line = list(color = "black", width = 1))) %>%
  layout(title = "Oyuncunun Top Surme Gucunun (DRIBBLING) Dagilimi", 
         xaxis = list(title = "DRIBBLING Degerleri"), 
         yaxis = list(title = "Oyuncu Sayisi"))

boxplot(fc24$DRIBBLING, main="DRIBBLING Boxplot", ylab="DRIBBLING Value", col="lightblue", border="black",horizontal = T)


# DEFENDING Degiskeninin Dagilimi
plot_ly(x = ~fc24$DEFENDING, type = "histogram", 
        marker = list(color = "pink", line = list(color = "black", width = 1))) %>%
  layout(title = "Oyuncunun Defans Gucunun (DEFENDING) Dagilimi", 
         xaxis = list(title = "DEFENDING Degerleri"), 
         yaxis = list(title = "Oyuncu Sayisi"))


boxplot(fc24$DEFENDING, main="DEFENDING Boxplot", ylab="DEFENDING Value", col="lightblue", border="black",horizontal = T)


# PHYSICALITY Degiskeninin Dagilimi
plot_ly(x = ~fc24$PHYSICALITY, type = "histogram", 
        marker = list(color = "pink", line = list(color = "black", width = 1))) %>%
  layout(title = "Oyuncunun Fiziksel Gucunun (PHYSICALITY) Dagilimi", 
         xaxis = list(title = "PHYSICALITY Degerleri"), 
         yaxis = list(title = "Oyuncu Sayisi"))

boxplot(fc24$PHYSICALITY, main="PHYSICALITY Boxplot", ylab="PHYSICALITY Value", col="lightblue", border="black",horizontal = T)

#######################

colnames(fc24)

fc24 <- fc24 %>%
  select(-c(1, 9, 12, 13, 14,15)) # Degiskenler Datadan Cikarildi
View(fc24)

head(fc24) # Datanin Ilk Gozlemleri


pairs(fc24[,1:8], pch = 19, col='red', lower.panel = NULL)
# Serpme Diagramlar Uzun Suruyor Kodun Calismayi Bekleyin


# Korelasyon Matrisleri

korelasyon_matrisi = cor(fc24[,1:8])

heatmap(korelasyon_matrisi, 
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Korelasyon Heatmap")


corrplot(korelasyon_matrisi, method = "circle")

qgraph(cor(fc24[,1:8]), layout = "spring", edge.color = "blue")
# Cizgiler ne kadar belirgin ise degiskenlerin birbiri arasindaki iliski o kadar guclu guzel gorseldir.



## MODEL 1 ##

as.factor(fc24$Foot)


regmodel_1 = lm(OVR ~., data=fc24)
summary(regmodel_1)
# Foot Degiskeni Anlamli degil model icin.





plot_ly(x = ~regmodel_1$residuals, type = "histogram", 
        marker = list(color = "pink", line = list(color = "black", width = 1))) %>%
  layout(title = "regmodel_2 Modelinin Residuals Degerlerinin Dagilimi", 
         xaxis = list(title = "Residuals Degerleri"), 
         yaxis = list(title = "Gozlem Sayisi"))

regmodel_1$residuals



shapiro.test(regmodel_1$residuals)
# Normallik Varsayimi Saglaniyor

# Kolmogorov-Smirnov Normallik Testi
ks_test_result <- ks.test(regmodel_1$residuals, "pnorm", mean = mean(regmodel_1$residuals), sd = sd(regmodel_1$residuals))
print(ks_test_result) 


bptest(regmodel_1)
# varyans homojenligi saglanmiyor

par(mfrow=c(2,2))
plot(regmodel_1)

vif(regmodel_1)
# Degiskenler arasi multicollinearity sorunu var gibi ama goz ardi edilebilecek duzeyde
# Bu yuzden vif degeri en yuksek olan DRIBBLING degiskeni modelden cikarilacak
# Veya Regularization yontemlerine gidilebilir (Lasso,Ridge Regression Modelleri)


# DRIBBLING ve Foot degiskeni modelden cikarildi.
# DRIBBLING Vif degeri yuksek oldugu icin, Foot modelde anlamsiz oldugu icin

k=ols_step_all_possible(regmodel_1)
plot(k)

ols_step_best_subset(regmodel_1)

ols_step_both_p(regmodel_1) 


ols_step_backward_p(regmodel_1)
# Algoritma Foot Degiskenini kaldirmamiz gerektigini soyluyor
# Modelde anlamsiz oldugu icin zaten kaldiracagiz.



colnames(fc24)
fc24_new <- fc24 %>%
  select(-c(5,9,10))
# DRIBBLING VE FOOT DEGISKENI CIKARILDI




remove_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_limit <- q1 - 1.5 * iqr
  upper_limit <- q3 + 1.5 * iqr
  x_filtered <- ifelse(x < lower_limit | x > upper_limit, NA, x)
  return(x_filtered)
}


numeric_cols <- sapply(fc24_new, is.numeric)
numeric_cols

find_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_limit <- q1 - 1.5 * iqr
  upper_limit <- q3 + 1.5 * iqr
  outliers <- which(x < lower_limit | x > upper_limit)
  return(outliers)
}


outliers_list <- lapply(fc24_new[numeric_cols], find_outliers)
outliers_list
# Tüm sayisal degişkenler icin aykiri degerleri cikar

for (index in c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 2298, 2299, 2300, 2301,
                51, 101, 172, 417, 435, 491, 586, 602, 678, 693, 728, 777, 778, 783, 831, 848, 930, 933, 951, 967, 992, 1021, 1048,
                1073, 1114, 1116, 1189, 1224, 1267, 1302, 1421, 1438, 1454, 1487, 1559, 1626, 1647, 1649, 1674, 1699, 1748, 1766, 1932, 1972, 1980, 2019,
                2035, 2053, 2108, 2187, 2261, 2269,
                1, 2, 4, 5, 6, 7, 14,
                1, 4, 5, 6, 8, 41, 172,
                8, 20, 65, 78, 163, 172, 180, 205, 233, 242, 348, 417, 437, 649, 906, 965, 1053, 1269, 1858,
                1876, 1934, 1936, 1949, 2190, 2206, 2210, 2219, 2221, 2238, 2258, 2278, 2279, 2295,
                586, 622, 777, 778, 2261)) {
  fc24_new <- fc24_new[-index, ]
}




fc24_new

## Model 2 ##
regmodel_2 = lm(OVR ~., data=fc24_new)
summary(regmodel_2)
vif(regmodel_2)


plot_ly(x = ~regmodel_2$residuals, type = "histogram", 
        marker = list(color = "pink", line = list(color = "black", width = 1))) %>%
  layout(title = "regmodel_2 Modelinin Residuals Degerlerinin Dagilimi", 
         xaxis = list(title = "Residuals Degerleri"), 
         yaxis = list(title = "Gozlem Sayisi"))


shapiro.test(regmodel_2$residuals)
# Normallik Varsayimi Saglaniyor

bptest(regmodel_2)
# Varyans Homojenligi Saglanmiyor


par(mfrow=c(2,2))
plot(regmodel_2)




standardized.residuals<-regmodel_2$residuals/sqrt(var(regmodel_2$residuals))

which(standardized.residuals<(-3))
which(standardized.residuals>(3))


# Residuals outlierlari cikarildi.
fc24_new_final <- fc24_new[-c(which(standardized.residuals<(-3)),which(standardized.residuals>(3))),] 





# Etkin Gozlemler (Cook Distance)
influence.measures(regmodel_2)
which(cooks.distance(regmodel_2)>4/regmodel_2$df.residual) 
influential_observations <- which(cooks.distance(regmodel_2) > 4/regmodel_2$df.residual)

# fc24_new veri çerçevesinden etkin gözlemleri çıkar
fc24_new_final<- fc24_new_final[-influential_observations, ]




#################################################


## Final Model ##

# Train - Test )

# fc24_new_final verisi %70 train datası %30 test datası olarak bolundu
# final modeli train datasi uzerinde egitildi. test datasi uzerinde model tahminleri gerceklestirildi
smp_size <- floor(0.65 * nrow(fc24_new_final))
set.seed(123)
train_ind <- sample(nrow(fc24_new_final), size = smp_size, replace = FALSE)
train_data <- fc24_new_final[train_ind, ]
test_data <- fc24_new_final[-train_ind, ]




regmodel_final = lm(OVR ~., data=train_data)
summary(regmodel_final)



vif(regmodel_final)


plot_ly(x = ~regmodel_final$residuals, type = "histogram", 
        marker = list(color = "pink", line = list(color = "black", width = 1))) %>%
  layout(title = "regmodel_2 Modelinin Residuals Degerlerinin Dagilimi", 
         xaxis = list(title = "Residuals Degerleri"), 
         yaxis = list(title = "Gozlem Sayisi"))


shapiro.test(regmodel_final$residuals)
# Normallik Varsayimi Saglaniyor

bptest(regmodel_final)
# Varyans Homojenligi Saglaniyor.

confint(regmodel_final) #katsayiların guven araliklari


# Test veri seti üzerinde tahmin yapma
predicted_values_test <- predict(regmodel_final, newdata = test_data)

predicted_values_test

rmse <- sqrt(mean((test_data$OVR - predicted_values_test)^2))
print(paste("RMSE:", rmse))

# güven araligi cercevesinde tahmin et


# Tahmin ve güven aralığını hesapla
prediction <- predict(regmodel_final, newdata = test_data, interval = "prediction", level = 0.95)
confidence_interval <- predict(regmodel_final, newdata = test_data, interval = "confidence", level = 0.95)

# Sonuçları yazdır
cat("Tahmin:", prediction[1], "\n")
cat("Güven Aralığı:", confidence_interval[1, 2], "-", confidence_interval[1, 3], "\n")
cat("Kestirim Aralığı:", prediction[1] - confidence_interval[1, 2], "-", prediction[1] + confidence_interval[1, 2], "\n")








# K- FOLD CROSS VALIDATION

train_control <- trainControl(method = "cv", number = 5)

regmodel_final_2 <- train(OVR ~ ., data = train_data, method = "lm",
                          trControl = train_control)


summary(regmodel_final_2)


# Hata Degerleri
regmodel_final_2$results






#########################
 # MODEL ARAYUZU #

fc24_data <- train_data


##############


# UI tanımı
ui <- fluidPage(
  # shinyjs'yi kullanıma al
  shinyjs::useShinyjs(),
  
  # Arka plan resmi eklemek için css stilini belirt
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('https://image.made-in-china.com/2f0j00HLZUdGEJCqoI/All-White-PU-Soccer-Football-Ball-for-Signatures.webp'); /* Resmin URL'sini belirtin */
        background-size: cover;
      }
    "))
  ),
  
  titlePanel("Regresyon Modeli Arayüzü"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("PACE", "Enter PACE value:", value = 0),
      numericInput("SHOOTING", "Enter SHOOTING value:", value = 0),
      numericInput("PASSING", "Enter PASSING value:", value = 0),
      numericInput("DEFENDING", "Enter DEFENDING value:", value = 0),
      numericInput("PHYSICALITY", "Enter PHYSICALITY value:", value = 0),
      numericInput("Age", "Enter Age value:", value = 0),
      actionButton("predict_button", "Predict OVR")
    ),
    mainPanel(
      plotOutput("prediction_plot"),
      textOutput("prediction_output")
    )
  )
)

# Server tanımı
server <- function(input, output) {
  prediction_data <- reactive({
    new_data <- data.frame(
      PACE = input$PACE,
      SHOOTING = input$SHOOTING,
      PASSING = input$PASSING,
      DEFENDING = input$DEFENDING,
      PHYSICALITY = input$PHYSICALITY,
      Age = input$Age
    )
    return(new_data)
  })
  
  output$prediction_output <- renderText({
    new_data <- prediction_data()
    predicted_OVR <- predict(regmodel_final, newdata = new_data)
    paste("Tahmin Edilen Oyuncu Gücü (OVR):", round(predicted_OVR, 2))
  })
}

# Uygulamayı başlatma
shinyApp(ui, server)


