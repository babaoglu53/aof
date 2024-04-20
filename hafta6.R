#i) Derste verilen örnekler üzerinden gerçekleştirilen aşamalar

install.packages("MASS")
install.packages("corrplot")
library(MASS)

# Veri setini oluşturma
set.seed(125) # Tekrarlanabilirlik için seed belirleme
n <- 150 # Gözlem sayımızı belirliyoruz
x1 <- rnorm(n, mean = 15, sd = 3) # Bağımsız değişken 1
x2 <- rnorm(n, mean = 7, sd = 1.75) # Bağımsız değişken 2
x3 <- rnorm(n, mean = 8, sd = 0.4) # Bağımsız değişken 3
x4 <- rnorm(n, mean = 6, sd = 1.5) # Bağımsız değişken 4

y <- 4 + 3*x1 - 2.5*x2 + 1.5*x3 - 0.8*x4 + rnorm(n, mean = 0, sd = 1.5) # Bağımlı değişken

# Oluşturulan veri setini bir veri çerçevesine dönüştürme
data <- data.frame(y, x1, x2, x3, x4)

# Oluşturulan veri setini gösterme
head(data)

#---
#Aktarım ve Gerekli ise Dönüşüm
# Örnek veri setini oluşturma
set.seed(125)
n <- 100
x1_f <- rnorm(n, mean = 8, sd = 2)
x2_f <- rnorm(n, mean = 4, sd = 1)
y_f <- rnorm(n, mean = 2 + 3*x1 - 1.5*x2, sd = 2)

# x1 değişkenini faktörel değişkene dönüştürme
x1_f_factor <- as.factor(x1_f)

# x2 değişkenini sayısal değişkene dönüştürme
x2_f_numeric <- as.numeric(x2_f)

# Değişken türlerini kontrol etme
str(x1_f_factor)
str(x2_f_numeric)

set.seed(125)
n <- 100
x1_ev <- rnorm(n, mean = 10, sd = 2)
x2_ev <- rnorm(n, mean = 5, sd = 1)
y_ev <- rnorm(n, mean = 3 + 2*x1 - 1.5*x2, sd = 2)

# Eksik veri tespiti
is_na_x1 <- is.na(x1_ev)
is_na_x2 <- is.na(x2_ev)


# Eksik verileri medyan ile doldurma
median_y_ev <- median(y_ev, na.rm = TRUE)
y_filled <- ifelse(is.na(y_ev), median_y_ev, y_ev)


#aykırı değer tespiti
# Örnek veri setini oluşturma
set.seed(125)
data <- rnorm(100)

# Kutu grafiği oluşturma
boxplot(data)
sort(data)


# Z-puanı hesaplama
z_scores <- scale(data)

# Aykırı değerleri belirleme
outliers <- abs(z_scores) > 3

# Alt ve üst çeyreklikleri hesaplama
Q1 <- quantile(data, 0.25)
Q3 <- quantile(data, 0.75)

# Alt ve üst sınırı hesaplama
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Aykırı değerleri belirleme
outliers <- data < lower_bound | data > upper_bound

#---
#dağılımları keşif
# Örnek veri setini oluşturma
set.seed(125)
data <- rnorm(100)

# Histogram oluşturma
hist(data)

# Kutu grafiği oluşturma
boxplot(data)

# Q-Q plot oluşturma
qqnorm(data)
qqline(data)

# Kantillerden yararlanma
summary(data)

# Örnek veri setini oluşturma
set.seed(125)
x <- rnorm(100) # Bağımsız değişken
y <- 2*x + rnorm(100, mean = 0, sd = 0.5) # Bağımlı değişken

# Korelasyon katsayısını hesaplama
correlation <- cor(x, y)

# Korelasyon katsayısını ekrana yazdırma
print(correlation)


#multicollinearity
# Örnek veri setini oluşturma
set.seed(125)
x1 <- rnorm(100) # Bağımsız değişken 1
x2 <- rnorm(100) # Bağımsız değişken 2
x3 <- rnorm(100) # Bağımsız değişken 3
y <- 2*x1 + 3*x2 - 1.5*x3 + rnorm(100, mean = 0, sd = 0.5) # Bağımlı değişken

# Çoklu doğrusal regresyon modelini oluşturma
model <- lm(y ~ x1 + x2 + x3)

# Model özetini alma
summary(model)

# Bağımsız değişkenler arasındaki korelasyonu hesaplama
correlation_matrix <- cor(data.frame(x1, x2, x3))
print(correlation_matrix)
# corrplot paketini yükleme
library(corrplot)
# Korelasyon matrisini görselleştirme
corrplot(correlation_matrix, method = "color")


# Bağımsız değişkenlerin grafiğini çizme
par(mfrow=c(1,3)) # Grafiklerin yan yana yerleştirilmesi için ayar
plot(x1, y, main = "x1 vs. y", xlab = "x1", ylab = "y", col = "blue", pch = 16)
plot(x2, y, main = "x2 vs. y", xlab = "x2", ylab = "y", col = "red", pch = 16)
plot(x3, y, main = "x3 vs. y", xlab = "x3", ylab = "y", col = "green", pch = 16)


# Örnek veri setini oluşturma
set.seed(125)
x1 <- rnorm(100) # Bağımsız değişken 1
x2 <- rnorm(100) # Bağımsız değişken 2
x3 <- rnorm(100) # Bağımsız değişken 3
y <- 2*x1 + 3*x2 - 1.5*x3 + rnorm(100, mean = 0, sd = 0.5) # Bağımlı değişken

# Veriyi standartlaştırma
x1_standardized <- scale(x1)
x2_standardized <- scale(x2)
x3_standardized <- scale(x3)
y_standardized <- scale(y)

# Standartlaştırılmış veriyi kontrol etme
summary(x1_standardized)
summary(x2_standardized)
summary(x3_standardized)
summary(y_standardized)

#---

#---
#kategorik keşif
#---

#---
#son değişiklik
# caTools paketini yükleme
install.packages("caTools")
library(caTools)

# Veri setini oluşturma (örnek veri)
set.seed(125)
x1 <- rnorm(100) # Bağımsız değişken 1
x2 <- rnorm(100) # Bağımsız değişken 2
x3 <- rnorm(100) # Bağımsız değişken 3
y <- 2*x1 + 3*x2 - 1.5*x3 + rnorm(100, mean = 0, sd = 0.5) # Bağımlı değişken

# Veriyi test ve eğitim alt kümelerine böleme
split <- sample.split(y, SplitRatio = 0.7) # 70% eğitim, 30% test
train_data <- subset(data.frame(x1, x2, x3, y), split == TRUE)
test_data <- subset(data.frame(x1, x2, x3, y), split == FALSE)

# Verinin boyutlarını kontrol etme
dim(train_data)
dim(test_data)

#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#

#ii) mtcars verisetini kullanarak gerçekleştirilen aşamalar

# mtcars veri setini yükleme
data(mtcars)

# Veri setinin yapısı
str(mtcars)

# Özet istatistikler
summary(mtcars)

# Görselleştirmeler
# Histogram oluşturma
hist(mtcars$mpg)

# Kutu grafiği oluşturma
boxplot(mtcars$mpg)

# Q-Q plot oluşturma
qqnorm(mtcars$mpg)
qqline(mtcars$mpg)

# Bağımlı değişkenlerin grafiğini çizme
par(mfrow=c(1,3))
plot(mtcars$mpg, mtcars$disp, main = "mpg vs. disp", xlab = "MPG", ylab = "Displacement", col = "blue", pch = 16)
plot(mtcars$mpg, mtcars$hp, main = "mpg vs. hp", xlab = "MPG", ylab = "Horsepower", col = "red", pch = 16)
plot(mtcars$mpg, mtcars$wt, main = "mpg vs. wt", xlab = "MPG", ylab = "Weight", col = "green", pch = 16)

# Korelasyon katsayısını hesaplama
correlation <- cor(mtcars$mpg, mtcars$disp)

# Korelasyon katsayısını ekrana yazdırma
print(correlation)

# Çoklu doğrusal regresyon modelini oluşturma
model <- lm(mpg ~ disp + hp + wt, data = mtcars)

# Model özetini alma
summary(model)

# Bağımsız değişkenler arasındaki korelasyonu hesaplama
correlation_matrix <- cor(mtcars[c("disp", "hp", "wt")])
print(correlation_matrix)

# corrplot paketini yükleme
install.packages("corrplot")
library(corrplot)

# Korelasyon matrisini görselleştirme
corrplot(correlation_matrix, method = "color")

install.packages("caTools")
library(caTools)

# Veriyi test ve eğitim alt kümelerine böleme
set.seed(125)
split <- sample.split(mtcars$mpg, SplitRatio = 0.7) # 70% eğitim, 30% test
train_data <- subset(mtcars, split == TRUE)
test_data <- subset(mtcars, split == FALSE)

# Verinin boyutlarını kontrol etme
dim(train_data)
dim(test_data)


