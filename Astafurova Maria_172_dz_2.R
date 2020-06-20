library(devtools)
library(rio) # импорт-экспорт данных
library(tidyverse) # манипуляции с данными, графики
library(dplyr) # манипуляции с данными
library(skimr) # описательные статистики
library(GGally) # диаграммы рассеивания
library(vsd) # для графиков с качественными переменными (ф-ция mosaic)
library(memisc)
library(lmtest)
library(sjPlot)
library(sgof)
library(hexbin)
library(foreign)
library(car) #не устанавился пакет
library(broom)
library(sandwich)
library(forecast)
library(quantmod)
library(sophisthse)
library(lubridate)
library(zoo)
library(xts)

m = import("C:/Users/Rain/Downloads/datasets_14446_19502_country_profile_variables.csv")
glimpse(m)
print(m)

head(m)
tail(m)
ncol(m) # кол-во переменных (столбцов)
nrow(m) # кол-во наблюдений (строк)
str(m) # кол-во наблюдений, переменных и тип переменных

#Задание 1.

#Пункты 1 и 2:
# Я хочу изучить влияние экономического развития общества на уровень рождаемости в данном обществе. Для этого в качестве объясняемой переменной я использую ln(Fertility rate). Многие исследователи, например M. Doepke, M. Tertilt (2016) и HaLeea, Lim, Hwang (2012) доказывают, что существует устойчивая взаимосвязь структуры семьи и эконмического развития. 
# Согласно подходу Беккера (Gary S. Becker " An Economic Analysis of Fertility"), рождение ребенка можно рассматривать как инвестиционное решение в терминах альтернативных издержек. Другими словами, женщина анализирует явные и неявные затраты на ребенка, в том числе и свое время. Логично предположить, что с ростом доходов растут и альтернативные издержки рождения ребенка. Соответсвенно, чем богаче и равноправнее общество, тем меньше детей приходится на одну семью. В качестве зависимой переменной я беру ВВП на душу населения,которая будет отражать сферу "богатства". В качестве контрольных - уровень младенческой смертности, кол-во врачей на 1000 чел. (сфера "здоровье") и кол-во женских мест в парламенте (сфера "равноправие"), кол-во занятых в сельском хозяйстве (сфера "развитость").

names(m)[27] <- "fertility"
names(m)[9] <- "GDP_cap" #сделаем логарифм
names(m)[32] <- "inf_mort" # непрерывная
names(m)[39] <- "women_parlam" #сделала бинарную
names(m)[13] <- "agricult" #непрерывная

print(m$fertility)
print(m$inf_mort)
print(m$agricult)


inf_mort_num <- as.numeric(m$inf_mort)
fertility_num <-as.numeric(m$fertility)
women_parlam_num <-as.numeric(m$women_parlam)
agric_num <-as.numeric(m$agricult)
print(inf_mort_num)
print(physic_num)
print(fertility_num)
print(women_parlam_num)
print(agric_num)

#создаем бинарную для кол-ва женщин в парламенте
women_parlam_num[women_parlam_num == -99.0] <- NA
print(women_parlam_num)
mean(women_parlam_num)
median(women_parlam_num)
women_dummy <- ifelse(women_parlam_num >= 15.0, 1, 0)
table(women_dummy)
print(women_dummy)

#Пункт 3
#3(a) выбросы:
colnames(m) <- make.unique(names(m))

m$fertility = fertility_num
m$agricult = agric_num
m$women_parlam = women_parlam_num
m$inf_mort =inf_mort_num

ggplot(data = m, aes(y = fertility, x = GDP_cap))+geom_boxplot(fill='powderblue', width=0.2)+labs(title = 'Диаграмма размаха для уровня рождаемости', x='ВВП на душу населения (в $)', y='Уровень рождаемости (кол-во детей на 1 женщину)')
ggplot(data = m, aes(x = fertility, y = GDP_cap))+geom_boxplot(fill='powderblue', width=0.2)+labs(title = 'Диаграмма размаха для ВВП', x='Уровень рождаемости (кол-во детей на 1 женщину)', y= 'ВВП на душу населения (в $)')
ggplot(data = m, aes(x = fertility, y = agricult))+geom_boxplot(fill='powderblue', width=0.2)+labs(title = 'Диаграмма размаха для занятых с с/х', x='Уровень рождаемости (кол-во детей на 1 женщину)', y='Занятые в сельском хозяйстве (в % от общего числа занятых)')
ggplot(data = m, aes(x = fertility, y = women_parlam))+geom_boxplot(fill='powderblue', width=0.2)+labs(title = 'Диаграмма размаха для женских мест в парламенте', x='Уровень рождаемости (кол-во детей на 1 женщину)', y='Количество женщин в парламенте (в % от общего числа парламентских мест)')
ggplot(data = m, aes(x = fertility, y = inf_mort))+geom_boxplot(fill='powderblue', width=0.2)+ labs(title = 'Диаграмма размаха для уровня детской смертности', x= 'Уровень рождаемости (кол-во детей на 1 женщину)', y='Уровень детской смертности на 1000 рожденных (в ед.)')

# Все наблюдения, которые выходят за рамки "усов" можно классфицировать как выбросы.

#3(б) пропущенные значения:
hist(fertility_num, breaks=100, col="powderblue", xlab = "Уровень рождаемости (кол-во детей на 1 женщину)", ylab = "Частота повторений", main = "Распределение уровня рождаемости")
hist(m$GDP_cap, breaks=100, col="powderblue", xlab = "ВВП на душу населения (в $)", ylab = "Частота повторений", main = "Распределение ВВП на душу населения")
hist(agric_num, breaks=100, col="powderblue", xlab = "Занятые в сельском хозяйстве (в % от общего числа занятых)", ylab = "Частота повторений", main = "Распределение занятых в с/х")
hist(women_parlam_num, breaks=100, col="powderblue", xlab = "Количество женщин в парламенте (в % от общего числа парламентских мест)", ylab = "Частота повторений", main = "Распределение женских мест в Парламенте")
hist(inf_mort_num, breaks=100, col="powderblue", xlab = "Уровень детской смертности на 1000 рожденных (в ед.)", ylab = "Частота повторений", main = "Распределение уровня детской смертности")

#На всех вышепредставленных графиках видно, что значения равные -99 являются в действительности пропущенными значениями. Они нарушают структуру взаимосвязи, поэтому для всех переменных я удалю пропуски.Для бинарной переменной я присвоила NA на шаг раньше.
fertility_num[fertility_num == -99.0] <- NA
m$GDP_cap[m$GDP_cap == -99.0] <- NA
agric_num[agric_num == -99.0] <- NA
inf_mort_num[inf_mort_num == -99.0] <- NA

k <- data.frame(fertility_num, m$GDP_cap, agric_num, inf_mort_num, women_dummy)
print(k)
k_itog <- na.omit(k)
print(k_itog)


#Пункт 4. Спецификация и проверка.
#y будет специфицирован как log(fertility_num), поскольку при данном преобразовании зависимость уровня рождаемости и ВВП похожа на линейную.
qplot(log(fertility_num), log(m$GDP_cap), xlab = "Уровень рождаемости (кол-во детей на 1 женщину)", ylab = "ВВП на душу населения (в $)" )

fert_itog <- k$fertility_num
GDP_cup_itog <- k$m.GDP_cap
agric_itog <- k$agric_num
inf_mort_itog <- k$inf_mort_num
women_itog <- k$women_dummy

model <- lm(log(fert_itog) ~ log(GDP_cup_itog) + agric_itog + inf_mort_itog + women_itog)

#4(a)
car::vif(model) #пакет car не устанавливается, не могу посмотреть результаты
cor(k_itog)
#Тест показал высокую корреляцию между показателями "детская смертность" и "доля с/х населения". Корреляция = 0.7853211. Следовательно, мультиколлинеарность присутствует. При мультиколлинеарности получаются высокие стандартные оценки коэффициентов и широкие доверительные интервалы.Используется лассо и ридж регрессия.

#4(б)
bptest(model)
#BP = 9.242, df = 4, p-value = 0.05533. Значит гипотеза H0 об условной гомоскедастичности не отвергается при альфа < 5.5% 
gqtest(model, fraction = 0.2)
#GQ = 1.9886, df1 = 66, df2 = 65, p-value = 0.003056.  Значит гипотеза H0 о гомоскедастичности отвергается при альфа > 0.3%. Значит, в модели присуствует гетероскедастичность согласно тесту Г-К.
#При гетероскедастичности оценки несмещенные, линейные по у, но не эффективные. А оценки стандартных отклонений несостоятельны. При использовании стандартных формул t-статистики не имеют t-распределения. В случае гетероскедастичности применяются оценки Уайта (устойчивые стандартные ошибки).

#4(c)
lmfit=lm(log(fert_itog) ~ log(GDP_cup_itog) + agric_itog + inf_mort_itog + women_itog)
resettest(lmfit, type="regressor", data=k_itog)
#RESET = 4.5737, df1 = 8, df2 = 164, p-value = 4.76e-05

#Согласно Reset тесту в данных есть эндогенность. При эндогенности оценки МНК несостоятельны и смещены. Чтобы "побороться" с эндогенностью, применяют метод инструментальных переменных.

#Пункт 5

model <- lm(log(fert_itog) ~ log(GDP_cup_itog) + agric_itog + inf_mort_itog + women_itog)
summary(model)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        0.7933214  0.2620907   3.027  0.00285 ** 
#log(GDP_cup_itog) -0.0326875  0.0260882  -1.253  0.21192    
#agric_itog         0.0002334  0.0015525   0.150  0.88067    
#inf_mort_itog      0.0157013  0.0012809  12.258  < 2e-16 ***
#women_itog         0.0062918  0.0381711   0.165  0.86927 

vcov(model)

k_itog <- augment(model, k_itog)
glimpse(k_itog)
vcovHC(model)


#Задание 2.
#Пункт 1

m1 <- arima.sim (n = 120, list(ar = 0.8)) # AR(1)
m2 <- arima.sim (n = 120, list(ar = c(0.1,0.2,0.3))) # AR(3)
m3 <- arima.sim (n = 120, list(ma = c(1.2,2))) # MA(2) 
tsdisplay(m1) # Процесс стационарен.
tsdisplay(m2) # Процесс стационарен.
tsdisplay(m3) # Процесс стационарен.
#На всез 3-х графиках наблюдаем быстро убывающие автокорреляционные функции, а это признак стационарности. Также у AR процессов корни характеристических многочленов < |1|

#Пункт 2

m4 <- arima.sim (n = 120, list(order = c(0,1,2), ma = c(0.6,-0.1)))
tsdisplay(m4) # Процесс нестационарен, т.к. график автокорреляционной функции убывает медленно.
m5 <- arima.sim (n = 120, list(order = c(0,0,0)))
tsdisplay(m5) # Процесс стационарен
m6 <- arima.sim ( n= 120, list(order = c(3,0,0), ar = c(0.5,0.2,0.25)))
tsdisplay(m6) # Процесс нестационарен

#Пункт 3

a <- arima.sim (n = 120, list (order = c(0,1,0)))
tsdisplay(a) 
# Процесс случайного блуждания по определению не стационарен.

#Пункт 4

m1 <- arima.sim (n = 120, list(ar = 0.8))
tsdisplay(m1) # AR(1) стационарный процесс. ACF высокая
m2 <- arima.sim (n = 120, list (order = c(0,1,0)))
tsdisplay(m2) # Процесс случайного блуждания, нестационарен по определению.
#По графику PACF сложно определить стационарность процесса.



