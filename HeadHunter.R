###### Download libraries
library(rvest)
library(purrr)
library(readr)
library(stringr)
library(RCurl)
library(curl)
library(XML)  
library(doMC)
library(rio)

system.time(d <- download.file( url = "https://github.com/Dmitryi/statistic/archive/master.zip", destfile = "statistic.zip"))
file.info("statistic.zip")
unzip(zipfile = "statistic.zip")
setwd("/Users/statistic-master")
list.files()


# Удалим не нужные файлы и папки
files.to.delete <- dir("/Users/statistic-master", pattern = ".*", recursive=F, full.names=T)
file.remove(files.to.delete)
unlink(c("css", "fonts", "img", "js"), recursive = T)

# Переходим в папку с данными
setwd("/Users/statistic-master/data")
list.files()



# Этап 2. Подготовим данные для вычислений
# Извлечём необходимые данные и создадим таблицы и списки по количеству вакансий/резюме и уровню заработной платы
# Количество и сумма вакансий/резюме по каждом региону отдельно
banki <- read.csv("databanki.csv", header=T, sep=",")[905:1346, 2:13]  # 2017 06 10
buhgalter <- read.csv("databuh.csv", header=T, sep=",")[905:1346,2:13]
bulding <- read.csv("databulding.csv", header=T, sep=",")[905:1346,2:13]
consult <- read.csv("dataconsult.csv", header=T, sep=",")[905:1346,2:13]
culture <-read.csv("dataculture.csv", header=T, sep=",")[905:1346,2:13]
admin <-read.csv("datadmin.csv", header=T, sep=",")[905:1346,2:13]


gos <-read.csv("datagos.csv", header=T, sep=",")[905:1346,2:13]
house <-read.csv("datahouse.csv", header=T, sep=",")[905:1346,2:13]
hr <-read.csv("datahr.csv", header=T, sep=",")[905:1346,2:13]
instal <-read.csv("datainstal.csv", header=T, sep=",")[905:1346,2:13]
it <-read.csv("datait.csv", header=T, sep=",")[905:1346,2:13]
logist <- read.csv("datalogist.csv", header=T, sep=",")[905:1346,2:13]
market <-read.csv("datamarket.csv", header=T, sep=",")[905:1346,2:13]
medicina <-read.csv("datamedicina.csv", header=T, sep=",")[905:1346,2:13]
nauka <-read.csv("datanauka.csv", header=T, sep=",")[905:1346,2:13]
proizvotstvo <- read.csv("dataproizvotstvo.csv", header=T, sep=",")[905:1346,2:13]
rab <-read.csv("datarab.csv", header=T, sep=",")[905:1346,2:13]
sales <- read.csv("datasales.csv", header=T, sep=",")[905:1346,2:13]
secure <-read.csv("datasecure.csv", header=T, sep=",")[905:1346,2:13]
sport <-  read.csv("datasport.csv", header=T, sep=",")[905:1346,2:13]
strahovanie <-  read.csv("datastrahovanie.csv", header=T, sep=",")[905:1346,2:13]
study <- read.csv("datastudy.csv", header=T, sep=",")[905:1346,2:13]
tek <- read.csv("datatek.csv", header=T, sep=",")[905:1346,2:13]
top <- read.csv("datatop.csv", header=T, sep=",")[905:1346,2:13]
turizm  <-read.csv("dataturizm.csv", header=T, sep=",")[905:1346,2:13]
urist <-  read.csv("dataurist.csv", header=T, sep=",")[905:1346,2:13]
auto <-  read.csv("datauto.csv", header=T, sep=",")[905:1346,2:13]
zakupki <-read.csv("datazakupki.csv", header=T, sep=",")[905:1346,2:13]

# Обьедим данные в один data frame (таблицу)
data_vac_res <- (banki + buhgalter + bulding + consult + culture + admin + gos + house + hr + instal + it + logist + market + medicina + nauka + proizvotstvo + rab + sales + secure + sport + strahovanie + study + tek + top + turizm + urist + auto + zakupki)

# Списки (матрицы), для каждого региона отдельно, количество вакансий/резюме по всем отраслям
data_vac_moscow <- rbind(banki$Вакансии.Москва, buhgalter$Вакансии.Москва, bulding$Вакансии.Москва, consult$Вакансии.Москва, culture$Вакансии.Москва, admin$Вакансии.Москва, gos$Вакансии.Москва, house$Вакансии.Москва, hr$Вакансии.Москва, instal$Вакансии.Москва, it$Вакансии.Москва, logist$Вакансии.Москва, market$Вакансии.Москва, medicina$Вакансии.Москва, nauka$Вакансии.Москва, proizvotstvo$Вакансии.Москва, rab$Вакансии.Москва, sales$Вакансии.Москва, secure$Вакансии.Москва, sport$Вакансии.Москва, strahovanie$Вакансии.Москва, study$Вакансии.Москва, tek$Вакансии.Москва, top$Вакансии.Москва, turizm$Вакансии.Москва, urist$Вакансии.Москва, auto$Вакансии.Москва, zakupki$Вакансии.Москва)
rownames(data_vac_moscow) <- c("banki","buhgalter","bulding","consult","culture","admin","gos","house","hr","instal","it","logist","market","medicina", "nauka","proizvotstvo","rab","sales","secure","sport","strahovanie","study","tek","top","turizm","urist","auto","zakupki")
data_vac_spb <- rbind(banki$Вакансии.Санкт.Петербург, buhgalter$Вакансии.Санкт.Петербург, bulding$Вакансии.Санкт.Петербург, consult$Вакансии.Санкт.Петербург, culture$Вакансии.Санкт.Петербург, admin$Вакансии.Санкт.Петербург, gos$Вакансии.Санкт.Петербург, house$Вакансии.Санкт.Петербург, hr$Вакансии.Санкт.Петербург, instal$Вакансии.Санкт.Петербург, it$Вакансии.Санкт.Петербург, logist$Вакансии.Санкт.Петербург, market$Вакансии.Санкт.Петербург, medicina$Вакансии.Санкт.Петербург, nauka$Вакансии.Санкт.Петербург, proizvotstvo$Вакансии.Санкт.Петербург, rab$Вакансии.Санкт.Петербург, sales$Вакансии.Санкт.Петербург, secure$Вакансии.Санкт.Петербург, sport$Вакансии.Санкт.Петербург, strahovanie$Вакансии.Санкт.Петербург, study$Вакансии.Санкт.Петербург, tek$Вакансии.Санкт.Петербург, top$Вакансии.Санкт.Петербург, turizm$Вакансии.Санкт.Петербург, urist$Вакансии.Санкт.Петербург, auto$Вакансии.Санкт.Петербург, zakupki$Вакансии.Санкт.Петербург)
rownames(data_vac_spb) <- c("banki","buhgalter","bulding","consult","culture","admin","gos","house","hr","instal","it","logist","market","medicina", "nauka","proizvotstvo","rab","sales","secure","sport","strahovanie","study","tek","top","turizm","urist","auto","zakupki")
data_vac_novosib <- rbind(banki$Вакансии.Новосибирск, buhgalter$Вакансии.Новосибирск, bulding$Вакансии.Новосибирск, consult$Вакансии.Новосибирск, culture$Вакансии.Новосибирск, admin$Вакансии.Новосибирск, gos$Вакансии.Новосибирск, house$Вакансии.Новосибирск, hr$Вакансии.Новосибирск, instal$Вакансии.Новосибирск, it$Вакансии.Новосибирск, logist$Вакансии.Новосибирск, market$Вакансии.Новосибирск, medicina$Вакансии.Новосибирск, nauka$Вакансии.Новосибирск, proizvotstvo$Вакансии.Новосибирск, rab$Вакансии.Новосибирск, sales$Вакансии.Новосибирск, secure$Вакансии.Новосибирск, sport$Вакансии.Новосибирск, strahovanie$Вакансии.Новосибирск, study$Вакансии.Новосибирск, tek$Вакансии.Новосибирск, top$Вакансии.Новосибирск, turizm$Вакансии.Новосибирск, urist$Вакансии.Новосибирск, auto$Вакансии.Новосибирск, zakupki$Вакансии.Новосибирск)
rownames(data_vac_novosib) <- c("banki","buhgalter","bulding","consult","culture","admin","gos","house","hr","instal","it","logist","market","medicina", "nauka","proizvotstvo","rab","sales","secure","sport","strahovanie","study","tek","top","turizm","urist","auto","zakupki")



data_vac_tatarstan <- rbind(banki$Вакансии.Татарстан, buhgalter$Вакансии.Татарстан, bulding$Вакансии.Татарстан, consult$Вакансии.Татарстан, culture$Вакансии.Татарстан, admin$Вакансии.Татарстан, gos$Вакансии.Татарстан, house$Вакансии.Татарстан, hr$Вакансии.Татарстан, instal$Вакансии.Татарстан, it$Вакансии.Татарстан, logist$Вакансии.Татарстан, market$Вакансии.Татарстан, medicina$Вакансии.Татарстан, nauka$Вакансии.Татарстан, proizvotstvo$Вакансии.Татарстан, rab$Вакансии.Татарстан, sales$Вакансии.Татарстан, secure$Вакансии.Татарстан, sport$Вакансии.Татарстан, strahovanie$Вакансии.Татарстан, study$Вакансии.Татарстан, tek$Вакансии.Татарстан, top$Вакансии.Татарстан, turizm$Вакансии.Татарстан, urist$Вакансии.Татарстан, auto$Вакансии.Татарстан, zakupki$Вакансии.Татарстан)
rownames(data_vac_tatarstan) <- c("banki","buhgalter","bulding","consult","culture","admin","gos","house","hr","instal","it","logist","market","medicina", "nauka","proizvotstvo","rab","sales","secure","sport","strahovanie","study","tek","top","turizm","urist","auto","zakupki")





# Данные по уровню заработной платы в вакансиях

tatarstan_v <- read.csv("datazpvachhtatarstan.csv", header = T, sep = ",")[,2:16]

# Расчитаем разницу между цифрами, построчно, и получим истинные данные о количестве вакансий
dif <- diff(t(tatarstan_v))
от20000 <- as.integer(gsub("-", "", dif[1,]))
от30000 <- as.integer(gsub("-", "", dif[2,]))
от40000 <- as.integer(gsub("-", "", dif[3,]))
от50000 <- as.integer(gsub("-", "", dif[4,]))
от60000 <- as.integer(gsub("-", "", dif[5,]))
от70000 <- as.integer(gsub("-", "", dif[6,]))
от80000 <- as.integer(gsub("-", "", dif[7,]))
от90000 <- as.integer(gsub("-", "", dif[8,]))
от100000 <- as.integer(gsub("-", "", dif[9,]))
от110000 <- as.integer(gsub("-", "", dif[10,]))
от120000 <- as.integer(gsub("-", "", dif[11,]))
от130000 <- as.integer(gsub("-", "", dif[12,]))
от140000 <- as.integer(gsub("-", "", dif[13,]))
от150000 <- as.integer(gsub("-", "", dif[14,]))


tatarstan_v <- data.frame(от20000, от30000, от40000, от50000, от60000, от70000, от80000, от90000, от100000, от110000, от120000, от130000, от140000, от150000)

# Данные по уровню заработной платы в резюме
tatarstan_r <- read.csv("datazpreshhtatarstan.csv", header = T, sep = ",")[,2:17]


tatar_vacancies <- cbind(banki$Вакансии.Татарстан, buhgalter$Вакансии.Татарстан, admin$Вакансии.Татарстан, 
                         auto$Вакансии.Татарстан, bulding$Вакансии.Татарстан, 
                         consult$Вакансии.Татарстан, culture$Вакансии.Татарстан, 
                         gos$Вакансии.Татарстан, house$Вакансии.Татарстан, 
                         hr$Вакансии.Татарстан, instal$Вакансии.Татарстан, 
                         it$Вакансии.Татарстан, logist$Вакансии.Татарстан, 
                         market$Вакансии.Татарстан, medicina$Вакансии.Татарстан, 
                         nauka$Вакансии.Татарстан, proizvotstvo$Вакансии.Татарстан, 
                         rab$Вакансии.Татарстан, sales$Вакансии.Татарстан, 
                         secure$Вакансии.Татарстан, sport$Вакансии.Татарстан, 
                         strahovanie$Вакансии.Татарстан, study$Вакансии.Татарстан, 
                           tek$Вакансии.Татарстан, top$Вакансии.Татарстан, 
                         turizm$Вакансии.Татарстан, urist$Вакансии.Татарстан, 
                         zakupki$Вакансии.Татарстан)
tatar_vacancies <- as.data.frame(tatar_vacancies)
colnames(tatar_vacancies) <- c("Банки","Бухгалтерия", "Административный персонал", "Автомобильный бизнес", "Строительство",
                               "Консультирование", "Культура", "Госслужба", "Домашний персонал", 
                               "Управление персоналом", "Инсталляция и сервис", "ИТ", "Логистика",
                               "Маркетинг", "Медицина", "Наука", "Производство", "Рабочий персонал", "Продажи",
                               "Безопасность", "Спорт", "Страхование", "Начало карьеры", "ТЭК", "Топ-менеджмент", 
                               "Туризм", "Юристы", "Закупки") 


####### Боксплот по всем профессиям в РТ
chart.Boxplot(tatar_vacancies, main = "Кого чаще всего ищут в Татарстане?", xlab = "Кол-во вакансий", mean.symbol = 16)



########## Теперь изучаем резюме
tatar_resumes <- cbind(banki$Резюме.Татарстан, buhgalter$Резюме.Татарстан, admin$Резюме.Татарстан, 
                         auto$Резюме.Татарстан, bulding$Резюме.Татарстан, 
                         consult$Резюме.Татарстан, culture$Резюме.Татарстан, 
                         gos$Резюме.Татарстан, house$Резюме.Татарстан, 
                         hr$Резюме.Татарстан, instal$Резюме.Татарстан, 
                         it$Резюме.Татарстан, logist$Резюме.Татарстан, 
                         market$Резюме.Татарстан, medicina$Резюме.Татарстан, 
                         nauka$Резюме.Татарстан, proizvotstvo$Резюме.Татарстан, 
                         rab$Резюме.Татарстан, sales$Резюме.Татарстан, 
                         secure$Резюме.Татарстан, sport$Резюме.Татарстан, 
                         strahovanie$Резюме.Татарстан, study$Резюме.Татарстан, 
                         tek$Резюме.Татарстан, top$Резюме.Татарстан, 
                         turizm$Резюме.Татарстан, urist$Резюме.Татарстан, 
                         zakupki$Резюме.Татарстан)
tatar_resumes <- as.data.frame(tatar_resumes)
colnames(tatar_resumes) <- c("Банки","Бухгалтерия", "Административный персонал", "Автомобильный бизнес", "Строительство",
                               "Консультирование", "Культура", "Госслужба", "Домашний персонал", 
                               "Управление персоналом", "Инсталляция и сервис", "ИТ", "Логистика",
                               "Маркетинг", "Медицина", "Наука", "Производство", "Рабочий персонал", "Продажи",
                               "Безопасность", "Спорт", "Страхование", "Начало карьеры", "ТЭК", "Топ-менеджмент", 
                               "Туризм", "Юристы", "Закупки") 

chart.Boxplot(tatar_resumes, main = "Кто чаще всего предлагает свои услуги на рынке труда в Татарстане?", xlab = "Кол-во резюме", mean.symbol = 16)


#Salaries
chart.Boxplot(tatarstan_v, main = "Какие заработные платы предлагаются в Татарстане?", xlab = "Кол-во вакансий", mean.symbol = 16)

colnames(tatarstan_r) <- c("до 9999 руб", "10000-19999 руб", "20000-29999 руб", 
                           "30000-39999 руб", "40000-49999 руб", "50000-59999 руб",
                           "60000-69999 руб", "70000-79999 руб", "80000-89999 руб",
                           "90000-99999 руб", "100000-109999 руб", "110000-119999 руб",
                           "120000-129999 руб","130000-139999 руб", "140000-149999 руб",
                          "от 150000 руб") 
chart.Boxplot(tatarstan_r, main = "Какие заработные платы хотят соискатели в Татарстане?", xlab = "Кол-во резюме", mean.symbol = 16)
