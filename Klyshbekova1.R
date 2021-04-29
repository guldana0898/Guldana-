#Клышбекова Г.K. ПАЭ 124
#для 23 региона рассчитайте урожайность пшеницы в 2004 году, взяв для расчета средние суммы активных температур за текущий год, с 19 ближайших метеостанций
#23 регоин-Краснодарский край
setwd("D:/mathmod/Klyshbekova1")
getwd()
#устанавливаем пакеты
install.packages("tidyverse")
install.packages("rnoaa")
#открываем нужные пакеты
library("tidyverse")
library("rnoaa")
library("lubridate")
#скачиваем станции
station_data = ghcnd_stations()
write.csv(station_data,file="stations.csv")
station_data=read.csv("stations.csv")


#После получения всписка всех станций, получите список станций ближайших к столице вашего региона,создав таблицу с именем региона и координатами его столицы
krasnodar = data.frame(id = "KRASNODAR", latitude = 45.02,  longitude = 38.59)
krasnodar_around = meteo_nearby_stations(lat_lon_df = krasnodar, station_data = station_data,
                                    limit = 19, var = c("PCR","TAVG"),
                                    year_min = 2003, year_max = 2005)

#krasnodar_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их 
# удалленности от краснодара, очевидно что первым элементом таблицы будет идентификатор метеостанции Краснодар, его то мы и попытаемся получить
krasnodar_id = krasnodar_around[["KRASNODAR"]][["id"]][1]
summary(krasnodar_id)
# для получения таблицы со всеми метеостанциями вокруг Краснодара 
# необходимо выбрать целиком первый объект из списка
krasnodar_table=krasnodar_around[[1]]
summary(krasnodar_table)
# в таблице krasnoddar_table оказалось 19 объектов, ранжированных по расстоянию от Краснодара
# сформируем список необходимых станций
# выведем индетификаторы отфильрованных метеостанций
krasnodar_stations=krasnodar_table
str(krasnodar_stations)

# список содержит 19 метеостанции расположенных вблизи Краснодара
# для получения таблицы со всеми метеостанциями вокруг 
# выведем индетификаторы отфильрованных метеостанций
krasnodar_stations$id

# чтобы получить все данные с 1 метеостанции используем команду meteo_tidy_ghcnd
all_krasnodar_data=meteo_tidy_ghcnd(stationid = krasnodar_id)
# посмотрим что мы скачали
summary(all_krasnodar_data)


# создать цикл, в котором бы скачивались нужные данные для всех метеостанций
# cоздадим объект, куда скачаем все данные всех метеостанций (колличество)
all_krasnodar_meteodata = data.frame()
# создаем цикл для наших метеостанций
stations_names=krasnodar_stations$id
stations_names=stations_names[1:19] 

for (sname in stations_names)
{ one_meteo=meteo_tidy_ghcnd( stationid = sname,
                              date_min = "2003-01-01",
                              date_max = "2005-12-31")
station_vars=names(one_meteo)
if (!("tavg" %in% station_vars)){
  if(!("tmax"%in% station_vars)){
    next()
  }

  
  
  
  one_meteo=one_meteo %>% mutate(tavg=(tmax+tmin)/2)}
one_meteo=one_meteo %>% select(id,date,tavg)
one_meteo = one_meteo %>% mutate(tavg=tavg/10)
all_krasnodar_meteodata=rbind(all_krasnodar_meteodata, one_meteo)}




# записываем полученные результаты
write.csv(all_krasnodar_meteodata,"all_krasnodar_meteodata.csv")
# считываем данные all_krasnodar_meteodata.csv
all_krasnodar_meteodata=read.csv("all_krasnodar_meteodata.csv")
# смотрим что получилось 
str(all_krasnodar_meteodata)

# добавим год, месяц, день
all_krasnodar_meteodata=all_krasnodar_meteodata %>% mutate(year=year(date), 
                                                           month=month(date), 
                                                           day=day(date))

# превратим NA в 0 и где tavg<5
all_krasnodar_meteodata[is.na(all_krasnodar_meteodata$tavg),"tavg"] = 0
all_krasnodar_meteodata[all_krasnodar_meteodata$tavg<5, "tavg"] = 0
summary(all_krasnodar_meteodata)





# сгруппируем метеостанции по id,месяцам и годам и проссумируем темперетатуру
# по этим группам, затем сгурппируем данные по месяцам и найдем среднее по месяцам
# для всех метеостанций
group_meteodata =all_krasnodar_meteodata %>% group_by(id,year,month)
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))

## Подготовка к расчету по формуле Урожая ##
### Ввод констант
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
# константа по табл.1. Создаем вектор
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
# константа по табл. 1. Создаем вектор
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
# отношение числа дней i-го месяца,
#входящих в период вегетации культуры, к общему
#числу дней  в месяце, константа по табл.1
y=1.0
# Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
Kf=300
# Коэффициент использования ФАР посевом 
Qj=1600
# калорийность урожая культуры
Lj=2.2
# сумма частей основной побочной продукции
Ej=25
# стандартная влажность культуры 
# Рассчитаем Fi по месяцам
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)
#Рассчитаем Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
##  Расчитываем урожай 
Yield = (sum(sumT_month$Yi)) 
Yield 
# Результат 20,4 ц/га





