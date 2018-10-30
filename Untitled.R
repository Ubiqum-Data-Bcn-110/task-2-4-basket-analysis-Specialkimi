# instal packages

setwd("~/Desktop/tasks /M2T4/task-2-4-basket-analysis-Specialkimi")


install.packages("arules")
install.packages("arulesViz")
install.packages("grid")
install.packages("Matrix")
library(grid)
library(arules)
library(arulesViz)


# upload dataset
ElectronidexTransactions2017 <- read.transactions("~/Desktop/tasks /M2T4/task-2-4-basket-analysis-Specialkimi/ElectronidexTransactions2017.csv" , format = "basket", sep = ",", rm.duplicates = FALSE )

# starting checking info 

inspect(ElectronidexTransactions2017)                                                
length (ElectronidexTransactions2017)                                  
size(ElectronidexTransactions2017)                                                 
list(ElectronidexTransactions2017)
size (ElectronidexTransactions2017)

# dataset vizualization
itemFrequency(ElectronidexTransactions2017)
itemFrequencyPlot(ElectronidexTransactions2017, topN = 20)

itemFrequencyPlot(ElectronidexTransactions2017, topN = 20)

# apriori algoritm

Aprirules1<- apriori (ElectronidexTransactions2017, parameter = list(supp = 0.1, conf = 0.8))
Aprirules2<- apriori(ElectronidexTransactions2017, parameter = list(supp = 0.05, conf = 0.3))
Aprirules3<- apriori (ElectronidexTransactions2017, parameter = list(supp = 0.05, conf = 0.2))
Aprirules4<- apriori (ElectronidexTransactions2017, parameter = list(supp = 0.05, conf = 0.1))
Aprirules5<- apriori (ElectronidexTransactions2017, parameter = list(supp = 0.002, conf = 0.8))

# visualize

inspect (Aprirules1)
inspect (Aprirules2)
inspect (Aprirules3)
inspect (Aprirules4)
inspect(Aprirules5)

# summarizing model

summary ( Aprirules1)
summary ( Aprirules2)
summary ( Aprirules3)
summary ( Aprirules4)
summary ( Aprirules5)

# categorizing products 
head(itemInfo(ElectronidexTransactions2017))


#### 4. Cleaning dataset ####

sizetransactions <- size(ElectronidexTransactions2017)
sum (sizetransactions == 0)
which (sizetransactions == 0)

### C: transaction 8707 and 9506 tiene 0 productos
hist(sizetransactions)

### C: 2163 transactions only have one product

sum(sizetransactions == 1)
which(sizetransactions == 1)
### C: 2163 transactions only have one product

ElectronidexTransactions_0 <- ElectronidexTransactions2017[sizetransactions == 0]
summary(ElectronidexTransactions_0)
hist(ElectronidexTransactions_0)

ElectronidexTransactions_1 <- ElectronidexTransactions2017[sizetransactions == 1]
summary(ElectronidexTransactions_1)
hist(ElectronidexTransactions_1)

### C: Most ferquent products that were bought soo: Apple MacBook Air(383), Apple Earpods (156), iMac (121) and CYBERPOWER Gamer Desktop (109)

ElectronidexTransactions_new <- ElectronidexTransactions2017[!size(ElectronidexTransactions2017)== 0]
summary(ElectronidexTransactions_new)

ElectronidexTransactions_new_2 <- ElectronidexTransactions_new[!size(ElectronidexTransactions_new)== "1"]

summary(ElectronidexTransactions_new_2)

### C: 7670 rows =  9835 - 2163 - 2

ElectronidexTransactions_clean <- ElectronidexTransactions_new_2

# creating new product cattegrories 

str(ElectronidexTransactions_clean)
Product_type <- ElectronidexTransactions_clean@itemInfo$labels
ElectronidexTransactions_clean@itemInfo$Product_type <- Product_type
str(ElectronidexTransactions_clean)

## 5.1 External Hardrives
grep("Hard Drive", Product_type)
Product_type[grep("Hard Drive", Product_type)]
Product_type[grep("Hard Drive", Product_type)] <- "External Hardrives"
Product_type
sum(Product_type == "External Hardrives")

## 5.2 Computer Stands
grep("Stand", Product_type)
Product_type[grep("Stand", Product_type)] <- "Computer Stands"
Product_type
grep("Mount", Product_type)
Product_type[grep("Mount", Product_type)] <- "Computer Stands"
Product_type
sum(Product_type == "Computer Stands")

## 5.3 Computer Tablets
grep("iPad", Product_type)
Product_type[grep("iPad", Product_type)] <- "Computer Tablets"
Product_type
grep("HD Tablet", Product_type)
Product_type[grep("HD Tablet", Product_type)] <- "Computer Tablets"
Product_type
grep("Galaxy Tab", Product_type)
Product_type[grep("Galaxy Tab", Product_type)] <- "Computer Tablets"
Product_type
grep("Kindle", Product_type)
Product_type[grep("Kindle", Product_type)] <- "Computer Tablets"
Product_type
sum(Product_type == "Computer Tablets")

## 5.4 Smart Home Devices
grep("Apple TV", Product_type)
Product_type[grep("Apple TV", Product_type)] <- "Smart Home Devices"
Product_type

grep("Google Home", Product_type)
Product_type[grep("Google Home", Product_type)] <- "Smart Home Devices"
Product_type

grep("Smart Light Bulb", Product_type)
Product_type[grep("Smart Light Bulb", Product_type)] <- "Smart Home Devices"
Product_type

grep("Fire TV Stick", Product_type)
Product_type[grep("Fire TV Stick", Product_type)] <- "Smart Home Devices"
Product_type

grep("Roku Express", Product_type)
Product_type[grep("Roku Express", Product_type)] <- "Smart Home Devices"
Product_type

## 5.1.5 Printer Ink
grep("Ink", Product_type)
Product_type[grep("Ink", Product_type)] <- "Printer Ink"
Product_type

grep("Toner", Product_type)
Product_type[grep("Toner", Product_type)] <- "Printer Ink"
Product_type

grep("Labeling Tape", Product_type)
Product_type[grep("Labeling Tape", Product_type)] <- "Printer Ink"
Product_type

sum(Product_type == "Printer Ink")
### C: 5 printer ink

## 5.5 Printers
grep("Epson Printer", Product_type)
Product_type[grep("Epson Printer", Product_type)] <- "Printers"

grep("HP Wireless Printer", Product_type)
Product_type[grep("HP Wireless Printer", Product_type)] <- "Printers"

grep("Canon Office Printer", Product_type)
Product_type[grep("Canon Office Printer", Product_type)] <- "Printers"

grep("Brother Printer", Product_type)
Product_type[grep("Brother Printer", Product_type)] <- "Printers"

grep("DYMO Label Manker", Product_type)
Product_type[grep("DYMO Label Manker", Product_type)] <- "Printers"
Product_type
sum(Product_type == "Printers")

## 5.6 Speakers
grep("Speaker", Product_type)
Product_type[grep("Speaker", Product_type)] <- "Speakers"
Product_type
sum(Product_type == "Speakers")

grep("DOSS Touch", Product_type)
Product_type[grep("DOSS Touch", Product_type)] <- "Speakers"
sum(Product_type == "Speakers")

grep("Cyber Acoustics", Product_type)
Product_type[grep("Cyber Acoustics", Product_type)] <- "Speakers"
sum(Product_type == "Speakers")

grep("Sonos", Product_type)
Product_type[grep("Sonos", Product_type)] <- "Speakers"
sum(Product_type == "Speakers")
Product_type

## 5.7 Laptops
grep("Laptop", Product_type)
Product_type[grep("Laptop", Product_type)] <- "Laptops"
Product_type
sum(Product_type == "Laptops")

grep("Acer Aspire", Product_type)
Product_type[grep("Acer Aspire", Product_type)] <- "Laptops"

grep("ASUS Chromebook", Product_type)
Product_type[grep("ASUS Chromebook", Product_type)] <- "Laptops"

grep("Apple MacBook Pro", Product_type)
Product_type[grep("Apple MacBook Pro", Product_type)] <- "Laptops"

grep("Apple MacBook Air", Product_type)
Product_type[grep("Apple MacBook Air", Product_type)] <- "Laptops"

## 5.8 Mouse and Keyboard Combo
grep("Combo", Product_type)
Product_type[grep("Combo", Product_type)] <- "Mouse and Keyboard Combo"
sum(Product_type == "Mouse and Keyboard Combo")

grep("Keyboard & Mouse", Product_type)
Product_type[grep("Keyboard & Mouse", Product_type)] <- "Mouse and Keyboard Combo"
sum(Product_type == "Mouse and Keyboard Combo")

grep("Keyboard and Mouse", Product_type)
Product_type[grep("Keyboard and Mouse", Product_type)] <- "Mouse and Keyboard Combo"
sum(Product_type == "Mouse and Keyboard Combo")

## 5.9 Desktops
grep("Desktops", Product_type)
Product_type[grep("Desktop", Product_type)] <- "Desktops"
sum(Product_type == "Desktops")

grep("iMac", Product_type)
Product_type[grep("iMac", Product_type)] <- "Desktops"
sum(Product_type == "Desktops")

## 5.10 Computer Cords
grep("Cable", Product_type)
Product_type[grep("Cable", Product_type)] <- "Computer Cords"
sum(Product_type == "Computer Cords")

grep("HDMI Adapter", Product_type)
Product_type[grep("HDMI Adapter", Product_type)] <- "Computer Cords"
sum(Product_type == "Computer Cords")

## 5.11 Monitors
grep("Monitor", Product_type)
Product_type[grep("Monitor", Product_type)] <- "Monitors"
sum(Product_type == "Monitors")

## 5.12 Accessories
grep("Microsoft Office Home and Student 2016", Product_type)
Product_type[grep("Microsoft Office Home and Student 2016", Product_type)] <- "Accessories"
sum(Product_type == "Accessories")

grep("Computer Game", Product_type)
Product_type[grep("Computer Game", Product_type)] <- "Accessories"

grep("Mouse Pad", Product_type)
Product_type[grep("Mouse Pad", Product_type)] <- "Accessories"

## 5.13 Active Headphones
grep("Apple Earpods", Product_type)
Product_type[grep("Apple Earpods", Product_type)] <- "Active Headphones"
sum(Product_type == "Active Headphones")

grep("Monster Beats By Dr Dre", Product_type)
Product_type[grep("Monster Beats By Dr Dre", Product_type)] <- "Active Headphones"
sum(Product_type == "Active Headphones")

grep("Panasonic In-Ear Headphone", Product_type)
Product_type[grep("Panasonic In-Ear Headphone", Product_type)] <- "Active Headphones"
sum(Product_type == "Active Headphones")

grep("Otium", Product_type)
Product_type[grep("Otium", Product_type)] <- "Active Headphones"
sum(Product_type == "Active Headphones")

grep("APIE", Product_type)
Product_type[grep("APIE", Product_type)] <- "Active Headphones"
sum(Product_type == "Active Headphones")

grep("Philips", Product_type)
Product_type[grep("Philips", Product_type)] <- "Active Headphones"
sum(Product_type == "Active Headphones")

## 5.14 Computer Headphones
grep("Headset", Product_type)
Product_type[grep("Headset", Product_type)] <- "Computer Headphones"
sum(Product_type == "Computer Headphones")

grep("Panasonic", Product_type)
Product_type[grep("Panasonic", Product_type)] <- "Computer Headphones"
sum(Product_type == "Computer Headphones")

grep("Kensington Headphones", Product_type)
Product_type[grep("Kensington Headphones", Product_type)] <- "Computer Headphones"
sum(Product_type == "Computer Headphones")

grep("Koss Home Headphones", Product_type)
Product_type[grep("Koss Home Headphones", Product_type)] <- "Computer Headphones"
sum(Product_type == "Computer Headphones")

grep("Ailihen Stereo Headphones", Product_type)
Product_type[grep("Ailihen Stereo Headphones", Product_type)] <- "Computer Headphones"
sum(Product_type == "Computer Headphones")

Product_type

## 5.15 Computer Mice
grep("3-Button Mouse", Product_type)
Product_type[grep("3-Button Mouse", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("Logitech Wireless Mouse", Product_type)
Product_type[grep("Logitech Wireless Mouse", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("Microsoft Basic Optical Mouse", Product_type)
Product_type[grep("Microsoft Basic Optical Mouse", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("Logitech 3-button Mouse", Product_type)
Product_type[grep("Logitech 3-button Mouse", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("Redragon Gaming Mouse", Product_type)
Product_type[grep("Redragon Gaming Mouse", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("HP Wireless Mouse", Product_type)
Product_type[grep("HP Wireless Mouse", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("Generic Black 3-Button", Product_type)
Product_type[grep("Generic Black 3-Button", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("Wireless Portable Mouse", Product_type)
Product_type[grep("Wireless Portable Mouse", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("Gaming Mouse Professional", Product_type)
Product_type[grep("Gaming Mouse Professional", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

grep("Slim Wireless Mouse", Product_type)
Product_type[grep("Slim Wireless Mouse", Product_type)] <- "Computer Mice"
sum(Product_type == "Computer Mice")

Product_type

## 5.16 Keyboards

grep("LED", Product_type)
Product_type[grep("LED", Product_type)] <- "Keyboards"
sum(Product_type == "Keyboards")

grep("Wireless", Product_type)
Product_type[grep("Wireless", Product_type)] <- "Keyboards"
sum(Product_type == "Keyboards")

grep("Magic", Product_type)
Product_type[grep("Magic", Product_type)] <- "Keyboards"
sum(Product_type == "Keyboards")

grep("Wired", Product_type)
Product_type[grep("Wired", Product_type)] <- "Keyboards"
sum(Product_type == "Keyboards")

grep("Logitech Keyboard", Product_type)
Product_type[grep("Logitech Keyboard", Product_type)] <- "Keyboards"
sum(Product_type == "Keyboards")

grep("HP USB Keyboard", Product_type)
Product_type[grep("HP USB Keyboard", Product_type)] <- "Keyboards"
sum(Product_type == "Keyboards")

Product_type
ElectronidexTransactions_clean@itemInfo$Product_type <- Product_type
str(ElectronidexTransactions_clean)


# creating new brand cattegrories

str(ElectronidexTransactions_clean)
Brand_type <- ElectronidexTransactions_clean@itemInfo$labels
ElectronidexTransactions_clean@itemInfo$Brand_type <- Brand_type
str(ElectronidexTransactions_clean)

## asus

grep("ASUS", Brand_type)
Brand_type[grep("ASUS", Brand_type)] <- "ASUS"
str(Brand_type)
sum(Brand_type == "ASUS")

## LG

grep("LG", Brand_type)
Brand_type[grep("ASUS", Brand_type)] <- "LG"
str(Brand_type)
sum(Brand_type == "LG")

## Acer

grep("Acer", Brand_type)
Brand_type[grep("Acer", Brand_type)] <- "Acer"
str(Brand_type)
sum(Brand_type == "Acer")

## HP

grep("HP", Brand_type)
Brand_type[grep("HP", Brand_type)] <- "HP"
str(Brand_type)
sum(Brand_type == "HP")

## Dell

grep("Dell", Brand_type)
Brand_type[grep("Dell", Brand_type)] <- "Dell"
str(Brand_type)
sum(Brand_type == "Dell")

## Logitech

grep("Logitech", Brand_type)
Brand_type[grep("Logitech", Brand_type)] <- "Logitech"
str(Brand_type)
sum(Brand_type == "Logitech")

## Samsung

grep("Samsung", Brand_type)
Brand_type[grep("Samsung", Brand_type)] <- "Samsung"
str(Brand_type)
sum(Brand_type == "Samsung")

## Lenovo

grep("Lenovo", Brand_type)
Brand_type[grep("Lenovo", Brand_type)] <- "Lenovo"
str(Brand_type)
sum(Brand_type == "Lenovo")

## Intel

grep("Intel", Brand_type)
Brand_type[grep("Intel", Brand_type)] <- "Intel"
str(Brand_type)
sum(Brand_type == "Intel")

## HP

grep("HP", Brand_type)
Brand_type[grep("HP", Brand_type)] <- "HP"
str(Brand_type)
sum(Brand_type == "HP")

## Apple 

grep("Apple", Brand_type)
Brand_type[grep("Apple", Brand_type)] <- "Apple"
str(Brand_type)
sum(Brand_type == "Apple")

grep("iPad", Brand_type)
Brand_type[grep("iPad", Brand_type)] <- "Apple"
sum(Brand_type == "Apple")

grep("iPhone", Brand_type)
Brand_type[grep("iPhone", Brand_type)] <- "Apple"
sum(Brand_type == "Apple")


grep("iMac", Brand_type)
Brand_type[grep("iMac", Brand_type)] <- "Apple"
sum(Brand_type == "Apple")


## Microsoft 

grep("Microsoft", Brand_type)
Brand_type[grep("Microsoft", Brand_type)] <- "Microsoft"
str(Brand_type)
sum(Brand_type == "Microsoft")

## viewsonic

grep("ViewSonic", Brand_type)
Brand_type[grep("ViewSonic", Brand_type)] <- "ViewSonic"
sum(Brand_type == "ViewSonic")

## Rii

grep("Rii", Brand_type)
Brand_type[grep("Rii", Brand_type)] <- "Rii"
sum(Brand_type == "Rii")

# DYMO

grep("DYMO", Brand_type)
Brand_type[grep("DYMO", Brand_type)] <- "DYMO"
sum(Brand_type == "DYMO")

## Canon
grep("Canon", Brand_type)
Brand_type[grep("Canon", Brand_type)] <- "Canon"
sum(Brand_type == "Canon")


## Epson
grep("Epson", Brand_type)
Brand_type[grep("Epson", Brand_type)] <- "Epson"
sum(Brand_type == "Epson")

# Other Brands

## 6.15 Other brands
grep("CYBERPOWER", Brand_type)
Brand_type[grep("CYBERPOWER", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("APIE", Brand_type)
Brand_type[grep("APIE", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Ailihen", Brand_type)
Brand_type[grep("Ailihen", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("ViewSonic", Brand_type)
Brand_type[grep("ViewSonic", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Headphones", Brand_type)
Brand_type[grep("Headphones", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Headphones", Brand_type)
Brand_type[grep("Headphones", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

str(ElectronidexTransactions_clean)


grep("Portable", Brand_type)
Brand_type[grep("Portable", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("3-Button Mouse", Brand_type)
Brand_type[grep("3-Button Mouse", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("5TB Desktop Hard Drive", Brand_type)
Brand_type[grep("5TB Desktop Hard Drive", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Mouse", Brand_type)
Brand_type[grep("Mouse", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Monitor", Brand_type)
Brand_type[grep("Monitor", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Headset", Brand_type)
Brand_type[grep("Headset", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Headphone", Brand_type)
Brand_type[grep("Headphone", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Speaker", Brand_type)
Brand_type[grep("Speaker", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")


grep("Roku Express", Brand_type)
Brand_type[grep("Roku Express", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("USB", Brand_type)
Brand_type[grep("USB", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Light", Brand_type)
Brand_type[grep("Light", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Laptop", Brand_type)
Brand_type[grep("Laptop", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Monster", Brand_type)
Brand_type[grep("Monster", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")
Brand_type

grep("Stand", Brand_type)
Brand_type[grep("Stand", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")
Brand_type

grep("Cable", Brand_type)
Brand_type[grep("Cable", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Kindle", Brand_type)
Brand_type[grep("Kindle", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("HDMI", Brand_type)
Brand_type[grep("HDMI", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Google", Brand_type)
Brand_type[grep("Google", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Printer", Brand_type)
Brand_type[grep("Printer", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("DOSS", Brand_type)
Brand_type[grep("DOSS", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Button", Brand_type)
Brand_type[grep("Button", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Sonos", Brand_type)
Brand_type[grep("Sonos", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Fire", Brand_type)
Brand_type[grep("Fire", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Cyber", Brand_type)
Brand_type[grep("Cyber", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Intel", Brand_type)
Brand_type[grep("Intel", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("Game", Brand_type)
Brand_type[grep("Game", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

grep("LED", Brand_type)
Brand_type[grep("LED", Brand_type)] <- "Other brands"
sum(Brand_type == "Other brands")

### C: 63 Producst of Other brands

str(ElectronidexTransactions_clean)
Brand_type

## 8.1 ElectronidexTransactions_clean
Rules_1 <- apriori(ElectronidexTransactions_clean, parameter = list(supp=0.01, conf=0.01, minlen = 2 ))
inspect(Rules_1)
inspect(head((sort(Rules_1, by="confidence")), n=20))
summary(Rules_1)
plot(Rules_1)

### P: to check top 20 rules, sorted by confidence and lift
Rules_2 <- apriori(ElectronidexTransactions_clean, parameter = list(supp=0.02, conf=0.1, minlen = 2 ))
inspect(Rules_2)
inspect(head((sort(Rules_2, by="confidence")), n=20))
inspect(head((sort(Rules_2, by="lift")), n=20))
summary(Rules_2)
plot(Rules_2)

### P: to check rules wuth iMac
ItemRules_trial <- subset(Rules_ByBrand, items %in% "Apple")
inspect(ItemRules_trial)
sum(is.redundant(ItemRules_trial)== "TRUE")
plot(ItemRules_trial[1:5], method="graph", control=list(type="items")) 

Rules_LHS_Apple <- apriori(TransactionsByBrand, parameter= list(supp= 0.01, conf = 0.2),
                           appearance = list(lhs=c("Apple")))
inspect(Rules_LHS_Apple)
plot(Rules_LHS_Apple[1:10],method = "graph", control = list(type="items"))


plot(Rules_2, method="graph", control=list(type="items")) 
### C/D: I can't interpret the graph

## 8.2 By category
TransactionsByType <- aggregate(ElectronidexTransactions_clean ,by = "Product_type")
TransactionsByBrand <- aggregate(ElectronidexTransactions_clean, by = "Brand_type")


str(ElectronidexTransactions_clean)

Rules_ByType_sup005_conf05 <- apriori(TransactionsByType, parameter = list(supp=0.05, conf=0.5, minlen = 2 ))
inspect(Rules_ByType_sup005_conf05)
summary(Rules_ByType_sup005_conf05)
plot(Rules_ByType_sup005_conf05)


Rules_ByBrand <- apriori(TransactionsByBrand, parameter = list(supp=0.02, conf=0.05, minlen = 2 ))
inspect(Rules_ByBrand)
plot(Rules_ByBrand)

plot(Rules_ByBrand[1:5], method="graph", control=list(type="items")) 
plot(Rules_ByType_sup005_conf05[1:5], method="graph", control=list(type="items")) 

# creating a new data set with only transactions under 5

ElectronidexTransactions_retail1 <- ElectronidexTransactions2017[!size(ElectronidexTransactions2017)== 0]
ElectronidexTransactions_retail2 <- ElectronidexTransactions_retail1[!size(ElectronidexTransactions_retail1)== "1"]
ElectronidexTransactions_retail3 <- ElectronidexTransactions_retail2[!size(ElectronidexTransactions_retail2)== "2"]
ElectronidexTransactions_retail4 <- ElectronidexTransactions_retail3[!size(ElectronidexTransactions_retail3)== "3"]
ElectronidexTransactions_retail5 <- ElectronidexTransactions_retail4[!size(ElectronidexTransactions_retail4)== "4"]

str(ElectronidexTransactions_retail5)
summary(ElectronidexTransactions_retail5)
plot(ElectronidexTransactions_retail5)

# model

Aprirulesretail5.1<- apriori (ElectronidexTransactions_retail5, parameter = list(supp = 0.1, conf = 0.8))
Aprirulesretail5.2<- apriori(ElectronidexTransactions_retail5, parameter = list(supp = 0.05, conf = 0.3))
Aprirulesretail5.3<- apriori (ElectronidexTransactions_retail5, parameter = list(supp = 0.05, conf = 0.2))
Aprirulesretail5.4<- apriori (ElectronidexTransactions_retail5, parameter = list(supp = 0.05, conf = 0.1))
Aprirulesretail5.5<- apriori (ElectronidexTransactions_retail5, parameter = list(supp = 0.002, conf = 0.8))

# visualize

inspect (Aprirulesretail5.1)
inspect (Aprirulesretail5.2)
inspect (Aprirulesretail5.3)
inspect (Aprirulesretail5.4)
inspect(Aprirulesretail5.5)

# summarizing model

summary ( Aprirulesretail5.1)
summary ( Aprirulesretail5.2)
summary ( Aprirulesretail5.3)
summary ( Aprirulesretail5.4)
summary ( Aprirulesretail5.5)

# splitting new dataset per categories

Product_typeRetail5 <- ElectronidexTransactions_retail5@itemInfo$labels
ElectronidexTransactions_clean@itemInfo$Product_typeRetail5 <- Product_typeRetail5
str( ElectronidexTransactions_retail5)

## 5.1 External Hardrives
grep("Hard Drive", Product_typeRetail5)
Product_typeRetail5[grep("Hard Drive", Product_typeRetail5)]
Product_typeRetail5[grep("Hard Drive", Product_typeRetail5)] <- "External Hardrives"
Product_typeRetail5
sum(Product_type == "External Hardrives")

## 5.2 Computer Stands
grep("Stand", Product_typeRetail5)
Product_typeRetail5 [grep("Stand", Product_typeRetail5)] <- "Computer Stands"
Product_typeRetail5
grep("Mount", Product_typeRetail5)

Product_typeRetail5[grep("Mount", Product_typeRetail5)] <- "Computer Stands"
Product_typeRetail5
sum(Product_typeRetail5 == "Computer Stands")

## 5.3 Computer Tablets
grep("iPad", Product_typeRetail5)
Product_typeRetail5[grep("iPad", Product_typeRetail5)] <- "Computer Tablets"
Product_typeRetail5
grep("HD Tablet", Product_typeRetail5)
Product_typeRetail5[grep("HD Tablet", Product_typeRetail5)] <- "Computer Tablets"
Product_typeRetail5
grep("Galaxy Tab", Product_typeRetail5)
Product_typeRetail5[grep("Galaxy Tab", Product_typeRetail5)] <- "Computer Tablets"
Product_typeRetail5
grep("Kindle", Product_typeRetail5)
Product_typeRetail5[grep("Kindle", Product_typeRetail5)] <- "Computer Tablets"
Product_typeRetail5
sum(Product_typeRetail5 == "Computer Tablets")

## 5.4 Smart Home Devices
grep("Apple TV", Product_typeRetail5)
Product_typeRetail5[grep("Apple TV", Product_typeRetail5)] <- "Smart Home Devices"
Product_typeRetail5

grep("Google Home", Product_typeRetail5)
Product_typeRetail5[grep("Google Home", Product_typeRetail5)] <- "Smart Home Devices"
Product_typeRetail5

grep("Smart Light Bulb", Product_typeRetail5)
Product_typeRetail5[grep("Smart Light Bulb", Product_typeRetail5)] <- "Smart Home Devices"
Product_typeRetail5

grep("Fire TV Stick", Product_typeRetail5)
Product_typeRetail5[grep("Fire TV Stick", Product_typeRetail5)] <- "Smart Home Devices"
Product_typeRetail5

grep("Roku Express", Product_type)
Product_typeRetail5[grep("Roku Express", Product_typeRetail5)] <- "Smart Home Devices"
Product_typeRetail5

## 5.1.5 Printer Ink
grep("Ink", Product_typeRetail5)
Product_typeRetail5[grep("Ink", Product_typeRetail5)] <- "Printer Ink"
Product_typeRetail5

grep("Toner", Product_typeRetail5)
Product_typeRetail5[grep("Toner", Product_typeRetail5)] <- "Printer Ink"
Product_typeRetail5

grep("Labeling Tape", Product_typeRetail5)
Product_typeRetail5[grep("Labeling Tape", Product_typeRetail5)] <- "Printer Ink"
Product_typeRetail5

sum(Product_type == "Printer Ink")
### C: 5 printer ink

## 5.5 Printers
grep("Epson Printer", Product_typeRetail5)
Product_typeRetail5[grep("Epson Printer", Product_typeRetail5)] <- "Printers"

grep("HP Wireless Printer", Product_typeRetail5)
Product_typeRetail5[grep("HP Wireless Printer", Product_typeRetail5)] <- "Printers"

grep("Canon Office Printer", Product_typeRetail5)
Product_typeRetail5[grep("Canon Office Printer", Product_typeRetail5)] <- "Printers"

grep("Brother Printer", Product_typeRetail5)
Product_typeRetail5[grep("Brother Printer", Product_typeRetail5)] <- "Printers"

grep("DYMO Label Manker", Product_typeRetail5)
Product_typeRetail5[grep("DYMO Label Manker", Product_typeRetail5)] <- "Printers"
Product_typeRetail5
sum(Product_typeRetail5 == "Printers")

## 5.6 Speakers
grep("Speaker", Product_typeRetail5)
Product_typeRetail5[grep("Speaker", Product_typeRetail5)] <- "Speakers"
Product_typeRetail5
sum(Product_typeRetail5 == "Speakers")

grep("DOSS Touch", Product_typeRetail5)
Product_typeRetail5[grep("DOSS Touch", Product_typeRetail5)] <- "Speakers"
sum(Product_typeRetail5 == "Speakers")

grep("Cyber Acoustics", Product_typeRetail5)
Product_typeRetail5[grep("Cyber Acoustics", Product_typeRetail5)] <- "Speakers"
sum(Product_typeRetail5 == "Speakers")

grep("Sonos", Product_typeRetail5)
Product_typeRetail5[grep("Sonos", Product_typeRetail5)] <- "Speakers"
sum(Product_typeRetail5 == "Speakers")
Product_typeRetail5

## 5.7 Laptops
grep("Laptop", Product_typeRetail5)
Product_typeRetail5[grep("Laptop", Product_typeRetail5)] <- "Laptops"
Product_typeRetail5
sum(Product_typeRetail5 == "Laptops")

grep("Acer Aspire", Product_typeRetail5)
Product_typeRetail5[grep("Acer Aspire", Product_typeRetail5)] <- "Laptops"

grep("ASUS Chromebook", Product_typeRetail5)
Product_typeRetail5[grep("ASUS Chromebook", Product_typeRetail5)] <- "Laptops"

grep("Apple MacBook Pro", Product_typeRetail5)
Product_typeRetail5[grep("Apple MacBook Pro", Product_typeRetail5)] <- "Laptops"

grep("Apple MacBook Air", Product_typeRetail5)
Product_typeRetail5[grep("Apple MacBook Air", Product_typeRetail5)] <- "Laptops"

## 5.8 Mouse and Keyboard Combo
grep("Combo", Product_typeRetail5)
Product_typeRetail5[grep("Combo", Product_typeRetail5)] <- "Mouse and Keyboard Combo"
sum(Product_typeRetail5 == "Mouse and Keyboard Combo")

grep("Keyboard & Mouse", Product_type)
Product_typeRetail5[grep("Keyboard & Mouse", Product_typeRetail5)] <- "Mouse and Keyboard Combo"
sum(Product_typeRetail5 == "Mouse and Keyboard Combo")

grep("Keyboard and Mouse", Product_typeRetail5)
Product_typeRetail5[grep("Keyboard and Mouse", Product_typeRetail5)] <- "Mouse and Keyboard Combo"
sum(Product_typeRetail5 == "Mouse and Keyboard Combo")

## 5.9 Desktops
grep("Desktops", Product_type)
Product_typeRetail5[grep("Desktop", Product_typeRetail5)] <- "Desktops"
sum(Product_typeRetail5 == "Desktops")

grep("iMac", Product_typeRetail5)
Product_typeRetail5[grep("iMac", Product_typeRetail5)] <- "Desktops"
sum(Product_typeRetail5 == "Desktops")

## 5.10 Computer Cords
grep("Cable", Product_typeRetail5)
Product_typeRetail5[grep("Cable", Product_typeRetail5)] <- "Computer Cords"
sum(Product_typeRetail5 == "Computer Cords")

grep("HDMI Adapter", Product_typeRetail5)
Product_typeRetail5[grep("HDMI Adapter", Product_typeRetail5)] <- "Computer Cords"
sum(Product_typeRetail5 == "Computer Cords")

## 5.11 Monitors
grep("Monitor", Product_typeRetail5)
Product_typeRetail5[grep("Monitor", Product_typeRetail5)] <- "Monitors"
sum(Product_typeRetail5 == "Monitors")

## 5.12 Accessories
grep("Microsoft Office Home and Student 2016", Product_typeRetail5)
Product_typeRetail5[grep("Microsoft Office Home and Student 2016", Product_typeRetail5)] <- "Accessories"
sum(Product_typeRetail5 == "Accessories")

grep("Computer Game", Product_typeRetail5)
Product_typeRetail5[grep("Computer Game", Product_typeRetail5)] <- "Accessories"

grep("Mouse Pad", Product_typeRetail5)
Product_typeRetail5[grep("Mouse Pad", Product_typeRetail5)] <- "Accessories"

## 5.13 Active Headphones
grep("Apple Earpods", Product_typeRetail5)
Product_typeRetail5[grep("Apple Earpods", Product_typeRetail5)] <- "Active Headphones"
sum(Product_typeRetail5 == "Active Headphones")

grep("Monster Beats By Dr Dre", Product_typeRetail5)
Product_typeRetail5[grep("Monster Beats By Dr Dre", Product_typeRetail5)] <- "Active Headphones"
sum(Product_typeRetail5 == "Active Headphones")

grep("Panasonic In-Ear Headphone", Product_typeRetail5)
Product_typeRetail5[grep("Panasonic In-Ear Headphone", Product_typeRetail5)] <- "Active Headphones"
sum(Product_typeRetail5 == "Active Headphones")

grep("Otium", Product_typeRetail5)
Product_typeRetail5[grep("Otium", Product_typeRetail5)] <- "Active Headphones"
sum(Product_typeRetail5 == "Active Headphones")

grep("APIE", Product_typeRetail5)
Product_typeRetail5[grep("APIE", Product_typeRetail5)] <- "Active Headphones"
sum(Product_typeRetail5 == "Active Headphones")

grep("Philips", Product_typeRetail5)
Product_typeRetail5[grep("Philips", Product_typeRetail5)] <- "Active Headphones"
sum(Product_typeRetail5 == "Active Headphones")

## 5.14 Computer Headphones
grep("Headset", Product_typeRetail5)
Product_typeRetail5[grep("Headset", Product_typeRetail5)] <- "Computer Headphones"
sum(Product_typeRetail5 == "Computer Headphones")

grep("Panasonic", Product_typeRetail5)
Product_typeRetail5[grep("Panasonic", Product_typeRetail5)] <- "Computer Headphones"
sum(Product_typeRetail5 == "Computer Headphones")

grep("Kensington Headphones", Product_typeRetail5)
Product_typeRetail5[grep("Kensington Headphones", Product_typeRetail5)] <- "Computer Headphones"
sum(Product_typeRetail5 == "Computer Headphones")

grep("Koss Home Headphones", Product_typeRetail5)
Product_typeRetail5[grep("Koss Home Headphones", Product_typeRetail5)] <- "Computer Headphones"
sum(Product_typeRetail5 == "Computer Headphones")

grep("Ailihen Stereo Headphones", Product_typeRetail5)
Product_typeRetail5[grep("Ailihen Stereo Headphones", Product_typeRetail5)] <- "Computer Headphones"
sum(Product_typeRetail5 == "Computer Headphones")

Product_typeRetail5

## 5.15 Computer Mice
grep("3-Button Mouse", Product_typeRetail5)
Product_typeRetail5[grep("3-Button Mouse", Product_typeRetail5)] <- "Computer Mice"
sum(Product_typeRetail5 == "Computer Mice")

grep("Logitech Wireless Mouse", Product_typeRetail5)
Product_typeRetail5[grep("Logitech Wireless Mouse", Product_typeRetail5)] <- "Computer Mice"
sum(Product_typeRetail5 == "Computer Mice")

grep("Microsoft Basic Optical Mouse", Product_typeRetail5)
Product_typeRetail5[grep("Microsoft Basic Optical Mouse", Product_typeRetail5)] <- "Computer Mice"
sum(Product_typeRetail5 == "Computer Mice")

grep("Logitech 3-button Mouse", Product_typeRetail5)
Product_typeRetail5[grep("Logitech 3-button Mouse", Product_typeRetail5)] <- "Computer Mice"
sum(Product_typeRetail5 == "Computer Mice")

grep("Redragon Gaming Mouse", Product_typeRetail5)
Product_typeRetail5[grep("Redragon Gaming Mouse", Product_typeRetail5)] <- "Computer Mice"
sum(Product_typeRetail5 == "Computer Mice")

grep("HP Wireless Mouse", Product_typeRetail5)
Product_typeRetail5[grep("HP Wireless Mouse", Product_typeRetail5)] <- "Computer Mice"
sum(Product_typeRetail5 == "Computer Mice")

grep("Generic Black 3-Button", Product_typeRetail5)
Product_typeRetail5[grep("Generic Black 3-Button", Product_typeRetail5)] <- "Computer Mice"
sum(Product_typeRetail5 == "Computer Mice")

grep("Wireless Portable Mouse", Product_typeRetail5)
Product_typeRetail5[grep("Wireless Portable Mouse", Product_typeRetail5)] <- "Computer Mice"
sum(Product_typeRetail5 == "Computer Mice")

grep("Gaming Mouse Professional", Product_typeRetail5)
Product_typeRetail5[grep("Gaming Mouse Professional", Product_typeRetail5)] <- "Computer Mice"
sum(Product_typeRetail5 == "Computer Mice")

grep("Slim Wireless Mouse", Product_typeRetail5)
Product_typeRetail5[grep("Slim Wireless Mouse", Product_typeRetail5)] <- "Computer Mice"
sum(Product_typeRetail5 == "Computer Mice")

Product_typeRetail5

## 5.16 Keyboards

grep("LED", Product_typeRetail5)
Product_typeRetail5[grep("LED", Product_typeRetail5)] <- "Keyboards"
sum(Product_typeRetail5 == "Keyboards")

grep("Wireless", Product_typeRetail5)
Product_typeRetail5[grep("Wireless", Product_typeRetail5)] <- "Keyboards"
sum(Product_typeRetail5 == "Keyboards")

grep("Magic", Product_typeRetail5)
Product_typeRetail5[grep("Magic", Product_typeRetail5)] <- "Keyboards"
sum(Product_typeRetail5 == "Keyboards")

grep("Wired", Product_typeRetail5)
Product_typeRetail5[grep("Wired", Product_typeRetail5)] <- "Keyboards"
sum(Product_typeRetail5 == "Keyboards")

grep("Logitech Keyboard", Product_typeRetail5)
Product_typeRetail5[grep("Logitech Keyboard", Product_typeRetail5)] <- "Keyboards"
sum(Product_typeRetail5 == "Keyboards")

grep("HP USB Keyboard", Product_typeRetail5)
Product_typeRetail5[grep("HP USB Keyboard", Product_typeRetail5)] <- "Keyboards"
sum(Product_typeRetail5 == "Keyboards")

Product_typeRetail5
ElectronidexTransactions_clean@itemInfo$Product_typeRetail5 <- Product_typeRetail5
str(ElectronidexTransactions_retail5)


# definning new rules 

Rules_Retail5 <- apriori(ElectronidexTransactions_retail5, parameter = list(supp=0.01, conf=0.01, minlen = 2 ))
inspect(Rules_Retail5)
inspect(head((sort(Rules_Retail5, by="confidence")), n=20))
summary(Rules_Retail5)
plot(Rules_Retail5)

### P: to check top 20 rules, sorted by confidence and lift
Rules_Retail5.1 <- apriori(ElectronidexTransactions_retail5, parameter = list(supp=0.02, conf=0.1, minlen = 2 ))
inspect(Rules_Retail5.1 )
inspect(head((sort(Rules_Retail5.1 , by="confidence")), n=20))
inspect(head((sort(Rules_Retail5.1 , by="lift")), n=20))
summary(Rules_Retail5.1 )
plot(Rules_Retail5.1 )


Rules_comp_type_LHS <- apriori(TransactionsComp_Type, parameter= list(supp= 0.0001, conf = 0.1, minlen = 2), appearance = list(lhs=c(“Desktops”, “Laptops”)))

inspect(head((sort(Rules_comp_type_LHS, by=“lift”)), n=20))
inspect(head((sort(Rules_comp_type_LHS, by=“confidence”)), n=20))
summary(Rules_comp_type_LHS)
plot(Rules_comp_type_LHS)
plot(Rules_comp_type_LHS, method=“graph”)
plot(sort(Rules_comp_type_LHS, method=“grouped”, control=list(K=10), by =“confidence”)[1:10])

plot(sort(Rules_comp_type_LHS, by =“confidence”)[1:10], method=“graph”)
















