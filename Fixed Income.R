# PART ONE VALUING A PORFTOLIO OF BOND SECURITIES

DATE <- function(yyyy, mm, dd) {
  dte  <- as.Date(sprintf("%i-%i-%i", yyyy, mm, dd), format = "%Y-%m-%d")
  return(dte)
}

as.Date2 <- function(x) {
  tryfmt <- c("%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d", "%b %d,%Y")
  return(as.Date(x, tryFormats = tryfmt))
}

BondPortfolio<-fread(file.choose())      # File Bond Portfolio

#PART 1.1
settle1 <- DATE(2021, 12, 30)
mature1 <- BondPortfolio$maturity[1]
coupon1 <- BondPortfolio$coupon[1]
price1  <- BondPortfolio$clean.price[1]
freq1   <- 2
conv1 <- "ACT/ACT"
comp.freq1 <- 2
ytm1 <- bond.yield(settle1, mature1, coupon1, freq1, price1, conv1, comp.freq1)
ytm1

settle2 <- DATE(2021, 12, 30)
mature2 <- BondPortfolio$maturity[2]
coupon2 <- BondPortfolio$coupon[2]
price2  <- BondPortfolio$clean.price[2]
freq2   <- 2
conv2 <- "ACT/ACT"
comp.freq2 <- 2
ytm2 <- bond.yield(settle2, mature2, coupon2, freq2, price2, conv2, comp.freq2)
ytm2

settle3 <- DATE(2021, 12, 30)
mature3 <- BondPortfolio$maturity[3]
coupon3 <- BondPortfolio$coupon[3]
price3  <- BondPortfolio$clean.price[3]
freq3   <- 2
conv3 <- "ACT/ACT"
comp.freq3 <- 2
ytm3 <- bond.yield(settle3, mature3, coupon3, freq3, price3, conv3, comp.freq3)
ytm3

settle4 <- DATE(2021, 12, 30)
mature4 <- BondPortfolio$maturity[4]
coupon4 <- BondPortfolio$coupon[4]
price4  <- BondPortfolio$clean.price[4]
freq4   <- 2
conv4 <- "ACT/ACT"
comp.freq4 <- 2
ytm4 <- bond.yield(settle4, mature4, coupon4, freq4, price4, conv4, comp.freq4)
ytm4

ytm_each_bond <- data.table(ytm1, ytm2, ytm3, ytm4)
ytm_each_bond


#PART 1.2

last_payment_date1 <- as.Date2('2021-11-15')
next_payment_date1 <- as.Date2('2022-05-15')
new_settle1 <- as.Date2('2021-12-30')
E1 <- as.numeric(next_payment_date1 - last_payment_date1)
DSC1 <- as.numeric(next_payment_date1 - new_settle1)
frac1 <- DSC1 / E1
period1 <- c(1:3)
period1 <- (period1 - 1 + frac1)
t1 <- period1 / 2
maturity1 <- c(DATE(2022, 5, 15), DATE(2022, 11, 15), DATE(2023, 5, 15))
new_coupon1 <- rep(coupon1 / 2, 3) * 100
face1 <- c(0, 0, 4000000)
pv.t1 <- 1 / (1 + ytm1 / 2) ^ (2 * t1)
data1 <- data.table(period1 = period1, maturity1 = maturity1, new_coupon1 = new_coupon1, face1 = face1, pv.t1 = pv.t1)
data1[, c(1:4)]
full_price_1 <- unlist(data1[, .(sum((new_coupon1 + face1) * pv.t1))])

accint1 <- bond.TCF(settle1, mature1, coupon1, freq1, conv1)$accrued
full.price_1 <- price1 + accint1
market_value1 <- full.price_1 * 40000
market_value1

accint2 <- bond.TCF(settle2, mature2, coupon2, freq2, conv2)$accrued
full.price_2 <- price2 + accint2
market_value2 <- full.price_2 * 50000
market_value2

accint3 <- bond.TCF(settle3, mature3, coupon3, freq3, conv3)$accrued
full.price_3 <- price3 + accint3
market_value3 <- full.price_3 * 60000
market_value3

accint4 <- bond.TCF(settle4, mature4, coupon4, freq4, conv4)$accrued
full.price_4 <- price4 + accint4
market_value4 <- full.price_4 * 70000
market_value4

market_value_portfolio <- market_value1 + market_value2 + market_value3 + market_value4
market_value_portfolio

#PART 1.3

payment_date1 <- coupons.dates(settle1, mature1, freq1)
payment_date1
cf1 <- data.table(maturity = payment_date1, cf.coupon = (coupon1 * 4000000) / 2, cf.principal = c(rep(0, 2), 4000000))
cf1 <- cbind(cf1, cf1$cf.coupon + cf1$cf.principal)
cf1

payment_date2 <- coupons.dates(settle2, mature2, freq2)
payment_date2
cf2 <- data.table(maturity = payment_date2, cf.coupon = (coupon2 * 5000000) / 2, cf.principal = c(rep(0, 8), 5000000))
cf2 <- cbind(cf2, cf2$cf.coupon + cf2$cf.principal)
cf2

payment_date3 <- coupons.dates(settle3, mature3, freq3)
payment_date3
cf3 <- data.table(maturity = payment_date3, cf.coupon = (coupon3 * 6000000) / 2, cf.principal = c(rep(0, 16), 6000000))
cf3 <- cbind(cf3, cf3$cf.coupon + cf3$cf.principal)
cf3

payment_date4 <- coupons.dates(settle4, mature4, freq4)
payment_date4
cf4 <- data.table(maturity = payment_date4, cf.coupon = (coupon4 * 7000000) / 2, cf.principal = c(rep(0, 56), 7000000))
cf4 <- cbind(cf4, cf4$cf.coupon + cf4$cf.principal)
cf4

cf1_2 <- cf1[cf2, on = 'maturity']
cf1_2_3 <- cf1_2[cf3, on = 'maturity']
cf1_2_3_4 <- cf1_2_3[cf4, on = 'maturity']
cf1_2_3_4$cf_sum <- apply(cbind(cf1_2_3_4[, 4], cf1_2_3_4[, 7], cf1_2_3_4[, 10], cf1_2_3_4[, 13]), 1, function(x) ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE)))
cf1_2_3_4
cf_portfolio <- cf1_2_3_4[, .(maturity, cf_sum)]
cf_portfolio

# PART 1.4

cf <- cf1_2_3_4$cf_sum
new.cf <- c(-market_value_portfolio, cf)
new.cf
cf.freq <- 2
round(irr(new.cf, cf.freq=2), 4)

##################################### END OF PART ONE ##################################################

# Part 2 Duration and Mod Duration

# PART 2.1
bond.convexity <- function(settle,mature,coupon,freq=2,yield,convention,comp.freq=freq) {
  z  <- as.data.frame(bond.TCF(settle,mature,coupon,freq,convention))
  cf <- z$cf
  t  <- z$t
  r  <- yield
  m  <- comp.freq
  return ( 1/sum( cf / (1+r/m)^(t*m) ) * sum( t * (t+1/m) * cf / (1 + r/m)^(t*m+2) ) )
}

library(quantmod)
library(reshape2)
library(xts)

settle1 <- DATE(2021,12,30)
mature1 <- DATE(2023,05,15)
coupon1 <-0.01750 
freq   <- 2
conv   <- "ACT/ACT"
comp.freq <- 2
cleanprice1<-101.6600

yield1 <- bond.yield(settle1,mature1,coupon1, freq,cleanprice1,conv,comp.freq)
accint1<- bond.TCF(settle1,mature1,coupon1,freq,conv)$accrued
fullprice1<-cleanprice1+accint1

dmac1 <- bond.duration(settle1,mature1,coupon1,freq,yield1,conv,modified=FALSE,comp.freq)  # macaulay duration
dmod1 <- bond.duration(settle1,mature1,coupon1,freq,yield1,conv,modified=TRUE,comp.freq)   # modified duration
round(dmac1,3)
round(dmod1,3)

convexity1 <- bond.convexity(settle1,mature1,coupon1,2,yield1,conv)
convexity1
#For Bond 2
settle2 <- DATE(2021,12,30)
mature2 <- DATE(2026,05,15)
coupon2 <-0.02125 
freq   <- 2
conv   <- "ACT/ACT"
comp.freq <- 2
cleanprice2<-103.4355

yield2 <- bond.yield(settle2,mature2,coupon2, freq,cleanprice2,conv,comp.freq)
accint2<- bond.TCF(settle2,mature2,coupon2,freq,conv)$accrued
fullprice2<-cleanprice2+accint2

dmac2 <- bond.duration(settle2,mature2,coupon2,freq,yield2,conv,modified=FALSE,comp.freq)  # macaulay duration
dmod2 <- bond.duration(settle2,mature2,coupon2,freq,yield2,conv,modified=TRUE,comp.freq)   # modified duration
round(dmac2,3)
round(dmod2,3)

convexity2 <- bond.convexity(settle2,mature2,coupon2,2,yield2,conv)

#For Bond Three
settle3 <- DATE(2021,12,30)
mature3 <- DATE(2030,05,15)
coupon3 <-0.00625
freq   <- 2
conv   <- "ACT/ACT"
comp.freq <- 2
cleanprice3<-93.2175

yield3 <- bond.yield(settle3,mature3,coupon3, freq,cleanprice3,conv,comp.freq)
accint3<- bond.TCF(settle3,mature3,coupon3,freq,conv)$accrued
fullprice3<-cleanprice3+accint3

dmac3 <- bond.duration(settle3,mature3,coupon3,freq,yield3,conv,modified=FALSE,comp.freq)  # macaulay duration
dmod3 <- bond.duration(settle3,mature3,coupon3,freq,yield3,conv,modified=TRUE,comp.freq)   # modified duration
round(dmac3,3)
round(dmod3,3)
convexity3 <- bond.convexity(settle3,mature3,coupon3,2,yield3,conv)

#For Bond Four
settle4 <- DATE(2021,12,30)
mature4 <- DATE(2050,05,15)
coupon4 <-0.01250
freq   <- 2
conv   <- "ACT/ACT"
comp.freq <- 2
cleanprice4<-83.9550

yield4 <- bond.yield(settle4,mature4,coupon4, freq,cleanprice4,conv,comp.freq)
accint4<- bond.TCF(settle4,mature4,coupon4,freq,conv)$accrued
fullprice4<-cleanprice4+accint4

dmac4 <- bond.duration(settle4,mature4,coupon4,freq,yield4,conv,modified=FALSE,comp.freq)  # macaulay duration
dmod4 <- bond.duration(settle4,mature4,coupon4,freq,yield4,conv,modified=TRUE,comp.freq)   # modified duration
round(dmac4,3)
round(dmod4,3)
convexity4 <- bond.convexity(settle4,mature4,coupon4,2,yield4,conv)

ModDuration<-c(dmod1,dmod2,dmod3,dmod4)
Convexity<-c(convexity1,convexity2,convexity3,convexity4)
Bond<-c("Bond1","Bond2","Bond3","Bond4")

Answer1A<-cbind(Bond,ModDuration,Convexity)
View(Answer1A)

#Part 2.2

No.of.Bonds1<-4000000/100
No.of.Bonds2<-5000000/100
No.of.Bonds3<-6000000/100
No.of.Bonds4<-7000000/100

MarketValueOfPortfolio<-(No.of.Bonds1*fullprice1) + (No.of.Bonds2*fullprice2) + (No.of.Bonds3*fullprice3) + (No.of.Bonds4*fullprice4)

WeightOf1<-(No.of.Bonds1*fullprice1)/(MarketValueOfPortfolio)
Weightof2<-(No.of.Bonds2*fullprice2)/(MarketValueOfPortfolio)
Weightof3<-(No.of.Bonds3*fullprice3)/(MarketValueOfPortfolio)
Weightof4<-(No.of.Bonds4*fullprice4)/(MarketValueOfPortfolio)

WeightOf1+Weightof2+Weightof3+Weightof4 #Just to Confirm Weights are Right

Mod.Duration.Portfolio<- (WeightOf1*dmod1) + (Weightof2*dmod2) + (Weightof3*dmod3) + (Weightof4*dmod4)
Convexity.Portfolio<-  (WeightOf1*convexity1) + (Weightof2*convexity2) + (Weightof3*convexity3) + (Weightof4*convexity4)

Mod.Duration.Portfolio
Convexity.Portfolio

# Part 2.3

ChangeInR<- 0.001
Approx_Portfolio<- -Mod.Duration.Portfolio*ChangeInR

#First Order Approximation
Approx_Portfolio


# Part 2.4
ChangeInR<-0.001
Second_Order_Approx<-(-Mod.Duration.Portfolio* ChangeInR) + (0.5 * Convexity.Portfolio * ChangeInR^2)
#Second Order Approximation
Second_Order_Approx

#PART 2.5
settle1 <- DATE(2021,12,30)
mature1 <- DATE(2023,05,15)
coupon1 <-0.01750
freq   <- 2
conv   <- "ACT/ACT"
comp.freq <- 2
newyield1<-yield1+ChangeInR
P1 <- bond.price(settle1,mature1,coupon1,freq,newyield1,conv,comp.freq)+accint1

#For Bond Two
settle2 <- DATE(2021,12,30)
mature2 <- DATE(2026,05,15)
coupon2 <-0.02125 
freq   <- 2
conv   <- "ACT/ACT"
comp.freq <- 2
newyield2<-yield2+ChangeInR
P2 <- bond.price(settle2,mature2,coupon2,freq,newyield2,conv,comp.freq)+accint2

#For Bond Three

settle3 <- DATE(2021,12,30)
mature3 <- DATE(2030,05,15)
coupon3 <-0.00625 
freq   <- 2
conv   <- "ACT/ACT"
comp.freq <- 2
newyield3<-yield3+ChangeInR
P3 <- bond.price(settle3,mature3,coupon3,freq,newyield3,conv,comp.freq)+accint3

#For Bond Four

settle4 <- DATE(2021,12,30)
mature4 <- DATE(2050,05,15)
coupon4 <- 0.01250
freq   <- 2
conv   <- "ACT/ACT"
comp.freq <- 2
newyield4<-yield4+ChangeInR
P4 <- bond.price(settle4,mature4,coupon4,freq,newyield4,conv,comp.freq)+accint4

NewMarketValueofPorfolio<-(No.of.Bonds1*P1) + (No.of.Bonds2*P2) + (No.of.Bonds3*P3) + (No.of.Bonds4*P4)

actualchange<-(NewMarketValueofPorfolio-MarketValueOfPortfolio)/MarketValueOfPortfolio
actualchange

# Actual Change with 10 basis points, Answer in Decimal


# Part 2.6
settle1 <- DATE(2021,12,30)
mature1 <- DATE(2023,05,15)
coupon1 <-0.01750
freq   <- 2
conv   <- "ACT/ACT"
comp.freq <- 2
newyield1<-yield1+0.0015
P1 <- bond.price(settle1,mature1,coupon1,freq,newyield1,conv,comp.freq)+accint1

#For Bond Two
settle2 <- DATE(2021,12,30)
mature2 <- DATE(2026,05,15)
coupon2 <-0.02125 
freq   <- 2
conv   <- "ACT/ACT"
comp.freq <- 2
newyield2<-yield2+0.0013
P2 <- bond.price(settle2,mature2,coupon2,freq,newyield2,conv,comp.freq)+accint2

#For Bond Three

settle3 <- DATE(2021,12,30)
mature3 <- DATE(2030,05,15)
coupon3 <-0.00625 
freq   <- 2
conv   <- "ACT/ACT"
comp.freq <- 2
newyield3<-yield3+0.0011
P3 <- bond.price(settle3,mature3,coupon3,freq,newyield3,conv,comp.freq)+accint3

#For Bond Four

settle4 <- DATE(2021,12,30)
mature4 <- DATE(2050,05,15)
coupon4 <- 0.01250
freq   <- 2
conv   <- "ACT/ACT"
comp.freq <- 2
newyield4<-yield4+0.0009
P4 <- bond.price(settle4,mature4,coupon4,freq,newyield4,conv,comp.freq)+accint4

NewMarketValueofPorfolio<-(No.of.Bonds1*P1) + (No.of.Bonds2*P2) + (No.of.Bonds3*P3) + (No.of.Bonds4*P4)

actualchange<-(NewMarketValueofPorfolio-MarketValueOfPortfolio)/MarketValueOfPortfolio
actualchange
# Actual Change with different change in yields for each bonds

#Approximation Method

ElementOne<- WeightOf1* ((-dmod1* 0.0015) + (0.5 * convexity1 * 0.0015^2))
ElementTwo<- Weightof2* ((-dmod2* 0.0013 ) + (0.5 * convexity2 * 0.0013^2))
ElementThree<-Weightof3*((-dmod3* 0.0011) + (0.5 * convexity3* 0.0011^2))
ElementFour<- Weightof4* ((-dmod4* 0.0009) + (0.5 * convexity4 * 0.0009^2))

ApproximateChangeInPortfolio<-ElementOne+ElementTwo+ElementThree+ElementFour
ApproximateChangeInPortfolio


