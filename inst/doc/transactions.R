## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load debkeepr------------------------------------------------------------
# load debkeepr
library(debkeepr)

## ----19. multiplication-------------------------------------------------------
# 19. For Rice's 3/5 part of the 100 says valued at £566.13.4 nr 18
tr18 <- deb_lsd(566, 13, 4)
tr18 * 3 / 5

## ----21. brokerage and provision----------------------------------------------
# 21. For brokerage costs in nr 18 and 20 at 1/8% and provision at 1/3%
sum(tr18 * 1 / 800, tr18 * 1 / 300)

## ----87. interest-------------------------------------------------------------
# 87. For £96.14.9 detained upon interest at 8% for 4 months
deb_lsd(96, 14, 9) * 0.08 * 4 / 12

## ----3. kettles weight--------------------------------------------------------
# 3. Gross weight
(kettles_gross <- deb_lsd(l = c(2, 2, 2, 2, 1),
                          s = c(3, 2, 1, 0, 3),
                          d = c(26, 18, 21, 17, 5),
                          bases = c(4, 28)))

# Tare
(kettles_tare <- deb_lsd(l = 0,
                         s = 0,
                         d = c(23, 21, 22, 19, 17),
                         bases = c(4, 28)))

# Net weight
(kettles_net <- sum(kettles_gross) - sum(kettles_tare))

## ----3. kettles value---------------------------------------------------------
# 3. Decimalize hundredweight
(kettles_num <- as.numeric(kettles_net))

# Value of the kettles
(deb_lsd(4, 19, 0) * kettles_num)

## ----54. sugar----------------------------------------------------------------
# 54. 51 hundredweight 3 quarters 4 pounds of sugar at 13d. per pound
# Weight in decimalized pounds
sugar_lbs <- deb_lsd(51, 3, 4, bases = c(4, 28)) %>% 
  deb_as_decimal(unit = "d") %>% 
  as.numeric()

# Price
deb_lsd(0, 0, 13) * sugar_lbs

## ----54. sugar tetra----------------------------------------------------------
# Normalize 51 hundredweight 3 quarters 4 pounds of sugar as tetrapartite value
deb_tetra(0, 51, 3, 4, bases = c(20, 4, 28)) %>% 
  deb_normalize()

## ----102/103. purchase and sale of sugar--------------------------------------
# 102. 77 hundredweight 2 quarters 20 pounds of sugar
sugar_lbs2 <- deb_lsd(77, 2, 20, bases = c(4, 28)) %>% 
  deb_as_decimal(unit = "d") %>% 
  as.numeric()

# purchase price of the sugar
(sugar_purchase <- deb_lsd(0, 0, 13) * sugar_lbs2)

# 103. profits from the sugar
deb_lsd(0, 0, 1) * sugar_lbs2

## ----105/106. silver bars-----------------------------------------------------
# 105. Payment for sugar
# Value of the 8 silver bars
deb_lsd(0, 6, 7.5) * 1733

# 106. Value of sugar at 13d. plus payment of cash
sum(sugar_purchase, deb_lsd(102, 16, 1))

## ----flemish to sterling------------------------------------------------------
# Rate for sterling to Flemish
sterling_to_flemish <- deb_lsd(0, 33, 4)

# Decimalized Flemish to sterling rate

# Divide deb_lsd vectors
(flemish_to_sterling <- deb_lsd(1, 0, 0) / sterling_to_flemish)

# Numeric method
as.numeric(sterling_to_flemish) ^ -1

# See the rate as a deb_lsd vector
1 / sterling_to_flemish

## ----5. guilders to sterling--------------------------------------------------
# 5. Guilders to Flemish
tr5_guilders <- deb_lsd(2290, 0, 0, bases = c(20, 16))
tr5_flemish <- deb_convert_bases(tr5_guilders, to = c(20, 12)) / 6

# Flemish to sterling
tr5_flemish * flemish_to_sterling

## ----33. guilders to sterling-------------------------------------------------
# 33. 1224 guilders 19s. 8d. through bills of exchange
# Inverse rate
tr33_rate <- deb_lsd(1, 0, 0) / deb_lsd(0, 36, 10)

# Convert bases and do exchange
tr33_guilders <- deb_lsd(1224, 19, 8, bases = c(20, 16))
tr33_flemish <- deb_convert_bases(tr33_guilders, to = c(20, 12)) / 6
tr33_flemish * tr33_rate

## ----40. sale of 60 Leeds dozens----------------------------------------------
# 40. Proceeds from the two sales
(tr40_sale <- sum(deb_lsd(45, 7, 8, bases = c(20, 16)) * 30, 
                  deb_lsd(50, 0, 0, bases = c(20, 16)) * 30))

# Sum of proceeds and conversion from guilders to Flemish
tr40_flemish <- deb_convert_bases(tr40_sale, to = c(20, 12)) / 6

# To sterling
tr40_flemish * flemish_to_sterling

## ----20. crowns to sterling---------------------------------------------------
# 20. For £2148 50s. 6d. French crowns at 63d. sterling
# Rate
as.numeric(deb_lsd(0, 0, 63))

# or with deb_decimal
deb_decimal(63, unit = "d") %>% 
  deb_convert_unit(to = "l") %>% 
  as.numeric()

tr20_crowns <- deb_lsd(2148, 50, 6, bases = c(60, 12))

deb_convert_bases(tr20_crowns, to = c(20, 12)) * 0.2625

## ----58. crowns, guilders, sterling-------------------------------------------
# 58. Crowns to Flemish to guilders
crowns_to_flemish <- as.numeric(deb_lsd(0, 0, 123))
tr58_crowns <- deb_lsd(1140, 17, 8, bases = c(60, 12))
tr58_flemish <- deb_convert_bases(tr58_crowns,
                                  to = c(20, 12)) * crowns_to_flemish
deb_convert_bases(tr58_flemish, to = c(20, 16)) * 6

# Crowns to sterling
crowns_to_sterling <- as.numeric(deb_lsd(0, 0, 72))
deb_convert_bases(tr58_crowns, to = c(20, 12)) * crowns_to_sterling

## ----64 and 65. florins to sterling-------------------------------------------
# 64 and 65. Bills of exchange to Danzig
# Rate of 232 gros per £1 Flemish
tr64_rate <- as.numeric(1 / deb_lsd(0, 232, 0, bases = c(30, 18)))

# Convert florins to sterling through pounds Flemish using the pipe
deb_lsd(l = c(3987, 1907),
        s = c(16, 26),
        d = 0,
        bases = c(30, 18)) %>% 
  deb_convert_bases(to = c(20, 12)) %>% 
  `*`(tr64_rate) %>% 
  `*`(flemish_to_sterling)

## ----51. purchase of figs-----------------------------------------------------
# 51. purchase of 1576 pieces of figs for 681 milréis 960 réis
tr51_reis <- deb_lsd(681, 960, 0, bases = c(1000, 12))
tr51_converted <- deb_convert_bases(tr51_reis, to = c(20, 12))

# Method 1: multiply rate of réis by 4 and invert rate
tr51_converted / as.numeric(deb_lsd(0, 400, 0, bases = c(1000, 10)) * 4)

# Method 2: multiply rate of shillings by 2.5 and convert to decimalized pounds
tr51_rate <- deb_decimal(5 * 2.5, "s") %>% 
  deb_convert_unit(to = "l")
tr51_converted * as.numeric(tr51_rate)

