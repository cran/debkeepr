## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----intro-to-types-----------------------------------------------------------
library(debkeepr)

# Create deb_lsd vector of length 3 with default bases
# representing £17 13s. 11d.,  £32 11s. 8d., and £18 10s. 5d. 
(lsd <- deb_lsd(l = c(17, 32, 18),
                s = c(13, 11, 10),
                d = c(11, 8, 5)))

# Create deb_tetra vector of length 3 with default bases 
# representing £17 13s. 11d. 3f., £32 11s. 8d. 2f., and £18 10s. 5d. 1f.
(tetra <- deb_tetra(l = c(17, 32, 18),
                    s = c(13, 11, 10),
                    d = c(11, 8, 5),
                    f = c(3, 2, 1)))

# Create deb_decimal vector of length 3 with default bases and unit
# representing £15 16s. 6d., £19 5s., and £9 12s. 3d. in decimal form 
(dec <- deb_decimal(x = c(15.825, 19.25, 9.6125)))

# Express the same values in solidus and denarius units
(dec_s <- deb_decimal(x = c(316.5, 385, 192.25), unit = "s"))
(dec_d <- deb_decimal(x = c(3798, 4620, 2307), unit = "d"))

# The same value as a tetrapartite value
(dec_tetra <- deb_decimal(x = c(15.825, 19.25, 9.6125),
                          bases = c(20, 12, 4)))

## ----polish-florin------------------------------------------------------------
# Create deb_lsd vector of length 3 with bases of Polish florin
(lsd_polish <- deb_lsd(l = c(32, 12, 26),
                       s = c(15, 1, 20),
                       d = c(5, 13, 8),
                       bases = c(30, 18)))

# Create deb_decimal vector of length 3 with bases of Polish florin
(dec_polish <- deb_decimal(x = c(15.825, 19.25, 9.6125),
                           bases = c(30, 18)))

## ----hundred-weight-----------------------------------------------------------
# Create a deb_tetra vector to represent avoirdupois weight
deb_tetra(l = c(1, 0, 1),
          s = c(11, 18, 3),
          d = c(1, 0, 3),
          f = c(12, 20, 17),
          bases = c(20, 4, 28))

## ----coercion-----------------------------------------------------------------
# Combine deb_lsd and deb_lsd
c(lsd, deb_lsd(l = 5, s = 13, d = 4))

# Combine deb_decimal and deb_decimal
num <- 17 / 3
c(dec, deb_decimal(num))
c(dec_s, deb_decimal(num, unit = "s"))

# Combine deb_lsd, deb_tetra, deb_decimal, and numeric
c(lsd, tetra, dec, num)

## ----tripartite-tetrapartite--------------------------------------------------
# deb_lsd and deb_tetra with same s and d units
c(lsd, tetra)

# Tetrapartite deb_decimal coerces to tripartite deb_decimal
c(dec_tetra, dec)

## ----unit-coercion------------------------------------------------------------
# farthing -> denarius
c(deb_decimal(x = 5440, unit = "f", bases = c(20, 12, 4)), dec_d)
# denarius -> solidus
c(deb_decimal(x = 1360, unit = "d"), dec_s)
# denarius -> libra
c(deb_decimal(x = 1360, unit = "d"), dec)
# solidus -> libra
c(deb_decimal(x = 340 / 3, unit = "s"), dec)

## ----c-vs-vec_c---------------------------------------------------------------
# Incorrect results with base class as first element
c(num, lsd)
c(num, dec)

# Consistent with vec_c()
library(vctrs)

vec_c(num, lsd)
vec_c(num, dec)

## ----coercion-errors, error = TRUE--------------------------------------------
# Cannot combine vectors with incompatible bases
c(lsd, lsd_polish)
c(tetra, lsd_polish)
c(dec, dec_polish)

## ----casting------------------------------------------------------------------
# Cast between deb_lsd, deb_tetra, and deb_decimal
deb_as_lsd(tetra)
deb_as_tetra(lsd, f = 4)
deb_as_decimal(lsd)
deb_as_decimal(tetra)

# unit is automatically taken into account
deb_as_lsd(dec_s)

# Can cast to any unit of deb_decimal
deb_as_decimal(lsd, unit = "s")
deb_as_decimal(tetra, unit = "f")

# Cast to and from numeric
deb_as_lsd(c(15.825, 19.25, 9.6125))
deb_as_tetra(c(15.825, 19.25, 9.6125))
deb_as_decimal(c(15.825, 19.25, 9.6125))

as.numeric(lsd)
as.numeric(tetra)
as.numeric(dec)

# Cast to character
as.character(lsd)
as.character(tetra)
as.character(dec)

## ----casting with lists-------------------------------------------------------
# deb_lsd()
deb_lsd(l = c(17, 32, 18),
        s = c(13, 11, 10),
        d = c(11, 8, 5))

# Cast from list to deb_lsd()
list(c(17, 13, 11),
     c(32, 11, 8),
     c(18, 10, 5)) %>% 
  deb_as_lsd()

## ----echo = FALSE, out.width = "50%"------------------------------------------
knitr::include_graphics("../man/figures/compound-arithmetic.png", dpi = 300)

## ----addition-----------------------------------------------------------------
# Normalize tripartite value: £132 53s. 35d.
x <- deb_lsd(132, 53, 35)
deb_normalize(x)

# Normalize tetrapartite value: £132 53s. 35d. 21f.
y <- deb_tetra(132, 53, 35, 21)
deb_normalize(y)

# Normalize numeric vector
deb_normalize(c(132, 53, 35))

# The process is the same for non-standard bases such as Polish florin
# Compare this to deb_normalize(x)
deb_lsd(132, 53, 35, bases = c(30, 18)) %>% 
  deb_normalize()

## ----mathematics--------------------------------------------------------------
# Mathematical functions
sum(lsd)
sum(tetra)
sum(dec)
sum(lsd, tetra, dec)
mean(lsd)
    
# Round works on denarius unit of deb_lsd vector and is normalized
round(deb_lsd(9, 19, 11.825))

## ----arithmetic---------------------------------------------------------------
# deb_lsd and deb_lsd
deb_lsd(15, 15, 9) + deb_lsd(6, 13, 4)
deb_lsd(15, 15, 9) / deb_lsd(6, 13, 4)

# deb_tetra and deb_tetra
deb_tetra(15, 15, 9, 3) + deb_tetra(6, 13, 4, 2)

# deb_decimal and deb_decimal
deb_decimal(15.7875) - deb_decimal(20 / 3)

# deb_lsd, deb_tetra, deb_decimal, and numeric
deb_lsd(6, 13, 4) / 2
deb_tetra(15, 15, 9, 3) / 2
deb_decimal(15.7875) + 5.25
18 - deb_decimal(20 / 3)
deb_decimal(15.7875) * 3

# deb_lsd, deb_tetra, and deb_decimal
deb_lsd(15, 15, 9) + deb_tetra(6, 13, 4, 2)
deb_lsd(15, 15, 9) + deb_decimal(20 / 3)
deb_lsd(15, 15, 9) / deb_decimal(15.7875)

## ----comparison, error = TRUE-------------------------------------------------
# Comparison
deb_lsd(15, 15, 9) < deb_lsd(6, 13, 4)
deb_lsd(15, 15, 9) < deb_tetra(6, 13, 4, 2)
deb_lsd(15, 15, 9) == deb_decimal(15.7875)
deb_lsd(6, 13, 4) > 23.5
deb_decimal(15.7875) < deb_decimal(3390, unit = "d")

# Cannot compare vectors with different bases
deb_lsd(15, 15, 9) > lsd_polish

# Maximum and minimum
max(lsd)
min(dec_polish)

# Checking for unique values takes into account normalization
unique(c(deb_lsd(15, 15, 9), deb_lsd(12, 71, 57)))

## ----conversion, error = TRUE-------------------------------------------------
# Convert pounds Flemish to guilders
deb_convert_bases(lsd, to = c(20, 16)) * 6

# Convert units
deb_convert_unit(dec, to = "d")

# Converting units maintains equality; converting bases does not
dec == deb_convert_unit(dec, to = "d")
lsd == deb_convert_bases(lsd, to = c(20, 16))

## ----packages, message = FALSE------------------------------------------------
# load packages
library(tibble)
library(dplyr)
library(ggplot2)

## ----deb_lsd-df---------------------------------------------------------------
# Create data frame with deb_lsd vector
tibble(id = 1:3, lsd = lsd)

# Cretae a data frame with a deb_tetra vector
tibble(id = 1:3, tetra = tetra)

# Data frame from separate unit columns with randomly created data
set.seed(240)
raw_data <- tibble(id = 1:10,
                   group = rep(1:5, 2),
                   pounds = sample(20:100, 10, replace = TRUE),
                   shillings = sample(1:19, 10, replace = TRUE),
                   pence = sample(1:11, 10, replace = TRUE))

(lsd_tbl <- deb_gather_lsd(raw_data,
                           l = pounds, s = shillings, d = pence,
                           replace = TRUE))

## ----dplyr--------------------------------------------------------------------
# deb_lsd work in dplyr pipelines
lsd_tbl %>% 
  filter(lsd > 50) %>% 
  group_by(group) %>% 
  summarise(sum = sum(lsd), .groups = "drop") %>% 
  mutate(dec = deb_as_decimal(sum))

## ----transaction-df-----------------------------------------------------------
# Create transactions data frame
accounts <- c("wheat", "silk", "linen", "cash")
set.seed(24)
(transactions <- lsd_tbl %>% 
  add_column(credit = sample(accounts, 10, replace = TRUE),
             debit = sample(accounts, 10, replace = TRUE),
             .before = 3))

## ----accounts-----------------------------------------------------------------
(trans_summary <- deb_account_summary(transactions, lsd = lsd,
                                      credit = credit, debit = debit))

## ----prep-ggplot--------------------------------------------------------------
dec_summary <- trans_summary %>% 
  mutate(across(where(deb_is_lsd), deb_as_decimal),
         debit = -debit,
         current_text = deb_text(deb_as_lsd(current)))

## ----ex-plot, fig.height = 4, fig.width = 8-----------------------------------
ggplot(data = dec_summary) + 
  geom_linerange(aes(x = account_id, ymin = debit, ymax = credit)) + 
  geom_point(aes(x = account_id, y = current)) + 
  geom_text(aes(x = account_id, y = current, label = current_text), nudge_x = 0.32) + 
  scale_y_continuous(labels = scales::label_dollar(prefix = "\u00a3")) + 
  geom_hline(yintercept = 0) + 
  labs(x = "Accounts",
       y = "Value in pounds",
       title = "Summary of Accounts",
       subtitle = "Total credit, debit, and current values") + 
  theme_light()

