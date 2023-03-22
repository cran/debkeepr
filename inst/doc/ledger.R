## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load debkeepr and data, message = FALSE----------------------------------
# load packages
library(debkeepr)
library(dplyr)
library(ggplot2)

# load data
transactions <- dafforne_transactions
accounts <- dafforne_accounts

## ----summary of transactions--------------------------------------------------
(transactions <- transactions %>% 
   mutate(dec = deb_as_decimal(lsd)) %>% 
   select(id, credit, debit, date, lsd, dec))

## ----credit-transactions, fig.height = 4, fig.width = 7-----------------------
ggplot(data = transactions) + 
  geom_point(aes(x = credit, y = dec), alpha = 0.7) + 
  scale_y_continuous(labels = scales::label_dollar(prefix = "\u00a3")) + 
  labs(x = "Creditor accounts",
       y = "Transaction values",
       title = "Value of Transactions by Creditor Account") + 
  theme_light()

## ----transactions, fig.height = 4, fig.width = 8------------------------------
ggplot(data = transactions) + 
  geom_point(aes(x = credit, y = dec, color = "Credit"), alpha = 0.7) + 
  geom_point(aes(x = debit, y = dec, color = "Debit"), alpha = 0.7) + 
  scale_color_manual(values = c(Credit = "black", Debit = "red")) + 
  scale_y_continuous(labels = scales::label_dollar(prefix = "\u00a3")) + 
  labs(x = "Accounts",
       y = "Transaction values",
       color = "Relationship",
       title = "Value of Transactions by Accounts") + 
  theme_light()

## ----account_summary----------------------------------------------------------
# Summary of accounts
deb_account_summary(transactions)

## ----account-summary, fig.height = 4, fig.width = 8---------------------------
# Plot summary of accounts
deb_account_summary(transactions, lsd = dec) %>% 
  mutate(debit = -debit) %>% 
  ggplot() + 
	  geom_linerange(aes(x = account_id, ymin = debit, ymax = credit)) + 
	  geom_point(aes(x = account_id, y = current,
	                 color = if_else(current == 0, "Closed", "Open"))) +
	  scale_color_manual(values = c(Open = "black", Closed = "blue")) + 
      scale_y_continuous(labels = scales::label_dollar(prefix = "\u00a3")) + 
      labs(x = "Accounts",
           y = "Pounds sterling",
           color = "Status",
           title = "Summary of the accounts") + 
    theme_light()

## ----top credit---------------------------------------------------------------
deb_credit(transactions) %>%
  filter(lsd > 1900) %>%
  left_join(select(accounts, id, account), by = c("account_id" = "id")) %>% 
  arrange(desc(lsd))

## ----network-analysis, warning = FALSE, message = FALSE-----------------------
library(igraph)
library(ggraph)

# Nodes: Total debit for each account
debits <- deb_debit(transactions, lsd = lsd) %>% 
  mutate(val = as.numeric(lsd)) %>% 
  left_join(select(accounts, id, investor), by = c("account_id" = "id"))

# Edges: Sum of transactions by link
transactions_sum <- transactions %>% 
  group_by(credit, debit) %>% 
  summarise(lsd = sum(lsd), .groups = "drop") %>% 
  mutate(val = as.numeric(lsd)) %>% 
  arrange(val)

# Create igraph object
ledger_graph <- graph_from_data_frame(d = transactions_sum,
                                      vertices = debits,
                                      directed = TRUE)

## ----ledger-graph, fig.height = 6, fig.width = 8------------------------------
# Ledger graph
set.seed(240)
ggraph(ledger_graph, layout = "kk") + 
  geom_edge_fan(aes(alpha = val),
                width = 1,
                arrow = arrow(length = unit(2, 'mm'),
                              type = "closed"),
                end_cap = circle(3, 'mm')) + 
  scale_edge_alpha(labels = scales::dollar_format(prefix = "£")) + 
  geom_node_point(aes(size = val, color = investor), alpha = 0.9) + 
  scale_color_brewer(palette = "Paired", direction = -1) + 
  geom_node_text(aes(label = name)) + 
  scale_size_continuous(range = c(1, 10),
                        labels = scales::dollar_format(prefix = "£")) + 
  labs(color = "Investor",
       size = "Accumulated Value",
       edge_alpha = "Accumulated \n Transactions",
       title = "Network of Dafforne's Journal") + 
  theme_graph()

## ----degree, eval = FALSE-----------------------------------------------------
#  # 10 accounts with the most connections
#  sort(degree(ledger_graph), decreasing = TRUE)[1:10]

## ----table degree, echo = FALSE-----------------------------------------------
# Present as a table
as.list(sort(degree(ledger_graph), decreasing = TRUE)[1:10]) %>% 
  as_tibble() %>% 
  knitr::kable(caption = "Connections per account")

## ----links and accounts-------------------------------------------------------
# Total links per investor
total_links <- tibble(id = as.numeric(names(degree(ledger_graph))),
                      links = degree(ledger_graph)) %>% 
  left_join(select(accounts, id, investor), by = "id") %>% 
  group_by(investor) %>% 
  summarise(links = sum(links), .groups = "drop") %>% 
  arrange(desc(links))

# Accounts per investor
accounts_investor <- accounts %>% 
  group_by(investor) %>% 
  summarise(accounts = n(), .groups = "drop")

# Create table
left_join(total_links, accounts_investor, by = "investor") %>% 
  knitr::kable(caption = "Links and accounts per investor")

## ----varino debits------------------------------------------------------------
(varino_debits <- filter(debits, account_id %in% 40:42))

# Total debit
sum(varino_debits$lsd)

## ----stock ledger-------------------------------------------------------------
# Stock is debtor
filter(transactions, debit == 2)

# Stock is creditor
filter(transactions, credit == 2)

## ----summary of stock---------------------------------------------------------
# Summary of stock account
deb_account(transactions, account_id = 2)

## ----stock to total-----------------------------------------------------------
# Percentage of bookkeeper's stock to total capital
deb_lsd(1856, 3, 9) / deb_lsd(2006, 3, 9)

## ----summary of profits and losses--------------------------------------------
# Summary of profit and loss
deb_account(transactions, account_id = 23)

## ----profits and losses-------------------------------------------------------
# Percentage of profits by account
transactions %>% 
  filter(credit == 23) %>% 
  group_by(debit) %>% 
  summarise(lsd = sum(lsd), .groups = "drop") %>% 
  mutate(pct = lsd / deb_lsd(1046, 8, 10) * 100) %>% 
  left_join(accounts, by = c("debit" = "id")) %>% 
  select(id = debit, account, lsd, pct) %>%
  arrange(desc(pct))

## ----open accounts------------------------------------------------------------
# Open accounts: Arranged from credits to debits
(balance <- deb_open(transactions) %>% 
  left_join(select(accounts, id, account), by = c("account_id" = "id")) %>% 
  arrange(desc(lsd)))

## ----total capital------------------------------------------------------------
# Stock at the close of the books
stock_balance <- filter(balance, account_id == 2 | account_id == 23)

sum(stock_balance$lsd)

## ----growth percentage--------------------------------------------------------
# Percentage of growth: profits over opening capital
balance$lsd[[2]] / balance$lsd[[1]]

## ----balance------------------------------------------------------------------
# Total balance remaining on the books
deb_balance(transactions)

# Growth in the capital on the books
(deb_lsd(4599, 11, 0) - deb_lsd(2006, 3, 9)) / deb_lsd(2006, 3, 9)

# Percentage of bookkeeper's stock to total capital
sum(balance$lsd[1:2]) / deb_lsd(4599, 11, 0)

