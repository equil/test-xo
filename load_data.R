library(googlesheets4)

clients <- read_sheet("https://docs.google.com/spreadsheets/d/1Ycg7zTxds9DZnDvTrFcyNNKuTUxg6Yy6WF0a8Wc02WQ/edit#gid=1840297911", "clients")

transactions <- read_sheet("https://docs.google.com/spreadsheets/d/1Ycg7zTxds9DZnDvTrFcyNNKuTUxg6Yy6WF0a8Wc02WQ/edit#gid=1840297911", "transactions")

managers <- read_sheet("https://docs.google.com/spreadsheets/d/1Ycg7zTxds9DZnDvTrFcyNNKuTUxg6Yy6WF0a8Wc02WQ/edit#gid=1840297911", "managers")

leads <- read_sheet("https://docs.google.com/spreadsheets/d/1Ycg7zTxds9DZnDvTrFcyNNKuTUxg6Yy6WF0a8Wc02WQ/edit#gid=1840297911", "leads")

