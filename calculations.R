library(tidyverse)

.managed_leads <- managers %>% 
	merge(leads, by.x="manager_id", by.y="l_manager_id", all.x=FALSE, all.y=TRUE) %>% 
	rename(date = created_at, manager = d_manager, utm = d_utm_source, club = d_club) %>% 
	mutate(utm=replace(utm, is.na(utm), "—")) %>% 
	mutate(manager=replace(manager, is.na(manager), "—")) %>% 
	mutate(club=replace(club, is.na(club), "—"))

leads_count <- .managed_leads %>% 
	mutate(date=replace(date, TRUE, as.Date(date))) %>%
	count(date, manager, club, utm, name="leads_count")

average_transactions_count_per_user_in_a_week <- .managed_leads %>% 
	merge(transactions, by='l_client_id', all.x=TRUE, all.y=FALSE) %>% 
	group_by(date, manager, club, utm, l_client_id) %>% 
		summarize(purchases=sum(ifelse(is.na(created_at), FALSE, date <= created_at & created_at < as.Date(date) + 7))) %>% 
	mutate(date=replace(date, TRUE, as.Date(date))) %>% 
	group_by(date, manager, club, utm) %>% 
		summarize(average_transactions_count_per_user_in_a_week=mean(purchases)) %>%
	as.data.frame()
	
garbage_leads <- .managed_leads %>% 
	mutate(date=replace(date, TRUE, as.Date(date))) %>% 
	group_by(date, manager, club, utm) %>% 
	summarize(garbage_leads_count=sum(l_client_id == "00000000-0000-0000-0000-000000000000")) %>% 
	as.data.frame()
 	
.first_transactions <- transactions %>% 
 	group_by(l_client_id) %>% 
 	summarize(first_transaction=min(created_at))
 
.first_leads <- leads %>% 
 	filter(l_client_id != "00000000-0000-0000-0000-000000000000") %>%
 	group_by(l_client_id) %>%
 	summarize(first_lead=min(created_at))
 	
.viable_managed_leads <- .managed_leads %>% 
	filter(l_client_id != "00000000-0000-0000-0000-000000000000")
 	
new_leads_count <- .viable_managed_leads %>% 
 	merge(.first_transactions, by="l_client_id", all.x=TRUE, all.y=FALSE) %>% 
 	merge(.first_leads, by="l_client_id", all.x=TRUE, all.y=FALSE) %>% 
 	filter(date <= first_lead & ifelse(is.na(first_transaction), TRUE, date <= first_transaction) ) %>% 
 	mutate(date=replace(date, TRUE, as.Date(date))) %>% 
 	count(date, manager, club, utm, name="new_leads_count")
 
new_clients_returns_count <- .viable_managed_leads %>% 
 	merge(.first_transactions, by="l_client_id", all.x=TRUE, all.y=FALSE) %>% 
 	merge(.first_leads, by="l_client_id", all.x=TRUE, all.y=FALSE) %>% 
 	filter(date > first_lead & ifelse(is.na(first_transaction), TRUE, date <= first_transaction) ) %>% 
 	mutate(date=replace(date, TRUE, as.Date(date))) %>% 
 	count(date, manager, club, utm, name="new_clients_returns_count")
 
buyers_count <- .viable_managed_leads %>% 
	merge(transactions, by="l_client_id", all.x=FALSE, all.y=FALSE) %>% 
	group_by(date, manager, club, utm, l_client_id) %>% 
		summarize(buyer=date <= created_at & created_at < as.Date(date) + 7) %>% 
	mutate(date=replace(date, TRUE, as.Date(date))) %>% 
	group_by(date, manager, club, utm) %>% 
		summarize(buyers_count=sum(buyer)) %>%
	as.data.frame()

new_buyers_count <- .viable_managed_leads %>% 
	merge(transactions, by="l_client_id", all.x=FALSE, all.y=FALSE) %>% 
	merge(.first_transactions, by="l_client_id", all.x=TRUE, all.y=FALSE) %>%
	group_by(date, manager, club, utm, l_client_id) %>% 
		summarize(buyer = date <= first_transaction & first_transaction < as.Date(date) + 7) %>% 
	mutate(date=replace(date, TRUE, as.Date(date))) %>% 
	group_by(date, manager, club, utm) %>% 
		summarize(new_buyers_count=sum(buyer)) %>%
	as.data.frame()

week_revenue <- .viable_managed_leads %>% 
	merge(transactions, by="l_client_id", all=FALSE) %>% 
	mutate(in_week_purchase=ifelse(date <= created_at & created_at < as.Date(date) + 7, m_real_amount, 0)) %>% 
	mutate(date=replace(date, TRUE, as.Date(date))) %>% 
	group_by(date, manager, club, utm) %>% 
	summarize(week_revenue=sum(in_week_purchase)) %>% 
	as.data.frame()

week_revenue_per_client <- .viable_managed_leads %>% 
	merge(transactions, by="l_client_id", all=FALSE) %>% 
	mutate(in_week_purchase=ifelse(date <= created_at & created_at < as.Date(date) + 7, m_real_amount, 0)) %>% 
	mutate(date=replace(date, TRUE, as.Date(date))) %>% 
	group_by(date, manager, club, utm, l_client_id) %>% 
	summarize(sum_per_user=sum(in_week_purchase)) %>% 
	ungroup() %>% 
	filter(sum_per_user != 0) %>% 
	group_by(date, manager, club, utm) %>% 
	summarize(week_revenue_per_client=mean(sum_per_user)) %>% 
	as.data.frame()
	
metrics <- leads_count %>% 
	merge(average_transactions_count_per_user_in_a_week, by=c("date", "manager", "club", "utm"), all=TRUE) %>% 
	merge(garbage_leads, by=c("date", "manager", "club", "utm"), all=TRUE) %>% 
	merge(new_leads_count, by=c("date", "manager", "club", "utm"), all=TRUE) %>% 
	merge(new_clients_returns_count, by=c("date", "manager", "club", "utm"), all=TRUE) %>% 
	merge(buyers_count, by=c("date", "manager", "club", "utm"), all=TRUE) %>% 
	merge(new_buyers_count, by=c("date", "manager", "club", "utm"), all=TRUE) %>% 
	merge(week_revenue, by=c("date", "manager", "club", "utm"), all=TRUE) %>% 
	merge(week_revenue_per_client, by=c("date", "manager", "club", "utm"), all=TRUE) %>% 
	mutate(leads_count=replace(leads_count, is.na(leads_count), 0)) %>% 
	mutate(average_transactions_count_per_user_in_a_week =replace(average_transactions_count_per_user_in_a_week, is.na(average_transactions_count_per_user_in_a_week), 0)) %>% 
	mutate(garbage_leads_count=replace(garbage_leads_count, is.na(garbage_leads_count), 0)) %>% 
	mutate(new_leads_count=replace(new_leads_count, is.na(new_leads_count), 0)) %>% 
	mutate(new_clients_returns_count=replace(new_clients_returns_count, is.na(new_clients_returns_count), 0)) %>% 
	mutate(buyers_count=replace(buyers_count, is.na(buyers_count), 0)) %>% 
	mutate(new_buyers_count=replace(new_buyers_count, is.na(new_buyers_count), 0)) %>% 
	mutate(week_revenue_per_client =replace(week_revenue_per_client, is.na(week_revenue_per_client), 0)) %>% 
	mutate(week_revenue=replace(week_revenue, is.na(week_revenue), 0))
	

