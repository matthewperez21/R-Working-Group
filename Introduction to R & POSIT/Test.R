library(tidyr)
library(ggplot2)

PPE_Provider_Contact_List <- read_excel("~/Documents/PPE Provider Contact List.xlsx")

PPE_Provider_Contact_List <- PPE_Provider_Contact_List %>%
  pivot_wider(names_from = "Legal Entity", values_from = "Signature Authority Email")

tp_df <- as.data.frame(t(PPE_Provider_Contact_List))


basic_eda <- function(data) {
  glimpse(data)
  df_status(data)
  freq(data)
  profiling_num(data)
  plot_num(data)
  describe(data)
}

basic_eda(client_data)


freq(client_data)

ggplot(data = client_data, mapping= aes(x = AGE, y = MONTHLY_INCOME))+
  geom_point()+
  ggtitle(label = "Relationship between Age and Monthly Income", subtitle = "All Clients")+
  xlab(label = "Age")+
  ylab(label="Monthly Income")
