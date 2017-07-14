library("ggplot2", lib.loc="/usr/local/lib/R/3.4/site-library")
library("plyr", lib.loc="/usr/local/lib/R/3.4/site-library")
library("scales", lib.loc="/usr/local/lib/R/3.4/site-library")
library("gridExtra", lib.loc="/usr/local/lib/R/3.4/site-library")
library("reshape2", lib.loc="/usr/local/lib/R/3.4/site-library")
library("RColorBrewer", lib.loc="/usr/local/lib/R/3.4/site-library")
library("colorRamps", lib.loc="/usr/local/lib/R/3.4/site-library")
setwd("~/Miracas")
### Dataframe
regenerate_dataframes = T
if (regenerate_dataframes) {
  dr = read.csv('sample_date.csv', stringsAsFactors = F)
  dr$date_add <- as.Date(dr$date_add)
  dr$date_add_month <- as.Date(cut(dr$date_add, breaks = "month"))
  dr = subset(dr, (date_add_month > as.Date("2015-05-01") & (date_add_month < as.Date("2016-06-01") )))
}

number_of_months = length(unique(as.character(dr$date_add_month)))

Cohorts = data.frame()
for (m in unique(as.character(dr$date_add_month))){
  u = subset(dr, dr$date_add_month == m) # data for month m
  ulist = unique(u$user_id) # orders by unique users in month m
  dr1 = subset(dr, dr$user_id %in% ulist)
  dr = subset(dr, !(dr$user_id %in% ulist))
  dr1s = ddply(dr1, .(date_add_month), summarize, total = length(user_id))
  colnames(dr1s) = c("Month", m)
  a = c(m, dr1s[,m])
  a = data.frame(t(a))
  Cohorts = rbind.fill(Cohorts, a)
}

col_names = paste("Month", array(1:number_of_months), sep = " ")
colnames(Cohorts) <- c("Month", col_names)
Cohorts["Month"] <- as.Date(Cohorts$Month)
Cohorts["max"] <- as.numeric(as.character(Cohorts[,2]))

df_plot <- melt(Cohorts, id.vars = c('Month', 'max'), value.name = 'Orders', variable.name = 'Month_after')
df_plot <- na.omit(df_plot)
df_plot["Value"] <- as.numeric(df_plot$Orders)
df_plot["Pc_value"] <- ceiling(df_plot$Value*100/df_plot$max)
df_plot["Percentage"] <- paste(ceiling(df_plot$Value*100/df_plot$max), "%", sep="")
df_plot["Percentage_2"] <- ifelse(df_plot$Percentage == "100%", df_plot$Value , df_plot$Percentage)
df_plot_average = ddply(df_plot, .(Month_after), summarize, av = ceiling(mean(Pc_value)))
df_plot_average = subset(df_plot_average, df_plot_average$Month_after != "Month 1")
df_plot_reduced = subset(df_plot, df_plot$Month_after != "Month 1")
df_plot_envelope = ddply(df_plot_reduced, .(Month_after), summarize, maxpc = max(Pc_value), minpc = min(Pc_value) )
df_plot_average = subset(df_plot_average, !(df_plot_average$Month_after %in% c("Month 10", "Month 11", "Month 12")))
df_plot_reduced = subset(df_plot_reduced, !(df_plot_reduced$Month_after %in% c("Month 12")))

## Cohorts tile chart
hm.palette <- colorRampPalette((brewer.pal(9, 'YlOrRd')), space='Lab', bias=10)
p <- ggplot()
p <- p + geom_tile(data = na.omit(df_plot), aes(x = Month, y = Month_after, fill=Value), width=31, height=1)
p <- p + geom_text(data =  na.omit(df_plot), aes(x = Month, y = Month_after, label = Percentage_2), color = "white")#, fontface = "bold")
p <- p + scale_fill_gradientn(colours = hm.palette(100)) + theme_bw() + coord_flip()
p <- p + theme(panel.background = element_blank(), axis.line.x = element_blank(), panel.border = element_blank())
p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none") + ylab("")
p <- p + xlab("Orders by new users")
print(p)

# p2 <- ggplot()
# ## Average Monthly retention rate
# p2 <- p2 + geom_ribbon(data = df_plot_envelope, aes(x = Month_after, ymax = maxpc, ymin = minpc, group = 1), fill = "grey95")
# p2 <- p2 + geom_path(data = df_plot_average, aes(x = Month_after, y = av, group = 1), color="orange", size=2)
# p2 <- p2 + geom_point(data = df_plot_average, aes(x = Month_after, y = av, group = 1), color="orange", shape=16, size=4)
# p2 <- p2 + ylim(0, 20) + xlab("") + ylab("Average retention percentage")
# p2 <- p2 + theme_bw()
# p2 <- p2 + theme(legend.position="none", panel.border = element_blank(), axis.line = element_line(colour = "black"), panel.grid = element_blank())
# p2 <- p2 + theme(axis.text.x = element_text(angle = 20, hjust = 1))


## Cohorts Monthly retention groups
# p2 <- p2 + geom_point(data = df_plot_reduced, aes(x = Month_after, y = Pc_value, group = Month_after, color = Month_after), size=1, shape=16)
# p2 <- p2 + geom_path(data = df_plot_reduced, aes(x = Month_after, y = Pc_value, group = Month_after, color = Month_after), size=0.2)
# p2 <- p2 + ylim(0, 20)
# print(p2)
