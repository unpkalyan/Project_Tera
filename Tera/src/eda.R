library('ProjectTemplate')
load.project()

# Plot A -----------------------------------------------------------------------
# Filtering and plotting the data for the heat map of total render time
hm_data <- prep_data_piv2%>%filter(eventName == "TotalRender")
hm_plot <- ggplot(hm_data, aes(y, -x)) + geom_tile(aes(fill = runtime))
ggsave(file.path("graphs", "Heatmap based on the Total Render time.png"))

# Plot B -----------------------------------------------------------------------
#box plot code to show which event type dominates the run-time
boxp = ggplot(prep_data_piv2, aes(y= runtime, fill = eventName)) + geom_boxplot()
ggsave(file.path("graphs", "Event type vs Run time.png"))


# code to calculate the GPUs with the fastest and slowest average run-times per total render task
avg_rt_data <- prep_data_piv2 %>% group_by(hostname) %>% filter(eventName == "TotalRender") %>% summarise(tot_runtime=sum(runtime), tasks_count = length(table(taskId)), avg_rt = tot_runtime/tasks_count)
scplot <- avg_rt_data[order(avg_rt_data$avg_rt),]
scplot1 <- head(scplot, n=15)
scplot2 <- tail(scplot, n=15)
scplot_final <- rbind(scplot1, scplot2)
scplot_final <- scplot_final %>% order_by(avg_rt)


# Plot C -----------------------------------------------------------------------
# scatter plot for just the fastest and slowest 15 GPUs
plot<- ggplot(scplot_final, aes(x =reorder(hostname, desc(-avg_rt)) , y = avg_rt)) + geom_col() + labs(title = "GPUs with the 15 least and 15 highest average task run times per Total Render", y = "Average run-time per a total render task (in seconds)", x = "GPUs") + theme(axis.text.x=element_blank()) + geom_text(aes(label=round(avg_rt, 2)))
ggsave(file.path("graphs", "15 slowest and fastest GPUs.png"))


# Plot D -----------------------------------------------------------------------
# scatter plot for all the 1024 GPUs
plot1<- ggplot(scplot, aes(x = hostname, y = avg_rt)) + geom_point() + geom_hline(yintercept = min(scplot$avg_rt), color="green", linetype="dashed") + geom_hline(yintercept = max(scplot$avg_rt), color="red", linetype="dashed") + theme(axis.text.x=element_blank()) + labs(title = "Average task run times of 1024 GPUs per Total Render task", y = "Average run-time per a total render task (in seconds)", x = "1024 GPUs")
ggsave(file.path("graphs", "Avg runtime per total Render task of 1024 GPUs.png"))


# Code to calculate the Average Power Consumption of each of the 1024 GPUs
group2 <- gpu %>% group_by(hostname) %>% summarise(tot_pwr=sum(powerDrawWatt), count=length(table(timestamp)))
group2$avgpwr <- round((group2$tot_pwr/group2$count), 2)


# Plot E -----------------------------------------------------------------------
# Scatter Plot to show the average power consumption of the 1024 GPUs
plot3 <- ggplot(group2, aes(hostname, avgpwr))+geom_point()
ggsave(file.path("graphs", "Average Power consumption of the 1024 GPUs.png"))



# Code to calculate idleness of the GPUs
testing3 <- gpu %>% filter(gpuUtilPerc == 0)
testing3 <- testing3 %>% group_by(hostname) %>% summarise(instances = length(gpuUtilPerc))
testing4 <- gpu %>% group_by(hostname) %>% summarise(tot_instances = length(gpuUtilPerc))
testing5 <- full_join(testing4, testing3, by="hostname")
testing5$idle_insta_per <- round((testing5$instances/testing5$tot_instances)*100, 2)


# Plot F -----------------------------------------------------------------------
# scatter plot to show the percentage of time the GPUs are Idle
plot2 <- ggplot(testing5, aes(hostname, idle_insta_per)) + geom_point() + geom_hline(yintercept = min(testing5$idle_insta_per), color="blue", linetype="dotted") + labs(x= "GPUs", y= "Idleness %", title= "Idleness of the 1024 GPUs")
ggsave(file.path("graphs", "Idleness of the 1024 GPUs.png"))