# Load R packages
library(shiny)
library(shinythemes)
library(shinyBS)
library(plotly)
library(ggplot2)
library(gridExtra)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("Market Segmentation and Product Recommendation System for a Credit Issuing Company"),
                navbarPage(
                  "",
                  tabPanel("Customer Segmentation",
                           sidebarLayout(
                           sidebarPanel(
                             tags$h4("Customer Profile:"),
                             selectInput(inputId = "var1", label = "Currently Enrolled", choices = ""),
                             selectInput(inputId = "var2", label = "Gender", choices = ""),
                             selectInput(inputId = "var3", label = "Occupation", choices = ""),
                             selectInput(inputId = "var4", label = "WorkClass", choices = ""),
                             selectInput(inputId = "var5", label = "Education", choices = ""),
                             selectInput(inputId = "var6", label = "Marital Status", choices = ""),
                             selectInput(inputId = "var7", label = "Relationship", choices = ""),
                             selectInput(inputId = "var8", label = "Race", choices = ""),
                             selectInput(inputId = "var9", label = "Native Country", choices = ""),
                             textInput("var10", "Age", "55"),
                             textInput("var11", "HoursPerWeek", "40"),
                             textInput("var12", "Wages/Salary", "1000"),
                             textInput("var13", "CapitalGain", "100"),
                             selectInput(inputId = "var14", label = "MetroSize", choices = ""),
                             selectInput(inputId = "var15", label = "State", choices = ""),
                             selectInput(inputId = "var16", label = "Region", choices = ""),
                             actionButton(inputId = "update", label = "Fit Customer")
                           ), # sidebarPanel
                           mainPanel(
                             bsCollapsePanel("Clusters and Customer mapping (click here for explanation)",
                                             p("The plot below shows the distribution of FOUR distinct clusters in the population against the two  principal component scales which best presents the variance."),
                                             p("The population dataset used is the UCI Adult data: https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data."),
                                             p("These clusters were identified by unsupervised learning algorithm based on the overall population's profile and manually validated to check it's fit for the business case of this application. The application attempts to map the right credit product (like a credit card) with the right customers based on their risk score which is determined by their mapping to one of the four clusters named as - Platinum, Gold, Silver and Bronze."),
                                             p("The company has FOUR suite of credit products - Platinum, Gold, Silver and Bronze in the descending order of risk scores. The application will recommend the ideal suite for a customer based on their mapped cluster.")
                                             ),
                             plotOutput(outputId = "plot"),
                             strong(verbatimTextOutput(outputId= "txtout_rec")),
                             
                             ## plots for quick cluster comparison
                             selectInput(inputId = "var91", label = "Select characteristic for quick cluster graphical comparison",multiple = FALSE, 
                                         choices =c("Work sector","Occupation","Education","Age","Marital status","Relationship","Gender","Native Culture","Race","Metropoly size","Income","Capital gains","Hours per week","State")),
                             
                             actionButton(inputId = "update_plot", label = "Visualize graphical comparison"),
                             
                             plotOutput(outputId = "plot_10"),
                             plotOutput(outputId = "plot_9")
                             
                             
                             
                           ) # mainPanel
                  )), # Customer Segmentation, tabPanel
                  tabPanel("Platinum", 
                           verticalLayout(
                             strong(p("This cluster has the best credit risk profile")),
                             verbatimTextOutput(outputId= "txtout_age3"),
                             verbatimTextOutput(outputId= "txtout_ed3"),
                             verbatimTextOutput(outputId= "txtout_cg3"),
                             verbatimTextOutput(outputId= "txtout_cl3"),
                             plotly::plotlyOutput(outputId = "plot3")
                           )
                  ),
                  tabPanel("Gold", 
                           verticalLayout(
                             strong(p("This cluster has a good credit risk profile")),
                             verbatimTextOutput(outputId= "txtout_age1"),
                             verbatimTextOutput(outputId= "txtout_ed1"),
                             verbatimTextOutput(outputId= "txtout_cg1"),
                             verbatimTextOutput(outputId= "txtout_cl1"),
                             plotly::plotlyOutput(outputId = "plot1")
                           )
                  ),
                  tabPanel("Silver", 
                           verticalLayout(
                             strong(p("This cluster has an average risk profile")),
                             verbatimTextOutput(outputId= "txtout_age4"),
                             verbatimTextOutput(outputId= "txtout_ed4"),
                             verbatimTextOutput(outputId= "txtout_cg4"),
                             verbatimTextOutput(outputId= "txtout_cl4"),
                             plotly::plotlyOutput(outputId = "plot4")
                           )
                           ),
                  tabPanel("Bronze", 
                           verticalLayout(
                             strong(p("This cluster has a below average credit risk profile")),
                             verbatimTextOutput(outputId= "txtout_age2"),
                             verbatimTextOutput(outputId= "txtout_ed2"),
                             verbatimTextOutput(outputId= "txtout_cg2"),
                             verbatimTextOutput(outputId= "txtout_cl2"),
                             plotly::plotlyOutput(outputId = "plot2")
                           )
                  )
                ) # navbarPage
) # fluidPage

# Define server function  
server <- function(input, output, session) {

  library(dplyr)
  library(data.table)
  library(stringr)
  library(ggplot2)
  library(plotly)
  library(ggthemes) 
  library(FactoMineR)
  library(factoextra)
  library(tidyr)
  library(reshape2)
  library(cluster)
  library(class)
  
  library(gridExtra)
  
  set.seed(919)

  # We will comment the code which calculates the clusters, plots and static models to improve run-time performance of the app. 
  # Instead the models and objects are pre-built using this same code and available in the Git repository, which we load after the commented code.
  
  # raw_df=read.csv("https://github.com/robinmath/US_census_clustering/raw/main/US_Census_2020.csv", header = TRUE)
  # 
  # raw_df<-na.omit(raw_df)
  # raw_df <- dplyr::select(raw_df, -X,-hours_per_week_bins,-age_bins,-Wages_n_salary_bins_50K,-Wages_n_salary_bins,-Wages_n_salary_bins_incometax,-capital_gains_bins)
  # raw_df<-raw_df %>% mutate(across(where(is.character), str_trim))
  # raw_df$hours_per_week[raw_df$hours_per_week<0] <- -1
  # 
  # df<-raw_df
  # 
  # df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
  # df[sapply(df, is.factor)] <- lapply(df[sapply(df, is.factor)], as.integer)
  # 
  # dt<-setDT(df)
  # scaled_dt<-scale(dt)
  # 
  # pop_cluster<-kmeans(scaled_dt,4,nstart=50)
  # 
  # pca<-prcomp(scaled_dt, scale = FALSE)
  # pca_scaled_dt <- predict(pca, newdata = scaled_dt)
  # 
  # cluster_pca_scaled_dt <- cbind(pca_scaled_dt, cluster = pop_cluster$cluster)
  # cluster_pca_scaled_df <- as.data.frame(cluster_pca_scaled_dt)
  # 
  # cluster_raw_df <- cbind(raw_df, cluster = pop_cluster$cluster)
  # 
  # final_df <- cbind(raw_df,PC1=cluster_pca_scaled_df$PC1,PC2=cluster_pca_scaled_df$PC2,cluster=cluster_pca_scaled_df$cluster)
  # # remove variables that can't be part of input for new customer
  # final_df<-select(final_df,-fnlwgt)
  # 
  # #remove unnecessary objects
  # rm(pca,cluster_pca_scaled_dt,df,dt,pca_scaled_dt,scaled_dt)
  # save.image(file="shinyapp_data1.RData")
  
  git_url<-"https://github.com/robinmath/US_census_clustering/raw/main/shinyapp_data1.RData"
  load(url(git_url))
  
  pc1_model<-lm(PC1 ~.,select(final_df,-PC2,-cluster))
  pc2_model<-lm(PC2 ~.,select(final_df,-PC1,-cluster))
  
  final_plot_df<-cluster_pca_scaled_df
  final_plot_df$cluster[final_plot_df$cluster==1]<-'Gold'
  final_plot_df$cluster[final_plot_df$cluster==2]<-'Bronze'
  final_plot_df$cluster[final_plot_df$cluster==3]<-'Platinum'
  final_plot_df$cluster[final_plot_df$cluster==4]<-'Silver'
  final_plot_df$cluster <- factor(final_plot_df$cluster, levels = c('Platinum','Gold','Silver','Bronze'))
  
  # ggObj<- ggplot(final_plot_df, aes(PC1,PC2))+
  #   geom_point(aes(color = as.factor(cluster)),size=1) +
  #   labs(color="Customer Type") +
  #   guides(color = guide_legend(override.aes = list(size = 3)))

  # This will load the plots pre-built and saved. These plots are static for this dataset.
  git_url<-"https://github.com/robinmath/US_census_clustering/raw/main/shinyapp_data2.RData"
  load(url(git_url))
  
  output$plot <- renderPlot(ggObj)
  
  #Cluster4 tab

  output$txtout_age4 <- renderText({
    paste( "Mean of age in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==4,c("age")]))
  })
  
  output$txtout_ed4 <- renderText({
    paste( "Mean of Wages_n_salary in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==4,c("Wages_n_salary")]))
  })
  
  output$txtout_cg4 <- renderText({
    paste( "Mean of capital_gains in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==4,c("capital_gains")]))
  })
  
  output$txtout_cl4 <- renderText({
    paste( "Median of hours_per_week in cluster: ", median(cluster_raw_df[cluster_raw_df$cluster==4,c("hours_per_week")]))
  })
  
  #Cluster1 tab
  
  output$txtout_age1 <- renderText({
    paste( "Mean of age in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==1,c("age")]))
  })
  
  output$txtout_ed1 <- renderText({
    paste( "Mean of Wages_n_salary in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==1,c("Wages_n_salary")]))
  })
  
  output$txtout_cg1 <- renderText({
    paste( "Mean of capital_gains in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==1,c("capital_gains")]))
  })
  
  output$txtout_cl1 <- renderText({
    paste( "Median of hours_per_week in cluster: ", median(cluster_raw_df[cluster_raw_df$cluster==1,c("hours_per_week")]))
  })
  
  #Cluster2 tab
  
  output$txtout_age2 <- renderText({
    paste( "Mean of age in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==2,c("age")]))
  })
  
  output$txtout_ed2 <- renderText({
    paste( "Mean of Wages_n_salary in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==2,c("Wages_n_salary")]))
  })
  
  output$txtout_cg2 <- renderText({
    paste( "Mean of capital_gains in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==2,c("capital_gains")]))
  })
  
  output$txtout_cl2 <- renderText({
    paste( "Median of hours_per_week in cluster: ", median(cluster_raw_df[cluster_raw_df$cluster==2,c("hours_per_week")]))
  })
  
  #Cluster3 tab
  
  output$txtout_age3 <- renderText({
    paste( "Mean of age in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==3,c("age")]))
  })
  
  output$txtout_ed3 <- renderText({
    paste( "Mean of Wages_n_salary in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==3,c("Wages_n_salary")]))
  })
  
  output$txtout_cg3 <- renderText({
    paste( "Mean of capital_gains in cluster: ", mean(cluster_raw_df[cluster_raw_df$cluster==3,c("capital_gains")]))
  })
  
  output$txtout_cl3 <- renderText({
    paste( "Median of hours_per_week in cluster: ", median(cluster_raw_df[cluster_raw_df$cluster==3,c("hours_per_week")]))
  })
  
  cluster_raw_chr_df<-select(cluster_raw_df,-c("hours_per_week","capital_gains","Wages_n_salary","age","fnlwgt"))
  
  ##Cluster1
  
  cluster_raw_chr_plot_df<-cluster_raw_chr_df[cluster_raw_chr_df$cluster==1,-13]
  
  # ggObj1 <- ggplot(gather(cluster_raw_chr_plot_df), aes(value)) + 
  #   geom_histogram(stat = "count", color='steelblue1', fill='steelblue4') + 
  #   facet_wrap(~key, scales = 'free_x') +
  #   theme_few() +
  #   theme(axis.text.x = element_text(angle = 45)) 
  
  output$plot1 <- plotly::renderPlotly(ggplotly(ggObj1, width = 1200, height = 1200))
  
  ##Cluster2
  
  cluster_raw_chr_plot_df<-cluster_raw_chr_df[cluster_raw_chr_df$cluster==2,-13]
  
  # ggObj2 <- ggplot(gather(cluster_raw_chr_plot_df), aes(value)) + 
  #   geom_histogram(stat = "count", color='steelblue1', fill='steelblue4') + 
  #   facet_wrap(~key, scales = 'free_x') +
  #   theme_few() +
  #   theme(axis.text.x = element_text(angle = 45)) 
  
  output$plot2 <- plotly::renderPlotly(ggplotly(ggObj2, width = 1200, height = 1200))
  
  ##Cluster3
  
  cluster_raw_chr_plot_df<-cluster_raw_chr_df[cluster_raw_chr_df$cluster==3,-13]
  
  # ggObj3 <- ggplot(gather(cluster_raw_chr_plot_df), aes(value)) + 
  #   geom_histogram(stat = "count", color='steelblue1', fill='steelblue4') + 
  #   facet_wrap(~key, scales = 'free_x') +
  #   theme_few() +
  #   theme(axis.text.x = element_text(angle = 45)) 
  
  output$plot3 <- plotly::renderPlotly(ggplotly(ggObj3, width = 1200, height = 1200))
  
  ##Cluster4
  
  cluster_raw_chr_plot_df<-cluster_raw_chr_df[cluster_raw_chr_df$cluster==4,-13]
  
  # ggObj4 <- ggplot(gather(cluster_raw_chr_plot_df), aes(value)) + 
  #   geom_histogram(stat = "count", color='steelblue1', fill='steelblue4') + 
  #   facet_wrap(~key, scales = 'free_x') +
  #   theme_few() +
  #   theme(axis.text.x = element_text(angle = 45)) 
  
  # save(ggObj,ggObj1,ggObj2,ggObj3,ggObj4, file = "shinyapp_data2.RData")
  
  output$plot4 <- plotly::renderPlotly(ggplotly(ggObj4, width = 1200, height = 1200))
  
  observe({
    updateSelectInput(session, "var1", choices = unique(final_df$School_enrolment))
    updateSelectInput(session, "var2", choices = unique(final_df$sex))
    updateSelectInput(session, "var3", choices = unique(final_df$occupation))
    updateSelectInput(session, "var4", choices = unique(final_df$workclass))
    updateSelectInput(session, "var5", choices = unique(final_df$education))
    updateSelectInput(session, "var6", choices = unique(final_df$marital_status))
    updateSelectInput(session, "var7", choices = unique(final_df$relationship))
    updateSelectInput(session, "var8", choices = unique(final_df$race))
    updateSelectInput(session, "var9", choices = unique(final_df$native_country))
    updateSelectInput(session, "var14", choices = unique(final_df$metropoly_size))
    updateSelectInput(session, "var15", choices = unique(final_df$state))
    updateSelectInput(session, "var16", choices = unique(final_df$region))
  })
  
  observeEvent(input$update,{
  new_cust<-as.data.frame(t(c(input$var1,input$var2,input$var3,input$var4,input$var5,input$var6,input$var7,input$var8,input$var9,input$var10,input$var11,input$var12,input$var13,input$var14,input$var15,input$var16)))
  colnames(new_cust)<-colnames(final_df[,-c(17,18,19)])
  
  new_cust$age<-as.integer(new_cust$age)
  new_cust$Wages_n_salary<-as.integer(new_cust$Wages_n_salary)
  new_cust$capital_gains<-as.integer(new_cust$capital_gains)
  new_cust$hours_per_week<-as.integer(new_cust$hours_per_week)
  
  pred_PC1<-predict(pc1_model,new_cust)
  pred_PC2<-predict(pc2_model,new_cust)
  new_cust$PC1<-pred_PC1
  new_cust$PC2<-pred_PC2
  
  pred_cluster<-knn(final_df[,c("PC1","PC2")],new_cust[,c("PC1","PC2")],final_df[,"cluster"],k=30)
  new_cust$cluster<-pred_cluster
  
  ggObj_cust<-ggObj +
    geom_point(aes(x=new_cust$PC1,y=new_cust$PC2),colour="black", fill="darkred", shape=23, size=3)
  
  output$plot <- renderPlot(ggObj_cust)
  
  output$txtout_rec <- renderText({
    paste( "The input customer falls in cluster: ", ifelse(new_cust$cluster==1,"Gold",ifelse(new_cust$cluster==2,"Bronze",ifelse(new_cust$cluster==3,"Platinum","Silver"))))
  })
  })
  
  
  ####   =================================
  ####   Visualize plots for quick cluster comparison
  ####   =================================
  
  # colors and variables
  INPUT_colors_categories=c( "#A50026",  "#F46D43", "#FDAE61", "#FEE090",  "#ABD9E9", "#74ADD1", "#4575B4" ,"#313695","#7FBC41" ,"#E0F3F8", "#BF812D","#D73027", "#FFFFBF", "#8C510A",
                             "#A50020",  "#F46D40", "#FDAE50", "#FEE080",  "#ABD9E0", "#74ADD0", "#4575B0" ,"#313660","#7FBC31" ,"#E0F3F0", "#BF811D","#D73010", "#FFFABF", "#8C511A",
                             "#A50026",  "#F46D43", "#FDAE61", "#FEE090",  "#ABD9E9", "#74ADD1", "#4075B4" ,"#313695","#7FBC41" ,"#E0F3F3", "#BF812D","#D73027", "#FFFBBF", "#8C710A",
                             "#A50026",  "#F46D43", "#FDAE61", "#FEE090",  "#ABD9E9", "#74ADD1", "#4575B4" ,"#313895","#7FBC40" ,"#E0F3F8", "#BF812D","#D73027", "#FFFCBF", "#8C520A",
                             "#A50029",  "#F46D47", "#FDAE69", "#FEE091",  "#ABD9E0", "#64ADD1", "#4535B4" ,"#313795","#7FBC49" ,"#E0F3F5", "#BF812D","#D73027", "#FFFDBF", "#8C610A")
  
  INPUT_model_vars<-c('workclass',       "Work sector",  
                      "occupation",      "Occupation",
                      "education",       "Education",
                      "age_bins",        "Age",
                      "marital_status",  "Marital status",
                      "relationship",    "Relationship",
                      'sex',             'Gender',
                      "native_country",  "Native Culture", 
                      "race",            "Race", 
                      "metropoly_size",  "Metropoly size", 
                      "state",           "State", 
                      "Wages_n_salary_bins",  "Income", 
                      "capital_gains_bins",   "Capital gains", 
                      "hours_per_week_bins",  "Hours per week")
  INPUT_model_vars<-matrix(INPUT_model_vars,ncol=2,byrow=TRUE)
  
  
  
  cluster_raw_df$Wages_n_salary_bins<-case_when(
    cluster_raw_df$Wages_n_salary <=10*10^3 ~  "(   0-10K]",
    cluster_raw_df$Wages_n_salary <=25*10^3 ~  "(  10-25K]",
    cluster_raw_df$Wages_n_salary <=50*10^3 ~  "(  25-50K]",
    cluster_raw_df$Wages_n_salary <=70*10^3 ~  "(  50-70K]",
    cluster_raw_df$Wages_n_salary <=100*10^3 ~ "(  70-100K]",
    cluster_raw_df$Wages_n_salary <=150*10^3 ~ "( 100-150K]",
    cluster_raw_df$Wages_n_salary <=200*10^3 ~ "( 150-200K]",
    cluster_raw_df$Wages_n_salary <=300*10^3 ~ "( 200-300K]",
    cluster_raw_df$Wages_n_salary <=360*10^3 ~ "( 300-360K]",
    cluster_raw_df$Wages_n_salary <=500*10^3 ~ "( 360-500K]",
    cluster_raw_df$Wages_n_salary <=1000*10^3 ~"( 500-1000K]",
    cluster_raw_df$Wages_n_salary >1000*10^3 ~ "(1000K+"
  )
  cluster_raw_df$capital_gains_bins<-case_when(
    cluster_raw_df$capital_gains <=0 ~        "   0",
    cluster_raw_df$capital_gains <=10*10^3 ~  "(  0-10K]",
    cluster_raw_df$capital_gains <=40*10^3 ~  "( 20-40K]",
    cluster_raw_df$capital_gains <=85*10^3 ~  "( 40-85K]",
    cluster_raw_df$capital_gains <=200*10^3 ~ "( 85-200K]",
    cluster_raw_df$capital_gains <=400*10^3 ~ "(200-400K]",
    cluster_raw_df$capital_gains >400*10^3 ~  "(400k+"
  )
  
  cluster_raw_df$hours_per_week_bins<-case_when(
    cluster_raw_df$hours_per_week <=-4 ~   " Variable",
    cluster_raw_df$hours_per_week <=-1 ~   "  NIU",
    cluster_raw_df$hours_per_week <=0 ~    " 0",
    cluster_raw_df$hours_per_week <=15 ~   "(0-15h]",
    cluster_raw_df$hours_per_week <=20~   "(15-20h]",
    cluster_raw_df$hours_per_week <=25~   "(20-25h]",
    cluster_raw_df$hours_per_week <=30~   "(25-30h]",
    cluster_raw_df$hours_per_week <=35~   "(30-35h]",
    cluster_raw_df$hours_per_week <=40~   "(35-40h]",
    cluster_raw_df$hours_per_week <=45~   "(40-45h]",
    cluster_raw_df$hours_per_week <=50~   "(45-50h]",
    cluster_raw_df$hours_per_week <=55~   "(50-55h]",
    cluster_raw_df$hours_per_week <=60~   "(55-60h]",
    cluster_raw_df$hours_per_week >60~   "(60h+"
  )
  
  cluster_raw_df$age_bins<-case_when(
    cluster_raw_df$age <=18 ~ "(16-18]",
    cluster_raw_df$age <=22 ~ "(18-22]",
    cluster_raw_df$age <=25 ~ "(20-25]",
    cluster_raw_df$age <=30 ~ "(24-30]",
    cluster_raw_df$age <=35 ~ "(30-35]", 
    cluster_raw_df$age <=40 ~ "(35-40]", 
    cluster_raw_df$age <=45 ~ "(40-45]",
    cluster_raw_df$age <=50 ~ "(45-50]",
    cluster_raw_df$age <=55 ~ "(50-55]",
    cluster_raw_df$age <=60 ~ "(55-60]",
    cluster_raw_df$age <=65 ~ "(60-65]",
    cluster_raw_df$age <=70 ~ "(65-70]",
    cluster_raw_df$age <=75 ~ "(70-75]",
    cluster_raw_df$age >75  ~ "(75+]"
  )
  

  # default graph
  INPUT_model_vars_selected<-c('workclass',"Work sector")
  
  cluster_raw_df$VAR_TGT<-cluster_raw_df[,INPUT_model_vars_selected[1]]
  
  p1 <- ggplot(cluster_raw_df, aes(x=VAR_TGT)) +
    geom_bar(aes(fill=as.factor(cluster)), width = 0.5)+
    theme(axis.text.x=element_text(angle=90),text = element_text(size=13), plot.title = element_text(size=14))+ 
    labs( fill="Clusters", y="Number of persons",x=INPUT_model_vars_selected[2], title=paste0("No persons by ",INPUT_model_vars_selected[2]," and Cluster"))+
    scale_fill_manual(values =INPUT_colors_categories)
  
  p11 <- ggplot(cluster_raw_df, aes(x=VAR_TGT, fill = as.factor(cluster))) +
    geom_bar(position = "fill", width = 0.5) +
    stat_count(position=position_fill(vjust=0.5))+
    theme(axis.text.x=element_text(angle=90),text = element_text(size=13), plot.title = element_text(size=14))+ 
    scale_y_continuous(labels=scales::percent)+
    labs(fill="Clusters", x=INPUT_model_vars_selected[2], title=paste0("Cluster distribution by ", INPUT_model_vars_selected[2]))+
    scale_fill_manual(values =INPUT_colors_categories)
  
  
  
  p21 <- ggplot(cluster_raw_df, aes(x=cluster)) +
    geom_bar(aes(fill=as.factor(VAR_TGT)), width = 0.5)+
    theme(axis.text.x=element_text(angle=90),text = element_text(size=13), plot.title = element_text(size=14))+ 
    labs( fill=INPUT_model_vars_selected[2], y="Number of persons",x="Clusters: 1-Gold, 2-Bronze, 3-Platinum, 4-Silver", title=paste0("No persons by Cluster and ",INPUT_model_vars_selected[2]))+
    scale_fill_manual(values =INPUT_colors_categories)
  
  
  p211 <- ggplot(cluster_raw_df, aes(x=cluster, fill = as.factor(VAR_TGT))) +
    geom_bar(position = "fill", width = 0.5) +
    stat_count(position=position_fill(vjust=0.5))+
    theme(axis.text.x=element_text(angle=90),text = element_text(size=13), plot.title = element_text(size=14))+ 
    scale_y_continuous(labels=scales::percent)+
    labs(fill=INPUT_model_vars_selected[2], x="Clusters: 1-Gold, 2-Bronze, 3-Platinum, 4-Silver", title=paste0(INPUT_model_vars_selected[2]," distribution by Cluster "))+
    scale_fill_manual(values =INPUT_colors_categories)

  require(ggplot2)
  
  output$plot_10<-renderPlot(grid.arrange( p21, p211, ncol = 2))
  output$plot_9<-renderPlot(grid.arrange( p1, p11,  ncol = 2))
  
  
  
  
  observeEvent(
    input$update_plot,
    {
      INPUT_model_vars_selected<-INPUT_model_vars[INPUT_model_vars[,2]==input$var91,1]
      
      
      #output$txtout_rec2 <- renderText(INPUT_model_vars_selected)
      
      
      INPUT_model_vars_selected<-c(INPUT_model_vars_selected,input$var91)
      
      cluster_raw_df$VAR_TGT<-cluster_raw_df[,INPUT_model_vars_selected[1]]
      
      p1 <- ggplot(cluster_raw_df, aes(x=VAR_TGT)) +
        geom_bar(aes(fill=as.factor(cluster)), width = 0.5)+
        theme(axis.text.x=element_text(angle=90),text = element_text(size=13), plot.title = element_text(size=14))+ 
        labs( fill="Cluster", y="Number of persons",x=INPUT_model_vars_selected[2], title=paste0("No persons by Cluster and ",INPUT_model_vars_selected[2]))+
        scale_fill_manual(values =INPUT_colors_categories)
      
      p11 <- ggplot(cluster_raw_df, aes(x=VAR_TGT, fill = as.factor(cluster))) +
        geom_bar(position = "fill", width = 0.5) +
        stat_count(position=position_fill(vjust=0.5))+
        theme(axis.text.x=element_text(angle=90),text = element_text(size=13), plot.title = element_text(size=14))+ 
        scale_y_continuous(labels=scales::percent)+
        labs(fill="Cluster", x=INPUT_model_vars_selected[2], title=paste0("Cluster distribution by ", INPUT_model_vars_selected[2]))+
        scale_fill_manual(values =INPUT_colors_categories)
      
      
      
      p21 <- ggplot(cluster_raw_df, aes(x=cluster)) +
        geom_bar(aes(fill=as.factor(VAR_TGT)), width = 0.5)+
        theme(axis.text.x=element_text(angle=90),text = element_text(size=13), plot.title = element_text(size=14))+ 
        labs( fill=INPUT_model_vars_selected[2], y="Number of persons",x="Clusters: 1-Gold, 2-Bronze, 3-Platinum, 4-Silver", title=paste0("No persons by Cluster and ",INPUT_model_vars_selected[2]))+
        scale_fill_manual(values =INPUT_colors_categories)
      
      
      p211 <- ggplot(cluster_raw_df, aes(x=cluster, fill = as.factor(VAR_TGT))) +
        geom_bar(position = "fill", width = 0.5) +
        stat_count(position=position_fill(vjust=0.5))+
        theme(axis.text.x=element_text(angle=90),text = element_text(size=13), plot.title = element_text(size=14))+ 
        scale_y_continuous(labels=scales::percent)+
        labs(fill=INPUT_model_vars_selected[2], x="Clusters: 1-Gold, 2-Bronze, 3-Platinum, 4-Silver", title=paste0(INPUT_model_vars_selected[2]," distribution by Cluster "))+
        scale_fill_manual(values =INPUT_colors_categories)
      
      require(ggplot2)
      
      
      output$plot_10<-renderPlot(grid.arrange( p21, p211, ncol = 2))
      output$plot_9<-renderPlot(grid.arrange( p1, p11,  ncol = 2))
      
      
    }
  )
  
  
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)