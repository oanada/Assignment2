# Load R packages
library(shiny)
library(shinythemes)
library(shinyBS)
library(ggplot2)
library(gridExtra)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("Exploration of the March 2020 US census data for adult persons using clustering"),

                  tabPanel("Population segmentation",
                           sidebarLayout(
                           sidebarPanel(
                             tags$style(type='text/css', #"label{ display: table-cell} .form-group { display: table-row;}",
                                        ".selectize-label { font-size: 12px; line-height: 8px;} .selectize-input { font-size: 12px; line-height: 8px;} .selectize-dropdown { font-size: 12px; line-height: 8px; }"),
                             
                             actionButton(inputId = "update", label = "Find which cluster the person belongs to"),
                             
                             strong(verbatimTextOutput(outputId= "txtout_rec")), 
                             
                             tags$h4("Select Person Profile:"),
                             selectInput(inputId = "var11", label = "Work sector", choices =c('Federal gov','Local gov','Never worked','Not in universe or children and Armed Forces ','Private','Self-emp inc.','Self-emp not-inc.','State gov','Without pay')),
                             selectInput(inputId = "var12", label = "Occupation", choices = c('Armed Forces','Construction, extraction','Farming, fishing, forestry','Installation, maintenance, repair','Management, business, financial ','NIU or children','Office-admin support','Production','Professional & related','Sales & related','Service','Transportation, material moving')),
                             selectInput(inputId = "var13", label = "Education", choices =c('0. Children','01. Preschool','02. 1st-4th','03. 5th-8th','04. 9th-12th','05. HS-grad','06. Some-college','07. Assoc-voc','08. Assoc-acdm','09. Prof-school','10. Bachelors','11. Masters','12. PhD')),
                             selectInput(inputId = "var21", label = "Age", choices =c('(16-18]','(18-22]','(20-25]','(24-30]','(30-35]','(35-40]','(40-45]','(45-50]','(50-55]','(55-60]','(60-65]','(65-70]','(70-75]','(75+]')),
                             selectInput(inputId = "var22", label = "Marital Status", choices =c('Divorced','Married-AF-spouse','Married-civ-spouse','Married-spouse-absent','Never-married','Separated','Widowed')),
                             selectInput(inputId = "var23", label = "Relationship", choices = c('Husband','Not-in-family','Other-relative','Own-child','Unmarried','Wife')),
                             selectInput(inputId = "var24", label = "Gender", choices = c("Female","Male")),
                             selectInput(inputId = "var31", label = "Native Culture", choices =c('AFRIC','ANGLO','CARIB','CONFUC','EST_EUR','GERMAN','India','LAT_AME','LAT_EUR','Mexico','MID_EAST','NORDIC','Other','PAC_ISL','SE_ASIA','United States')),
                             selectInput(inputId = "var32", label = "Race",choices =c(' Am.Indian/Alaskan Nat. only',' Asian only',' Black only',' Hawai/Pac. Islander only',' White only','Black-Other','Other','White-Other')),
                             selectInput(inputId = "var41", label = "Metropoly size", choices =c('0. NI/nonmetropolitan','1.  100,000-249,999','2.  250,000-499,999','3.  500,000-999,999','4. 1,000,000-2,499,999','5. 2,500,000-4,999,999','6. 5,000,000+')),
                             selectInput(inputId = "var42", label = "State", choices =c('Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware','District of Columbia','Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico','New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin','Wyoming')),
                             selectInput(inputId = "var51", label = "Income", choices =c('(   0-10K]','(  10-25K]','(  25-50K]','(  50-70K]','(  70-100K]','( 100-150K]','( 150-200K]','( 200-300K]','( 300-360K]','( 360-500K]','( 500-1000K]','(1000K+')),
                             selectInput(inputId = "var52", label = "Capital gains", choices =c('0','(  0-10K]','( 20-40K]','( 40-85K]','( 85-200K]','(200-400K]','(400k+')),
                             selectInput(inputId = "var53", label = "Hours per week", choices = c('  NIU',' Variable','0','(0-15h]','(15-20h]','(20-25h]','(25-30h]','(30-35h]','(35-40h]','(40-45h]','(45-50h]','(50-55h]','(55-60h]','(60h+')),
                             
                             width = 4), # sidebarPanel
                           
                           
                           mainPanel(
                             bsCollapsePanel("Clusters mapping. Please wait for US Census data to be processed and the graph below to be populated (click for graph explanation)",
                                             p("The plot below shows the distribution of FOUR distinct clusters in the population against the two  principal component scales which best presents the variance."),
                                              p("These clusters were identified using the K-means unsupervised learning algorithm based on the overall population's profile.")
                                             ),

                             
                             plotOutput(outputId = "plot_1"),
                             
                             
                             selectInput(inputId = "var91", label = "Select criteria for comparing clusters",multiple = FALSE, 
                                         choices =c("Work sector","Occupation","Education","Age","Marital status","Relationship","Gender","Native Culture","Race","Metropoly size","State","Income","Capital gains","Hours per week")),
                             
                             actionButton(inputId = "update_plot", label = "Visualize"),
                             
                             #strong(verbatimTextOutput(outputId= "txtout_rec2")),
                             
                             plotOutput(outputId = "plot_9"),
                             plotOutput(outputId = "plot_10")
                             
                           ) # mainPanel
                  )), 
                  

) # fluidPage



# Define server function  
server <- function(input, output, session) 
{


  ####   ======================================
  ####   K-means analysis
  ####   ======================================
  
  require(ggplot2)
 
  
  set.seed(99)
  
  INPUT_file_path="https://raw.githubusercontent.com/oanada/Assignment2/main/US_Census_2020.csv"
  raw_df<-read.csv(INPUT_file_path, header = TRUE);
  
  
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
  
  df<-raw_df[,(names(raw_df) %in% INPUT_model_vars[,1])]
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
  df[sapply(df, is.factor)] <- lapply(df[sapply(df, is.factor)], as.integer)
  
  dt<-setDT(df)
  scaled_dt<-scale(dt)
  
  pop_cluster<-kmeans(scaled_dt,centers = 4,iter.max = 20,nstart=50)
  
  pca<-prcomp(scaled_dt, scale = FALSE)
  pca_scaled_dt <- predict(pca, newdata = scaled_dt)
  
  cluster_pca_scaled_dt <- cbind(pca_scaled_dt, cluster = pop_cluster$cluster)
  cluster_pca_scaled_df <- as.data.frame(cluster_pca_scaled_dt)
  
  cluster_raw_df <- cbind(raw_df, cluster = pop_cluster$cluster)
  
  final_df <- cbind(raw_df,PC1=cluster_pca_scaled_df$PC1,PC2=cluster_pca_scaled_df$PC2,cluster=cluster_pca_scaled_df$cluster)
  final_df<-select(final_df,-fnlwgt)
  
  pc1_model<-lm(PC1 ~.,select(final_df,-PC2,-cluster))
  pc2_model<-lm(PC2 ~.,select(final_df,-PC1,-cluster))
  
  
  ####   ======================================
  ####   Visualize K-means population clusters
  ####   ====================================== 
  
 INPUT_colors_categories=c( "#A50026",  "#F46D43", "#FDAE61", "#FEE090",  "#ABD9E9", "#74ADD1", "#4575B4" ,"#313695","#7FBC41" ,"#E0F3F8", "#BF812D","#D73027", "#FFFFBF", "#8C510A")  
  final_plot_df<-cluster_pca_scaled_df
  # final_plot_df$cluster[final_plot_df$cluster==1]<-'Gold'
  #  final_plot_df$cluster[final_plot_df$cluster==2]<-'Silver'
  #  final_plot_df$cluster[final_plot_df$cluster==3]<-'Bronze'
  #  final_plot_df$cluster[final_plot_df$cluster==4]<-'Platinum'
  #  final_plot_df$cluster <- factor(final_plot_df$cluster, levels = c('Platinum','Gold','Silver','Bronze'))

  ggObj<- ggplot(final_plot_df, aes(PC1,PC2))+
    geom_point(aes(color = as.factor(cluster)),size=1) +
    guides(color = guide_legend(override.aes = list(size = 3)))+
    scale_colour_manual(values =INPUT_colors_categories)+
    labs(color="Cluster",fill="Cluster", 
         x='PC1 - combination of personal caracteristics explaining most the dispersion in the data ',
         y='PC2 - explaining 2nd most the dispersion in the data',  
         title="Cluster distribution by first two PC - Pricipal Components")+
    theme(text = element_text(size=13), plot.title = element_text(size=14)) 
  
    output$plot_1 <- renderPlot(ggObj)
  
  
  
  
  ####   ======================================
  ####   Inputs on the app page that can chage
  ####   ======================================

  observe
    ({
    updateSelectInput(session, "var11", choices = unique(final_df$workclass))
    updateSelectInput(session, "var12", choices = unique(final_df$occupation))
    updateSelectInput(session, "var13", choices = unique(final_df$education))
    
    updateSelectInput(session, "var21", choices = unique(final_df$age_bins))
    updateSelectInput(session, "var22", choices = unique(final_df$marital_status))
    updateSelectInput(session, "var23", choices = unique(final_df$relationship))
    updateSelectInput(session, "var24", choices = unique(final_df$sex))
    
    updateSelectInput(session, "var31", choices = unique(final_df$native_country))
    updateSelectInput(session, "var32", choices = unique(final_df$race))
    
    updateSelectInput(session, "var41", choices = unique(final_df$metropoly_size))
    updateSelectInput(session, "var42", choices = unique(final_df$state))
    
    updateSelectInput(session, "var51", choices = unique(final_df$Wages_n_salary_bins))
    updateSelectInput(session, "var52", choices = unique(final_df$capital_gains_bins))
    updateSelectInput(session, "var53", choices = unique(final_df$hours_per_week_bins))
    
    updateSelectInput(session, "var91", choices = unique(INPUT_model_vars[,2]))
    })
  
  
  
  
  
  observeEvent(
    input$update,
    {
    new_cust<-as.data.frame(t(c(input$var11,input$var12,input$var13,
                                input$var21,input$var22,input$var23,
                                input$var24,input$var31,input$var32,
                                input$var41,input$var42,input$var51,
                                input$var52,input$var53)))
    
    colnames(new_cust)<-INPUT_model_vars[,2]
  

  
    new_cust$cluster=1
    output$txtout_rec <- renderText({
                         paste( "The input customer falls in cluster: ", new_cust$cluster)
                          })
    }
    )
  
  
  
  
  
  
  
  
  ####   =================================
  ####   Visualize clusters details
  ####   =================================
  
  # default graph
      INPUT_model_vars_selected<-c('workclass',"Work sector")
      
      final_df$VAR_TGT<-final_df[,INPUT_model_vars_selected[1]]
      
      p1 <- ggplot(final_df, aes(x=VAR_TGT)) +
        geom_bar(aes(fill=as.factor(cluster)), width = 0.5)+
        theme(axis.text.x=element_text(angle=90),text = element_text(size=13), plot.title = element_text(size=14))+ 
        labs( fill="Cluster", y="Number of persons",x=INPUT_model_vars_selected[2], title=paste0("No persons by ",INPUT_model_vars_selected[2]," and Cluster"))+
        scale_fill_manual(values =INPUT_colors_categories)
      
      p11 <- ggplot(final_df, aes(x=VAR_TGT, fill = as.factor(cluster))) +
        geom_bar(position = "fill", width = 0.5) +
        stat_count(position=position_fill(vjust=0.5))+
        theme(axis.text.x=element_text(angle=90),text = element_text(size=13), plot.title = element_text(size=14))+ 
        scale_y_continuous(labels=scales::percent)+
        labs(fill="Cluster", x=INPUT_model_vars_selected[2], title=paste0("Cluster distribution by ", INPUT_model_vars_selected[2]))+
        scale_fill_manual(values =INPUT_colors_categories)
      
      
      
      p21 <- ggplot(final_df, aes(x=cluster)) +
        geom_bar(aes(fill=as.factor(VAR_TGT)), width = 0.5)+
        theme(axis.text.x=element_text(angle=90),text = element_text(size=13), plot.title = element_text(size=14))+ 
        labs( fill=INPUT_model_vars_selected[2], y="Number of persons",x="Cluster", title=paste0("No persons by Cluster and ",INPUT_model_vars_selected[2]))+
        scale_fill_manual(values =INPUT_colors_categories)
      
      
      p211 <- ggplot(final_df, aes(x=cluster, fill = as.factor(VAR_TGT))) +
        geom_bar(position = "fill", width = 0.5) +
        stat_count(position=position_fill(vjust=0.5))+
        theme(axis.text.x=element_text(angle=90),text = element_text(size=13), plot.title = element_text(size=14))+ 
        scale_y_continuous(labels=scales::percent)+
        labs(fill=INPUT_model_vars_selected[2], x="Cluster", title=paste0(INPUT_model_vars_selected[2]," distribution by Cluster "))+
        scale_fill_manual(values =INPUT_colors_categories)
      
      require(ggplot2)
      
      output$plot_9<-renderPlot(grid.arrange( p1, p11,  ncol = 2))
      output$plot_10<-renderPlot(grid.arrange( p21, p211, ncol = 2))
  
  
  
  observeEvent(
    input$update_plot,
    {
    INPUT_model_vars_selected<-INPUT_model_vars[INPUT_model_vars[,2]==input$var91,1]
    
    
    #output$txtout_rec2 <- renderText(INPUT_model_vars_selected)
    
    
    INPUT_model_vars_selected<-c(INPUT_model_vars_selected,input$var91)
    
    final_df$VAR_TGT<-final_df[,INPUT_model_vars_selected[1]]
    
    p1 <- ggplot(final_df, aes(x=VAR_TGT)) +
      geom_bar(aes(fill=as.factor(cluster)), width = 0.5)+
      theme(axis.text.x=element_text(angle=90),text = element_text(size=13), plot.title = element_text(size=14))+ 
      labs( fill="Cluster", y="Number of persons",x=INPUT_model_vars_selected[2], title=paste0("No persons by Cluster and ",INPUT_model_vars_selected[2]))+
      scale_fill_manual(values =INPUT_colors_categories)
    
    p11 <- ggplot(final_df, aes(x=VAR_TGT, fill = as.factor(cluster))) +
      geom_bar(position = "fill", width = 0.5) +
      stat_count(position=position_fill(vjust=0.5))+
      theme(axis.text.x=element_text(angle=90),text = element_text(size=13), plot.title = element_text(size=14))+ 
      scale_y_continuous(labels=scales::percent)+
      labs(fill="Cluster", x=INPUT_model_vars_selected[2], title=paste0("Cluster distribution by ", INPUT_model_vars_selected[2]))+
      scale_fill_manual(values =INPUT_colors_categories)
    
    
    
    p21 <- ggplot(final_df, aes(x=cluster)) +
      geom_bar(aes(fill=as.factor(VAR_TGT)), width = 0.5)+
      theme(axis.text.x=element_text(angle=90),text = element_text(size=13), plot.title = element_text(size=14))+ 
      labs( fill=INPUT_model_vars_selected[2], y="Number of persons",x="Cluster", title=paste0("No persons by Cluster and ",INPUT_model_vars_selected[2]))+
      scale_fill_manual(values =INPUT_colors_categories)
    
    
    p211 <- ggplot(final_df, aes(x=cluster, fill = as.factor(VAR_TGT))) +
      geom_bar(position = "fill", width = 0.5) +
      stat_count(position=position_fill(vjust=0.5))+
      theme(axis.text.x=element_text(angle=90),text = element_text(size=13), plot.title = element_text(size=14))+ 
      scale_y_continuous(labels=scales::percent)+
      labs(fill=INPUT_model_vars_selected[2], x="Cluster", title=paste0(INPUT_model_vars_selected[2]," distribution by Cluster "))+
      scale_fill_manual(values =INPUT_colors_categories)
    
    require(ggplot2)
  
    
    
    output$plot_9<-renderPlot(grid.arrange( p1, p11,  ncol = 2))
    output$plot_10<-renderPlot(grid.arrange( p21, p211, ncol = 2))
      
    }
    )

  
    
    

}# server




# Create Shiny object
shinyApp(ui = ui, server = server)