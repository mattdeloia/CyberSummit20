library(blastula)
library(tidyverse)
library(keyring)
library(readxl)
library(DT)
library(Rmisc)
library(knitr)
library(plotly)
library(data.table)
library(formattable)

#read data frames
dfx<- read_excel("JPIRTechnicalManual.xls") %>% distinct(Dimension,Description) %>% select(Dimension, Description)

#idlist<- df %>% distinct(Email) %>% drop_na(Email)
create_smtp_creds_key(
        id = "outlook_creds",
        user = "mattdeloia@outlook.com",
        provider = "outlook",
)

#count of respondants
n_CyberPro <- df %>% filter(CyberPro=="Yes") %>% count(CyberPro) 
n_Not_CyberPro <- df %>% filter (CyberPro=="No") %>% count(CyberPro)

df_scored2b <- df_scored2 %>% drop_na(Email) #%>%  filter(ID %in% c("1")) #Remove filter when going live
df_scored2c <- df_scored2b %>% as.data.frame() %>%   mutate("ID"=rownames(df_scored2b))
df_scored2d <- df_scored2c %>% gather(Anxiety:Traditional_Values, key=Dimension, value=Score) %>%
        mutate(Percentile=(round(pnorm(Score), 2)*100)) %>%
        mutate(Comparison=if_else(Percentile<=31, "low", if_else(Percentile <=69, "average", "high"))) %>% 
        mutate(Group=if_else(Dimension %in% c("Innovation", "Complexity", "Breadth_of_Interest", "Tolerance"), "Analytical", if_else(Dimension %in% c("Empathy", "Anxiety", "Cooperativeness"), "Emotional", if_else(Dimension %in% c("Sociability", "Social_Confidence", "Energy_Level"), "Exroverted", if_else(Dimension %in% c("Social_Astuteness", "Risk_Taking"), "Opportunistic", "Responsibility"))))) %>% 
        left_join(dfx) %>% 
        select(ID, Dimension, Group, Percentile, Comparison, Email, Description, Cognitive_Score)

df_scored2d$Comparison <- factor (df_scored2d$Comparison, levels = c("high", "average", "low"))

###############################################
for (i in 1:80) {
id2 <- df_scored2c %>% filter(ID==i) 
id3 <- id2$Email
date_time <- add_readable_time()

df_scored2e <- df_scored2d %>% filter(Email == id3) 

plot <- df_scored2e %>%    
        ggplot(aes(x=Dimension, y=Percentile, fill=Comparison)) + 
        geom_col() +
        geom_text(aes(x=Dimension, y=Percentile,label = Percentile),  hjust = 1,  size=3.5, color="black") + 
        scale_fill_manual(name="general workforce comparison", values = c( "skyblue", "snow3", "palegreen3")) +
        ylab("percentile") + xlab("") + ylim(0,100) +
        coord_flip() + 
        geom_hline(aes(yintercept=50), color="red", linetype="dashed") +
        theme(legend.title= element_text(color="black", size=10), legend.position = "top") + facet_grid(Group~., scales = "free_y")

cognitive <- mean(df_scored2e$Cognitive_Score) 
table <- df_scored2e %>% select(Dimension, Percentile, Description) %>% arrange(-Percentile) 

button <- add_cta_button("https://mattdeloia.github.io/CyberSummit20/", "Group Analysis" )
plot_email <- add_ggplot(plot)

###################################
email <- compose_email(
        
        body = md( c(
"Thank you for your support of Northrop Grumman and our Columbus, Georgia team supporting Army Research. 

Your individual scores are based on responses submitted to the Cybersecurity Workforce Questionnaire. There are no correct answers and your unique pattern of scores, high and low, should help you understand yourself.",

plot_email,

"Your cognitive score was", cognitive,  "out of 6 correct.",

button
)),

        footer = md( c( "Comments and suggestions to matthew.deloia@ngc.com.  
                        Email sent on ", date_time, "." ) ))
######################        
email %>%
        smtp_send(
                from = "mattdeloia@hotmail.com",
                to = id3,
                subject = "Individual Personality Report",
                credentials = creds_key(id = "outlook_creds")
        ) } 
