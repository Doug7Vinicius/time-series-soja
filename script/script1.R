library(ggplot2)
library(visR)
library(gt)
library(DT)

# Metadata Title
DATASET <- paste0("NCCTG Lung Cancer Dataset (from survival package ", 
                  packageVersion("survival"), ")")

# Save original options()
old <- options()  

# Global formatting options
options(digits = 3)

# Global ggplot settings
theme_set(theme_bw())

# Global table settings 
options(DT.options = list(pageLength = 10, 
                          language = list(search = 'Filter:'), 
                          scrollX = TRUE))

lung_cohort <- survival::lung

# Change gender to be a factor and rename some variables to make output look nicer
lung_cohort <- lung_cohort %>%  
  dplyr::mutate(sex = as.factor(ifelse(sex == 1, "Male", "Female")))  %>%  
  dplyr::rename(Age = "age", Sex = "sex", Status = "status", Days = "time")

# Restore original options()
options(old)

# Select variables of interest and change names to look nicer
lung_cohort_tab1 <- lung_cohort %>%  
  dplyr::select(Age, Sex) 

# Create a table one
tab1 <- visR::get_tableone(lung_cohort_tab1)

# Render the tableone
visR::render(tab1, title = "Overview over Lung Cancer patients", datasource = DATASET)

# Use wrapper functionality to create and display a tableone
visR::tableone(lung_cohort_tab1, title = "Overview over Lung Cancer patients", datasource = DATASET)

# Create and render a tableone with a stratifier and without displaying the total
visR::tableone(lung_cohort_tab1, strata = "Sex", overall = FALSE,
               title = "Overview over Lung Cancer patients", datasource = DATASET)

# Create and render a tableone with with dt as an engine
visR::tableone(lung_cohort_tab1, strata = "Sex", overall = FALSE,
               title = "Overview over Lung Cancer patients", datasource = DATASET, 
               engine = "dt")

# Create and render a tableone with with kable as an engine and html as output format
visR::tableone(lung_cohort_tab1, strata = "Sex", overall = FALSE, 
               title = "Overview over Lung Cancer patients", datasource = DATASET, 
               engine = "kable", output_format="html")








library(RMySQL)

mysqlconnection = dbConnect(RMySQL::MySQL(),
                            dbname='survival',
                            host='douglas@localhost',
                            port=3306,
                            user='douglas',
                            password='douglas.777')

mydb = dbConnect(MySQL(), user='douglas', password='douglas.777', dbname='survival', host='host')



library(DBI)
library(pool)
pool <- dbPool(drv = RMySQL::MySQL(), dbname = "douglas", host = "localhost", username = "root", password = "", port = 3306, unix.sock = "/var/run/mysqld/mysqld.sock")
df <- dbGetQuery(pool, "SELECT * FROM tablename;")
