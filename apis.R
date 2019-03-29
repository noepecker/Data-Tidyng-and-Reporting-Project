

q=kgl_datasets_list(owner_dataset='drgilermo/nba-players-stats')

t=kgl_datasets_download(owner_dataset='drgilermo/nba-players-stats' ,fileName='Seasons_Stats.csv')
url= t$url

temp <- tempfile()
download.file(url, temp)
a=getwd()
data <- read.csv(file.path(temp2, "Seasons_Stats.csv.zip"))

data <- read.table(unz(temp, "Seasons_Stats.csv.zip"))

df=unzip("Seasons_Stats.csv.zip")

t=kgl_datasets_download(owner_dataset='drgilermo/nba-players-stats' ,fileName='Seasons_Stats.csv')
url= t$url

temp <- tempfile()
download.file(url, temp)

devtools::install_github("jimhester/archive") 
library(archive)
df = read.csv(url)
data <- read.table("Seasons_stats.csv.zip", header=T, quote="\"", sep=",")

temp <- tempfile()
download.file(url,temp)
x=unz(temp, "Seasons_Stats.csv.zip")
data <- read.table(x)
unlink(temp)