rm(list=ls())
#df_bigdataset=read.csv("C:\\Users\\shiva\\Downloads\\Insight\\complaints.csv")
#df=read.csv("C:\\Users\\shiva\\Downloads\\Insight\\consumer_complaints-master\\insight_testsuite\\test_1\\input\\complaints.csv")

df=read.csv("complaints.csv")#reading the datafile
#b=levels(df_bigdataset$Product)
#products=unique(df_bigdataset$Product)
#years
#companys=unique(df_bigdataset$Company)
#product=unique(df$Product)
#dropping the na values
df=df[!is.na(df$Product),]
df=df[!is.na(df$Date.received),]
df=df[!is.na(df$Company),]
product=levels(factor(df$Product))
company=levels(factor(df$Company))

date <-as.Date(df$Date.received,'%m/%d/%Y')
year <- as.numeric(format(date,'%Y'))
df$year<-year#adding the year 
year_unique=unique(year)#getting the unique years

no_of_complains<-function(year_no,product_no)#function to get the total complaints for that product and year
{
  complaints=0
  for( i in 1:nrow(df))
  {
    if(df$year[i]==year_no & df$Product[i]==product_no)
    {
      complaints=complaints+1
      #print(complaints)
    }
  }
  return (complaints)
  
}
#no_of_complains(year_unique[2],product[2])

company_number<-function(year_no,product_no)#function for getting the number of companies with at least 1 complaint
{company_count=0
for(i in 1:length(company))
{
  for( j in 1:nrow(df))
  {
    if(df$year[j]==year_no & df$Product[j]==product_no & df$Company[j]==company[i] )
    {
      company_count=company_count+1
      #print(paste(company_count,"number of companies",df$year[j],df$Company[j]))
      
      break
      # print(company_count)
    }
  }
}
return(company_count)
}

#company_number(year_unique[2],product[2])



highest_percentage<-function(year_no,product_no)#getting the company with the highest percentage
{
  maximum_cnt=0
  percent_total=0
  for(i in 1:length(company))
  {count=0
  for(j in 1:nrow(df))
  {
    if(df$year[j]==year_no & df$Product[j]==product_no & df$Company[j]==company[i] )
    {
      count=count+1 #counting the company cases in that year
      #print(paste(company_count))
      #print(paste(company_count,"number of companies",df$year[j],df$Company[j]))
    }
    if(count>maximum_cnt)
    {
      maximum_cnt=count
    }
    
  }
  }
  total=no_of_complains(year_no,product_no)#total number of complaints in that year for a product
  percent_total=maximum_cnt/total#getting the percentage
  percent_total=(round((percent_total*100),2))
  #print(paste(total,maximum_cnt,percent_total))
  #print(percent_total)
  return(percent_total)
}
#highest_percentage(year_unique[1],product[1])

# final execution
final<-c()
for(i in 1:length(product) )
{
  for(j in 1:length(year_unique) )
  {
    complaint_nos=no_of_complains(year_unique[j],product[i])
    if(complaint_nos>0)
    {
      no_of_companies=company_number(year_unique[j],product[i])
      percent_highest=highest_percentage(year_unique[i],product[j])
      #print(no_of_companies)
      product_name=product[i]
      print(product_name)
      yr=year_unique[j]
      temp<-c()
      #final<-merge(product_name,yr,complaint_nos,no_of_companies,percent_highest)
      temp<-cbind(product_name,yr,complaint_nos,no_of_companies,percent_highest)
      #print(paste(temp,"temp"))
      final<-rbind(final,temp)
      #print(paste(final,"final"))

    }
    
    
  }
}
final
write.csv(final,"report.csv", row.names = FALSE)


