#convert period and start date in numbers

get.pd = function(period1){
  if(period1  == '1 Week'){ pd = 1 }
  else if (period1 == 'Calender Month'){ pd = 2 }
  else if (period1 == 'Quarterly'){ pd = 3 }
  else if (period1 == '6 Month'){ pd = 4 }
  else { pd = 5 }
}


get.st.date = function(days){
  
  if(days == "Mon"){st.date = 2}
  else if (days == "Tue") {st.date = 3}
  else if (days == "Wed") {st.date = 4}
  else if (days == "Thur") {st.date = 5}
  else if (days == "Fri") {st.date = 6}
  else if (days == "Sat") {st.date = 7}
  else if (days == "Sun") {st.date = 1}
  else if (days == "Jan"){st.date = 1}
  else if (days == "Feb") {st.date = 2}
  else if (days == "Mar") {st.date = 3}
  else if (days == "Apr") {st.date = 4}
  else if (days == "May") {st.date = 5}
  else if (days == "Jun") {st.date = 6}
  else if (days == "July") {st.date = 7}
  else if (days == "Aug") {st.date = 8}
  else if (days == "Sep") {st.date = 9}
  else if (days == "Oct") {st.date = 10}
  else if (days == "Nov") {st.date = 11}
  else {st.date = 12}
  
}


#function to order according to heirarchy

ord.heir = function(data2, level){
  
  data2 = data.frame(data2,stringsAsFactors=FALSE) #form the data as data frame format
  colnames(data2) = c("ID", "Period", "Item")
  
  if(level == 1){
    return(data2)
  }
  
  if(level == 2){
    # data2temp = data2
    # data2temp[,3] = "L2a"    #assuming A,B,C,D is L2a
    # data2temp[data2[,3]=="I",,3] = "L2b"    #Assuming I,K,L,M is L2b
    #data2temp[] = "L2c"   #assuming X,Y,Z is L2c
    
    
    # return(data2temp)
    #data2$Item = as.character(data$Item)
    data2 = within(data2, Item[Item == "A" | Item == "B"] <- 'L2a')
    #data2$Item = as.factor(data$Item)
    
    return(data2)
  }
  
  if(level == 3){
    
  }
}
