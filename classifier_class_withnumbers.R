classifier_class_gmatcol_number<-function(dataset,min,max,no_of_classes,class_width)
{
  CC<-vector(mode='list',length=nrow(dataset))#creates an empty list
  upper_class_boundary=min+class_width
  x=1
  z=1
  
  
  while(x<=274)
  {
    while(upper_class_boundary<=max)
    {
      if(dataset[x,"gmat_tot"]<=upper_class_boundary)
      {
        CC[x]<-z#enters each gmat class number into 1-274 rows,starting from 1 to 20
        x=x+1
        break
      }
      
      else
      {
        upper_class_boundary=upper_class_boundary+class_width
        z=z+1
      }
      
    }
    
  }
  dataset_new=mutate(dataset,gmatclass_num=dataset[,"gmat_tot"]*0)#adds a column to the original table with zeroes
  x=1
  while(x<=274)
  {
    dataset_new[x,"gmatclass_num"]=CC[x]#inputs the class number into column(gmatclass_num-which had zeroes before)
    x=x+1
  }
  return(dataset_new)
}