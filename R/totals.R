# find the aggregates for 1_ Total sales 2_ total profit, 3) total quantity and 4) total Discount

find_totals<-function(grp.var=dplyr::quo(Sales),df=df.filter){
# options are:"t.sales", "t.profit",t.quantity, t.discount
  print(dplyr::quo_name(grp.var))
  
  t<-df %>% dplyr::select(!!grp.var)%>% dplyr::summarise(total=sum(.,na.rm = TRUE)) %>% dplyr::pull()
  t
  #huxtable::fmt_pretty(t,big.mark =",",scientific = FALSE  )
}


# aggregate by age.bin and balance.bin
simple_paste<-function(...){
  
  purrr::map_chr(dplyr::enquos(...),rlang::as_label ) %>%
    paste0()
}


# Find the top 20 of customers or sold products
top_n_var<-function(grp.var=dplyr::quo(`Customer Name`),var.total=dplyr::quo(`Sales`), df=df.filter()){
  
  
   name<-paste0(simple_paste(!!var.total))
  df %>% dplyr::select(!!grp.var,Country,State,Region,City,`Product Name`,!!var.total) %>%
    dplyr::group_by(!!grp.var,State,Region,City) %>%
    dplyr::summarise(Total = sum(!!var.total)) %>% 
    dplyr::ungroup() %>%
    dplyr::slice_max(Total,n=20) %>%
    dplyr::mutate_at(dplyr::vars(Total),function(x) formatC(x,format="d", big.mark=',' )) %>%
    dplyr::rename( {{var.total}}:= Total)
 
}


