library(shiny)
library(shinymaterial)
library(echarts4r)
library(quantmod)
library(tidyverse)
library(lubridate)
library(dplyr)
library(XBRL)
library(finreportr)
library(edgarWebR)
#library(tidyverse)
library(httr)
library(rvest)
library(shinyjs)
library(DT)
library(shinyBS)

options(stringsAsFactors = FALSE)
bs1 <- function(xbrl.vars) {
  test1 <- xbrl.vars$role[xbrl.vars$role$type == 'Statement',] %>% filter(grepl('BALANCE', description)|grepl('Balance', description)|grepl('balance', description)) %>%
    filter(!grepl('Parenth', description))
  
  
  
  
  # let's get the balace sheet
  role_id <- (test1$roleId)
  
  pres <- 
    xbrl.vars$presentation %>%
    filter(roleId %in% role_id) %>%
    mutate(order = as.numeric(order))
  
  
  pres <- data.frame(lapply(pres, as.character), stringsAsFactors=FALSE)
  
  pres_df <- 
    (pres %>%
       anti_join(pres, by = c("fromElementId" = "toElementId")) %>%
       select(elementId = fromElementId)) %>% distinct()
  
  #pres_df <- data.frame(elementId = pres_df[1,])
  
  while({
    df1 <- pres_df %>%
      na.omit() %>%
      left_join( pres, by = c("elementId" = "fromElementId")) %>%
      arrange(elementId, order) %>%
      select(elementId, child = toElementId);
    nrow(df1) > 0
  }) 
  {
    # add each new level to data frame
    pres_df <- pres_df %>% left_join(df1, by = "elementId")
    names(pres_df) <-  c(sprintf("level%d", 1:(ncol(pres_df)-1)), "elementId")
  }
  # add last level as special column (the hierarchy may not be uniformly deep)
  pres_df["elementId"] <- 
    apply( t(pres_df), 2, function(x){tail( x[!is.na(x)], 1)})
  pres_df["elOrder"] <- 1:nrow(pres_df) 
  
  pres_df <- distinct(pres_df)
  
  
  relations <- 
    xbrl.vars$calculation %>% 
    filter(roleId == role_id) %>% 
    select(fromElementId, toElementId, order)
  
  elements <-
    data.frame( 
      elementId = with(relations, unique(c(fromElementId, toElementId))),
      stringsAsFactors = FALSE
    )  %>%
    left_join(xbrl.vars$element, by = c("elementId")) %>%
    left_join(relations, by = c("elementId" = "toElementId")) %>%
    left_join(xbrl.vars$label, by = c("elementId")) %>%
    filter(labelRole == "http://www.xbrl.org/2003/role/label") %>% 
    transmute(elementId, parentId = fromElementId, order, balance, labelString)
  
  # get top element(s) in hierarchy  
  # level <- 1
  # df1 <- elements %>%
  #   filter(is.na(parentId)) %>%
  #   mutate(id = "") %>% 
  #   arrange(desc(balance))
  # 
  # # search the tree
  # while({
  #   level_str <- 
  #     unname(unlist(lapply(split(df1$id, df1$id), function(x) {
  #       sprintf("%s%02d", x, 1:length(x))
  #     })))
  #   
  #   elements[elements$elementId %in% df1$elementId, "level"] <- level
  #   to_update <- elements[elements$elementId %in% df1$elementId, "elementId"]
  #   elements[ 
  #     #order(match(elements$elementId, to_update))[1:length(level_str)], 
  #     order(match(elements$elementId, df1$elementId))[1:length(level_str)], 
  #     "id"] <- level_str
  #   
  #   df1 <- elements %>%
  #     filter(parentId %in% df1$elementId) %>%
  #     arrange(order) %>%
  #     select(elementId, parentId) %>%
  #     left_join(elements, by=c("parentId"="elementId")) %>%
  #     arrange(id)
  #   nrow(df1) > 0})
  # {
  #   level <- level + 1
  # }
  # 
  # # order by hierarchy ID and mark terminal nodes 
  # elements <- 
  #   elements %>%  
  #   dplyr::arrange_(~id) %>% 
  #   dplyr::mutate( 
  #     terminal = !elementId %in% parentId,
  #     Element = paste(
  #       substring(paste(rep("&nbsp;",10), collapse = ""), 1, (level-1)*2*6),
  #       gsub("us-gaap_", "",elementId)
  #     )
  #   )
  # 
  pres_df <- pres_df %>% merge(elements[,c('elementId', 'parentId')], all.x=TRUE) %>% arrange(elOrder)
  na_count <- sum(is.na(pres_df$parentId))
  
  if(na_count <5){
    list1 <- pres_df %>% filter(grepl('AssetsCurrent', parentId))
    level1<- pres_df %>% filter(grepl('AssetsCurrent', elementId)) %>% filter(!elOrder %in% list1$elOrder)
    test1 <- rbind(list1, level1)
    list2 <- pres_df %>% filter(grepl('Inventory', parentId)) %>% filter(!elOrder %in% test1$elOrder)
    level2<- pres_df %>% filter(grepl('Inventory', elementId)) %>%filter(!elOrder %in% list2$elOrder) %>% filter(!elOrder %in% test1$elOrder)
    test1 <- rbind(test1, list2, level2)
    list2 <- pres_df %>% filter(grepl('Property', parentId)) %>% filter(!elOrder %in% test1$elOrder)
    level2<- pres_df %>% filter(grepl('Property', elementId)) %>%filter(!elOrder %in% list2$elOrder) %>% filter(!elOrder %in% test1$elOrder)
    test1 <- rbind(test1, list2, level2)
    list3 <- pres_df %>% filter(!grepl('AssetsCurrent', parentId)) %>% filter(grepl('Assets', parentId)) %>% filter(!elOrder %in% test1$elOrder)
    level3 <- pres_df %>%  filter(grepl('Assets', elementId)) %>% filter(!elOrder %in% test1$elOrder) %>% filter(!elOrder %in% list3$elOrder)
    test1 <- rbind(test1, list3, level3)
    
    list2 <- pres_df %>% filter(grepl('Inventory', parentId)) %>% filter(!elOrder %in% test1$elOrder)
    level2<- pres_df %>% filter(grepl('Inventory', elementId)) %>%filter(!elOrder %in% list2$elOrder) %>% filter(!elOrder %in% test1$elOrder)
    top1 <- pres_df %>% filter(elementId == 'us-gaap_Assets')
    test1 <- rbind(test1, list2, level2, top1)
    
    #list1 <- test1[1,]
    
    list1 <- pres_df %>% filter(grepl('LiabilitiesCurrent', parentId))
    level1<- pres_df %>% filter(grepl('LiabilitiesCurrent', elementId)) %>% filter(!elOrder %in% list1$elOrder) %>% filter(!elOrder %in% test1$elOrder)
    test1 <- rbind(test1, list1, level1)
    
    list1 <- pres_df %>% filter(grepl('DebtNoncurrent', parentId))
    level1<- pres_df %>% filter(grepl('DebtNoncurrent', elementId)) %>% filter(!elOrder %in% list1$elOrder) %>% filter(!elOrder %in% test1$elOrder)
    test1 <- rbind(test1, list1, level1)
    
    list2 <- pres_df %>% filter(grepl('Liabilities', parentId)) %>% filter(!elOrder %in% test1$elOrder) %>% filter(!grepl('Equity', parentId))
    level2<- pres_df %>% filter(grepl('Liabilities', elementId)) %>% filter(!grepl('Equity', elementId)) %>%
      filter(!elOrder %in% list2$elOrder) %>% filter(!elOrder %in% test1$elOrder)
    test1 <- rbind(test1, list2, level2)
    
    
    
    list2 <- pres_df %>% filter(grepl('Equity', parentId)) %>% filter(!elOrder %in% test1$elOrder)
    level2<- pres_df %>% filter(grepl('Equity', elementId)) %>%
      filter(!elOrder %in% list2$elOrder) %>% filter(!elOrder %in% test1$elOrder)
    top1 <- pres_df %>% filter(grepl('us-gaap_LiabilitiesAnd', elementId))
    test1 <- rbind(test1, list2, level2, top1)
    
    test1 <- test1 %>% mutate(elOrder = cumsum(elOrder/elOrder))
    
    pres_df <- test1
  }
  
  
  pres_df_num <-
    pres_df %>%
    left_join(xbrl.vars$fact, by = "elementId") %>%
    left_join(xbrl.vars$context, by = "contextId") %>%
    filter(is.na(dimension1)|grepl('OtherCurrentL', value1)|grepl('Predece', value1)|grepl('Success', value1)) %>% distinct() %>% 
    mutate(elementId = replace(elementId, grepl('OtherCurrentL', value1), value1[grepl('OtherCurrentL', value1)])) %>% distinct() %>%
    filter(!is.na(endDate)) %>%
    select(elOrder, contains("level"), elementId, fact, decimals, endDate)%>% group_by(endDate) %>% mutate(count=n()) %>% ungroup() %>%
    distinct() %>%
    filter(count >= max(count)*0.5) %>% distinct() %>%
    select(elOrder, contains("level"), elementId, fact, decimals, endDate) %>% filter(!duplicated(paste0(elementId, fact))) %>%
    mutate( fact = as.numeric(fact) * 10^as.numeric(decimals)) %>% group_by(elementId, endDate) %>% mutate(fact = sum(fact, na.rm=TRUE)) %>% ungroup() %>% distinct() %>%
    spread(endDate, fact ) %>%
    arrange(elOrder)# %>% group_by()
  
  
  pres_df_num <- pres_df_num %>% filter(!duplicated(elementId))
  
  ncol1 <- ncol(pres_df_num)
  ncol2 <- ncol1-1
  yr2 <- as.character(lubridate::year(as.POSIXct(names(pres_df_num)[ncol2], format = '%Y-%m-%d')))
  yr1 <- as.character(lubridate::year(as.POSIXct(names(pres_df_num)[ncol1], format = '%Y-%m-%d')))
  
  
  # library(pander)
  # pres_df_num %>% 
  #   select(elementId, contains(yr1), contains(yr2)) %>%
  #   pandoc.table(
  #     style = "rmarkdown",
  #     split.table = 200,
  #     justify = c("left", "right", "right")
  #   )
  
  pres_df_num$elementId <- gsub(':', '_', pres_df_num$elementId, fixed=TRUE)
  pres_df_num <- pres_df_num %>% mutate(elOrder = seq(1, nrow(pres_df_num), 1))
  
  #pres_df_num$id <- paste0(pres_df_num$level4, pres_df_num[,(ncol(pres_df_num)-1)], pres_df_num[,ncol(pres_df_num)])
  
  pres_df_num <- pres_df_num %>% rowwise() %>% mutate(id = paste0(level2, .[elOrder,(ncol(pres_df_num)-1)], .[elOrder,ncol(pres_df_num)])) %>% ungroup()
  pres_df_num <- pres_df_num %>% filter(!duplicated(id))
  pres_df_num <- subset(pres_df_num, select = -c(id))
  pres_df_num$elOrder[grepl('Assets', pres_df_num$level2) & grepl('CurrentLiab', pres_df_num$elementId)] <- NA
  pres_df_num <- pres_df_num %>% filter(!is.na(elOrder))
  
  x_labels <-
    xbrl.vars$presentation %>%
    filter(roleId == role_id) %>%
    select(elementId = toElementId, labelRole = preferredLabel) %>%
    semi_join(pres_df_num, by = "elementId") %>%
    left_join(xbrl.vars$label, by = c("elementId", "labelRole")) %>%
    filter(lang == "en-US") %>%
    select(elementId, labelString)
  
  x_labels1 <- pres_df_num %>% filter(!elementId %in% x_labels$elementId)
  x_labels1 <- xbrl.vars$presentation %>% filter(toElementId %in% x_labels1$elementId)
  x_labels1 <- x_labels1 %>% select(elementId = toElementId, labelRole = preferredLabel) %>% filter(!is.na(labelRole))%>%
    semi_join(pres_df_num, by = "elementId") %>%
    left_join(xbrl.vars$label, by = c("elementId", "labelRole")) %>%
    filter(lang == "en-US") %>%
    select(elementId, labelString)
  
  x_labels <- rbind(x_labels, x_labels1)
  
  x_labelsCheck <- xbrl.vars$label %>% filter(elementId %in% pres_df_num $elementId) %>%
    filter(!elementId %in% x_labels$elementId) %>% filter(!duplicated(elementId)) %>%
    select(elementId, labelString)
  
  x_labels <- rbind(x_labels, x_labelsCheck)
  
  
  
  x_labels <- x_labels %>% filter(!duplicated(paste0(elementId, labelString))) %>% merge(pres_df_num[,c('elementId', 'elOrder')]) %>% arrange(elOrder)
  # calculated elements in this statement component
  x_calc <- xbrl.vars$calculation %>%
    filter(roleId == role_id) %>%
    select(elementId = fromElementId, calcRoleId = arcrole) %>%
    unique()
  
  # join concepts and numbers with labels
  # balance_sheet_pretty <- pres_df_num %>%
  #   left_join(x_labels, by = "elementId") %>%
  #   left_join(x_calc, by = "elementId") %>%
  #   select(labelString, contains(yr1), contains(yr2), calcRoleId)
  balance_sheet_pretty <- pres_df_num %>%
    left_join(x_labels, by = "elementId") %>%
    left_join(x_calc, by = "elementId")  %>% filter(!is.na(labelString))
  check1 <- data.frame(names(balance_sheet_pretty))
  names(check1) <- 'columns1'
  check1 <- check1 %>% filter(!grepl('level', columns1))
  check1 <- check1$columns1
  balance_sheet_pretty <- subset(balance_sheet_pretty, select =(dput(check1)))
  balance_sheet_pretty <- subset(balance_sheet_pretty, select =-c(elOrder.y, decimals))                           
  balance_sheet_pretty$elOrder.x <- seq(1, nrow(balance_sheet_pretty), 1)
  
  balance_sheet_pretty <- balance_sheet_pretty %>% group_by(labelString, elementId) %>%
    mutate(elOrder.x = min(elOrder.x)) %>% ungroup() %>%
    group_by(labelString, calcRoleId, elOrder.x, elementId) %>%
    summarise_all(sum, na.rm=TRUE) %>% ungroup() %>% arrange(elOrder.x)#%>%
  #select(labelString, contains('months'), calcRoleId)
  balance_sheet_pretty <- subset(balance_sheet_pretty, select = -c(elOrder.x, elementId))
  
  names(balance_sheet_pretty)[1] <- 
    "CONDENSED CONSOLIDATED BALANCE SHEETS"
  
  #names(balance_sheet_pretty)[2:3] <-
  #  format(as.Date(names(balance_sheet_pretty)[2:3]), "%Y")
  # rendering balance sheet
  balance_sheet_pretty$`CONDENSED CONSOLIDATED BALANCE SHEETS`[!is.na(balance_sheet_pretty$calcRoleId)] <- 
    toupper(balance_sheet_pretty$`CONDENSED CONSOLIDATED BALANCE SHEETS`[!is.na(balance_sheet_pretty$calcRoleId)])
  balance_sheet_pretty <- subset(balance_sheet_pretty, select = -c(calcRoleId))
  
  return(balance_sheet_pretty)
}

is1 <- function(xbrl.vars, comp.ticker) {
  test1 <- xbrl.vars$role[xbrl.vars$role$type == 'Statement',] %>% filter(grepl('OPERATIONS', description)|grepl('INCOME', description)|grepl('EARNINGS', description)|
                                                                            grepl('Operations', description)|grepl('Income', description)|grepl('Earnings', description)|
                                                                            grepl('operations', description)|grepl('income', description)|grepl('earnings', description)) %>%
    filter(!grepl('Parenth', description)) %>% filter(!grepl('Comprehensive', description))%>% filter(!grepl('COMPREHENSIVE', description))
  
  if(nrow(test1) == 0){
    test1 <- xbrl.vars$role[xbrl.vars$role$type == 'Statement',] %>% filter(grepl('OPERATIONS', description)|grepl('INCOME', description)|grepl('EARNINGS', description)|
                                                                              grepl('Operations', description)|grepl('Income', description)|grepl('Earnings', description)|
                                                                              grepl('operations', description)|grepl('income', description)|grepl('earnings', description)) %>%
      filter(!grepl('Parenth', description))
  }
  
  
  # let's get the balace sheet
  role_id <- test1$roleId[1]
  
  pres <- 
    xbrl.vars$presentation %>%
    filter(roleId %in% role_id) %>%
    mutate(order = as.numeric(order)) %>% mutate(elementId = fromElementId)
  
  
  
  pres_df <- 
    pres %>%
    anti_join(pres, by = c("fromElementId" = "toElementId")) %>%
    select(elementId = fromElementId)# %>% filter(!duplicated(elementId))
  
  
  while({
    df1 <- pres_df %>%
      na.omit() %>%
      left_join( pres, by = c("elementId" = "fromElementId")) %>%
      arrange(elementId, order) %>% distinct() %>%
      select(elementId, child = toElementId);
    nrow(df1) > 0
  })
    
  {
    # add each new level to data frame
    pres_df <- pres_df %>% left_join(df1, by = "elementId")
    names(pres_df) <-  c(sprintf("level%d", 1:(ncol(pres_df)-1)), "elementId")
  }
  # add last level as special column (the hierarchy may not be uniformly deep)
  pres_df["elementId"] <- 
    apply( t(pres_df), 2, function(x){tail( x[!is.na(x)], 1)})
  pres_df["elOrder"] <- 1:nrow(pres_df) 
  
  pres_df <- distinct(pres_df)
  test1 <- xbrl.vars$fact[grepl('us-gaap_RevenueFromContract', xbrl.vars$fact$elementId),] %>% filter(!is.na(fact))
  test1$fact <- as.numeric(test1$fact)
  test1 <- test1 %>% filter(!is.na(fact))
  
  pres_df$elementId[pres_df$elementId == 'us-gaap_RevenueFromContractWithCustomerProductAndServiceExtensibleList' & comp.ticker == 'WLL'] <-
    'us-gaap_RevenueFromContractWithCustomerExcludingAssessedTax'
  
  
  
  pres_df_num <-
    pres_df %>%
    left_join(xbrl.vars$fact, by = "elementId") %>%
    left_join(xbrl.vars$context, by = "contextId") %>%
    filter(is.na(dimension1)|grepl('Member', value1) & grepl('Product', dimension1)|grepl('Predece', value1)|grepl('Success', value1)) %>% distinct() %>% filter(is.na(dimension2)) %>%
    mutate(elementId = replace(elementId, grepl('Member', value1), value1[grepl('Member', value1)])) %>%
    filter(!is.na(endDate)) %>% mutate(months = lubridate::interval(startDate, endDate) %/% months(1)+1) %>%
    mutate(endDate = paste0(months, ' months ended ', endDate)) %>%
    select(elOrder, contains("level"), elementId, fact, decimals, endDate) %>% distinct()  %>% filter(!duplicated(paste0(elementId, fact))) %>%
    mutate( fact = as.numeric(fact) * 10^as.numeric(decimals)) %>% group_by(endDate) %>% mutate(count=n()) %>% ungroup() %>%
    filter(count >= max(count)*0.5) %>% select(elOrder, contains("level"), elementId, fact, decimals, endDate)%>% filter(!is.infinite(fact)) %>%filter(!duplicated(paste0(elementId, fact))) %>%
    spread(endDate, fact ) %>%
    arrange(elOrder) %>% group_by(elementId) %>% filter(as.numeric(decimals) == max(as.numeric(decimals))) %>% ungroup()
  
  
  
  
  pres_df_num <- pres_df_num %>% filter(!duplicated(elementId))
  ct1 <- which(names(pres_df_num) == 'decimals')
  
  pres_df_num$elementId <- gsub(':', '_', pres_df_num$elementId, fixed =TRUE)
  pres_df_num <- pres_df_num %>% mutate(elOrder = seq(1, nrow(pres_df_num), 1))
  
  #pres_df_num$id <- paste0(pres_df_num$level4, pres_df_num[,(ncol(pres_df_num)-1)], pres_df_num[,ncol(pres_df_num)])
  
  pres_df_num <- pres_df_num %>% rowwise() %>% mutate(id = paste0(level2, .[elOrder,(ncol(pres_df_num)-1)], .[elOrder,ncol(pres_df_num)])) %>% ungroup()
  pres_df_num <- pres_df_num %>% filter(!duplicated(id))
  pres_df_num <- subset(pres_df_num, select = -c(id))
  
  x_labels <-
    xbrl.vars$presentation %>%
    filter(roleId == role_id) %>%
    select(elementId = toElementId, labelRole = preferredLabel) %>%
    semi_join(pres_df_num, by = "elementId") %>%
    left_join(xbrl.vars$label, by = c("elementId", "labelRole")) %>%
    filter(lang == "en-US") %>%
    select(elementId, labelString)
  x_labels1 <- pres_df_num %>% filter(!elementId %in% x_labels$elementId)
  x_labels1 <- xbrl.vars$presentation %>% filter(toElementId %in% x_labels1$elementId)
  x_labels1 <- x_labels1 %>% select(elementId = toElementId, labelRole = preferredLabel) %>% filter(!is.na(labelRole))%>%
    semi_join(pres_df_num, by = "elementId") %>%
    left_join(xbrl.vars$label, by = c("elementId", "labelRole")) %>%
    filter(lang == "en-US") %>%
    select(elementId, labelString)
  
  x_labels <- rbind(x_labels, x_labels1)
  
  x_labelsCheck <- xbrl.vars$label %>% filter(elementId %in% pres_df_num $elementId) %>%
    filter(!elementId %in% x_labels$elementId) %>% filter(!duplicated(elementId)) %>%
    select(elementId, labelString)
  
  x_labels <- rbind(x_labels, x_labelsCheck)
  
  x_labels <- x_labels %>% filter(!duplicated(elementId)) %>% merge(pres_df_num[,c('elementId', 'elOrder')]) %>% arrange(elOrder)
  x_labels$labelString <- gsub('[Member]', '', x_labels$labelString, fixed=TRUE)
  
  # calculated elements in this statement component
  x_calc <- xbrl.vars$calculation %>%
    filter(roleId == role_id) %>%
    select(elementId = fromElementId, calcRoleId = arcrole) %>%
    unique()
  
  test1 <- ncol(pres_df_num) - ct1
  
  #pres_df_num <- pres_df_num %>% group_by(contains('level'), elementId, decimals) %>% mutate(elOrder = min(elOrder)) %>%
  #  ungroup() %>% group_by(elOrder, contains('level'), elementId, decimals) %>% summarise_all(sum,na.rm=TRUE) %>% ungroup()
  
  income_statement_pretty <- pres_df_num %>%
    left_join(x_labels, by = "elementId") %>%
    left_join(x_calc, by = "elementId") %>%
    select(labelString, contains('months'), calcRoleId, elOrder.x)
  income_statement_pretty <- income_statement_pretty %>% group_by(labelString) %>%
    mutate(elOrder.x = min(elOrder.x)) %>% ungroup() %>%
    group_by(labelString, calcRoleId, elOrder.x) %>%
    summarise_all(sum, na.rm=TRUE) %>% ungroup() %>% arrange(elOrder.x)%>%
    select(labelString, contains('months'), calcRoleId)
  names(income_statement_pretty)[1] <- 
    "INCOME STATEMENT"
  
  
  
  # join concepts and numbers with labels
  
  
  
  # rendering balance sheet
  income_statement_pretty$`INCOME STATEMENT`[!is.na(income_statement_pretty$calcRoleId)] <- 
    toupper(income_statement_pretty$`INCOME STATEMENT`[!is.na(income_statement_pretty$calcRoleId)])
  income_statement_pretty$`INCOME STATEMENT`[grepl('Total',income_statement_pretty$`INCOME STATEMENT`)] <- 
    toupper(income_statement_pretty$`INCOME STATEMENT`[grepl('Total',income_statement_pretty$`INCOME STATEMENT`)])
  income_statement_pretty <- subset(income_statement_pretty, select = -c(calcRoleId))
  return(income_statement_pretty)
}

cf1 <- function(xbrl.vars) {
  test1 <- xbrl.vars$role[xbrl.vars$role$type == 'Statement',] %>% filter(grepl('CASH', description)|grepl('Cash', description)|grepl('cash', description)) %>%
    filter(!grepl('Parenth', description))
  
  
  
  # let's get the balace sheet
  role_id <- test1$roleId[1]
  
  pres <- 
    xbrl.vars$presentation %>%
    filter(roleId %in% role_id) %>%
    mutate(order = as.numeric(order))
  
  pres_df <- 
    pres %>%
    anti_join(pres, by = c("fromElementId" = "toElementId")) %>%
    select(elementId = fromElementId) %>% distinct()
  
  
  while({
    df1 <- pres_df %>%
      na.omit() %>%
      left_join( pres, by = c("elementId" = "fromElementId")) %>%
      arrange(elementId, order) %>% distinct() %>%
      select(elementId, child = toElementId);
    nrow(df1) > 0
  })
    
  {
    # add each new level to data frame
    pres_df <- pres_df %>% left_join(df1, by = "elementId")
    names(pres_df) <-  c(sprintf("level%d", 1:(ncol(pres_df)-1)), "elementId")
  }
  # add last level as special column (the hierarchy may not be uniformly deep)
  pres_df["elementId"] <- 
    apply( t(pres_df), 2, function(x){tail( x[!is.na(x)], 1)})
  pres_df["elOrder"] <- 1:nrow(pres_df) 
  
  pres_df <- distinct(pres_df)
  #test1 <- xbrl.vars$fact[grepl('us-gaap_RevenueFromContract', xbrl.vars$fact$elementId),] %>% filter(!is.na(fact))
  #test1$fact <- as.numeric(test1$fact)
  #test1 <- test1 %>% filter(!is.na(fact))
  
  #pres_df$elementId[pres_df$elementId == 'us-gaap_RevenueFromContractWithCustomerProductAndServiceExtensibleList' & comp.ticker == 'WLL'] <-
  #  'us-gaap_RevenueFromContractWithCustomerExcludingAssessedTax'
  
  pres_df_num <-
    pres_df %>%
    left_join(xbrl.vars$fact, by = "elementId") %>%
    left_join(xbrl.vars$context, by = "contextId") %>%
    filter(is.na(dimension1)) %>%
    filter(!is.na(endDate)) %>% mutate(months = lubridate::interval(startDate, endDate) %/% months(1)+1) %>%
    mutate(endDate = paste0(months, ' months ended ', endDate)) %>%
    select(elOrder, contains("level"), elementId, fact, decimals, endDate) %>% distinct()  %>% filter(!duplicated(paste0(elementId, fact))) %>%
    mutate( fact = as.numeric(fact) * 10^as.numeric(decimals)) %>% group_by(endDate) %>% mutate(count=n()) %>% ungroup() %>%
    filter(count >= max(count)*0.5) %>% select(elOrder, contains("level"), elementId, fact, decimals, endDate)%>% 
    spread(endDate, fact ) %>%
    arrange(elOrder)
  
  
  
  
  pres_df_num <- pres_df_num %>% filter(!duplicated(elementId))
  ct1 <- which(names(pres_df_num) == 'decimals')
  
  
  x_labels <-
    xbrl.vars$presentation %>%
    filter(roleId == role_id) %>%
    select(elementId = toElementId, labelRole = preferredLabel) %>%
    semi_join(pres_df_num, by = "elementId") %>%
    left_join(xbrl.vars$label, by = c("elementId", "labelRole")) %>%
    filter(lang == "en-US") %>%
    select(elementId, labelString)
  x_labels1 <- pres_df_num %>% filter(!elementId %in% x_labels$elementId)
  x_labels1 <- xbrl.vars$presentation %>% filter(toElementId %in% x_labels1$elementId)
  x_labels1 <- x_labels1 %>% select(elementId = toElementId, labelRole = preferredLabel) %>% filter(!is.na(labelRole))%>%
    semi_join(pres_df_num, by = "elementId") %>%
    left_join(xbrl.vars$label, by = c("elementId", "labelRole")) %>%
    filter(lang == "en-US") %>%
    select(elementId, labelString)
  
  x_labels <- rbind(x_labels, x_labels1)
  
  x_labelsCheck <- xbrl.vars$label %>% filter(elementId %in% pres_df_num $elementId) %>%
    filter(!elementId %in% x_labels$elementId) %>% filter(!duplicated(elementId)) %>%
    select(elementId, labelString)
  
  x_labels <- rbind(x_labels, x_labelsCheck)
  
  x_labels <- x_labels %>% filter(!duplicated(elementId)) %>% merge(pres_df_num[,c('elementId', 'elOrder')]) %>% arrange(elOrder)
  # calculated elements in this statement component
  x_calc <- xbrl.vars$calculation %>%
    filter(roleId == role_id) %>%
    select(elementId = fromElementId, calcRoleId = arcrole) %>%
    unique()
  
  test1 <- ncol(pres_df_num) - ct1
  
  cf_statement_pretty <- pres_df_num %>%
    left_join(x_labels, by = "elementId") %>%
    left_join(x_calc, by = "elementId") %>%
    select(labelString, contains('month'), calcRoleId)
  names(cf_statement_pretty)[1] <- 
    "CASH FLOW STATEMENT"
  
  
  # join concepts and numbers with labels
  
  
  
  # rendering balance sheet
  cf_statement_pretty$`CASH FLOW STATEMENT`[!is.na(cf_statement_pretty$calcRoleId)] <- 
    toupper(cf_statement_pretty$`CASH FLOW STATEMENT`[!is.na(cf_statement_pretty$calcRoleId)])
  cf_statement_pretty <- subset(cf_statement_pretty, select = -c(calcRoleId))
  return(cf_statement_pretty)
}



otherGraphs <- function(xbrl.vars, role) tryCatch ({
  
  role_id <- role
  
  pres <- 
    xbrl.vars$presentation %>%
    filter(roleId %in% role_id) %>%
    mutate(order = as.numeric(order))
  
  pres <- data.frame(lapply(pres, as.character), stringsAsFactors=FALSE)
  
  pres_df <- 
    (pres %>%
       anti_join(pres, by = c("fromElementId" = "toElementId")) %>%
       select(elementId = fromElementId)) %>% distinct()
  
  while({
    df1 <- pres_df %>%
      na.omit() %>%
      left_join( pres, by = c("elementId" = "fromElementId")) %>%
      arrange(elementId, order) %>%
      select(elementId, child = toElementId);
    nrow(df1) > 0
  }) 
  {
    # add each new level to data frame
    pres_df <- pres_df %>% left_join(df1, by = "elementId")
    names(pres_df) <-  c(sprintf("level%d", 1:(ncol(pres_df)-1)), "elementId")
  }
  # add last level as special column (the hierarchy may not be uniformly deep)
  pres_df["elementId"] <- 
    apply( t(pres_df), 2, function(x){tail( x[!is.na(x)], 1)})
  pres_df["elOrder"] <- 1:nrow(pres_df) 
  
  pres_df <- distinct(pres_df)
  
  pres_df_num <-
    pres_df %>%
    left_join(xbrl.vars$fact, by = "elementId") %>%
    left_join(xbrl.vars$context, by = "contextId") %>%
    filter(!is.na(fact)) %>% distinct() %>% 
    #mutate(elementId = replace(elementId, grepl('OtherCurrentL', value1), value1[grepl('OtherCurrentL', value1)])) %>% distinct() %>%
    filter(!is.na(endDate)) %>%
    select(elOrder, contains("level"), elementId, fact, decimals, endDate)%>% group_by(endDate) %>% mutate(count=n()) %>% ungroup() %>%
    distinct() %>%
    #filter(count >= max(count)*0.5) %>% distinct() %>%
    select(elOrder, contains("level"), elementId, fact, decimals, endDate) %>% filter(!duplicated(paste0(elementId, fact))) %>%
    mutate( fact = as.numeric(fact) * 10^as.numeric(decimals)) %>% group_by(elementId, endDate) %>% mutate(fact = sum(fact, na.rm=TRUE)) %>% ungroup() %>% distinct() %>%
    spread(endDate, fact ) %>%
    arrange(elOrder)# %>% group_by()
  
  x_labels <-
    xbrl.vars$presentation %>%
    filter(roleId == role_id) %>%
    select(elementId = toElementId, labelRole = preferredLabel) %>%
    semi_join(pres_df_num, by = "elementId") %>%
    left_join(xbrl.vars$label, by = c("elementId", "labelRole")) %>%
    filter(lang == "en-US") %>%
    select(elementId, labelString)
  
  x_calc <- xbrl.vars$calculation %>%
    filter(roleId == role_id) %>%
    select(elementId = fromElementId, calcRoleId = arcrole) %>%
    unique()
  
  balance_sheet_pretty <- pres_df_num %>%
    left_join(x_labels, by = "elementId") %>%
    left_join(x_calc, by = "elementId")  %>% filter(!is.na(labelString))
  
  balance_sheet_pretty$units <- paste0(1/10^as.numeric(balance_sheet_pretty$decimals), "'s")
  
  check1 <- data.frame(names(balance_sheet_pretty))
  names(check1) <- 'columns1'
  check1 <- check1 %>% filter(!grepl('level', columns1))
  check1 <- check1$columns1
  balance_sheet_pretty <- subset(balance_sheet_pretty, select =(dput(check1)))
  balance_sheet_pretty <- subset(balance_sheet_pretty, select =-c(elOrder, decimals))                           
  #balance_sheet_pretty$elOrder.x <- seq(1, nrow(balance_sheet_pretty), 1)
  
  balance_sheet_pretty <- balance_sheet_pretty %>%# group_by(labelString, elementId) %>%
    # mutate(elOrder = min(elOrder)) %>% ungroup() %>%
    group_by(labelString, calcRoleId,  elementId, units) %>%
    summarise_all(sum, na.rm=TRUE) %>% ungroup()# %>% arrange(elOrder)#%>%
  #select(labelString, contains('months'), calcRoleId)
  balance_sheet_pretty <- subset(balance_sheet_pretty, select = -c(elementId))
  
  names(balance_sheet_pretty)[1] <- 
    pres$fromElementId[1]
  
  #names(balance_sheet_pretty)[2:3] <-
  #  format(as.Date(names(balance_sheet_pretty)[2:3]), "%Y")
  # rendering balance sheet
  
  balance_sheet_pretty <- subset(balance_sheet_pretty, select = -c(calcRoleId))
  
  x_labels1 <-
    xbrl.vars$presentation %>% filter(fromElementId %in% names(balance_sheet_pretty[,1])) %>% filter(grepl('Text', toElementId)) %>% .[1,]%>%
    select(elementId = toElementId, labelRole = preferredLabel) %>%
    #semi_join(pres_df_num, by = "elementId") %>%
    left_join(xbrl.vars$label, by = c("elementId", "labelRole")) %>%
    filter(lang == "en-US") %>%
    select(elementId, labelString)
  
  names(balance_sheet_pretty)[1] <- x_labels1$labelString
  return(balance_sheet_pretty)
},
error = function(e) {
  e
  NULL
})


css <- HTML(
  "#bs > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
  transform:rotateX(180deg);
  }
  #bs > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
  transform:rotateX(180deg);
  }
    #custom > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
  transform:rotateX(180deg);
  }
  #custom > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
  transform:rotateX(180deg);
   }
    #is > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
  transform:rotateX(180deg);
  }
  #is > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
  transform:rotateX(180deg);
   }
   #cf > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
  transform:rotateX(180deg);
  }
  #cf > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
  transform:rotateX(180deg);
   }"
)

url1 <- function(x) {
  paste0('https://www.sec.gov', ((xml2::read_html(x) %>%
                                    rvest::html_nodes('table') %>% .[1] %>%
                                    rvest::html_nodes('a') %>% 
                                    rvest::html_attr('href')))[1])
}

opList1 <- c('APA',  'AR', 'AREX', 'BCEI',  'CDEV', 'CHAP', 'CHK', 'CLR',
             'CNX', 'COG', 'COP', 'CPE',  'CRK', 'CVX',  'CXO',  'DVN', 'ECA','EOG',
             'EQT', 'ESTE', 'FANG', 'GPOR', 'GDP',  'HES', 'HPR',  'LLEX', 'LPI', 'MGY', 'MR',
             'MRO', 'MTDR', 'MUR', 'NBL',  'OAS', 'OXY', 'PDCE', 'PE', 'PVAC', 'PXD', 'QEP', 
             'RRC', 'SBOW', 'SM', 'SWN',  'WLL', 'WPX', 'XEC', 'XOG', 'XOM')  

opLink <- data.frame(ticker = opList1, operator = c('Apache', 'Antero Resources',
                                                    'Approach Resources',  'Bonanza Creek Energy','Centennial Resource Development',
                                                    'Chaparral Energy', 'Chesapeake Energy', 'Continental Resources',
                                                    'CNX Resources', 'Cabot Oil & Gas', 'ConocoPhillips', 'Callon Petroleum',
                                                    'Comstock Resources', 'Chevron', 'Concho Resources',
                                                    'Devon Energy', 'Encana Corporation', 'EOG Resources', 'EQT Corporation', 'Earthstone Energy',
                                                    'Diamondback Energy', 'Gulfport Energy', 'Goodrich Petroleum',
                                                    'Hess Corporation', 'HighPoint Resources', 
                                                    'Lilis Energy', 'Laredo Petroleum',
                                                    'Magnolia Oil & Gas Operating', 'Montage Resources', 'Marathon Oil',
                                                    'Matador Resources', 'Murphy Oil', 'Noble Energy', 'Oasis Petroleum',
                                                    'Occidental Petroleum', 'PDC Energy', 'Parsley Energy', 'Penn Virginia Corporation',
                                                    'Pioneer Natural Resources', 'QEP Resources', 'Range Resources',
                                                    'SilverBow Resources', 'SM Energy', 'Southwestern Energy', 'Whiting Petroleum Corporation',
                                                    'WPX Energy', 'Cimarex Energy', 'Extraction Oil & Gas', 'ExxonMobil'))


shinyApp(
  ui = material_page(
    useShinyjs(),
    title = "Energy Financial Explorer",
    nav_bar_fixed = TRUE,
    nav_bar_color = 'teal',
    background_color = 'white',
    tags$br(),
    tags$head(tags$style(css)),
    material_row(
      material_column(
        width = 4,
        material_card(
          title = "Company",
          depth = 4,
          
          material_dropdown('operator', '',
                            choices = unique(opLink$operator)),
          
          bsButton('loadFilings', 'Load Filings', 
                   icon = icon('table'), size = 'default', style = 'info')
          
        ),
        material_card(
          title = 'Selected Period',
          depth =4,
          material_dropdown('Filing', 'Filing', choices = ''),
          bsButton('calc', 'Load Tables', icon = icon('table'),
                   size = 'default', style = 'default')
        ),
        material_card(
          title = "Filings",
          depth = 4,
          DT::dataTableOutput("filingList")
      )),
      material_column(
        width = 8,
        material_card(
          title = "Stock Price Performance",
          depth = 4,
          dateInput('start_date', 'Start Date', value = as.POSIXct('2019-01-01', format = '%Y-%m-%d'), max = Sys.Date()),
          echarts4rOutput('hcplot')
        ),
        material_row(
          width =12,
          material_column(
            width = 12,
            material_card(
              title = 'Balance Sheet',
              depth = 4,
              DT::dataTableOutput('bs')
            )
          ),
          material_column(
            width = 12,
            material_card(
              title = 'Income Statement',
              depth = 4,
              DT::dataTableOutput('is')
            )
          ),
          material_column(
            width = 12,
            material_card(
              title = 'Cash Flow',
              depth = 4,
              DT::dataTableOutput('cf')
            )
          ),
          material_column(
            width = 12,
            material_card(
              title = 'Other Tables',
              depth = 4,
              pickerInput('role', 'Avalable Tables', choices = '',
                          options = list(
                            `actions-box` = TRUE,
                            `live-search` = TRUE,
                            `virtualScroll` = 10,
                            size = 10
                          )),
              DT::dataTableOutput('custom')
            )
          )
        )
      )
    )
  ),
  
  server <- function(input, output, session) {
    
    values <- reactiveValues()
    
    observe({
      if(is.null(values$xbrl.vars)){
        NULL
      } else {
        test1 <- values$xbrl.vars$role[values$xbrl.vars$role$type == 'Disclosure',] %>% filter(grepl('Details', description))
        print(head(test1))
        updatePickerInput(session, inputId ='role', selected = test1$description[1], choices = unique(test1$description))
      }
    })
    
    comp.ticker <- reactive(
      (opLink %>% filter(operator%in% input$operator))$ticker
    )
    
    observeEvent(input$operator, {
      values$xbrl.vars <- NULL
      values$filingList1 <- NULL
      update_material_dropdown(session, input_id='Filing', choices = '', value = '')
    })
    
    observeEvent(input$Filing, {
      values$xbrl.vars <- NULL
      #values$filingList1 <- NULL
      #updateSelectizeInput(session, 'Filing', choices = '')
    })
    
    output$hcplot <- renderEcharts4r({
      #ticker <- input$operator
      ticker <- comp.ticker()
      #print(ticker)
      stock <- getSymbols(ticker, src='yahoo', auto.assign = FALSE, setSymbolLookup('stock'))
      df <- data.frame(Date = index(stock), coredata(stock))
      df <- df %>% filter(Date >= input$start_date)
      rm(stock)
      removeSymbols()
      names(df)[1:5] <- c('date', 'opening', 'high', 'low', 'closing')
      #print(names(df))
      df$date <- as.character(df$date)
      df %>%
        e_charts(date) %>%
        e_candle(opening, closing, low, high, name = input$operator) %>% 
        e_datazoom(type = "slider") %>% 
        e_title("Candlestick chart", "Quantmod data")%>%
        e_theme('auritus')%>%
        e_tooltip(trigger = "axis")
      
    })
    
    observeEvent(input$loadFilings, {
      #updateButton(session, 'loadFilings', label = 'Gathering...', style = 'danger')
      updateButton(session, 'loadFilings', label = 'Caclulating...')
      shinyjs::disable('loadFilings')
      #operatorSelect <- opLink %>% filter(operator %in% input$operator)
      operatorSelect <- comp.ticker()
      #print(operatorSelect)
      filingList <- data.frame(edgarWebR::company_details(operatorSelect, type = '10-K', count = 8)) %>% filter(!grepl('A', filings.type))
      filingList1 <- data.frame(edgarWebR::company_details(operatorSelect, type = '10-Q', count = 24)) %>% filter(!grepl('A', filings.type))
      
      filingList <- rbind(filingList, filingList1) %>% arrange(desc(filings.filing_date))
      
      compInfo <- finreportr::CompanyInfo(operatorSelect)
      filingList$Company <- compInfo$company
      rm(filingList1)
      
      filingList$url1 <- lapply(filingList$filings.href,  url1)
      filingList$url1 <- gsub('/ix?doc=', '', filingList$url1, fixed=TRUE)
      #print(head(filingList))
      values$check <- filingList
      
      filingList <- filingList[,c('Company', 'filings.filing_date', 'filings.type', 'url1')] %>% arrange(desc(filings.filing_date))
      filingList$quarter <- lubridate::quarter(filingList$filings.filing_date) - 1
      filingList$year <- lubridate::year(filingList$filings.filing_date)
      filingList$quarter[filingList$quarter == 0] <- 4
      filingList$year[filingList$quarter == 4] <- filingList$year[filingList$quarter == 4]-1
      filingList$period <- paste0('Q', filingList$quarter, filingList$year)
      update_material_dropdown(session, input_id='Filing', choices = filingList$period, value = filingList$period[1])
      names(filingList)[1:4] <- c('Company', 'filingDate', 'type', 'url1')
      filingList <- filingList[,c('Company', 'period', 'filingDate', 'type', 'url1')]
      #print(head(filingList))
      filingList <- data.frame(lapply(filingList, function(x){
        gsub("iXBRL", "", x)
      }))
      filingList <- data.frame(lapply(filingList, function(x){
        gsub("\\s+", "", x)
      }))
      
      filingList$type <- paste0('<a href="', filingList$url1, '" target="_blank">', filingList$type,'</a>')
      values$filingList <- filingList
      filingList <- filingList[,c('Company', 'period', 'filingDate', 'type')]
      names(filingList) <- c('Company', 'Filing Period', 'Filing Date', 'Report')
      filingList <- as.data.frame(filingList)
      values$filingList1 <- filingList
      #updateButton(session, 'loadFilings', label = 'SUCCESS', style = 'success')
      updateButton(session, 'loadFilings', label = 'Load Filings')
      shinyjs::enable('loadFilings')
    })
    
    output$filingList <- DT::renderDataTable({
      if(is.null(values$filingList1)){
        NULL
      } else {
        filingList <- values$filingList1
        filingList <- subset(filingList, select = -c(Company))
        DT::datatable(filingList, escape = FALSE, rownames = FALSE, options = list(paging = FALSE, searching = FALSE))
      }
    })
    
    observeEvent(input$calc,{
      updateButton(session, 'calc', 'Calculating....')
      shinyjs::disable('calc')
      #comp.ticker <- opLink %>% filter(operator %in% input$operator)
      comp.ticker <- comp.ticker()
      FilingsonEdgar <- edgarWebR::company_filings(x = comp.ticker, type = "10-")
      FilingsonEdgar <- FilingsonEdgar %>% mutate(QUARTER = quarter(filing_date)-1, YEAR = year(filing_date))
      FilingsonEdgar$QUARTER[FilingsonEdgar$QUARTER == 0] <- 4
      FilingsonEdgar$YEAR[FilingsonEdgar$QUARTER == 4] <- FilingsonEdgar$YEAR[FilingsonEdgar$QUARTER == 4] -1
      FilingsonEdgar$PERIOD <- paste0('Q', FilingsonEdgar$QUARTER, FilingsonEdgar$YEAR) 
      #FilingsonEdgar <- FilingsonEdgar%>% filter(!grepl('A', type))
      FilingsonEdgar <- FilingsonEdgar %>% mutate(PERIOD = replace(PERIOD, grepl('A', type), paste0(PERIOD[grepl('A',type)], 'A')))
      FilingsonEdgar <- FilingsonEdgar %>% filter(YEAR >= 2015) %>% filter(!duplicated(PERIOD)) %>% filter(PERIOD %in% input$Filing)
      DocumentsonEdgar <-  edgarWebR::filing_documents(x = FilingsonEdgar$href[1])
      link <- DocumentsonEdgar[DocumentsonEdgar[5] == 'XML'|DocumentsonEdgar[5] == 'EX-101.INS', 4]
      
      values$xbrl.vars <- XBRL::xbrlDoAll(link, verbose=TRUE)
      shinyjs::enable('calc')
      updateButton(session, 'calc', 'Load Tables')
    })
    
    
    output$custom <- DT::renderDataTable({
      if(is.null(values$xbrl.vars)){
        NULL
      } else {
        
        role <- values$xbrl.vars$role[values$xbrl.vars$role$type == 'Disclosure',] %>% filter(description %in% input$role)
        role <- role$roleId
        custom <- otherGraphs(values$xbrl.vars, role)
        
        
        DT::datatable(custom, rownames = FALSE,
                      extensions = c('Buttons', 'Scroller'), 
                      options = list(
                        dom = 'Bfrtip',
                        scrollX = TRUE,
                        scrollY = FALSE,
                        deferRender = TRUE,
                        paging = FALSE,
                        searching = FALSE,
                        buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print')
                      ))
        
      }
    })
    
    output$bs <- DT::renderDataTable({
      if(is.null(values$xbrl.vars)){
        NULL
      } else {
        BS <- bs1(values$xbrl.vars)
        values$BS <- BS
        DT::datatable(BS, rownames = FALSE,
                      extensions = c('Buttons', 'Scroller'), 
                      options = list(
                        dom = 'Bfrtip',
                        scrollX = TRUE,
                        scrollY = FALSE,
                        deferRender = TRUE,
                        paging = FALSE,
                        searching = FALSE,
                        buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print')
                      ))
        
      }
    })
    
    output$is <- DT::renderDataTable({
      if(is.null(values$xbrl.vars)){
        NULL
      } else {
        IS <- is1(values$xbrl.vars, comp.ticker())
        values$IS <- IS
        DT::datatable(IS, rownames = FALSE,
                      extensions = c('Buttons', 'Scroller'), 
                      options = list(
                        dom = 'Bfrtip',
                        scrollX = TRUE,
                        scrollY = FALSE,
                        deferRender = TRUE,
                        paging = FALSE,
                        searching = FALSE,
                        buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print')
                      ))
        
      }
    })
    
    output$cf <- DT::renderDataTable({
      if(is.null(values$xbrl.vars)){
        NULL
      } else {
        CF <- cf1(values$xbrl.vars)
        values$CF <- CF
        DT::datatable(CF, rownames = FALSE,
                      extensions = c('Buttons', 'Scroller'), 
                      options = list(
                        dom = 'Bfrtip',
                        scrollX = TRUE,
                        scrollY = FALSE,
                        deferRender = TRUE,
                        paging = FALSE,
                        searching = FALSE,
                        buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print')
                      ))
        
      }
    })
  }
)