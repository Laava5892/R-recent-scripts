##### PACKAGES #####
# install.packages('rsconnect')
# library('rsconnect')
# 
# rsconnect::setAccountInfo(name='paml-adobe', token='F2029EBA9515AEA913E45F3CA4B43BF0', secret='GGmXl7C77AlltpYQq6GjoA8vJPqg9Ug1BCYxZINH')
# 
# rsconnect::deployApp('path/to/your/app')


# 
# install.packages("rvest")
# library("rvest")
# getwd()
# setwd("/Users/lganesh/Desktop/Search_Dashboard") # change to your working directory and place the search diagonistics image in www folder
packages <- c('rvest', 'rsconnect', 'tsapi', 'RInside', 'mime', 'RCurl', 'jsonlite', 'stringr', 'futile.logger', 'reshape', 'ggplot2', 
              'DT', 'datasets', 'devtools', 'dplyr', 'lubridate', 'shinyjs', 'httr', 
              'jsonlite', 'markdown', 'shinyBS', 'shiny', 'tidyr', 'scales', 'lubridate','curl','shinydashboard','plotly','data.table')

# install.packages("shinydashboard")
# library(shinydashboard)
# 
# install.packages("plotly")
# library(plotly)
# 
# install.packages("data.table")
# library("data.table")
# 
# install.packages("futile.logger")
# library("futile.logger")
# determine which required packages are not installed
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]

# if packages are missing, install them
if(length(new.packages)) install.packages(new.packages)

# after package install, initialise all packages
lapply(packages, FUN = function(X) {
  do.call("require", list(X))
})

# klmno_user = " "
# klmno_password = " "

# ts_api
ts_api <- function (query = NA, db_tag = NA, dc = NA) 
{
  URL <- "https://ts-api.efrontier.com/api/super/execute"
  result <- postForm(URL, query = query, dbtags = db_tag, dc = dc, 
                     .opts = list(timeout = 600, ssl.verifypeer = TRUE))
  #.opts = curlOptions(header = TRUE, userpwd = "release:kT7J%*", netrc = TRUE))
  return(jsonlite::fromJSON(result))
}

# amo_db_get
amo_db_get <- function (query = NA, db_tag = NA, dc = NA, debug = FALSE) 
{
  if (debug) 
    flog.layout(layout.format(paste("[~l] [amo_db_get] [~t] ~m", 
                                    sep = "")))
  if (any(sapply(list(query, db_tag, dc), length)) < 1) 
    stop("Failed: missing input parameter [query, db_tag, dc]")
  if (any(sapply(list(query, db_tag, dc), is.character)) == 
      FALSE) 
    stop("Failed: all inputs must be strings")
  if (!(dc %in% c("scl2", "or1", "lon5", "all"))) 
    stop("Failed: Incorrect data centre ID")
  if (debug) 
    flog.info("Verified input parameters")
  if (debug) 
    flog.info("Attempting query: \"%s\"", gsub("\r?\n|\r|\\s+", 
                                               " ", query))
  func_start_time <- Sys.time()
  api_out <- ts_api(query = query, db_tag = db_tag, dc = dc)
  if (!valid_response(api_out)) 
    stop("Failed: please check your query is correct (dates must be strings)")
  if (valid_response(api_out)) {
    if (is.character(api_out$response) && grepl(sprintf("%s not found", 
                                                        db_tag), api_out$response)) {
      stop(sprintf("Failed: db_tag %s not found on %s data centre", 
                   db_tag, dc))
    }
    if (debug) 
      flog.info("Query successful for db_tag %s on %s dc", 
                db_tag, dc)
    if (debug) 
      flog.info("Completed (runtime: %.2f sec)", as.numeric(Sys.time() - 
                                                              func_start_time, units = "secs"))
    return(data.frame(api_out$response, stringsAsFactors = FALSE))
  }
}

valid_response <- function (input) {
  ifelse(input$status$code == "200" & length(input$response) > 0, TRUE, FALSE)}

# to enter PIDs in correct form in the table
options(scipen=999)
  
server = function(input, output, session ) {

  ##### Caching functionnn   nnb v                                                                                                                          
  cache_users <- function() {
    
    get_active_usernames <- function() {
      admin_query = "select userid, username, db_tag, tzid from users where managed = 'm';"
      #userids, names, db_tag from OR1 - US and A where (date(mtime) > (current_date - 90))
      admin_or1 = amo_db_get(query=admin_query, db_tag = 'admin', dc = 'or1', debug = TRUE)
      admin_or1$dc <-'or1'
      admin_global <- admin_or1
      #userids, names, db_tag from lon5 - UK
      admin_lon5 = amo_db_get(query=admin_query, db_tag = 'admin', dc = 'lon5', debug = TRUE)
      admin_lon5$dc <-'lon5'
      #rbind the DFs from each Data Center
      admin_global = rbind(admin_or1, admin_lon5)
      admin_global = admin_global[order(admin_global$username), ]
    }
    
    ##### FOR VARIOUS ACTIVE USERS #####
    
    cache_file <- "./users_cached.csv"     # set filename and path
    sprintf("Cache file: %s", cache_file)     # logging statement
    if( !file.exists(cache_file) ) {
      # if the cache_file does not exist, retrieve list of active
      # clients and write to file
      active_users <- get_active_usernames()
      #rtb_users <- rtb_users$response
      write.csv(active_users, file = cache_file, row.names=FALSE)
    } else {
      # if cache_file exists, determine whether it is outdated. 
      cache_modified = format(file.mtime(cache_file), "%Y-%m-%d") # get modified date for cache_file
      current_date = format(Sys.Date(), "%Y-%m-%d") # get current date
      if (cache_modified != current_date) {
        #if cache_file was not modified on the current date, it is old.
        #re-query rtb_users and overwrite local file
        active_users <- get_active_usernames()
        #rtb_users <- rtb_users$response
        write.csv(active_users, file = cache_file, row.names=FALSE)
      } else {
        # if cache_file exists and was last modified today, read rtb
        # users from the saved file without querying
        active_users <- read.csv(cache_file, stringsAsFactors = FALSE)
      }
    }
  
    return(active_users)
    
  }
  
  #####
  users <- cache_users() # execute function before shinyServer()
  
  users$dbtagdc = paste(users$db_tag, users$dc, sep=",")
  
  passdata <-eventReactive(input$goButton,{
    
    withProgress(message = 'Loading in progress...', value = 0, {
      n <- 10
      for (i in 1:n) {
        incProgress(1/n, detail = paste(""))
        Sys.sleep(.1)
      }
      
      if (!is.na(input$username != "All")) {
        users_1 <- users[users$username == input$username & users$dbtagdc == input$Dbtag_DC,]
        
        dbtag <- as.character(users_1$db_tag)
        dc_choose <- as.character(users_1$dc)

        # GET all pids
        
        query_all_pid <- "select pid from user_portfolios ; "
        pid_or1 <- amo_db_get(query=query_all_pid, db_tag = dbtag, dc = dc_choose, debug=TRUE)
        active_pid_or1 <- sprintf("(%s)", paste(unique(pid_or1$pid), collapse=","))
        active_pid_or1 <- noquote(gsub("\\(|\\)", "", active_pid_or1))
        
        date1 <- Sys.Date()-8
        date2 <- Sys.Date()-1
        
        admin_query_pid = sprintf("select pid,db_tag, userid as Client_Account, portfolio_id as Portfolio_Name, username as Client_Account_Name, status_code as Portfolio_Status from user_portfolios left join users using (userid) where pid in (%s) order by 1", active_pid_or1) #input[characterstring][3]
        admin_pid = amo_db_get(query=admin_query_pid, db_tag = dbtag, dc = dc_choose, debug = TRUE)
        admin_pidd <- admin_pid %>% mutate(portfolio_status = case_when(portfolio_status == 'a' ~ "Active",
                                                                        portfolio_status == 'i' ~ "Inactive",
                                                                        portfolio_status == 'z' ~ "Optimize",
                                                                        portfolio_status == 'r' ~ "Deleted" ))
        admin_pidd$dcchoose <- dc_choose
        admin_final <- admin_pidd
        
admin_query_daily_accuracy_agg = sprintf("SELECT p.pid as pid, 
SUM(predicted_clicks) as pred_clicks, 
SUM(actual_clicks) as act_clicks,
(100.0 * SUM(actual_clicks )/ SUM(NULLIF( predicted_clicks, 0 )) )::NUMERIC(20,2) AS click_acc,
SUM(predicted_spend) as pred_cost,
SUM(actual_spend) as act_cost,
SUM(budget)::NUMERIC(20,2) AS budget ,
(100.0 * SUM(actual_spend) / SUM(NULLIF( predicted_spend, 0 )) )::NUMERIC(20,2) AS cost_acc,
SUM(predicted_rev) as pred_rev,
SUM(crev) as act_rev,
(100.0 * SUM(crev )/ SUM(NULLIF ( predicted_rev, 0 ) ))::NUMERIC(20,2) AS rev_acc,
SUM(predicted_impr) as pred_impr,
SUM(actual_impr) as act_impr,
(100.0 * SUM(actual_impr)/SUM(NULLIF( predicted_impr, 0) ))::NUMERIC(20,2) AS impr_acc,
(100.0 * SUM((actual_spend/NULLIF( actual_clicks, 0 ))) /SUM(NULLIF((predicted_spend/NULLIF( predicted_clicks, 0 )),0))) ::NUMERIC(20,2) AS cpc_acc,
(100.0 *SUM(actual_spend )/ SUM(NULLIF( budget, 0 ) ))::NUMERIC(20,2) AS pacing_acc,
(100.0 * SUM(crev/NULLIF( actual_clicks, 0 )) /SUM(NULLIF((predicted_rev/NULLIF( predicted_clicks, 0 )),0))) ::NUMERIC(20,2) AS rpc_acc 
FROM ( SELECT (j.job_start_time AT TIME ZONE t.tz + INTERVAL '6 hours')::date as date,j.pid,
       AVG(j.converged_clicks)::NUMERIC(20,2) AS predicted_clicks,
       AVG(j.converged_impr)::NUMERIC(20,2) AS predicted_impr,
       ( AVG(j.converged_cost)/100.0 )::NUMERIC(20,2) AS predicted_spend,
       AVG(j.converged_rev)::NUMERIC(40,4) AS predicted_rev,
       AVG( CASE j.stid WHEN 5 THEN j.st_constraint ELSE j.budget_high END ) AS budget,
       COUNT(1) as opt_runs,
       MAX(j.job_start_time at time zone t.tz) as last_ran,
       ( array_agg(pov.objid ORDER BY j.job_start_time DESC))[1] as objid
       FROM optimizer_jobs j
       JOIN user_portfolios up ON (up.pid = j.pid)
       LEFT JOIN portfolio_objectives_versions pov ON (up.pid = pov.pid and j.job_start_time between pov.xstart_time and pov.xend_time)
       JOIN ( users u JOIN timezones t ON (t.tzid = u.tzid) ) t ON (up.userid = t.userid)
       WHERE up.pid in (%1$s)
       AND j.job_start_time >=  DATE '%2$s' - INTERVAL '2 days'
       AND j.job_end_time <= DATE '%3$s' + INTERVAL '2 days'
       AND (j.job_start_time at time zone t.tz + interval '6 hours')::DATE >= DATE '%2$s'
       AND (j.job_end_time at time zone t.tz + interval '6 hours')::DATE <= DATE '%3$s'
       GROUP BY 1,j.pid ) as p
FULL JOIN ( SELECT CPH.pid,WH.date,
           SUM(WH.clicks) AS actual_clicks,
           SUM(WH.impressions) AS actual_impr,
           ( SUM(WH.cost)/100.0 )::NUMERIC(20,2) AS actual_spend
           FROM day_sem_campaign_agg WH
           JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
           JOIN user_campaigns uc on (CPH.cid = uc.cid)
           JOIN user_accts ua on (uc.user_acctid = ua.user_acctid)
           WHERE CPH.pid in (%1$s)
           AND ua.sid <> 77
           AND WH.date >= '%2$s'
           AND WH.date <= '%3$s' 
           GROUP BY WH.date,CPH.pid 
           
           UNION ALL
           
           SELECT CPH.pid,WH.date,
           SUM(WH.est_clicks) AS actual_clicks,
           SUM(WH.est_impressions) AS actual_impr,
           ( SUM(WH.est_cost)/100.0 )::NUMERIC(20,2) AS actual_spend
           FROM day_trig_campaign_agg WH
           JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
           JOIN user_campaigns uc on (CPH.cid = uc.cid)
           JOIN user_accts ua on (uc.user_acctid = ua.user_acctid)
           WHERE CPH.pid in (%1$s)
           AND ua.sid = 77
           AND WH.date >= '%2$s'
           AND WH.date <= '%3$s'
           GROUP BY WH.date,CPH.pid 
           ) AS c USING (pid,date)       
FULL JOIN ( SELECT CPH.pid, WH.date,
             SUM( case when (device = 'm' AND mobile_weights_enabled = TRUE)
             then coalesce(f.mobile_weight, f.weight) 
             else f.weight 
             end * (COALESCE(CT_VALUE_LAST, 0) + COALESCE(CTVT_VALUE_LAST, 0) * (1 - 0) + COALESCE(VTCT_VALUE_LAST, 0) * 0 + 0.4 * ( COALESCE (VT_VALUE_LAST, 0)) ) )   as crev,
             (SUM( case when (device = 'm' AND mobile_weights_enabled = TRUE)
             then coalesce(f.mobile_weight, f.weight) 
             else f.weight 
             end * ( COALESCE(CT_VALUE_LAST, 0) / COALESCE(D.click_thru_percentage, 1) + COALESCE(CTVT_VALUE_LAST, 0) * (1 - 0) / COALESCE(D.click_thru_percentage, 1) + COALESCE(VTCT_VALUE_LAST, 0) * 0 / COALESCE(D.view_thru_percentage, 1) +  COALESCE (VT_VALUE_LAST, 0) * 0.4 / COALESCE(D.view_thru_percentage, 1)   ) ))::NUMERIC(20,2)   as crev_d
             FROM day_revenue_campaign_agg WH
             JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
             JOIN portfolio_objectives PO ON (CPH.pid = PO.pid)
             JOIN objective_function f ON (f.objid = PO.objid AND f.propertyid = WH.propertyid)
             JOIN objectives ob ON ob.objid = f.objid
             LEFT JOIN (   
             SELECT  pid,
             propertyid,
             (now() at time zone tz)::date - delay AS date,
             CASE
             WHEN click_thru_percentage = 0 
             THEN 1
             WHEN click_thru_percentage < 0.01
             THEN 0.01
             ELSE
             LEAST(click_thru_percentage, 1)
             END AS click_thru_percentage,
             CASE
             WHEN view_thru_percentage = 0 
             THEN 1
             WHEN view_thru_percentage < 0.01
             THEN 0.01
             ELSE
             LEAST(view_thru_percentage, 1)
             END AS view_thru_percentage
             FROM property_daily_delayed_rev_factors
             JOIN user_portfolios using(pid)
             JOIN users using(userid)
             JOIN timezones using(tzid)
             WHERE delay >= 0
             AND pid in (%1$s)
             ) D on (wh.propertyid = D.propertyid and CPH.pid = D.pid and WH.date = D.date)
             WHERE CPH.pid in (%1$s)
             AND WH.date >= '%2$s'
             AND WH.date <= '%3$s' 
             GROUP BY WH.date,CPH.pid ) AS cr USING (pid,date)
JOIN ( SELECT CPH.pid,WH.date,
       SUM( case when (device = 'm' AND mobile_weights_enabled = TRUE) then coalesce(f.mobile_weight, f.weight) else f.weight end * (COALESCE(CT_VALUE_LAST, 0) + (1 - 0) * COALESCE(CTVT_VALUE_LAST, 0) + 0.4 * ( COALESCE (VT_VALUE_LAST, 0) + 0 * COALESCE(VTCT_VALUE_LAST, 0) ) ) )   as trev
       FROM tday_revenue_campaign_agg WH
       JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
       JOIN portfolio_objectives PO ON (CPH.pid = PO.pid)
       JOIN objective_function f ON (f.objid = PO.objid AND f.propertyid = WH.propertyid)
       JOIN objectives ob ON ob.objid = f.objid
       WHERE CPH.pid in (%1$s)
       AND WH.date >= '%2$s'
       AND WH.date <= '%3$s' 
       GROUP BY WH.date,CPH.pid ) AS tr USING (pid,date)
GROUP BY 1
ORDER BY 1 DESC,2 ;", active_pid_or1,date1,date2)
        
        admin_daily_1 = amo_db_get(query=admin_query_daily_accuracy_agg, db_tag = dbtag, dc = dc_choose, debug = TRUE)
        admin_final <- merge(admin_final, admin_daily_1, by.x="pid", by.y = "pid", all.x = TRUE)
        
        admin_final[is.na(admin_final)] <- 0
        
        admin_final <- admin_final %>% 
          mutate(Scenario = case_when(cost_acc > 0 & cost_acc < 80 & rpc_acc >=80 & rpc_acc<=120 ~ "Underspend AND RPC Accuracy OK",
                                      cost_acc > 0 & cost_acc < 80 & rpc_acc <80 | rpc_acc >120 ~ "Underspend AND Poor RPC Accuracy ",
                                      cost_acc > 120 & rpc_acc >=80 & rpc_acc<=120 ~ "Overspend AND RPC Accuracy OK",
                                      cost_acc > 120 & rpc_acc > 0 &  rpc_acc < 80 | rpc_acc > 120  ~ "Overspend AND Poor RPC Accuracy",
                                      cost_acc >= 80 & cost_acc <=120 & rpc_acc >=80 & rpc_acc<=120 ~ "Cost Accuracy Within Range AND RPC Accuracy OK",
                                      cost_acc >= 80 & cost_acc <=120 & rpc_acc > 0 & rpc_acc < 80 | rpc_acc > 120  ~ "Cost Accuracy Within Range AND Poor RPC Accuracy",
                                      cost_acc == 0 & rpc_acc == 0 ~ "No Data"
          ))
        
        admin_final <- admin_final %>% mutate(Spend_Scenario = case_when(cost_acc > 0 & cost_acc < 80 ~ "Underspend",
                                                                         cost_acc > 120 ~ "Overspend",
                                                                         cost_acc >= 80 & cost_acc <=120 ~ "Cost Accuracy Within Range",
                                                                         cost_acc == 0 ~ "No Data"))
        
        admin_final <- admin_final %>% mutate(RPC_Scenario = case_when(rpc_acc > 120 | rpc_acc < 80 ~ "Poor RPC Accuracy",
                                                                       rpc_acc >= 80 & rpc_acc <=120 ~ "RPC Accuracy OK",
                                                                       rpc_acc == 0 ~ "No Data"))
        
        admin_final <- admin_final[order(admin_final$pred_cost, decreasing = TRUE),]
        
        admin_final$portfolionamepid = paste(admin_final$portfolio_name, admin_final$pid, sep=",")
        
        return(admin_final)
      }
      
    })
    
  })
  
  passdata1 <-eventReactive(input$go,{
    
                     withProgress(message = 'Loading in progress...', value = 0, {
                       n <- 10
                       for (i in 1:n) {
                         incProgress(1/n, detail = paste(""))
                         Sys.sleep(.1)
                       }
    
                       if (!is.na(input$PortfolioNamePID != "All")) {
                         pf_1 <- passdata()[passdata()$portfolionamepid == input$PortfolioNamePID,]
                         
                         dbtag1 <- as.character(pf_1$db_tag)
                         dc_choose1 <- as.character(pf_1$dcchoose)
                         pid1 <- as.character(pf_1$pid)
                         
                         # dbtag1 = 'c11504'
                         # dc_choose1 = 'lon5'
                         # pid1= '1700000005'
                         
                         date1 <- Sys.Date()-8
                         date2 <- Sys.Date()-1

                         date3 <- Sys.Date()-90
                         date4 <- Sys.Date()-1
                         
                         date5 <- Sys.Date()-30
                         date6 <- Sys.Date()-1
                         
    
    admin_query_pid = sprintf("select pid,db_tag, userid as Client_Account, portfolio_id as Portfolio_Name, username as Client_Account_Name, status_code as Portfolio_Status from user_portfolios left join users using (userid) where pid = (%s) order by 1", pid1) #input[characterstring][3]
    admin_pid = amo_db_get(query=admin_query_pid, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)
    admin_pidd <- admin_pid %>% mutate(portfolio_status = case_when(portfolio_status == 'a' ~ "Active",
                                                                    portfolio_status == 'i' ~ "Inactive",
                                                                    portfolio_status == 'z' ~ "Optimize",
                                                                    portfolio_status == 'r' ~ "Deleted" ))
    # admin_pidd <- admin_pidd %>%filter(portfolio_status == 'Optimize' | portfolio_status == 'Active')
    # admin_pidd <- cache_portfolio(dbtag, dc_choose)
    admin_final <- admin_pidd
    

    admin_query_optv6v7 = sprintf(" select c.pid, p.handler as Opt_version from
                              (select pid,
                              COALESCE(model_type, 'o') as model_type,
                              COALESCE(modelid, 68) as modelid
                              from
                              user_portfolios
                              left join model_dispatch_portfolio_level using(pid)) as c
                              left join models p on (c.modelid=p.modelid)
                              where c.modelid in (81,68)
                              and pid = (%s)
                              order by 1;", pid1)

    admin_optv6v7 = amo_db_get(query=admin_query_optv6v7, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)

    admin_final <- merge(admin_final, admin_optv6v7, by.x="pid", by.y = "pid", all.x = TRUE)


    #DBA

    admin_query_dba_computer = "with base_1 as(

    select oa.pid, 'mobile' as device, coalesce(param_value, default_value)::boolean as value
    from model_parameters mp
    join optimizer_arguments oa using(paramid)
    where param_name like 'enable_mobile_bid_adjustment'
                     ),
    base_2 as(

    select oa.pid, 'mobile' as device, coalesce(param_value, default_value) as max_bid_adj
    from model_parameters mp
    join optimizer_arguments oa using(paramid)
    where param_name like 'min_mobile_bid_adjustment'
    ),
    base_3 as(

    select oa.pid, 'mobile' as device, coalesce(param_value, default_value) as min_bid_adj
    from model_parameters mp
    join optimizer_arguments oa using(paramid)
    where param_name like 'max_mobile_bid_adjustment'
    ),
    optv7 as (
    select pid, bid_adjustment_type as device, coalesce(aba.auto_adjust,false)::boolean as value, aba.max_bid_adjustment as max_bid_adj, aba.min_bid_adjustment as min_bid_adj
    from auto_bid_adjustments aba
    join auto_bid_adjustments_types abt using(bid_adjustment_type_id)
    join model_dispatch_portfolio_level mp using(pid)
    join models using(modelid)
    where handler like 'optv7'
    and bid_adjustment_type in ('computer','mobile','tablet')
    )
    select up.pid, d.device,
    case
    when d.device::text = 'mobile'
    then
    coalesce(optv7.value, coalesce(b1.value, false))
    else
    coalesce(optv7.value, false)
    end as BA_enable_computer,
    case
    when d.device::text = 'mobile'
    then
    coalesce(optv7.max_bid_adj::text, coalesce(b2.max_bid_adj,'none'))
    else
    coalesce(optv7.max_bid_adj::text, 'none')
    end as max_BA_computer,
    case
    when d.device::text = 'mobile'
    then
    coalesce(optv7.min_bid_adj::text, coalesce(b3.min_bid_adj,'none'))
    else
    coalesce(optv7.min_bid_adj::text, 'none')
    end as min_BA_computer
    from user_portfolios up
    join (select * from ( values ('computer'),('tablet'), ('mobile') ) as foo(device) ) as d on (True)
    left join base_1 b1 on ( d.device::text = b1.device::text and b1.pid = up.pid)
    left join base_2 b2 on ( d.device::text = b2.device::text and b2.pid = up.pid)
    left join base_3 b3 on ( d.device::text = b3.device::text and b3.pid = up.pid)
    left join optv7 on (optv7.device::text = d.device::text and optv7.pid = up.pid)
    where d.device='computer';"
    admin_opt_dba_computer = amo_db_get(query=admin_query_dba_computer, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)

    admin_final <- merge(admin_final,admin_opt_dba_computer,by.x = "pid",by.y = "pid",all.x = TRUE)


    admin_query_dba_tablet = "with base_1 as(

    select oa.pid, 'mobile' as device, coalesce(param_value, default_value)::boolean as value
    from model_parameters mp
    join optimizer_arguments oa using(paramid)
    where param_name like 'enable_mobile_bid_adjustment'
  ),
    base_2 as(

    select oa.pid, 'mobile' as device, coalesce(param_value, default_value) as max_bid_adj
    from model_parameters mp
    join optimizer_arguments oa using(paramid)
    where param_name like 'min_mobile_bid_adjustment'
    ),
    base_3 as(

    select oa.pid, 'mobile' as device, coalesce(param_value, default_value) as min_bid_adj
    from model_parameters mp
    join optimizer_arguments oa using(paramid)
    where param_name like 'max_mobile_bid_adjustment'
    ),
    optv7 as (
    select pid, bid_adjustment_type as device, coalesce(aba.auto_adjust,false)::boolean as value, aba.max_bid_adjustment as max_bid_adj, aba.min_bid_adjustment as min_bid_adj
    from auto_bid_adjustments aba
    join auto_bid_adjustments_types abt using(bid_adjustment_type_id)
    join model_dispatch_portfolio_level mp using(pid)
    join models using(modelid)
    where handler like 'optv7'
    and bid_adjustment_type in ('computer','mobile','tablet')
    )
    select up.pid, d.device,
    case
    when d.device::text = 'mobile'
    then
    coalesce(optv7.value, coalesce(b1.value, false))
    else
    coalesce(optv7.value, false)
    end as BA_enable_tablet,
    case
    when d.device::text = 'mobile'
    then
    coalesce(optv7.max_bid_adj::text, coalesce(b2.max_bid_adj,'none'))
    else
    coalesce(optv7.max_bid_adj::text, 'none')
    end as max_BA_tablet,
    case
    when d.device::text = 'mobile'
    then
    coalesce(optv7.min_bid_adj::text, coalesce(b3.min_bid_adj,'none'))
    else
    coalesce(optv7.min_bid_adj::text, 'none')
    end as min_BA_tablet
    from user_portfolios up
    join (select * from ( values ('computer'),('tablet'), ('mobile') ) as foo(device) ) as d on (True)
    left join base_1 b1 on ( d.device::text = b1.device::text and b1.pid = up.pid)
    left join base_2 b2 on ( d.device::text = b2.device::text and b2.pid = up.pid)
    left join base_3 b3 on ( d.device::text = b3.device::text and b3.pid = up.pid)
    left join optv7 on (optv7.device::text = d.device::text and optv7.pid = up.pid)
    where d.device='tablet';"
    admin_opt_dba_tablet = amo_db_get(query=admin_query_dba_tablet, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)

    admin_final <- merge(admin_final,admin_opt_dba_tablet,by.x = "pid",by.y = "pid",all.x = TRUE)


    admin_query_dba_mobile = "with base_1 as(

    select oa.pid, 'mobile' as device, coalesce(param_value, default_value)::boolean as value
    from model_parameters mp
    join optimizer_arguments oa using(paramid)
    where param_name like 'enable_mobile_bid_adjustment'
    ),
    base_2 as(

    select oa.pid, 'mobile' as device, coalesce(param_value, default_value) as max_bid_adj
    from model_parameters mp
    join optimizer_arguments oa using(paramid)
    where param_name like 'min_mobile_bid_adjustment'
    ),
    base_3 as(

    select oa.pid, 'mobile' as device, coalesce(param_value, default_value) as min_bid_adj
    from model_parameters mp
    join optimizer_arguments oa using(paramid)
    where param_name like 'max_mobile_bid_adjustment'
    ),
    optv7 as (
    select pid, bid_adjustment_type as device, coalesce(aba.auto_adjust,false)::boolean as value, aba.max_bid_adjustment as max_bid_adj, aba.min_bid_adjustment as min_bid_adj
    from auto_bid_adjustments aba
    join auto_bid_adjustments_types abt using(bid_adjustment_type_id)
    join model_dispatch_portfolio_level mp using(pid)
    join models using(modelid)
    where handler like 'optv7'
    and bid_adjustment_type in ('computer','mobile','tablet')
    )
    select up.pid, d.device,
    case
    when d.device::text = 'mobile'
    then
    coalesce(optv7.value, coalesce(b1.value, false))
    else
    coalesce(optv7.value, false)
    end as BA_enable_mobile,
    case
    when d.device::text = 'mobile'
    then
    coalesce(optv7.max_bid_adj::text, coalesce(b2.max_bid_adj,'none'))
    else
    coalesce(optv7.max_bid_adj::text, 'none')
    end as max_BA_mobile,
    case
    when d.device::text = 'mobile'
    then
    coalesce(optv7.min_bid_adj::text, coalesce(b3.min_bid_adj,'none'))
    else
    coalesce(optv7.min_bid_adj::text, 'none')
    end as min_BA_mobile
    from user_portfolios up
    join (select * from ( values ('computer'),('tablet'), ('mobile') ) as foo(device) ) as d on (True)
    left join base_1 b1 on ( d.device::text = b1.device::text and b1.pid = up.pid)
    left join base_2 b2 on ( d.device::text = b2.device::text and b2.pid = up.pid)
    left join base_3 b3 on ( d.device::text = b3.device::text and b3.pid = up.pid)
    left join optv7 on (optv7.device::text = d.device::text and optv7.pid = up.pid)
    where d.device='mobile';"
admin_opt_dba_mobile = amo_db_get(query=admin_query_dba_mobile, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)

admin_final <- merge(admin_final,admin_opt_dba_mobile,by.x = "pid",by.y = "pid",all.x = TRUE)

admin_final <- admin_final %>% mutate(DEVfeature = case_when(ba_enable_mobile==TRUE | ba_enable_tablet==TRUE | ba_enable_computer==TRUE ~ "Enabled",
                                                 ba_enable_mobile==FALSE & ba_enable_tablet==FALSE & ba_enable_computer==FALSE ~ "Not_Enabled"))

    
    #Daily Accuracy... daily level
admin_query_daily_accuracy = sprintf("SELECT p.pid as pid,TO_CHAR(date, 'YYYY-MM-DD') as date, extract(isodow from date) as dow, 
     predicted_clicks as pred_clicks, 
     actual_clicks as act_clicks,
     (100.0 * actual_clicks / NULLIF( predicted_clicks, 0 ) )::NUMERIC(20,2) AS click_acc,
     predicted_spend as pred_cost,
     actual_spend as act_cost,
     budget::NUMERIC(20,2) AS budget ,
     (100.0 * actual_spend / NULLIF( predicted_spend, 0 ) )::NUMERIC(20,2) AS cost_acc,
     predicted_rev as pred_rev,
     crev as act_rev,
     (100.0 * crev / NULLIF ( predicted_rev, 0 ) )::NUMERIC(20,2) AS rev_acc,
     crev_d as act_rev_adj, 
     (100.0 * crev_d / NULLIF ( predicted_rev, 0 ) )::NUMERIC(20,2) AS rev_d_acc,
     trev as act_trev,  
     (100.0 * trev / NULLIF ( predicted_rev, 0 ) )::NUMERIC(20,2) AS trev_acc,
     predicted_impr as pred_impr,
     actual_impr as act_impr,
     (100.0 * actual_impr/ NULLIF( predicted_impr, 0) )::NUMERIC(20,2) AS impr_acc,
     opt_runs,objid, last_ran,
     (100.0 * (actual_spend/NULLIF( actual_clicks, 0 )) /NULLIF((predicted_spend/NULLIF( predicted_clicks, 0 )),0)) ::NUMERIC(20,2) AS cpc_acc,
     (100.0 * actual_spend / NULLIF( budget, 0 ) )::NUMERIC(20,2) AS pacing_accuracy,
     (100.0 * (crev/NULLIF( actual_clicks, 0 )) /NULLIF((predicted_rev/NULLIF( predicted_clicks, 0 )),0)) ::NUMERIC(20,2) AS rpc_acc 
     FROM ( SELECT (j.job_start_time AT TIME ZONE t.tz + INTERVAL '6 hours')::date as date,j.pid,
           AVG(j.converged_clicks)::NUMERIC(20,2) AS predicted_clicks,
           AVG(j.converged_impr)::NUMERIC(20,2) AS predicted_impr,
           ( AVG(j.converged_cost)/100.0 )::NUMERIC(20,2) AS predicted_spend,
           AVG(j.converged_rev)::NUMERIC(40,4) AS predicted_rev,
           AVG( CASE j.stid WHEN 5 THEN j.st_constraint ELSE j.budget_high END ) AS budget,
           COUNT(1) as opt_runs,
           MAX(j.job_start_time at time zone t.tz) as last_ran,
           ( array_agg(pov.objid ORDER BY j.job_start_time DESC))[1] as objid
           FROM optimizer_jobs j
           JOIN user_portfolios up ON (up.pid = j.pid)
           LEFT JOIN portfolio_objectives_versions pov ON (up.pid = pov.pid and j.job_start_time between pov.xstart_time and pov.xend_time)
           JOIN ( users u JOIN timezones t ON (t.tzid = u.tzid) ) t ON (up.userid = t.userid)
           WHERE up.pid = (%1$s)
           AND j.job_start_time >=  DATE '%2$s' - INTERVAL '2 days'
           AND j.job_end_time <= DATE '%3$s' + INTERVAL '2 days'
           AND (j.job_start_time at time zone t.tz + interval '6 hours')::DATE >= DATE '%2$s'
           AND (j.job_end_time at time zone t.tz + interval '6 hours')::DATE <= DATE '%3$s'
           GROUP BY 1,j.pid ) as p
FULL JOIN ( SELECT CPH.pid,WH.date,
           SUM(WH.clicks) AS actual_clicks,
           SUM(WH.impressions) AS actual_impr,
           ( SUM(WH.cost)/100.0 )::NUMERIC(20,2) AS actual_spend
           FROM day_sem_campaign_agg WH
           JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
           JOIN user_campaigns uc on (CPH.cid = uc.cid)
           JOIN user_accts ua on (uc.user_acctid = ua.user_acctid)
           WHERE CPH.pid = (%1$s)
           AND ua.sid <> 77
           AND WH.date >= '%2$s'
           AND WH.date <= '%3$s' 
           GROUP BY WH.date,CPH.pid 
           
           UNION ALL
           
           SELECT CPH.pid,WH.date,
           SUM(WH.est_clicks) AS actual_clicks,
           SUM(WH.est_impressions) AS actual_impr,
           ( SUM(WH.est_cost)/100.0 )::NUMERIC(20,2) AS actual_spend
           FROM day_trig_campaign_agg WH
           JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
           JOIN user_campaigns uc on (CPH.cid = uc.cid)
           JOIN user_accts ua on (uc.user_acctid = ua.user_acctid)
           WHERE CPH.pid = (%1$s)
           AND ua.sid = 77
           AND WH.date >= '%2$s'
           AND WH.date <= '%3$s' 
           GROUP BY WH.date,CPH.pid 
           ) AS c USING (pid,date)       
FULL JOIN ( SELECT CPH.pid, WH.date,
             SUM( case when (device = 'm' AND mobile_weights_enabled = TRUE)
             then coalesce(f.mobile_weight, f.weight) 
             else f.weight 
             end * (COALESCE(CT_VALUE_LAST, 0) + COALESCE(CTVT_VALUE_LAST, 0) * (1 - 0) + COALESCE(VTCT_VALUE_LAST, 0) * 0 + 0.4 * ( COALESCE (VT_VALUE_LAST, 0)) ) )   as crev,
             
             (SUM( case when (device = 'm' AND mobile_weights_enabled = TRUE)
             then coalesce(f.mobile_weight, f.weight) 
             else f.weight 
             end * ( COALESCE(CT_VALUE_LAST, 0) / COALESCE(D.click_thru_percentage, 1) + COALESCE(CTVT_VALUE_LAST, 0) * (1 - 0) / COALESCE(D.click_thru_percentage, 1) + COALESCE(VTCT_VALUE_LAST, 0) * 0 / COALESCE(D.view_thru_percentage, 1) +  COALESCE (VT_VALUE_LAST, 0) * 0.4 / COALESCE(D.view_thru_percentage, 1)   ) ))::NUMERIC(20,2)   as crev_d
             FROM day_revenue_campaign_agg WH
             JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
             JOIN portfolio_objectives PO ON (CPH.pid = PO.pid)
             JOIN objective_function f ON (f.objid = PO.objid AND f.propertyid = WH.propertyid)
             JOIN objectives ob ON ob.objid = f.objid
             LEFT JOIN (   
             SELECT  pid,
             propertyid,
             (now() at time zone tz)::date - delay AS date,
             CASE
             WHEN click_thru_percentage = 0 
             THEN 1
             WHEN click_thru_percentage < 0.01
             THEN 0.01
             ELSE
             LEAST(click_thru_percentage, 1)
             END AS click_thru_percentage,
             CASE
             WHEN view_thru_percentage = 0 
             THEN 1
             WHEN view_thru_percentage < 0.01
             THEN 0.01
             ELSE
             LEAST(view_thru_percentage, 1)
             END AS view_thru_percentage
             FROM property_daily_delayed_rev_factors
             JOIN user_portfolios using(pid)
             JOIN users using(userid)
             JOIN timezones using(tzid)
             WHERE delay >= 0
             AND pid = (%1$s)
             ) D on (wh.propertyid = D.propertyid and CPH.pid = D.pid and WH.date = D.date)
             WHERE CPH.pid = (%1$s)
             AND WH.date >= '%2$s'
             AND WH.date <= '%3$s' 
             GROUP BY WH.date,CPH.pid ) AS cr USING (pid,date)
             JOIN ( SELECT CPH.pid,WH.date,
             SUM( case when (device = 'm' AND mobile_weights_enabled = TRUE) then coalesce(f.mobile_weight, f.weight) else f.weight end * (COALESCE(CT_VALUE_LAST, 0) + (1 - 0) * COALESCE(CTVT_VALUE_LAST, 0) + 
             0.4 * ( COALESCE (VT_VALUE_LAST, 0) + 0 * COALESCE(VTCT_VALUE_LAST, 0) ) ) )   as trev
             FROM tday_revenue_campaign_agg WH
             JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
             JOIN portfolio_objectives PO ON (CPH.pid = PO.pid)
             JOIN objective_function f ON (f.objid = PO.objid AND f.propertyid = WH.propertyid)
             JOIN objectives ob ON ob.objid = f.objid
             WHERE CPH.pid = (%1$s)
             AND WH.date >= '%2$s'
             AND WH.date <= '%3$s' 
             GROUP BY WH.date,CPH.pid ) AS tr USING (pid,date)
ORDER BY 1 DESC,2 ;", pid1,date1,date2)
    #as.character(input$dateRange[1]),as.character(input$dateRange[2])
    #,date1,date2,active_pid_or1,date1,date2,active_pid_or1,date1,date2,active_pid_or1,active_pid_or1,date1,date2,active_pid_or1,date1,date2)
    
    #daily accuracy... aggregate level

    #Daily Accuracy... daily level
admin_query_daily_accuracy_1 = sprintf("SELECT p.pid as pid,TO_CHAR(date, 'YYYY-MM-DD') as date, extract(isodow from date) as dow, 
predicted_clicks as pred_clicks, 
actual_clicks as act_clicks,
(100.0 * actual_clicks / NULLIF( predicted_clicks, 0 ) )::NUMERIC(20,2) AS click_acc,
predicted_spend as pred_cost,
actual_spend as act_cost,
budget::NUMERIC(20,2) AS budget ,
(100.0 * actual_spend / NULLIF( predicted_spend, 0 ) )::NUMERIC(20,2) AS cost_acc,
predicted_rev as pred_rev,
crev as act_rev,
(100.0 * crev / NULLIF ( predicted_rev, 0 ) )::NUMERIC(20,2) AS rev_acc,
crev_d as act_rev_adj, 
(100.0 * crev_d / NULLIF ( predicted_rev, 0 ) )::NUMERIC(20,2) AS rev_d_acc,
trev as act_trev,  
(100.0 * trev / NULLIF ( predicted_rev, 0 ) )::NUMERIC(20,2) AS trev_acc,
predicted_impr as pred_impr,
actual_impr as act_impr,
(100.0 * actual_impr/ NULLIF( predicted_impr, 0) )::NUMERIC(20,2) AS impr_acc,
opt_runs,objid, last_ran,
(100.0 * (actual_spend/NULLIF( actual_clicks, 0 )) /NULLIF((predicted_spend/NULLIF( predicted_clicks, 0 )),0)) ::NUMERIC(20,2) AS cpc_acc,
(100.0 * actual_spend / NULLIF( budget, 0 ) )::NUMERIC(20,2) AS pacing_accuracy,
(100.0 * (crev/NULLIF( actual_clicks, 0 )) /NULLIF((predicted_rev/NULLIF( predicted_clicks, 0 )),0)) ::NUMERIC(20,2) AS rpc_acc 
FROM ( SELECT (j.job_start_time AT TIME ZONE t.tz + INTERVAL '6 hours')::date as date,j.pid,
       AVG(j.converged_clicks)::NUMERIC(20,2) AS predicted_clicks,
       AVG(j.converged_impr)::NUMERIC(20,2) AS predicted_impr,
       ( AVG(j.converged_cost)/100.0 )::NUMERIC(20,2) AS predicted_spend,
       AVG(j.converged_rev)::NUMERIC(40,4) AS predicted_rev,
       AVG( CASE j.stid WHEN 5 THEN j.st_constraint ELSE j.budget_high END ) AS budget,
       COUNT(1) as opt_runs,
       MAX(j.job_start_time at time zone t.tz) as last_ran,
       ( array_agg(pov.objid ORDER BY j.job_start_time DESC))[1] as objid
       FROM optimizer_jobs j
       JOIN user_portfolios up ON (up.pid = j.pid)
       LEFT JOIN portfolio_objectives_versions pov ON (up.pid = pov.pid and j.job_start_time between pov.xstart_time and pov.xend_time)
       JOIN ( users u JOIN timezones t ON (t.tzid = u.tzid) ) t ON (up.userid = t.userid)
       WHERE up.pid = (%1$s)
       AND j.job_start_time >=  DATE '%2$s' - INTERVAL '2 days'
       AND j.job_end_time <= DATE '%3$s' + INTERVAL '2 days'
       AND (j.job_start_time at time zone t.tz + interval '6 hours')::DATE >= DATE '%2$s'
       AND (j.job_end_time at time zone t.tz + interval '6 hours')::DATE <= DATE '%3$s'
       GROUP BY 1,j.pid ) as p
FULL JOIN ( SELECT CPH.pid,WH.date,
             SUM(WH.clicks) AS actual_clicks,
             SUM(WH.impressions) AS actual_impr,
             ( SUM(WH.cost)/100.0 )::NUMERIC(20,2) AS actual_spend
             FROM day_sem_campaign_agg WH
             JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
             JOIN user_campaigns uc on (CPH.cid = uc.cid)
             JOIN user_accts ua on (uc.user_acctid = ua.user_acctid)
             WHERE CPH.pid = (%1$s)
             AND ua.sid <> 77
             AND WH.date >= '%2$s'
             AND WH.date <= '%3$s' 
             GROUP BY WH.date,CPH.pid 
             
             UNION ALL
             
             SELECT CPH.pid,WH.date,
             SUM(WH.est_clicks) AS actual_clicks,
             SUM(WH.est_impressions) AS actual_impr,
             ( SUM(WH.est_cost)/100.0 )::NUMERIC(20,2) AS actual_spend
             FROM day_trig_campaign_agg WH
             JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
             JOIN user_campaigns uc on (CPH.cid = uc.cid)
             JOIN user_accts ua on (uc.user_acctid = ua.user_acctid)
             WHERE CPH.pid = (%1$s)
             AND ua.sid = 77
             AND WH.date >= '%2$s'
             AND WH.date <= '%3$s' 
             GROUP BY WH.date,CPH.pid 
             ) AS c USING (date,pid)       
FULL JOIN ( SELECT CPH.pid, WH.date,
             SUM( case when (device = 'm' AND mobile_weights_enabled = TRUE)
             then coalesce(f.mobile_weight, f.weight) 
             else f.weight 
             end * (COALESCE(CT_VALUE_LAST, 0) + COALESCE(CTVT_VALUE_LAST, 0) * (1 - 0) + COALESCE(VTCT_VALUE_LAST, 0) * 0 + 0.4 * ( COALESCE (VT_VALUE_LAST, 0)) ) )   as crev,
             
             (SUM( case when (device = 'm' AND mobile_weights_enabled = TRUE)
             then coalesce(f.mobile_weight, f.weight) 
             else f.weight 
             end * ( COALESCE(CT_VALUE_LAST, 0) / COALESCE(D.click_thru_percentage, 1) + COALESCE(CTVT_VALUE_LAST, 0) * (1 - 0) / COALESCE(D.click_thru_percentage, 1) + COALESCE(VTCT_VALUE_LAST, 0) * 0 / COALESCE(D.view_thru_percentage, 1) +  COALESCE (VT_VALUE_LAST, 0) * 0.4 / COALESCE(D.view_thru_percentage, 1)   ) ))::NUMERIC(20,2)   as crev_d
             FROM day_revenue_campaign_agg WH
             JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
             JOIN portfolio_objectives PO ON (CPH.pid = PO.pid)
             JOIN objective_function f ON (f.objid = PO.objid AND f.propertyid = WH.propertyid)
             JOIN objectives ob ON ob.objid = f.objid
             LEFT JOIN (   
             SELECT  pid,
             propertyid,
             (now() at time zone tz)::date - delay AS date,
             CASE
             WHEN click_thru_percentage = 0 
             THEN 1
             WHEN click_thru_percentage < 0.01
             THEN 0.01
             ELSE
             LEAST(click_thru_percentage, 1)
             END AS click_thru_percentage,
             CASE
             WHEN view_thru_percentage = 0 
             THEN 1
             WHEN view_thru_percentage < 0.01
             THEN 0.01
             ELSE
             LEAST(view_thru_percentage, 1)
             END AS view_thru_percentage
             FROM property_daily_delayed_rev_factors
             JOIN user_portfolios using(pid)
             JOIN users using(userid)
             JOIN timezones using(tzid)
             WHERE delay >= 0
             AND pid = (%1$s)
             ) D on (wh.propertyid = D.propertyid and CPH.pid = D.pid and WH.date = D.date)
             WHERE CPH.pid = (%1$s)
             AND WH.date >= '%2$s'
             AND WH.date <= '%3$s' 
             GROUP BY WH.date,CPH.pid ) AS cr USING (date,pid)
             JOIN ( SELECT CPH.pid,WH.date,
             SUM( case when (device = 'm' AND mobile_weights_enabled = TRUE) then coalesce(f.mobile_weight, f.weight) else f.weight end * (COALESCE(CT_VALUE_LAST, 0) + (1 - 0) * COALESCE(CTVT_VALUE_LAST, 0) + 
             0.4 * ( COALESCE (VT_VALUE_LAST, 0) + 0 * COALESCE(VTCT_VALUE_LAST, 0) ) ) )   as trev
             FROM tday_revenue_campaign_agg WH
             JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
             JOIN portfolio_objectives PO ON (CPH.pid = PO.pid)
             JOIN objective_function f ON (f.objid = PO.objid AND f.propertyid = WH.propertyid)
             JOIN objectives ob ON ob.objid = f.objid
             WHERE CPH.pid = (%1$s)
             AND WH.date >= '%2$s'
             AND WH.date <= '%3$s' 
             GROUP BY WH.date,CPH.pid ) AS tr USING (date,pid)
ORDER BY 1 DESC,2 ;", pid1,date3,date4)

admin_query_daily_accuracy_2 = sprintf("SELECT p.pid as pid,TO_CHAR(date, 'YYYY-MM-DD') as date, extract(isodow from date) as dow, 
predicted_clicks as pred_clicks, 
actual_clicks as act_clicks,
(100.0 * actual_clicks / NULLIF( predicted_clicks, 0 ) )::NUMERIC(20,2) AS click_acc,
predicted_spend as pred_cost,
actual_spend as act_cost,
budget::NUMERIC(20,2) AS budget ,
(100.0 * actual_spend / NULLIF( predicted_spend, 0 ) )::NUMERIC(20,2) AS cost_acc,
predicted_rev as pred_rev,
crev as act_rev,
(100.0 * crev / NULLIF ( predicted_rev, 0 ) )::NUMERIC(20,2) AS rev_acc,
crev_d as act_rev_adj, 
(100.0 * crev_d / NULLIF ( predicted_rev, 0 ) )::NUMERIC(20,2) AS rev_d_acc,
trev as act_trev,  
(100.0 * trev / NULLIF ( predicted_rev, 0 ) )::NUMERIC(20,2) AS trev_acc,
predicted_impr as pred_impr,
actual_impr as act_impr,
(100.0 * actual_impr/ NULLIF( predicted_impr, 0) )::NUMERIC(20,2) AS impr_acc,
opt_runs,objid, last_ran,
(100.0 * (actual_spend/NULLIF( actual_clicks, 0 )) /NULLIF((predicted_spend/NULLIF( predicted_clicks, 0 )),0)) ::NUMERIC(20,2) AS cpc_acc,
(100.0 * actual_spend / NULLIF( budget, 0 ) )::NUMERIC(20,2) AS pacing_accuracy,
(100.0 * (crev/NULLIF( actual_clicks, 0 )) /NULLIF((predicted_rev/NULLIF( predicted_clicks, 0 )),0)) ::NUMERIC(20,2) AS rpc_acc 
FROM ( SELECT (j.job_start_time AT TIME ZONE t.tz + INTERVAL '6 hours')::date as date,j.pid,
       AVG(j.converged_clicks)::NUMERIC(20,2) AS predicted_clicks,
       AVG(j.converged_impr)::NUMERIC(20,2) AS predicted_impr,
       ( AVG(j.converged_cost)/100.0 )::NUMERIC(20,2) AS predicted_spend,
       AVG(j.converged_rev)::NUMERIC(40,4) AS predicted_rev,
       AVG( CASE j.stid WHEN 5 THEN j.st_constraint ELSE j.budget_high END ) AS budget,
       COUNT(1) as opt_runs,
       MAX(j.job_start_time at time zone t.tz) as last_ran,
       ( array_agg(pov.objid ORDER BY j.job_start_time DESC))[1] as objid
       FROM optimizer_jobs j
       JOIN user_portfolios up ON (up.pid = j.pid)
       LEFT JOIN portfolio_objectives_versions pov ON (up.pid = pov.pid and j.job_start_time between pov.xstart_time and pov.xend_time)
       JOIN ( users u JOIN timezones t ON (t.tzid = u.tzid) ) t ON (up.userid = t.userid)
       WHERE up.pid = (%1$s)
       AND j.job_start_time >=  DATE '%2$s' - INTERVAL '2 days'
       AND j.job_end_time <= DATE '%3$s' + INTERVAL '2 days'
       AND (j.job_start_time at time zone t.tz + interval '6 hours')::DATE >= DATE '%2$s'
       AND (j.job_end_time at time zone t.tz + interval '6 hours')::DATE <= DATE '%3$s'
       GROUP BY 1,j.pid ) as p
FULL JOIN ( SELECT CPH.pid,WH.date,
           SUM(WH.clicks) AS actual_clicks,
           SUM(WH.impressions) AS actual_impr,
           ( SUM(WH.cost)/100.0 )::NUMERIC(20,2) AS actual_spend
           FROM day_sem_campaign_agg WH
           JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
           JOIN user_campaigns uc on (CPH.cid = uc.cid)
           JOIN user_accts ua on (uc.user_acctid = ua.user_acctid)
           WHERE CPH.pid = (%1$s)
           AND ua.sid <> 77
           AND WH.date >= '%2$s'
           AND WH.date <= '%3$s' 
           GROUP BY WH.date,CPH.pid 
           
           UNION ALL
           
           SELECT CPH.pid,WH.date,
           SUM(WH.est_clicks) AS actual_clicks,
           SUM(WH.est_impressions) AS actual_impr,
           ( SUM(WH.est_cost)/100.0 )::NUMERIC(20,2) AS actual_spend
           FROM day_trig_campaign_agg WH
           JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
           JOIN user_campaigns uc on (CPH.cid = uc.cid)
           JOIN user_accts ua on (uc.user_acctid = ua.user_acctid)
           WHERE CPH.pid = (%1$s)
           AND ua.sid = 77
           AND WH.date >= '%2$s'
           AND WH.date <= '%3$s' 
           GROUP BY WH.date,CPH.pid 
           ) AS c USING (date,pid)       
FULL JOIN ( SELECT CPH.pid, WH.date,
           SUM( case when (device = 'm' AND mobile_weights_enabled = TRUE)
           then coalesce(f.mobile_weight, f.weight) 
           else f.weight 
           end * (COALESCE(CT_VALUE_LAST, 0) + COALESCE(CTVT_VALUE_LAST, 0) * (1 - 0) + COALESCE(VTCT_VALUE_LAST, 0) * 0 + 0.4 * ( COALESCE (VT_VALUE_LAST, 0)) ) )   as crev,
           
           (SUM( case when (device = 'm' AND mobile_weights_enabled = TRUE)
           then coalesce(f.mobile_weight, f.weight) 
           else f.weight 
           end * ( COALESCE(CT_VALUE_LAST, 0) / COALESCE(D.click_thru_percentage, 1) + COALESCE(CTVT_VALUE_LAST, 0) * (1 - 0) / COALESCE(D.click_thru_percentage, 1) + COALESCE(VTCT_VALUE_LAST, 0) * 0 / COALESCE(D.view_thru_percentage, 1) +  COALESCE (VT_VALUE_LAST, 0) * 0.4 / COALESCE(D.view_thru_percentage, 1)   ) ))::NUMERIC(20,2)   as crev_d
           FROM day_revenue_campaign_agg WH
           JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
           JOIN portfolio_objectives PO ON (CPH.pid = PO.pid)
           JOIN objective_function f ON (f.objid = PO.objid AND f.propertyid = WH.propertyid)
           JOIN objectives ob ON ob.objid = f.objid
           LEFT JOIN (   
           SELECT  pid,
           propertyid,
           (now() at time zone tz)::date - delay AS date,
           CASE
           WHEN click_thru_percentage = 0 
           THEN 1
           WHEN click_thru_percentage < 0.01
           THEN 0.01
           ELSE
           LEAST(click_thru_percentage, 1)
           END AS click_thru_percentage,
           CASE
           WHEN view_thru_percentage = 0 
           THEN 1
           WHEN view_thru_percentage < 0.01
           THEN 0.01
           ELSE
           LEAST(view_thru_percentage, 1)
           END AS view_thru_percentage
           FROM property_daily_delayed_rev_factors
           JOIN user_portfolios using(pid)
           JOIN users using(userid)
           JOIN timezones using(tzid)
           WHERE delay >= 0
           AND pid = (%1$s)
           ) D on (wh.propertyid = D.propertyid and CPH.pid = D.pid and WH.date = D.date)
           WHERE CPH.pid = (%1$s)
           AND WH.date >= '%2$s'
           AND WH.date <= '%3$s' 
           GROUP BY WH.date,CPH.pid ) AS cr USING (date,pid)
JOIN ( SELECT CPH.pid,WH.date,
       SUM( case when (device = 'm' AND mobile_weights_enabled = TRUE) then coalesce(f.mobile_weight, f.weight) else f.weight end * (COALESCE(CT_VALUE_LAST, 0) + (1 - 0) * COALESCE(CTVT_VALUE_LAST, 0) + 
       0.4 * ( COALESCE (VT_VALUE_LAST, 0) + 0 * COALESCE(VTCT_VALUE_LAST, 0) ) ) )   as trev
       FROM tday_revenue_campaign_agg WH
       JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
       JOIN portfolio_objectives PO ON (CPH.pid = PO.pid)
       JOIN objective_function f ON (f.objid = PO.objid AND f.propertyid = WH.propertyid)
       JOIN objectives ob ON ob.objid = f.objid
       WHERE CPH.pid = (%1$s)
       AND WH.date >= '%2$s'
       AND WH.date <= '%3$s' 
       GROUP BY WH.date,CPH.pid ) AS tr USING (date,pid)
ORDER BY 1 DESC,2 ;", pid1,date5,date6)

admin_query_daily_accuracy_agg = sprintf("SELECT p.pid as pid, 
 SUM(predicted_clicks) as pred_clicks, 
 SUM(actual_clicks) as act_clicks,
 (100.0 * SUM(actual_clicks )/ SUM(NULLIF( predicted_clicks, 0 )) )::NUMERIC(20,2) AS click_acc,
 SUM(predicted_spend) as pred_cost,
 SUM(actual_spend) as act_cost,
 SUM(budget)::NUMERIC(20,2) AS budget ,
 (100.0 * SUM(actual_spend) / SUM(NULLIF( predicted_spend, 0 )) )::NUMERIC(20,2) AS cost_acc,
 SUM(predicted_rev) as pred_rev,
 SUM(crev) as act_rev,
 (100.0 * SUM(crev )/ SUM(NULLIF ( predicted_rev, 0 ) ))::NUMERIC(20,2) AS rev_acc,
 SUM(predicted_impr) as pred_impr,
 SUM(actual_impr) as act_impr,
 (100.0 * SUM(actual_impr)/SUM(NULLIF( predicted_impr, 0) ))::NUMERIC(20,2) AS impr_acc,
 (100.0 * SUM((actual_spend/NULLIF( actual_clicks, 0 ))) /SUM(NULLIF((predicted_spend/NULLIF( predicted_clicks, 0 )),0))) ::NUMERIC(20,2) AS cpc_acc,
 (100.0 *SUM(actual_spend )/ SUM(NULLIF( budget, 0 ) ))::NUMERIC(20,2) AS pacing_acc,
 (100.0 * SUM(crev/NULLIF( actual_clicks, 0 )) /SUM(NULLIF((predicted_rev/NULLIF( predicted_clicks, 0 )),0))) ::NUMERIC(20,2) AS rpc_acc 
 FROM ( SELECT (j.job_start_time AT TIME ZONE t.tz + INTERVAL '6 hours')::date as date,j.pid,
       AVG(j.converged_clicks)::NUMERIC(20,2) AS predicted_clicks,
       AVG(j.converged_impr)::NUMERIC(20,2) AS predicted_impr,
       ( AVG(j.converged_cost)/100.0 )::NUMERIC(20,2) AS predicted_spend,
       AVG(j.converged_rev)::NUMERIC(40,4) AS predicted_rev,
       AVG( CASE j.stid WHEN 5 THEN j.st_constraint ELSE j.budget_high END ) AS budget,
       COUNT(1) as opt_runs,
       MAX(j.job_start_time at time zone t.tz) as last_ran,
       ( array_agg(pov.objid ORDER BY j.job_start_time DESC))[1] as objid
       FROM optimizer_jobs j
       JOIN user_portfolios up ON (up.pid = j.pid)
       LEFT JOIN portfolio_objectives_versions pov ON (up.pid = pov.pid and j.job_start_time between pov.xstart_time and pov.xend_time)
       JOIN ( users u JOIN timezones t ON (t.tzid = u.tzid) ) t ON (up.userid = t.userid)
       WHERE up.pid = (%1$s)
       AND j.job_start_time >=  DATE '%2$s' - INTERVAL '2 days'
       AND j.job_end_time <= DATE '%3$s' + INTERVAL '2 days'
       AND (j.job_start_time at time zone t.tz + interval '6 hours')::DATE >= DATE '%2$s'
       AND (j.job_end_time at time zone t.tz + interval '6 hours')::DATE <= DATE '%3$s'
       GROUP BY 1,j.pid ) as p
FULL JOIN ( SELECT CPH.pid,WH.date,
           SUM(WH.clicks) AS actual_clicks,
           SUM(WH.impressions) AS actual_impr,
           ( SUM(WH.cost)/100.0 )::NUMERIC(20,2) AS actual_spend
           FROM day_sem_campaign_agg WH
           JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
           JOIN user_campaigns uc on (CPH.cid = uc.cid)
           JOIN user_accts ua on (uc.user_acctid = ua.user_acctid)
           WHERE CPH.pid = (%1$s)
           AND ua.sid <> 77
           AND WH.date >= '%2$s'
           AND WH.date <= '%3$s' 
           GROUP BY WH.date,CPH.pid 
           
           UNION ALL
           
           SELECT CPH.pid,WH.date,
           SUM(WH.est_clicks) AS actual_clicks,
           SUM(WH.est_impressions) AS actual_impr,
           ( SUM(WH.est_cost)/100.0 )::NUMERIC(20,2) AS actual_spend
           FROM day_trig_campaign_agg WH
           JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
           JOIN user_campaigns uc on (CPH.cid = uc.cid)
           JOIN user_accts ua on (uc.user_acctid = ua.user_acctid)
           WHERE CPH.pid = (%1$s)
           AND ua.sid = 77
           AND WH.date >= '%2$s'
           AND WH.date <= '%3$s'
           GROUP BY WH.date,CPH.pid 
           ) AS c USING (pid,date)       
FULL JOIN ( SELECT CPH.pid, WH.date,
             SUM( case when (device = 'm' AND mobile_weights_enabled = TRUE)
             then coalesce(f.mobile_weight, f.weight) 
             else f.weight 
             end * (COALESCE(CT_VALUE_LAST, 0) + COALESCE(CTVT_VALUE_LAST, 0) * (1 - 0) + COALESCE(VTCT_VALUE_LAST, 0) * 0 + 0.4 * ( COALESCE (VT_VALUE_LAST, 0)) ) )   as crev,
             (SUM( case when (device = 'm' AND mobile_weights_enabled = TRUE)
             then coalesce(f.mobile_weight, f.weight) 
             else f.weight 
             end * ( COALESCE(CT_VALUE_LAST, 0) / COALESCE(D.click_thru_percentage, 1) + COALESCE(CTVT_VALUE_LAST, 0) * (1 - 0) / COALESCE(D.click_thru_percentage, 1) + COALESCE(VTCT_VALUE_LAST, 0) * 0 / COALESCE(D.view_thru_percentage, 1) +  COALESCE (VT_VALUE_LAST, 0) * 0.4 / COALESCE(D.view_thru_percentage, 1)   ) ))::NUMERIC(20,2)   as crev_d
             FROM day_revenue_campaign_agg WH
             JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
             JOIN portfolio_objectives PO ON (CPH.pid = PO.pid)
             JOIN objective_function f ON (f.objid = PO.objid AND f.propertyid = WH.propertyid)
             JOIN objectives ob ON ob.objid = f.objid
             LEFT JOIN (   
             SELECT  pid,
             propertyid,
             (now() at time zone tz)::date - delay AS date,
             CASE
             WHEN click_thru_percentage = 0 
             THEN 1
             WHEN click_thru_percentage < 0.01
             THEN 0.01
             ELSE
             LEAST(click_thru_percentage, 1)
             END AS click_thru_percentage,
             CASE
             WHEN view_thru_percentage = 0 
             THEN 1
             WHEN view_thru_percentage < 0.01
             THEN 0.01
             ELSE
             LEAST(view_thru_percentage, 1)
             END AS view_thru_percentage
             FROM property_daily_delayed_rev_factors
             JOIN user_portfolios using(pid)
             JOIN users using(userid)
             JOIN timezones using(tzid)
             WHERE delay >= 0
             AND pid = (%1$s)
             ) D on (wh.propertyid = D.propertyid and CPH.pid = D.pid and WH.date = D.date)
             WHERE CPH.pid = (%1$s)
             AND WH.date >= '%2$s'
             AND WH.date <= '%3$s' 
             GROUP BY WH.date,CPH.pid ) AS cr USING (pid,date)
             JOIN ( SELECT CPH.pid,WH.date,
             SUM( case when (device = 'm' AND mobile_weights_enabled = TRUE) then coalesce(f.mobile_weight, f.weight) else f.weight end * (COALESCE(CT_VALUE_LAST, 0) + (1 - 0) * COALESCE(CTVT_VALUE_LAST, 0) + 0.4 * ( COALESCE (VT_VALUE_LAST, 0) + 0 * COALESCE(VTCT_VALUE_LAST, 0) ) ) )   as trev
             FROM tday_revenue_campaign_agg WH
             JOIN cid_pid_history CPH ON (WH.cid = CPH.cid AND WH.date = CPH.date)
             JOIN portfolio_objectives PO ON (CPH.pid = PO.pid)
             JOIN objective_function f ON (f.objid = PO.objid AND f.propertyid = WH.propertyid)
             JOIN objectives ob ON ob.objid = f.objid
             WHERE CPH.pid = (%1$s)
             AND WH.date >= '%2$s'
             AND WH.date <= '%3$s' 
             GROUP BY WH.date,CPH.pid ) AS tr USING (pid,date)
GROUP BY 1
ORDER BY 1 DESC,2 ;", pid1,date1,date2)

                                         
#,date1,date2,active_pid_or1,date1,date2,active_pid_or1,date1,date2,active_pid_or1,active_pid_or1,date1,date2,active_pid_or1,date1,date2)
#                                          
#Daily Accuracy
admin_daily = amo_db_get(query=admin_query_daily_accuracy, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)
admin_daily_2 = amo_db_get(query=admin_query_daily_accuracy_1, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)
admin_daily_3 = amo_db_get(query=admin_query_daily_accuracy_2, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)
# admin_daily_v21 <- merge(admin_pidd, admin_daily_2, by.x = "pid",by.y = "pid", all.x = TRUE)
# admin_daily_v212 <- admin_daily_v21[admin_daily_v21$portfolio_name == "Grainger PLA",]
# data <- admin_daily_v212[order(admin_daily_v212$date, decreasing = TRUE),]


admin_daily_v11 <- admin_daily_2 %>%select(pid,date,cost_acc,rpc_acc)
admin_daily_v11 <- merge(admin_pidd, admin_daily_v11, by.x = "pid",by.y = "pid", all.x = TRUE)

admin_daily_v31 <- admin_daily_3 %>%select(pid,date,cost_acc,rpc_acc)
admin_daily_v31 <- merge(admin_pidd, admin_daily_v31, by.x = "pid",by.y = "pid", all.x = TRUE)

admin_daily_v32 <- admin_daily_3 %>%select(pid,date,click_acc,cpc_acc)
admin_daily_v32 <- merge(admin_pidd, admin_daily_v32, by.x = "pid",by.y = "pid", all.x = TRUE)

admin_daily_v3 <- admin_daily_2 %>%select(pid,date,click_acc,cpc_acc)
admin_daily_v3 <- merge(admin_pidd, admin_daily_v3, by.x = "pid",by.y = "pid", all.x = TRUE)

admin_daily_v12 <- admin_daily_2 %>%select(pid,date,act_cost, act_rev, pred_cost, pred_rev)
admin_daily_v12 <- merge(admin_pidd, admin_daily_v12, by.x = "pid",by.y = "pid", all.x = TRUE)
# 
# admin_daily_v12[is.na(admin_daily_v12)] <- 0
# admin_daily_v12 <- admin_daily_v12[admin_daily_v12$portfolio_name == "Grainger PLA",]
# 
# library(lubridate)
# admin_daily_v12$date <- as.Date(admin_daily_v12$date)
# admin_daily_v12$dow <- wday(admin_daily_v12$date, label=TRUE)
# #
# admin_daily_v13 <- admin_daily_v12 %>% select(portfolio_name, dow, act_cost, act_rev, pred_cost, pred_rev)
# 
# 
# # p <-ggplot(admin_daily_v14, aes(x=dow, y=roas_acc)) +geom_bar(stat = "identity")
# # print(p)
# #
# # admin_daily_v13 <- admin_daily_v12 %>% select(portfolio_name, dow, roas_acc)
# 
# admin_daily_v14 <- aggregate(.~portfolio_name+dow,admin_daily_v13,sum)
# 
# data <- admin_daily_v14 %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
# data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
# data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
# 
# admin_daily_v14 <- admin_daily_v14 %>% mutate(act_roas=act_rev/act_cost)
# admin_daily_v14 <- admin_daily_v14 %>% mutate(pred_roas=pred_rev/pred_cost)
# admin_daily_v14 <- admin_daily_v14 %>% mutate(roas_acc=act_roas/pred_roas*100)

# admin_daily_v15 <- admin_daily_v14 %>% select(portfolio_name,act_roas,dow)
# admin_v6 <- melt(admin_daily_v15)
# 
# ggplot(admin_v6, aes(x=dow, y=value, fill=variable)) +
#   geom_bar(stat='identity', position='dodge2')+ facet_grid(. ~ portfolio_name)
# 
# admin_daily_v14$Variance_di <- var(admin_daily_v14$act_roas)
# admin_daily_v14$avg_di <- mean(admin_daily_v14$act_roas)
# admin_daily_v14$dispersion_index <- admin_daily_v14$Variance_di/admin_daily_v14$avg_di
# 
# admin_daily_v14$Variance_er <- var(admin_daily_v14$roas_acc)
# admin_daily_v14$avg_er <- mean(admin_daily_v14$roas_acc)
# admin_daily_v14$error_rate <- admin_daily_v14$Variance_er/admin_daily_v14$avg_er
# 
# di_dow <- unique(admin_daily_v14$dispersion_index)
# er_dow <- unique(admin_daily_v14$error_rate)
# 
# admin_final <- admin_final[admin_final$portfolio_name == "Grainger PLA RLSA",]
# 
# admin_final$di <- di
# admin_final$er <- er
# #
#  paste("click coverage:", admin_final$di)
# # #
# admin_final <- admin_final %>% mutate(reco_dow = case_when(di > 0.2 & DOWfeature == "Not_Enabled" & Spend_Scenario == "Overspend"  ~ "Enable_DOW",
#                                                            di > 0.2 & DOWfeature == "Not_Enabled" & Spend_Scenario == "Underspend"~ "Enable_DOW",
#                                                            di > 0.2 & DOWfeature == "Not_Enabled" & RPC_Scenario =="Poor_RPC" ~ "Enable_DOW",
#                                                            Spend_Scenario =="Poor_RPC" & di < 0.2 & DOWfeature == "Enabled" ~ "Disable_DOW",
#                                                            Spend_Scenario =="Overspend" & di < 0.2 & DOWfeature == "Enabled" ~ "Disable_DOW",
#                                                            Spend_Scenario =="Underspend" & di < 0.2 & DOWfeature == "Enabled" ~ "Disable_DOW",
#                                                            Spend_Scenario == "Overspend" & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 & click_coverage > 0.1  ~ "Further_investigate",
#                                                            Spend_Scenario == "Overspend" & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 & click_coverage < 0.1 ~ "Further_investigate",
#                                                            Spend_Scenario == "Overspend" & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 & revenue_coverage < 0.05 ~ "Further_investigate",
#                                                            Spend_Scenario == "Overspend" & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 & revenue_coverage >0.05 ~ "Further_investigate",
#                                                            Spend_Scenario == "Underspend"  & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 & revenue_coverage > 0.05 ~ "Further_investigate",
#                                                            Spend_Scenario == "Underspend"  & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 &  revenue_coverage < 0.05 ~ "Further_investigate",
#                                                            Spend_Scenario == "Underspend"  & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 &  click_coverage < 0.1 ~ "Further_investigate",
#                                                            Spend_Scenario == "Underspend"  & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 &  click_coverage > 0.1 ~ "Further_investigate",
#                                                            RPC_Scenario == "Poor_RPC"  & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 & revenue_coverage > 0.05 ~ "Further_investigate",
#                                                            RPC_Scenario == "Poor_RPC"  & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 &  revenue_coverage < 0.05 ~ "Further_investigate",
#                                                            RPC_Scenario == "Poor_RPC"  & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 &  click_coverage < 0.1 ~ "Further_investigate",
#                                                            RPC_Scenario == "Poor_RPC"  & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 &  click_coverage > 0.1 ~ "Further_investigate",
#                                                            TRUE ~ DOWfeature))

# admin_final$DOWfeature[1] <- "Not_Enabled"
# 
# admin_daily_v12 <- merge(admin_pidd, admin_daily_v11, by.x = "pid",by.y = "pid", all.x = TRUE)

# unique(admin_daily_v11$portfolio_name)
# # #
# data_1 <- admin_daily_v11[admin_daily_v11$portfolio_name == "Grainger PLA",]
# data_1 <- admin_daily_v11 %>% select(portfolio_name, date, cost_acc, rpc_acc)
# 
# y_data <- data_1 %>% select(date,cost_acc, rpc_acc)
# 
# y_data <- y_data %>%arrange(desc(date))
# 
# y_data <- tail(y_data, -7)
# 
# 
# y_data[is.na(y_data)] <- 0
# 
# 
# 
# library(changepoint)
# 
# cptm_CP         <- cpt.mean(y_data$cost_acc, penalty='MBIC',pen.value=0, method='BinSeg',
#                            test.stat="Normal", minseglen=7, class=TRUE)
# cptm_CP
# 
# plot(cptm_CP)
# abline(v=y_data[max(cpts_CP),1], col="blue")
# 
# cpts_CP         <- cpts(cptm_CP) # change point time points
# cpts_CP
# 
# cost_change <-  ifelse(max(cpts_CP)==0,0,(Sys.Date()-(as.Date(y_data[max(cpts_CP),1]))))
# 
#   c<-  ifelse(max(cpts_CP)==0,0,(Sys.Date()-(as.Date(y_data[max(cpts_CP),1]))))
# 
# library(changepoint)
# 
# cptm_CP_1         <- cpt.mean(y_data$rpc_acc, penalty='MBIC',pen.value=0, method='BinSeg',
#                             test.stat="Normal", minseglen=7, class=TRUE)
# cptm_CP_1
# 
# plot(cptm_CP_1)
# 
# cpts_CP_1         <- cpts(cptm_CP_1) # change point time points
# cpts_CP_1
# 
# rpc_change <-  ifelse(max(cpts_CP_1)==0,0,(Sys.Date()-(as.Date(y_data[max(cpts_CP_1),1]))))
# 
# 
# print( paste("latest change in mean on", y_data[max(cpts_CP_1),1] , "that is", Sys.Date()-(as.Date(y_data[max(cpts_CP_1),1])), "days ago." ))
# print( paste("latest change in mean on", y_data[max(cpts_CP),1] , "that is", Sys.Date()-(as.Date(y_data[max(cpts_CP),1])), "days ago." ))
# 
# #
# library("outliers")
# y_data$Cost_Acc_score <- scores(type="z", y_data$cost_acc, prob=.95)
# y_data$RPC_Acc_score <- scores(type="z", y_data$rpc_acc, prob=.95)
# cost_outliers <- list(subset(y_data, Cost_Acc_score==TRUE)[,1])
# cost_outliers
# revenue_outliers <- list(subset(y_data, RPC_Acc_score==TRUE)[,1])
# revenue_outliers
# d1 <- as.data.frame(count(unlist(revenue_outliers)))
# 
# sf <- sum(d1$freq)
# 
# count(revenue_outliers)
# 
# y_data_1 <- subset(y_data, RPC_Acc_score==TRUE)
# y_data_2 <- subset(y_data, Cost_Acc_score==TRUE)
# 
# # #ggplot(data_1, aes(x=date, y=value, color = variable, group=1)) +geom_line() + facet_grid(. ~ portfolio_name)
# #
# p= ggplot() +
#   geom_line(data =data_1, aes(x = as.Date(date), y = cost_acc, group=1, color="red")) +
#   geom_line(data = data_1, aes(x = as.Date(date), y = rpc_acc, group=1, color="darkcyan")) +
#   geom_point(data = y_data_1, aes(x = as.Date(date), y = rpc_acc, group=1, color="darkcyan", shape="circle", size=3))+
#   geom_point(data = y_data_2, aes(x = as.Date(date), y = cost_acc, group=1, color="red", shape = "triangle", size=3))+
#   xlab('Date') +
#   ylab('Value') + facet_grid(. ~ portfolio_name)+
#   scale_x_date(date_breaks = "1 month")+
#   geom_vline(xintercept=as.Date(y_data[max(cpts_CP),1]), linetype=4, color = "red", show.legend = T)+
#   geom_text(data=y_data, mapping=aes(x=as.Date(y_data[max(cpts_CP),1]), y=0, label=as.Date(y_data[max(cpts_CP),1] , sep = " ")), size=3, vjust=1, hjust=0, angle=90)+
#   geom_vline(xintercept=as.Date(y_data[max(cpts_CP_1),1]), linetype=4, color = "darkcyan", show.legend = T)+
#   geom_text(data=y_data, mapping=aes(x=as.Date(y_data[max(cpts_CP_1),1]), y=0, label=as.Date(y_data[max(cpts_CP_1),1] , sep = " ")), size=3, vjust=1, hjust=0, angle=90)+
#   scale_color_manual(name = "Value", labels = c("darkcyan"="cost_acc", "red"="rpc_acc"), values = c("darkcyan", "red"))
# 
# p= ggplot() +
#   geom_line(data =data_1, aes(x = as.Date(date), y = cost_acc, group=1, color="red")) +
#   geom_point(data = y_data_2, aes(x = as.Date(date), y = cost_acc, group=1, color="red", shape = "triangle", size=3), show.legend = F)+
#   xlab('Date') +
#   ylab('Value') + facet_grid(. ~ portfolio_name)+
#   scale_x_date(date_breaks = "1 month")+
#   geom_vline(xintercept=as.Date(y_data[max(cpts_CP),1]), linetype=4, color = "red", show.legend = T)+
#   geom_text(data=y_data, mapping=aes(x=as.Date(y_data[max(cpts_CP),1]), y=0, label=as.Date(y_data[max(cpts_CP),1] , sep = " ")), size=3, vjust=1, hjust=0, angle=90)+
#   scale_color_manual(name = "Value", labels = c("red"="cost_acc"), values = c("red"))
# 
# print(p)
# 
# # 
# p= ggplot() +
#   geom_line(data = data_1, aes(x = as.Date(date), y = rpc_acc, group=1), color="darkcyan" , show.legend = F) +
#   geom_point(data = y_data_1, aes(x = as.Date(date), y = rpc_acc, group=1, shape="circle"),size=5, color="darkcyan", show.legend = T)+
#   xlab('Date') +
#   ylab('rpc_acc') + facet_grid(. ~ portfolio_name)+
#   scale_x_date(date_breaks = "1 week")+
#   geom_vline(data=y_data, mapping = aes(xintercept=as.Date(y_data[max(cpts_CP),1]), linetype="twodash"),size=1, color = "darkcyan", show.legend =T)+
#   geom_hline(data=y_data, mapping = aes(yintercept=100, linetype="dotted"),size=1, color = "darkcyan", show.legend =T)+
#   geom_text(data=y_data, mapping=aes(x=as.Date(y_data[max(cpts_CP_1),1]), y=0, label=as.Date(y_data[max(cpts_CP_1),1] , sep = " ")), size=3, vjust=1, hjust=0, angle=90)+
#   scale_shape_manual(name =  " Legend-Shape", labels = c("circle"="outliers"), values = c("circle"))+
#   scale_linetype_manual(name =  "Legend-Line ", labels = c("dotted"="100 %","twodash"="change_mean"), values = c("dotted","twodash"))+
#   guides(shape = guide_legend("Legend-Shape",override.aes = list(linetype = 0, color="darkcyan")),
#          linetype = guide_legend("Legend-Line",override.aes = list(shape = 0, color="darkcyan")))
# p
# 
# 
# p= ggplot() +
#   geom_line(data =data_1, aes(x = as.Date(date), y = cost_acc, group=1, color="red"), show.legend = F) +
#   geom_point(data = y_data_2, aes(x = as.Date(date), y = cost_acc, group=1, shape = "circle", color="red"), size=5, show.legend = T)+
#   xlab('Date') +
#   ylab('cost_acc') + facet_grid(. ~ portfolio_name)+
#   scale_x_date(date_breaks = "1 month")+
#   geom_vline(data=y_data, mapping = aes(xintercept=as.Date(y_data[max(cpts_CP),1]), linetype="twodash", color = "red"),size=1, show.legend =T)+
#   geom_text(data=y_data, mapping=aes(x=as.Date(y_data[max(cpts_CP),1]), y=0, label=as.Date(y_data[max(cpts_CP),1] , sep = " ")), size=3, vjust=1, hjust=0, angle=90)+
#   scale_shape_manual(name =  " Legend-Shape", labels = c("circle"="outliers"), values = c("circle"))+
#   scale_linetype_manual(name =  "Legend-Line ", labels = c("twodash"="change_mean"), values = c("twodash"))+
#   scale_color_discrete(guide = FALSE)+
#   guides(shape = guide_legend("Legend-Shape",override.aes = list(linetype = 0)), 
#          linetype = guide_legend("Legend-Line",override.aes = list(shape = 0)))


admin_daily_1 = amo_db_get(query=admin_query_daily_accuracy_agg, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)

# admin_daily_agg1 = amo_db_get(query=admin_query_daily_accuracy_agg_1, db_tag = dbtag, dc = dc_choose, debug = TRUE)
# admin_daily_agg_v21 <- merge(admin_pidd, admin_daily_agg1, by.x = "pid",by.y = "pid", all.x = TRUE)
# admin_daily_agg_v212 <- admin_daily_agg_v21[admin_daily_agg_v21$portfolio_name == "Grainger PLA",]
# data <- admin_daily_agg_v212[order(admin_daily_agg_v212$date, decreasing = TRUE),]

admin_final <- merge(admin_final, admin_daily_1, by.x="pid", by.y = "pid", all.x = TRUE)

# data_1 <- admin_final[admin_final$client_account_name == "grainger",]
# data <- data_1 %>% select(portfolio_name, pred_cost,cost_acc, rpc_acc, Scenario)
# data[is.na(data)] <- 0
# data <- data[order(data$pred_cost, decreasing = TRUE),]
# # 
# library("formattable")
# improvement_formatter <-
#   formatter("span",
#             style = x ~ style(
#               font.weight = "bold",
#               color = ifelse(x > 120, "Green", ifelse(x < 80, "Red", "black"))),
#             x ~ icontext(ifelse(x > 120, "arrow-up", ifelse(x < 80, "arrow-down", " ")), x))
# 
# formattable(data, list(
#   'cost_acc' = improvement_formatter
# ))
# 
# as.datatable(formattable(data, list(
#   'cost_acc' = improvement_formatter
# )))

# data$cost_acc <- paste(round(data$cost_acc,digits=1),"%",sep="")
# data$rpc_acc <- paste(round(data$rpc_acc,digits=1),"%",sep="")
# data$pred_cost <- format(data$pred_cost,big.mark=",", trim=TRUE,scientific=FALSE)
# data$pred_cost <- as.numeric(unlist(regmatches(data$pred_cost,
#                              gregexpr("[[:digit:]]+\\.*[[:digit:]]*",data$pred_cost))
# )      )
# data <- data[order(data$pred_cost, decreasing = TRUE),]
# range(data$pred_cost)
# View(data)

admin_final[is.na(admin_final)] <- 0

#SCENARIOS
admin_final <- admin_final %>% 
  mutate(Scenario = case_when(cost_acc > 0 & cost_acc < 80 & rpc_acc >=80 & rpc_acc<=120 ~ "Underspend AND RPC Accuracy OK",
                              cost_acc > 0 & cost_acc < 80 & rpc_acc <80 | rpc_acc >120 ~ "Underspend AND Poor RPC Accuracy ",
                              cost_acc > 120 & rpc_acc >=80 & rpc_acc<=120 ~ "Overspend AND RPC Accuracy OK",
                              cost_acc > 120 & rpc_acc > 0 &  rpc_acc < 80 | rpc_acc > 120  ~ "Overspend AND Poor RPC Accuracy",
                              cost_acc >= 80 & cost_acc <=120 & rpc_acc >=80 & rpc_acc<=120 ~ "Cost Accuracy Within Range AND RPC Accuracy OK",
                              cost_acc >= 80 & cost_acc <=120 & rpc_acc > 0 & rpc_acc < 80 | rpc_acc > 120  ~ "Cost Accuracy Within Range AND Poor RPC Accuracy",
                              cost_acc == 0 & rpc_acc == 0 ~ "No Data"
  ))


  
  # admin_query_spend_multiple=sprintf("select pid, spend_limit_multiple as Campaign_spend_multiple from portfolio_campaign_spend_limit where pid = (%s) order by 1", pid1)
  admin_query_spend_multiple=sprintf("select pid, spend_limit_multiple as Campaign_spend_multiple from portfolio_campaign_spend_limit")
  admin_opt_sm = amo_db_get(query=admin_query_spend_multiple, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)
  
  admin_opt_sm[is.na(admin_opt_sm)] <- 0
  
  admin_final <- merge(admin_final, admin_opt_sm, by.x="pid", by.y = "pid", all.x = TRUE)
  
  admin_query_cost_rev_KW = sprintf(" select pid, active_ads as Active_KW_count, max_unconstrained_revenue, max_constrained_revenue,
                                    (max_unconstrained_revenue-max_constrained_revenue) as inc_unconstrained_revenue,
                                    max_constrained_spend, max_unconstrained_spend,
                                    (max_unconstrained_spend-max_constrained_spend) as inc_unconstrained_cost,
                                    constrained_kwds_count from user_portfolio_stats where pid = (%s) order by 1;", pid1)
  
  admin_cost_rev_KW = amo_db_get(query=admin_query_cost_rev_KW, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)
  
  admin_cost_rev_KW[is.na(admin_cost_rev_KW)] <- 0
  
  admin_cost_rev_KW$unconstrained_rev_share_percent <- admin_cost_rev_KW$inc_unconstrained_revenue/admin_cost_rev_KW$max_unconstrained_revenue*100
  
  admin_final <- merge(admin_final, admin_cost_rev_KW, by.x="pid", by.y = "pid", all.x = TRUE)
  
  admin_final[is.na(admin_final)] <- 0
  
  admin_final <- admin_final[order(admin_final$pred_cost, decreasing = TRUE),]
  
  admin_final <- admin_final %>% mutate(Spend_Scenario = case_when(cost_acc > 0 & cost_acc < 80 ~ "Underspend",
                                                                   cost_acc > 120 ~ "Overspend",
                                                                   cost_acc >= 80 & cost_acc <=120 ~ "Cost Accuracy Within Range",
                                                                   cost_acc == 0 ~ "No Data"))
  
  admin_final <- admin_final %>% mutate(RPC_Scenario = case_when(rpc_acc > 120 | rpc_acc < 80 ~ "Poor RPC Accuracy",
                                                                 rpc_acc >= 80 & rpc_acc <=120 ~ "RPC Accuracy OK",
                                                                 rpc_acc == 0 ~ "No Data"))
  
  admin_final <- admin_final %>% mutate(reco_max_bid = case_when(Spend_Scenario!= "Overspend" & unconstrained_rev_share_percent > 10 ~ "remove constraints",
                                                                 TRUE ~ as.character(unconstrained_rev_share_percent)))

  
  admin_final$reco_sm <-  ifelse(admin_final$Spend_Scenario == "Overspend" & admin_final$campaign_spend_multiple > 1.2, ifelse(1.2 > admin_final$campaign_spend_multiple/2, 1.2, admin_final$campaign_spend_multiple/2), 
                            ifelse(admin_final$Spend_Scenario == "Underspend" & admin_final$campaign_spend_multiple < 2, ifelse(2 < admin_final$campaign_spend_multiple*3/2, 2, admin_final$campaign_spend_multiple*3/2), admin_final$campaign_spend_multiple))
  
  admin_query_opt_args_intraday = "select up.pid,param_name as param_name_intraday,coalesce(oa.param_value, mp.default_value) as param_value_intraday
                          from user_portfolios up 
  join  models m on (True)
  join model_parameters mp using (modelid)
  left join (select pid, paramid, param_value
  from  model_parameters 
  join optimizer_arguments_portfolio_level op using(paramid)
  
  ) oa on (oa.pid = up.pid and oa.paramid = mp.paramid)
  
  where m.model_type = 'o' and param_name= 'enable_intraday'
  and up.pid in (select pid from user_portfolios )
  and (not default_value isnull or
  not oa.param_value isnull);"
  
  admin_query_opt_args_learning_budget = "select up.pid,param_name as param_name_lb,coalesce(oa.param_value, mp.default_value) as param_value_LB
  from user_portfolios up 
  join  models m on (True)
  join model_parameters mp using (modelid)
  left join (select pid, paramid, param_value
  from  model_parameters 
  join optimizer_arguments_portfolio_level op using(paramid)
  
  ) oa on (oa.pid = up.pid and oa.paramid = mp.paramid)
  
  where m.model_type = 'o' and param_name= 'profile_fract'
  and up.pid in (select pid from user_portfolios )
  and (not default_value isnull or
  not oa.param_value isnull);"
  
  admin_query_opt_args_ZIL = "select up.pid,param_name as param_name_ZIL,coalesce(oa.param_value, mp.default_value) as param_value_ZIL
  from user_portfolios up 
  join  models m on (True)
  join model_parameters mp using (modelid)
  left join (select pid, paramid, param_value
  from  model_parameters 
  join optimizer_arguments_portfolio_level op using(paramid)
  
  ) oa on (oa.pid = up.pid and oa.paramid = mp.paramid)
  
  where m.model_type = 'o' and param_name= 'activate_learning_zero_impr'
  and up.pid in (select pid from user_portfolios )
  and (not default_value isnull or
  not oa.param_value isnull);"
  
  
  admin_opt_opt_args_intraday = amo_db_get(query=admin_query_opt_args_intraday, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)
  admin_opt_opt_args_learning_budget = amo_db_get(query=admin_query_opt_args_learning_budget, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)
  admin_opt_opt_args_ZIL = amo_db_get(query=admin_query_opt_args_ZIL, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)
  
  admin_final <- merge(admin_final,admin_opt_opt_args_intraday,by.x = "pid",by.y = "pid",all.x = TRUE)
  admin_final <- merge(admin_final,admin_opt_opt_args_learning_budget,by.x = "pid",by.y = "pid",all.x = TRUE)
  admin_final <- merge(admin_final,admin_opt_opt_args_ZIL,by.x = "pid",by.y = "pid",all.x = TRUE)
  
  #interested columns: Cost HL, Revenue HL
  admin_query_hl = sprintf(" select pid as PID, cost_model_half_life, revenue_model_half_life from portfolio_model_arguments  where pid = (%s) order by 1;", pid1)
  admin_opt_hl = amo_db_get(query=admin_query_hl, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)
  
  admin_final <- merge(admin_final, admin_opt_hl, by.x="pid", by.y = "pid",all.x = TRUE)
  
  admin_query_model_coverage = sprintf("select portfolio_id as portfolio, pid,
                                     cast(revenue_model_stats.ctime as date) as date_most_recent,
                                       ads as bid_units,
                                       cost_modeled_ads as click_models,
                                       ads_w_revenue as revenue_models,
                                       round(cost_modeled_ads / cast(ads as numeric), 3) as click_coverage, 
                                       round(ads_w_revenue / cast(ads as numeric), 3) as revenue_coverage 
                                       from revenue_model_stats 
                                       join (select	pid,
                                       max(ctime) as ctime 
                                       from revenue_model_stats 
                                       group by pid) temp_pid_maxctime using (pid, ctime) 
                                       join alg_user_portfolio_stats using (pid) 
                                       join user_portfolios using (pid) 
                                       where status_code in ('z', 'a')
                                        and (cast(ads as numeric) >0)
                                       and pid = (%s)
                                       order by pid;", pid1)
  
  admin_model_coverage = amo_db_get(query=admin_query_model_coverage, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)
  
  require(dplyr)
  admin_model_coverage <- admin_model_coverage %>%select(pid,click_coverage,revenue_coverage)
  admin_final <- merge(admin_final, admin_model_coverage, by.x="pid", by.y = "pid", all.x = TRUE)
  
  admin_final[is.na(admin_final)] <- 0
  

  # df1 <- admin_final[admin_final$portfolio_name == "Grainger SEM",]
  # df1$cchange <- cost_change
  # df1$rchange <- rpc_change

  # df1$click_coverage[1]= 0.5
  # df1$cost_model_half_life[1]= 11
  # df1$cost_acc[1]= 75
  # df1$cchange[1]= 11

  # df1 <- df1 %>% mutate(reco_cost_HL = case_when(Spend_Scenario!= "Within_Range" & cchange > 0 & click_coverage < 0.1 & max(cost_model_half_life,cchange) < 14 & max(cost_model_half_life,cchange) > 3 ~ max(cost_model_half_life, cchange),
  #                                                Spend_Scenario!= "Within_Range" & cchange > 0 & click_coverage < 0.1 & max(cost_model_half_life,cchange) >14 ~ 14,
  #                                                Spend_Scenario!= "Within_Range" & cchange > 0 & click_coverage < 0.1 & max(cost_model_half_life,cchange) < 3 ~ 3,
  #                                                Spend_Scenario!= "Within_Range" & cchange > 0 & click_coverage > 0.1 & min((cost_model_half_life-3)/2, 3) < 3 ~ 3,
  #                                                Spend_Scenario!= "Within_Range" & cchange > 0 & click_coverage > 0.1 & ((cost_model_half_life-3)/2) > 3 ~ (as.numeric(min((cost_model_half_life-3)/2, 3) )),
  #                       TRUE ~ as.numeric(cost_model_half_life)))
  # 
  # df1 <- df1 %>% mutate(reco_rev_HL = case_when(RPC_Scenario!= "RPC_OK" & rchange > 0 & revenue_coverage < 0.05 & max(revenue_model_half_life,rchange) < 60 & max(revenue_model_half_life,rchange) > 10 ~ max(revenue_model_half_life, rchange),
  #                                               RPC_Scenario!= "RPC_OK" & rchange > 0 & revenue_coverage < 0.05 & max(revenue_model_half_life,rchange) >60 ~ 60,
  #                                               RPC_Scenario!= "RPC_OK" & rchange > 0 & revenue_coverage < 0.05 & max(revenue_model_half_life,rchange) < 10 ~ 10,
  #                                               RPC_Scenario!= "RPC_OK" & rchange > 0 & revenue_coverage > 0.05 & min((revenue_model_half_life-10)/2, 10) < 10 ~ 10,
  #                                               RPC_Scenario!= "RPC_OK" & rchange > 0 & revenue_coverage > 0.05 & ((revenue_model_half_life-3)/2) > 10 ~ (as.numeric(min((revenue_model_half_life-10)/2, 10) )),
  #                                               TRUE ~ revenue_model_half_life))

  admin_final <- admin_final %>%
    mutate(reco_ZILstatus = case_when(Spend_Scenario == "Overspend" & param_value_zil == "True" ~ "False",
                                      Spend_Scenario == "Underspend" & param_value_zil == "False" ~ "True",
                                      TRUE ~ param_value_zil))
  
  admin_final <- admin_final[order(admin_final$pred_cost, decreasing = TRUE),]
  
  admin_final$param_value_lb <- as.double(admin_final$param_value_lb)
  
  admin_final <- admin_final %>%
    mutate(reco_LearningBudget = case_when(Spend_Scenario == "Overspend" & param_value_lb > 0 ~ 0.0,
                                           TRUE ~ as.double(param_value_lb)
                                         ##  Spend_Scenario == "Underspend" & param_value_lb < 0.2 ~ 0.2,
                                           )
                                            )

    
  admin_final <- admin_final %>%
    mutate(reco_Intraday = case_when(Spend_Scenario != "Cost Accuracy Within Range" & param_value_intraday == "False" ~ "True",
                                           TRUE ~ as.character(param_value_intraday))
    )
  
  admin_query_spend_strategy = sprintf("select a.pid as pid, p.budget_steer_period as strategy
                                        from  budget_steer_periods p
                                       full join budget_steer_args a
                                       on p.budget_steer_periodid = a.budget_steer_periodid 
                                       where a.pid = (%s);", pid1)
  
  admin_spend_strategy = amo_db_get(query=admin_query_spend_strategy, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)
  
  admin_final <- merge(admin_final, admin_spend_strategy, by.x="pid", by.y = "pid", all.x = TRUE)
  
  admin_query_opt_args_cdow = "select up.pid,param_name as param_name_cdow,coalesce(oa.param_value, mp.default_value) as param_value_cdow
                          from user_portfolios up 
  join  models m on (True)
  join model_parameters mp using (modelid)
  left join (select pid, paramid, param_value
  from  model_parameters 
  join optimizer_arguments_portfolio_level op using(paramid)
  
  ) oa on (oa.pid = up.pid and oa.paramid = mp.paramid)
  
  where m.model_type = 'o' and param_name= 'enable_click_dow'
  and up.pid in (select pid from user_portfolios )
  and (not default_value isnull or
  not oa.param_value isnull);"
  
  
  admin_cdow = amo_db_get(query=admin_query_opt_args_cdow, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)
  
  admin_final <- merge(admin_final,admin_cdow,by.x = "pid",by.y = "pid",all.x = TRUE)
  
  # #VALIDATION
  # 
  # View(admin_final)
  # 
  # #campaign_spend_multiple
  # 
  # admin_final$Spend_Scenario <- "Overspend"
  # admin_final$campaign_spend_multiple <- 5
  # admin_final$param_value_lb <- 5
  # admin_final$param_value_zil <- "True"
  # admin_final$param_value_intraday <- "False"
  # admin_final$reco_sm <-  ifelse(admin_final$Spend_Scenario == "Overspend" & admin_final$campaign_spend_multiple > 1.2, ifelse(1.2 > admin_final$campaign_spend_multiple/2, 1.2, admin_final$campaign_spend_multiple/2), 
  #                                ifelse(admin_final$Spend_Scenario == "Underspend" & admin_final$campaign_spend_multiple < 2, ifelse(2 < admin_final$campaign_spend_multiple*3/2, 2, admin_final$campaign_spend_multiple*3/2), admin_final$campaign_spend_multiple))
  # admin_final <- admin_final %>%
  #   mutate(reco_ZILstatus = case_when(Spend_Scenario == "Overspend" & param_value_zil == "True" ~ "False",
  #                                     Spend_Scenario == "Underspend" & param_value_zil == "False" ~ "True",
  #                                     TRUE ~ param_value_zil))
  # 
  # admin_final <- admin_final %>%
  #   mutate(reco_LearningBudget = case_when(Spend_Scenario == "Overspend" & param_value_lb > 0 ~ 0,
  #                                          Spend_Scenario == "Underspend" & param_value_lb < 0.2 ~ 0.2,
  #                                          TRUE ~ as.double(param_value_lb)))
  # admin_final <- admin_final %>%
  #   mutate(reco_Intraday = case_when(Spend_Scenario != "Cost Accuracy Within Range" & param_value_intraday == "False" ~ "True",
  #                                    TRUE ~ as.character(param_value_intraday))
  #   )
  # 
  # admin_final$unconstrained_rev_share_percent 
  # 
  # 
  # 
  # admin_final$Spend_Scenario <- "Underspend"
  # admin_final$campaign_spend_multiple <- 1.7
  # admin_final$param_value_lb <- 0.1
  # admin_final$param_value_zil <- "False"
  # admin_final$param_value_intraday <- "False"
  # admin_final$reco_sm <-  ifelse(admin_final$Spend_Scenario == "Overspend" & admin_final$campaign_spend_multiple > 1.2, ifelse(1.2 > admin_final$campaign_spend_multiple/2, 1.2, admin_final$campaign_spend_multiple/2), 
  #                                ifelse(admin_final$Spend_Scenario == "Underspend" & admin_final$campaign_spend_multiple < 2, ifelse(2 < admin_final$campaign_spend_multiple*3/2, 2, admin_final$campaign_spend_multiple*3/2), admin_final$campaign_spend_multiple))
  # 
  # admin_final <- admin_final %>%
  #   mutate(reco_ZILstatus = case_when(Spend_Scenario == "Overspend" & param_value_zil == "True" ~ "False",
  #                                     Spend_Scenario == "Underspend" & param_value_zil == "False" ~ "True",
  #                                     TRUE ~ param_value_zil))
  # admin_final <- admin_final %>%
  #   mutate(reco_LearningBudget = case_when(Spend_Scenario == "Overspend" & param_value_lb > 0 ~ 0,
  #                                          Spend_Scenario == "Underspend" & param_value_lb < 0.2 ~ 0.2,
  #                                          TRUE ~ as.double(param_value_lb)))
  # admin_final <- admin_final %>%
  #   mutate(reco_Intraday = case_when(Spend_Scenario != "Cost Accuracy Within Range" & param_value_intraday == "False" ~ "True",
  #                                    TRUE ~ as.character(param_value_intraday))
  #   )
  # 
  # 
  
  
  #Match Type
  admin_query_MT_accuracy_2 = 
    sprintf("SELECT c.pid,
            match_type_display as match_type,  
            sum(e.oclicks)::NUMERIC(20,2) AS pred_clicks,
            sum(c.clicks) AS act_clicks,
            (100.0 * SUM(c.clicks) / NULLIF( SUM(e.oclicks), 0 ) )::NUMERIC(20,2) AS click_acc,
            sum(e.orev)::NUMERIC(20,4) AS pred_rev,
            sum(r.rev)::NUMERIC(20,4) AS act_rev,
            sum( COALESCE((e.orev), 0) - COALESCE((r.rev), 0) )::NUMERIC(20,4) AS rev_diff,
            (100.0 * SUM(r.rev) / NULLIF ( SUM(e.orev), 0 ) )::NUMERIC(20,2) AS rev_acc,
            SUM(e.ocost)::NUMERIC(20,2) AS pred_cost,
            SUM(c.spend) AS act_cost,
            ( COALESCE(SUM(e.ocost), 0) - COALESCE(SUM(c.spend), 0) )::NUMERIC(20,2) AS cost_diff,
            (100.0 * SUM(c.spend)/ NULLIF( SUM(e.ocost), 0 ) )::NUMERIC(20,2) AS cost_acc,
            (100.0 * SUM((c.spend/NULLIF( c.clicks, 0 )) /NULLIF((e.ocost/NULLIF( e.oclicks, 0 )),0)) )::NUMERIC(20,2) AS CPC_acc,
            (100.0 * SUM((r.rev/NULLIF(c.clicks, 0 )) /NULLIF((e.orev/NULLIF( e.oclicks, 0 )),0))) ::NUMERIC(20,2) AS RPC_acc 
            FROM ( SELECT pid, adid,
            device,
            sum(oclicks) as oclicks,
            sum(orev) as orev,
            sum(ocost) as ocost
            FROM (SELECT e.historical_pid as pid,e.adid,
            e.device,
            ((e.xtime + INTERVAL '6 hours') at time zone tz)::date,
            avg(e.clicks) as oclicks,
            avg(e.revenue) as orev,
            avg(coalesce(total_cost, e.clicks * e.cost)) as ocost
            FROM ad_execute_inserts e
            JOIN user_portfolios up ON (e.historical_pid = up.pid)
            JOIN users U ON (up.userid = u.userid)
            JOIN timezones t ON (t.tzid = u.tzid)
            WHERE up.pid = (%1$s)
            and ((e.xtime + INTERVAL '6 hours') at time zone tz) between DATE '%2$s' and DATE '%3$s' + INTERVAL '1 day'
            and e.xtime between (DATE '%2$s' - INTERVAL '1 day') and (DATE '%3$s' + INTERVAL '2 day')
            group by 1,2,3,4) daily_avg
            group by pid,adid, device ) AS e
            RIGHT OUTER JOIN ( SELECT wh.pid,WH.adid,
            CASE WHEN device = 'm' THEN device ELSE 'b'::TEXT END as device, -- ad_execute has device = 'b' for desktop, tablet and non-device SEs            
            SUM(wh.clicks) AS clicks,
            SUM(wh.cost) AS spend
            FROM day_sem_keyword_agg WH
            WHERE WH.date >= DATE '%2$s'
            AND WH.date <= DATE '%3$s'
            AND WH.pid = (%1$s)
            AND WH.sid <> 77
            GROUP BY 1,2,3 
            
            UNION ALL
            
            SELECT
            wh.pid,wh.adid,
            'b'::TEXT as device,
            SUM(wh.est_clicks) AS clicks,
            SUM(wh.est_cost) AS spend
            FROM day_trig_keyword_agg WH
            WHERE WH.date >= DATE '%2$s'
            AND WH.date <= DATE '%3$s'
            AND WH.pid = (%1$s)
            AND WH.sid = 77
            GROUP BY 1,2,3 
            
            ) AS c USING (pid,adid, device)
            LEFT OUTER JOIN (SELECT wh.pid,wh.adid,
            CASE WHEN device = 'm' THEN device ELSE 'b'::TEXT END as device, -- ad_execute has device = 'b' for desktop, tablet and non-device SEs            
            SUM(coalesce(f.mobile_weight, weight) * (COALESCE(CT_VALUE_LAST, 0) + COALESCE(CTVT_VALUE_LAST, 0)) ) AS rev
            FROM day_revenue_keyword_agg WH
            JOIN portfolio_objectives PO ON (PO.pid = WH.pid)
            JOIN objective_function F ON (F.objid = PO.objid AND F.propertyid = WH.propertyid)
            WHERE WH.date >= DATE '%2$s'
            AND WH.date <= DATE '%3$s'  
            AND WH.pid = (%1$s)
            GROUP BY 1,2,3) AS r USING (pid,adid, device)
            JOIN ads using (adid)
            JOIN mgroupid_mtid using (mgroupid)
            JOIN match_types using (mtid)
            GROUP BY 1,2
            ORDER BY 8 DESC, 12 DESC",pid1,date3, date4)
  
  admin_opt_MT_2 = amo_db_get(query=admin_query_MT_accuracy_2, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)
  
  
  admin_opt_MT_2[is.na(admin_opt_MT_2)] <- 0
  
  admin_opt_MT_v2 <- merge(admin_pidd, admin_opt_MT_2, by.x = "pid",by.y = "pid", all.x = TRUE)
  admin_opt_MT_v2[is.na(admin_opt_MT_v2)] <- 0
  admin_opt_MT_v2 <- admin_opt_MT_v2[order(admin_opt_MT_v2$pid, decreasing = TRUE),]
  
  
  admin_final[is.na(admin_final)] <- 0
  
  admin_final$strategy <- ifelse(admin_final$strategy == "0", "None", admin_final$strategy)
  
  admin_final <- admin_final %>%mutate(DOWfeature = case_when(strategy == "weekly" ~ "Enabled",
                                                              strategy == "day_of_week" & param_value_cdow == "True" ~ "Enabled",
                                                              TRUE ~ "Not_Enabled"))
  
  admin_final <- admin_final[order(admin_final$pred_cost, decreasing = TRUE),]
  
# admin_opt_MT_v2 <- admin_opt_MT_v2[admin_opt_MT_v2$portfolio_name == "Grainger PLA",]
# 
# require(dplyr)
# admin_opt_MT_v1 <- admin_opt_MT_v2 %>% select(portfolio_name,match_type, act_cost, act_rev, pred_cost, pred_rev)
# admin_opt_MT_v1 <- admin_opt_MT_v1 %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
# admin_opt_MT_v1 <- admin_opt_MT_v1%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
# admin_opt_MT_v1 <- admin_opt_MT_v1 %>% mutate_at(vars(spendperc), funs(round(., 2)))
# colnames(admin_opt_MT_v1)[colnames(admin_opt_MT_v1)=="match_type"] <- "matchtype"
# admin_opt_MT_v12 <- admin_opt_MT_v1%>% group_by(portfolio_name) %>% filter(rank==1)
# admin_opt_MT_v12 <- admin_opt_MT_v1 %>% select(portfolio_name,matchtype, spendperc)
# 
#   mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
# 
#   ggplot(admin_opt_MT_v1, aes(x = "", y = spendperc, fill = match_type)) +
#     geom_bar(width = 1, stat = "identity", color = "white") +
#     coord_polar("y", start = 0)+
#     geom_text(aes(y = spendperc, label = spendperc), color = "white")+
# theme_void()
# 
#   library(plotly)
# 
#   p1 <- plot_ly(admin_opt_MT_v1, labels = ~ matchtype, values = ~ spendperc, type = 'pie', text = ~paste("Match Type:",matchtype, ",","Spend Percent:", spendperc), hoverinfo="text", textinfo="text") %>%
#     layout(title = 'Match_Type_Spend_Percent',
#            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
#   print(p1)
# 
#   admin_opt_MT_v3 <- admin_opt_MT_v2 %>% mutate(act_roas=act_rev/act_cost)
#   admin_opt_MT_v3 <- admin_opt_MT_v3 %>% mutate(pred_roas=pred_rev/pred_cost)
#   admin_opt_MT_v3 <- admin_opt_MT_v3 %>% mutate(roas_acc=act_roas/pred_roas*100)
#   admin_opt_MT_v3[is.na(admin_opt_MT_v3)] <- 0
#   admin_opt_MT_v5 <- admin_opt_MT_v3 %>% select(portfolio_name,act_roas, pred_roas,match_type)
#   admin_opt_MT_v6 <- melt(admin_opt_MT_v5)
# 
#   ggplot(admin_opt_MT_v6, aes(x=match_type, y=value, fill=variable)) +
#     geom_bar(stat='identity', position='dodge2')+ facet_grid(. ~ portfolio_name)
# 
#   admin_opt_MT_v3$Variance_di <- var(admin_opt_MT_v3$act_roas)
#   admin_opt_MT_v3$avg_di <- mean(admin_opt_MT_v3$act_roas)
#   admin_opt_MT_v3$dispersion_index_mt <- admin_opt_MT_v3$Variance_di/admin_opt_MT_v3$avg_di
# 
#   admin_opt_MT_v3$Variance_er <- var(admin_opt_MT_v3$roas_acc)
#   admin_opt_MT_v3$avg_er <- mean(admin_opt_MT_v3$roas_acc)
#   admin_opt_MT_v3$error_rate_mt <- admin_opt_MT_v3$Variance_er/admin_opt_MT_v3$avg_er
# 
#   di_mt <- unique(admin_opt_MT_v3$dispersion_index_mt)
#   er_mt <- unique(admin_opt_MT_v3$error_rate_mt)
# 
#   admin_final <- admin_final[admin_final$portfolio_name == "Grainger PLA RLSA",]
# 
#   admin_final$di <- di_mt
#   admin_final$er <- er_mt
  
  
admin_query_Click_level_accuracy_2 =   
    sprintf("SELECT pid,
            (CASE WHEN e.oclicks > 0 and e.oclicks <= 0.5
            THEN 0.1
            WHEN e.oclicks > 0.5 and e.oclicks <= 5
            THEN 1
            WHEN e.oclicks > 5 and e.oclicks <= 50
            THEN 10
            WHEN e.oclicks > 50 and e.oclicks <= 500
            THEN 100
            WHEN e.oclicks > 500 and e.oclicks <= 5000
            THEN 1000
            WHEN e.oclicks > 5000
            THEN 10000
            ELSE 0
            END)::NUMERIC(20,2)::TEXT AS clicklevel,
            SUM(e.oclicks)::NUMERIC(20,2) AS pred_clicks,
            SUM(c.clicks) AS act_clicks,
            SUM(e.ocost)::NUMERIC(20,2) AS pred_cost,
            SUM(c.spend) AS act_cost,
            ( COALESCE(SUM(e.ocost), 0) - COALESCE(SUM(c.spend), 0) )::NUMERIC(20,2) AS cost_diff,
            sum(e.orev)::NUMERIC(20,4) AS pred_rev,
            sum(r.rev)::NUMERIC(20,4) AS act_rev,
            sum( COALESCE((e.orev), 0) - COALESCE((r.rev), 0) )::NUMERIC(20,4) AS rev_diff,
            (100.0 * SUM(c.spend)/ NULLIF( SUM(e.ocost), 0 ) )::NUMERIC(20,2) AS cost_acc,
            (100.0 * SUM(c.clicks) / NULLIF( SUM(e.oclicks), 0 ) )::NUMERIC(20,2) AS click_acc,
            (100.0 * SUM(r.rev) / NULLIF ( SUM(e.orev), 0 ) )::NUMERIC(20,2) AS rev_acc,
            (100.0 * SUM((c.spend/NULLIF( c.clicks, 0 )) /NULLIF((e.ocost/NULLIF( e.oclicks, 0 )),0)) )::NUMERIC(20,2) AS CPC_acc,
            (100.0 * SUM((r.rev/NULLIF(c.clicks, 0 )) /NULLIF((e.orev/NULLIF( e.oclicks, 0 )),0))) ::NUMERIC(20,2) AS RPC_acc
            
            FROM ( SELECT pid, adid,
            device,
            sum(oclicks) as oclicks,
            sum(ocost) as ocost,
            sum(orev) as orev
            FROM (
            SELECT e.historical_pid as pid,e.adid,
            e.device,
            ((e.xtime + INTERVAL '6 hours') at time zone tz)::date,
            avg(e.clicks) as oclicks,
            avg(coalesce(total_cost, e.clicks * e.cost)) as ocost,
            avg(e.revenue) as orev
            FROM ad_execute_inserts e
            JOIN user_portfolios up ON (e.historical_pid = up.pid)
            JOIN users U ON (up.userid = u.userid)
            JOIN timezones t ON (t.tzid = u.tzid)
            WHERE up.pid = (%1$s)
            and ((e.xtime + INTERVAL '6 hours') at time zone tz) between DATE '%2$s' and DATE '%3$s' + INTERVAL '1 day'
            and e.xtime between (DATE '%2$s' - INTERVAL '1 day') and (DATE '%3$s' + INTERVAL '2 day')
            group by 2,3,4,1) daily_avg
            group by adid, device, pid ) AS e
            FULL JOIN (
            SELECT wh.pid,                  
            wh.adid,
            CASE WHEN WH.sid in (3, 94, 10) AND device = 'm' THEN device ELSE 'b' END as device, 
            SUM(wh.clicks) AS clicks,
            SUM(wh.cost) AS spend
            FROM day_sem_keyword_agg WH
            WHERE WH.date >= DATE '%2$s'
            AND WH.date <= DATE '%3$s'
            AND WH.pid = (%1$s)
            AND WH.sid <> 77
            GROUP BY 2,3,1
            
            UNION ALL
            
            SELECT wh.pid,                  
            wh.adid,
            'b'::TEXT as device,
            SUM(wh.est_clicks) AS clicks,
            SUM(wh.est_cost) AS spend
            FROM day_trig_keyword_agg WH
            WHERE WH.date >= DATE '%2$s'
            AND WH.date <= DATE '%3$s'
            AND WH.pid = (%1$s)
            AND WH.sid = 77
            GROUP BY wh.adid, wh.pid 
            ) AS c USING (adid, device,pid)
            FULL JOIN (
            SELECT wh.pid,                  
            wh.adid,
            CASE WHEN device = 'm' THEN device ELSE 'b'::TEXT END as device,
            SUM(coalesce(f.mobile_weight, weight) * (COALESCE(CT_VALUE_LAST, 0) + COALESCE(CTVT_VALUE_LAST, 0)) ) AS rev
            FROM day_revenue_keyword_agg WH
            JOIN portfolio_objectives PO ON (PO.pid = WH.pid)
            JOIN objective_function F ON (F.objid = PO.objid AND F.propertyid = WH.propertyid)
            WHERE WH.date >= DATE '%2$s'
            AND WH.date <= DATE '%3$s'
            AND WH.pid = (%1$s)
            GROUP BY wh.adid, 3,1 ) AS r USING (adid, device,pid)
            GROUP BY clicklevel,pid 
            
            UNION ALL 
            
            SELECT pid,
            'Total'::TEXT clicklevel,
            SUM(e.oclicks)::NUMERIC(20,2) AS pred_clicks,
            SUM(c.clicks) AS act_clicks,
            SUM(e.ocost)::NUMERIC(20,2) AS pred_cost,
            SUM(c.spend) AS act_cost,
            ( COALESCE(SUM(e.ocost), 0) - COALESCE(SUM(c.spend), 0) )::NUMERIC(20,2) AS cost_diff,
            sum(e.orev)::NUMERIC(20,4) AS pred_rev,
            sum(r.rev)::NUMERIC(20,4) AS act_rev,
            sum( COALESCE((e.orev), 0) - COALESCE((r.rev), 0) )::NUMERIC(20,4) AS rev_diff,
            (100.0 * SUM(c.spend)/ NULLIF( SUM(e.ocost), 0 ) )::NUMERIC(20,2) AS cost_acc,
            (100.0 * SUM(c.clicks) / NULLIF( SUM(e.oclicks), 0 ) )::NUMERIC(20,2) AS click_acc,
            (100.0 * SUM(r.rev) / NULLIF ( SUM(e.orev), 0 ) )::NUMERIC(20,2) AS rev_acc,
            (100.0 * SUM((c.spend/NULLIF( c.clicks, 0 )) /NULLIF((e.ocost/NULLIF( e.oclicks, 0 )),0)) )::NUMERIC(20,2) AS CPC_acc,
            (100.0 * SUM((r.rev/NULLIF(c.clicks, 0 )) /NULLIF((e.orev/NULLIF( e.oclicks, 0 )),0))) ::NUMERIC(20,2) AS RPC_acc
            
            FROM ( SELECT pid,adid,
            device,
            sum(oclicks) as oclicks,
            sum(ocost) as ocost,
            sum(orev) as orev
            FROM (
            SELECT e.historical_pid as pid,e.adid,
            e.device,
            ((e.xtime + INTERVAL '6 hours') at time zone tz)::date,
            avg(e.clicks) as oclicks,
            avg(coalesce(total_cost, e.clicks * e.cost)) as ocost,
            avg(e.revenue) as orev
            FROM ad_execute_inserts e
            JOIN user_portfolios up ON (e.historical_pid = up.pid)
            JOIN users U ON (up.userid = u.userid)
            JOIN timezones t ON (t.tzid = u.tzid)
            WHERE up.pid = (%1$s)
            and ((e.xtime + INTERVAL '6 hours') at time zone tz) between DATE '%2$s' and DATE '%3$s' + INTERVAL '1 day'
            and e.xtime between (DATE '%2$s' - INTERVAL '1 day') and (DATE '%3$s' + INTERVAL '2 day')
            group by 2,3,4,1) daily_avg
            group by adid, device,pid ) AS e
            FULL JOIN (
            SELECT  wh.pid,                 
            wh.adid,
            CASE WHEN WH.sid in (3, 94, 10) AND device = 'm' THEN device ELSE 'b' END as device, 
            SUM(wh.clicks) AS clicks,
            SUM(wh.cost) AS spend
            FROM day_sem_keyword_agg WH
            WHERE WH.date >= DATE '%2$s'
            AND WH.date <= DATE '%3$s'
            AND WH.pid = (%1$s)
            AND WH.sid <> 77
            GROUP BY wh.adid, 3,1
            
            UNION ALL
            
            SELECT  wh.pid,                 
            wh.adid,
            'b'::TEXT as device,
            SUM(wh.est_clicks) AS clicks,
            SUM(wh.est_cost) AS spend
            FROM day_trig_keyword_agg WH
            WHERE WH.date >= DATE '%2$s'
            AND WH.date <= DATE '%3$s'
            AND WH.pid = (%1$s)
            AND WH.sid = 77
            GROUP BY wh.adid, wh.pid 
            
            ) AS c USING (adid, device, pid)
            FULL JOIN (
            SELECT wh.pid,                  
            wh.adid,
            CASE WHEN device = 'm' THEN device ELSE 'b'::TEXT END as device,
            SUM(coalesce(f.mobile_weight, weight) * (COALESCE(CT_VALUE_LAST, 0) + COALESCE(CTVT_VALUE_LAST, 0)) ) AS rev
            FROM day_revenue_keyword_agg WH
            JOIN portfolio_objectives PO ON (PO.pid = WH.pid)
            JOIN objective_function F ON (F.objid = PO.objid AND F.propertyid = WH.propertyid)
            WHERE WH.date >= DATE '%2$s'
            AND WH.date <= DATE '%3$s'
            AND WH.pid = (%1$s)
            GROUP BY wh.adid, 3,1 ) AS r USING (adid, device,pid)
            
            GROUP BY clicklevel,pid
            
            ORDER BY clicklevel,pid; ", pid1,date3,date4)
  
admin_opt_Clicklevel_2 = amo_db_get(query=admin_query_Click_level_accuracy_2, db_tag =dbtag1, dc = dc_choose1, debug = TRUE)
  
admin_opt_Clicklevel_2[is.na(admin_opt_Clicklevel_2)] <- 0

admin_opt_Clicklevel_2 <- merge(admin_pidd, admin_opt_Clicklevel_2, by.x = "pid",by.y = "pid", all.x = TRUE)
admin_opt_Clicklevel_2[is.na(admin_opt_Clicklevel_2)] <- 0
admin_opt_Clicklevel_2 <- admin_opt_Clicklevel_2[order(admin_opt_Clicklevel_2$pid, decreasing = TRUE),]
# 
# admin_opt_Clicklevel_2 <- admin_opt_Clicklevel_2[admin_opt_Clicklevel_2$portfolio_name == "Grainger PLA",]
# admin_opt_Clicklevel_2 <- admin_opt_Clicklevel_2[order(admin_opt_Clicklevel_2$clicklevel, decreasing = FALSE),]
# 
# target <- c("0.00","0.10","1.00","10.00","100.00","1000.00","10000.00")
# admin_opt_v2 <- admin_opt_Clicklevel_2%>%filter(clicklevel %in% target)
# admin_opt_v2 <- admin_opt_v2 %>% group_by(portfolio_name) %>%mutate(total_cost=sum(act_cost))
# admin_opt_v2 <- admin_opt_v2 %>%filter(clicklevel=="0.00")%>%group_by(portfolio_name)%>%mutate(spendperc0=100*act_cost/total_cost)
# 
# 
# target <- c("0.00","0.10","1.00","10.00","100.00","1000.00","10000.00")
# admin_opt_v3 <- admin_opt_Clicklevel_2%>%filter(clicklevel %in% target)
# admin_opt_v3 <- admin_opt_v3 %>% select(portfolio_name,clicklevel, act_cost, act_rev, pred_cost, pred_rev)
# admin_opt_v3 <- admin_opt_v3 %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
# admin_opt_v3 <- admin_opt_v3%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
# admin_opt_v3 <- admin_opt_v3 %>% mutate_at(vars(spendperc), funs(round(., 2)))
# admin_opt_v3 <- admin_opt_v3 %>% mutate(act_roas=act_rev/act_cost)
# admin_opt_v3 <- admin_opt_v3 %>% mutate(pred_roas=pred_rev/pred_cost)
# admin_opt_v3 <- admin_opt_v3 %>% mutate(roas_acc=act_roas/pred_roas*100)
# admin_opt_v3[is.na(admin_opt_v3)] <- 0
# 
# admin_opt_v3$Variance_di <- var(admin_opt_v3$act_roas)
# admin_opt_v3$avg_di <- mean(admin_opt_v3$act_roas)
# admin_opt_v3$dispersion_index_cl <- admin_opt_v3$Variance_di/admin_opt_v3$avg_di
# 
# admin_opt_v3$Variance_er <- var(admin_opt_v3$roas_acc)
# admin_opt_v3$avg_er <- mean(admin_opt_v3$roas_acc)
# admin_opt_v3$error_rate_cl <- admin_opt_v3$Variance_er/admin_opt_v3$avg_er
# 
# di_cl <- unique(admin_opt_v3$dispersion_index_cl)
# er_cl <- unique(admin_opt_v3$error_rate_cl)
# 
#   p1 <- plot_ly(admin_opt_v3, labels = ~ clicklevel, values = ~ spendperc, type = 'pie', text = ~paste("Click Level:",clicklevel, ",","Spend Percent:", spendperc), hoverinfo="text", textinfo="text") %>%
#     layout( xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
#   print(p1)
# 
#   admin_opt_CL_v5 <- admin_opt_v3 %>% select(portfolio_name,act_roas, pred_roas,clicklevel)
#   admin_opt_CL_v5 <- as.data.frame(admin_opt_CL_v5)
#   admin_opt_CL_v6 <- melt(admin_opt_CL_v5)
# 
#   ggplot(admin_opt_CL_v6, aes(x=clicklevel, y=value, fill=variable)) +
#     geom_bar(stat='identity', position='dodge2')+ facet_grid(. ~ portfolio_name)


admin_query_device_accuracy_1 = 
  sprintf("SELECT c.pid,device, 
          sum(e.oclicks)::NUMERIC(20,2) AS pred_clicks,
          sum(c.clicks) AS act_clicks,
          (100.0 * SUM(c.clicks) / NULLIF( SUM(e.oclicks), 0 ) )::NUMERIC(20,2) AS click_acc,
          sum(e.orev)::NUMERIC(20,4) AS pred_rev,
          sum(r.rev)::NUMERIC(20,4) AS act_rev,
          sum( COALESCE((e.orev), 0) - COALESCE((r.rev), 0) )::NUMERIC(20,4) AS rev_diff,
          (100.0 * SUM(r.rev) / NULLIF ( SUM(e.orev), 0 ) )::NUMERIC(20,2) AS rev_acc,
          SUM(e.ocost)::NUMERIC(20,2) AS pred_cost,
          SUM(c.spend) AS act_cost,
          ( COALESCE(SUM(e.ocost), 0) - COALESCE(SUM(c.spend), 0) )::NUMERIC(20,2) AS cost_diff,
          (100.0 * SUM(c.spend)/ NULLIF( SUM(e.ocost), 0 ) )::NUMERIC(20,2) AS cost_acc,
          (100.0 * SUM((c.spend/NULLIF( c.clicks, 0 )) /NULLIF((e.ocost/NULLIF( e.oclicks, 0 )),0)) )::NUMERIC(20,2) AS CPC_acc,
          (100.0 * SUM((r.rev/NULLIF(c.clicks, 0 )) /NULLIF((e.orev/NULLIF( e.oclicks, 0 )),0))) ::NUMERIC(20,2) AS RPC_acc
          FROM ( SELECT pid,adid, device,
          sum(oclicks) as oclicks,
          sum(orev) as orev,
          sum(ocost) as ocost
          FROM ( SELECT e.historical_pid as pid,e.adid,
          e.device,
          ((e.xtime + INTERVAL '6 hours') at time zone tz)::date,
          avg(e.clicks) as oclicks,
          avg(e.revenue) as orev,
          avg(coalesce(total_cost, e.clicks * e.cost)) as ocost
          FROM ad_execute_inserts e
          JOIN user_portfolios up ON (e.historical_pid = up.pid)
          JOIN users U ON (up.userid = u.userid)
          JOIN timezones t ON (t.tzid = u.tzid)
          WHERE up.pid = (%1$s)
          and ((e.xtime + INTERVAL '6 hours') at time zone tz) between DATE '%2$s' and DATE '%3$s' + INTERVAL '1 day'
          and e.xtime between (DATE '%2$s' - INTERVAL '1 day') and (DATE '%3$s' + INTERVAL '2 day')
          group by 1,e.adid, e.device, 4 ) daily_avg
          group by pid,adid, device ) AS e
          RIGHT OUTER JOIN (SELECT 
          WH.pid,WH.adid,
          CASE WHEN device = 'm' THEN device ELSE 'b'::TEXT END as device, 
          SUM(wh.clicks) AS clicks,
          SUM(wh.cost) AS spend
          FROM day_sem_keyword_agg WH
          WHERE WH.date >= DATE '%2$s'
          AND WH.date <= DATE '%3$s'
          AND WH.pid = (%1$s)
          AND WH.sid <> 77
          GROUP BY wh.pid, wh.adid, 3 
          
          UNION ALL
          
          SELECT
          wh.pid,wh.adid,
          'b'::TEXT as device,
          SUM(wh.est_clicks) AS clicks,
          SUM(wh.est_cost) AS spend
          FROM day_trig_keyword_agg WH
          WHERE WH.date >= DATE '%2$s'
          AND WH.date <= DATE '%3$s'
          AND WH.pid = (%1$s)
          AND WH.sid = 77
          GROUP BY wh.pid, wh.adid, 3 
          
          ) AS c USING (pid,adid, device)
          LEFT OUTER JOIN (SELECT wh.pid,wh.adid,
          CASE WHEN device = 'm' THEN device ELSE 'b'::TEXT END as device, 
          SUM(coalesce(f.mobile_weight, weight) * (COALESCE(CT_VALUE_LAST, 0) + COALESCE(CTVT_VALUE_LAST, 0)) ) AS rev
          FROM day_revenue_keyword_agg WH
          JOIN portfolio_objectives PO ON (PO.pid = WH.pid)
          JOIN objective_function F ON (F.objid = PO.objid AND F.propertyid = WH.propertyid)
          WHERE WH.date >= DATE '%2$s'
          AND WH.date <= DATE '%3$s'
          AND WH.pid = (%1$s)
          GROUP BY wh.pid,wh.adid, 3 ) AS r USING (pid,adid, device)
          JOIN ads using (adid)
          JOIN mgroupid_mtid using (mgroupid)
          JOIN match_types using (mtid)
          GROUP BY 1,2
          ORDER BY 8 DESC, 12 DESC;", pid1,date3, date4)

#,date1,date2,date1,date2,active_pid_or1,date1,date2,active_pid_or1,date1,date2,active_pid_or1)

admin_opt_device_1 = amo_db_get(query=admin_query_device_accuracy_1, db_tag = dbtag1, dc = dc_choose1, debug = TRUE)

#
admin_opt_device_1[is.na(admin_opt_device_1)] <- 0

admin_opt_device_1 <- merge(admin_pidd, admin_opt_device_1, by.x = "pid",by.y = "pid", all.x = TRUE)
admin_opt_device_1$device[is.na(admin_opt_device_1$device)] <- "None"
admin_opt_device_1[is.na(admin_opt_device_1)] <- 0
admin_opt_device_1 <- admin_opt_device_1[order(admin_opt_device_1$pid, decreasing = TRUE),]

# admin_opt_device_1 <- admin_opt_device_1[admin_opt_device_1$portfolio_name == "Grainger PLA",]
# 
# 
# admin_opt_v4 <- admin_opt_device_1 %>% select(portfolio_name,device, act_cost, act_rev, pred_cost, pred_rev)
# admin_opt_v4 <- admin_opt_v4 %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
# admin_opt_v4 <- admin_opt_v4%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
# admin_opt_v4 <- admin_opt_v4 %>% mutate_at(vars(spendperc), funs(round(., 2)))
# admin_opt_v4 <- admin_opt_v4 %>% mutate(act_roas=act_rev/act_cost)
# admin_opt_v4 <- admin_opt_v4 %>% mutate(pred_roas=pred_rev/pred_cost)
# admin_opt_v4 <- admin_opt_v4 %>% mutate(roas_acc=act_roas/pred_roas*100)
# admin_opt_v4[is.na(admin_opt_v4)] <- 0
#
# 
# data_1 <- admin_opt_v4
# data <- admin_opt_v4 %>% ungroup() %>% select(device, act_cost, pred_cost, act_rev, pred_rev, rank, spendperc, act_roas, pred_roas, roas_acc)

#
# admin_opt_v4$Variance_di <- var(admin_opt_v4$act_roas)
# admin_opt_v4$avg_di <- mean(admin_opt_v4$act_roas)
# admin_opt_v4$dispersion_index_dev <- admin_opt_v4$Variance_di/admin_opt_v4$avg_di
# 
# admin_opt_v4$Variance_er <- var(admin_opt_v4$roas_acc)
# admin_opt_v4$avg_er <- mean(admin_opt_v4$roas_acc)
# admin_opt_v4$error_rate_dev <- admin_opt_v4$Variance_er/admin_opt_v4$avg_er
# 
# var_dev <- unique(admin_opt_v4$dispersion_index_dev)
# er_dev <- unique(admin_opt_v4$error_rate_dev)
# 
# admin_final$devdi <- var_dev
# admin_final$dever <- er_dev
# 
# admin_final <-   admin_final %>% mutate(reco_devf = case_when(devdi > 0.2 & DEVfeature == "Not_Enabled" & Spend_Scenario == "Overspend"  ~ "Enable",
#                                                     devdi > 0.2 & DEVfeature == "Not_Enabled" & Spend_Scenario == "Underspend"~ "Enable",
#                                                     devdi > 0.2 & DEVfeature == "Not_Enabled" & RPC_Scenario =="Poor_RPC" ~ "Enable",
#                                                     Spend_Scenario == "Overspend" & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 & click_coverage > 0.1  ~ "Further_investigate",
#                                                     Spend_Scenario == "Overspend" & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 & click_coverage < 0.1 ~ "Disable",
#                                                     Spend_Scenario == "Overspend" & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 & revenue_coverage < 0.05 ~ "Disable",
#                                                     Spend_Scenario == "Overspend" & devdi > 0.2 & DEVfeature == "Enabled" & dever < 0.2  ~ "Disable",
#                                                     Spend_Scenario == "Overspend" & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 & revenue_coverage >0.05 ~ "Further_investigate",
#                                                     Spend_Scenario == "Underspend"  & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 & revenue_coverage > 0.05 ~ "Further_investigate",
#                                                     Spend_Scenario == "Underspend"  & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 &  revenue_coverage < 0.05 ~ "Disable",
#                                                     Spend_Scenario == "Underspend"  & devdi > 0.2 & DEVfeature == "Enabled" & dever < 0.2  ~ "Disable",
#                                                     Spend_Scenario == "Underspend"  & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 &  click_coverage < 0.1 ~ "Disable",
#                                                     Spend_Scenario == "Underspend"  & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 &  click_coverage > 0.1 ~ "Further_investigate",
#                                                     RPC_Scenario == "Poor_RPC"  & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 & revenue_coverage > 0.05 ~ "Further_investigate",
#                                                     RPC_Scenario == "Poor_RPC"  & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 &  revenue_coverage < 0.05 ~ "Disable",
#                                                     RPC_Scenario == "Poor_RPC"  & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 &  click_coverage < 0.1 ~ "Disable",
#                                                     RPC_Scenario == "Poor_RPC"  & devdi > 0.2 & DEVfeature == "Enabled" & dever < 0.2  ~ "Disable",
#                                                     RPC_Scenario == "Poor_RPC"  & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 &  click_coverage > 0.1 ~ "Further_investigate",
#                                                     TRUE ~ DEVfeature))

# 
# df <- data.frame()
# 
# df <- rbind(df, c("Match Type",di_mt,er_mt), c("Device",var_dev,er_dev),c("Click Level",di_cl,er_cl),c("Day of Week",di_dow,er_dow))
# 
# df[ df == "NaN" ] <- NA
# 
# names(df) <- c("Dimension", "Variance","Error Rate")
# 
# df
# 
# # , c("Device",var_dev,er_dev),c("Click Level",var_cl,er_cl),c("Day of Week",var_dow,er_dow)
# 
# library(plyr)
# rename(df, c("X1"="Dimension", "X2"="Variance", "X3"="Error Rate"))
# 


  admin_final_v1 <- admin_final %>%select(portfolio_name,campaign_spend_multiple,reco_sm, param_value_zil, reco_ZILstatus, param_value_lb, reco_LearningBudget, inc_unconstrained_revenue, reco_max_bid, param_value_intraday, reco_Intraday)
  #reco_cost_HL,reco_rev_HL
  admin_finall_v12 <- melt(as.data.table(admin_final_v1), id.vars =  "portfolio_name")
  admin_finall_v13 <- admin_finall_v12 %>% filter(variable == "campaign_spend_multiple" | variable == "param_value_zil" | variable == "param_value_lb" | variable == "inc_unconstrained_revenue")
  colnames(admin_finall_v13) <- c("portfolio_name","Settings","Current_value")
  
  library("plyr")
  admin_finall_v13$Settings <- revalue(admin_finall_v13$Settings, c("param_value_zil"="Zero_imp_rule", "param_value_lb"="Learning_budget", "inc_unconstrained_revenue" = "Portfolio_max_bid"))
  
  admin_finall_v14 <- admin_finall_v12 %>% filter(variable == "reco_sm" | variable == "reco_ZILstatus" |  variable == "reco_LearningBudget" | variable == "reco_max_bid" )
  colnames(admin_finall_v14) <- c("portfolio_name","Settings","Recommended_value")
  admin_finall_v14$Settings <- revalue(admin_finall_v14$Settings, c("reco_sm"="campaign_spend_multiple","reco_ZILstatus"="Zero_imp_rule" ,"reco_LearningBudget"="Learning_budget", "reco_max_bid" = "Portfolio_max_bid"))
  
  admin_finall_v15 <- merge(admin_finall_v13, admin_finall_v14, by = c("portfolio_name","Settings"))
  
  admin_finall_v15[is.na(admin_finall_v15)] <- 0
  
  data_1 <- admin_final[admin_final$client_account_name == "grainger",]
  data_1 <- data_1 %>% select(portfolio_name, pred_cost,cost_acc, rpc_acc, Scenario)
  write.csv(data_1, file="portfolios.csv")
  
  data <- list(df1=admin_final, df2=admin_finall_v15, df3=admin_daily_v11, df4=admin_daily_v12, df5=admin_daily_v3, df6=admin_opt_MT_v2, df7=admin_opt_Clicklevel_2, df8=admin_opt_device_1, df9=admin_daily_v31, df10=admin_daily_v32)
  
  return(data)
    }
                       
    })

  })


  output$choose_dataset_1 <- renderUI({
    selectInput("username", "Username:", c("choose your desired client", unique(as.character((as.data.frame(users))$username))))
  })

  output$choose_dataset <- renderUI({
    selectInput("Dbtag_DC", "Dbtag_DC:", " ")
  })
  
  output$choose_dataset_2 <- renderUI({
    selectInput("PortfolioNamePID", "Portfolio_Name, PID:", c("select your desired portfolio",unique(as.character((as.data.frame(passdata()))$portfolionamepid))))
  })

  observeEvent(input$username,{
    updateSelectInput(session,'Dbtag_DC',
                      choices = unique(as.character((as.data.frame(users))$dbtagdc)[(as.character((as.data.frame(users))$username))==input$username]))
  })
  
  
  output$selected_var <- renderText({ 
    paste("You have selected the Client Account Name : ", input$username)
  })
  
  output$daterange1 <- renderText({ 
    paste("The diagnostics are based on the cost and rpc accuracies for the last 7 days: ", Sys.Date()-8, "-",Sys.Date()-1)
  })
  
  output$daterange2 <- renderText({ 
    paste("The recommendations are based on the model accuracies stemmed for the last 90 days: ", Sys.Date()-90, "-",Sys.Date()-1,"except cost model half life recommendations that stem from last 30 days:", Sys.Date()-30, "-",Sys.Date()-1)
  })
  

  install.packages("formattable")
  library(formattable)

  output$tbl_1 <- DT::renderDataTable(server = FALSE, DT::datatable({
    data <- as.data.frame(passdata())
    data <- data %>% select(pid,portfolio_name,portfolio_status, pred_cost,cost_acc, rpc_acc, Scenario)
    data <- setNames(data, c("PID","portfolio_name", "portfolio_status","predicted_cost","cost_acc_%", "rpc_acc_%", "Scenario"))
    data
  }, 
  extensions = 'Buttons', options = list(scrollX=TRUE,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv')
  ),rownames = FALSE) %>%
    formatStyle('predicted_cost',
                background = styleColorBar(range(data$predicted_cost), 'lightblue'),
                backgroundSize = '95% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'right')%>%
    formatCurrency('predicted_cost',currency = "", interval = 3, mark = ",")
  %>%
    formatStyle(
      'cost_acc_%',
      color = styleInterval(c(80, 120), c('red', 'black', 'red'))
    )
  %>%
    formatRound('cost_acc_%', digits = 2)
  %>%
    formatStyle(
      'rpc_acc_%',
      color = styleInterval(c(80, 120), c('red', 'black', 'red'))
    )
  %>%
    formatRound('rpc_acc_%', digits = 2)
  )

    
   output$selected_var_1 <- renderText({
   
       data <- as.data.frame(passdata1()$df1)
 
       paste("Diagnostic : ", data$Scenario)
  
  })
   
   output$selected_var_3 <- renderText({
   
       data <- as.data.frame(passdata1()$df1)
    
       paste("Cost_Accuracy_% : ", data$cost_acc)
 
   })
   
   output$selected_var_4 <- renderText({

       data <- as.data.frame(passdata1()$df1)
     
       paste("RPC_Accuracy_% : ", data$rpc_acc)

   })

   
   
   output$dimdt <- DT::renderDataTable(DT::datatable({
     

       
       data <- as.data.frame(passdata1()$df6)

       
       data_1 <- as.data.frame(passdata1()$df8)

       
       data_2 <- as.data.frame(passdata1()$df7)

       
       data_3 <- as.data.frame(passdata1()$df4)
  
       
       data <- data %>% select(portfolio_name,match_type, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       colnames(data)[colnames(data)=="match_type"] <- "matchtype"
       data <- data %>% mutate(act_roas=act_rev/act_cost)
       data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
       data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
       
       data[is.na(data)] <- 0
       
       data$Variance_di <- var(data$act_roas)
       data$avg_di <- mean(data$act_roas)
       data$dispersion_index_mt <- data$Variance_di/data$avg_di
       
       data$Variance_er <- var(data$roas_acc)
       data$avg_er <- mean(data$roas_acc)
       data$error_rate_mt <- data$Variance_er/data$avg_er
       
       var_mt <- unique(data$dispersion_index_mt)
       er_mt <- unique(data$error_rate_mt)


       data_1 <- data_1 %>% select(portfolio_name,device, act_cost, act_rev, pred_cost, pred_rev)
       data_1 <- data_1 %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data_1 <- data_1%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data_1 <- data_1 %>% mutate_at(vars(spendperc), funs(round(., 2)))
       data_1 <- data_1 %>% mutate(act_roas=act_rev/act_cost)
       data_1 <- data_1 %>% mutate(pred_roas=pred_rev/pred_cost)
       data_1 <- data_1 %>% mutate(roas_acc=act_roas/pred_roas*100)

       data_1[is.na(data_1)] <- 0

       data_1$Variance_di <- var(data_1$act_roas)
       data_1$avg_di <- mean(data_1$act_roas)
       data_1$dispersion_index_dev <- data_1$Variance_di/data_1$avg_di

       data_1$Variance_er <- var(data_1$roas_acc)
       data_1$avg_er <- mean(data_1$roas_acc)
       data_1$error_rate_dev <- data_1$Variance_er/data_1$avg_er

       var_dev <- unique(data_1$dispersion_index_dev)
       er_dev <- unique(data_1$error_rate_dev)


       data_2 <- data_2[order(data_2$clicklevel, decreasing = FALSE),]
       target <- c("0.00","0.10","1.00","10.00","100.00","1000.00","10000.00")
       data_2 <- data_2%>%filter(clicklevel %in% target)

       data_2 <- data_2 %>% select(portfolio_name,clicklevel, act_cost, act_rev, pred_cost, pred_rev)
       data_2 <- data_2 %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data_2 <- data_2%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data_2 <- data_2 %>% mutate_at(vars(spendperc), funs(round(., 2)))
       data_2 <- data_2 %>% mutate(act_roas=act_rev/act_cost)
       data_2 <- data_2 %>% mutate(pred_roas=pred_rev/pred_cost)
       data_2 <- data_2 %>% mutate(roas_acc=act_roas/pred_roas*100)

       data_2[is.na(data_2)] <- 0

       data_2$Variance_di <- var(data_2$act_roas)
       data_2$avg_di <- mean(data_2$act_roas)
       data_2$dispersion_index_cl <- data_2$Variance_di/data_2$avg_di

       data_2$Variance_er <- var(data_2$roas_acc)
       data_2$avg_er <- mean(data_2$roas_acc)
       data_2$error_rate_cl <- data_2$Variance_er/data_2$avg_er

       var_cl <- unique(data_2$dispersion_index_cl)
       er_cl <- unique(data_2$error_rate_cl)


       data_3[is.na(data_3)] <- 0

       library(lubridate)
       data_3$date <- as.Date(data_3$date)
       data_3$dow <- lubridate::wday(data_3$date, label=TRUE)

       data_v3 <- data_3 %>% select(portfolio_name, dow, act_cost, act_rev, pred_cost, pred_rev)

       data_v4 <- aggregate(.~portfolio_name+dow,data_v3,sum)

       data_v4 <- data_v4 %>% mutate(act_roas=act_rev/act_cost)
       data_v4 <- data_v4 %>% mutate(pred_roas=pred_rev/pred_cost)
       data_v4 <- data_v4 %>% mutate(roas_acc=act_roas/pred_roas*100)

       data_v4$Variance_di <- var(data_v4$act_roas)
       data_v4$avg_di <- mean(data_v4$act_roas)
       data_v4$dispersion_index_dow <- data_v4$Variance_di/data_v4$avg_di

       data_v4$Variance_er <- var(data_v4$roas_acc)
       data_v4$avg_er <- mean(data_v4$roas_acc)
       data_v4$error_rate_dow <- data_v4$Variance_er/data_v4$avg_er

       var_dow <- unique(data_v4$dispersion_index)
       er_dow <- unique(data_v4$error_rate)
       
       df <- data.frame()

       df <- rbind(df, c("Match Type",var_mt,er_mt), c("Device",var_dev,er_dev), c("Click Level",var_cl,er_cl), c("Day of Week",var_dow,er_dow) )
       
       df[ df == "NaN" ] <- NA
       
       names(df) <- c("Dimension", "Dispersion_Index","Error_Rate")
       
       df[is.na(df)] <- 0
       
       df <- df[order(df$Dispersion_Index, decreasing = TRUE),]
       
       rownames(df) <- NULL

       df
       

       # , c("Device",var_dev,er_dev),c("Click Level",var_cl,er_cl),c("Day of Week",var_dow,er_dow)

       # library(plyr)
       # rename(df, c("X1"="Dimension", "X2"="Variance", "X3"="Error Rate"))
 
     
   },extensions = 'Buttons', options = list(scrollX=TRUE,
                                           dom = 'Bfrtip',
                                           buttons = c('copy', 'csv')
   ),rownames = FALSE)
   %>%formatRound('Dispersion_Index', digits = 3)
   %>%formatRound('Error_Rate', digits = 3)
   )
   
   
   
   output$admin_dev <- DT::renderDataTable(DT::datatable({
     
  
       data <- as.data.frame(passdata1()$df8)
     
       data <- data %>% select(portfolio_name,device, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       data <- data %>% mutate(act_roas=act_rev/act_cost)
       data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
       data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
       data[is.na(data)] <- 0
       data <- data %>% ungroup()%>% select(device, act_cost, pred_cost, act_rev, pred_rev, rank, spendperc, act_roas, pred_roas, roas_acc)
       data
  
     
   }, extensions = 'Buttons', options = list(scrollX=TRUE,
                                              dom = 'Bfrtip',
                                              buttons = c('copy', 'csv')
   ),rownames = FALSE) %>%formatRound('act_cost', digits = 3)
   %>%formatRound('act_rev', digits = 3)
   %>%formatRound('pred_cost', digits = 3)
   %>%formatRound('pred_rev', digits = 3)
   %>%formatRound('spendperc', digits = 3)
   %>%formatRound('act_roas', digits = 3)
   %>%formatRound('pred_roas', digits = 3)
   %>%formatRound('roas_acc', digits = 3))
   
   output$admin_MT <- DT::renderDataTable(DT::datatable({
     

       data <- as.data.frame(passdata1()$df6)
      
       data <- data %>% select(portfolio_name,match_type, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       colnames(data)[colnames(data)=="match_type"] <- "matchtype"
       data <- data %>% mutate(act_roas=act_rev/act_cost)
       data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
       data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
       data[is.na(data)] <- 0
       data <- data %>% ungroup()%>% select(matchtype, act_cost, pred_cost, act_rev, pred_rev, rank, spendperc, act_roas, pred_roas, roas_acc)
       data

     
   }, extensions = 'Buttons', options = list(scrollX=TRUE,
                                             dom = 'Bfrtip',
                                             buttons = c('copy', 'csv')
   ),rownames = FALSE) %>%formatRound('act_cost', digits = 3)
   %>%formatRound('act_rev', digits = 3)
   %>%formatRound('pred_cost', digits = 3)
   %>%formatRound('pred_rev', digits = 3)
   %>%formatRound('spendperc', digits = 3)
   %>%formatRound('act_roas', digits = 3)
   %>%formatRound('pred_roas', digits = 3)
   %>%formatRound('roas_acc', digits = 3))
   
   output$admin_CL <- DT::renderDataTable(DT::datatable({
     

       data <- as.data.frame(passdata1()$df7)

       data <- data[order(data$clicklevel, decreasing = FALSE),]
       target <- c("0.00","0.10","1.00","10.00","100.00","1000.00","10000.00")
       data <- data%>%filter(clicklevel %in% target)
       data <- data %>% select(portfolio_name,clicklevel, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       data <- data %>% mutate(act_roas=act_rev/act_cost)
       data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
       data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
       data[is.na(data)] <- 0
       data <- data %>% ungroup()%>% select(clicklevel, act_cost, pred_cost, act_rev, pred_rev, rank, spendperc, act_roas, pred_roas, roas_acc)
       data <- data[order(data$clicklevel, decreasing = FALSE),]
       data
 
     
   }, extensions = 'Buttons', options = list(scrollX=TRUE,
                                             dom = 'Bfrtip',
                                             buttons = c('copy', 'csv')
   ),rownames = FALSE) %>%formatRound('act_cost', digits = 3)
   %>%formatRound('act_rev', digits = 3)
   %>%formatRound('pred_cost', digits = 3)
   %>%formatRound('pred_rev', digits = 3)
   %>%formatRound('spendperc', digits = 3)
   %>%formatRound('act_roas', digits = 3)
   %>%formatRound('pred_roas', digits = 3)
   %>%formatRound('roas_acc', digits = 3))
   
    
   output$textcl <- renderText({
   
       
       data <- as.data.frame(passdata1()$df7)
     
       
       admin_opt_Clicklevel_2 <- data[order(data$clicklevel, decreasing = FALSE),]

       target <- c("0.00","0.10","1.00","10.00","100.00","1000.00","10000.00")
       admin_opt_v2 <- admin_opt_Clicklevel_2%>%filter(clicklevel %in% target)
       admin_opt_v2 <- admin_opt_v2 %>% group_by(portfolio_name) %>%mutate(total_cost=sum(act_cost))
       admin_opt_v2 <- admin_opt_v2 %>%filter(clicklevel=="0.00")%>%group_by(portfolio_name)%>%mutate(spendperc0=100*act_cost/total_cost)

       
       paste("Unpredicted cost share:", round(unique(admin_opt_v2$spendperc0),digits = 3))
 
   })
   
   output$textdevdi <- renderText({

       data <- as.data.frame(passdata1()$df8)

       
       data <- data %>% select(portfolio_name,device, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       data <- data %>% mutate(act_roas=act_rev/act_cost)
       data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
       data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
       
       data[is.na(data)] <- 0
       
       data$Variance_di <- var(data$act_roas)
       data$avg_di <- mean(data$act_roas)
       data$dispersion_index_dev <- data$Variance_di/data$avg_di
       
       data$Variance_er <- var(data$roas_acc)
       data$avg_er <- mean(data$roas_acc)
       data$error_rate_dev <- data$Variance_er/data$avg_er
       
       data[ data == "NaN" ] <- NA
       
       
       data[is.na(data)] <- 0
       
       paste("Device Dispersion Index:", round(unique(data$dispersion_index_dev),digits = 3))
   
     
   })
   
   output$textmtdi <- renderText({


       data <- as.data.frame(passdata1()$df6)

       
       data <- data %>% select(portfolio_name,match_type, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       colnames(data)[colnames(data)=="match_type"] <- "matchtype"
       data <- data %>% mutate(act_roas=act_rev/act_cost)
       data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
       data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
       
       data[is.na(data)] <- 0
       
       data$Variance_di <- var(data$act_roas)
       data$avg_di <- mean(data$act_roas)
       data$dispersion_index_mt <- data$Variance_di/data$avg_di
       
       data$Variance_er <- var(data$roas_acc)
       data$avg_er <- mean(data$roas_acc)
       data$error_rate_mt <- data$Variance_er/data$avg_er
       
       data[ data == "NaN" ] <- NA
       
       
       data[is.na(data)] <- 0

       paste("Match Type Dispersion Index:", round(unique(data$dispersion_index_mt),digits=3))
  
   })
   
   
   output$textcldi <- renderText({

       
       data <- as.data.frame(passdata1()$df7)
    
       
       data <- data[order(data$clicklevel, decreasing = FALSE),]
       target <- c("0.00","0.10","1.00","10.00","100.00","1000.00","10000.00")
       data <- data%>%filter(clicklevel %in% target)
       
       data <- data %>% select(portfolio_name,clicklevel, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       data <- data %>% mutate(act_roas=act_rev/act_cost)
       data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
       data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
       
       data[is.na(data)] <- 0
       
       data$Variance_di <- var(data$act_roas)
       data$avg_di <- mean(data$act_roas)
       data$dispersion_index_cl <- data$Variance_di/data$avg_di
       
       data$Variance_er <- var(data$roas_acc)
       data$avg_er <- mean(data$roas_acc)
       data$error_rate_cl <- data$Variance_er/data$avg_er
       
       
       data[ data == "NaN" ] <- NA
       
       
       data[is.na(data)] <- 0
       
       paste("Click Level Dispersion Index:", round(unique(data$dispersion_index_cl),digits=3))

   })
   
   
   output$textcler <- renderText({

       
       data <- as.data.frame(passdata1()$df7)

       
       data <- data[order(data$clicklevel, decreasing = FALSE),]
       target <- c("0.00","0.10","1.00","10.00","100.00","1000.00","10000.00")
       data <- data%>%filter(clicklevel %in% target)
       
       data <- data %>% select(portfolio_name,clicklevel, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       data <- data %>% mutate(act_roas=act_rev/act_cost)
       data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
       data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
       
       data[is.na(data)] <- 0
       
       data$Variance_di <- var(data$act_roas)
       data$avg_di <- mean(data$act_roas)
       data$dispersion_index_cl <- data$Variance_di/data$avg_di
       
       data$Variance_er <- var(data$roas_acc)
       data$avg_er <- mean(data$roas_acc)
       data$error_rate_cl <- data$Variance_er/data$avg_er
       
       data[ data == "NaN" ] <- NA
       
       
       data[is.na(data)] <- 0
       
       paste("Click Level Error Rate:", round(unique(data$error_rate_cl),digits=3))
 
     
   })
   
   
   output$textmter <- renderText({
 
       
       data <- as.data.frame(passdata1()$df6)
       data <- data %>% select(portfolio_name,match_type, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       colnames(data)[colnames(data)=="match_type"] <- "matchtype"
       data <- data %>% mutate(act_roas=act_rev/act_cost)
       data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
       data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
       
       data[is.na(data)] <- 0
       
       data$Variance_di <- var(data$act_roas)
       data$avg_di <- mean(data$act_roas)
       data$dispersion_index_mt <- data$Variance_di/data$avg_di
       
       data$Variance_er <- var(data$roas_acc)
       data$avg_er <- mean(data$roas_acc)
       data$error_rate_mt <- data$Variance_er/data$avg_er
       
       data[ data == "NaN" ] <- NA
       
       
       data[is.na(data)] <- 0
       
       paste("Match Type Error Rate:", round(unique(data$error_rate_mt), digits = 3))
   
     
   })
   
   
   output$textdever <- renderText({
 
       
       data <- as.data.frame(passdata1()$df8)
      
       
       data <- data %>% select(portfolio_name,device, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       data <- data %>% mutate(act_roas=act_rev/act_cost)
       data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
       data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
       
       data[is.na(data)] <- 0
       
       data$Variance_di <- var(data$act_roas)
       data$avg_di <- mean(data$act_roas)
       data$dispersion_index_dev <- data$Variance_di/data$avg_di
       
       data$Variance_er <- var(data$roas_acc)
       data$avg_er <- mean(data$roas_acc)
       data$error_rate_dev <- data$Variance_er/data$avg_er
       
       data[ data == "NaN" ] <- NA
       
       
       data[is.na(data)] <- 0
       
       paste("Device Error Rate:", round(unique(data$error_rate_dev),digits=3))
  
     
   })
   
   
   output$devplot <- renderPlot({

       data <- as.data.frame(passdata1()$df8)

       data <- data %>% select(portfolio_name,device, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       data <- data %>% mutate(act_roas=act_rev/act_cost)
       data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
       data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
       
       data[is.na(data)] <- 0
       data1 <- data %>% select(portfolio_name,act_roas,device)
       data1 <- as.data.frame(data1)
       data2 <- melt(data1)
       
       p <- ggplot() +
         geom_bar(data=data2, aes(x=device, y=value),stat='identity', position='dodge2', color="darkcyan", fill="darkcyan")+ facet_grid(. ~ portfolio_name)+labs(x="device", y="actual_roas") + scale_fill_discrete(guide=FALSE)+geom_hline(data=data1, mapping = aes(yintercept=mean(act_roas), linetype="dotted"),size=1, color = "darkcyan", show.legend =T)+scale_linetype_manual(name =  "Legend-Line ", labels = c("dotted"="mean roas"), values = c("twodash"))+
         guides(linetype = guide_legend("Legend-Line",override.aes = list(shape = 0, color="darkcyan")))+
         # theme_set(theme_gray(base_size = 18))
         theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
               axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
       
       print(p)
     
   })
   
   output$devplot1 <- renderPlot({
     
     data <- as.data.frame(passdata1()$df8)
     
     data <- data %>% select(portfolio_name,device, act_cost, act_rev, pred_cost, pred_rev)
     data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
     data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
     data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
     data <- data %>% mutate(act_roas=act_rev/act_cost)
     data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
     data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
     
     data[is.na(data)] <- 0
     data1 <- data %>% select(portfolio_name,roas_acc,device)
     data1 <- as.data.frame(data1)
     data2 <- melt(data1)
     
     p <- ggplot() +
       geom_bar(data=data2, aes(x=device, y=value),stat='identity', position='dodge2', color="darkcyan", fill="darkcyan")+ facet_grid(. ~ portfolio_name)+labs(x="device", y="roas_acc") + scale_fill_discrete(guide=FALSE)+geom_hline(data=data1, mapping = aes(yintercept=100, linetype="dotted"),size=1, color = "darkcyan", show.legend =T)+scale_linetype_manual(name =  "Legend-Line ", labels = c("dotted"="100 %"), values = c("twodash"))+
       guides(linetype = guide_legend("Legend-Line",override.aes = list(shape = 0, color="darkcyan")))+
       theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
             axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
     
     print(p)
     
   })
   
   output$devplot2 <- renderPlot({
     
     data <- as.data.frame(passdata1()$df8)
     
     data <- data %>% select(portfolio_name,device, act_cost, act_rev, pred_cost, pred_rev)
     data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
     data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
     data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
     data <- data %>% mutate(act_roas=act_rev/act_cost)
     data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
     data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
     
     data[is.na(data)] <- 0
     data1 <- data %>% select(portfolio_name,act_cost,device)
     data1 <- as.data.frame(data1)
     data2 <- melt(data1)
     
     p <- ggplot() +
       geom_bar(data=data2, aes(x=device, y=value),stat='identity', position='dodge2', color="red", fill="red")+ facet_grid(. ~ portfolio_name)+labs(x="device", y="actual_cost") + scale_fill_discrete(guide=FALSE)+
       theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
             axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
     
     print(p)
     
   })
   
   output$devplot3 <- renderPlot({
     
     data <- as.data.frame(passdata1()$df8)
     
     data <- data %>% select(portfolio_name,device, act_cost, act_rev, pred_cost, pred_rev)
     data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
     data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
     data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
     data <- data %>% mutate(act_roas=act_rev/act_cost)
     data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
     data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
     data <- data %>% mutate(cost_acc=act_cost/pred_cost*100)
     
     data[is.na(data)] <- 0
     data1 <- data %>% select(portfolio_name,cost_acc,device)
     data1 <- as.data.frame(data1)
     data2 <- melt(data1)
     
     p <- ggplot() +
       geom_bar(data=data2, aes(x=device, y=value),stat='identity', position='dodge2', color="red", fill="red")+ facet_grid(. ~ portfolio_name)+labs(x="device", y="cost_acc") + scale_fill_discrete(guide=FALSE)+
       theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
             axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
     
     print(p)
     
   })
   
   
   output$MTplot <- renderPlot({

       data <- as.data.frame(passdata1()$df6)
       

       data <- data %>% select(portfolio_name,match_type, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       colnames(data)[colnames(data)=="match_type"] <- "matchtype"
       data <- data %>% mutate(act_roas=act_rev/act_cost)
       data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
       data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
       
       data[is.na(data)] <- 0
       data1 <- data %>% select(portfolio_name,act_roas,matchtype)
       data1 <- as.data.frame(data1)
       data2 <- melt(data1)
       
       p <- ggplot() +
         geom_bar(data=data2, aes(x=matchtype, y=value),stat='identity', position='dodge2', color="darkcyan", fill="darkcyan")+ facet_grid(. ~ portfolio_name)+labs(x="match type", y="actual_roas") + scale_fill_discrete(guide=FALSE)+geom_hline(data=data1, mapping = aes(yintercept=mean(act_roas), linetype="dotted"),size=1, color = "darkcyan", show.legend =T)+scale_linetype_manual(name =  "Legend-Line ", labels = c("dotted"="mean roas"), values = c("twodash"))+
         guides(linetype = guide_legend("Legend-Line",override.aes = list(shape = 0, color="darkcyan")))+
         theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
               axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
       
       print(p)
       
 
   })
   
   output$MTplot1 <- renderPlot({
     
     data <- as.data.frame(passdata1()$df6)
     
     
     data <- data %>% select(portfolio_name,match_type, act_cost, act_rev, pred_cost, pred_rev)
     data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
     data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
     data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
     colnames(data)[colnames(data)=="match_type"] <- "matchtype"
     data <- data %>% mutate(act_roas=act_rev/act_cost)
     data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
     data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
     
     data[is.na(data)] <- 0
     data1 <- data %>% select(portfolio_name,roas_acc,matchtype)
     data1 <- as.data.frame(data1)
     data2 <- melt(data1)
     
     p <- ggplot() +
       geom_bar(data=data2, aes(x=matchtype, y=value),stat='identity', position='dodge2', color="darkcyan", fill="darkcyan")+ facet_grid(. ~ portfolio_name)+labs(x="match type", y="roas_acc") + scale_fill_discrete(guide=FALSE)+geom_hline(data=data1, mapping = aes(yintercept=100, linetype="dotted"),size=1, color = "darkcyan", show.legend =T)+scale_linetype_manual(name =  "Legend-Line ", labels = c("dotted"="100 %"), values = c("twodash"))+
       guides(linetype = guide_legend("Legend-Line",override.aes = list(shape = 0, color="darkcyan")))+
       theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
             axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
     
     print(p)
     
   })
   
   output$MTplot2 <- renderPlot({
     
     data <- as.data.frame(passdata1()$df6)
     
     
     data <- data %>% select(portfolio_name,match_type, act_cost, act_rev, pred_cost, pred_rev)
     data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
     data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
     data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
     colnames(data)[colnames(data)=="match_type"] <- "matchtype"
     data <- data %>% mutate(act_roas=act_rev/act_cost)
     data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
     data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
     
     data[is.na(data)] <- 0
     data1 <- data %>% select(portfolio_name,act_cost,matchtype)
     data1 <- as.data.frame(data1)
     data2 <- melt(data1)
     
     p <- ggplot() +
       geom_bar(data=data2, aes(x=matchtype, y=value),stat='identity', position='dodge2', color="red", fill="red")+ facet_grid(. ~ portfolio_name)+labs(x="match type", y="act_cost") + scale_fill_discrete(guide=FALSE)+
       theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
             axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
     print(p)
   })
   
   output$MTplot3 <- renderPlot({
     
     data <- as.data.frame(passdata1()$df6)
     
     
     data <- data %>% select(portfolio_name,match_type, act_cost, act_rev, pred_cost, pred_rev)
     data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
     data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
     data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
     colnames(data)[colnames(data)=="match_type"] <- "matchtype"
     data <- data %>% mutate(act_roas=act_rev/act_cost)
     data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
     data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
     data <- data %>% mutate(cost_acc=act_cost/pred_cost*100)
     
     data[is.na(data)] <- 0
     data1 <- data %>% select(portfolio_name,cost_acc,matchtype)
     data1 <- as.data.frame(data1)
     data2 <- melt(data1)
     
     p <- ggplot() +
       geom_bar(data=data2, aes(x=matchtype, y=value),stat='identity', position='dodge2', color="red", fill="red")+ facet_grid(. ~ portfolio_name)+labs(x="match type", y="cost_acc") + scale_fill_discrete(guide=FALSE)+
       theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
             axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
     print(p)
     
   })
   
   
   
   output$CLplot <- renderPlot({
     
       data <- as.data.frame(passdata1()$df7)
       

       
       data <- data[order(data$clicklevel, decreasing = FALSE),]
       target <- c("0.00","0.10","1.00","10.00","100.00","1000.00","10000.00")
       data <- data%>%filter(clicklevel %in% target)
       
       data <- data %>% select(portfolio_name,clicklevel, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       data <- data %>% mutate(act_roas=act_rev/act_cost)
       data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
       data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
       
       data[is.na(data)] <- 0
       data1 <- data %>% select(portfolio_name,act_roas,clicklevel)
       data1 <- as.data.frame(data1)
       data2 <- melt(data1)
       
       p <- ggplot() +
         geom_bar(data=data2, aes(x=clicklevel, y=value),stat='identity', position='dodge2', color="darkcyan", fill="darkcyan")+ facet_grid(. ~ portfolio_name)+labs(x="clicklevel", y="act_roas") + scale_fill_discrete(guide=FALSE)+geom_hline(data=data1, mapping = aes(yintercept=mean(act_roas), linetype="dotted"),size=1, color = "darkcyan", show.legend =T)+scale_linetype_manual(name =  "Legend-Line ", labels = c("dotted"="mean roas"), values = c("twodash"))+
         guides(linetype = guide_legend("Legend-Line",override.aes = list(shape = 0, color="darkcyan")))+
         theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
               axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
       
       print(p)
    
     
   })
   
   output$CLplot1<- renderPlot({
     
     data <- as.data.frame(passdata1()$df7)
     
     
     
     data <- data[order(data$clicklevel, decreasing = FALSE),]
     target <- c("0.00","0.10","1.00","10.00","100.00","1000.00","10000.00")
     data <- data%>%filter(clicklevel %in% target)
     
     data <- data %>% select(portfolio_name,clicklevel, act_cost, act_rev, pred_cost, pred_rev)
     data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
     data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
     data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
     data <- data %>% mutate(act_roas=act_rev/act_cost)
     data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
     data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
     
     data[is.na(data)] <- 0
     data1 <- data %>% select(portfolio_name,roas_acc,clicklevel)
     data1 <- as.data.frame(data1)
     data2 <- melt(data1)
     
     p <- ggplot() +
       geom_bar(data=data2, aes(x=clicklevel, y=value),stat='identity', position='dodge2', color="darkcyan", fill="darkcyan")+ facet_grid(. ~ portfolio_name)+labs(x="clicklevel", y="roas_acc") + scale_fill_discrete(guide=FALSE)+geom_hline(data=data1, mapping = aes(yintercept=100, linetype="dotted"),size=1, color = "darkcyan", show.legend =T)+scale_linetype_manual(name =  "Legend-Line ", labels = c("dotted"="100 %"), values = c("twodash"))+
       guides(linetype = guide_legend("Legend-Line",override.aes = list(shape = 0, color="darkcyan")))+
       theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
             axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
     
     print(p)
     
     
   })
   
   output$CLplot2 <- renderPlot({
     
     data <- as.data.frame(passdata1()$df7)
     
     
     
     data <- data[order(data$clicklevel, decreasing = FALSE),]
     target <- c("0.00","0.10","1.00","10.00","100.00","1000.00","10000.00")
     data <- data%>%filter(clicklevel %in% target)
     
     data <- data %>% select(portfolio_name,clicklevel, act_cost, act_rev, pred_cost, pred_rev)
     data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
     data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
     data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
     data <- data %>% mutate(act_roas=act_rev/act_cost)
     data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
     data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
     
     data[is.na(data)] <- 0
     data1 <- data %>% select(portfolio_name,act_cost,clicklevel)
     data1 <- as.data.frame(data1)
     data2 <- melt(data1)
     
     p <- ggplot() +
       geom_bar(data=data2, aes(x=clicklevel, y=value),stat='identity', position='dodge2', color="red", fill="red")+ facet_grid(. ~ portfolio_name)+labs(x="clicklevel", y="act_cost") + scale_fill_discrete(guide=FALSE)+
       theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
             axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
     
     print(p)
     
     
   })
   
   output$CLplot3 <- renderPlot({
     
     data <- as.data.frame(passdata1()$df7)
     
     
     
     data <- data[order(data$clicklevel, decreasing = FALSE),]
     target <- c("0.00","0.10","1.00","10.00","100.00","1000.00","10000.00")
     data <- data%>%filter(clicklevel %in% target)
     
     data <- data %>% select(portfolio_name,clicklevel, act_cost, act_rev, pred_cost, pred_rev)
     data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
     data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
     data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
     data <- data %>% mutate(act_roas=act_rev/act_cost)
     data <- data %>% mutate(pred_roas=pred_rev/pred_cost)
     data <- data %>% mutate(roas_acc=act_roas/pred_roas*100)
     data <- data %>% mutate(cost_acc=act_cost/pred_cost*100)
     
     data[is.na(data)] <- 0
     data1 <- data %>% select(portfolio_name,cost_acc,clicklevel)
     data1 <- as.data.frame(data1)
     data2 <- melt(data1)
     
     p <- ggplot() +
       geom_bar(data=data2, aes(x=clicklevel, y=value),stat='identity', position='dodge2', color="red", fill="red")+ facet_grid(. ~ portfolio_name)+labs(x="clicklevel", y="cost_acc") + scale_fill_discrete(guide=FALSE)+
       theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
             axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
     
     print(p)
     
     
   })
   
   output$topdevice <- renderText({
 
       data <- as.data.frame(passdata1()$df8)
   
       data <- data %>% select(portfolio_name,device, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       data <- data%>% group_by(portfolio_name) %>% filter(rank==1)
       data <- data %>% select(portfolio_name,device, spendperc)    
       paste("Top Cost Device : ", data$device)

   })
   
   output$topmatch <- renderText({

       data <- as.data.frame(passdata1()$df6)
     
       data <- data %>% select(portfolio_name,match_type, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       colnames(data)[colnames(data)=="match_type"] <- "matchtype"
       data <- data%>% group_by(portfolio_name) %>% filter(rank==1)
       data <- data %>% select(portfolio_name,matchtype, spendperc)    
       paste("Top Cost Match Type : ", data$matchtype)

   })
   
   output$plotMT <- renderPlotly({
  
       
       data <- as.data.frame(passdata1()$df6)
   
       data <- data %>% select(portfolio_name,match_type, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       colnames(data)[colnames(data)=="match_type"] <- "matchtype"
       
       library(plotly)

       plot_ly(data, labels = ~ matchtype, values = ~ spendperc, type = 'pie', text = ~paste("Match Type:",matchtype, ",","Spend Percent:", spendperc), hoverinfo="text", textinfo="text") %>%
         layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


   })
   
   output$plotdev <- renderPlotly({

       
       data <- as.data.frame(passdata1()$df8)
      
       data <- data %>% select(portfolio_name,device, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       
       library(plotly)
       
       plot_ly(data, labels = ~ device, values = ~ spendperc, type = 'pie', text = ~paste("Device:",device, ",","Spend Percent:", spendperc), hoverinfo="text", textinfo="text") %>%
         layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
       

     
   })
   
   output$plotCL <- renderPlotly({
 
       
       data <- as.data.frame(passdata1()$df7)
   
       
       data <- data[order(data$clicklevel, decreasing = FALSE),]
       target <- c("0.00","0.10","1.00","10.00","100.00","1000.00","10000.00")
       data <- data%>%filter(clicklevel %in% target)
       
       data <- data %>% select(portfolio_name,clicklevel, act_cost, act_rev, pred_cost, pred_rev)
       data <- data %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data <- data%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data <- data %>% mutate_at(vars(spendperc), funs(round(., 2)))
       
       library(plotly)
       
       plot_ly(data, labels = ~ clicklevel, values = ~ spendperc, type = 'pie', text = ~paste("Click Level:",clicklevel, ",","Spend Percent:", spendperc), hoverinfo="text", textinfo="text") %>%
         layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
       
 
     
   })
   
   
   output$admin_reco_2 <- DT::renderDataTable(DT::datatable({

 
       data_1 <- as.data.frame(passdata1()$df1)
      
       
       data_2 <- as.data.frame(passdata1()$df3)

       
       data_3 <- as.data.frame(passdata1()$df4)
       
       data_5 <- as.data.frame(passdata1()$df8)
       
       data_6 <- as.data.frame(passdata1()$df9)

       
       data_2 <- data_2 %>% select(portfolio_name, date, cost_acc, rpc_acc)
       
       y_data <- data_2 %>% select(date,cost_acc, rpc_acc)
       
       y_data[is.na(y_data)] <- 0
       
       data_21 <- data_6 %>% select(portfolio_name, date, cost_acc)
       
       y_data1 <- data_21 %>% select(date,cost_acc)
       
       y_data1[is.na(y_data1)] <- 0
       
       library(changepoint)
       
       cptm_CP         <- cpt.mean(y_data1$cost_acc, penalty='MBIC',pen.value=0, method='BinSeg',
                                   test.stat="Normal", minseglen=7, class=TRUE)
       cptm_CP
       
       cpts_CP         <- cpts(cptm_CP) # change point time points
       cpts_CP
       
       cost_change <-  ifelse(max(cpts_CP)==0,0,(Sys.Date()-(as.Date(y_data1[max(cpts_CP),1]))))
       
       library(changepoint)
       
       cptm_CP_1         <- cpt.mean(y_data$rpc_acc, penalty='MBIC',pen.value=0, method='BinSeg',
                                     test.stat="Normal", minseglen=7, class=TRUE)
       cptm_CP_1
       
       cpts_CP_1         <- cpts(cptm_CP_1) # change point time points
       cpts_CP_1
       
       rpc_change <-  ifelse(max(cpts_CP_1)==0,0,(Sys.Date()-(as.Date(y_data[max(cpts_CP_1),1]))))
       

       data_1$cchange <- cost_change
       data_1$rchange <- rpc_change
       
       data_3[is.na(data_3)] <- 0
       
       library(lubridate)
       data_3$date <- as.Date(data_3$date)
       data_3$dow <- lubridate::wday(data_3$date, label=TRUE)

       data_v3 <- data_3 %>% select(portfolio_name, dow, act_cost, act_rev, pred_cost, pred_rev)

       data_v4 <- aggregate(.~portfolio_name+dow,data_v3,sum)

       data_v4 <- data_v4 %>% mutate(act_roas=act_rev/act_cost)
       data_v4 <- data_v4 %>% mutate(pred_roas=pred_rev/pred_cost)
       data_v4 <- data_v4 %>% mutate(roas_acc=act_roas/pred_roas*100)

       data_v4$Variance_di <- var(data_v4$act_roas)
       data_v4$avg_di <- mean(data_v4$act_roas)
       data_v4$dispersion_index <- data_v4$Variance_di/data_v4$avg_di
       
       data_v4$Variance_er <- var(data_v4$roas_acc)
       data_v4$avg_er <- mean(data_v4$roas_acc)
       data_v4$error_rate <- data_v4$Variance_er/data_v4$avg_er

       di <- unique(data_v4$dispersion_index)
       er <- unique(data_v4$error_rate)
       
       data_1$di <- di
       data_1$er <- er
       
       data_1 <- data_1 %>% mutate(reco_dow = case_when(di > 0.2 & DOWfeature == "Not_Enabled" & Spend_Scenario == "Overspend"  ~ "Enable_DOW",
                                                                  di > 0.2 & DOWfeature == "Not_Enabled" & Spend_Scenario == "Underspend"~ "Enable_DOW",
                                                                  di > 0.2 & DOWfeature == "Not_Enabled" & RPC_Scenario =="Poor RPC Accuracy" ~ "Enable_DOW",
                                                                  Spend_Scenario == "Overspend" & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 & click_coverage > 0.1  ~ "Further_investigate",
                                                                  Spend_Scenario == "Overspend" & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 & click_coverage < 0.1 ~ "Disable_DOW",
                                                                  Spend_Scenario == "Overspend" & di > 0.2 & DOWfeature == "Enabled" & er < 0.2 ~ "Disable_DOW",
                                                                  Spend_Scenario == "Overspend" & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 & revenue_coverage < 0.05 ~ "Disable_DOW",
                                                                  Spend_Scenario == "Overspend" & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 & revenue_coverage >0.05 ~ "Further_investigate",
                                                                  Spend_Scenario == "Underspend"  & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 & revenue_coverage > 0.05 ~ "Further_investigate",
                                                                  Spend_Scenario == "Underspend"  & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 &  revenue_coverage < 0.05 ~ "Disable_DOW",
                                                                  Spend_Scenario == "Underspend"  & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 &  click_coverage < 0.1 ~ "Disable_DOW",
                                                                  Spend_Scenario == "Underspend"  & di > 0.2 & DOWfeature == "Enabled" & er < 0.2 ~ "Disable_DOW",
                                                                  Spend_Scenario == "Underspend"  & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 &  click_coverage > 0.1 ~ "Further_investigate",
                                                                  RPC_Scenario == "Poor RPC Accuracy"  & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 & revenue_coverage > 0.05 ~ "Further_investigate",
                                                                  RPC_Scenario == "Poor RPC Accuracy"  & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 &  revenue_coverage < 0.05 ~ "Disable_DOW",
                                                                  RPC_Scenario == "Poor RPC Accuracy"  & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 &  click_coverage < 0.1 ~ "Disable_DOW",
                                                                  RPC_Scenario == "Poor RPC Accuracy"  & di > 0.2 & DOWfeature == "Enabled" & er < 0.2  ~ "Disable_DOW",
                                                                  RPC_Scenario == "Poor RPC Accuracy"  & di > 0.2 & DOWfeature == "Enabled" & er > 0.2 &  click_coverage > 0.1 ~ "Further_investigate",
                                                                  TRUE ~ DOWfeature))
       
      
       data_1 <- data_1 %>% mutate(reco_cost_HL = case_when(Spend_Scenario!= "Cost Accuracy Within Range" & cchange > 0 & click_coverage < 0.1 & max(cost_model_half_life,cchange) < 14 & max(cost_model_half_life,cchange) > 3 ~ max(cost_model_half_life, cchange),
                                                            Spend_Scenario!= "Cost Accuracy Within Range" & cchange > 0 & click_coverage < 0.1 & max(cost_model_half_life,cchange) >14 ~ 14,
                                                            Spend_Scenario!= "Cost Accuracy Within Range" & cchange > 0 & click_coverage < 0.1 & max(cost_model_half_life,cchange) < 3 ~ 3,
                                                            Spend_Scenario!= "Cost Accuracy Within Range" & cchange > 0 & click_coverage > 0.1 & min((cost_model_half_life-3)/2, 3) < 3 ~ 3,
                                                            Spend_Scenario!= "Cost Accuracy Within Range" & cchange > 0 & click_coverage > 0.1 & ((cost_model_half_life-3)/2) > 3 ~ (as.numeric(min((cost_model_half_life-3)/2, 3) )),
                                                            TRUE ~ as.numeric(cost_model_half_life)))
       
       data_1 <-  data_1 %>% mutate(reco_rev_HL = case_when(RPC_Scenario!= "RPC Accuracy OK" & rchange > 0 & revenue_coverage < 0.05 & max(revenue_model_half_life,rchange) < 60 & max(revenue_model_half_life,rchange) > 10 ~ max(revenue_model_half_life, rchange),
                                                            RPC_Scenario!= "RPC Accuracy OK" & rchange > 0 & revenue_coverage < 0.05 & max(revenue_model_half_life,rchange) >60 ~ 60,
                                                            RPC_Scenario!= "RPC Accuracy OK" & rchange > 0 & revenue_coverage < 0.05 & max(revenue_model_half_life,rchange) < 10 ~ 10,
                                                            RPC_Scenario!= "RPC Accuracy OK" & rchange > 0 & revenue_coverage > 0.05 & min((revenue_model_half_life-10)/2, 10) < 10 ~ 10,
                                                            RPC_Scenario!= "RPC Accuracy OK" & rchange > 0 & revenue_coverage > 0.05 & ((revenue_model_half_life-3)/2) > 10 ~ (as.numeric(min((revenue_model_half_life-10)/2, 10) )),
                                                            TRUE ~ revenue_model_half_life))
       
       
       
       data_5 <- data_5 %>% select(portfolio_name,device, act_cost, act_rev, pred_cost, pred_rev)
       data_5 <- data_5 %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data_5 <- data_5%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data_5 <- data_5 %>% mutate_at(vars(spendperc), funs(round(., 2)))
       data_5 <- data_5 %>% mutate(act_roas=act_rev/act_cost)
       data_5 <- data_5 %>% mutate(pred_roas=pred_rev/pred_cost)
       data_5 <- data_5 %>% mutate(roas_acc=act_roas/pred_roas*100)
       
       data_5[is.na(data_5)] <- 0
       
       data_5$Variance_di <- var(data_5$act_roas)
       data_5$avg_di <- mean(data_5$act_roas)
       data_5$dispersion_index_dev <- data_5$Variance_di/data_5$avg_di
       
       data_5$Variance_er <- var(data_5$roas_acc)
       data_5$avg_er <- mean(data_5$roas_acc)
       data_5$error_rate_dev <- data_5$Variance_er/data_5$avg_er
       
       data_5[ data_5 == "NaN" ] <- NA
       
       
       data_5[is.na(data_5)] <- 0

      data_1$devdi <- unique(data_5$dispersion_index_dev)
      data_1$dever <- unique(data_5$error_rate_dev)
      
       
       # data_1 <- data_1 %>% mutate(DEVfeature = case_when(ba_enable_mobile==TRUE | ba_enable_tablet==TRUE | ba_enable_computer==TRUE ~ "Enabled",
       #                                                  ba_enable_mobile==FALSE & ba_enable_tablet==FALSE & ba_enable_computer==FALSE ~ "Not_Enabled"))
       # 
       # min and max default Mobile -50, 100: tablet -0, 0: comp: 0, 0, audience/loc -50 200
       #  decrease max bid by 20%, keep positive, increase min bid by 20%, reduce range
       
        data_1 <-   data_1 %>% mutate(reco_devf = case_when(devdi > 0.2 & DEVfeature == "Not_Enabled" & Spend_Scenario == "Overspend"  ~ "Enable",
                                                    devdi > 0.2 & DEVfeature == "Not_Enabled" & Spend_Scenario == "Underspend"~ "Enable",
                                                    devdi > 0.2 & DEVfeature == "Not_Enabled" & RPC_Scenario =="Poor_RPC" ~ "Enable",
                                                    Spend_Scenario == "Overspend" & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 & click_coverage > 0.1  ~ "Further_investigate",
                                                    Spend_Scenario == "Overspend" & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 & click_coverage < 0.1 ~ "Disable",
                                                    Spend_Scenario == "Overspend" & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 & revenue_coverage < 0.05 ~ "Disable",
                                                    Spend_Scenario == "Overspend" & devdi > 0.2 & DEVfeature == "Enabled" & dever < 0.2  ~ "Disable",
                                                    Spend_Scenario == "Overspend" & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 & revenue_coverage >0.05 ~ "Further_investigate",
                                                    Spend_Scenario == "Underspend"  & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 & revenue_coverage > 0.05 ~ "Further_investigate",
                                                    Spend_Scenario == "Underspend"  & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 &  revenue_coverage < 0.05 ~ "Disable",
                                                    Spend_Scenario == "Underspend"  & devdi > 0.2 & DEVfeature == "Enabled" & dever < 0.2  ~ "Disable",
                                                    Spend_Scenario == "Underspend"  & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 &  click_coverage < 0.1 ~ "Disable",
                                                    Spend_Scenario == "Underspend"  & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 &  click_coverage > 0.1 ~ "Further_investigate",
                                                    RPC_Scenario == "Poor_RPC"  & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 & revenue_coverage > 0.05 ~ "Further_investigate",
                                                    RPC_Scenario == "Poor_RPC"  & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 &  revenue_coverage < 0.05 ~ "Disable",
                                                    RPC_Scenario == "Poor_RPC"  & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 &  click_coverage < 0.1 ~ "Disable",
                                                    RPC_Scenario == "Poor_RPC"  & devdi > 0.2 & DEVfeature == "Enabled" & dever < 0.2  ~ "Disable",
                                                    RPC_Scenario == "Poor_RPC"  & devdi > 0.2 & DEVfeature == "Enabled" & dever > 0.2 &  click_coverage > 0.1 ~ "Further_investigate",
                                                    TRUE ~ DEVfeature))
       

       data <- data_1 %>%select(portfolio_name,campaign_spend_multiple,reco_sm, param_value_zil, reco_ZILstatus, param_value_lb, reco_LearningBudget, unconstrained_rev_share_percent, reco_max_bid, cost_model_half_life, revenue_model_half_life, reco_cost_HL, reco_rev_HL, param_value_intraday, reco_Intraday, DOWfeature, reco_dow, DEVfeature, reco_devf)
       #reco_cost_HL,reco_rev_HL
       
       data_v12 <- melt(as.data.table(data), id.vars =  "portfolio_name")
       data_v13 <- data_v12 %>% filter(variable == "campaign_spend_multiple" | variable == "param_value_zil" | variable == "param_value_lb" | variable == "unconstrained_rev_share_percent" | variable == "cost_model_half_life" | variable == "revenue_model_half_life" | variable == "param_value_intraday" | variable == "DOWfeature" | variable == "DEVfeature")
       colnames(data_v13) <- c("portfolio_name","Settings","Current_value")

       library("plyr")
       data_v13$Settings <- revalue(data_v13$Settings, c("param_value_zil"="Zero Impression Bid Units", "param_value_lb"="Learning Budget", "unconstrained_rev_share_percent" = "Inc.Unconstrained Revenue % ", "param_value_intraday"= "Intraday", "DOWfeature"= "DOW Modeling", "cost_model_half_life" = "Cost Model Half Life", "revenue_model_half_life" = "Revenue Model Half Life","campaign_spend_multiple"="Multiple", "DEVfeature"="Device BId Adjustments"))

       data_v14 <- data_v12 %>% filter(variable == "reco_sm" | variable == "reco_ZILstatus" |  variable == "reco_LearningBudget" | variable == "reco_max_bid" | variable == "reco_cost_HL" | variable == "reco_rev_HL" | variable == "reco_Intraday" | variable == "reco_dow"| variable == "reco_devf")
       colnames(data_v14) <- c("portfolio_name","Settings","Recommended_value")
       data_v14$Settings <- revalue(data_v14$Settings, c("reco_sm"="Multiple","reco_ZILstatus"="Zero Impression Bid Units" ,"reco_LearningBudget"="Learning Budget", "reco_max_bid" = "Inc.Unconstrained Revenue % ", "reco_cost_HL" = "Cost Model Half Life", "reco_rev_HL" = "Revenue Model Half Life", "reco_Intraday" = "Intraday", "reco_dow" = "DOW Modeling", "reco_devf"="Device BId Adjustments"))

       data_v15 <- merge(data_v13, data_v14, by = c("portfolio_name","Settings"))

       data_v15[is.na(data_v15)] <- 0

       data_v15 <- data_v15 %>% select(Settings, Current_value, Recommended_value )
       
       library(dplyr)
       x <- c("DOW Modeling", "Multiple","Device BId Adjustments", "Learning Budget", "Zero Impression Bid Units","Intraday","Cost Model Half Life","Revenue Model Half Life","Inc.Unconstrained Revenue % ")
       
      
       data_v15$NewTemp <- ifelse(data_v15$Recommended_value == data_v15$Current_value, 1, 0)
       
       data_v15 <- data_v15 %>%
         slice(match(x, Settings))
       
       
       data_v15
 
   },
   extensions = 'Buttons', options = list(scrollX=TRUE,
                                           dom = 'Bfrtip',
                                           buttons = c('copy', 'csv'), columnDefs = list(list(targets = 4, visible = FALSE))
   ))%>% formatStyle('Recommended_value', 'NewTemp',backgroundColor = styleEqual(c(1,0), c('white', 'yellow')), fontWeight = styleEqual(c(1,0), c('normal', 'bold'))))
   
   
   output$dowTable <- DT::renderDataTable(DT::datatable({
     
 
       
       data_3 <- as.data.frame(passdata1()$df4)
    
       data_3[is.na(data_3)] <- 0
       
       library(lubridate)
       data_3$date <- as.Date(data_3$date)
       data_3$dow <- lubridate::wday(data_3$date, label=TRUE)
       
       data_v3 <- data_3 %>% select(portfolio_name, dow, act_cost, act_rev, pred_cost, pred_rev)
       
       data_v4 <- aggregate(.~portfolio_name+dow,data_v3,sum)
       
       data_v4 <- data_v4 %>% group_by(portfolio_name) %>% mutate(rank = rank(desc(act_cost)), total_cost= sum(act_cost)) %>% arrange(rank)
       data_v4 <- data_v4%>% group_by(portfolio_name) %>% mutate(spendperc=100*act_cost/total_cost)
       data_v4 <- data_v4 %>% mutate_at(vars(spendperc), funs(round(., 2)))
       
       data_v4 <- data_v4 %>% mutate(act_roas=act_rev/act_cost)
       data_v4 <- data_v4 %>% mutate(pred_roas=pred_rev/pred_cost)
       data_v4 <- data_v4 %>% mutate(roas_acc=act_roas/pred_roas*100)
       
       data_v4[is.na(data_v4)] <- 0
       data_v4 <- data_v4 %>% ungroup()%>% select(dow, act_cost, pred_cost, act_rev, pred_rev, rank, spendperc, act_roas, pred_roas, roas_acc)
       data_v4

       

   }, extensions = 'Buttons', options = list(scrollX=TRUE,
                                             dom = 'Bfrtip',
                                             buttons = c('copy', 'csv')
   ),rownames = FALSE) %>%formatRound('act_cost', digits = 3)
   %>%formatRound('act_rev', digits = 3)
   %>%formatRound('pred_cost', digits = 3)
   %>%formatRound('pred_rev', digits = 3)
   %>%formatRound('spendperc', digits = 3)
   %>%formatRound('act_roas', digits = 3)
   %>%formatRound('pred_roas', digits = 3)
   %>%formatRound('roas_acc', digits = 3))
   
  
   output$plot4 <- renderPlot({

       data_1 <- as.data.frame(passdata1()$df10)

       data_1 <- data_1 %>% select(portfolio_name, date, click_acc, cpc_acc)
       
       y_data <- data_1 %>% select(date,click_acc, cpc_acc)
       
       y_data[is.na(y_data)] <- 0
       
       library(changepoint)
       
       cptm_CP         <- cpt.mean(y_data$click_acc, penalty='MBIC',pen.value=0, method='BinSeg',
                                   test.stat="Normal", minseglen=7, class=TRUE)
       cptm_CP
       
       plot(cptm_CP)
       
       cpts_CP         <- cpts(cptm_CP) # change point time points
       cpts_CP
       
       library(changepoint)
       
       cptm_CP_1         <- cpt.mean(y_data$cpc_acc, penalty='MBIC',pen.value=0, method='BinSeg',
                                     test.stat="Normal", minseglen=7, class=TRUE)
       cptm_CP_1
       
       plot(cptm_CP_1)
       
       cpts_CP_1         <- cpts(cptm_CP_1) # change point time points
       cpts_CP_1
       
       library("outliers")
       y_data$Click_Acc_score <- scores(type="z", y_data$click_acc, prob=.95)
       y_data$CPC_Acc_score <- scores(type="z", y_data$cpc_acc, prob=.95)
       cost_outliers <- list(subset(y_data, Click_Acc_score==TRUE)[,1])
       cost_outliers
       revenue_outliers <- list(subset(y_data, CPC_Acc_score==TRUE)[,1])
       revenue_outliers
       
       y_data_1 <- subset(y_data, CPC_Acc_score==TRUE)
       y_data_2 <- subset(y_data, Click_Acc_score==TRUE)
       
       p= ggplot() +
         geom_line(data = data_1, aes(x = as.Date(date), y = click_acc, group=1), color="darkred" , show.legend = F) +
         # geom_point(data = y_data_2, aes(x = as.Date(date), y = click_acc, group=1, shape="circle"),size=5, color="darkred", show.legend = T)+
         xlab('Date') +
         ylab('click_acc') + facet_grid(. ~ portfolio_name)+
         scale_x_date(date_breaks = "1 week")+
         theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
               axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
         # geom_vline(data=y_data, mapping = aes(xintercept=as.Date(y_data[max(cpts_CP),1]), linetype="twodash"),size=1, color = "darkred", show.legend =T)+
         # geom_text(data=y_data, mapping=aes(x=as.Date(y_data[max(cpts_CP),1]), y=0, label=as.Date(y_data[max(cpts_CP),1] , sep = " ")), size=3, vjust=1, hjust=0, angle=90)+
         # scale_shape_manual(name =  " Legend-Shape", labels = c("circle"="outliers"), values = c("circle"))+
         # scale_linetype_manual(name =  "Legend-Line ", labels = c("twodash"="change_mean"), values = c("twodash"))+
         # guides(shape = guide_legend("Legend-Shape",override.aes = list(linetype = 0, color="darkred")), 
         #        linetype = guide_legend("Legend-Line",override.aes = list(shape = 0, color="darkred")))
       print(p)       

     
   })
   
   
   output$plot5 <- renderPlot({
    
       data_1 <- as.data.frame(passdata1()$df10)
   
       data_1 <- data_1 %>% select(portfolio_name, date, click_acc, cpc_acc)
       
       y_data <- data_1 %>% select(date,click_acc, cpc_acc)
       
       y_data[is.na(y_data)] <- 0
       
       library(changepoint)
       
       cptm_CP         <- cpt.mean(y_data$click_acc, penalty='MBIC',pen.value=0, method='BinSeg',
                                   test.stat="Normal", minseglen=7, class=TRUE)
       cptm_CP
       
       plot(cptm_CP)
       
       cpts_CP         <- cpts(cptm_CP) # change point time points
       cpts_CP
       
       library(changepoint)
       
       cptm_CP_1         <- cpt.mean(y_data$cpc_acc, penalty='MBIC',pen.value=0, method='BinSeg',
                                     test.stat="Normal", minseglen=7, class=TRUE)
       cptm_CP_1
       
       plot(cptm_CP_1)
       
       cpts_CP_1         <- cpts(cptm_CP_1) # change point time points
       cpts_CP_1
       
       library("outliers")
       y_data$Click_Acc_score <- scores(type="z", y_data$click_acc, prob=.95)
       y_data$CPC_Acc_score <- scores(type="z", y_data$cpc_acc, prob=.95)
       cost_outliers <- list(subset(y_data, Click_Acc_score==TRUE)[,1])
       cost_outliers
       revenue_outliers <- list(subset(y_data, CPC_Acc_score==TRUE)[,1])
       revenue_outliers
       
       y_data_1 <- subset(y_data, CPC_Acc_score==TRUE)
       y_data_2 <- subset(y_data, Click_Acc_score==TRUE)
       
       p= ggplot() +
         geom_line(data = data_1, aes(x = as.Date(date), y = cpc_acc, group=1), color="darkred" , show.legend = F) +
         # geom_point(data = y_data_1, aes(x = as.Date(date), y = cpc_acc, group=1, shape="circle"),size=5, color="darkred", show.legend = T)+
         xlab('Date') +
         ylab('cpc_acc') + facet_grid(. ~ portfolio_name)+
         scale_x_date(date_breaks = "1 week")+
         theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
               axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
         # geom_vline(data=y_data, mapping = aes(xintercept=as.Date(y_data[max(cpts_CP_1),1]), linetype="twodash"),size=1, color = "darkred", show.legend =T)+
         # geom_text(data=y_data, mapping=aes(x=as.Date(y_data[max(cpts_CP_1),1]), y=0, label=as.Date(y_data[max(cpts_CP_1),1] , sep = " ")), size=3, vjust=1, hjust=0, angle=90)+
         # scale_shape_manual(name =  " Legend-Shape", labels = c("circle"="outliers"), values = c("circle"))+
         # scale_linetype_manual(name =  "Legend-Line ", labels = c("twodash"="change_mean"), values = c("twodash"))+
         # guides(shape = guide_legend("Legend-Shape",override.aes = list(linetype = 0, color="darkred")), 
         #        linetype = guide_legend("Legend-Line",override.aes = list(shape = 0, color="darkred")))
       print(p)       
       
       #+ facet_grid(. ~ portfolio_name)
 
     
   })
   
   output$plot1 <- renderPlot({
    
       data_1 <- as.data.frame(passdata1()$df9)

       data_1 <- data_1 %>% select(portfolio_name, date, cost_acc, rpc_acc)

       y_data <- data_1 %>% select(date,cost_acc, rpc_acc)
       
       y_data[is.na(y_data)] <- 0

       library(changepoint)

       cptm_CP         <- cpt.mean(y_data$cost_acc, penalty='MBIC',pen.value=0, method='BinSeg',
                                  test.stat="Normal", minseglen=7, class=TRUE)
       cptm_CP

       plot(cptm_CP)

       cpts_CP         <- cpts(cptm_CP) # change point time points
       cpts_CP
       
       library(changepoint)
       
       cptm_CP_1         <- cpt.mean(y_data$rpc_acc, penalty='MBIC',pen.value=0, method='BinSeg',
                                   test.stat="Normal", minseglen=7, class=TRUE)
       cptm_CP_1
       
       plot(cptm_CP_1)
       
       cpts_CP_1         <- cpts(cptm_CP_1) # change point time points
       cpts_CP_1
       
       library("outliers")
       y_data$Cost_Acc_score <- scores(type="z", y_data$cost_acc, prob=.95)
       y_data$RPC_Acc_score <- scores(type="z", y_data$rpc_acc, prob=.95)
       cost_outliers <- list(subset(y_data, Cost_Acc_score==TRUE)[,1])
       cost_outliers
       revenue_outliers <- list(subset(y_data, RPC_Acc_score==TRUE)[,1])
       revenue_outliers
       
       y_data_1 <- subset(y_data, RPC_Acc_score==TRUE)
       y_data_2 <- subset(y_data, Cost_Acc_score==TRUE)

       p= ggplot() +
         geom_line(data = data_1, aes(x = as.Date(date), y = cost_acc, group=1), color="red" , show.legend = F) +
         geom_point(data = y_data_2, aes(x = as.Date(date), y = cost_acc, group=1, shape="circle"),size=5, color="red", show.legend = T)+
         xlab('Date') +
         ylab('cost_acc') + facet_grid(. ~ portfolio_name)+
         scale_x_date(date_breaks = "1 week")+
         geom_vline(data=y_data, mapping = aes(xintercept=as.Date(y_data[max(cpts_CP),1]), linetype="twodash"),size=1, color = "red", show.legend =T)+
         geom_hline(data=data_1, mapping = aes(yintercept=100, linetype="dotted"),size=1, color = "red", show.legend =T)+
         geom_text(data=y_data, mapping=aes(x=as.Date(y_data[max(cpts_CP),1]), y=0, label=as.Date(y_data[max(cpts_CP),1] , sep = " ")), size=3, vjust=1, hjust=0, angle=90)+
         scale_shape_manual(name =  " Legend-Shape", labels = c("circle"="outliers"), values = c("circle"))+
         scale_linetype_manual(name =  "Legend-Line ", labels = c("dotted"="100 %","twodash"="change_mean"), values = c("dotted","twodash"))+
         guides(shape = guide_legend("Legend-Shape",override.aes = list(linetype = 0, color="red")), 
                linetype = guide_legend("Legend-Line",override.aes = list(shape = 0, color="red")))+
         # theme_set(theme_gray(base_size = 18))+
         theme(axis.text.x = element_text(color = "grey20",size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
               axis.text.y = element_text(color = "grey20",size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"),
               axis.title.y = element_text(size = 15, angle = 90),
               axis.title.x = element_text(size = 15, angle = 00),
               strip.text.x = element_text(size = 15, colour = "black", angle = 00))
       print(p)       

  
  
   })
   
   output$plot3 <- renderPlot({
     
     
     data_1 <- as.data.frame(passdata1()$df1)
     
     
     data_3 <- as.data.frame(passdata1()$df4)
     
     
     data_3[is.na(data_3)] <- 0
     
     library(lubridate)
     data_3$date <- as.Date(data_3$date)
     data_3$dow <- lubridate::wday(data_3$date, label=TRUE)
     
     data_v3 <- data_3 %>% select(portfolio_name, dow, act_cost, act_rev, pred_cost, pred_rev)
     
     data_v4 <- aggregate(.~portfolio_name+dow,data_v3,sum)
     
     data_v4 <- data_v4 %>% mutate(act_roas=act_rev/act_cost)
     data_v4 <- data_v4 %>% mutate(pred_roas=pred_rev/pred_cost)
     data_v4 <- data_v4 %>% mutate(roas_acc=act_roas/pred_roas*100)
     
     data_v4 <- data_v4 %>% select(portfolio_name,act_roas,dow)
     admin_v6 <- melt(data_v4)
     p <- ggplot() +
       geom_bar(data=admin_v6, aes(x=dow, y=value),stat='identity', position='dodge2', color="darkcyan", fill="darkcyan")+ facet_grid(. ~ portfolio_name)+labs(x="day of week", y="actual_roas") + scale_fill_discrete(guide=FALSE)+geom_hline(data=data_v4, mapping = aes(yintercept=mean(act_roas), linetype="dotted"),size=1, color = "darkcyan", show.legend =T)+scale_linetype_manual(name =  "Legend-Line ", labels = c("dotted"="mean roas"), values = c("twodash"))+
       guides(linetype = guide_legend("Legend-Line",override.aes = list(shape = 0, color="darkcyan")))+
       theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
             axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
     
     print(p)
     
     
     
   })
   
   output$plotcost <- renderPlot({
     
     
     data_1 <- as.data.frame(passdata1()$df1)
     
     
     data_3 <- as.data.frame(passdata1()$df4)
     
     
     data_3[is.na(data_3)] <- 0
     
     library(lubridate)
     data_3$date <- as.Date(data_3$date)
     data_3$dow <- lubridate::wday(data_3$date, label=TRUE)
     
     data_v3 <- data_3 %>% select(portfolio_name, dow, act_cost, act_rev, pred_cost, pred_rev)
     
     data_v4 <- aggregate(.~portfolio_name+dow,data_v3,sum)
     
     # data_v4 <- data_v4 %>% mutate(act_roas=act_rev/act_cost)
     # data_v4 <- data_v4 %>% mutate(pred_roas=pred_rev/pred_cost)
     # data_v4 <- data_v4 %>% mutate(roas_acc=act_roas/pred_roas*100)
     
     data_v4 <- data_v4 %>% select(portfolio_name,act_cost,dow)
     admin_v6 <- melt(data_v4)
     p <- ggplot() +
       geom_bar(data=admin_v6, aes(x=dow, y=value),stat='identity', position='dodge2', color="red", fill="red")+ facet_grid(. ~ portfolio_name)+labs(x="day of week", y="actual_cost") + scale_fill_discrete(guide=FALSE)+
       theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
             axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
     
     print(p)
     
     
     
   })
   
   output$plotcostacc <- renderPlot({
     
     
     data_1 <- as.data.frame(passdata1()$df1)
     
     
     data_3 <- as.data.frame(passdata1()$df4)
     
     
     data_3[is.na(data_3)] <- 0
     
     library(lubridate)
     data_3$date <- as.Date(data_3$date)
     data_3$dow <- lubridate::wday(data_3$date, label=TRUE)
     
     data_v3 <- data_3 %>% select(portfolio_name, dow, act_cost, act_rev, pred_cost, pred_rev)
     
     data_v4 <- aggregate(.~portfolio_name+dow,data_v3,sum)
     
     # data_v4 <- data_v4 %>% mutate(act_roas=act_rev/act_cost)
     # data_v4 <- data_v4 %>% mutate(pred_roas=pred_rev/pred_cost)
     data_v4 <- data_v4 %>% mutate(cost_acc=act_cost/pred_cost*100)
     
     data_v4 <- data_v4 %>% select(portfolio_name,cost_acc,dow)
     admin_v6 <- melt(data_v4)
     p <- ggplot() +
       geom_bar(data=admin_v6, aes(x=dow, y=value),stat='identity', position='dodge2', color="red", fill="red")+ facet_grid(. ~ portfolio_name)+labs(x="day of week", y="cost_acc") + scale_fill_discrete(guide=FALSE)+
       theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
             axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
     print(p)
     
     
     
   })
   
   
   output$plotroasacc <- renderPlot({
     
     
     data_1 <- as.data.frame(passdata1()$df1)
     
     
     data_3 <- as.data.frame(passdata1()$df4)
     
     
     data_3[is.na(data_3)] <- 0
     
     library(lubridate)
     data_3$date <- as.Date(data_3$date)
     data_3$dow <- lubridate::wday(data_3$date, label=TRUE)
     
     data_v3 <- data_3 %>% select(portfolio_name, dow, act_cost, act_rev, pred_cost, pred_rev)
     
     data_v4 <- aggregate(.~portfolio_name+dow,data_v3,sum)
     
     data_v4 <- data_v4 %>% mutate(act_roas=act_rev/act_cost)
     data_v4 <- data_v4 %>% mutate(pred_roas=pred_rev/pred_cost)
     data_v4 <- data_v4 %>% mutate(roas_acc=act_cost/pred_cost*100)
     
     data_v4 <- data_v4 %>% select(portfolio_name,roas_acc,dow)
     admin_v6 <- melt(data_v4)
     p <- ggplot() +
       geom_bar(data=admin_v6, aes(x=dow, y=value),stat='identity', position='dodge2', color="darkcyan", fill="darkcyan")+ facet_grid(. ~ portfolio_name)+labs(x="day of week", y="roas_acc") + scale_fill_discrete(guide=FALSE)+geom_hline(data=data_v4, mapping = aes(yintercept=100, linetype="dotted"),size=1, color = "darkcyan", show.legend =T)+scale_linetype_manual(name =  "Legend-Line ", labels = c("dotted"="100 %"), values = c("twodash"))+
       guides(linetype = guide_legend("Legend-Line",override.aes = list(shape = 0, color="darkcyan")))+
       theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
             axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
     
     print(p)
     
     
     
   })
   
   

   output$text8 <- renderText({
   
       
       data_1 <- as.data.frame(passdata1()$df1)

       
       data_3 <- as.data.frame(passdata1()$df4)

       
       data_3[is.na(data_3)] <- 0
       
       library(lubridate)
       data_3$date <- as.Date(data_3$date)
       data_3$dow <- lubridate::wday(data_3$date, label=TRUE)
       
       data_v3 <- data_3 %>% select(portfolio_name, dow, act_cost, act_rev, pred_cost, pred_rev)
       
       data_v4 <- aggregate(.~portfolio_name+dow,data_v3,sum)
       
       data_v4 <- data_v4 %>% mutate(act_roas=act_rev/act_cost)
       data_v4 <- data_v4 %>% mutate(pred_roas=pred_rev/pred_cost)
       data_v4 <- data_v4 %>% mutate(roas_acc=act_roas/pred_roas*100)
       
       data_v4$Variance_di <- var(data_v4$act_roas)
       data_v4$avg_di <- mean(data_v4$act_roas)
       data_v4$dispersion_index <- data_v4$Variance_di/data_v4$avg_di
       
       data_v4$Variance_er <- var(data_v4$roas_acc)
       data_v4$avg_er <- mean(data_v4$roas_acc)
       data_v4$error_rate <- data_v4$Variance_er/data_v4$avg_er
       
       data_v4[ data_v4 == "NaN" ] <- NA
       
       
       data_v4[is.na(data_v4)] <- 0
       
       
       di <- unique(data_v4$dispersion_index)
       er <- unique(data_v4$error_rate)
       
       data_1$di <- di
       data_1$er <- er
       
       paste("DOW Dispersion Index:", round(data_1$di, digits = 3))
   
     
   })
   
   output$text12 <- renderText({


       paste("Enable_DOW recommendation could mean any of the following :")
  
     
   })
   
   output$textintra <- renderText({
     
     
     paste("Enabling Intraday has the highest impact when campaigns capping is enabled.")
     
     
   })
   
   output$intro <- renderText({
     
     
     paste("This app aims at diagnosing SEM performance from data modeling stand point and recommending optimal portfolio settings.")
    
     
   })
   
   
   output$text9 <- renderText({

       
       data_1 <- as.data.frame(passdata1()$df1)

       
       data_3 <- as.data.frame(passdata1()$df4)
    
       
       
       data_3[is.na(data_3)] <- 0
       
       library(lubridate)
       data_3$date <- as.Date(data_3$date)
       data_3$dow <- lubridate::wday(data_3$date, label=TRUE)
       
       data_v3 <- data_3 %>% select(portfolio_name, dow, act_cost, act_rev, pred_cost, pred_rev)
       
       data_v4 <- aggregate(.~portfolio_name+dow,data_v3,sum)
       
       data_v4 <- data_v4 %>% mutate(act_roas=act_rev/act_cost)
       data_v4 <- data_v4 %>% mutate(pred_roas=pred_rev/pred_cost)
       data_v4 <- data_v4 %>% mutate(roas_acc=act_roas/pred_roas*100)
       
       data_v4$Variance_di <- var(data_v4$act_roas)
       data_v4$avg_di <- mean(data_v4$act_roas)
       data_v4$dispersion_index <- data_v4$Variance_di/data_v4$avg_di
       
       data_v4$Variance_er <- var(data_v4$roas_acc)
       data_v4$avg_er <- mean(data_v4$roas_acc)
       data_v4$error_rate <- data_v4$Variance_er/data_v4$avg_er
       
       data_v4[ data_v4 == "NaN" ] <- NA
      
       
       data_v4[is.na(data_v4)] <- 0
       
       di <- unique(data_v4$dispersion_index)
       er <- unique(data_v4$error_rate)
       
       data_1$di <- di
       data_1$er <- er
       
       paste("DOW Error Rate:", round(data_1$er, digits = 3))
   
     
   })
   
   
   output$plot2 <- renderPlot({
 
       data_1 <- as.data.frame(passdata1()$df3)
   
       data_1 <- data_1 %>% select(portfolio_name, date, cost_acc, rpc_acc)
       
       y_data <- data_1 %>% select(date,cost_acc, rpc_acc)
       
       y_data <- y_data %>%arrange(desc(date))
       
       y_data <- tail(y_data, -7)
       
       y_data[is.na(y_data)] <- 0
       
       # library(changepoint)
       # 
       # cptm_CP         <- cpt.mean(y_data$cost_acc, penalty='MBIC',pen.value=0, method='BinSeg',
       #                             test.stat="Normal", minseglen=7, class=TRUE)
       # cptm_CP
       # 
       # plot(cptm_CP)
       # 
       # cpts_CP         <- cpts(cptm_CP) # change point time points
       # cpts_CP
       
       library(changepoint)
       
       cptm_CP_1         <- cpt.mean(y_data$rpc_acc, penalty='MBIC',pen.value=0, method='BinSeg',
                                     test.stat="Normal", minseglen=7, class=TRUE)
       cptm_CP_1
       
       plot(cptm_CP_1)
       
       cpts_CP_1         <- cpts(cptm_CP_1) # change point time points
       cpts_CP_1
       
       library("outliers")
       y_data$Cost_Acc_score <- scores(type="z", y_data$cost_acc, prob=.95)
       y_data$RPC_Acc_score <- scores(type="z", y_data$rpc_acc, prob=.95)
       cost_outliers <- list(subset(y_data, Cost_Acc_score==TRUE)[,1])
       cost_outliers
       revenue_outliers <- list(subset(y_data, RPC_Acc_score==TRUE)[,1])
       revenue_outliers
       
       y_data_1 <- subset(y_data, RPC_Acc_score==TRUE)
       y_data_2 <- subset(y_data, Cost_Acc_score==TRUE)
       
       p= ggplot() +
         geom_line(data = data_1, aes(x = as.Date(date), y = rpc_acc, group=1), color="darkcyan" , show.legend = F) +
         geom_point(data = y_data_1, aes(x = as.Date(date), y = rpc_acc, group=1, shape="circle"),size=5, color="darkcyan", show.legend = T)+
         xlab('Date') +
         ylab('rpc_acc') + facet_grid(. ~ portfolio_name)+
         scale_x_date(date_breaks = "1 week")+
         geom_vline(data=y_data, mapping = aes(xintercept=as.Date(y_data[max(cpts_CP_1),1]), linetype="twodash"),size=1, color = "darkcyan", show.legend =T)+
         geom_hline(data=data_1, mapping = aes(yintercept=100, linetype="dotted"),size=1, color = "darkcyan", show.legend =T)+
         geom_text(data=y_data, mapping=aes(x=as.Date(y_data[max(cpts_CP_1),1]), y=0, label=as.Date(y_data[max(cpts_CP_1),1] , sep = " ")), size=3, vjust=1, hjust=0, angle=90)+
         scale_shape_manual(name =  " Legend-Shape", labels = c("circle"="outliers"), values = c("circle"))+
         scale_linetype_manual(name =  "Legend-Line ", labels = c("dotted"="100 %","twodash"="change_mean"), values = c("dotted","twodash"))+
          guides(shape = guide_legend("Legend-Shape",override.aes = list(linetype = 0, color="darkcyan")), 
                linetype = guide_legend("Legend-Line",override.aes = list(shape = 0, color="darkcyan"))) +
         theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 45, hjust = .5, vjust = .5, face = "plain"),
                 axis.text.y = element_text(color = "grey20", size = 15, angle = 0, hjust = 1, vjust = 0, face = "plain"))
               
      
       print(p)       
       
 
     
   })
   
   output$text3 <- renderText({
   
       data_1 <- as.data.frame(passdata1()$df3)

       data_1 <- data_1 %>% select(portfolio_name, date, cost_acc, rpc_acc)
       
       y_data <- data_1 %>% select(date,cost_acc, rpc_acc)
       
       y_data <- y_data %>%arrange(desc(date))
       
       y_data <- tail(y_data, -7)
       
       y_data[is.na(y_data)] <- 0
       
       library("outliers")
       y_data$Cost_Acc_score <- scores(type="z", y_data$cost_acc, prob=.95)
       y_data$RPC_Acc_score <- scores(type="z", y_data$rpc_acc, prob=.95)
       cost_outliers <- list(subset(y_data, Cost_Acc_score==TRUE)[,1])
       cost_outliers
       revenue_outliers <- list(subset(y_data, RPC_Acc_score==TRUE)[,1])
       revenue_outliers
       
       paste(c("Recommendation -- exclude the following dates from the revenue model:", unlist(revenue_outliers)), collapse = " ")     
       

     
   })
   
   output$rpcdiag <- renderText({
     
     data_1 <- as.data.frame(passdata1()$df3)
     
     data_1 <- data_1 %>% select(portfolio_name, date, cost_acc, rpc_acc)
     
     y_data <- data_1 %>% select(date,cost_acc, rpc_acc)
     
     y_data <- y_data %>%arrange(desc(date))
     
     y_data <- tail(y_data, -7)
     
     y_data[is.na(y_data)] <- 0
     
     library("outliers")
     y_data$Cost_Acc_score <- scores(type="z", y_data$cost_acc, prob=.95)
     y_data$RPC_Acc_score <- scores(type="z", y_data$rpc_acc, prob=.95)
     cost_outliers <- list(subset(y_data, Cost_Acc_score==TRUE)[,1])
     cost_outliers
     revenue_outliers <- list(subset(y_data, RPC_Acc_score==TRUE)[,1])
     revenue_outliers
     
     d1 <- as.data.frame(count(unlist(revenue_outliers)))  
     
     sf <- sum(d1$freq)
     
     paste(c("There are", sf, "outliers in the RPC accuracy"), collapse = " ")     
     
     
     
   })
   
   output$costdiag <- renderText({
     
     data_1 <- as.data.frame(passdata1()$df9)
     
     data_1 <- data_1 %>% select(portfolio_name, date, cost_acc, rpc_acc)
     
     y_data <- data_1 %>% select(date,cost_acc, rpc_acc)
     
     y_data[is.na(y_data)] <- 0
     
     library("outliers")
     y_data$Cost_Acc_score <- scores(type="z", y_data$cost_acc, prob=.95)
     y_data$RPC_Acc_score <- scores(type="z", y_data$rpc_acc, prob=.95)
     cost_outliers <- list(subset(y_data, Cost_Acc_score==TRUE)[,1])
     cost_outliers
     revenue_outliers <- list(subset(y_data, RPC_Acc_score==TRUE)[,1])
     revenue_outliers
     
     d1 <- as.data.frame(count(unlist(cost_outliers)))  
     
     sf <- sum(d1$freq)
     
     paste(c("There are", sf,"outliers in the Cost Accuracy"), collapse = " ")     
     
     
     
   })
   
   output$text4 <- renderText({

       data_1 <- as.data.frame(passdata1()$df9)
  
       data_1 <- data_1 %>% select(portfolio_name, date, cost_acc, rpc_acc)
       
       y_data <- data_1 %>% select(date,cost_acc, rpc_acc)
       
       y_data[is.na(y_data)] <- 0
       
       library("outliers")
       y_data$Cost_Acc_score <- scores(type="z", y_data$cost_acc, prob=.95)
       y_data$RPC_Acc_score <- scores(type="z", y_data$rpc_acc, prob=.95)
       cost_outliers <- list(subset(y_data, Cost_Acc_score==TRUE)[,1])
       cost_outliers
       revenue_outliers <- list(subset(y_data, RPC_Acc_score==TRUE)[,1])
       revenue_outliers
       
       paste(c("Recommendation -- exclude the following dates from the cost model:", unlist(cost_outliers)), collapse = " ")     
       

     
   })
   
   output$text5 <- renderText({
   
       data_1 <- as.data.frame(passdata1()$df1)
 
       
       paste("Click Coverage:", data_1$click_coverage)     
       

     
   })
   
   output$text6 <- renderText({
 
       data_1 <- as.data.frame(passdata1()$df1)
    
       
       paste("Revenue Coverage:", data_1$revenue_coverage)     
       

     
   })
   
   output$text7 <- renderText({
 
       data_1 <- as.data.frame(passdata1()$df1)
  
       
       paste("Spend Strategy:", data_1$strategy)     
       

     
   })
   
   output$text1 <- renderText({
  
       data_1 <- as.data.frame(passdata1()$df9)
    
       data_1 <- data_1 %>% select(date,cost_acc)
       
       data_1[is.na(data_1)] <- 0
       
       # y_data <- data_1 %>% select(date,cost_acc)

       library(changepoint)

       cptm_CP <- cpt.mean(data_1$cost_acc, penalty='MBIC',pen.value=0, method='BinSeg',
                                  test.stat="Normal", minseglen=7, class=TRUE)
       cptm_CP

       plot(cptm_CP)

       cpts_CP <- cpts(cptm_CP) # change point time points
       cpts_CP
       
       
       #ggplot(data_1, aes(x=date, y=value, color = variable, group=1)) +geom_line() + facet_grid(. ~ portfolio_name)
       
       print( paste("Diagnostic -- latest change in mean of cost accuracy occured on", data_1[max(cpts_CP),1] , "that is", Sys.Date()-(as.Date(data_1[max(cpts_CP),1])), "days ago." ))
       
 

   })
   
   
   output$text2 <- renderText({
 
       data_1 <- as.data.frame(passdata1()$df3)
   
       data_1 <- data_1 %>% select(date,rpc_acc)
       
       data_1 <- data_1 %>%arrange(desc(date))
       
       data_1 <- tail(data_1, -7)
       
       data_1[is.na(data_1)] <- 0
       
       # y_data <- data_1 %>% select(date,cost_acc)
       
       library(changepoint)
       
       cptm_CP <- cpt.mean(data_1$rpc_acc, penalty='MBIC',pen.value=0, method='BinSeg',
                           test.stat="Normal", minseglen=7, class=TRUE)
       cptm_CP
       
       plot(cptm_CP)
       
       cpts_CP <- cpts(cptm_CP) # change point time points
       cpts_CP
       
       
       #ggplot(data_1, aes(x=date, y=value, color = variable, group=1)) +geom_line() + facet_grid(. ~ portfolio_name)
       
       print( paste("Diagnostic -- latest change in mean of RPC accuracy occured on", data_1[max(cpts_CP),1] , "that is", Sys.Date()-(as.Date(data_1[max(cpts_CP),1])), "days ago." ))

   
  })
  
  output$selected_var_2 <- renderText({ 
    paste("You have selected the Portfolio Name and PID : ", input$PortfolioNamePID)
  })
  
  output$contact<- renderText({ 
    paste("For any questions please reach out to:")
  })
  
  output$diag<- renderText({ 
    paste("Instructions and Descriptions per tab:")
  })
  
  output$gloss<- renderText({ 
    paste("The ways in which we came up with the recommendations along with a comprehensive understanding of the different terminologies mentioned in the dashboard is given in the link below:")
  })
  
  output$dq<- renderText({ 
    paste("Data Quality validated. JIRA Ticket Number:")
  })
  
  output$dq1<- renderText({ 
    paste("Data Quality validated. JIRA Ticket Number:")
  })
  
  output$dq12<- renderText({ 
    paste("Data Quality  validated. JIRA Ticket Numbers:")
  })
  
  output$nt<- renderText({ 
    paste("Data Quality has been validated for daily accuracy data. Dimensional data is yet to be validated. JIRA tickets have been raised for the same")
  })
  

}

ui <- dashboardPage( 
  
  dashboardHeader(
    title="Search Performance App"
    # tags$li(class="dropdown",
    #         tags$img(src="frontier32x32.png"),"Search Diagnostics")
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Instructions",tabName = "Instructions"),
      menuItem("Account_Diagnostics",tabName = "Account_Diagnostics"),
      menuItem("Portfolio_Recommendations",tabName = "Portfolio_Recommendations"),
      menuItem("Daily_Accuracy",tabName = "Daily_Accuracy"),
      menuItem("DOW_Modeling",tabName = "DOW_Modeling"),
      menuItem("Match_Type",tabName = "Match_Type"),
      menuItem("Click_Level",tabName = "Click_Level"),
      menuItem("Device_Level",tabName = "Device_Level")
    )
  ),
  
  dashboardBody(
    
  tags$head(tags$style(HTML(' .main-sidebar{ width: 250px; } .main-header > .navbar { margin-left: 300px; } .main-header .logo { width: 300px; } .content-wrapper, .main-footer, .right-side { margin-left: 300px; } .box.box-solid.box-primary>.box-header {background:navy} .box.box-solid.box-info>.box-header {background:teal} .box.box-solid.box-warning>.box-header {background:purple}'))),
    
    tabItems( tabItem(tabName = "Instructions",
                      fluidRow(box(width=12, title="ABOUT THE DASHBOARD", status="info", solidHeader = TRUE, textOutput('intro'), tags$head(tags$style("#intro{color: black;
                                 font-size: 20px;}"
                      ))),
                               box(width=12, title="WORKFLOW", status="info", solidHeader = TRUE, textOutput('diag'),  tags$head(
                                 tags$style("#diag{color: blue;
                                            font-size: 20px;font-style: italic;}", HTML("
                                                                                        li {
                                                                                        font-size: 10px;
                                                                                        
                                                                                        }
                                                                                        li span {
                                                                                        font-size: 18px;
                                                                                        }
                                                                                        ul {
                                                                                        list-style-type: square;
                                                                                        }
                                                                                        
                                                                                        "))
                                 ),
                                 
                                 tags$div(tags$ul(
                                   tags$li(tags$span("Account_Diagnostics - Pick a client, db_tag, data center and submit. That will return cross portfolio diagnostic on the basis of cost and rpc accuracy. The portfolios are sorted by predicted cost")),
                                   tags$li(tags$span("Portfolio_Recommendations - Pick a specific portfolio and submit. That will return two tables. The first table would be providing recommendations for portfolio settings in the UI. The second table would be providing details of the dispersion index and error rate per dimension. For more details on the terminologies please see the glossary section below")),
                                   tags$li(tags$span("Daily_Accuracy - Based on the portfolio selected in the previous tab, this tab would help delving deeper into the half life recommendations of the previous tab. Also, it would provide recommendations for outlier removals.For more details on the terminologies please see the glossary section below ")),
                                   tags$li(tags$span("Match_Type, Device_level, Click_Level, DOW_Modeling - These are the different dimensions that we will be looking at for each portfolio selected. The visualizations, tables and numerics in each of these tabs will help comprehending what is happening in each dimension and take bettter action using some of the recommendations provided in the previous tab. "))
                                 ))),
                         box(width=12, title="DECISION TREE", status="info", solidHeader = TRUE, tags$li(class="dropdown",
                                 tags$img(src="Searchdiagnostics.png", height="100%", width="100%",align="center"))),
                      
                      box(width=12, title="GLOSSARY", status="info", solidHeader = TRUE, textOutput('gloss'),  tags$head(
                        tags$style("#gloss{color: blue;
                                            font-size: 20px;font-style: italic;}", HTML("
                                                                                        li {
                                                                                        font-size: 10px;
                                                                                        
                                                                                        }
                                                                                        li span {
                                                                                        font-size: 18px;
                                                                                        }
                                                                                        ul {
                                                                                        list-style-type: square;
                                                                                        }
                                                                                        
                                                                                        "))
                      ),
                      
                      tags$a(href='Glossary.pdf', target='blank', 'CLICK HERE', download = 'Glossary.pdf')
                      ),
              
                         box(width=12, title="CONTACT INFO", status="info", solidHeader = TRUE, textOutput('contact'),  tags$head(
                           tags$style("#contact{color: blue;
                                      font-size: 20px;font-style: italic;}", HTML("
                                                                                  li {
                                                                                  font-size: 10px;
                                                                                  
                                                                                  }
                                                                                  li span {
                                                                                  font-size: 18px;
                                                                                  }
                                                                                  ul {
                                                                                  list-style-type: square;
                                                                                  }
                                                                                  
                                                                                  "))
                           ),
                           
                           tags$div(tags$ul(
                             tags$li(tags$span("Laavanya Ganesh: lganesh@adobe.com")),
                             tags$li(tags$span("Benjamin Vigneron: vigneron@adobe.com")),
                             tags$li(tags$span("Alex Lambrakis: alambrak@adobe.com"))
                             ))
          
                                      ))
                      ),
                 
             tabItem(tabName = "Account_Diagnostics",
                         fluidRow(
                           box(title = "PICK A CLIENT, DB TAG, DATA CENTER AND SUBMIT",status="primary",color="navy",solidHeader = TRUE,
                               #actionButton("do", "Load active usernames"),
                               uiOutput("choose_dataset_1"),
                               uiOutput("choose_dataset"),
                               actionButton("goButton", "Submit")),
                           box(title= "DATE RANGE for DIAGNOSTICS",width=12, status="info",solidHeader = TRUE, textOutput('daterange1'), tags$head(tags$style("#daterange1{color: black;
                                 font-size: 20px;font-style: italic;}"
                           )
                           )),
                           box(title= "DIAGNOSTICS FOR DIFFERENT PORTFOLIOS WITHIN THE ACCOUNT",width=12, status="success",solidHeader = TRUE, DT::dataTableOutput('tbl_1')
                          ),
                          box(title= "NOTE",width=12, status="info",solidHeader = TRUE, textOutput('dq'), tags$head(tags$style("#dq{color: green;
                                 font-size: 20px;font-style: italic;}"
                          )
                          ),
                          
                          tags$a(href='https://jira.corp.adobe.com/browse/AMO-130089', target='_blank', 'AMO-130089')
                          )
                          # tags$a(href='https://jira.corp.adobe.com/browse/AMO-130089', 'AMO-130089', , target="_blank"'))
                          # downloadButton("downloadData", "Download")
                 ) ),
             
             tabItem(tabName = "Portfolio_Recommendations",
                     fluidRow(
                       box(width=12, textOutput('selected_var'), tags$head(tags$style("#selected_var{color: red;
                                 font-size: 20px;font-style: italic;}"
                       ))  
                         ),
                         box(title = "PICK A PORTFOLIO AND SUBMIT", status="primary", solidHeader = TRUE,
                             uiOutput("choose_dataset_2"),
                             actionButton("go", "Submit")),
                       box(title = "OVERVIEW", status="info",solidHeader = TRUE,width=12, textOutput('selected_var_1'), tags$head(tags$style("#selected_var_1{color: blue;
                                 font-size: 20px;font-style: italic;}"
                       )),  textOutput('selected_var_3'), tags$head(tags$style("#selected_var_3{color: blue;
                                 font-size: 20px;font-style: italic;}"
                       )),textOutput('selected_var_4'), tags$head(tags$style("#selected_var_4{color: blue;
                                 font-size: 20px;font-style: italic;}"
                       ))   
                       ),
                       box(title= "DATE RANGE for RECOMMENDATIONS",width=12, status="info",solidHeader = TRUE, textOutput('daterange2'), tags$head(tags$style("#daterange2{color: black;
                                 font-size: 20px;font-style: italic;}"
                       )
                       )),
                       # box(width=12, textOutput('selected_var_3'), tags$head(tags$style("#selected_var_3{color: blue;
                       #           font-size: 20px;font-style: italic;}"
                       # )),textOutput('selected_var_4'), tags$head(tags$style("#selected_var_4{color: blue;
                       #           font-size: 20px;font-style: italic;}"
                       # ))  
                       # ),
                       # box(width=12, textOutput('selected_var_4'), tags$head(tags$style("#selected_var_4{color: blue;
                       #           font-size: 20px;font-style: italic;}"
                       # ))  
                       # ),
                       # box(width=12, DT::dataTableOutput('admin_reco')
                       # ),
                       box(title= "PORTFOLIO RECOMMENDATIONS FOR UI SETTINGS",status="success",solidHeader = TRUE,width=12, DT::dataTableOutput('admin_reco_2')
                       ),
                       box(title="NOTE", status="info",solidHeader = TRUE,width=12, textOutput('text12'),  tags$head(
                         tags$style("#text12{color: blue;
                                 font-size: 20px;font-style: italic;}", HTML("
                                         li {
                                         font-size: 10px;
                                         
                                         }
                                         li span {
                                         font-size: 18px;
                                         }
                                         ul {
                                         list-style-type: square;
                                         }
                                         
                                         "))
                         ),
                         
                         tags$div(tags$ul(
                           tags$li(tags$span("Enable Weekly Spend Strategy")),
                           tags$li(tags$span("Enable Day of Week Spend Strategy with Day of Week Models"))))
                           # tags$li(tags$span("Enable Day of Week Spend Strategy without Day of Week Models"))))  
                       ),
                       box(title= "NOTE",width=12, status="info",solidHeader = TRUE, textOutput('textintra'), tags$head(tags$style("#textintra{color: blue;
                                 font-size: 20px;font-style: italic;}"
                       )
                       )),
                       box(title= "NOTE",width=12, status="info",solidHeader = TRUE, textOutput('dq1'), tags$head(tags$style("#dq1{color: green;
                                 font-size: 20px;font-style: italic;}"
                       )
                       ),
                       
                       tags$a(href='https://jira.corp.adobe.com/browse/AMO-130089', target='_blank', 'AMO-130089')
                       # tags$a(href='https://jira.corp.adobe.com/browse/AMO-130110', target='_blank', 'AMO-130110'),
                       # tags$a(href='https://jira.corp.adobe.com/browse/AMO-130112', target='_blank', 'AMO-130112')
                       ),
                       box(title= "DIMENSION OPPORTUNITIES AND ERROR RATES",status="success",solidHeader = TRUE, width=12, DT::dataTableOutput('dimdt')
                       )
                       )
                      ),
             tabItem(tabName = "Daily_Accuracy",
                     fluidRow(
                       box(width=12, textOutput('selected_var_2'), tags$head(tags$style("#selected_var_2{color: blue;
                                                                                      font-size: 20px;font-style: italic;}"
                       ))  
                       ),
                       box(width=12, status = "warning", solidHeader = TRUE, title = "COST ACCURACY",  plotOutput("plot1"),
                       textOutput('text1'), tags$head(tags$style("#text1{color: red;
                                                                                      font-size: 20px;font-style: italic;}"
                       )),textOutput('costdiag'), tags$head(tags$style("#costdiag{color: red;
                                                                                      font-size: 20px;font-style: italic;}"
                       ))  , textOutput('text5'), tags$head(tags$style("#text4{color: red;
                                                                                      font-size: 20px;font-style: italic;}"
                       ))  ,textOutput('text4'), tags$head(tags$style("#text5{color: red;
                                                                                      font-size: 20px;font-style: italic;}"
                       ))  
                       
                       ),
                       # box(width=12, textOutput('text4'), tags$head(tags$style("#text4{color: red;
                       #                                                                font-size: 20px;font-style: italic;}"
                       # ))  
                       # ),
                       # box(width=12, textOutput('text5'), tags$head(tags$style("#text5{color: red;
                       #                                                                font-size: 20px;font-style: italic;}"
                       # ))  
                       # ),
        
                       box(width=12, status = "warning", solidHeader = TRUE, title = "RPC ACCURACY", plotOutput("plot2"),
                        textOutput('text2'), tags$head(tags$style("#text2{color: darkcyan;
                                                                                      font-size: 20px;font-style: italic;}"
                       ))  ,
                       textOutput('rpcdiag'), tags$head(tags$style("#rpcdiag{color: darkcyan;
                                                                   font-size: 20px;font-style: italic;}"
                       ))   ,
                       textOutput('text6'), tags$head(tags$style("#text3{color: darkcyan;
                                                                                      font-size: 20px;font-style: italic;}"
                       ))  ,
                       textOutput('text3'), tags$head(tags$style("#text6{color: darkcyan;
                                                                                      font-size: 20px;font-style: italic;}"
                       )) 
                       ),
                       box(width=12, status = "warning", solidHeader = TRUE, title = "EXTRA PLOTS"),
                       box(width=6, title = "CLICK ACCURACY", plotOutput("plot4")),
                       box(width=6, title = "CPC ACCURACY", plotOutput("plot5"))
                       # box(width=12, textOutput('text3'), tags$head(tags$style("#text3{color: darkcyan;
                       #                                                                font-size: 20px;font-style: italic;}"
                       # ))  
                       # ),
                       # box(width=12, textOutput('text6'), tags$head(tags$style("#text6{color: darkcyan;
                       #                                                                font-size: 20px;font-style: italic;}"
                       # ))  
                       # )
                       )
             ),
             tabItem(tabName = "DOW_Modeling",
                     fluidRow(
                       box(width=12, textOutput('text7'), tags$head(tags$style("#text7{color: blue;
                                                                                        font-size: 20px;font-style: italic;}"
                       ))  
                       ),
                       box(width=6, status = "warning", solidHeader = TRUE, title = "DAY OF WEEK ACTUAL COST ", plotOutput("plotcost")),
                       box(width=6, status = "warning", solidHeader = TRUE, title = "DAY OF WEEK ACTUAL ROAS", plotOutput("plot3")),
                       box(width=6, status = "warning", solidHeader = TRUE, title = "DAY OF WEEK COST ACCURACY", plotOutput("plotcostacc")),
                       box(width=6, status = "warning", solidHeader = TRUE, title = "DAY OF WEEK ROAS ACCURACY", plotOutput("plotroasacc")),
                       box(title= "DATA ACROSS DAY OF WEEK",status="success",solidHeader = TRUE,width=12, DT::dataTableOutput('dowTable')
                       ),
                       
                       box(title = "OVERVIEW", status="info",solidHeader = TRUE,width=12, textOutput('text8'), tags$head(tags$style("#text8{color: green;
                                                                                      font-size: 20px;font-style: italic;}"                       )),
                                   textOutput('text9'), tags$head(tags$style("#text9{color: green;
                                                                                      font-size: 20px;font-style: italic;}"
                                                                               ))

                       )
                       # box(width=12, textOutput('text9'), tags$head(tags$style("#text9{color: green;
                       #                                                                font-size: 20px;font-style: italic;}"
                       # ))  
                       # )
                       )
                       ),
             tabItem(tabName = "Match_Type",
                     fluidRow(
                       box(width=12, textOutput('topmatch'), tags$head(tags$style("#topmatch{color: blue;
                                                                                        font-size: 20px;font-style: italic;}"
                       ))  
                       ),
                       # box(width=12, plotlyOutput("plotMT")),
                       box(title= "DATA ACROSS MATCH TYPES",status="success",solidHeader = TRUE,width=12, DT::dataTableOutput('admin_MT')
                       ),
                       box(width=6, status = "warning", solidHeader = TRUE, title = "MATCH TYPE ACTUAL COST ", plotOutput("MTplot2")),
                       box(width=6, status = "warning", solidHeader = TRUE, title = "MATCH TYPE ACTUAL ROAS", plotOutput("MTplot")),
                       box(width=6, status = "warning", solidHeader = TRUE, title = "MATCH TYPE COST ACCURACY", plotOutput("MTplot3")),
                       box(width=6, status = "warning", solidHeader = TRUE, title = "MATCH TYPE ROAS ACCURACY", plotOutput("MTplot1")),
                       box(title = "OVERVIEW", status="info",solidHeader = TRUE,width=12, textOutput('textmtdi'), tags$head(tags$style("#textmtdi{color: blue;
                                                                                        font-size: 20px;font-style: italic;}"                       ))  ,
                                          textOutput('textmter'), tags$head(tags$style("#textmter{color: blue;
                                                                                        font-size: 20px;font-style: italic;}"
                                                                                                                                    ))
                   
                       )
                       # box(width=12, textOutput('textmter'), tags$head(tags$style("#textmter{color: blue;
                       #                                                                  font-size: 20px;font-style: italic;}"
                       # ))  
                       # )
                       
                     )
             ),
             tabItem(tabName = "Device_Level",
                     fluidRow(
                       box(width=12, textOutput('topdevice'), tags$head(tags$style("#topdevice{color: blue;
                                                                                  font-size: 20px;font-style: italic;}"
                       ))  
                       ),
                       # box(width=12, plotlyOutput("plotdev")),
                       box(title= "DATA ACROSS DEVICES",status="success",solidHeader = TRUE,width=12, DT::dataTableOutput('admin_dev')
                       ),
                       box(width=6, status = "warning", solidHeader = TRUE, title = "DEVICE ACTUAL COST ", plotOutput("devplot2")),
                       box(width=6, status = "warning", solidHeader = TRUE, title = "DEVICE ACTUAL ROAS", plotOutput("devplot")),
                       box(width=6, status = "warning", solidHeader = TRUE, title = "DEVICE COST ACCURACY", plotOutput("devplot3")),
                       box(width=6, status = "warning", solidHeader = TRUE, title = "DEVICE ROAS ACCURACY", plotOutput("devplot1")),
                       box(title = "OVERVIEW", status="info",solidHeader = TRUE,width=12, textOutput('textdevdi'), tags$head(tags$style("#textdevdi{color: blue;
                                                                                  font-size: 20px;font-style: italic;}"                       ))  ,
                                         textOutput('textdever'), tags$head(tags$style("#textdever{color: blue;
                                                                                  font-size: 20px;font-style: italic;}" ))
                 
                       )
                       # box(width=12, textOutput('textdever'), tags$head(tags$style("#textdever{color: blue;
                       #                                                            font-size: 20px;font-style: italic;}"
                       # ))  
                       # )
                       
                       )
                       ),
             tabItem(tabName = "Click_Level",
                     fluidRow(
                       box(width=12, textOutput('textcl'), tags$head(tags$style("#textcl{color: blue;
                                                                                  font-size: 20px;font-style: italic;}"
                       ))  
                       ),
                       box(title= "DATA ACROSS CLICK LEVELS",status="success",solidHeader = TRUE,width=12, DT::dataTableOutput('admin_CL')
                       ),
                       box(width=6, status = "warning", solidHeader = TRUE, title = "CLICKLEVEL ACTUAL COST ", plotOutput("CLplot2")),
                       box(width=6, status = "warning", solidHeader = TRUE, title = "CLICKLEVEL ACTUAL ROAS", plotOutput("CLplot")),
                       box(width=6, status = "warning", solidHeader = TRUE, title = "CLICKLEVEL COST ACCURACY", plotOutput("CLplot3")),
                       box(width=6, status = "warning", solidHeader = TRUE, title = "CLICKLEVEL ROAS ACCURACY", plotOutput("CLplot1")),
                       box(title = "OVERVIEW", status="info",solidHeader = TRUE,width=12, textOutput('textcldi'), tags$head(tags$style("#textcldi{color: blue;
                                                                                  font-size: 20px;font-style: italic;}"                       ))  ,
                                                                                  
                           textOutput('textcler'), tags$head(tags$style("#textcler{color: blue;
                                                                                  font-size: 20px;font-style: italic;}"
                                                                                  ))
                     
                       )
                       # box(width=12, textOutput('textcler'), tags$head(tags$style("#textcler{color: blue;
                       #                                                            font-size: 20px;font-style: italic;}"
                       # ))  
                       # )
                       )
                       )
    )
  )
)

shinyApp(ui = ui, server = server)

