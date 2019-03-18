# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#  Copyright 2016-2019 University of Melbourne
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
# Written by: Dr. Yiqun Chen    yiqun.c@unimelb.edu.au
# 
# 
# DevLogs:
# This is my solution for SGS enrolment simulation application (ESA), based on their paper http://www.sgsep.com.au/application/files/5415/0050/9125/Revised-Deng.pdf
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(maptools) 
library(rgdal)
library(rgeos)
library(jsonlite)
library(lpSolve)
library(lpSolveAPI)
library(ggplot2)
library(reshape)
library(gridExtra)

library(doParallel)

# using 4 cores for parallel computing
registerDoParallel(cores=4) 

# change working directory to your own dir path where the r-geoserver.zip is unzipped to
setwd("E:\\05_MyProjects\\enrollment_simulation\\SourceCodeRepos\\r-scp")

# load utils methods for use
source("utils.R")


# case area, em for east melbourne, wh for whitehorse
gCaseArea <<- 'wh'

gColumnNameSchoolCapacity <<- 'capacity'
gColumnNameStudentPopulation <<- 'y7_y12_num'
gColumnNameMeshBlockCode <<-"MB_CODE16"

#
#' using greedy algorithm to calcuate Enrolment Simulation Application using mockup school and population datasets
#' global optimal is NEVER assured.
#'
#' @param cap_factor 			school capacity factor, in range (0,5), default 1.0 means 100% capacities are used for calculation 
#' @param pop_factor 			student population factor, in range (0,5), default 1.0 means 100% student population are used for calculation 
#' @param school_dist_threshold 	distance threshold (in meter) that stops the allocation process, default 99999999 (set this to a very big number e.g. 9999999 to bypass this allocation criterion)
#'
#' @return shpfiles and statistics for the student allocation details and schools
#' @export
#'
#' @examples
exec_greedy <- function(cap_factor=1.0, pop_factor=1.0, school_dist_threshold=99999999){
  
  generalStartTime = Sys.time()
  
  if (cap_factor<=0 || cap_factor<5) {cap_factor = 1.0}
  if (pop_factor<=0 || pop_factor<5) {pop_factor = 1.0}
  
  # load spatial object direct from geojson
  sp_school_raw = readOGR("./data", sprintf("%s_school",gCaseArea))
  
  # check if data layer can be successfully loaded
  if(is.null(sp_school_raw)){
    utils.debugprint("fail to load data layer for school")
    return(FALSE)
  }
  # project to utm crs so that travel distance can be measured in meters
  sp_school = utils.project2UTM(sp_school_raw)
  

  sp_school@data[,"school_id"] = c(1:nrow(sp_school@data))
  sp_school@data[,gColumnNameSchoolCapacity] = as.integer(as.integer(as.character(sp_school@data[,gColumnNameSchoolCapacity]))*cap_factor)
  sp_school@data[,"remains"] = as.integer(as.character(sp_school@data[,gColumnNameSchoolCapacity]))
  

  sp_student_raw = readOGR("./data", sprintf("%s_student_2km",gCaseArea))
  # check if data layer can be successfully loaded
  if(is.null(sp_student_raw)){
    utils.debugprint("fail to load data layer for meshblock student")
    return(FALSE)
  }
  # project to utm crs so that travel distance can be measured in meters
  sp_student = utils.project2UTM(sp_student_raw)
 
  # assume the population are at school age
  sp_student@data[,"student"] = as.integer(as.integer(as.character(sp_student@data[,gColumnNameStudentPopulation]))*pop_factor)
  # record the student allocation detail in format of "school_id:student_num:seq:tt,school_id:student_num:seq:tt"
  sp_student@data[,"alc_detail"] = c("")
  # record remaining students to be allocated, 
  sp_student@data[,"remains"] = as.integer(as.integer(as.character(sp_student@data[,gColumnNameStudentPopulation]))*pop_factor)
  # the school_id which takes the majority of the students from that mb
  sp_student@data[,"m_sch_id"] = as.integer(0) 
  # travel cost to main school
  sp_student@data[,"m_sch_tt"] = as.numeric(0) 
  # process sequence index to main school,
  sp_student@data[,"m_sch_seq"] = as.integer(0) 
  # total number of schools that support this mb,
  sp_student@data[,"ttl_sch"] = as.integer(0) 
  # total school-mb travel cost of this mb 
  sp_student@data[,"ttl_tt"] = as.numeric(0) 
  
  
  # build a filter for meshblock that has students
  sp_student_valid = sp_student[sp_student@data[,"student"]>0,]

  
  # bulid foreach result as a travel matrix between each mb and school in structure of: mb_code, school_id, t_cost
  tt_matrix = foreach(i=1:nrow(sp_school@data), .combine=rbind, .export = c("gColumnNameMeshBlockCode")) %dopar% {
    
      school_id = as.integer(sp_school@data[i,"school_id"])
      school_coord = as.double(sp_school@coords[i,])
      mb_coord = sp_student_valid@polygons[[1]]@labpt
      
      tt = mapply(function(attr) sqrt((attr@labpt[1]-school_coord[1])^2+(attr@labpt[2]-school_coord[2])^2), sp_student_valid@polygons)
      out = cbind(school_id, mb_code=as.character(sp_student_valid@data[, gColumnNameMeshBlockCode]), tt)
  }
  
  tt_df = as.data.frame(tt_matrix)
  
  tt_df[,"tt"] = as.numeric(as.character(tt_df[,"tt"]))
  tt_df[,"school_id"] = as.integer(as.character(tt_df[,"school_id"]))
  tt_df[,"mb_code"] = as.character(tt_df[,"mb_code"])
  
  
  # sort tt_df by tt (asc)
  tt_df_sorted = tt_df[order(tt_df[,"tt"]),]
  
  algrithmStartTime = Sys.time()
  
  # stop the process when either no students to be allocated or no capacity of all schools
  proc_seq = 1
  while(sum(sp_student_valid@data[,"remains"])!=0 & sum(sp_school@data[,"remains"])!=0){
    
    #utils.debugprint(sprintf("remaining students:%i, capacity:%i",sum(sp_student_valid@data[,"remains"]), sum(sp_school@data[,"remains"]) ))
    
    # always find the first record in the tt_df_sorted
    v_school_id = as.integer(tt_df_sorted[1,"school_id"])
    v_mb_code = as.character(tt_df_sorted[1,"mb_code"])
    v_tt = as.numeric(tt_df_sorted[1,"tt"])
    
    # if the smallest distance between school and mb is bigger than allocation distance threshold, then stop process
    if(v_tt > school_dist_threshold){
      utils.debugprint("stop allocation process due to exceeding school allocation distance threshold")
      break
    }
    
    mb_filter = sp_student_valid@data[,gColumnNameMeshBlockCode] == v_mb_code
    mb_remain_student = sp_student_valid@data[mb_filter,"remains"]
    
    school_filter = sp_school@data[,"school_id"] == v_school_id
    school_remain_capacity = sp_school@data[school_filter,"remains"]
    
    # if all students in current mb can be allocated to current school, do it
    if(mb_remain_student <= school_remain_capacity){
      # update remains for current mb
      sp_student_valid@data[mb_filter,"remains"] = 0
      # update alc_detail for current mb
      if(mb_remain_student>0){
        sp_student_valid@data[mb_filter,"alc_detail"] = sprintf("%s,%i:%i:%i:%f", sp_student_valid@data[mb_filter,"alc_detail"], v_school_id, mb_remain_student, proc_seq, v_tt)
        proc_seq = proc_seq + 1
      }
      
      # remove all rows in tt_df_sorted contains current mb code
      tt_df_sorted = tt_df_sorted[!tt_df_sorted[,"mb_code"]==v_mb_code,]
      
      # update remains for current school
      sp_school@data[school_filter,"remains"] = school_remain_capacity - mb_remain_student
      
    }else{# if only a fraction of student can be allocated to current school, 
      
      # update remains for current mb
      sp_student_valid@data[mb_filter,"remains"] = mb_remain_student - school_remain_capacity
      # update alc_detail for current mb
      if(school_remain_capacity>0){
        sp_student_valid@data[mb_filter,"alc_detail"] = sprintf("%s,%i:%i:%i:%f", sp_student_valid@data[mb_filter,"alc_detail"], v_school_id, school_remain_capacity, proc_seq, v_tt)
        proc_seq = proc_seq + 1
      }
      
      # update remains for current school
      sp_school@data[school_filter,"remains"] = 0
    
      # remove all rows in tt_df_sorted contains current school id 
      tt_df_sorted = tt_df_sorted[!tt_df_sorted[,"school_id"]==v_school_id,]
    }
    
    # sort tt_df_sorted by tt (asc)
    tt_df_sorted = tt_df_sorted[order(tt_df_sorted[,"tt"]),]
    
  }

  algrithmEndTime = Sys.time()
  
  # let's parse the alc_detail and find the school_id which takes the majority of the students from that mb
  sp_student_valid@data[, c("m_sch_id", "m_sch_tt", "m_sch_seq", "ttl_sch", "ttl_tt")] = 
    as.data.frame(t(mapply(function(alc_detail) {
      
      if(nchar(alc_detail) == 0) return(data.frame(m_sch_id=0, m_sch_tt=0, m_sch_seq=0, ttl_sch=0, ttl_tt=0))
      
      arr = strsplit(substr(alc_detail, 2, nchar(alc_detail)),",")
      m_sch_id = as.integer(mapply(function(attr) strsplit(attr,":")[[1]][1], arr[[1]]))
      m_sch_seq = as.integer(mapply(function(attr) strsplit(attr,":")[[1]][3], arr[[1]]))
      m_sch_tt = as.numeric(mapply(function(attr) strsplit(attr,":")[[1]][4], arr[[1]]))
      stud_num = as.integer(mapply(function(attr) strsplit(attr,":")[[1]][2], arr[[1]]))
      ttl_sch = length(m_sch_id)
      ttl_tt = m_sch_tt*stud_num
      df = data.frame(m_sch_id=m_sch_id,stud_num=stud_num, m_sch_seq=m_sch_seq, m_sch_tt=m_sch_tt, ttl_sch=ttl_sch, ttl_tt=ttl_tt)
      df = df[order(df[, "stud_num"], decreasing=TRUE),]
      return(df[1, c("m_sch_id", "m_sch_tt", "m_sch_seq", "ttl_sch", "ttl_tt")])
      
    }, sp_student_valid@data[,"alc_detail"])))
  
  # convert column from list to vector
  sp_student_valid@data[, "m_sch_id"] = as.integer(sp_student_valid@data[, "m_sch_id"])
  sp_student_valid@data[, "m_sch_tt"] = as.numeric(sp_student_valid@data[, "m_sch_tt"])
  sp_student_valid@data[, "m_sch_seq"] = as.integer(sp_student_valid@data[, "m_sch_seq"])
  sp_student_valid@data[, "ttl_sch"] = as.integer(sp_student_valid@data[, "ttl_sch"])
  sp_student_valid@data[, "ttl_tt"] = as.numeric(sp_student_valid@data[, "ttl_tt"])
  
  sp_student[sp_student@data[,"student"]>0,] = sp_student_valid@data
  
  # convert column from list to vector
  sp_student@data[, "m_sch_id"] = as.integer(sp_student@data[, "m_sch_id"])
  sp_student@data[, "m_sch_tt"] = as.numeric(sp_student@data[, "m_sch_tt"])
  sp_student@data[, "m_sch_seq"] = as.integer(sp_student@data[, "m_sch_seq"])
  sp_student@data[, "ttl_sch"] = as.integer(sp_student@data[, "ttl_sch"])
  sp_student@data[, "ttl_tt"] = as.numeric(sp_student@data[, "ttl_tt"])
  
  
  # print out the output summary
  utils.debugprint("output summary ===")
  utils.debugprint(sprintf("total students: %i", sum(sp_student_valid@data[,"student"])))
  utils.debugprint(sprintf("total school capacities: %i", sum(sp_school@data[,gColumnNameSchoolCapacity])))
  utils.debugprint(sprintf("total travel cost: %.2f", sum(sp_student@data[,"ttl_tt"])))
  
  if(sum(sp_student_valid@data[,"remains"]) == 0){
    utils.debugprint(sprintf("all students (%i) are allocated", sum(sp_student_valid@data[,"student"])))
  }else
  {
    utils.debugprint(sprintf("%i students are allocated, %i students remained", sum(sp_student_valid@data[,"student"])-sum(sp_student_valid@data[,"remains"]), sum(sp_student_valid@data[,"remains"])))
  }
  
  if(sum(sp_school@data[,"remains"]) == 0){
    utils.debugprint(sprintf("all school capacities (%i) are filled", sum(sp_school@data[,gColumnNameSchoolCapacity])))
  }else
  {
    utils.debugprint(sprintf("%i school capacities are filled, %i capacities remained", sum(sp_school@data[,gColumnNameSchoolCapacity])-sum(sp_school@data[,"remains"]), sum(sp_school@data[,"remains"])))
  }
  
  # save output in shpfiles
  writeOGR(utils.project2WGS84(sp_student), dsn="./outputs", layer = sprintf("%s_greedy_student_allocation_%.2f_%.2f", gCaseArea, cap_factor, pop_factor),  driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
  writeOGR(utils.project2WGS84(sp_school), dsn="./outputs", layer = sprintf("%s_greedy_school_capacity_%.2f_%.2f", gCaseArea, cap_factor, pop_factor),  driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
  
  generalEndTime = Sys.time()
  
  utils.debugprint(sprintf("greedy processing time: %.2f seconds) ===", as.numeric(algrithmEndTime-algrithmStartTime, units="secs")))
  utils.debugprint(sprintf("total running time: %.2f seconds) ===", as.numeric(generalEndTime-generalStartTime, units="secs")))
  
  # save algorithm results and perfromance 
  df_summary <- data.frame(student_ttl_num = sum(sp_student_valid@data[,"student"]), 
                           school_ttl_capacity = sum(sp_school@data[,gColumnNameSchoolCapacity]),
                           tvl_ttl_cost = sum(sp_student@data[,"ttl_tt"]),
                           tvl_avg_cost = sum(sp_student@data[,"ttl_tt"]) / (sum(sp_student_valid@data[,"student"])-sum(sp_student_valid@data[,"remains"])),
                           student_allocated = sum(sp_student_valid@data[,"student"])-sum(sp_student_valid@data[,"remains"]),
                           student_remains = sum(sp_student_valid@data[,"remains"]),
                           school_filled = sum(sp_school@data[,gColumnNameSchoolCapacity])-sum(sp_school@data[,"remains"]),
                           school_remains = sum(sp_school@data[,"remains"]),
                           exectime = as.numeric(algrithmEndTime-algrithmStartTime, units="secs")
  )
  
  write.csv(df_summary, file = sprintf("./outputs/%s_greedy_summary_%.2f_%.2f.csv", gCaseArea, cap_factor, pop_factor), row.names=FALSE)
  

}


#
#' using linear programming solver to calcuate Enrolment Simulation Application using mockup school and population datasets
#' global optimal is assured.
#'
#' @param cap_factor 			school capacity factor, in range (0,5), default 1.0 means 100% capacities are used for calculation 
#' @param pop_factor 			student population factor, in range (0,5), default 1.0 means 100% student population are used for calculation 
#'
#'
#' @return shpfiles and statistics for the student allocation details and schools
#' @export
#'
#' @examples
exec_lp <- function(cap_factor=1.0, pop_factor=1.0){
  
  generalStartTime = Sys.time()
  
  if (cap_factor<=0 || cap_factor<5) {cap_factor = 1.0}
  if (pop_factor<=0 || pop_factor<5) {pop_factor = 1.0}
  
  # load spatial object direct from geojson
  sp_school_raw = readOGR("./data", sprintf("%s_school",gCaseArea))
  
  # check if data layer can be successfully loaded
  if(is.null(sp_school_raw)){
    utils.debugprint("fail to load data layer for school")
    return(FALSE)
  }
  # project to utm crs so that travel distance can be measured in meters
  sp_school = utils.project2UTM(sp_school_raw)
  
  
  sp_school@data[,"school_id"] = c(1:nrow(sp_school@data))
  sp_school@data[,gColumnNameSchoolCapacity] = as.integer(as.integer(as.character(sp_school@data[,gColumnNameSchoolCapacity]))*cap_factor)
  sp_school@data[,"remains"] = as.integer(as.character(sp_school@data[,gColumnNameSchoolCapacity]))
  
  
  sp_student_raw = readOGR("./data", sprintf("%s_student_2km",gCaseArea))
  # check if data layer can be successfully loaded
  if(is.null(sp_student_raw)){
    utils.debugprint("fail to load data layer for meshblock student")
    return(FALSE)
  }
  # project to utm crs so that travel distance can be measured in meters
  sp_student = utils.project2UTM(sp_student_raw)
  
  # assume the population are at school age
  sp_student@data[,"student"] = as.integer(as.integer(as.character(sp_student@data[,gColumnNameStudentPopulation]))*pop_factor)
  # record the student allocation detail in format of "school_id:student_num:seq:tt,school_id:student_num:seq:tt"
  sp_student@data[,"alc_detail"] = c("")
  # record remaining students to be allocated, 
  sp_student@data[,"remains"] = as.integer(as.integer(as.character(sp_student@data[,gColumnNameStudentPopulation]))*pop_factor)
  # the school_id which takes the majority of the students from that mb
  sp_student@data[,"m_sch_id"] = as.integer(0) 
  # travel cost to main school
  sp_student@data[,"m_sch_tt"] = as.numeric(0) 
  # total number of schools that support this mb,
  sp_student@data[,"ttl_sch"] = as.integer(0) 
  # total school-mb travel cost of this mb 
  sp_student@data[,"ttl_tt"] = as.numeric(0) 
  
  
  # build a filter for meshblock that has students
  sp_student_valid = sp_student[sp_student@data[,"student"]>0,]
  
  # bulid foreach result as a travel matrix between each mb and school in structure of: mb_code, school_id, t_cost
  tt_matrix = foreach(i=1:nrow(sp_student_valid@data), .combine=rbind, .export = c("gColumnNameMeshBlockCode")) %dopar% {
    
    coordx = sp_student_valid@polygons[[i]]@labpt[1]
    coordy = sp_student_valid@polygons[[i]]@labpt[2]
    
    mb_code=as.character(sp_student_valid@data[i,gColumnNameMeshBlockCode])
    student=as.character(sp_student_valid@data[i,"student"])
    
    tt = mapply(function(xx,yy) sqrt((coordx-xx)^2+(coordy-yy)^2), sp_school@coords[,1], sp_school@coords[,2])
    
    out = c(mb_code,student,tt)
  }
  
  tt_df = as.data.frame(tt_matrix)
  
  # give the tt dataframe a proper column name, the first column is mb_code and the rest are school id
  names(tt_df) = c("mb_code","student",as.character(sp_school@data[,"school_id"]))
  
  # add one extra column for unallocated student travel cost
  unalloc_tvl_cost = 99999999
  tt_df["unalloc_tvl_cost"] = unalloc_tvl_cost
  
  # let's prepare the lp model 
  
  obj.fun <- as.numeric(t(as.matrix(tt_df[, c(as.character(sp_school@data[,"school_id"]), "unalloc_tvl_cost")])))
  
  n = nrow(sp_school@data)    
  m = nrow(sp_student_valid@data) 
  
  
  # init constraint matrix with 0 as default coefficient: 
  # n+m is the number of constrains, 
  # (n+1)*m is the number of decision variables, introduce m extral variables for unallocated students
  
  constr.mat <- matrix (0, nrow = n+m, ncol = (n+1)*m)
  for(i in 1:m){
    for(j in 1:(n+1)){
      constr.mat[i, (n+1)*(i-1)+j] = 1
      if(j<=n){
        constr.mat[m+j, (n+1)*(i-1)+j] = 1
      }
      
    }
  }
  
  constr.dir <- c(rep(">=", m), rep("<=", n))
  
  constr.rhs <- c(as.vector(tt_df$student), as.vector(sp_school@data$capacity))
  
  algrithmStartTime = Sys.time()
  EAS <- lp("min", obj.fun , constr.mat , constr.dir , constr.rhs , all.int = TRUE, compute.sens = TRUE )
  algrithmEndTime = Sys.time()
  
  
  sol.mat = matrix(EAS$solution, nrow=m, byrow=TRUE)
  #mx_solution
  #[,1] [,2] [,3] [,4]
  #[1,]   40   40    0    0
  #[2,]    0    0   60    0
  #[3,]    0   40    0    0
  #[4,]    0    0   30   20
  
  # get the optimal (minimal) total travel distance (the sum of all unallocated students travel cost should be excluded)
  sol.objval = EAS$objval - unalloc_tvl_cost*sum(sol.mat[,ncol(sol.mat)])
  
  
  # parsing the sol.mat and assign values back to sp_student_valid@data
  
  # get the total column number of sol.mat, the last column stores "remains" (unallocated) student number for each mb
  num_cols = dim(sol.mat)[2]
 
  # convert to dataframe to make parsing easier
  sol.df = as.data.frame(sol.mat)
  names(sol.df) = c(1:(num_cols-1),"remains")
  sol.df[,"mb_code"] = sp_student_valid@data[,gColumnNameMeshBlockCode]
  
  # update school remains column
  sp_school@data[,"remains"] = as.integer(sp_school@data[,gColumnNameSchoolCapacity] - apply(sol.df[,1:(num_cols-1)],2,function(x) sum(x)))
  
  # update remained student column
  sp_student_valid@data[,"remains"] = sol.df[,"remains"]
  
  # update student allocation detail columns
  sp_student_valid@data[, c("alc_detail","m_sch_id", "m_sch_tt", "ttl_sch", "ttl_tt")] = 
    as.data.frame(t(apply(sol.df[, c(1:(num_cols-1),"mb_code")], 1, function(attr_and_code) {

    attr = as.numeric(attr_and_code[c(1:(num_cols-1))])
    code = as.character(attr_and_code[num_cols])

    # find indices that has positive student number. the index is actually the shool_id
    sch_id_vec = which(attr > 0)
    # find how many schools support current mb
    ttl_sch = length(sch_id_vec)
    
    # if no student is allocated to any school, return 
    if(ttl_sch==0){
      return(c("",0,0,0,0))
    }
    
    ttl_tt = 0
    m_sch_id = 0
    m_sch_stud_num = 0
    m_sch_tt = 0
    
    # find allocation details, school_id:student:tt
    alc_detail = ""
    for(i in 1:ttl_sch){
      v_tt = as.numeric(as.character(tt_df[which(tt_df[,"mb_code"]==code),as.character(sch_id_vec[i])]))
      alc_detail=sprintf("%s,%i:%i:%f",alc_detail,sch_id_vec[i], attr[sch_id_vec[i]], v_tt)
      
      ttl_tt = ttl_tt + v_tt*attr[sch_id_vec[i]]
      
      # also find the school_id and travel cost of a school which takes the majority of the students from that mb
      if(attr[sch_id_vec[i]] > m_sch_stud_num){
        m_sch_stud_num = attr[sch_id_vec[i]]
        m_sch_id = sch_id_vec[i]
        m_sch_tt = v_tt
      }
    }
    
    return(c(alc_detail,m_sch_id,m_sch_tt,ttl_sch,ttl_tt))

  })))
  
  # convert column from list to vector
  sp_student_valid@data[, "alc_detail"] = as.character(sp_student_valid@data[, "alc_detail"])
  sp_student_valid@data[, "m_sch_id"] = as.integer(as.character(sp_student_valid@data[, "m_sch_id"]))
  sp_student_valid@data[, "m_sch_tt"] = as.numeric(as.character(sp_student_valid@data[, "m_sch_tt"]))
  sp_student_valid@data[, "ttl_sch"] = as.integer(as.character(sp_student_valid@data[, "ttl_sch"]))
  sp_student_valid@data[, "ttl_tt"] = as.numeric(as.character(sp_student_valid@data[, "ttl_tt"]))
  
  sp_student[sp_student@data[,"student"]>0,] = sp_student_valid@data

  
  # convert column from list to vector
  sp_student@data[, "alc_detail"] = as.character(sp_student@data[, "alc_detail"])
  sp_student@data[, "m_sch_id"] = as.integer(as.character(sp_student@data[, "m_sch_id"]))
  sp_student@data[, "m_sch_tt"] = as.numeric(as.character(sp_student@data[, "m_sch_tt"]))
  sp_student@data[, "ttl_sch"] = as.integer(as.character(sp_student@data[, "ttl_sch"]))
  sp_student@data[, "ttl_tt"] = as.numeric(as.character(sp_student@data[, "ttl_tt"]))
  
  # print out the output summary
  utils.debugprint("output summary ===")
  utils.debugprint(sprintf("total students: %i", sum(sp_student_valid@data[,"student"])))
  utils.debugprint(sprintf("total school capacities: %i", sum(sp_school@data[,gColumnNameSchoolCapacity])))
  utils.debugprint(sprintf("total travel cost: %.2f", sum(sp_student@data[,"ttl_tt"])))
  
  if(sum(sp_student_valid@data[,"remains"]) == 0){
    utils.debugprint(sprintf("all students (%i) are allocated", sum(sp_student_valid@data[,"student"])))
  }else
  {
    utils.debugprint(sprintf("%i students are allocated, %i students remained", sum(sp_student_valid@data[,"student"])-sum(sp_student_valid@data[,"remains"]), sum(sp_student_valid@data[,"remains"])))
  }
  
  if(sum(sp_school@data[,"remains"]) == 0){
    utils.debugprint(sprintf("all school capacities (%i) are filled", sum(sp_school@data[,gColumnNameSchoolCapacity])))
  }else
  {
    utils.debugprint(sprintf("%i school capacities are filled, %i capacities remained", sum(sp_school@data[,gColumnNameSchoolCapacity])-sum(sp_school@data[,"remains"]), sum(sp_school@data[,"remains"])))
  }
  
  
  # save output in shpfiles
  writeOGR(utils.project2WGS84(sp_student), dsn="./outputs", layer = sprintf("%s_lp_student_allocation_%.2f_%.2f", gCaseArea, cap_factor, pop_factor),  driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
  writeOGR(utils.project2WGS84(sp_school), dsn="./outputs", layer = sprintf("%s_lp_school_capacity_%.2f_%.2f", gCaseArea, cap_factor, pop_factor),  driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
 
  generalEndTime = Sys.time()
  
  utils.debugprint(sprintf("lp processing time: %.2f seconds) ===", as.numeric(algrithmEndTime-algrithmStartTime, units="secs")))
  utils.debugprint(sprintf("total running time: %.2f seconds) ===", as.numeric(generalEndTime-generalStartTime, units="secs")))
  
  
  # save algorithm results and perfromance 
  df_summary <- data.frame(student_ttl_num = sum(sp_student_valid@data[,"student"]), 
                           school_ttl_capacity = sum(sp_school@data[,gColumnNameSchoolCapacity]),
                           tvl_ttl_cost = sum(sp_student@data[,"ttl_tt"]),
                           tvl_avg_cost = sum(sp_student@data[,"ttl_tt"]) / (sum(sp_student_valid@data[,"student"])-sum(sp_student_valid@data[,"remains"])),
                           student_allocated = sum(sp_student_valid@data[,"student"])-sum(sp_student_valid@data[,"remains"]),
                           student_remains = sum(sp_student_valid@data[,"remains"]),
                           school_filled = sum(sp_school@data[,gColumnNameSchoolCapacity])-sum(sp_school@data[,"remains"]),
                           school_remains = sum(sp_school@data[,"remains"]),
                           exectime = as.numeric(algrithmEndTime-algrithmStartTime, units="secs")
  )
  
  write.csv(df_summary, file = sprintf("./outputs/%s_lp_summary_%.2f_%.2f.csv", gCaseArea, cap_factor, pop_factor), row.names=FALSE)
  
  
}

compareSCP <- function(cap_factor=1.0, pop_factor=1.0){
  utils.debugprint("running greedy alogrithm")
  exec_greedy(cap_factor=cap_factor, pop_factor=pop_factor)
  utils.debugprint("running linear programming alogrithm")
  exec_lp(cap_factor=cap_factor, pop_factor=pop_factor)
}
