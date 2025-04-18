#' @param data 数据框
#' @param var 变量列表
#' @return 问卷分析对象
#' @export
#' @name Questionnaire
#' @title Questionnaire

library(dplyr)
Questionnaire <- function(data,var){
  questionnaire<-R6::R6Class(
    "questionnaire",
    public = list(
      Data = NULL,
      Variable  = NULL,
      DataMean = NULL,
      initialize = function(data,var){
        self$Data = dplyr::select(data,dplyr::all_of(var))
        names(self$Data)=gsub("([a-zA-Z])([0-9])", "\\1_\\2", names(self$Data))
        self$Variable =names(self$Data)
      },
      ComputeMean = function(){
        self$DataMean=private$calculate_means(self$Data)
        return(self$DataMean)
      },
      Describe = function(){
        if(is.null(self$DataMean)){
          stop("请先运行get_mean")
        }
        result=self$DataMean%>%
          dplyr::select(where(is.numeric) & !matches("id"))%>%
          private$describe_data()
        result
      },
      ScaleMean=function(){
        if(is.null(self$DataMean)){
          stop("请先运行get_mean")
        }
        data=self$DataMean%>%
          mutate(across(where(is.numeric) & !matches("id"), ~ (.x-mean(.x, na.rm = TRUE))/sd(.x, na.rm = TRUE)))
        return(data)
      },
      ItemAnalysis=function(){
        ia=ltm::grm(self$Data[,-1])
        coef(ia)
      },
      RunCfa=function(){
        private$Measurement= self$Data %>%
          dplyr::select(dplyr::matches("[a-zA-Z]+_[0-9]+"))%>%
          tidySEM::tidy_sem() %>%
          tidySEM::measurement()
        private$CFA=tidySEM::estimate_lavaan(private$Measurement)
      },
      Alpha= function(){
        if(is.null(private$CFA)){
          stop("请先运行CFA")
        }
        alpha=semTools::compRelSEM(private$CFA)
        alpha
      },
      AVE = function(){
        if(is.null(private$CFA)){
          stop("请先运行CFA")
        }
        ave=semTools::AVE(private$CFA)
        ave
      },
      CR = function(){
        if(is.null(private$CFA)){
          stop("请先运行CFA")
        }
        CR=lavaan::standardizedsolution(private$CFA)%>%
          filter(op=="=~")%>%
          group_by(lhs)%>%
          summarise(CR=sum(est.std)^2/(sum(est.std)^2+sum(1-est.std^2)))%>%
          ungroup()
        cr=CR$CR
        names(cr)=CR$lhs
        cr
      },
      Discriminant_Validity = function(){
        if(is.null(private$CFA) & is.null(self$DataMean)){
          stop("请先运行CFA和get_mean")
        }
        la_cor<-self$Describe()
        la_cor<-la_cor[,3:ncol(la_cor)]
        la_cor<-cbind(la_cor,NA)
        colnames(la_cor)<-rownames(la_cor)
        diag(la_cor)<-sqrt(self$AVE())
        la_cor<-round(la_cor,2)
        la_cor
      },
      Loadings = function(){
        if(is.null(private$CFA)){
          stop("请先运行CFA")
        }
        loadings=lavaan::standardizedsolution(private$CFA)%>%
          filter(op=="=~")%>%
          dplyr::select(lhs,rhs,est.std,se,z,pvalue)%>%
          rename(Loadings=est.std,variable=lhs,items=rhs)
        loadings
      },
      Fitmeasures = function(index=c("chisq","df","pvalue","cfi","tli","rmsea","srmr") ){
        if(is.null(private$CFA)){
          stop("请先运行CFA")
        }
        fitmeasures=semTools::fitMeasures(private$CFA,fit.measures = index)
        fitmeasures
      }
    ),
    private = list(
      Measurement = NULL,
      CFA = NULL,
      calculate_means = function(data) {
        # 参数：
        # data: 包含变量的数据框
        # project_names: 分组的项目名称

        # 初始化结果数据框

        data_num=dplyr::select(data,where(is.numeric))
        vars=unique(sub("([a-zA-Z]+)([_0-9]+)", "\\1", names(data_num)))
        # 遍历 var 列表，计算每个项目的均值
        mean_data <- data%>%dplyr::select(-where(is.numeric),id)
        for (var in vars) {
          mean_var <- data%>%
            rowwise()%>%
            transmute(!!sym(var):=mean(c_across(starts_with(var)),na.rm=T))%>%
            pull(var)
          # 将结果添加到数据框中
          mean_data[[var]]<-mean_var
        }
        return(dplyr::select(mean_data,id,everything()))
      },
      describe_data = function(data) {
        # 提取数值型变量
        vars <- names(data)[sapply(data, is.numeric)]
        n <- length(vars)

        # 计算描述性统计
        means <- sapply(data[vars], mean, na.rm = TRUE)
        sds <- sapply(data[vars], sd, na.rm = TRUE)

        # 计算相关矩阵
        cor_matrix <- cor(data[vars], use = "pairwise.complete.obs")
        cor_matrix[upper.tri(cor_matrix, diag = T)]<-NA

        # 构建输出矩阵
        result <- matrix(NA, nrow = n, ncol =(n + 2))
        rownames(result) <- vars
        colnames(result) <- c("Mean", "SD", vars)

        # 添加均值
        result[1:n, 1] <- means
        # 添加标准差
        result[1:n, 2] <- sds

        # 添加下三角相关矩阵
        result[1:n, 3:(n+2)] <- cor_matrix
        result<-round(result, 2)
        result<-result[1:n,1:(n+1)]
        return(result)
      }
    )
  )
  return(questionnaire$new(data,var))
}
