
#' Extract available CWatM fluxes
#'
#' @description
#' The function extracts the available fluxes codes from the CWatM Circles database.
#'
#' @return a `character` vector of  fluxes.
#'
#' @examples
#' getFluxes()
#'
#' @export
getFluxes <- function() {
  return(names(cwatmViz::dbase))
}


#' Extract available CWatM circles
#'
#' @description
#' The function extracts the available circles' codes for  any selected flux.
#'
#' @param flux A `character` vector with flux codes. See `getFluxes()`.
#'
#' @return a `data.frame` with available circles per flux.
#'
#' @examples
#' getCodes(flux = "hydro")
#'
#' @export
getCodes <- function(flux =  NULL) {
  if(!is.null(flux)) {
    stopifnot("Unknown flux code, use NULL or check getFluxes()" = all(flux %in% names(cwatmViz::dbase)))

    return(unique(do.call("rbind", lapply(flux, function(fl) {
      inst <- cwatmViz::dbase[[fl]]
      inst[c("crcl_name", "crcl_code")]
    }))))
  }
  return(unique(do.call("rbind", lapply(cwatmViz::dbase, function(inst) {
    inst[c("crcl_name", "crcl_code")]
  }))))
}

#' Extract required CWatM circle variables
#'
#' @description
#' The function provide a string with all  CWatM output variables, required to create  selected circles, given specified settings.
#'
#' @param circle A `character` vector with circle names. See `getCodes()`, (default: `NULL`, so all circles are included).
#' @param flux A `character` vector with flux codes. See `getFluxes()`, (default: `hydro`, so only water-circles are included).
#' @param modflow A `logical` variable indicating if modflow simulation was included (default: `FALSE`).
#' @param wastewater A `logical` variable indicating if the wastewater module was included (default: `FALSE`).
#' @param limitAbstr A `logical` variable indicating if limitAbstraction was set to `TRUE`, (default: `FALSE`).
#'
#' @return a comma-separated `character` string with all required output variables.
#'
#' @examples
#' getVars(circle = c("soil", "main"), flux = "hydro", limitAbstr = TRUE)
#'
#' @export
getVars <- function(circle = NULL,
                    flux = "hydro",
                    modflow = FALSE,
                    wastewater = FALSE,
                    limitAbstr = FALSE) {

  if(!is.null(flux)) {
    stopifnot("Unknown flux code, use NULL or check getFluxes()" = all(flux %in% names(cwatmViz::dbase)))
  }

  #subset fluxes
  tmpdbase <- unique(do.call("rbind", lapply(flux, function(flx) {
    inst <- cwatmViz::dbase[[flx]]
    inst$flux <- flx
    return(inst)
  })))

  #subset circles
  if(!is.null(circle)) stopifnot("Unknown circle code, use NULL or check getCodes()" = all(circle %in% tmpdbase$crcl_code))
  if(is.null(circle)) {
    circle <- unique(tmpdbase$crcl_code)
  }

  tmp <- tmpdbase[tmpdbase$crcl_code %in% circle, ]

  # filter/add variables based on settings
  if(!modflow) tmp <- tmp[tmp$modflow_T == 0, ]
  if(modflow) tmp <- tmp[tmp$modflow_F == 0, ]

  if(!wastewater) tmp <- tmp[tmp$wastewater_T == 0, ]
  if(wastewater) tmp <- tmp[tmp$wastewater_F == 0, ]

  if(limitAbstr)  tmp <- tmp[tmp$limitAbst_T == 0, ]

  return(paste0(unique(tmp$var), collapse = " ,"))
}


#' Extract required CWatM circle variables
#'
#' @description
#' The function provide a string with all  CWatM output variables, required to create  selected circles, given specified settings.
#'
#' @param circles A `character` vector with circle names. See `getCodes()`, (default: `NULL`, so all circles are included).
#' @param flux A `character` vector with a flux code. See `getFluxes()`, (default: `hydro`, so only water-circles are included).
#' @param dataPath A `character` string, defines the location of the simulation outputs.
#' @param modflow A `logical` variable indicating if `modflow` simulation was included (default: `FALSE`).
#' @param wastewater A `logical` variable indicating if the `wastewater` module was included (default: `FALSE`).
#' @param limitAbstr A `logical` variable indicating if `limitAbstraction` was set to `TRUE`, (default: `FALSE`).
#' @param outlet A `numeric` vector defined (x, y) coordinate of the outlet, (default: `NULL`, only required in some circles).
#' @param spatial A `data.frame` with  (x, y) coordinate to create a circle for specific grid-cells (e.g., lake), or a boolean `RasterLayer` mask, (default: `NULL`).
#' @param timeCons A `Date` vector with a (start_date, end_date) for temporal sub-setting, (default: `NULL`).
#' @param annualAverage A `logical`. If set to `TRUE` the circles show annual average values, (default: `FALSE`).
#' @param printBalance A `logical`. If set to `TRUE` the circles' `data.frame` are printed, (default: `FALSE`).
#' @param loud A `logical`. If set to `TRUE` the function prints the names of all loaded files, (default: `FALSE`).

#'
#' @return a comma-separated `character` string with all required output variables.
#'
#'
#' @export
buildCircle <- function(circles = NULL,
                        flux = "hydro",
                        dataPath = "./",
                        modflow = FALSE,
                        wastewater = FALSE,
                        limitAbstr = TRUE,
                        outlet = NULL, # (x, y)
                        spatial = NULL,
                        timeCons = NULL,
                        annualAverage = FALSE,
                        printBalance = FALSE,
                        cancelLables = FALSE,
                        loud = FALSE) {


  replaceEmpty <- function(x) {
    if(class(x) %in% "data.frame" && nrow(x) == 0) {
      x <- 0
    } else if(length(x) == 0) {
      x <- 0
    }
    return(x)
  }

  if(!is.null(flux)) {
    stopifnot("Unknown flux code, use NULL or check getFluxes()" = all(flux %in% names(cwatmViz::dbase)))
  }

  #subset fluxes
  tmpdbase <- unique(do.call("rbind", lapply(flux, function(flx) {
    inst <- cwatmViz::dbase[[flx]]
    inst$flux <- flx
    return(inst)
  })))

  #subset circles
  if(!is.null(circles)) stopifnot("Unknown circle code, use NULL or check getCodes()" = all(circles %in% tmpdbase$crcl_code))
  if(is.null(circles)) {
    circles <- unique(tmpdbase$crcl_code)
  }

  if(!wastewater) {circles <- circles[!circles %in% "wastewater"]}

  tmpdbase <- tmpdbase[tmpdbase$crcl_code %in% circles, ]

  # filter/add variables based on settings
  if(!modflow) tmpdbase <- tmpdbase[tmpdbase$modflow_T == 0, ]
  if(modflow) tmpdbase <- tmpdbase[tmpdbase$modflow_F == 0, ]

  if(!wastewater) tmpdbase <- tmpdbase[tmpdbase$wastewater_T == 0, ]
  if(wastewater) tmpdbase <- tmpdbase[tmpdbase$wastewater_F == 0, ]

  if(limitAbstr)  tmpdbase <- tmpdbase[tmpdbase$limitAbst_T == 0, ]

  # error handling - xy missing where relevant circles are needed. e.g. outlet?
  # handle spatial settings
  isPts <- FALSE
  if(class(spatial) %in% "data.frame") isPts <- TRUE

  # conversions
  areamask <- cwatmRutils::ncdf2raster(sprintf("%s/cellArea_totalend.nc", dataPath), transpose = TRUE,
                                       spatial = spatial)[[1]]

  if(isPts) {
    MtoM3 <- cwatmRutils::ncdf2raster(pth = sprintf("%s/cellArea_totalend.nc", dataPath), transpose = TRUE)
    cells <- raster::cellFromXY(MtoM3, as.matrix(spatial[ , c("x", "y")]))
    MtoM3[!seq_len(raster::ncell(MtoM3)) %in% cells] <- NA
  }

  # for spatial - outlet should be treated (discharge)
  M3StoM3 <- 86400

  # read data in MCM ####
  invars <- unique(tmpdbase[c("var")])

  # CHECK IF DISCHARGE IS INCLUDED
  includeDischarge <- "discharge" %in% invars$var
  if(includeDischarge) {
    disvar <- invars[invars$var %in% "discharge", ]
    invars <- as.data.frame(invars[!invars$var %in% "discharge", ])
    names(invars) <- "var"
  }

  dailyGeneral <- do.call("rbind", lapply(seq_len(nrow(invars)), function(i) {
    fin <- sprintf("%s/%s_daily.nc", dataPath, invars$var[i])

    if(loud) print(fin)
    if(tmpdbase[tmpdbase$var %in% invars$var[i], "var_unit"] == "M") {
      r <- cwatmRutils::ncdf2raster(pth = fin, transpose = TRUE, spatial = areamask,
                                    flip = NULL, fun = sum, na.rm = TRUE, time = timeCons)
    } else {
      r <- cwatmRutils::ncdf2raster(pth = fin, transpose = TRUE, spatial = !is.na(areamask),
                                    flip = NULL, fun = sum, na.rm = TRUE, time = timeCons)
    }



    if(tmpdbase[tmpdbase$var %in% invars$var[i], "var_unit"] == "ton") {
      r$value <- r$value * 10 ^ 3
    }

    if(tmpdbase[tmpdbase$var %in% invars$var[i], "var_unit"] == "M3") {
      r$value <- r$value
    }

    return(r)
  }))

  if(includeDischarge) {
    fin <- sprintf("%s/%s_daily.nc", dataPath, disvar)

    if(!class(outlet) %in% "data.frame") outlet <- as.data.frame(matrix(outlet, nrow = 1, dimnames = list(NULL, c("x", "y"))))

    d <- cwatmRutils::ncdf2raster(pth = fin, transpose = TRUE, spatial = outlet, time = timeCons)

    d <- dplyr::summarise(dplyr::group_by(d, var, time), "value" = sum(value, na.rm = TRUE))
    d$value <- d$value * M3StoM3# / 10^6

    #d$flow <- tmpdbase[tmpdbase$var %in% "discharge" , "var_flow"]

    dailyGeneral <- as.data.frame(rbind(dailyGeneral, d[c("var" ,"time", "value")]))
  }

  sunBursts <- setNames(lapply(circles, function(circle) {
    if(loud) print(circle)

    crclinfo <- tmpdbase[tmpdbase$crcl_code %in% circle, ]
    daily <- dailyGeneral[dailyGeneral$var %in% crclinfo$var, ]
    daily <- merge(daily, crclinfo[c("var", "var_name", "var_flow")],
                   by = "var", all.x = TRUE, sort = FALSE)
    names(daily)[5] <- "flow"
    subsetidx_daily <- paste(daily$var, daily$flow, sep = "_")
    subsetidx_crclinfo <- paste(crclinfo$var, crclinfo$var_flow, sep = "_")
    daily_c <- daily[subsetidx_daily %in% subsetidx_crclinfo, ]
    daily_c <- unique(daily_c[daily_c$var_name %in% crclinfo$var_name, ])
    daily_c <- as.data.frame(dplyr::summarise(dplyr::group_by(daily_c, var_name, time, flow),
                                              "value" = sum(value, na.rm = TRUE)))
    # create circle dataframes ####
    balance <- tidyr::spread(daily_c[c("time", "var_name", "value")], var_name, value, fill = 0)
    balSum <- colSums(balance[,-1], na.rm = TRUE)


    # fix storage change
    if("Storage" %in% crclinfo$var_flow) {
      stoChange <- tail(balance$Storage, 1) - balance$Storage[1]
      if(circle == "main") {
        GWStorage_stochng <-  tail(balance$`GW Storage`, 1) - balance$`GW Storage`[1]
        rivers_stochng <-  tail(balance$Rivers, 1) - balance$Rivers[1]
        lakes_stochng <-  tail(balance$Lakes, 1) - balance$Lakes[1]
        soil_stochng <-  tail(balance$Soil, 1) - balance$Soil[1]
        WWTP_stochng <-  tail(balance$`WWTP Storage`, 1) - balance$`WWTP Storage`[1]
      }
    }

    invars_c <- tmpdbase[tmpdbase$crcl_code %in% circle, ]

    var2 <- names(balSum)
    var2_rplc <- unlist(lapply(names(balSum), function(nm) {unique(invars_c[invars_c$var_name %in% nm, "var_parent"])}))
    var2_rplc[is.na(var2_rplc)] <- ""

    var3 <- var2_rplc
    var3[!var2_rplc %in% ""] <- var2[!var2_rplc %in% ""]
    var2[!var2_rplc %in% ""] <- var2_rplc[!var2_rplc %in% ""]
    flows <- unlist(lapply(names(balSum), function(nm) {unique(invars_c[invars_c$var_name %in% nm, "var_flow"])}))
    df <- data.frame("var_lvl3" = var3,
                     "var_lvl2" = var2,
                     "var_lvl1" = flows,
                     "value" = balSum,
                     stringsAsFactors =  FALSE)
    df[!var2_rplc %in% "", "var_lvl1"] <- ""

    if("Storage" %in% crclinfo$var_flow) {
      if(circle == "main") {
        df[df$var_lvl2 %in% "GW Storage", "value"] <- GWStorage_stochng
        df[df$var_lvl2 %in% "Rivers", "value"] <- rivers_stochng
        df[df$var_lvl2 %in% "Lakes", "value"] <- lakes_stochng
        df[df$var_lvl2 %in% "Soil", "value"] <- soil_stochng
        df[df$var_lvl2 %in% "WWTP Storage", "value"] <- WWTP_stochng

      } else {
        df[df$var_lvl1 %in% "Storage", "value"] <- stoChange
      }

    }
    df <- df[df$value != 0, ]

    if("Storage" %in% crclinfo$var_flow) {
      df[df$value < 0 & df$var_lvl1 %in% "Storage", "var_lvl1"] <- "Storage dec."
      df[df$value > 0 & df$var_lvl1 %in% "Storage", "var_lvl1"] <- "Storage Inc."
      df$value <- abs(df$value)
    }


    tmpdf <- as.data.frame(dplyr::summarise(dplyr::group_by(df, var_lvl1), "value" =  sum(value, na.rm = TRUE)))
    bal <- replaceEmpty(tmpdf[tmpdf$var_lvl1 %in% "Inputs", "value"]) +
      replaceEmpty(tmpdf[tmpdf$var_lvl1 %in% "Storage dec.", "value"]) -
      replaceEmpty(tmpdf[tmpdf$var_lvl1 %in% "Outputs", "value"]) -
      replaceEmpty(tmpdf[tmpdf$var_lvl1 %in% "Storage Inc.", "value"])
    df <- rbind(df, data.frame("var_lvl3" = "Balance", "var_lvl2" = "Balance", "var_lvl1" = "Balance", "value" =  bal))
    if (annualAverage) {
      years <- ((tail(daily$time, 1) - head(daily$time, 1)) + 1)/ 365
      df$value <- df$value / years
    }
    df$value_lvl23 <- round(abs(df$value), 0)
    df <- as.data.frame(dplyr::mutate(dplyr::group_by(df, var_lvl1), "value_lvl1" = round(abs(sum(value, na.rm = TRUE)), 0)))


    df_lvl1 <- unique(df[c("var_lvl1", "value_lvl1")])
    df_lvl1 <- df_lvl1[!df_lvl1$var_lvl1 %in% "", ]
    df_lvl1_tmp <- df_lvl1[!df_lvl1$var_lvl1 %in% "Balance", ][order(df_lvl1[!df_lvl1$var_lvl1 %in% "Balance", "value_lvl1"], decreasing = TRUE), ]
    df_lvl1 <- rbind(df_lvl1_tmp, df_lvl1[df_lvl1$var_lvl1 %in% "Balance", ])
    names(df_lvl1) <- c("var", "value")
    df_lvl1$parents <- "Root"

    df_lvl2_tmp <- df[!df$var_lvl1 %in% "Balance", ][order(df[!df$var_lvl1 %in% "Balance", "value_lvl1"],
                                                           df[!df$var_lvl1 %in% "Balance", "value_lvl23"],
                                                           decreasing = TRUE), ]
    df_lvl2 <- rbind(df_lvl2_tmp, df[df$var_lvl1 %in% "Balance", ])

    df_lvl3 <- df_lvl2[!df_lvl2$var_lvl3 %in% "", ]
    df_lvl2 <- df_lvl2[df_lvl2$var_lvl3 %in% "", ]

    df_lvl2 <- df_lvl2[c("var_lvl2", "value_lvl23", "var_lvl1")]
    names(df_lvl2) <- c("var", "value", "parents")
    df_lvl3 <- df_lvl3[!df_lvl3$var_lvl3 %in% "Balance", c("var_lvl3", "value_lvl23", "var_lvl2")]
    names(df_lvl3) <- c("var", "value", "parents")

    df_lvl1$lbl <- df_lvl1$var
    df_lvl2$lbl <- df_lvl2$var
    df_lvl3$lbl <- df_lvl3$var

    df_lvl1$var <- paste0(df_lvl1$var, "_lvl1")
    df_lvl2$var <- paste0(df_lvl2$var, "_lvl2")
    if(nrow(df_lvl3) > 0) df_lvl3$var <- paste0(df_lvl3$var, "_lvl3")

    df_lvl2$parents <- paste0(df_lvl2$parents, "_lvl1")
    if(nrow(df_lvl3) > 0) df_lvl3$parents <- paste0(df_lvl3$parents, "_lvl2")

    df2 <- rbind(df_lvl1, df_lvl2)
    if(nrow(df_lvl3) > 0) df2 <- rbind(df2, df_lvl3)
    df2$hovertxt <- paste0(round(100 * df2$value / sum(df2$value), 0), "%")

    df2 <- rbind(df2, df2[df2$var %in% c("Balance_lvl1"), ])
    df2[nrow(df2), "parents"] <- df2[nrow(df2), "var"]
    df2[nrow(df2), "var"] <- gsub("lvl1", "lvl2", df2[nrow(df2), "var"])
    # correct balance

    df2$hovertxt <- paste0(round(100 * df2$value / sum(df2[grep("_lvl1", df2$var), "value"]), 0), "%")

    total <- sum(df2[grep("_lvl1", df2$var), "value"], na.rm = TRUE)


    if(flux == "hydro") {
      totLbl <- total / 10 ^ 6
      lblunit <- "MCM"
      if (total <= 1000) {
        totLbl <- total
        lblunit <- "M3"
      } else if(total >= 10^9) {
        totLbl <- total / 10 ^ 9
        lblunit <- "BCM"
      }

    } else {
      totLbl <- total
      lblunit <- "Ton"
      if (total > 10^5) {
        totLbl <- total / 1000
        lblunit <- "kTon" # kilo
      }
      if(total > 10^6) {
        totLbl <- total / 1000000
        lblunit <- "MTon" # Mega
      }
      if(total > 10^9) {
        totLbl <- total / 1000000000
        lblunit <- "GTon" #Giga
      }
    }


    if(totLbl < 1) {
      totLbl <- round(totLbl, 3)
    } else {
      totLbl <- round(totLbl, 0)
    }

    df2 <- rbind(data.frame("var" = "Root", "value" = total,
                            "parents" = "", "lbl" = paste0(totLbl, " ", lblunit),
                            hovertxt = "",
                            stringsAsFactors = FALSE), df2)

    df2$parents[df2$parents == ""] <- NA

    df2o <- df2
    # correct levels inconsistencies Only if higher level is > lower level
    for(row in grep("Root",df2o$parents)) {
      lbl <- df2o[row, "var"]
      idxRows <- which(df2o$parents %in% lbl)

      vals <- df2o[idxRows, "value"]
      if(sum(vals) > df2o[row, "value"]) {
        w <- vals / df2o[row, "value"]
        w <- w / sum(w)
        df2o[idxRows, "value"] <- df2o[row, "value"] * w
      }
    }

    for(row in grep("_lvl1",df2o$parents)) {
      lbl <- df2o[row, "var"]
      idxRows <- which(df2o$parents %in% lbl)

      vals <- df2o[idxRows, "value"]
      if(sum(vals) > df2o[row, "value"]) {
        w <- vals / df2o[row, "value"]
        w <- w / sum(w)
        df2o[idxRows, "value"] <- df2o[row, "value"] * w
      }
    }

    # color sets (inputs, outputs, storage, balance)
    if (flux == "hydro") {
      clrs <- c("#69C3EC", "#e75f70", "#d6d6d6", "#FFFFFF")
    } else {
      clrs <- c("#c5e9a0", "#eeb094", "#d6d6d6", "#FFFFFF")
    }


    colSet <- df2o$parents
    colSet[colSet %in% "Root"] <- df2o[colSet %in% "Root", "var"]
    colSet <- gsub("Inputs_lvl1", clrs[1], colSet) #69C3EC
    if("Storage" %in% crclinfo$var_flow) colSet <- gsub("Storage Inc._lvl1", clrs[3], colSet)
    if("Storage" %in% crclinfo$var_flow) colSet <- gsub("Storage dec._lvl1", clrs[3], colSet)
    colSet[is.na(colSet)] <- clrs[4]

    colSet <- gsub("Balance_lvl1", clrs[4], colSet)
    colSet <- gsub("Outputs_lvl1", clrs[2], colSet) #e75f70

    idx <- which(circles %in% circle)
    if(cancelLables) {
      df2o$lbl[grep("Root$", df2o$var, invert = TRUE)] <- ""
    }
    p <- plotly::plot_ly(df2o, ids = ~var,
                 values = ~ value,
                 parents = ~parents,
                 labels = ~lbl,
                 type = 'sunburst',
                 sort = FALSE,
                 branchvalues = "total",
                 hovertext = ~hovertxt,
                 domain = list(row = ceiling(idx / 2) - 1, column = 1 - idx %% 2),
                 marker = list(colors = as.list(colSet)),
                 name = invars_c$crcl_name[1])
    if(printBalance) print(df2)
    return(p)
  }), nm = circles)

  fig <- plotly::layout(do.call(eval(parse(text="plotly::subplot")), sunBursts),
                grid = list(columns = 1 + (length(circles) > 1),
                            rows = ceiling(length(circles)/2)),
                title = "Water Circles")

  fig

}

