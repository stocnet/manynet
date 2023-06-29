# # Test plot functions
# test_that("agreements_plot() returns the correct output format", {
#   agreements <- data.frame(manyID = c("GEO-GRC[IEO]_2000P1", "GRC-TUR[HEP]_2000S",
#                                       "KAZ-KGZ[RCT]_2000A", "BGR-URY[ORA]_2000A",
#                                       "CRTBBD_2000P:CBD_1992A", "ESP-PRT[QRA]_2000A",
#                                       "STP-EC[STP]_2000A", "KAZ-KGZ[CTR]_2000A",
#                                       "EGY-GRC[OHA]_2000P5", "GT10PP_2000A"),
#                            Title = c("Protocol Of The 1 Session Of The Joint Intergovernmental Commission For Economic And Technical Cooperation Between Greece And Georgia",
#                                      "Memorandum Of Understanding Between The Hellenic Republic And The Republic Of Turkey Concerning Cooperation On Environmental Protection",
#                                      "AGREEMENT Between The Government Of The Kazakh Republic And The Government Of The Kyrgyz Republic On The Use Of Water Management Facilities Of Intergovernmental Status On The Rivers Chu And Talas",
#                                      "Agreement Between The Oriental Republic Of Uruguay And The Republic Of Bulgaria On Antarctic Cooperation",
#                                      "Cartagena Protocol On Biosafety To The Convention On Biological Diversity",
#                                      "Agreement On Cooperation For The Protection And Sustainable Use Of The Waters Of The Spanish-Portuguese Hydrographic Basins Made Quotad Referendumquot In Albufeira On November 30 1998",
#                                      "Agreement In The Form Of An Exchange Of Letters Concerning The Provisional Application Of The Protocol Setting Out For The Period 1 June 1999 To 31 May 2002 The Fishing Opportunities And The Financial Contribution Provided For By The Agreement Between The European Community And The Government Of The Democratic Republic Of Sao Tome E Principe On Fishing Off The Coast Of Sao Tome E Principe",
#                                      "Agreement Between The Government Of The Republic Of Kazakhstan And The Government Of The Kyrgyz Republic On Utilization Of The Water Facilities Of Interstate Use On The Chu And Talas Rivers",
#                                      "Protocol Of The 5 Session Of The Joint Ministerial Committee On Economic And Technical Cooperation Between The Hellenic Republic And The Arab Republic Of Egypt",
#                                      "Multilateral Agreement Between The Governments Of The Treaty For A Customs Union And The Common Economic Space On Joint Exploration Of Outer Space For Peaceful Purposes"),
#                            Begin = c("2000-01-18", "2000-01-20", "2000-01-21",
#                                      "2000-01-27", "2000-01-29", "2000-01-31",
#                                      "2000-02-03", "2000-02-11", "2000-02-16",
#                                      "2000-02-17"),
#                            DocType = c("B", "B", "B", "B", "M", "B", "B", "B",
#                                        "B", "M"))
#   p <- plot_agreements(agreements)
#   expect_type(p, "list")
#   expect_length(p, 9)
#   expect_true(ggplot2::is.ggplot(p))
#   expect_named(p, c("data", "layers", "scales", "mapping", "theme",
#                      "coordinates", "facet", "plot_env", "labels"))
#   expect_true(p[["plot_env"]][["layout"]] == "circle")
# })
# 
# test_that("membership_plot() returns the correct output format", {
#   memberships <- data.frame(manyID = c("IE05PD_2000P:IE05PD_1971A",
#                                        "IOPPRC_2000A", "IE05PD_2000P:IE05PD_1971A",
#                                        "ICLOPD_2000P:CLC_1969A",   
#                                        "CTMHWD_2000E:CTMHWD_1989A",
#                                        "INTRPP_2000S", "ICLOPD_2000P:CLC_1969A",   
#                                        "CF04TD_2000A", "TRNEIA_2000A",             
#                                        "IT04FF_2000E:CITES_1973A"),
#                             Title = c("Protocol Of 1992 To Amend The International Convention On The Establishment Of An International Fund For Compensation For Oil Pollution Damage 1971",
#                                       "International Convention On Oil Pollution Preparedness Response And Cooperation",
#                                       "Protocol Of 1992 To Amend The International Convention On The Establishment Of An International Fund For Compensation For Oil Pollution Damage 1971",
#                                       "Protocol Of 1992 To Amend The International Convention On Civil Liability For Oil Pollution Damage 1969",
#                                       "Amendment To The Basel Convention On The Control Of Transboundary Movements Of Hazardous Wastes And Their Disposal",
#                                       "International Plant Protection Convention",
#                                       "Protocol Of 1992 To Amend The International Convention On Civil Liability For Oil Pollution Damage 1969",
#                                       "Cooperation Agreement On The Forecast Prevention And Mitigation Of Natural And Technological Disasters",
#                                       "Convention On The Transboundary Effects Of Industrial Accidents",
#                                       "Amendment To The Convention On International Trade In Endangered Species Of Wild Fauna And Flora (Art11)"),
#                             Begin = c("2000-01-05", "2000-01-05", "2000-01-06",
#                                       "2000-01-06", "2000-01-12", "2000-01-12",
#                                       "2000-01-15", "2000-01-19", "2000-01-20",
#                                       "2000-01-20"),
#                             stateID = c("COG", "COM", "MDV", "MLT", "TTO",
#                                         "LUX", "COM", "SVK", "HRV", "KEN"))
#   p <- plot_memberships(memberships)
#   expect_type(p, "list")
#   expect_length(p, 9)
#   expect_true(ggplot2::is.ggplot(p))
#   expect_named(p, c("data", "layers", "scales", "mapping", "theme",
#                     "coordinates", "facet", "plot_env", "labels"))
#   expect_true(p[["plot_env"]][["layout"]] == "bipartite")
# })
