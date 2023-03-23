000100 IDENTIFICATION DIVISION.                                         SM2084.2
000200 PROGRAM-ID. SM208A.                                              SM2084.2
000300 REPLACE OFF.                                                     SM2084.2
000400 ENVIRONMENT DIVISION.                                            SM2084.2
000500 DATA DIVISION.                                                   SM2084.2
000600 WORKING-STORAGE SECTION.                                         SM2084.2
000700 REPLACE ==01  A     PICTURE X.== BY                              SM2084.2
000800         ==01  B     PICTURE S9(7) COMP.==.                       SM2084.2
000900 01  A     PICTURE X.                                             SM2084.2
001000 01  C     PICTURE XXBXX/XX.                                      SM2084.2
001100 REPLACE OFF.                                                     SM2084.2
001200 01  D     PICTURE X(7) VALUE "PICTURE".                          SM2084.2
001300 01  WRK-XN-00001  PIC X.                                         SM2084.2
001400 01  WRK-XN-00020  PIC X(20).                                     SM2084.2
001500 01  WRK-XN-00322  PIC X(322).                                    SM2084.2