
# the list of headers used in Vloc_Rloc file of LMGC90
#                                                                                                               11111
#                     11111111112222222222333333333344444444445555555555666666666677777777778888888888999999999900000
#            12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
HEADERS = [ ' cdbdy  numbr  cdtac  numbr  CDVER  behav  anbdy  numbr  antac  numbr  sttus'                           ,
            ' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  sttus'                                  ,
            ' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  sttus   iadj'                           ,
            ' cdbdy  numbr  cdtac  numbr  vertx  numbr  behav  anbdy  numbr  antac  numbr                sttus iadj ',
            ' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr                sttus iadj '              ,
            '           cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  segmt  sttus   iadj'          ,
            '        cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  sttus'             ,
            ' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  vertx  numbr                sttus iadj ',
            ' cdbdy  numbr  cdtac  numbr  vertx  numbr  behav  anbdy  numbr  antac  numbr  segmt  numbr'             ,
            ' cdbdy  numbr  cdtac  numbr  vertx  numbr  behav  anbdy  numbr  antac  numbr                sttus iadj ',
          ]

# keys are index in FORMAT2HEADER list
HEADER2INTERS = { 0 : { 'CDCDx', 'CDPLx', 'PRASp', 'PRPLx', 'PRPRx', 'SPCDx', 'SPDCx', 'SPPLx', 'SPSPx', },
                  1 : { 'CSASx', },
                  2 : { 'DKDKL', 'DKDKx', 'DKJCx', 'DKKDx', 'CLJCx', 'P2P2L', 'PTPT2', 'PTPT3', },
                  3 : { 'PLJCx', 'PLALp', },
                  4 : { 'DKALp', },
                  5 : { 'CLALp', 'CSPRx', },
                  6 : { 'CSPRx', },
                  7 : { 'DKPLx', },
                  8 : { 'PLPLx', },
                }

# the format of the int/str part of the interaction
FORMATS = {0: ( ('cdbdy', str,  ' ', 5,), ('icdbdy', int, '  ', 5,), ('cdtac', str, '  ', 5,), ('icdtac', int, '  ', 5,), ('icdsci', int, '  ', 5,),
                ('behav', str, '  ', 5,),
                ('anbdy', str, '  ', 5,), ('ianbdy', int, '  ', 5,), ('antac', str, '  ', 5,), ('iantac', int, '  ', 5,), ('status', str, '  ', 5,), ),
           1: ( ('cdbdy', str,  ' ', 5,), ('icdbdy', int, '  ', 5,), ('cdtac', str, '  ', 5,), ('icdtac', int, '  ', 5,), ('behav' , str,' '*9, 5,),
                ('anbdy', str, '  ', 5,), ('ianbdy', int, '  ', 5,), ('antac', str, '  ', 5,), ('iantac', int, '  ', 5,), ('status', str, '  ', 5,), ),
           2: ( ('cdbdy', str,  ' ', 5,), ('icdbdy', int, '  ', 5,), ('cdtac', str, '  ', 5,), ('icdtac', int, '  ', 5,), ('behav' , str,' '*9, 5,),
                ('anbdy', str, '  ', 5,), ('ianbdy', int, '  ', 5,), ('antac', str, '  ', 5,), ('iantac', int, '  ', 5,), ('status', str, '  ', 5,),
                ('iadj' , int, '  ', 5,), ),
           3: ( ('cdbdy', str,  ' ', 5,), ('icdbdy', int, '  ', 5,), ('cdtac', str, '  ', 5,), ('icdtac', int, '  ', 5,), ('behav' , str, '  ', 5,),
                ('anbdy', str, '  ', 5,), ('ianbdy', int, '  ', 5,), ('antac', str, '  ', 5,), ('iantac', int, '  ', 5,), ('status', str, '  ', 5,), ),
           4: ( ('cdbdy', str,  ' ', 5,), ('icdbdy', int, '  ', 5,), ('cdtac', str, '  ', 5,), ('icdtac', int, '  ', 5,), ('behav' , str, '  ', 5,),
                ('anbdy', str, '  ', 5,), ('ianbdy', int, '  ', 5,), ('antac', str, '  ', 5,), ('iantac', int, '  ', 5,), ('status', str, '  ', 5,),
                ('iadj' , int, '  ', 5,), ),
           5: ( ('cdbdy', str,  ' ', 5,), ('icdbdy', int, '  ', 5,), ('cdtac', str, '  ', 5,), ('icdtac', int, '  ', 5,), ('icdsci', int, '  CDVER  ', 5,),
                ('behav', str, '  ', 5,),
                ('anbdy', str, '  ', 5,), ('ianbdy', int, '  ', 5,), ('antac', str, '  ', 5,), ('iantac', int, '  ', 5,), ('status', str, ' '*16, 5,),
                ('iadj' , int,  ' ', 5,), ),
           6: ( ('cdbdy', str,  ' ', 5,), ('icdbdy', int, '  ', 5,), ('cdtac', str, '  ', 5,), ('icdtac', int, '  ', 5,), ('behav' , str, '  ', 5,),
                ('anbdy', str, '  ', 5,), ('ianbdy', int, '  ', 5,), ('antac', str, '  ', 5,), ('iantac', int, '  ', 5,), ('iansci', int, '  ', 5,),
                ('status',str,' '*9, 5,), ('iadj' , int,  ' ', 5,), ),
           7: ( ('cdbdy', str,  ' ', 5,), ('icdbdy', int, '  ', 5,), ('cdtac', str, '  ', 5,), ('icdtac', int, '  ', 5,), ('behav' , str, '  ', 5,),
                ('anbdy', str, '  ', 5,), ('ianbdy', int, '  ', 5,), ('antac', str, '  ', 5,), ('iantac', int, '  ', 5,), ('iansci', int, '  ', 5,),
                ('status',str, '  ', 5,), ('iadj'  , int, '  ', 5,), ),
           8: ( ('cdbdy', str,  ' ', 5,), ('icdbdy', int, '  ', 5,), ('cdtac', str, '  ', 5,), ('icdtac', int, '  ', 5,), ('behav' , str, '  ', 5,),
                ('anbdy', str, '  ', 5,), ('ianbdy', int, '  ', 5,), ('antac', str, '  ', 5,), ('iantac', int, '  ', 5,), ('iansci', int, '  CDVER  ', 5,),
                ('status',str,' '*16,5,), ('iadj'  , int,  ' ', 5,), ),
           9: ( ('cdbdy', str,  ' ', 5,), ('icdbdy', int, '  ', 5,), ('cdtac', str, '  ', 5,), ('icdtac', int, '  ', 5,), ('icdsci', int, '  CDVER  ', 5,),
                ('behav', str, '  ', 5,),
                ('anbdy', str, '  ', 5,), ('ianbdy', int, '  ', 5,), ('antac', str, '  ', 5,), ('iantac', int, '  ', 5,), ('iansci', int, '  ANSEG  ', 5,),
                ('status',str, '  ', 5,), ('iadj'  , int,  ' ', 5,), ),
          10: ( ('cdbdy', str,  ' ', 5,), ('icdbdy', int, '  ', 5,), ('cdtac', str, '  ', 5,), ('icdtac', int, '  ', 5,), ('icdsci', int, '  CDVER  ', 5,),
                ('behav', str, '  ', 5,),
                ('anbdy', str, '  ', 5,), ('ianbdy', int, '  ', 5,), ('antac', str, '  ', 5,), ('iantac', int, '  ', 5,), ('iansci', int, '  ', 5,),
                ('status',str,' '*9, 5,), ('iadj'  , int,  ' ', 5,), ),
          11: ( ('cdbdy', str,  ' ', 5,), ('icdbdy', int, '  ', 7,), ('cdtac', str, '  ', 5,), ('icdtac', int, '  ', 7,), ('behav' , str,' '*9, 5,),
                ('anbdy', str, '  ', 5,), ('ianbdy', int, '  ', 7,), ('antac', str, '  ', 5,), ('iantac', int, '  ', 7,), ('status', str, '  ', 5,),
                ('iadj' , int, '  ', 7,), ),
          12: ( ('cdbdy', str,  ' ', 5,), ('icdbdy', int, '  ',10,), ('cdtac', str, '  ', 5,), ('icdtac', int, '  ', 5,), ('behav' , str,' '*9, 5,),
                ('anbdy', str, '  ', 5,), ('ianbdy', int, '  ',10,), ('antac', str, '  ', 5,), ('iantac', int, '  ', 5,), ('status', str, '  ', 5,),
                ('iadj' , int, '  ', 5,), ),
          }

# keys are keys in FORMATS dict
FORMAT2INTERS = { 0 : { 'PRASp', 'PRPLx', 'PRPRx', },
                  1 : { 'CDCDx', 'CDPLx', 'SPCDx', 'SPPLx', },
                  2 : { 'SPDCx', 'SPSPx', },
                  3 : { 'CSASx', 'CSPRx', },
                  4 : { 'DKDKL', 'DKDKx', 'DKJCx', 'DKKDx', 'CLJCx', 'P2P2L', 'PTPT2', 'PTPT3', },
                  5 : { 'PLJCx', },
                  6 : { 'DKALp', },
                  7 : { 'CLALp', },
                  8 : { 'DKPLx', },
                  9 : { 'PLPLx', },
                 10 : { 'PLALp', },
                }

# the inter type allowing XL write
XL = {'DKDKx':12, 'DKJCx':12,'SPSPx':11,}

# reverse map to easily get format from an inter type
INTERS2FORMAT = { e:k for k,l in FORMAT2INTERS.items() for e in l}
INTERS2HEADER = { e:k for k,l in HEADER2INTERS.items() for e in l}

# the set of contact written in TNS or STN order
STN_INTERS = { 'CDCDx', 'CDPLx', 'SPPLx', 'SPSPx' }
TNS_INTERS = { 'CSASx', 'CSPRx', 'PRASp', 'PRPLx', 'PRPRx', 'SPCDx', 'SPDCx' }
COO_FIRST  = { 'CSASx', 'CSPRx', 'PRASp', 'PRPLx', 'PRPRx' }
STN_ORDER  = ( (2,'s',) , (0,'t',), (1,'n',), )
TNS_ORDER  = ( (0,'t',) , (1,'n',), (2,'s',), )

