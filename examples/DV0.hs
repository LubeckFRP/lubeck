

testSimple1, testSimple2, testSimple3 :: Drawing
testSimple1 = snd $ simpleLinePlot showJS showJS
  id id id id 10 10
  [(0.2, 0.3), (0.4, 1), (1,1)]

testSimple2 = snd $ simpleLinePlot showJS showJS
  id id id id 10 10
  (zip [-10,1,3,4,7,15] [10,20,0,300,30,50])

testSimple3 = snd $ simpleLinePlot showJS showJS
  utcTimeToApproxReal realToApproxUTCTime id id 10 10
  (zip
    (fmap ((\(Just x) -> x) . parseDateAndTimeToUTC)
      [ "1400-01-01T06:00:00"
      , "1900-12-24T06:53:00"
      , "2016-02-03T06:53:16"
      ])
    [10,15,-1]
    )
