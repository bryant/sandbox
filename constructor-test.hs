data NJTrip = Bridge String | Cycling Integer | Walking Bool

parseTrip :: NJTrip -> String
parseTrip z = case z of
    Bridge x -> x
    Walking _ -> "walked"
    -- intentionally forget to handle Cycling constructor

main = do
    print . parseTrip $ Bridge "hassenpfeffer"
    print . parseTrip $ Cycling 23
