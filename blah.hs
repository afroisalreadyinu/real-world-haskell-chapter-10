newtype IntOperation a = IntOperation { process :: a -> Either String a }

multiplier = IntOperation $ \arg -> if arg > 10 then (Left "Int too big") else Right (arg * 10)
