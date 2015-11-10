import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker" ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                          then Right code
                          else Left $ "Locker" ++ show lockerNumber ++ "is already taken"

lockers :: LockerMap
lockers = Map.fromList [(100, (Taken, "ZD39I"))
                       ,(101, (Free,  "JAH3I"))
                       ,(102, (Free,  "NSDS3"))
                       ,(103, (Free,  "983JJ"))
                       ,(104, (Taken, "99292"))
                       ,(105, (Taken, "ISOA9"))]