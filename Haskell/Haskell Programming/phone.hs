module Phone where

{- 
Phone Layout
+--------------------------+
| 1      | 2 ABC  | 3 DEF  |
|--------+--------+--------|
| 4 GHI  | 5 JKL  | 6 MNO  |
|--------+--------+--------|
| 7 PQRS | 8 TUV  | 9 WXYZ |
|--------+--------+--------|
| * ^    | 0 + _  | # . ,  |
+--------------------------+
-}

data DaPhone = ["+ ","","ABC","DEF","GHI","JKL","MNO","PQRS","TUV","WXYZ","^",".,"]

convo = ["Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol ok. Have u ever tasted alcohol lol",
         "Lol ya",
         "Wow ur cool haha. Ur turn",
         "Ok. Do u think i am pretty Lol",
         "Lol ya",
         "Haha thanks just make sure rofl ur turn"]

-- valid Buttons = "1234567890*#"
type Digit = Char

-- valid presses = [1..4]
type Presses = Int

cellPhonesDeas :: DaPhone -> String -> [(Digit, Presses)]

findButton c (x:xs) | elem c x  = 1
                    | otherwise = 1 + findButton c xs

transButton n | n == "10" = "0"
              | n == "11" = "#"
              | otherwise = n

countPress c (x:xs) | c == ' '  = 1
                    | c == x    = 1
                    | otherwise = 1 + countPress c xs



{-
> map transButton $ map show $ map (\ x -> findButton (toUpper x) layout) "Wanna play 20 questions."
["9","2","6","6","2","0","7","5","2","9","0","2","0","0","7","8","3","7","8","4","6","6","7","#"]
-}