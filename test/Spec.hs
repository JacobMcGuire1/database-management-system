import Lib

main :: IO ()
main = do
    putStrLn $ "Should be good: " ++ show (insertInto testTable goodtestRow)
    putStrLn $ "Should be good: " ++ chain [flip insertInto goodtestRow, flip insertInto goodtestRow, flip insertInto goodtestRow] testTable
    putStrLn $ "Should be error: " ++ show (insertInto testTable wrongtypesrow)
    putStrLn $ "Should be error: " ++ show (insertInto testTable manyargsrow)
    putStrLn $ "Should be error: " ++ show (insertInto testTable fewargsrow)
    


--chain :: a -> (a -> Either Err a) -> a
--chain tb f =
  --  case f tb of
   --     Left err -> tb
      --  Right table -> table

--run :: [Table -> Either Err Table] -> Either Err Table -> String
--run fs a =
  --  case a of
     --   Left err -> show err
    --    Right tb -> case length fs of
           -- 1 -> show (head fs tb)
           -- _ -> run (tail fs) (head fs tb)

chain :: Show a => [a -> Either Err a] -> a -> String
chain fs tb =
    case length fs of
        0 -> show tb
        _ -> 
            case head fs tb of
                Left err -> show err
                Right tb2 -> chain (tail fs) tb2



--runInterpreter :: Doc -> Handler RunResponse
--runInterpreter (Doc vs stmts) =
 --   case interpret stmts [(v,0) | v <- vs] of
     --   Left err -> return $ RunFailure (show err)
      --  Right mem -> return $ RunSuccess mem


--Test rows
goodtestRow :: Row
goodtestRow =
    Row [sfield, ifield]
    where
        sfield = StringField "dwad"
        ifield = IntField 4324

wrongtypesrow :: Row
wrongtypesrow =
    Row [ifield, sfield]
    where
        sfield = StringField "dwad"
        ifield = IntField 4324

manyargsrow :: Row
manyargsrow =
    Row [ifield, sfield, ifield, sfield]
    where
        sfield = StringField "dwad"
        ifield = IntField 4324

fewargsrow :: Row
fewargsrow =
    Row []
    where
        sfield = StringField "dwad"
        ifield = IntField 4324

--Test tables
testTable :: Table
testTable =
    Table "Table 2" [mycolheader, mycolheader2] []
    where
        mycolheader = StringColHeader "Col 1"
        mycolheader2 = IntColHeader "Col 2"