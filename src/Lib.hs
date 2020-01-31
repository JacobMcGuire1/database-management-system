
module Lib

     where

--import NumericPrelude.List.Generic
import Data.List

data Err = TableNameAlreadyExistsError String
    |
    TableNotFoundError String
    deriving Show

data Database = Database {
    dbname :: String,
    tables :: [Table]
} deriving Show

data Table = Table {
    tablename :: String,
    colheaders :: [ColumnHeader],
    rows :: [Row]
} 
instance Show Table where
    show (Table name colheaders rows) = "\n\n Table " ++ show name ++ ": \n\n" ++ show colheaders ++ "\n" ++ show rows

data ColumnHeader = ColumnHeader {
    colname :: String,
    datatype :: String
} 
instance Show ColumnHeader where
    show (ColumnHeader name dtype) = show name ++ ", "

data Row = Row {
    fields :: [DataField]
} 
instance Show Row where
    show (Row fs) = show (head fs) ++ ", " ++ show (tail fs) ++ "\n"

data DataField = StringField {
    strdata :: String
    }
    | IntField {
    intdata :: Int
    }
instance Show DataField where
    show (StringField s) = show s
    show (IntField i) = show i

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--Check if table with same name already exists
addTable :: Database -> Table -> Either Err Database
addTable db tb =
    if nameavailable then Right newdb
    else Left $ TableNameAlreadyExistsError $ tablename tb
    where
        newdb = Database (dbname db) (tables db ++ [tb])
        nameavailable = all (\x -> tablename tb /= tablename x) (tables db)


createTable :: String -> [ColumnHeader] -> Table
createTable name colheaders =
    Table name colheaders []


{-simpleTest :: String -> Int -> Database
simpleTest strarg intarg = do
    sfield <- StringField "strarg"
    ifield <- IntField intarg
    myrow <- Row [sfield, ifield]
    mycolheader <- ColumnHeader "Col 1" "String"
    mycolheader2 <- ColumnHeader "Col 2" "Int"
    mytable <- Table "Table 1" [mycolheader, mycolheader2] [myrow]
    mydb <- Database "Db 1" [mytable]
    mydb-}

simpleTest :: String -> Int -> Database
simpleTest strarg intarg =
    mydb
    where
        sfield = StringField strarg
        ifield = IntField intarg
        myrow = Row [sfield, ifield]
        mycolheader = ColumnHeader "Col 1" "String"
        mycolheader2 = ColumnHeader "Col 2" "Int"
        mytable = Table "Table 1" [mycolheader, mycolheader2] [myrow]
        mydb = Database "Db 1" [mytable]

seeDb :: Database
seeDb =
    mydb
    where
        sfield = StringField "strarg"
        ifield = IntField 23234
        myrow = Row [sfield, ifield]
        mycolheader = ColumnHeader "Col 1" "String"
        mycolheader2 = ColumnHeader "Col 2" "Int"
        mytable = Table "Table 1" [mycolheader, mycolheader2] [myrow]
        mydb = Database "Db 1" [mytable]

--Should return an error as the tabke name already exists
errorTest :: Either Err Database
errorTest = do
    db2 <- addTable db tb
    addTable db2 tb
    where
        tb = createTable "Table 1" []
        db = simpleTest "uigiu" 12

select :: Database -> [String] -> String -> Either Err Table
select db columns tbname =
    Right tb
    where
        tb = head $ filter (\x -> tablename x == tbname) (tables db)

--Returns the table with the selected columns, in the original order.
getColsFromTable :: Table -> [String] -> Table
getColsFromTable tb cols
    | null cols = tb
    | otherwise = Table  (tablename tb) newheaders newrows
    where
        name = tablename tb
        headernames = map colname $ colheaders tb
        indexes = map (\x -> elemIndex x headernames) cols --list of column indexes we want
        oldrows = rows tb
        newrows = map (\x -> Row $ map fst ( filter (\y -> elem (snd y) indexes)(zip (fields x) (map Just [0..])))) oldrows
        newheaders = map fst ( filter (\y -> elem (snd y) indexes)(zip (colheaders tb) (map Just [0..])))

getTableByName :: Database -> String -> Either Err Table
getTableByName db tbname =
    if null tb then Left $ TableNotFoundError $ "Table '" ++ tbname ++ "' not found."
    else Right $ head tb
    where
      tb = filter (\x -> tablename x == tbname) (tables db)

{-makerow :: String -> Int -> Row
makerow strarg intarg = 
    myrow
    where
        sfield = StringField "strarg"
        ifield = IntField intarg
        myrow = Row [sfield, ifield]-}
