
module Lib

     where

--import NumericPrelude.List.Generic
import Data.List
import Data.Typeable


data Err = TableNameAlreadyExistsError String
    |
    TableNotFoundError String
    |
    WrongDataTypeError String
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

data ColumnHeader = StringColHeader {
    colname :: String
    --datatype :: String
    }
    | IntColHeader {
        colname :: String
    }
instance Show ColumnHeader where
    show colheader = show $ colname colheader ++ ", "

data Row = Row {
    fields :: [DataField]
}
instance Show Row where
    --show (Row fs) = --map (\x -> (show x)) fs
    show (Row fs) = show (head fs) ++ ", " ++ (if not (null (tail fs)) then show (Row $ tail fs) else "\n")  --need to fix error

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

--Check if table with same name already exists
addTable :: Database -> Table -> Either Err Database
addTable db tb =
    if nameavailable then Right newdb
    else Left $ TableNameAlreadyExistsError $ tablename tb
    where
        newdb = Database (dbname db) (tables db ++ [tb])
        nameavailable = all (\x -> tablename tb /= tablename x) (tables db)

--Inserts the row into the table if it's eligible.
insertInto :: Table -> Row -> Either Err Table
insertInto tb row =
    if length rowdata /= length headers then Left $ WrongDataTypeError "Wrong number of fields assigned to when inserting a row."
    else do
        checkdata <- mapM checkFieldDataType zipped
        Right $ Table (tablename tb) (colheaders tb) (rows tb ++ [Row checkdata])
    where
        rowdata = fields row
        headers = colheaders tb
        zipped = zip rowdata headers


checkFieldDataType :: (DataField, ColumnHeader) -> Either Err DataField
checkFieldDataType (StringField s, StringColHeader _) = Right $ StringField s
checkFieldDataType (IntField i, IntColHeader _) = Right $ IntField i
checkFieldDataType (a, b) = Left $ WrongDataTypeError $ "Attempted to assign " ++ show (typeOf a) ++ " a field " ++ show (typeOf b) ++ " the wrong data type."

--Creates table based on list of constraints
createTable :: String -> [ColumnHeader] -> Table
createTable name colheaders =
    Table name colheaders []

simpleTest :: String -> Int -> Database
simpleTest strarg intarg =
    mydb
    where
        sfield = StringField strarg
        ifield = IntField intarg
        myrow = Row [sfield, ifield]
        mycolheader = StringColHeader "Col 1"
        mycolheader2 = IntColHeader "Col 2"
        mytable = Table "Table 1" [mycolheader, mycolheader2] [myrow]
        mydb = Database "Db 1" [mytable]





seeDb :: Database
seeDb = simpleTest "strarg" 23234

--Should return an error as the tabke name already exists
errorTest :: Either Err Database
errorTest = do
    db2 <- addTable db tb
    addTable db2 tb
    where
        tb = createTable "Table 1" []
        db = simpleTest "uigiu" 12
