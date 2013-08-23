import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import qualified Data.Text.Read as Read

main = do 
    raw <- IO.getContents
    print . sum . map (readInt . Read.decimal) . Text.words $ raw
    where readInt (Right (n, _)) = n
