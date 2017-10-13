module WordReferenceReq
	(
		traduci
	) where

import Network.HTTP
import Network.URI
import Text.ParserCombinators.Parsec
import Codec.Binary.UTF8.String

traduci x = getTable x >>= estrai

estrai :: Either ParseError String -> IO String
estrai (Left _) = return "<!DOCTYPE html><html><body><h1>Errore</h1><p>Parola non trovata...</p></body></html>"
estrai (Right x) = return x

-- getTable: download and parse the page for word x, returning only the table (= after parsing the page)
getTable x = getpage x >>= \html -> return (parse wrParse "GHE NE CALCOSSA CHE NO VA!"  html)

-- get the specified page (ex: "http://www.wordreference.com/enit/work")
getpage x = simpleHTTP (request x) >>= getResponseBody >>= \x -> return $ decodeString x

request x = Request (myUri x) GET myHeaders myBody
myUri x = URI "http:" (Just $ URIAuth [] "www.API-URL.com" ":80") (myUriPath++x) [] []
myHeaders = [Header HdrUserAgent "Mozilla/5.0 (iPad; CPU OS 3_2_1 like Mac OS X; en-us) AppleWebKit/534.34 (KHTML, like Gecko) Mobile/7B405"]
myUriPath = "/enit/"    -- :t uriPath
myBody = ""

-- Main parser for the html page.
wrParse:: Parser String
wrParse = do
			first <- firstParser
			second <- skipToTable
			skipFirstLine
			third <- secondParser
			return (first ++ second ++ third ++ "</body></html>") -- magari dopo first e second mettere \r\n

firstParser :: Parser String
firstParser = do
				try (string "<body>" >> return "<body>\r\n")
                <|> do
						a <- anyChar
						b <- firstParser
						return (a:b)

skipToTable :: Parser String
skipToTable = do
                try (string "<div id=\"articleWRD\">" >>= return)
                <|> (anyChar >> skipToTable)

skipFirstLine :: Parser String
skipFirstLine = do
                try (string "</div>" >>= return)
                <|> (anyChar >> skipFirstLine)

secondParser :: Parser String
secondParser = do
					try (string "<table class='WRreporterror'>" >> return [])
               		<|> do
							a <- anyChar
							b <- secondParser
							return (a:b)

-- return the next 40 characters
prox :: Parser [Char]
prox = do
          count 40 anyChar
