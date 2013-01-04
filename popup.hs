{-# LANGUAGE EmptyDataDecls, NamedFieldPuns, NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards                                   #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns  #-}
module Popup (main) where
import Language.Fay.FFI
import Language.Fay.Prelude

main :: Fay ()
main = do
  xhr <- xmlHttpRequest
  open xhr "GET" "http://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=90485e931f687a9b9c2a66bf58a3861a&text=hello%20world&safe_search=1&content_type=1&sort=relevance&per_page=20"
  onload xhr handler
  send xhr
  where
    handler xhr = responseXML xhr >>= showPhotos

data Photo = Photo { farm :: String
                   , server :: String
                   , pid :: String
                   , secret :: String
                   }
instance Foreign Photo

constructImageURL :: Photo -> String
constructImageURL Photo{farm, server, pid, secret} =
  concat [ "http://farm" , farm , ".static.flickr.com/"
         , server , "/" , pid , "_" , secret , "_s.jpg"
         ]

photo :: Element -> Fay Photo
photo e = do
    farm <- get "farm"
    server <- get "server"
    pid <- get "id"
    secret <- get "secret"
    return $ Photo farm server pid secret
  where
    get = getAttribute e

getPhotos :: Element -> Fay [Photo]
getPhotos e =
  getElementsByTagName e "photo" >>= sequence . map photo

photoElem :: Photo -> Fay Element
photoElem p = do
  img <- createElement "image"
  setAttribute img "src" (constructImageURL p)

showPhotos :: Element -> Fay ()
showPhotos e = do
  ps <- getPhotos e
  es <- (sequence . map photoElem) ps
  mapM_ (appendChild body) es

data Element
instance Foreign Element
instance Show Element

getAttribute :: Element -> String -> Fay String
getAttribute =
  ffi "%1.getAttribute(%2)"

getElementsByTagName :: Element -> String -> Fay [Element]
getElementsByTagName =
  ffi "%1.getElementsByTagName(%2)"

createElement :: String -> Fay Element
createElement =
  ffi "document.createElement(%1)"

setAttribute :: Element -> String -> String -> Fay Element
setAttribute =
  ffi "(function(e,n,v){e[n]=v;return e;})(%1,%2,%3)"

body :: Element
body =
  ffi "document.body"

appendChild :: Element -> Element -> Fay ()
appendChild =
  ffi "%1.appendChild(%2)"

data XMLHttpRequest
instance Foreign XMLHttpRequest

xmlHttpRequest :: Fay XMLHttpRequest
xmlHttpRequest =
  ffi "new XMLHttpRequest()"

open :: XMLHttpRequest -> String -> String -> Fay XMLHttpRequest
open =
  ffi "(function(xhr, method, url) { xhr['open'](method, url, true); return xhr; })(%1, %2, %3)"

onload :: XMLHttpRequest -> (XMLHttpRequest -> Fay ()) -> Fay XMLHttpRequest
onload =
  ffi "(function(xhr, handler){xhr['onload']=function(){handler(xhr);};})(%1,%2)"

send :: XMLHttpRequest -> Fay ()
send = ffi "%1['send']()"

responseXML :: XMLHttpRequest -> Fay Element
responseXML =
  ffi "%1['responseXML']"
  