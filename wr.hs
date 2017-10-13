-- An interface for www.wordreference.com, without using the API..
-- It search a word on this site, parsing the output (for delete
-- advertisements) and opening this page with google's WebKit.

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import WordReferenceReq as WR
import Control.Monad.Trans

main = do

	_ <- initGUI

	-- pix <- pixbufNewFromFile "myIcon.jpg"

	resWin  <- windowNew  -- result window
	entryWin <- windowNew -- window for text entry
	--windowSetIcon entryWin (Just pix)
	--windowSetIcon resWin (Just pix)

	sw <- scrolledWindowNew Nothing Nothing -- container for webKit (with scrolling)
	wv <- webViewNew

	ent <- entryNew
	ent `onEntryActivate` do
								s <- entryGetText ent
								widgetDestroy entryWin
								string <- traduci s -- traduci: get the specified page and parse it.
								webViewLoadHtmlString wv string ("http://www.API-URL.com/enit/"++s)
								widgetShowAll resWin

	containerAdd entryWin ent
	widgetShowAll entryWin

	windowSetTitle resWin "WordReference Interface"
	windowSetTitle entryWin "Eng -> Ita"
	set resWin
		[ containerChild       := sw
		, windowDefaultWidth   := 700
		, windowDefaultHeight  := 400
		, containerBorderWidth := 2
		]

	-- set the child of the scrolled windows to the webview.
	set sw [ containerChild := wv ]

	onDestroy resWin mainQuit
	on resWin keyPressEvent $ tryEvent $ do
										"Escape" <- eventKeyName
										liftIO $ widgetDestroy resWin
										liftIO $ mainQuit
	on entryWin keyPressEvent $ tryEvent $ do
										"Escape" <- eventKeyName
										liftIO $ widgetDestroy entryWin
										liftIO $ mainQuit
	mainGUI
