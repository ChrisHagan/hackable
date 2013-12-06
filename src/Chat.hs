{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
-- | This modules defines a subsite that allows you to insert a chat box on
-- any page of your site. It uses eventsource for sending the messages from
-- the server to the browser.
module Chat
    ( module Chat
    , module Chat.Data
    ) where

import           Blaze.ByteString.Builder.Char.Utf8 (fromText)
import           Chat.Data
import           Control.Concurrent.Chan            (dupChan, writeChan)
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import           Network.Wai.EventSource            (ServerEvent (..),
                                                     eventSourceAppChan)
import           Yesod

-- | We need to know how to check if a user is logged in and how to get
-- his/her username (for printing messages).
class (Yesod master, RenderMessage master FormMessage)
        => YesodChat master where
    getUserName :: HandlerT master IO Text
    isLoggedIn :: HandlerT master IO Bool

type ChatHandler a =
    forall master. YesodChat master =>
    HandlerT Chat (HandlerT master IO) a

-- | Get a message from the user and send it to all listeners.
postSendR :: ChatHandler ()
postSendR = do
    from <- lift getUserName

    -- Note that we're using GET parameters for simplicity of the Ajax code.
    -- This could easily be switched to POST. Nonetheless, our overall
    -- approach is still RESTful since this route can only be accessed via a
    -- POST request.
    body <- lift $ runInputGet $ ireq textField "message"

    -- Get the channel
    Chat chan <- getYesod

    -- Send an event to all listeners with the user's name and message.
    liftIO $ writeChan chan $ ServerEvent Nothing Nothing $ return $
        fromText from <> fromText ": " <> fromText body

-- | Send an eventstream response with all messages streamed in.
getReceiveR :: ChatHandler ()
getReceiveR = do
    -- First we get the main channel
    Chat chan0 <- getYesod

    -- We duplicated the channel, which allows us to create broadcast
    -- channels.
    chan <- liftIO $ dupChan chan0

    -- Now we use the event source API. eventSourceAppChan takes two parameters:
    -- the channel of events to read from, and the WAI request. It returns a
    -- WAI response, which we can return with sendWaiResponse.
    req <- waiRequest
    res <- liftResourceT $ eventSourceAppChan chan req
    sendWaiResponse res

-- | Provide a widget that the master site can embed on any page.
chatWidget :: YesodChat master
           => (Route Chat -> Route master)
           -> WidgetT master IO ()
-- This toMaster argument tells us how to convert a Route Chat into a master
-- route. You might think this is redundant information, but taking this
-- approach means we can have multiple chat subsites in a single site.
chatWidget toMaster = do
    -- Get some unique identifiers to help in creating our HTML/CSS. Remember,
    -- we have no idea what the master site's HTML will look like, so we
    -- should not assume we can make up identifiers that won't be reused.
    -- Also, it's possible that multiple chatWidgets could be embedded in the
    -- same page.
    chat <- newIdent   -- the containing div
    output <- newIdent -- the box containing the messages
    input <- newIdent  -- input field from the user

    ili <- handlerToWidget isLoggedIn  -- check if we're already logged in
    if ili
        then do
            -- Logged in: show the widget
            [whamlet|
                <div ##{chat}>
                    <h2>Chat
                    <div ##{output}>
                    <input ##{input} type=text placeholder="Enter Message">
            |]
            -- Just some CSS
            toWidget [lucius|
                ##{chat} {
                    position: absolute;
                    top: 2em;
                    right: 2em;
                }
                ##{output} {
                    width: 200px;
                    height: 300px;
                    border: 1px solid #999;
                    overflow: auto;
                }
            |]
            -- And now that Javascript
            toWidgetBody [julius|
                // Set up the receiving end
                var output = document.getElementById(#{toJSON output});
                var src = new EventSource("@{toMaster ReceiveR}");
                src.onmessage = function(msg) {
                    // This function will be called for each new message.
                    var p = document.createElement("p");
                    p.appendChild(document.createTextNode(msg.data));
                    output.appendChild(p);
                
                    // And now scroll down within the output div so the most recent message
                    // is displayed.
                    output.scrollTop = output.scrollHeight;
                };
                
                // Set up the sending end: send a message via Ajax whenever the user hits
                // enter.
                var input = document.getElementById(#{toJSON input});
                input.onkeyup = function(event) {
                    var keycode = (event.keyCode ? event.keyCode : event.which);
                    if (keycode == '13') {
                        var xhr = new XMLHttpRequest();
                        var val = input.value;
                        input.value = "";
                        var params = "?message=" + encodeURI(val);
                        xhr.open("POST", "@{toMaster SendR}" + params);
                        xhr.send(null);
                    }
                }
            |]
        else do
            -- User isn't logged in, give a not-logged-in message.
            master <- getYesod
            [whamlet|
                <p>
                    You must be #
                    $maybe ar <- authRoute master
                        <a href=@{ar}>logged in
                    $nothing
                        logged in
                    \ to chat.
            |]

instance YesodChat master => YesodSubDispatch Chat (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesChat)
