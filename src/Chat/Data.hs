{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- | This modules defines a subsite that allows you to insert a chat box on
-- any page of your site. It uses eventsource for sending the messages from
-- the server to the browser.
module Chat.Data where

import           Control.Concurrent.Chan (Chan)
import           Network.Wai.EventSource (ServerEvent)
import           Yesod

-- | Our subsite foundation. We keep a channel of events that all connections
-- will share.
data Chat = Chat (Chan ServerEvent)

-- Now we set up our subsite. The first argument is the subsite, very similar
-- to how we've used mkYesod in the past. The second argument is specific to
-- subsites. What it means here is "the master site must be an instance of
-- YesodChat".
--
-- We define two routes: a route for sending messages from the client to the
-- server, and one for opening up the event stream to receive messages from
-- the server.
mkYesodSubData "Chat" [parseRoutes|
/send SendR POST
/recv ReceiveR GET
|]