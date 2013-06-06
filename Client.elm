import Graphics.Input as Input
import Http
import WebSocket
import JavaScript.Experimental as JS
import Json
import Window
import Keyboard

(nameInput, username) = Input.field "Your name here"

(chatInput, chats) = Input.field "Type here to chat!"

chatSubmit = keepWhen Keyboard.enter [] chats

type GameState = { name : String,
                   members   : [String],
                   chatlog : [String] }


messages : Signal String -> Signal [String]
messages websock = foldp (\m acc -> if (length m) > 0 then acc ++ [m] else acc) [] websock

others : Signal [String]
others = constant ["just me"]

gameState websock = GameState <~ username
                               ~ others
                               ~ messages websock


display : (Int,Int) -> GameState -> Element -> Element -> Element
display (w,h) gameState name chat = asText gameState `above` name `above` chat


connectWS = WebSocket.connect "ws://localhost:4000" chatSubmit 

main = display <~ Window.dimensions
                ~ (gameState connectWS)
                ~ nameInput
                ~ chatInput
