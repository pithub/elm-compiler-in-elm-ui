module Terminal.Command exposing
    ( LocalState
    , Output
    , State
    , ask
    , clearInput
    , clearPrompt
    , clearPutLine
    , clearStdOut
    , getDurationSinceLastInput
    , getInput
    , getLine
    , getLineWithInitial
    , getText
    , gotInput
    , gotLine
    , initialState
    , lensInput
    , lensPrompt
    , lensStdIn
    , lensStdOut
    , putDoc
    , putLine
    , putTemporary
    , setCurrentInput
    , setInput
    , setNextInput
    )

import Builder.Generate as Generate
import Compiler.Reporting.Doc as D
import Extra.System.IO as IO
import Extra.Type.Lens exposing (Lens)
import Extra.Type.List exposing (TList)
import Global
import Time



-- PUBLIC STATE


type alias State a g h =
    Generate.State a (LocalState a g h) g h


type LocalState a g h
    = LocalState
        -- stdIn
        (TList (String -> IO a g h ()))
        -- stdOut
        (TList Output)
        -- prompt
        String
        -- input
        String
        -- waiting
        (Maybe String)
        -- inputTime
        (Maybe Int)


initialState : LocalState a g h
initialState =
    LocalState
        -- stdIn
        []
        -- stdOut
        []
        -- prompt
        ""
        -- input
        ""
        -- waiting
        Nothing
        -- inputTime
        Nothing


lensStdIn : Lens (State a g h) (TList (String -> IO a g h ()))
lensStdIn =
    { getter = \(Global.State _ _ _ _ _ (LocalState x _ _ _ _ _) _ _) -> x
    , setter = \x (Global.State a b c d e (LocalState _ bi ci di ei fi) g h) -> Global.State a b c d e (LocalState x bi ci di ei fi) g h
    }


lensStdOut : Lens (State a g h) (TList Output)
lensStdOut =
    { getter = \(Global.State _ _ _ _ _ (LocalState _ x _ _ _ _) _ _) -> x
    , setter = \x (Global.State a b c d e (LocalState ai _ ci di ei fi) g h) -> Global.State a b c d e (LocalState ai x ci di ei fi) g h
    }


lensPrompt : Lens (State a g h) String
lensPrompt =
    { getter = \(Global.State _ _ _ _ _ (LocalState _ _ x _ _ _) _ _) -> x
    , setter = \x (Global.State a b c d e (LocalState ai bi _ di ei fi) g h) -> Global.State a b c d e (LocalState ai bi x di ei fi) g h
    }


lensInput : Lens (State a g h) String
lensInput =
    { getter = \(Global.State _ _ _ _ _ (LocalState _ _ _ x _ _) _ _) -> x
    , setter = \x (Global.State a b c d e (LocalState ai bi ci _ ei fi) g h) -> Global.State a b c d e (LocalState ai bi ci x ei fi) g h
    }


lensWaiting : Lens (State a g h) (Maybe String)
lensWaiting =
    { getter = \(Global.State _ _ _ _ _ (LocalState _ _ _ _ x _) _ _) -> x
    , setter = \x (Global.State a b c d e (LocalState ai bi ci di _ fi) g h) -> Global.State a b c d e (LocalState ai bi ci di x fi) g h
    }


lensInputTime : Lens (State a g h) (Maybe Int)
lensInputTime =
    { getter = \(Global.State _ _ _ _ _ (LocalState _ _ _ _ _ x) _ _) -> x
    , setter = \x (Global.State a b c d e (LocalState ai bi ci di ei _) g h) -> Global.State a b c d e (LocalState ai bi ci di ei x) g h
    }



-- PRIVATE IO


type alias IO a g h v =
    IO.IO (State a g h) v



-- STDIN


getLine : IO a g h String
getLine =
    getLineWithInitial "" ""


getLineWithInitial : String -> String -> IO a g h String
getLineWithInitial prompt prefill =
    IO.bindSequence
        [ IO.putLens lensPrompt prompt
        , IO.putLens lensInput prefill
        ]
        (IO.liftCont <|
            \cont ->
                IO.bind (IO.getLens lensWaiting) <|
                    \waiting ->
                        case waiting of
                            Just line ->
                                IO.bindSequence
                                    [ IO.putLens lensWaiting Nothing ]
                                    (cont line)

                            Nothing ->
                                IO.modifyLens lensStdIn <| \cs -> cont :: cs
        )


gotLine : String -> IO a g h ()
gotLine line =
    IO.bind (IO.getLens lensStdIn) <|
        \stdIn ->
            case stdIn of
                [] ->
                    IO.noOp

                continuation :: cs ->
                    IO.bindSequence
                        [ IO.putLens lensStdIn cs
                        , setInputTime
                        ]
                        (continuation line)



-- STDOUT


type Output
    = Permanent String
    | Temporary String


getText : Output -> String
getText output =
    case output of
        Permanent string ->
            string

        Temporary string ->
            string


putLine : String -> IO a g h ()
putLine line =
    putOutput <| Permanent line


putTemporary : String -> IO a g h ()
putTemporary line =
    putOutput <| Temporary line


putDoc : D.Doc -> IO a g h ()
putDoc doc =
    putOutput <| Permanent (D.toString doc)


putOutput : Output -> IO a g h ()
putOutput output =
    IO.modifyLens lensStdOut <|
        \stdOut ->
            case stdOut of
                (Temporary _) :: rest ->
                    output :: rest

                _ ->
                    output :: stdOut


clearStdOut : IO a g h ()
clearStdOut =
    IO.putLens lensStdOut []


clearPutLine : String -> IO a g h ()
clearPutLine string =
    IO.sequence
        [ clearStdOut
        , putLine string
        ]



-- PROMPT


clearPrompt : IO a g h ()
clearPrompt =
    IO.putLens lensPrompt ""



-- ASK


ask : D.Doc -> IO a g h Bool
ask doc =
    IO.bind clearInput <|
        \_ ->
            IO.bind (putDoc doc) <|
                \_ ->
                    askHelp


askHelp : IO a g h Bool
askHelp =
    IO.bind (getLineWithInitial "?\u{2000}" "") <|
        \input ->
            IO.bind clearInput <|
                \_ ->
                    case input of
                        "" ->
                            IO.return True

                        "Y" ->
                            IO.return True

                        "y" ->
                            IO.return True

                        "n" ->
                            IO.bindSequence
                                [ clearInputTime ]
                                (IO.return False)

                        _ ->
                            IO.bind (putTemporary "Must type 'y' for yes or 'n' for no: ") <|
                                \_ ->
                                    askHelp



-- INPUT


clearInput : IO a g h ()
clearInput =
    setInput ""


setInput : String -> IO a g h ()
setInput input =
    IO.putLens lensInput input


getInput : IO a g h String
getInput =
    IO.getLens lensInput


gotInput : IO a g h ()
gotInput =
    IO.bind (IO.getLens lensInput) gotLine



-- WAITING


setCurrentInput : String -> IO a g h ()
setCurrentInput input =
    gotLine input


setNextInput : String -> IO a g h ()
setNextInput input =
    IO.putLens lensWaiting (Just input)



-- INPUT TIME


clearInputTime : IO a g h ()
clearInputTime =
    IO.putLens lensInputTime Nothing


setInputTime : IO a g h ()
setInputTime =
    IO.bind IO.now <|
        \now ->
            IO.putLens lensInputTime (Just (Time.posixToMillis now))


getDurationSinceLastInput : IO a g h (Maybe Int)
getDurationSinceLastInput =
    IO.bind (IO.getLens lensInputTime) <|
        \inputTime ->
            case inputTime of
                Just lastInputTime ->
                    IO.bind IO.now <|
                        \now ->
                            IO.return (Just (Time.posixToMillis now - lastInputTime))

                Nothing ->
                    IO.return Nothing
