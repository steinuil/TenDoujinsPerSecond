open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
open System.Runtime.CompilerServices
open System.IO
open System.Linq
open GameStuff
open Microsoft.Xna.Framework.Audio


let gameName = "10 Doujins Per Second"


[<Struct; IsReadOnly>]
type KeyboardKeys =
    { PrevPressed: Set<Keys>
      Pressed: Set<Keys> }

    member this.IsJustPressed key =
        this.Pressed.Contains key
        && not (this.PrevPressed.Contains key)

    member this.IsJustReleased key =
        not (this.Pressed.Contains key)
        && this.PrevPressed.Contains key

    member this.IsPressed key = this.Pressed.Contains key

    member prev.Update =
        { Pressed =
            Keyboard.GetState().GetPressedKeys()
            |> Set.ofArray
          PrevPressed = prev.Pressed }

    static member empty =
        { Pressed = Set.empty
          PrevPressed = Set.empty }


type MouseEvents =
    { Prev: MouseState
      Current: MouseState }

    member this.HasJustClickedLeft =
        this.Current.LeftButton = ButtonState.Pressed
        && this.Prev.LeftButton = ButtonState.Released

    member this.HasJustReleasedLeft =
        this.Current.LeftButton = ButtonState.Released
        && this.Prev.LeftButton = ButtonState.Pressed

    member this.IsPressedLeft =
        this.Current.LeftButton = ButtonState.Pressed

    member this.X = this.Current.X
    member this.Y = this.Current.Y

    member this.Position = Point(this.Current.X, this.Current.Y)

    member prev.Update =
        { Prev = prev.Current
          Current = Mouse.GetState() }

    static member Initialize() =
        let state = Mouse.GetState()
        { Prev = state; Current = state }


type TenDoujinsPerSecondGame() as this =
    inherit Game()

    let graphics =
        new GraphicsDeviceManager(this, PreferredBackBufferWidth = 1024, PreferredBackBufferHeight = 768)

    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    let mutable keys = KeyboardKeys.empty
    let mutable mouse = MouseEvents.Initialize()

    let mutable font = Unchecked.defaultof<SpriteFont>

    let mutable table = Unchecked.defaultof<_>
    let mutable arrow = Unchecked.defaultof<_>
    let mutable clickables = Unchecked.defaultof<_>
    let mutable box = Unchecked.defaultof<_>
    let mutable background = Unchecked.defaultof<_>
    let mutable bigsight = Unchecked.defaultof<_>
    let mutable backgroundDark = Unchecked.defaultof<_>

    let mutable creditsScreen = Unchecked.defaultof<_>
    let mutable splashScreen = Unchecked.defaultof<_>

    let mutable black = Unchecked.defaultof<_>
    let mutable pink = Unchecked.defaultof<_>
    // let mutable green = Unchecked.defaultof<_>
    let mutable gray = Unchecked.defaultof<_>

    let mutable clock = Unchecked.defaultof<_>

    // let mutable draggingPink = None

    // let mutable drag = None

    // let mutable doujin = Unchecked.defaultof<_>

    // let mutable openDoujinshiKind = Some ThinBook


    // SOUNDS
    let mutable loopMusicFile = Unchecked.defaultof<_>
    let mutable loopMusic = Unchecked.defaultof<_>

    let mutable clickSound = Unchecked.defaultof<_>
    let mutable unclickSound = Unchecked.defaultof<_>

    let mutable kachingSound = Unchecked.defaultof<_>
    let mutable plingSound = Unchecked.defaultof<_>

    // let mutable whi


    // Objects
    let mutable customers: Customer [] = Unchecked.defaultof<_>
    let mutable doujinshi: Doujinshi [] = Unchecked.defaultof<_>
    let mutable settings: Settings = Unchecked.defaultof<_>

    let mutable nesoberiSold = false


    let mutable state: GameStateMachine = Unchecked.defaultof<_>

    let rand =
        let seed = System.Security.Cryptography.RandomNumberGenerator.GetInt32(Int32.MaxValue)
        Random(seed)


    // let mutable doujinshiRotations = [| 0.f; 0.f; 0.f; 0.f |]


    // let mutable activeCustomer = 0
    // let mutable customerAnimation = None

    let customerBoundingBox = Rectangle(200, 0, 429, 685)

    let popUpBoundingBox = Rectangle(508, 30, 517, 430)

    // let doujinPositions =
    //     [| Point(600, 95)
    //        Point(600, 260)
    //        Point(800, 95)
    //        Point(800, 260) |]

    let thinBooksStackBoundingBox = Rectangle(50, 510, 200, 231)
    let musicStackBoundingBox = Rectangle(288, 567, 153, 177)
    let nuigurumiStackBoundingBox = Rectangle(674, 489, 282, 257)

    let buttonBoundingBox = Rectangle(360, 600 - 25, 1024 - 360 * 2, 65)

    let mutable customerRoundTimeMs = 10_000
    let mutable dayRoundTimeMs = 30_000

    let mutable prevCustomerBodyParts  = [||]
    let mutable customerBodyParts = [||]

    let introTotalTime = 20_000

    let mutable score = 0
    let winningScore = 9

    // let mutable greetingIndex = 0
    // let mutable dialogueKind = Greeting


    // Helpers
    let loadTexture dir name =
        this.Content.Load<Texture2D>(Path.Combine(dir, name))

    let loadSound dir name =
        this.Content.Load<SoundEffect>(Path.Combine(dir, name))

    let draw (texture: Texture2D) (rect: Rectangle) =
        spriteBatch.Draw(texture, rect, Color.White)

    let drawRotated (texture: Texture2D) (rect: Rectangle) (rot: float32) =
        spriteBatch.Draw(
            texture,
            Rectangle(rect.Center.X, rect.Center.Y, rect.Width, rect.Height),
            Nullable<Rectangle>(),
            Color.White,
            MathHelper.ToRadians(rot),
            Vector2(float32 texture.Width / 2.f, float32 texture.Height / 2.f),
            SpriteEffects.None,
            1.f
        )

    let drawStringScale (str: string) (point: Point) scale =
        spriteBatch.DrawString(
            font,
            str,
            Vector2(float32 point.X, float32 point.Y),
            Color.White,
            0.f,
            Vector2.Zero,
            Vector2(scale, scale),
            SpriteEffects.None,
            1.f
        )

    let makeColor color =
        let texture = new Texture2D(graphics.GraphicsDevice, 1,1)
        texture.SetData([| color |])
        texture

    let drawCustomer (customer: Customer) (parts: int[]) offsetX alpha =
        let boundingBox =
            Rectangle(Point(customerBoundingBox.X - offsetX, customerBoundingBox.Y), customerBoundingBox.Size)
        let color = Color.White * alpha

        if customer.BodyParts.Length = 0 then
            spriteBatch.Draw(customer.Texture, boundingBox, color)
        else
            spriteBatch.Draw(customer.BodyParts.[0].[parts.[0]], boundingBox, color)
            spriteBatch.Draw(customer.Texture, boundingBox, color)
            for i in 1 .. parts.Length - 1 do
                let part = parts.[i]
                spriteBatch.Draw(customer.BodyParts.[i].[part], boundingBox, color)

    let drawDialogue (customer: CustomerState) =
        let dialogue =
            match customer.Dialogue.State with
            | Greeting ->
                customer.Customer.Dialogue.Greetings.[customer.Dialogue.GreetingIndex]
            | PositiveDialogue ->
                customer.Customer.Dialogue.Positive
            | NegativeDialogue _ ->
                customer.Customer.Dialogue.Negative

        let dialogueSize = font.MeasureString(dialogue)

        spriteBatch.Draw(black, Rectangle(12, 400, int dialogueSize.X + 16, int dialogueSize.Y + 8), Color.White)
        spriteBatch.DrawString(font, dialogue, Vector2(20.f, 404.f), Color.White)

    let drawStringCentered (str: string) height color scale =
        let size = font.MeasureString(str)
        spriteBatch.DrawString(
            font,
            str,
            Vector2(1024.f / 2.f, float32 height),
            color,
            0.f,
            Vector2(size.X / 2.f, 0.f),
            Vector2(scale, scale),
            SpriteEffects.None,
            1.f
        )

    let shuffleArray (a: 'T []) =
        a.OrderBy(fun _ -> rand.Next()).ToArray()

    let drawButton str =
        let color = if buttonBoundingBox.Contains(mouse.Position) then gray else black
        draw color buttonBoundingBox
        drawStringCentered str 600 Color.White 1.f

    // Initialization
    do
        this.IsMouseVisible <- true
        this.Content.RootDirectory <- "Content"

    override _.Initialize() =
        this.Window.Title <- gameName

        base.Initialize()


    override _.LoadContent() =
        spriteBatch <- new SpriteBatch(graphics.GraphicsDevice)

        font <- this.Content.Load<SpriteFont>("sharp-pc3k")
        font.LineSpacing <- 20

        settings <- Settings.Read()

        customers <- settings.LoadCustomers loadTexture
        doujinshi <- settings.LoadDoujinshi loadTexture

        settings.Nesoberi.Texture <- loadTexture "Doujinshi" "nuigurumi_kusodeka"

        table <- loadTexture "Table" "table"
        arrow <- loadTexture "Table" "arrow"
        clickables <- loadTexture "Table" "clickables"
        box <- loadTexture "Table" "box"
        background <- loadTexture "Table" "bg"
        bigsight <- loadTexture "Menus" "bigsight"
        backgroundDark <- loadTexture "Table" "bgdark"

        creditsScreen <- loadTexture "Menus" "credits"
        splashScreen <- loadTexture "Menus" "splashscreen"

        black <- makeColor Color.Black
        pink <- makeColor Color.HotPink
        gray <- makeColor Color.SlateGray

        loopMusicFile <- loadSound "Sounds" "badloop2wav"
        loopMusic <- loopMusicFile.CreateInstance()
        loopMusic.Volume <- 0.4f

        clickSound <- loadSound "Sounds" "mousedown"
        unclickSound <- loadSound "Sounds" "mouseup"

        kachingSound <- loadSound "Sounds" "kaching"
        plingSound <- loadSound "Sounds" "pling"

        clock <- seq { for i in 0 .. 9 -> loadTexture "Clock" (sprintf "clock%d" i) } |> Seq.toArray

        settings.BullshitReasons <- settings.BullshitReasons |> shuffleArray

        state <- SplashScreen 3000

        base.LoadContent()


    override _.Update(gameTime: GameTime) =
        keys <- keys.Update
        mouse <- mouse.Update

        if mouse.HasJustClickedLeft then
            clickSound.Play() |> ignore
        else if mouse.HasJustReleasedLeft then
            unclickSound.Play() |> ignore

        match state with
        | Day(dayNumber, CustomersQueue queueState) when queueState.TimeLeft <= 0 ->
            if dayNumber >= 3 then
                state <- EndScreen
            else
                state <- Interim dayNumber

        | Day(dayNumber, CustomerTransition transition) when transition.TransitionMs <= 0 ->
            let queueState =
                { ActiveCustomer =
                    { Customer = transition.NextCustomer
                      Dialogue =
                        { GreetingIndex = rand.Next(0, transition.NextCustomer.Dialogue.Greetings.Length)
                          State = Greeting } }
                  ActiveCustomerTimeLeft = customerRoundTimeMs
                  CustomersBag = transition.CustomersBag
                  DoujinshiBox = None
                  TimeLeft = transition.TimeLeft }

            state <- Day(dayNumber, CustomersQueue queueState)

        | Day(dayNumber, CustomerTransition transition) ->
            let transition =
                { transition with
                    TransitionMs = transition.TransitionMs - gameTime.ElapsedGameTime.Milliseconds
                    TimeLeft = transition.TimeLeft - gameTime.ElapsedGameTime.Milliseconds }

            state <- Day(dayNumber, CustomerTransition transition)

        | Day(dayNumber, CustomersQueue queueState) when queueState.ActiveCustomerTimeLeft <= 0 ->
            let nextCustomer, customersBag =
                if queueState.CustomersBag.Length = 0 then
                    let customerBag =
                        settings.LeftOverCustomerBag
                        |> shuffleArray
                        |> Array.map (fun name -> customers |> Seq.find (fun c -> c.Name = name))
                    Array.head customerBag, Array.tail customerBag
                else
                    Array.head queueState.CustomersBag, Array.tail queueState.CustomersBag

            prevCustomerBodyParts <- customerBodyParts
            customerBodyParts <-
                nextCustomer.BodyParts |> Array.map (fun parts -> rand.Next(0, parts.Length))

            let transition =
                { PreviousCustomer = Some { queueState.ActiveCustomer with Dialogue = { queueState.ActiveCustomer.Dialogue with State = NegativeDialogue 0 } }
                  NextCustomer = nextCustomer
                  CustomersBag = customersBag
                  TransitionMs = 1000
                  TimeLeft = queueState.TimeLeft }

            state <- Day(dayNumber, CustomerTransition transition)
            ()

        | Day(dayNumber, CustomersQueue queueState) ->
            // Check click on doujinshi box
            let doujinshiBox, dropResult =
                match queueState.DoujinshiBox with
                | Some { Kind = kind; DragState = None } when mouse.HasJustClickedLeft && popUpBoundingBox.Contains(mouse.Position) ->
                    match
                        doujinshi |> Seq.tryFind (fun p ->
                            p.Kind = kind && Rectangle(p.Position, p.Texture.Bounds.Size).Contains(mouse.Position))
                    with
                    | None ->
                        queueState.DoujinshiBox, None
                    | Some d ->
                        let dragState =
                            { Doujinshi = d
                              Offset = Point(mouse.X - d.Position.X, mouse.Y - d.Position.Y) }
                        Some { Kind = kind; DragState = Some dragState }, None

                | Some { Kind = kind; DragState = Some dragState } when not mouse.IsPressedLeft ->
                    if customerBoundingBox.Contains(mouse.Position) then
                        if queueState.ActiveCustomer.Customer.Likes |> Seq.contains dragState.Doujinshi.Tag then
                            if kind = Nesoberi then
                                nesoberiSold <- true

                            None, Some LikesDoujinshi
                        else
                            let doujinshiIndex = doujinshi |> Seq.tryFindIndex (fun d -> d.Name = dragState.Doujinshi.Name) |> Option.defaultValue -1
                            if
                                queueState.ActiveCustomer.Customer.Name = "Menhera"
                                && queueState.ActiveCustomer.Dialogue.GreetingIndex = doujinshiIndex
                            then
                                None, Some LikesDoujinshi
                            else
                                Some { Kind = kind; DragState = None }, Some (DamageTimeMs 1000)
                    else
                        Some { Kind = kind; DragState = None }, None

                | _ when mouse.HasJustClickedLeft && thinBooksStackBoundingBox.Contains(mouse.Position) ->
                    // doujinshiRotations <- doujinshiRotations |> Array.map (fun _ -> rand.NextSingle() * 10.f - 5.f)
                    Some { Kind = ThinBook; DragState = None }, None

                | _ when mouse.HasJustClickedLeft && musicStackBoundingBox.Contains(mouse.Position) ->
                    // doujinshiRotations <- doujinshiRotations |> Array.map (fun _ -> rand.NextSingle() * 10.f - 5.f)
                    Some { Kind = Music; DragState = None }, None

                | _ when mouse.HasJustClickedLeft && nuigurumiStackBoundingBox.Contains(mouse.Position) ->
                    // doujinshiRotations <- doujinshiRotations |> Array.map (fun _ -> rand.NextSingle() * 10.f - 5.f)
                    Some { Kind = Nuigurumi; DragState = None }, None

                | _ when not nesoberiSold && mouse.HasJustClickedLeft && settings.Nesoberi.Bounds.Contains(mouse.Position) ->
                    let dragState =
                        { Doujinshi = settings.Nesoberi
                          Offset = Point(mouse.X - settings.Nesoberi.Position.X, mouse.Y - settings.Nesoberi.Position.Y) }

                    Some { Kind = Nesoberi; DragState = Some dragState }, None

                | _ ->
                    queueState.DoujinshiBox, None

            match dropResult with
            | None ->
                let timeLeft = queueState.TimeLeft - gameTime.ElapsedGameTime.Milliseconds
                let activeCustomerTimeLeft = queueState.ActiveCustomerTimeLeft - gameTime.ElapsedGameTime.Milliseconds

                let queueState =
                    { queueState with
                        TimeLeft = timeLeft
                        ActiveCustomerTimeLeft = activeCustomerTimeLeft
                        DoujinshiBox = doujinshiBox }

                state <- Day(dayNumber, CustomersQueue queueState)

            | Some (DamageTimeMs ms) ->
                let timeLeft = queueState.TimeLeft - gameTime.ElapsedGameTime.Milliseconds
                let activeCustomerTimeLeft = queueState.ActiveCustomerTimeLeft - gameTime.ElapsedGameTime.Milliseconds - ms

                let queueState =
                    { queueState with
                        TimeLeft = timeLeft
                        ActiveCustomerTimeLeft = activeCustomerTimeLeft
                        DoujinshiBox = doujinshiBox
                        ActiveCustomer =
                            { Customer = queueState.ActiveCustomer.Customer
                              Dialogue =
                                { GreetingIndex = queueState.ActiveCustomer.Dialogue.GreetingIndex
                                  State = NegativeDialogue 1000 } } }

                state <- Day(dayNumber, CustomersQueue queueState)


            | Some LikesDoujinshi ->
                let nextCustomer, customersBag =
                    if queueState.CustomersBag.Length = 0 then
                        let customerBag =
                            settings.LeftOverCustomerBag
                            |> shuffleArray
                            |> Array.map (fun name -> customers |> Seq.find (fun c -> c.Name = name))
                        Array.head customerBag, Array.tail customerBag
                    else
                        Array.head queueState.CustomersBag, Array.tail queueState.CustomersBag

                prevCustomerBodyParts <- customerBodyParts
                customerBodyParts <-
                    nextCustomer.BodyParts |> Array.map (fun parts -> rand.Next(0, parts.Length))

                score <- score + 1

                kachingSound.Play() |> ignore
                plingSound.Play() |> ignore

                let transition =
                    { PreviousCustomer = Some { queueState.ActiveCustomer with Dialogue = { queueState.ActiveCustomer.Dialogue with State = PositiveDialogue } }
                      NextCustomer = nextCustomer
                      CustomersBag = customersBag
                      TransitionMs = 1000
                      TimeLeft = queueState.TimeLeft }

                state <- Day(dayNumber, CustomerTransition transition)

            ()

            match state with
            | Day(day, CustomersQueue ({ ActiveCustomer = ({ Dialogue = ({ State = NegativeDialogue n } as dialogue) } as customer) } as queue)) ->
                if n <= 0 then
                    state <- Day(day, CustomersQueue { queue with ActiveCustomer = { customer with Dialogue = { dialogue with State = Greeting } }})
                else
                    state <- Day(day, CustomersQueue { queue with ActiveCustomer = { customer with Dialogue = { dialogue with State = NegativeDialogue (n - gameTime.ElapsedGameTime.Milliseconds) } }})
            | _ -> ()

        | Menu when mouse.HasJustClickedLeft && buttonBoundingBox.Contains(mouse.Position) ->
            state <- Intro introTotalTime

        | Menu -> ()

        | Intro ms when ms <= 0 && mouse.HasJustClickedLeft && buttonBoundingBox.Contains(mouse.Position) ->
            state <- Day(1, TitleCrawl)
            // let customersBag =
            //     settings.CustomerBagsByDay.[0]
            //     |> shuffleArray
            //     |> Array.map (fun name -> customers |> Seq.find (fun c -> c.Name = name))

            // let activeCustomer = customersBag |> Array.head
            // let customersBag = customersBag |> Array.tail

            // customerBodyParts <-
            //     activeCustomer.BodyParts |> Array.map (fun parts -> rand.Next(0, parts.Length))

            // state <-
            //     Day(
            //         1,
            //         CustomerTransition
            //             { PreviousCustomer = None
            //               NextCustomer = activeCustomer
            //               CustomersBag = customersBag
            //               TransitionMs = 1000
            //               TimeLeft = dayRoundTimeMs })

        | Intro ms when ms <= 0 -> ()

        | Intro _ when mouse.HasJustClickedLeft ->
            state <- Intro 0

        | Intro ms ->
            state <- Intro (ms - gameTime.ElapsedGameTime.Milliseconds)

        | Interim day when mouse.HasJustClickedLeft && buttonBoundingBox.Contains(mouse.Position) ->
            state <- Day(day + 1, TitleCrawl)

            // let customersBag =
            //     settings.CustomerBagsByDay.[day]
            //     |> shuffleArray
            //     |> Array.map (fun name -> customers |> Seq.find (fun c -> c.Name = name))

            // let activeCustomer = customersBag |> Array.head
            // let customersBag = customersBag |> Array.tail

            // customerBodyParts <-
            //     activeCustomer.BodyParts |> Array.map (fun parts -> rand.Next(0, parts.Length))

            // state <-
            //     Day(
            //         day + 1,
            //         CustomerTransition
            //             { PreviousCustomer = None
            //               NextCustomer = activeCustomer
            //               CustomersBag = customersBag
            //               TransitionMs = 1000
            //               TimeLeft = dayRoundTimeMs })

        | Interim _ -> ()

        | Day(dayNumber, TitleCrawl) when mouse.HasJustClickedLeft && buttonBoundingBox.Contains(mouse.Position) ->
            let customersBag =
                settings.CustomerBagsByDay.[dayNumber - 1]
                |> shuffleArray
                |> Array.map (fun name -> customers |> Seq.find (fun c -> c.Name = name))

            let activeCustomer = customersBag |> Array.head
            let customersBag = customersBag |> Array.tail

            customerBodyParts <-
                activeCustomer.BodyParts |> Array.map (fun parts -> rand.Next(0, parts.Length))

            state <-
                Day(
                    dayNumber,
                    CustomerTransition
                        { PreviousCustomer = None
                          NextCustomer = activeCustomer
                          CustomersBag = customersBag
                          TransitionMs = 1000
                          TimeLeft = dayRoundTimeMs })

            loopMusic.Play() |> ignore

        | Day(_, TitleCrawl) -> ()

        | EndScreen when mouse.HasJustClickedLeft && buttonBoundingBox.Contains(mouse.Position) ->
            if score < winningScore then
                state <- Day(1, TitleCrawl)
            else
                state <- Credits 10000

        | EndScreen -> ()

        | Credits _ -> ()

        | SplashScreen ms when ms <= 0 ->
            state <- Menu

        | SplashScreen ms ->
            state <- SplashScreen (ms - gameTime.ElapsedGameTime.Milliseconds)

        base.Update(gameTime)


    override _.Draw(gameTime: GameTime) =
        graphics.GraphicsDevice.Clear(Color.DimGray)

        spriteBatch.Begin()

        match state with
        | Day(dayNumber, CustomersQueue queueState) ->
            spriteBatch.Draw(background, Rectangle(Point(0, 0), background.Bounds.Size), Color.White)
            spriteBatch.DrawString(font, sprintf "Day %d" dayNumber, Vector2(20.f, 20.f), Color.Black)

            spriteBatch.DrawString(
                font,
                sprintf "%2d seconds remain today" ((queueState.TimeLeft / 1000) + 1), Vector2(20.f, 40.f),
                Color.Black,
                0.f,
                Vector2.Zero,
                Vector2(0.8f, 0.8f),
                SpriteEffects.None,
                1.f
            )

            drawCustomer queueState.ActiveCustomer.Customer customerBodyParts 0 1.f

            let clockIndex = Math.Clamp(9 - (queueState.ActiveCustomerTimeLeft / (customerRoundTimeMs / 10)), 0, 9)

            let posXDelta =
                if clockIndex >= 7 then
                    sin (float32 queueState.ActiveCustomerTimeLeft / 2.f) * ((float32 clockIndex) - 5.f) |> int
                else
                    0

            draw clock.[clockIndex] (Rectangle(Point(40 + posXDelta, 80), clock.[clockIndex].Bounds.Size))

            spriteBatch.Draw(table, Rectangle(0, 0, table.Width, table.Height), Color.White)
            spriteBatch.Draw(clickables, Rectangle(-7, 460, clickables.Width, clickables.Height), Color.White)

            match queueState.DoujinshiBox with
            | None | Some { DragState = None } ->
                if thinBooksStackBoundingBox.Contains(mouse.Position) then
                    spriteBatch.Draw(arrow, Rectangle(130, 499, arrow.Width, arrow.Height), Color.White)
                if musicStackBoundingBox.Contains(mouse.Position) then
                    spriteBatch.Draw(arrow, Rectangle(344, 556, arrow.Width, arrow.Height), Color.White)
                if nuigurumiStackBoundingBox.Contains(mouse.Position) then
                    spriteBatch.Draw(arrow, Rectangle(803, 491, arrow.Width, arrow.Height), Color.White)

            | _ -> ()

            drawDialogue queueState.ActiveCustomer

            match queueState.DoujinshiBox with
            | None ->
                if not nesoberiSold then
                    spriteBatch.Draw(settings.Nesoberi.Texture, settings.Nesoberi.Bounds, Color.White)

            | Some { Kind = kind; DragState = dragState } ->
                if kind <> Nesoberi then
                    spriteBatch.Draw(box, popUpBoundingBox, Color.White)

                    let kindString =
                        match kind with
                        | ThinBook -> "Doujins"
                        | Music -> "CDs"
                        | Nuigurumi -> "Plushies"
                        | _ -> ""

                    spriteBatch.DrawString(font, kindString, Vector2(528.f, 20.f), Color.Black)

                match dragState with
                | None ->
                    for doujin in doujinshi |> Seq.filter (fun d -> d.Kind = kind) do
                        draw doujin.Texture doujin.Bounds

                    if not nesoberiSold then
                        spriteBatch.Draw(settings.Nesoberi.Texture, settings.Nesoberi.Bounds, Color.White)

                    match
                        doujinshi
                        |> Seq.tryFind (fun d -> d.Kind = kind && d.Bounds.Contains(mouse.Position))
                    with
                    | None -> ()
                    | Some doujin ->
                        let nameSize = font.MeasureString(sprintf "%s" doujin.Name)

                        let descriptionSize = font.MeasureString(doujin.Description) * 0.8f

                        let size =
                            Point(
                                int <| max nameSize.X descriptionSize.X,
                                int nameSize.Y + 4 + int descriptionSize.Y
                            )

                        draw black <| Rectangle(mouse.X - size.X - 4 - 16, mouse.Y - 6, size.X + 16, size.Y + 12)
                        spriteBatch.DrawString(font, sprintf "%s" doujin.Name, Vector2(float32 mouse.X - 12.f - float32 size.X, float32 mouse.Y), Color.White)
                        drawStringScale
                            doujin.Description
                            (Point(
                                mouse.X - 12 - size.X,
                                mouse.Y + 4 + int nameSize.Y
                            ))
                            0.8f

                | Some draggedDoujin ->
                    for doujin in doujinshi |> Seq.filter (fun d -> d.Kind = kind && d.Name <> draggedDoujin.Doujinshi.Name) do
                        draw doujin.Texture doujin.Bounds

                    if draggedDoujin.Doujinshi.Kind <> Nesoberi && not nesoberiSold then
                        spriteBatch.Draw(settings.Nesoberi.Texture, settings.Nesoberi.Bounds, Color.White)

                    spriteBatch.Draw(
                        draggedDoujin.Doujinshi.Texture,
                        Rectangle(Point(mouse.X - draggedDoujin.Offset.X, mouse.Y - draggedDoujin.Offset.Y), draggedDoujin.Doujinshi.Bounds.Size),
                        Color.White)

            match queueState.DoujinshiBox with
            | None | Some { DragState = None } when not nesoberiSold && settings.Nesoberi.Bounds.Contains(mouse.Position) ->
                let doujin = settings.Nesoberi

                let nameSize = font.MeasureString(sprintf "%s" doujin.Name)

                let descriptionSize = font.MeasureString(doujin.Description) * 0.8f

                let size =
                    Point(
                        int <| max nameSize.X descriptionSize.X,
                        int nameSize.Y + 4 + int descriptionSize.Y
                    )

                draw black <| Rectangle(mouse.X - size.X - 4 - 16, mouse.Y - 6, size.X + 16, size.Y + 12)
                spriteBatch.DrawString(font, sprintf "%s" doujin.Name, Vector2(float32 mouse.X - 12.f - float32 size.X, float32 mouse.Y), Color.White)
                drawStringScale
                    doujin.Description
                    (Point(
                        mouse.X - 12 - size.X,
                        mouse.Y + 4 + int nameSize.Y
                    ))
                    0.8f
            | _ -> ()


        | Day(dayNumber, CustomerTransition transition) ->
            spriteBatch.Draw(background, Rectangle(Point(0, 0), background.Bounds.Size), Color.White)

            spriteBatch.DrawString(font, sprintf "Day %d" dayNumber, Vector2(20.f, 20.f), Color.Black)

            spriteBatch.DrawString(
                font,
                sprintf "%2d seconds remain today" ((transition.TimeLeft / 1000) + 1), Vector2(20.f, 40.f),
                Color.Black,
                0.f,
                Vector2.Zero,
                Vector2(0.8f, 0.8f),
                SpriteEffects.None,
                1.f
            )

            match transition.PreviousCustomer with
            | None -> ()
            | Some customer ->
                let d = 1.f - float32 transition.TransitionMs / 1000.f

                let offsetX = int (d * 1000.f)

                drawCustomer customer.Customer prevCustomerBodyParts offsetX 1.f

                if transition.TransitionMs > 600 then
                    drawDialogue customer

            if transition.TransitionMs < 500 then
                let d = 1.f - float32 transition.TransitionMs / 500.f

                drawCustomer transition.NextCustomer customerBodyParts 0  d

            spriteBatch.Draw(table, Rectangle(0, 0, table.Width, table.Height), Color.White)
            spriteBatch.Draw(clickables, Rectangle(-7, 460, clickables.Width, clickables.Height), Color.White)

            if not nesoberiSold then
                spriteBatch.Draw(settings.Nesoberi.Texture, settings.Nesoberi.Bounds, Color.White)

        | Menu ->
            spriteBatch.Draw(bigsight, Rectangle(0, 0, bigsight.Width, bigsight.Height), Color.White)
            // spriteBatch.DrawString(font, "Click to start the game.", Color.)

            drawStringCentered gameName 300 Color.Black 2.f

            drawButton "Start the game"

        | Intro ms ->
            let d = 1.f - float32 ms / float32 introTotalTime

            spriteBatch.Draw(bigsight, Rectangle(0, -(int (d * 768.f)), 1024, 768 * 2), Color.White)

            let originalMs = ms
            let ms = introTotalTime - originalMs

            if ms > 2000 then
                drawStringCentered "Oh, it's summertime, and the living's easy." 100 Color.White 1.f
                drawStringCentered
                    "Your friends asked you to help them sell\ndoujin goods at a big comic convention,\nbut when you arrive you realize\nnone of the others made it."
                    150 Color.White
                    1.f

            let names = [|"CalpicoSour"; "Steen"; "Zoopditto"; "Seki"|]
            for i in 0 .. 3 do
                if ms > 8000 + (1500 * i) then
                    let msg = settings.BullshitReasons.[i].Replace("$", names.[i])
                    drawStringCentered msg (250 + (60 * i)) Color.White 1.f

            if ms > 16000 then
                drawStringCentered "It is now up to you to do the work\nof four people, for three days straight!" 480 Color.White 1.f
                drawStringCentered "Better keep everything rolling!" 530 Color.White 1.f

            if originalMs <= 0 then
                drawButton "Start the game"
                // draw black buttonBoundingBox
                // drawStringCentered "Click to start" 600 Color.White 1.f

        | Day(dayNumber, TitleCrawl) ->
            spriteBatch.Draw(background, Rectangle(0, 0, 1024, 768), Color.White)

            let dayStr =
                match dayNumber with
                | 1 -> "first"
                | 2 -> "second"
                | _ -> "third"

            drawStringCentered (sprintf "Dawn of the %s day" dayStr) 300 Color.White 2.f

            drawButton "Continue"

        | Interim(dayNumber) ->
            spriteBatch.Draw(backgroundDark, Rectangle(0, 0, 1024, 768), Color.White)

            let dayStr =
                match dayNumber with
                | 1 -> "first"
                | 2 -> "second"
                | _ -> "third"

            drawStringCentered (sprintf "End of the %s day" dayStr) 300 Color.White 2.f

            drawStringCentered (sprintf "You sold %d goods so far" score) 400 Color.White 1.f

            drawButton "Continue"

        | EndScreen ->
            spriteBatch.Draw(bigsight, Rectangle(0, -768, 1024, 768 * 2), Color.White)

            if score >= winningScore then
                drawStringCentered (sprintf "You successfully sold %d yen worth of goods." (500 * score)) 400 Color.White 1.f
                drawStringCentered "Unfortunately you blew it all at the izakaya." 450 Color.White 1.f
                drawStringCentered "Better luck next year!" 500 Color.White 1.f
                drawButton "Continue"
            else
                drawStringCentered "You didn't sell enough goods." 400 Color.White 1.f
                drawStringCentered "Your friends are very disappointed in you." 450 Color.White 1.f
                drawButton "Retry"

        | Credits(_) ->
            spriteBatch.Draw(creditsScreen, Rectangle(0, 0, 1024, 768), Color.White)

        | SplashScreen _ ->
            spriteBatch.Draw(splashScreen, Rectangle(0, 0, 1024, 768), Color.White)


        spriteBatch.End()

        base.Draw(gameTime)


[<EntryPoint; STAThread>]
let main _argv =
    use game = new TenDoujinsPerSecondGame()
    game.Run()
    0
