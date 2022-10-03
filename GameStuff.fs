module GameStuff


open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
open System.Runtime.CompilerServices
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Collections.Generic


type OptionJsonConverter<'T>() =
    inherit JsonConverter<'T option>()

    override _.Read(reader: byref<Utf8JsonReader>, _: Type, options: JsonSerializerOptions) =
        match reader.TokenType with
        | JsonTokenType.Null -> None
        | _ -> Some(JsonSerializer.Deserialize<'T>(&reader, options))

    override _.Write(writer: Utf8JsonWriter, value: 'T option, options: JsonSerializerOptions) =
        match value with
        | None -> writer.WriteNullValue()
        | Some value -> JsonSerializer.Serialize(writer, value, options)


type CustomerDialogue =
    { Greetings: string []
      Positive: string
      Negative: string }


type DialogueKind =
    | Greeting
    | PositiveDialogue
    | NegativeDialogue of int


type Customer =
    { Name: string
      TextureFile: string
      [<JsonIgnore>]
      Texture: Texture2D
      BodyPartsFiles: string [] []
      [<JsonIgnore>]
      BodyParts: Texture2D [] []
      Likes: string []
      [<JsonConverter(typeof<OptionJsonConverter<string>>)>]
      FavoriteItem: string option
      Dialogue: CustomerDialogue }


type DoujinshiKind =
    | Music
    | ThinBook
    | Nuigurumi
    | Nesoberi


type DoujinshiKindJsonConverter() =
    inherit JsonConverter<DoujinshiKind>()

    override _.Read(reader: byref<Utf8JsonReader>, _: Type, _: JsonSerializerOptions) =
        match reader.GetString() with
        | "Music" -> Music
        | "ThinBook" -> ThinBook
        | "Nuigurumi" -> Nuigurumi
        | "Nesoberi" -> Nesoberi
        | str -> raise (JsonException(sprintf "Invalid DoujinshiKind: %s" str))

    override _.Write(writer: Utf8JsonWriter, kind: DoujinshiKind, _: JsonSerializerOptions) =
        let str =
            match kind with
            | Music -> "Music"
            | ThinBook -> "ThinBook"
            | Nuigurumi -> "Nuigurumi"
            | Nesoberi -> "Nesoberi"

        writer.WriteStringValue(str)


type PointJsonConverter() =
    inherit JsonConverter<Point>()

    override _.Read(reader: byref<Utf8JsonReader>, _: Type, options: JsonSerializerOptions) =
        let dict =
            JsonSerializer.Deserialize<Dictionary<string, int>>(&reader, options)

        Point(dict.["X"], dict.["Y"])

    override _.Write(writer: Utf8JsonWriter, point: Point, _: JsonSerializerOptions) = raise (NotImplementedException())


type Doujinshi =
    { Name: string
      TextureFile: string
      [<JsonIgnore>]
      mutable Texture: Texture2D
      [<JsonConverter(typeof<DoujinshiKindJsonConverter>)>]
      Kind: DoujinshiKind
      Description: string
      Tag: string
      Index: int
      [<JsonConverter(typeof<PointJsonConverter>)>]
      Position: Point }

    member this.Bounds =
        Rectangle(this.Position, this.Texture.Bounds.Size)


type Settings =
    { Customers: Customer []
      Doujinshi: Doujinshi []
      Nesoberi: Doujinshi
      CustomerBagsByDay: string [] []
      LeftOverCustomerBag: string []
      mutable BullshitReasons: string [] }

    static member Read() =
        let options = JsonSerializerOptions()
        options.Converters.Add(JsonStringEnumConverter())
        JsonSerializer.Deserialize<Settings>(File.ReadAllText(@"Content/settings.json"), options)

    member this.LoadCustomers loadTexture : Customer [] =
        this.Customers
        |> Array.map (fun c ->
            { c with
                Texture = loadTexture "Customers" c.TextureFile
                BodyParts =
                    c.BodyPartsFiles
                    |> Array.map (fun p ->
                        p
                        |> Array.map (fun name -> loadTexture "Customers/Otaku" name)) })

    member this.LoadDoujinshi loadTexture : Doujinshi [] =
        this.Doujinshi
        |> Array.map (fun d -> { d with Texture = loadTexture "Doujinshi" d.TextureFile })


type DraggedDoujinshi = { Doujinshi: Doujinshi; Offset: Point }


type DoujinshiBox =
    { Kind: DoujinshiKind
      DragState: DraggedDoujinshi option }


type GreetingStatus =
    { GreetingIndex: int
      State: DialogueKind }


type CustomerState =
    { Customer: Customer
      Dialogue: GreetingStatus }


type CustomersQueueState =
    { ActiveCustomer: CustomerState
      ActiveCustomerTimeLeft: int // Milliseconds
      CustomersBag: Customer []
      DoujinshiBox: DoujinshiBox option
      TimeLeft: int } // Milliseconds


type CustomerTransitionState =
    { PreviousCustomer: CustomerState option
      NextCustomer: Customer
      CustomersBag: Customer []
      TransitionMs: int
      TimeLeft: int }


type DayState =
    | TitleCrawl
    | CustomersQueue of CustomersQueueState
    | CustomerTransition of CustomerTransitionState


// type InterimState = { Message: string }


type GameStateMachine =
    | SplashScreen of int
    | Menu
    | Intro of int
    | Day of int * DayState
    | Interim of int
    | EndScreen
    | Credits of int


type DoujinshiDropResult =
    | LikesDoujinshi
    | DamageTimeMs of int
