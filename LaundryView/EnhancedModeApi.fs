/// Functions for consuming data from the "enhanced" API
/// that was originally intended to be used by LaundryView's
/// Flash application (laundry_room.php)
module LaundryView.EnhancedModeApi

open AsyncExtensions
open HttpClient

let [<Literal>] private baseUrl = "http://www.laundryview.com"
let private staticDataUrl = sprintf "%s/staticRoomData.php" baseUrl
let private dynamicDataUrl = sprintf "%s/dynamicRoomData.php" baseUrl

let [<Literal>] private userAgent =
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/62.0.3202.62 Safari/537.36"

let private blessSession propertyCode = async {
    let startingUrl = sprintf "%s/%s" baseUrl propertyCode

    let! resp =
        createRequest Get startingUrl
        |> withHeader (RequestHeader.UserAgent(userAgent))
        |> withAutoFollowRedirectsDisabled
        |> getResponseAsync

    let cookieName = "PHPSESSID"

    match Map.tryFind cookieName resp.Cookies with
    | Some sessionId ->
        let cookie = { name = cookieName; value = sessionId }
        // We have to follow the redirect before the session
        // will be blessed. -_-
        do!
            resp.Headers
            |> Map.find ResponseHeader.Location // Redirect URL from 1st request
            |> createRequest Get
            |> withCookie cookie
            |> withHeader (RequestHeader.UserAgent(userAgent))
            |> getResponseAsync
            |> Async.Ignore

        return Some cookie
    | None -> return None
}

let private makeRequestAsFlashApp roomId cookie url =
    let referer = sprintf "%s/laundry_room.php?lr=%s" baseUrl roomId

    createRequest Get url
    |> withQueryStringItem { name = "location"; value = roomId }
    |> withCookie cookie
    |> withHeader (RequestHeader.UserAgent(userAgent))
    |> withHeader (RequestHeader.Custom({ name = "X-Requested-With"; value = "ShockwaveFlash/27.0.0.170"}))
    |> withHeader (RequestHeader.Accept("*/*"))
    |> withHeader (RequestHeader.Referer(referer))
    |> withHeader (RequestHeader.AcceptLanguage("en-US,en;q=0.9"))
    |> withAutoDecompression (DecompressionScheme.GZip ||| DecompressionScheme.Deflate)
    |> getResponseBodyAsync

type private ResponseType = ``Static`` of string | Dynamic of string

let getLaundryDataForRoom propertySlug roomId = async {
    let! sessionToken = blessSession propertySlug

    match sessionToken with
    | Some cookie ->
        let requester = makeRequestAsFlashApp roomId cookie

        let! staticAndDynamicData =
            [ requester staticDataUrl |> Async.Map ResponseType.``Static``
              requester dynamicDataUrl |> Async.Map ResponseType.Dynamic ]
            |> Async.Parallel

        let (stResp, dynResp) =
            staticAndDynamicData
            |> Seq.fold (fun (st, dyn) next ->
                match st, dyn, next with
                | None, _, ResponseType.``Static`` staticResponse ->
                    Some staticResponse, dyn
                | _, None, ResponseType.Dynamic dynamicResponse ->
                    st, Some dynamicResponse
                | _ -> st, dyn
                ) (None, None)

        match stResp, dynResp with
        | Some s, Some d ->
            let mach =
                match StaticData.Parser.parseStaticData s with
                | Ok machines -> machines
                | Error msg -> failwith (sprintf "Error parsing static data: %s" msg)

            let states =
                match DynamicData.Parser.parseDynamicData d with
                | Ok states -> states
                | Error msg -> failwith (sprintf "Error parsing dynamic data: %s" msg)

            return mach, states
        | _ -> return failwith "Could not extract static and/or dynamic response"
    | None ->
        return failwith "Could not get session token"
}
