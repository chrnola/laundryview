/// Functions for consuming data from the "enhanced" API
/// that was originally intended to be used by LaundryView's
/// Flash application (laundry_room.php)
module LaundryView.EnhancedModeApi

open Hopac
open HttpFs.Client
open HttpFs.Client.Request

let [<Literal>] private baseUrl = "http://www.laundryview.com"
let private staticDataUrl = sprintf "%s/staticRoomData.php" baseUrl
let private dynamicDataUrl = sprintf "%s/dynamicRoomData.php" baseUrl

let [<Literal>] private userAgent =
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/62.0.3202.62 Safari/537.36"

let private blessSession propertyCode = job {
    let startingUrl = sprintf "%s/%s" baseUrl propertyCode

    let! resp =
        Request.createUrl Get startingUrl
        |> Request.setHeader (RequestHeader.UserAgent(userAgent))
        |> Request.autoFollowRedirectsDisabled
        |> getResponse

    let cookieName = "PHPSESSID"

    match Map.tryFind cookieName resp.cookies with
    | Some sessionId ->
        let cookie =
            { Cookie.name = cookieName
              Cookie.domain = Some (".laundryview.com")
              Cookie.path = Some ("/")
              Cookie.expires = None
              Cookie.httpOnly = false
              Cookie.value = sessionId
              Cookie.secure = false }

        // We have to follow the redirect before the session
        // will be blessed. -_-
        do!
            resp.headers
            |> Map.find ResponseHeader.Location // Redirect URL from 1st request
            |> Request.createUrl Get
            |> Request.cookie cookie
            |> Request.setHeader (RequestHeader.UserAgent(userAgent))
            |> getResponse
            |> Alt.Ignore

        return Some cookie
    | None -> return None
}

let private makeRequestAsFlashApp roomId cookie url =
    let referer = sprintf "%s/laundry_room.php?lr=%s" baseUrl roomId

    Request.createUrl Get url
    |> Request.queryStringItem "location" roomId
    |> Request.cookie cookie
    |> Request.setHeader (RequestHeader.UserAgent(userAgent))
    |> Request.setHeader (RequestHeader.Custom("X-Requested-With", "ShockwaveFlash/27.0.0.170"))
    |> Request.setHeader (RequestHeader.Accept("*/*"))
    |> Request.setHeader (RequestHeader.Referer(referer))
    |> Request.setHeader (RequestHeader.AcceptLanguage("en-US,en;q=0.9"))
    |> Request.autoDecompression (DecompressionScheme.GZip ||| DecompressionScheme.Deflate)
    |> Request.responseAsString

type private ResponseType = ``Static`` of string | Dynamic of string

let private fetchData startingUrl roomId = job {
    let! sessionToken = blessSession startingUrl

    match sessionToken with
    | Some cookie ->
        let requester = makeRequestAsFlashApp roomId cookie

        let! staticAndDynamicData =
            [ requester staticDataUrl |> Job.map ResponseType.``Static``
              requester dynamicDataUrl |> Job.map ResponseType.Dynamic ]
            |> Job.conCollect

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

            // TODO: Parse the dynamic data

            return mach, d
        | _ -> return failwith "Could not extract static and/or dynamic response"
    | None ->
        return failwith "Could not get session token"
}

let getLaundryDataForRoom propertySlug roomId =
    fetchData propertySlug roomId |> run
