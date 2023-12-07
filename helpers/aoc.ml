open Lwt
open Lwt_process
open Cohttp
open Cohttp_lwt_unix

let getcookie =
  let cmd = shell "GNUPGHOME=~/configs/gnupg/ pass show aoccookie" in
  let _ = exec ~stdout:`Dev_null cmd in
  pread cmd >|= CCString.rtrim

let input day =
  getcookie >>= fun cookie ->
  let headers = Header.init_with "Cookie" ("session=" ^ cookie) in
  Client.get ~headers
    (Uri.of_string
       (CCFormat.sprintf "https://adventofcode.com/2023/day/%d/input" day))
  >>= fun (_, body) -> body |> Cohttp_lwt.Body.to_string
