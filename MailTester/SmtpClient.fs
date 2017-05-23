module SmtpClient

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Threading
open System.Net.Mail
open ARSoft.Tools.Net
open ARSoft.Tools.Net.Dns

let (|IsCode|_|) (line:string) =
  let parts = line.Split([|' '|], 2, StringSplitOptions.RemoveEmptyEntries)
  if parts.Length <= 1
  then None
  else
    let code = ref 0
    if Int32.TryParse(parts.[0], code)
    then Some (!code, parts.[1])
    else None

type Response =
  | ConnectionSucceed of string
  | CommandAccepted of string
  | WaitMessageBody of string
  | FloodDetection of string
  | TooManyReceipts of string
  | UnknownReceipt of string
  | Blacklisted of string
  | UnknownResponse
  static member Parse(lines:string list) =
    match lines with
    | [] -> UnknownResponse
    | (IsCode (220,text)) :: _ -> ConnectionSucceed text
    | (IsCode (250,text)) :: _ -> CommandAccepted text
    | (IsCode (354,text)) :: _ -> WaitMessageBody text
    | (IsCode (421,text)) :: _ -> FloodDetection text
    | (IsCode (452,text)) :: _ -> TooManyReceipts text
    | (IsCode (550,text)) :: _ -> UnknownReceipt text
    | (IsCode (554,text)) :: _ -> Blacklisted text
    | _ -> UnknownResponse

let quoteMail (mail:MailAddress) =
  let m = mail.Address
  let m2 = if not <| m.StartsWith "<" then ("<" + m) else m
  if not <| m2.EndsWith ">" then (m2 + ">") else m2

type Session (host:string, port:int) =
  let client = new TcpClient()
  do client.Connect(host, port)
  
  let stream = client.GetStream()
  let reader = new StreamReader(stream)
  let writer = new StreamWriter(stream)

  let send (m:string) =
    writer.WriteLine m
    writer.Flush()

  let sendBetweenCrlf (m:string) =
    writer.Write '\r'
    writer.Write '\n'
    writer.Write m
    writer.Write '\r'
    writer.Write '\n'
    writer.Flush()

  let readLinesWithTimeout (timeout:TimeSpan) =
    let limit = DateTime.Now.Add timeout
    while DateTime.Now < limit && not stream.DataAvailable do
      Thread.Sleep(100)
    seq {
      while stream.DataAvailable do
        yield reader.ReadLine()
        Thread.Sleep(100)
    } |> Seq.toList

  let readLines () =
    readLinesWithTimeout (TimeSpan.FromSeconds 15.)

  let readResponse () =
    readLines() |> Response.Parse
    
  let mutable mailTo = ""
  let mutable mailFrom = ""

  do readLines() |> ignore

  member val Name="fsharp_smtp_framework" with get,set
  
  member __.Hello() =
    send <| sprintf "HELO %s" __.Name
    readResponse()
  member __.MailFrom m =
    mailFrom <- quoteMail m
    send <| sprintf "MAIL FROM: %s" mailFrom
    readResponse()
  member __.MailTo m =
    mailTo <- quoteMail m
    send <| sprintf "RCPT TO: %s" mailTo
    readResponse()
  member __.Message (subject:string, content:string) =
    send "DATA"
    let dataRs = readResponse()
    //"354  Go ahead f46si4687887wrf.78 - gsmtp"
    send <| sprintf "From: %s" mailFrom
    send <| sprintf "To: %s" mailTo
    send <| sprintf "Subject: %s" subject
    send content
    sendBetweenCrlf "."
    readResponse()
  member __.Quit() =
    send "QUIT"
    let quitRs = readResponse()
    //"550-5.7.1 not RFC 5322 compliant:"
    client.Close()
    //client.Dispose()

  type MailChecker (verifier:MailAddress) =
    member __.Check (mail:MailAddress) =
      let domain = DomainName.Parse mail.Host
      let response = DnsClient.Default.Resolve(domain, RecordType.Mx)
      match response.AnswerRecords |> Seq.filter(fun r -> r :? MxRecord) |> Seq.cast<MxRecord> |> Seq.tryHead with
      | None -> false
      | Some record ->
        let mx = record.ExchangeDomainName
        try
          let session = new Session(mx.ToString(), 25)
          session.Hello () |> ignore
          session.MailFrom verifier |> ignore
          let reply = session.MailTo mail
          session.Quit() |> ignore
          match reply with
          | CommandAccepted _ -> true
          | _ -> false
        with _ -> false
    

