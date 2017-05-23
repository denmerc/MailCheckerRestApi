open SmtpClient
open ARSoft.Tools.Net.Dns
open ARSoft.Tools.Net
open System.Net.Mail
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Swagger
open Rest
open FunnyDsl
open Swagger
open Suave
open System
open Suave.Successful

[<CLIMutable>]
type SubtractionResult = { Result:int }
and [<CLIMutable>] SubtractionRequest =
  { First:int
    Second:int
  }

let now : WebPart =
  fun (x : HttpContext) ->
    async {
      // The MODEL helper checks the "Accept" header 
      // and switches between XML and JSON format
      return! MODEL DateTime.Now x
    }
let subtractObj =
  JsonBody<SubtractionRequest>(fun {First=a;Second=b} -> MODEL {Result=(a-b)})

module Server =
  let verify (mail:string) =
    printfn "Checking %s ..." mail
    let checker = new MailChecker(new MailAddress("toto@msn.com"))
    let r = checker.Check(new MailAddress(mail))
    printfn "%b" r
    r
  
  let checkmail (mail:string) =
    OK <| sprintf "%b" (verify mail)

  let api = 
    swagger {
      
      for route in getting <| urlFormat "/checkmail/%s" checkmail do
        yield description Of route is "Check if mail exists"
        
    }

open System.Threading
open Topshelf
open System.Net

[<EntryPoint>]
let main argv = 

  let cancellationTokenSource = ref None
  
  let start hc = 
      let cts = new CancellationTokenSource()
      let token = cts.Token
      let config = 
        { defaultConfig with 
            cancellationToken = token
            bindings = [ HttpBinding.create HTTP IPAddress.Any 8285us ]
        }

      startWebServerAsync config Server.api.App
      |> snd
      |> Async.StartAsTask 
      |> ignore

      cancellationTokenSource := Some cts
      true

  let stop hc = 
      match !cancellationTokenSource with
      | Some cts -> cts.Cancel()
      | None -> ()
      true
  
  Service.Default 
  |> display_name "MailTester REST FULL API"
  |> instance_name "MailTester"
  |> with_start start
  |> with_stop stop
  |> run

