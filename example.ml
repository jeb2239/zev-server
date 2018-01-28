open Core
open Async

let quest name = 
  let%bind new_file_name = Reader.file_contents name in
  let%bind new_value = Reader.file_contents new_file_name in
  return new_value
;;

let rec copy_blocks buffer r w =
  match%bind Reader.read r buffer with
  | `Eof -> return ()
  | `Ok bytes_read ->
    if String.sub ~pos:0 ~len:(Int.min bytes_read 4) buffer = "echo" then
      Writer.write w buffer ~pos:5 ~len:(bytes_read - 5)
    else
      Writer.write w "you suck\n" ~len:9;

    let%bind () = Writer.flushed w in
    copy_blocks buffer r w

let main () =
  let%bind _server =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port 8700)
      (fun _addr r w ->
         let buffer = Bytes.create (16 * 1024) in
         copy_blocks buffer r w
      )
  in 
  Deferred.never ()

let () =
  let open Command.Let_syntax in
  Command.run
    (Command.async ~summary:"Do something"
       [%map_open
         let () = return () in
         fun () -> main ()
       ])
