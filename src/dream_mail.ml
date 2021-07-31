open Lwt.Syntax

(** Common *)

let ( >>= ) = Lwt.bind

module Address = struct
  type t = string [@@deriving yojson]

  let parse address =
    match Emile.of_string address with Ok _ -> Some address | Error _ -> None

  let to_string address = address
end

type email = { address : Address.t; subject : string; text : string }
[@@deriving yojson]

module type Queue = sig
  type t

  val connect : unit -> t Lwt.t

  val send : t -> string -> unit Lwt.t

  val receive : t -> string option Lwt.t
end

(** Plugins *)

type email_plugin = { send : email -> unit Lwt.t }

type queue_plugin = { queue : (module Queue) }

type plugin = EmailP of email_plugin | QueueP of queue_plugin

let plugins = ref []

let queue_plugin () =
  List.find_map
    (function EmailP _ -> None | QueueP queue -> Some queue)
    !plugins
  |> Option.get

let email_plugin () =
  List.find_map
    (function EmailP email -> Some email | QueueP _ -> None)
    !plugins
  |> Option.get

(** Interface *)

let handle_message message =
  let email =
    try Some (message |> Yojson.Safe.from_string |> email_of_yojson)
    with _ ->
      Printf.printf "Received invalid email record from RabbitMQ: %s" message;
      None
  in
  match email with
  | Some e -> (email_plugin ()).send e
  | None -> Lwt.return_unit

let queue_worker () =
  Printf.printf "Starting Queue Worker\n";
  let module Q = (val (queue_plugin ()).queue : Queue) in
  let* queue = Q.connect () in
  let rec handle () =
    Q.receive queue >>= fun message ->
    let task =
      match message with
      | Some m ->
          flush stdout;
          handle_message m
      | _ ->
          Printf.printf "No new tasks, waiting.\n";
          flush stdout;
          Lwt_unix.sleep 5.
    in
    task >>= handle
  in
  handle ()

let queue_email email =
  let module Q = (val (queue_plugin ()).queue : Queue) in
  let text = email |> yojson_of_email |> Yojson.Safe.to_string in
  let* queue = Q.connect () in
  let* _ = Q.send queue text in
  Lwt.return_unit

(** Main *)

let () =
  let send email =
    Printf.printf
      "Sending message with subject \"%s\" and message \"%s\" to address \"%s\""
      email.subject email.text
      (Address.to_string email.address);
    Lwt.return_unit
  in
  plugins := EmailP { send } :: !plugins;

  let module Q : Queue = struct
    type t = string Stdlib.Queue.t

    let _queue = Stdlib.Queue.create ()

    let connect () = Lwt.return _queue

    let send queue message =
      Stdlib.Queue.add message queue;
      Lwt.return_unit

    let receive queue = Lwt.return @@ Stdlib.Queue.take_opt queue
  end in
  plugins := QueueP { queue = (module Q) } :: !plugins;
  let email =
    {
      address = Option.get @@ Address.parse "test@test.com";
      text = "Test body";
      subject = "Test subject";
    }
  in
  Lwt_main.run (queue_email email >>= fun _ -> queue_worker ())
