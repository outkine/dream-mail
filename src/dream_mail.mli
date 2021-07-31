(** Common *)

module Address : sig
  type t

  val parse : string -> t option

  val to_string : t -> string
end

type email = { address : Address.t; subject : string; text : string }
[@@deriving yojson]

(** Plugins *)

module type Queue = sig
  type t

  val connect : unit -> t Lwt.t

  val send : t -> string -> unit Lwt.t

  val receive : t -> string option Lwt.t
end

type email_plugin = { send : email -> unit Lwt.t }

type queue_plugin = { queue : (module Queue) }

type plugin = EmailP of email_plugin | QueueP of queue_plugin

val plugins : plugin list ref

(** Interface *)

val queue_worker : unit -> unit Lwt.t

val queue_email : email -> unit Lwt.t
