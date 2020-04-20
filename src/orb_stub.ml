(* environment variable support for orb
   - turns out, most environment variables don't need to be set
   - exceptions are PATH (to find binaries) HOME (to find .opam)
     - we normalise PATH to a whitelist
     - we retain HOME
   - this is all on toplevel and in here since it needs to be run before
     opam captures the environment
*)
(*
let path = try Some (Unix.getenv "PATH") with Not_found -> None;;

external unsetenv : string -> unit = "orb_unsetenv";;

Array.iter (fun k ->
    match String.index_opt k '=' with
    | None -> Printf.printf "ignoring environment %s\n" k
    | Some idx ->
      let k = String.sub k 0 idx in
      if k <> "HOME" then unsetenv k)
  (Unix.environment ());;

module S = Set.Make(String)

let cuts s sep =
  let rec one s acc =
    match String.index_opt s sep with
    | None -> List.rev (s :: acc)
    | Some idx ->
      let idx' = succ idx in
      let fst = String.sub s 0 idx
      and snd = String.sub s idx' (String.length s - idx')
      in
      let acc' = if List.mem fst acc then acc else fst :: acc in
      one snd acc'
  in
  one s []

let find_path () =
  let allowed =
    S.of_list [ "/bin" ; "/usr/bin" ; "/usr/local/bin" ; "/opt/bin" ]
  in
  let whitelisted x = S.mem x allowed in
  let our_path = match path with
    | None -> S.elements allowed
    | Some paths -> List.filter whitelisted (cuts paths ':')
  in
  String.concat ":" our_path;;

Unix.putenv "PATH" (find_path ());;
*)

let main () = ()
