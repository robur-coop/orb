(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Cmdliner
open Orb_lib

let get2 = function  s1::s2::[]-> s1, s2 | _ -> assert false

let list_init len f =
  let rec init_aux i n f =
    if i >= n then []
    else
      let r = f i in
      r :: init_aux (i+1) n f
  in
  init_aux 0 len f

let seq l f =
  List.iter f l

let fork lst f =
  let pids =
    List.map (fun arg -> (* to be sure to have at least 1s of delay in tms *)
        Unix.sleep 1;
        match Unix.fork () with
        | 0 -> (try f arg with OpamStd.Sys.Exit n -> exit n); exit 255
        | pid -> pid) lst
    |> OpamStd.IntSet.of_list
  in
  let w, err =
    List.fold_left (fun (w,err) (p,e) ->
        let w = p::w in
        if e = Unix.WEXITED 255 then w, err else w, e::err)
      ([],[])
      (list_init (OpamStd.IntSet.cardinal pids) (fun _ -> Unix.wait ()))
  in
  let w = OpamStd.IntSet.of_list w in
  let remaining = OpamStd.IntSet.Op.(pids -- w) in
  if not (OpamStd.IntSet.is_empty remaining) then
    exit_error `Not_found "Remaining pids %s"
      (OpamStd.IntSet.to_string remaining);
  if err <> [] then
    exit_error `False
      "Some subprocesses ended with a non-zero code: %s"
      (OpamStd.Format.pretty_list (List.map (function
           | Unix.WEXITED e -> Printf.sprintf "exit %d" e
           | Unix.WSIGNALED e -> Printf.sprintf "signal %d" e
           | Unix.WSTOPPED e -> Printf.sprintf "stop %d" e) err))

let export name switch =
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
  let switch_out = OpamFile.make (OpamFilename.of_string name) in
  OpamSwitchCommand.export rt ~switch ~full:true (Some switch_out);
  drop_states ~gt ~rt ()

let generate_diffs root1 root2 final_map dir =
  let root1 = OpamFilename.Dir.of_string (root1 ^ "/_opam")
  and root2 = OpamFilename.Dir.of_string (root2 ^ "/_opam")
  in
  let open OpamFilename.Op in
  OpamFilename.mkdir dir;
  OpamPackage.Map.iter (fun pkg _ ->
      OpamFilename.mkdir (dir / OpamPackage.to_string pkg)) final_map;
  let full =
    let copy pkg file num =
      let root, suffix =
        match num with
        | `fst -> root1, "orb_fst"
        | `snd -> root2, "orb_snd"
      in
      OpamFilename.copy ~src:(root // file)
        ~dst:(OpamFilename.add_extension (dir / pkg // file) suffix)
    in
    let make_cmd pkg file =
      OpamSystem.make_command "diffoscope"
        [OpamFilename.to_string (root1 // file);
         OpamFilename.to_string (root2 // file);
         "--text"; (OpamFilename.to_string (dir / pkg // (file ^ ".diff")))]
    in
    OpamPackage.Map.fold (fun pkg tr acc ->
        let spkg = OpamPackage.to_string pkg in
        OpamStd.String.Map.fold (fun file diff acc ->
            (diff, (copy spkg file), (make_cmd spkg file)) :: acc) tr acc)
      final_map []
  in
  (* Here we use opam parallelism *)
  let open OpamProcess.Job.Op in
  let command (diff, copy, cmd) =
    match diff with
    | Both (_, _) ->
      copy `fst;
      copy `snd;
      cmd @@> fun _ -> Done ()
    | First _ -> copy `fst; Done ()
    | Second _ -> copy `snd; Done ()
  in
  try
    OpamParallel.iter ~jobs:OpamStateConfig.(Lazy.force !r.jobs) ~command full
  with OpamParallel.Errors (_, err, can)->
    OpamConsole.error "Some jobs failed:\n%s\nand other canceled:\n%s\n"
      (OpamStd.Format.itemize (fun (i,e) ->
           let _, _, cmd = List.nth full i in
           Printf.sprintf "%s : %s"
             (OpamProcess.string_of_command cmd) (Printexc.to_string e))
          err)
      (OpamStd.Format.itemize (fun i ->
           let _, _, cmd = List.nth full i in
           OpamProcess.string_of_command cmd) can)

(* Main function *)
let orb global_options build_options diffoscope twice keep_switches compiler_switches use_switches
    repos atoms_or_locals =
  if atoms_or_locals = [] then
    exit_error `Bad_arguments
      "I can't check reproductibility of nothing, by definition, it is";
  if diffoscope <> None && OpamSystem.resolve_command "diffoscope" = None then
    exit_error `Not_found "diffoscope not found";
  let root = OpamStateConfig.(!r.root_dir) in
  let config_f = OpamPath.config root in
  let already_init = OpamFile.exists config_f in
  if not already_init then
    exit_error `False "orb needs an already initialised opam";
  OpamArg.apply_global_options global_options;
  OpamArg.apply_build_options build_options;
  OpamCoreConfig.update ~precise_tracking:true ~answer:(Some true) ();
  (match compiler_switches, use_switches with
   | None, None -> OpamStateConfig.update ~unlock_base:true ();
   | _ -> ());
  let sw = OpamSystem.mk_temp_dir ~prefix:"orb" () in
  let switch = OpamSwitch.of_string sw in
  let epoch = Unix.time () in
  if use_switches = None then begin
    install_switch epoch ?repos compiler_switches switch;
    clean_switches := (fun () ->
        if not keep_switches then begin
          remove_switch switch;
          OpamFilename.rmdir
            (OpamFilename.Dir.of_string (OpamSwitch.to_string switch))
        end else
          log "Switch is %s" (OpamSwitch.to_string switch))
  end else
    update_switch_env epoch switch;
  log "environments extended, installing";

  (try install switch atoms_or_locals
   with (OpamStd.Sys.Exit _) as e -> !clean_switches (); raise e);
  log "installed";

  let tracking_map = tracking_maps switch atoms_or_locals in

    (* output build info:
       - export switch
       - recorded changes above (tracking_map)
       - ?export buildpath / environment?
    *)
  let bidir = OpamSystem.mk_temp_dir ~prefix:"buildinfo" () in
  log "%s" (OpamConsole.colorise `green "BUILD INFO");
  output_buildinfo bidir tracking_map;

  (* switch export - make a full one *)
  export (bidir ^ "/repo.export") switch;

  (* environment variables -- SOURCE_DATE_EPOCH and switch name for now *)
  let env = [
    "SOURCE_DATE_EPOCH", string_of_float epoch ;
    "BUILD_PATH", OpamSwitch.to_string switch ;
    (* ARCH/OS/OS-FAMILY/OS-DISTRIBUTION/OS-VERSION *)
  ] in
  write_file OpamFilename.(create (Dir.of_string bidir) (Base.of_string "env"))
    (String.concat "\n" (List.map (fun (k, v) -> k ^ "=" ^ v) env));
  if twice then begin
    let build1st = copy_build_dir bidir switch in
    !clean_switches ();
    clean_switches := (fun () -> ());
    let switch = OpamSwitch.of_string sw in
    let switch_in = OpamFile.make (OpamFilename.of_string (bidir ^ "/repo.export")) in
    import_switch epoch switch (Some switch_in);
    log "switch imported (installed stuff)";
    let build2nd = copy_build_dir bidir switch in
    let tracking_map' = tracking_maps switch atoms_or_locals in
    log "tracking map";
    log "%s" (OpamConsole.colorise `green "BUILD INFO");
    output_buildinfo bidir tracking_map';
    log "wrote tracking map";
    let final_map = diff_map tracking_map tracking_map' in
    if OpamPackage.Map.is_empty final_map then
      log "%s" (OpamConsole.colorise `green "It is reproductible!!!")
    else
      (log "There are some %s\n%s"
         (OpamConsole.colorise `red "mismatching hashes")
         (OpamPackage.Map.to_string
            (OpamStd.String.Map.to_string string_of_diff) final_map);
       OpamStd.Option.iter (generate_diffs build1st build2nd final_map) diffoscope);
    !clean_switches ()
  end

(** CLI *)
let orb_cmd =
  let open OpamArg in
  let doc = "Check reproducibility of opam packages" in
  let man = [
    `S "DESCRIPTION";
    `P "$(b,orb) is a tool to check the reproducibility of opam package builds. \
        It relies on an already installed opam, creates two switches and launches \
        the install of the packages in parallel, so that paths and time differ. \
        To check the reproducibility of the package itself, a first step installs \
        all dependencies, then a second tracks the changes after the installation \
        of PACKAGES.";
    `S "ARGUMENTS";
    `S "OPTIONS";
    `S OpamArg.build_option_section;
  ]
  in
  let diffoscope =
    mk_opt ["diffoscope"] "[DIR]"
      "use diffoscope to generate a report"
      Arg.(some dirname) ~vopt:(Some (OpamFilename.Dir.of_string "diffoscope")) None
  in
  let keep_switches =
    mk_flag ["keep-switches"] "keep built temporary switches"
  in
  let twice =
    mk_flag ["twice"] "build twice and output diffoscope"
  in
  let compiler_switches =
    mk_opt [ "compiler-switches" ] "[DIR]"
      "use the compiler in [DIR] for the temporary switches"
      Arg.(some string) None
  in
  let use_switches =
    let switches =
      let parse str =
        match List.map OpamSwitch.of_string (OpamStd.String.split str ',') with
        | [sw1; sw2] when sw1 <> sw2 -> `Ok (sw1, sw2)
        | _ -> `Error "Need two distinct switches in argument"
      in
      let print ppf (sw1,sw2) =
        Format.pp_print_string ppf
          (Printf.sprintf "%s,%s" (OpamSwitch.to_string sw1) (OpamSwitch.to_string sw2))
      in
      parse, print
    in
    mk_opt ["use-switches"] "SWITCH,SWITCH"
      "use those two switches instead of newly generated ones"
      Arg.(some switches) None
  in
  let repos =
    mk_opt ["repos"] "REPOS"
      "Include only packages that took their origin from one of the given \
       repositories (unless $(i,no-switch) is also specified, this excludes \
       pinned packages)."
      Arg.(some & list & OpamArg.repository_name) None
  in
  Term.((const orb $ global_options $ build_options
         $ diffoscope $ twice $ keep_switches $ compiler_switches $ use_switches
         $ repos $ atom_or_local_list)),
  Term.info "orb" ~man ~doc

let () =
  let buff = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buff in
  match Term.eval ~err:fmt ~catch:true orb_cmd with
  | `Error (`Term | `Parse) ->
    Format.pp_print_flush fmt ();
    OpamConsole.msg "%s" (Buffer.contents buff)
  | _ -> ()
