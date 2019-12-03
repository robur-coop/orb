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
open OpamStateTypes

(** Utils *)
let log ?num fmt =
  let orb =
    let orb = "[ORB]" in
    let col s = OpamConsole.(colorise `bold s |> colorise `cyan) in
    match num with
    | Some n -> col (Printf.sprintf "%s[%d]" orb n)
    | None -> col orb
  in
  OpamConsole.msg ("\n%s "^^fmt^^"\n\n") orb

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

let clean_switches = ref (fun () -> ())
let exit_error reason ?num fmt =
  !clean_switches ();
  let str = OpamStd.Option.to_string ~none:"" (Printf.sprintf "[%d] ") num in
  OpamConsole.error_and_exit reason ("%s"^^fmt) str

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

let drop_states ?gt ?rt ?st () =
  OpamStd.Option.iter OpamSwitchState.drop st;
  OpamStd.Option.iter OpamRepositoryState.drop rt;
  OpamStd.Option.iter OpamGlobalState.drop gt

let target = "/opt/share/ocaml"

let add_env =
  let variables = [
    "SOURCE_DATE_EPOCH", OpamParserTypes.Eq, string_of_float (Unix.time ()),
    Some "Reproducible builds date" ;
  ] in
  fun switch gt st ->
    let env = OpamFile.Switch_config.env st.switch_config in
    let value = target ^ "=" ^ OpamSwitch.to_string switch in
    log "BPPM is %s!" (OpamConsole.colorise `green value);
    let prefix_map =
      "BUILD_PATH_PREFIX_MAP", OpamParserTypes.Eq,
      value, Some "Build path prefix map"
    in
    let to_add =
      List.fold_left (fun swc v ->
          let s,_,_,_ = v in
          match OpamStd.List.find_opt (fun (s',_,_,_) -> s = s') env with
          | Some _ -> swc
          | None -> v::swc) [] (prefix_map :: variables)
    in
    let switch_config =
      OpamFile.Switch_config.with_env (to_add @ env) st.switch_config
    in
    let st = { st with switch_config } in
    let switch = st.switch in
    OpamFile.Switch_config.write
      (OpamPath.Switch.switch_config gt.root switch) st.switch_config;
    st

(** Steps *)
let install_switch compiler_switch num switch =
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
  let gt, st =
    OpamSwitchCommand.install gt ~rt ~local_compiler:true
      ~packages:[] ~update_config:false switch
  in
  let st = match compiler_switch with
    | None -> st
    | Some path ->
      let src = `Source (OpamUrl.parse ("git+file://" ^ path)) in
      let st =
        OpamClient.PIN.pin st
          (OpamPackage.Name.of_string "ocaml-variants")
          src
      in
      OpamSwitchCommand.set_compiler st
        [OpamPackage.Name.of_string "ocaml-variants", None]
  in
  let st = add_env switch gt st in
  log ~num "Switch %s created!"
    (OpamConsole.colorise `green (OpamSwitch.to_string switch));
  drop_states ~gt ~rt ~st ()

let update_switch_env num switch =
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  if not (OpamGlobalState.switch_exists gt switch) then
    (drop_states ~gt ();
     exit_error `Not_found ~num "Switch %s doesn't exist"
       (OpamSwitch.to_string switch |> OpamConsole.colorise `underline))
  else
    OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
    OpamSwitchState.with_ `Lock_write ~rt ~switch gt @@ fun st ->
    let st = add_env switch gt st in
    drop_states ~gt ~rt ~st ()

let install num switch atoms_or_locals =
  log ~num "Install start";
  if Sys.file_exists target then
    exit_error `Not_found
      "target of build path prefix map %s already exists" target;
  Unix.symlink (OpamSwitch.to_string switch) target;
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
  OpamSwitchState.with_ `Lock_write ~rt ~switch gt @@ fun st ->
  let st, atoms =
    OpamAuxCommands.autopin st ~simulate:true atoms_or_locals
  in
  (* Remove already installed ones *)
  let st =
    let installed =
      OpamPackage.Name.Set.Op.(
        OpamPackage.names_of_packages
          (OpamFormula.packages_of_atoms st.packages atoms)
        %% OpamPackage.names_of_packages st.installed)
    in
    if OpamPackage.Name.Set.is_empty installed then st else
      (log ~num "Remove previsouly installed packages: %s"
         (OpamPackage.Name.Set.to_string installed);
       OpamClient.remove st ~autoremove:false ~force:false atoms)
  in
  let st =
    log ~num "Install deps of %s" (OpamFormula.string_of_atoms atoms);
    OpamClient.install st atoms ~deps_only:true
  in
  log ~num "Install %s" (OpamFormula.string_of_atoms atoms);
  let st = OpamClient.install st atoms in
  Sys.remove target;
  drop_states ~gt ~rt ~st ()

let tracking_maps switch atoms_or_locals =
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_none ~switch gt @@ fun st ->
  let st, packages =
    let p =
      OpamAuxCommands.resolve_locals atoms_or_locals
      |> snd
      |> OpamFormula.packages_of_atoms st.packages
      |> OpamPackage.Set.inter st.installed
    in
    st, p
  in
  let tr =
    OpamPackage.Set.fold (fun pkg acc ->
        let name = OpamPackage.name pkg in
        let changes_file =
          OpamPath.Switch.changes gt.root switch name
          |> OpamFile.Changes.read
        in
        OpamPackage.Map.add pkg changes_file acc)
      packages OpamPackage.Map.empty
  in
  drop_states ~gt ~st ();
  tr

type diff =
  | Both of OpamDirTrack.change * OpamDirTrack.change
  | First of OpamDirTrack.change
  | Second of OpamDirTrack.change

let string_of_diff = function
  | Both (c,c') ->
    OpamDirTrack.string_of_change c ^ " / " ^ OpamDirTrack.string_of_change c'
  | First c -> "๛ / " ^ OpamDirTrack.string_of_change c
  | Second c -> OpamDirTrack.string_of_change c ^ " / ๛"

(* Calculate the final diff map *)
let diff_map track1 track2 =
  OpamPackage.Map.fold (fun pkg sm1 map ->
      let file_map =
        match OpamPackage.Map.find_opt pkg track2 with
        | Some sm2 ->
          let diff, rest =
            OpamStd.String.Map.fold (fun file d1 (diff, sm2) ->
                match OpamStd.String.Map.find_opt file sm2 with
                | Some d2 ->
                  let diff =
                    if d1 <> d2 then
                      OpamStd.String.Map.add file (Both (d1,d2)) diff
                    else diff
                  in
                  diff,
                  OpamStd.String.Map.remove file sm2
                | None ->
                  OpamStd.String.Map.add file (First d1) diff, sm2)
              sm1 (OpamStd.String.Map.empty, sm2)
          in
          if OpamStd.String.Map.is_empty rest then diff else
            OpamStd.String.Map.union (fun _ _ -> assert false) diff
              (OpamStd.String.Map.map (fun d2 -> Second d2) rest)
        | None -> assert false
      in
      if OpamStd.String.Map.is_empty file_map then map
      else OpamPackage.Map.add pkg file_map map
    ) track1 OpamPackage.Map.empty

let generate_diffs root switches final_map dir =
  let open OpamFilename.Op in
  OpamFilename.mkdir dir;
  OpamPackage.Map.iter (fun pkg _ ->
      OpamFilename.mkdir (dir / OpamPackage.to_string pkg)) final_map;
  let full =
    let root1, root2 =
      get2 (List.map (fun (_,sw) -> OpamPath.Switch.root root sw) switches)
    in
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

let remove_switch num switch =
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  OpamGlobalState.drop @@
  OpamSwitchCommand.remove ~confirm:false gt switch;
  log ~num "Switch %s removed"
    (OpamSwitch.to_string switch |> OpamConsole.colorise `blue)

(* Main function *)
let orb global_options build_options diffoscope keep_switches compiler_switches use_switches
    atoms_or_locals =
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
  (* OpamStateConfig.update ~unlock_base:true (); *)
  let switches =
    match use_switches with
    | Some (sw1, sw2) ->
      OpamConsole.note "Use switches %s and %s"
        (OpamConsole.colorise `blue (OpamSwitch.to_string sw1))
        (OpamConsole.colorise `blue (OpamSwitch.to_string sw2));
      [1, sw1; 2, sw2]
    | None ->
      list_init 2 (fun i ->
          i, OpamSwitch.of_string (OpamSystem.mk_temp_dir ~prefix:"orb" ()))
  in
  if use_switches = None then
    (seq switches (fun (num,sw) -> install_switch compiler_switches num sw);
     clean_switches := (fun () ->
         (try Sys.remove target with _ -> ());
         if not keep_switches then
           seq switches (fun (num,sw) ->
               remove_switch num sw;
               OpamFilename.rmdir
                 (OpamFilename.Dir.of_string (OpamSwitch.to_string sw)))
         else
           log "Switches are %s" (OpamStd.List.to_string (fun (num, sw) ->
               Printf.sprintf "#%d - %s" num (OpamSwitch.to_string sw)) switches)))
  else
    seq switches (fun (num, sw) -> update_switch_env num sw);
  log "environments extended, installing";
  (try
     seq switches (fun (num,sw) -> install num sw atoms_or_locals)
   with
     (OpamStd.Sys.Exit _) as e -> !clean_switches (); raise e);
  log "installed";
  let tracking_maps =
    List.map (fun (_,sw) -> tracking_maps sw atoms_or_locals) switches
  in
  (* calculate tracking maps *)
  let final_map =
    let tr1, tr2 = get2 tracking_maps in
    diff_map tr1 tr2
  in
  if OpamPackage.Map.is_empty final_map then
    log "%s" (OpamConsole.colorise `green "It is reproductible!!!")
  else
    (log "There are some %s\n%s"
       (OpamConsole.colorise `red "mismatching hashes")
       (OpamPackage.Map.to_string
          (OpamStd.String.Map.to_string string_of_diff) final_map);
     OpamStd.Option.iter (generate_diffs root switches final_map) diffoscope);
  !clean_switches ()

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
  Term.((const orb $ global_options $ build_options
         $ diffoscope $ keep_switches $ compiler_switches $ use_switches
         $ atom_or_local_list)),
  Term.info "orb" ~man ~doc

let () =
  let buff = Buffer.create 0124 in
  let fmt = Format.formatter_of_buffer buff in
  match Term.eval ~err:fmt ~catch:true orb_cmd with
  | `Error (`Term | `Parse) ->
    Format.pp_print_flush fmt ();
    OpamConsole.msg "%s" (Buffer.contents buff)
  | _ -> ()
