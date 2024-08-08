(**************************************************************************)
(*                                                                        *)
(*    Copyright 2019 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamStateTypes

let cli = OpamArg.cli2_1, `Default

(** Utils *)
let log fmt =
  let orb = OpamConsole.(colorise `bold "[ORB]" |> colorise `cyan) in
  OpamConsole.msg ("%s "^^fmt^^"\n") orb

let ts now =
  let open Unix in
  let tm = gmtime now in
  Format.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let read_file file =
  let filename = OpamFilename.to_string file in
  let ic =
    OpamFilename.(mkdir (dirname file));
    try open_in_bin filename with Sys_error _ -> raise (OpamSystem.File_not_found filename)
  in
  try
    let len = in_channel_length ic in
    let data = really_input_string ic len in
    close_in ic;
    data
  with e ->
    OpamStd.Exn.finalise e @@ fun () ->
    close_in ic

let write_file file value =
  let filename = OpamFilename.to_string file in
  let oc =
    OpamFilename.(mkdir (dirname file));
    try open_out_bin filename with Sys_error _ -> raise (OpamSystem.File_not_found filename)
  in
  try
    Unix.lockf (Unix.descr_of_out_channel oc) Unix.F_LOCK 0;
    output_string oc value;
    close_out oc;
  with e ->
    OpamStd.Exn.finalise e @@ fun () ->
    close_out oc; OpamFilename.remove file

let dot_switch = "opam-switch"
let dot_hash = ".build-hashes"
let dot_env = "build-environment"
let dot_packages = "system-packages"
let dot_build = "build"

(* custom opam extensions *)
let opam_monorepo_duni = "x-opam-monorepo-duniverse-dirs"
let orb_deps = "x-orb-dependencies"
let mirage_opam_lock = "x-mirage-opam-lock-location"
let mirage_configure = "x-mirage-configure"
let mirage_pre_build = "x-mirage-pre-build"
let mirage_extra_repo = "x-mirage-extra-repo"

let build_dir () =
  OpamFilename.Dir.of_string
    Filename.(concat (get_temp_dir_name ()) "orb-build")

let orb_data = "2"

let custom_env_keys = [
  "OS" ; "OS_DISTRIBUTION" ; "OS_VERSION" ; "OS_FAMILY" ; "SWITCH_PATH" ; "ORB_DATA"
]

let retrieve_opam_vars gt_vars = {
  Common.os = OpamSysPoll.os gt_vars ;
  os_distribution = OpamSysPoll.os_distribution gt_vars ;
  os_version = OpamSysPoll.os_version gt_vars ;
  os_family = OpamSysPoll.os_family gt_vars ;
  arch = OpamSysPoll.arch gt_vars ;
}

let custom_env vars s = [
  "OS", vars.Common.os;
  "OS_DISTRIBUTION", vars.os_distribution;
  "OS_VERSION", vars.os_version;
  "OS_FAMILY", vars.os_family;
  "ARCH", vars.arch;
  "ORB_DATA", Some orb_data;
  "SWITCH_PATH", s
]

let create_env vars (s : string) =
  Array.to_list (Unix.environment () ) @
  List.fold_left
    (fun acc (k, v) -> match v with None -> acc | Some v -> (k^"="^v)::acc)
    [] (custom_env vars (Some s))

let env_to_string env =
  String.concat "\n" env

let env_of_string str =
  let lines = String.split_on_char '\n' str in
  List.fold_left (fun acc line ->
      match String.split_on_char '=' line with
      | [ key ; value ] -> (key, value) :: acc
      | [ "" ] -> acc
      | _ -> Printf.printf "bad environment line %s\n" line ; acc)
    [] lines

let dump_system_packages ~os ~os_family filename =
  match os, os_family with
  | Some "freebsd", _ ->
    let r = Sys.command ("/usr/sbin/pkg query %n-%v > " ^ filename) in
    if r <> 0 then log "failed to dump system packages (exit %d)" r
  | Some "linux", Some "debian" ->
    let r = Sys.command ("dpkg-query --showformat='${Package}=${Version}\n' -W > " ^ filename) in
    if r <> 0 then log "failed to dump system packages (exit %d)" r
  | Some os, Some family ->
    log "unsupported OS for host system packages (os=%s, os-family=%s)" os family
  | _, _ ->
    log "unsupported OS (no system packages)"

let install_system_packages ~os ~os_family dir =
  let filename = Printf.sprintf "%s/%s" dir dot_packages in
  match os, os_family with
  | Some "freebsd", _ ->
    let r = Sys.command ("cat " ^ filename ^ " | xargs /usr/sbin/pkg install -y") in
    if r <> 0 then log "couldn't install packages"
  | Some "linux", Some "debian" ->
    let r = Sys.command ("cat " ^ filename ^ " | xargs apt-get install -y") in
    if r <> 0 then log "couldn't install packages"
  | Some os, Some family ->
    log "unsupported OS for host system packages (os=%s, os-family=%s)" os family
  | _, _ ->
    log "unsupported OS (no system packages)"

let remove_switch switch =
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  OpamGlobalState.drop @@
  OpamSwitchCommand.remove ~confirm:false gt switch;
  log "Switch %s removed"
    (OpamSwitch.to_string switch |> OpamConsole.colorise `blue)

let output_system_packages_and_env ~skip_system ~os ~os_family dir env =
  let prefix = OpamFilename.Dir.of_string dir in
  (* output metadata: hashes, environment, system packages *)
  let filename file =
    OpamFilename.(create prefix (Base.of_string file))
  in
  let fn = filename dot_env in
  write_file fn (env_to_string env);
  if not skip_system then begin
    let pkgs = filename dot_packages in
    dump_system_packages ~os ~os_family (OpamFilename.to_string pkgs)
  end

let drop_states ?gt ?rt ?st () =
  OpamStd.Option.iter OpamSwitchState.drop st;
  OpamStd.Option.iter OpamRepositoryState.drop rt;
  OpamStd.Option.iter OpamGlobalState.drop gt

let switch_filename dir =
  let fn = Printf.sprintf "%s/%s" dir dot_switch in
  OpamFile.make (OpamFilename.of_string fn)

let export switch dir =
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
  let switch_out = switch_filename dir in
  OpamSwitchCommand.export rt ~freeze:true ~full:true ~switch (Some switch_out);
  drop_states ~gt ~rt ()

let clean_switch = ref None

let cleanup () =
  log "cleaning up";
  match !clean_switch with
  | None -> ()
  | Some (switch, skip_system, dir, sw) ->
    let gt_vars =
      OpamGlobalState.with_ `Lock_none (fun { OpamStateTypes.global_variables; _ } -> global_variables)
    in
    let env_vars = retrieve_opam_vars gt_vars in
    output_system_packages_and_env ~skip_system ~os:env_vars.os ~os_family:env_vars.os_family dir (create_env env_vars sw);
    export switch dir;
    remove_switch switch;
    OpamFilename.rmdir
      (OpamFilename.Dir.of_string (OpamSwitch.to_string switch));
    clean_switch := None

let exit_error reason fmt =
  cleanup ();
  OpamConsole.error_and_exit reason fmt

let convert_date x = string_of_int (int_of_float x)

let os_matches env =
  (* TODO may relax e.g. OS_VERSION is DISTRIBUTION is detailed enough *)
  let opt_compare key v =
    match v, List.assoc_opt key env with
    | None, _ -> log "key %s not available" key ; true
    | _, None -> log "key %s not found in environment" key ; true
    | Some v, Some v' ->
      if String.equal v v' then
        true
      else begin
        log "value for key %s differ: %S vs %S" key v v';
        false
      end
  in
  let gt_vars =
    OpamGlobalState.with_ `Lock_none (fun { OpamStateTypes.global_variables; _ } -> global_variables)
  in
  let vars = retrieve_opam_vars gt_vars in
  List.for_all (fun (k, v) -> opt_compare k v) (custom_env vars None)

(** Steps *)
let import_switch skip_system dir sw switch export =
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
  let (), st =
    OpamSwitchCommand.create gt ~rt ~update_config:false
      ~invariant:OpamFormula.Empty switch (fun st -> (), st)
  in
  log "Switch %s created!"
    (OpamConsole.colorise `green (OpamSwitch.to_string switch));
  clean_switch := Some (switch, skip_system, dir, sw);
  log "now importing switch";
  (* TODO fake-install opam-monorepo *)
  let st = OpamSwitchCommand.import st export in
  log "Switch %s imported!"
    (OpamConsole.colorise `green (OpamSwitch.to_string switch));
  drop_states ~gt ~rt ~st ()

let install_switch ?repos switch =
  OpamStateConfig.update ~unlock_base:true ();
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
  let (), st =
    OpamSwitchCommand.create gt ~rt ?repos ~update_config:false
      ~invariant:OpamFormula.Empty switch (fun st -> (), st)
  in
  log "Switch %s created!"
    (OpamConsole.colorise `green (OpamSwitch.to_string switch));
  drop_states ~gt ~rt ~st ()

let install ?deps_only switch atom =
  log "Install start";
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
  OpamSwitchState.with_ `Lock_write ~rt ~switch gt @@ fun st ->
  log "Install %s" (OpamFormula.string_of_atom atom);
  try
    let add_to_roots = match deps_only with
      | None -> None
      | Some x -> Some (not x)
    in
    let st = OpamClient.install ?add_to_roots ?deps_only st [ atom ] in
    log "Installed %s" (OpamFormula.string_of_atom atom);
    gt, rt, st
  with
  | OpamStd.Sys.Exit n ->
    log "Installation failed with %d" n;
    exit n
  | e ->
    log "Exception while installing: %s" (Printexc.to_string e);
    exit 1

let tracking_map switch package =
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_none ~switch gt @@ fun st ->
  log "tracking map got locks";
  let changes =
    OpamPath.Switch.changes gt.root switch (OpamPackage.name package)
    |> OpamFile.Changes.read
  in
  log "got tracking map, dropping states";
  drop_states ~gt ~st ();
  changes

let read_tracking_map dir package =
  let nam = Printf.sprintf "%s/%s%s" dir package dot_hash in
  OpamFile.Changes.read_opt (OpamFile.make (OpamFilename.of_string nam))

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
let diff_map sm1 sm2 =
  if OpamStd.String.Map.compare compare sm1 sm2 = 0 then
    OpamStd.String.Map.empty
  else
    let diff, rest =
      OpamStd.String.Map.fold (fun file d1 (diff, sm2) ->
          match OpamStd.String.Map.find_opt file sm2 with
          | Some d2 ->
            let diff =
              if d1 <> d2 then begin
                log "%s d1 and d2 mismatch" file;
                OpamStd.String.Map.add file (Both (d1, d2)) diff
              end else diff
            in
            diff,
            OpamStd.String.Map.remove file sm2
          | None ->
            log "diff map for %s not found in sm2" file;
            OpamStd.String.Map.add file (First d1) diff, sm2)
        sm1 (OpamStd.String.Map.empty, sm2)
    in
    if OpamStd.String.Map.is_empty rest then
      diff
    else
      OpamStd.String.Map.union (fun _ _ -> assert false) diff
        (OpamStd.String.Map.map (fun d2 -> Second d2) rest)

let output_artifacts sw_prefix dir pkg changes =
  let prefix = OpamFilename.Dir.of_string dir in
  (* copy artifacts *)
  OpamStd.String.Map.iter (fun name -> function
      | OpamDirTrack.Added _ ->
        let src = Format.sprintf "%s/%s" sw_prefix name
        and tgt = Format.sprintf "%s/%s" dir name
        in
        let r = Sys.command ("mkdir -p " ^ Filename.dirname tgt) in
        if r <> 0 then log "failed to create directory for %s" tgt
        else begin
          let r = Sys.command ("cp " ^ src ^ " " ^ tgt) in
          if r <> 0 then log "failed to copy %s to %s" src tgt
        end
      | _ -> ()) changes;
  (* output metadata: hashes, environment, system packages *)
  let filename file =
    OpamFilename.(create prefix (Base.of_string file))
  in
  let fn = filename OpamPackage.(Name.to_string pkg.name ^ dot_hash) in
  log "writing %s" (OpamFilename.to_string fn);
  write_file fn (OpamFile.Changes.write_to_string changes)

let find_build_dir dir =
  let dir = Printf.sprintf "%s/%s" dir dot_build in
  if Sys.file_exists dir then Some dir else None

let copy_build_dir tgt src =
  let target = Printf.sprintf "%s/%s" tgt dot_build in
  log "preserving build dir in %s" target;
  OpamFilename.copy_dir
    ~src:(OpamFilename.Dir.of_string src)
    ~dst:(OpamFilename.Dir.of_string target);
  target

let common_start global_options disable_sandboxing build_options cache =
  (* all environment variables need to be set/unset before the following line,
     which forces the lazy Unix.environment in OpamStd *)
  OpamArg.apply_global_options cli global_options;
  OpamArg.apply_build_options cli build_options;
  let root = OpamStateConfig.(!r.root_dir) in
  let config_f = OpamPath.config root in
  let already_init = OpamFile.exists config_f in
  OpamCoreConfig.update ~precise_tracking:true ~yes:(Some true) ~confirm_level:`unsafe_yes ();
  if not already_init then begin (* could also be assert *)
    let init_config = OpamInitDefaults.init_config ~sandboxing:(not disable_sandboxing) () in
    let repo_url = "/tmp/nonexisting" in
    write_file
      OpamFilename.(create (Dir.of_string repo_url) (Base.of_string "repo"))
      "opam-version: \"2.0\"";
    let repo = OpamTypes.{ repo_name = OpamRepositoryName.of_string "empty" ;
                           repo_url = OpamUrl.parse repo_url ;
                           repo_trust = None }
    in
    let gt, rt, _default_compiler =
      OpamClient.init ~init_config ~interactive:false ~repo ~bypass_checks:true
        ~update_config:false ~completion:false
        (OpamStd.Sys.guess_shell_compat ())
    in
    drop_states ~gt ~rt ();
    (* this code block is here until ocaml/opam#5315 is solved, and we can pass through init_config *)
    (match cache with
     | None -> ()
     | Some url ->
       let config = OpamFile.Config.read config_f in
       let config =
         OpamFile.Config.with_dl_cache [ OpamUrl.of_string url ] config
       in
       OpamFile.Config.write config_f config);
  end;
  OpamStd.Sys.at_exit cleanup

let compare_builds changes changes' build_dir build_dir' =
  let final_map = diff_map changes changes' in
  if OpamStd.String.Map.is_empty final_map then
    log "%s" (OpamConsole.colorise `green "It is reproducible!!!")
  else begin
    log "There are some %s\n%s"
      (OpamConsole.colorise `red "mismatching hashes")
      (OpamStd.String.Map.to_string string_of_diff final_map);
    match build_dir with
    | Some b -> log "Plaese compare build directories %s and %s" b build_dir'
    | _ -> ()
  end

let read_env dir =
  let env_file = Filename.concat dir dot_env in
  env_of_string (read_file (OpamFilename.of_string env_file))

external unsetenv : string -> unit = "orb_unsetenv"

module S = Set.Make(String)

let strip_path () =
  let current_path = try Some (Unix.getenv "PATH") with Not_found -> None in
  let stripped_path =
    let allowed =
      S.of_list [ "/bin" ; "/sbin" ; "/usr/bin" ; "/usr/sbin" ; "/usr/local/bin" ; "/usr/local/sbin" ]
    in
    let whitelisted x = S.mem x allowed in
    let our_path = match current_path with
      | None -> allowed
      | Some paths ->
        S.filter whitelisted (S.of_list (String.split_on_char ':' paths))
    in
    String.concat ":" (S.elements our_path)
  in
  stripped_path

let strip_env ?(preserve = []) () =
  Array.iter (fun k ->
      let key =
        match String.index_opt k '=' with
        | None -> k
        | Some idx -> String.sub k 0 idx
      in
      if List.mem key preserve then () else unsetenv key)
    (Unix.environment ())

(* from opamSolution.ml *)
let display_error action package error =
  let disp =
    OpamConsole.header_error "while %s %s" action (OpamPackage.to_string package)
  in
  match error with
  | Sys.Break | OpamParallel.Aborted -> ()
  | Failure s -> disp "%s" s
  | OpamSystem.Process_error e -> disp "%s" (OpamProcess.string_of_result e)
  | e ->
    disp "%s" (Printexc.to_string e);
    if OpamConsole.debug () then
      OpamConsole.errmsg "%s" (OpamStd.Exn.pretty_backtrace e)

let download_and_extract_job st package dirname =
  let open OpamProcess.Job.Op in
  OpamAction.download_package st package @@+ function
  | Some (_, s) ->
    display_error "download" package (Failure s);
    Done (Error ("failed to download " ^ s))
  | None ->
    let src = OpamSwitchState.source_dir st package in
    OpamFilename.copy_dir ~src ~dst:dirname;
    OpamAction.prepare_package_source st package dirname @@| function
    | None -> Ok ()
    | Some e ->
      display_error "preparing source" package e;
      Error ("failed to extract " ^ Printexc.to_string e)

let duniverse_dirs =
  let open OpamParserTypes.FullPos in
  let ( let* ) = Result.bind in
  let string ~ctx = function
    | { pelem = String s ; _ } -> Ok s
    | _ -> Error (`Msg ("couldn't find a string " ^ ctx))
  in
  let extract_data = function
    | { pelem = List { pelem = [ url ; dir ; hashes ] ; _ } ; _ } ->
      let* url = string ~ctx:"url" url in
      let url = OpamUrl.of_string url in
      let* hashes =
        match hashes with
        | { pelem = List { pelem = hashes ; _ } ; _ } ->
          List.fold_left (fun acc hash ->
              let* acc = acc in
              let* hash = string ~ctx:"hash" hash in
              let* h = match OpamHash.of_string_opt hash with
                | Some h -> Ok h
                | None -> Error (`Msg ("couldn't decode opam hash in " ^ hash))
              in
              Ok (h :: acc))
            (Ok []) hashes
        | _ -> Error (`Msg "couldn't decode hashes")
      in
      let* dir = string ~ctx:"directory" dir in
      Ok (url, dir, List.rev hashes)
    | _ -> Error (`Msg "expected a string or identifier")
  in
  function
  | { pelem = List { pelem = lbody ; _ } ; _ } ->
    let* data =
      List.fold_left (fun acc v ->
          let* acc = acc in
          let* data = extract_data v in
          Ok (data :: acc))
        (Ok []) lbody
    in
    Ok (List.rev data)
  | _ -> Error (`Msg "expected a list or a nested list")

let execute_commands dirname prefix cmds =
  OpamFilename.in_dir dirname (fun () ->
      let path = Unix.getenv "PATH" in
      let p' = prefix ^ "/bin:" ^ path in
      Unix.putenv "PATH" p';
      let r =
        List.fold_left (fun acc cmd_args ->
            Result.bind acc (fun () ->
                match cmd_args with
                | cmd :: args ->
                  let cmd = Filename.quote_command cmd args in
                  let r = Sys.command cmd in
                  if r <> 0 then
                    Error (cmd ^ " exited with " ^ string_of_int r)
                  else
                    Ok ()
                | [] -> Ok ()))
          (Ok ()) cmds
      in
      Unix.putenv "PATH" path;
      r)

let of_opam_value =
  (* TODO could use OpamFilter.commands .. .. *)
  let open OpamParserTypes.FullPos in
  let ( let* ) = Result.bind in
  let extract_data = function
    | { pelem = String s ; _ } -> Ok s
    | { pelem = Ident s ; _ } ->
      if String.equal s "make" then
        Ok (Lazy.force OpamStateConfig.(!r.makecmd))
      else
        Error (`Msg ("unexpected variable " ^ String.escaped s))
    | _ -> Error (`Msg "expected a string or identifier")
  in
  function
  | { pelem = List { pelem = lbody ; _ } ; _ } ->
    let* data =
      List.fold_left (fun acc v ->
          let* acc = acc in
          let* data = extract_data v in
          Ok (data :: acc))
        (Ok []) lbody
    in
    Ok (List.rev data)
  | _ -> Error (`Msg "expected a list")

let build_and_install st dirname package =
  let open OpamProcess.Job.Op in
  let pkg_name = OpamPackage.to_string package in
  log "now building %s" pkg_name;
  OpamAction.build_package st dirname package @@+ function
  | Some exn ->
    display_error "compiling" package exn;
    Done (Error ("failed to build package " ^ pkg_name ^ ": " ^ Printexc.to_string exn))
  | None ->
    log "built %s, now installing" pkg_name;
    OpamAction.install_package st ~build_dir:dirname package @@| function
    | Left conf ->
      log "installed %s, now registering" pkg_name;
      let conf_files =
        let add_conf conf = OpamPackage.Name.Map.add package.name conf st.conf_files in
        OpamStd.Option.map_default add_conf st.conf_files conf
      in
      Ok (OpamSwitchAction.add_to_installed {st with conf_files} ~root:true package)
    | Right exn ->
      display_error "installing" package exn;
      Error ("failed to install package " ^ pkg_name ^ ": " ^ Printexc.to_string exn)

let location_of_opam =
  let open OpamParserTypes.FullPos in
  function
  | { pelem = String s ; _ } -> Ok s
  | _ -> Error (`Msg "expected a string")

let rebuild ~skip_system ~sw ~bidir out =
  let switch = OpamSwitch.of_string sw in
  let switch_in = switch_filename bidir in
  let sw_exp = OpamFile.SwitchExport.read switch_in in
  let package =
    let p = sw_exp.OpamFile.SwitchExport.selections.OpamTypes.sel_roots in
    if OpamPackage.Set.cardinal p = 1 then
      OpamPackage.Set.choose p
    else begin
      log "multiple roots, unclear what to do"; exit 1
    end
  in
  let opam =
    OpamPackage.Name.Map.find (OpamPackage.name package)
      sw_exp.OpamFile.SwitchExport.overlays
  in
  let monorepo, switch_in =
    match OpamFile.OPAM.extended opam opam_monorepo_duni Fun.id with
    | None -> false, switch_in
    | Some _ ->
      let overlays =
        OpamPackage.Name.Map.remove (OpamPackage.name package)
          sw_exp.OpamFile.SwitchExport.overlays
      in
      let selections =
        let sel = sw_exp.OpamFile.SwitchExport.selections in
        { sel with
          sel_installed = OpamPackage.Set.remove package sel.sel_installed ;
          sel_roots = OpamPackage.Set.empty }
      in
      let sw_exp = { sw_exp with overlays ; selections } in
      let tmp = OpamFilename.of_string (Filename.temp_file "orb" "export") in
      OpamStd.Sys.at_exit (fun () -> OpamFilename.remove tmp);
      let tmp = OpamFile.make tmp in
      OpamFile.SwitchExport.write tmp sw_exp;
      true, tmp
  in
  import_switch skip_system out sw switch (Some switch_in);
  (if monorepo then begin
      log "extracting sources";
      OpamGlobalState.with_ `Lock_write @@ fun gt ->
      OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
      OpamSwitchState.with_ `Lock_write ~rt ~switch gt @@ fun st ->
      OpamSwitchCommand.switch `Lock_none gt switch;
      let dirname = build_dir () in
      OpamFilename.rmdir dirname;
      OpamFilename.mkdir dirname;
      OpamStd.Sys.at_exit (fun () ->
          if not (OpamClientConfig.(!r.keep_build_dir)) then OpamFilename.rmdir dirname);
      let st = OpamSwitchState.update_package_metadata package opam st in
      (match OpamProcess.Job.run (download_and_extract_job st package dirname) with
       | Ok () -> ()
       | Error msg -> log "%s" msg; exit 1);
      (match OpamFile.OPAM.extended opam opam_monorepo_duni duniverse_dirs with
       | None -> log "expected duniverse-dirs to be present" ; exit 1
       | Some Error `Msg msg -> log "failed to parse duniverse-dirs %s" msg ; exit 1
       | Some Ok v ->
         log "found %d duniverse dirs" (List.length v);
         let prefix =
           match OpamFile.OPAM.extended opam mirage_opam_lock location_of_opam with
           | None -> log "no mirage opam lock location in opam file"; exit 1
           | Some Error `Msg s -> log "error retrieving opam-lock-location %s" s; exit 1
           | Some Ok path ->
             match List.rev (String.split_on_char '/' path) with
             | _file'opam'locked :: _mirage :: tl -> String.concat "/" (List.rev tl)
             | _ -> log "expected a path with at least 2 elements, got %s" path; exit 1
         in
         let duni_dir = (if prefix = "" then "" else prefix ^ "/") ^ "duniverse" in
         OpamFilename.in_dir dirname (fun () ->
             let dir = OpamFilename.Dir.of_string duni_dir in
             OpamFilename.mkdir dir;
             OpamFilename.write
               OpamFilename.(create dir (Base.of_string "dune"))
               "(vendored_dirs *)");
         let jobs = OpamFile.Config.dl_jobs gt.config
         and cache_urls = OpamFile.Config.dl_cache gt.config
         and cache_dir = OpamRepositoryPath.download_cache gt.root
         in
         let pull_one (url, dir, hashes) =
           let open OpamProcess.Job.Op in
           let out = OpamFilename.Dir.(of_string (to_string dirname ^ "/" ^ duni_dir ^ "/" ^ dir)) in
           OpamRepository.pull_tree ~cache_dir ~cache_urls dir out hashes [ url ] @@| function
           | Result _ | Up_to_date _ -> Ok ()
           | Not_available (_, long_msg) ->
             Error ("failed to download " ^ OpamUrl.to_string url ^ " into " ^ dir ^ ": " ^ long_msg)
         in
         let rs = OpamParallel.map ~jobs ~command:pull_one v in
         match List.fold_left (fun acc r -> Result.bind acc (Fun.const r)) (Ok ()) rs with
         | Ok () -> log "downloaded %d tarballs" (List.length rs);
         | Error e -> log "download error %s" e; exit 1);
      (match OpamFile.OPAM.extended opam mirage_configure of_opam_value with
       | None -> log "failed to find %s" mirage_configure; exit 1
       | Some Error `Msg msg -> log "failed to parse %s: %s" mirage_configure msg; exit 1
       | Some Ok configure ->
         (match execute_commands dirname (Unix.getenv "PREFIX") [ configure ] with
          | Ok () -> ();
          | Error msg -> log "%s" msg; exit 1));
      let st =
        match OpamProcess.Job.run (build_and_install st dirname package) with
        | Ok st -> st
        | Error msg -> log "%s" msg; exit 1
      in
      drop_states ~gt ~rt ~st ()
    end);
  let changes = tracking_map switch package in
  output_artifacts (Unix.getenv "PREFIX") out package changes;
  let build2nd = if OpamClientConfig.(!r.keep_build_dir) then copy_build_dir out sw else sw in
  changes, build2nd, package

let add_repo s (name, url) =
  (* todo trust anchors *)
  let url = OpamUrl.parse url in
  OpamRepositoryCommand.add s name url None

let add_repos repos =
  let repos = List.map (fun (n, u) -> OpamRepositoryName.of_string n, u) repos in
  let names = List.map fst repos in
  OpamGlobalState.with_ `Lock_none (fun gt ->
      OpamRepositoryState.with_ `Lock_write gt (fun rt ->
          let rt = List.fold_left add_repo rt repos in
          let failed, _rt = OpamRepositoryCommand.update_with_auto_upgrade rt names in
          List.iter
            (fun rn -> log "repo update failed for %s" (OpamRepositoryName.to_string rn))
            failed
        ));
  names

let drop_slash s =
  let l = String.length s in
  if l > 0 && String.get s (l - 1) = '/' then
    String.sub s 0 (l - 1)
  else
    s

let repos_of_opam =
  let open OpamParserTypes.FullPos in
  let ( let* ) = Result.bind in
  let extract_string = function
    | { pelem = String s ; _ } -> Ok s
    | _ -> Error (`Msg "expected a string")
  in
  let extract_repo = function
    | { pelem = List { pelem = [ name ; url ] ; _ } ; _ } ->
      let* name = extract_string name in
      let* url = extract_string url in
      Ok (name, url)
    | { pelem = List { pelem = _lbody ; _ } ; _ } ->
      Error (`Msg "expected exactly two strings")
    | _ -> Error (`Msg "expected a pair of strings")
  in
  function
  | { pelem = List { pelem = lbody ; _ } ; _ } ->
    let* data =
      List.fold_left (fun acc v ->
          let* acc = acc in
          let* repo = extract_repo v in
          Ok (repo :: acc))
        (Ok []) lbody
    in
    Ok (List.rev data)
  | _ -> Error (`Msg "expected a list")

let modify_opam_file vars st package opam dirname =
  match OpamFile.OPAM.extended opam mirage_opam_lock location_of_opam with
  | None -> Ok st
  | Some Error `Msg s -> Error ("error retrieving opam-lock-location " ^ s);
  | Some Ok path ->
    let opam_lock =
      let base = OpamFilename.Base.of_string path in
      OpamFile.OPAM.read (OpamFile.make (OpamFilename.create dirname base))
    in
    match OpamFile.OPAM.extended opam_lock opam_monorepo_duni Fun.id with
    | None -> Error "expected duniverse-dirs to be present"
    | Some v ->
      let opam = OpamFile.OPAM.add_extension opam opam_monorepo_duni v in
      let opam =
        try
          let deps = Duni_deps.build_graph vars st package opam_lock opam in
          let deps = Duni_deps.deps_opam v.pos deps in
          OpamFile.OPAM.add_extension opam orb_deps deps
        with
        | Invalid_argument e ->
          log "error in duni_deps: %s" e;
          opam
      in
      Ok (OpamSwitchState.update_package_metadata package opam st)

(* Main function *)
let build global_options disable_sandboxing build_options twice
    repos cache out_dir switch_name epoch skip_system solver_timeout atom
    latest =
  let () =
    match latest, atom with
    | true, (_package, Some (`Eq, _version)) ->
      failwith ">:(" (* FIXME *)
    | _ -> ()
  in
  strip_env ~preserve:["HOME";"PATH"] ();
  Unix.putenv "PATH" (strip_path ());
  Unix.putenv "SOURCE_DATE_EPOCH"
    (convert_date (match epoch with
         | Some x -> float_of_string x
         | None -> Unix.time ()));
  (match solver_timeout with None -> () | Some x -> Unix.putenv "OPAMSOLVERTIMEOUT" x);
  Unix.putenv "OPAMERRLOGLEN" "0";
  let name = OpamPackage.Name.to_string (fst atom) in
  Unix.putenv "ORB_BUILDING_PACKAGE" name;
  let opam_root, sw =
    match switch_name with
    | None -> None, Filename.(basename (temp_file "orb" "temp"))
    | Some x ->
      let x = drop_slash x in
      if not (Filename.is_relative x) then
        Some (Filename.dirname x), Filename.basename x
      else
        try
          let _ = String.index x '/' in
          invalid_arg "either absolute path or no /"
        with
          Not_found -> None, x
  in
  Option.may (fun opamroot -> Unix.putenv "OPAMROOT" opamroot) opam_root;
  let bidir = match out_dir with None -> "." | Some dir -> drop_slash dir in
  let switch = OpamSwitch.of_string sw in
  let root = Option.default (Unix.getenv "HOME" ^ "/.opam") opam_root in
  let prefix = root ^ "/" ^ sw in
  Unix.putenv "PREFIX" prefix;
  common_start global_options disable_sandboxing build_options cache;
  log "using root %S and switch %S" root sw;
  if not OpamClientConfig.(!r.keep_build_dir) then
    clean_switch := Some (switch, skip_system, bidir, sw);
  let repos =
    let repos = match repos with
      | None | Some [] -> [ "default", "https://opam.ocaml.org" ]
      | Some xs -> xs
    in
    add_repos repos
  in
  install_switch ~repos switch;
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
  OpamSwitchState.with_ `Lock_write ~rt ~switch gt @@ fun st ->
  let env_vars =
    let gt_vars = gt.OpamStateTypes.global_variables in
    retrieve_opam_vars gt_vars
  in
  let package, atom =
    match latest, atom with
    | false, (name, None) ->
      begin try OpamSwitchState.get_package st name, atom with Not_found ->
        log "opam package %s not found" (OpamPackage.Name.to_string name); exit 1
      end
    | true, (name, None) ->
      begin match OpamPackage.max_version (Lazy.force st.available_packages) name with
        | package -> package, (name, Some (`Eq, package.version))
        | exception Not_found ->
          log "opam package %s not found" (OpamPackage.Name.to_string name); exit 1
      end
    | _, (_, Some (`Eq, _)) -> (* we error out earlier on latest = true in this case *)
      let packages = OpamSwitchState.packages_of_atoms st [ (atom :> OpamFormula.atom) ] in
      if OpamPackage.Set.cardinal packages = 1 then
        OpamPackage.Set.choose packages, atom
      else begin
        log "expecting exactly one, but found %d solutions for %s"
          (OpamPackage.Set.cardinal packages) (OpamFormula.string_of_atom (atom :> OpamFormula.atom));
        exit 1
      end
  in
  let opam = OpamSwitchState.opam st package in
  drop_states ~gt ~rt ~st ();
  begin
    match
      OpamFile.OPAM.extended opam mirage_configure of_opam_value,
      OpamFile.OPAM.extended opam mirage_pre_build of_opam_value
    with
    | None, None ->
      let gt, rt, st = install switch (atom :> OpamFormula.atom) in
      drop_states ~gt ~rt ~st ()
    | Some Ok configure, Some Ok pre_build ->
      log "installing dependencies";
      let gt, rt, st = install ~deps_only:true switch (atom :> OpamFormula.atom) in
      log "installed dependencies";
      let dirname = build_dir () in
      OpamFilename.rmdir dirname;
      OpamFilename.mkdir dirname;
      OpamStd.Sys.at_exit (fun () ->
          if not (OpamClientConfig.(!r.keep_build_dir)) then OpamFilename.rmdir dirname);
      (match OpamProcess.Job.run (download_and_extract_job st package dirname) with
       | Ok () -> ()
       | Error msg -> log "%s" msg; exit 1);
      (match OpamFile.OPAM.extended opam mirage_extra_repo repos_of_opam with
       | None -> ()
       | Some Error `Msg m -> log "error parsing extra repositories %s" m; exit 1
       | Some Ok repos ->
         let repo_names = add_repos repos in
         OpamSwitchState.update_repositories gt (fun old_repos ->
             repo_names @ old_repos) switch);
      drop_states ~gt ~rt ~st ();
      OpamGlobalState.with_ `Lock_write @@ fun gt ->
      OpamSwitchCommand.switch `Lock_none gt switch;
      drop_states ~gt ();
      (match execute_commands dirname prefix [ configure ; pre_build ] with
       | Ok () -> ();
       | Error msg -> log "%s" msg; exit 1);
      OpamGlobalState.with_ `Lock_none @@ fun gt ->
      OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
      OpamSwitchState.with_ `Lock_write ~rt ~switch gt @@ fun st ->
      let st =
        match modify_opam_file env_vars st package opam dirname with
        | Ok st -> st
        | Error msg -> log "%s" msg; exit 1
      in
      let st =
        match OpamProcess.Job.run (build_and_install st dirname package) with
        | Ok st -> st
        | Error msg -> log "%s" msg; exit 1
      in
      drop_states ~gt ~rt ~st ();
    | Some Error `Msg m, _ ->
      log "error parsing %s: %s" mirage_configure m;
      exit 1
    | _, Some Error `Msg m ->
      log "error parsing %s: %s" mirage_pre_build m;
      exit 1
    | None, Some _ ->
      log "only %s, but no %s present" mirage_pre_build mirage_configure;
      exit 1
    | Some _, None ->
      log "only %s, but no %s present" mirage_configure mirage_pre_build;
      exit 1
  end;
  let changes = tracking_map switch package in
  output_artifacts prefix bidir package changes;
  let build1st =
    if OpamClientConfig.(!r.keep_build_dir)
    then Some (copy_build_dir bidir prefix)
    else None
  in
  if OpamClientConfig.(!r.keep_build_dir) then begin
    output_system_packages_and_env ~skip_system ~os:env_vars.os ~os_family:env_vars.os_family bidir (create_env env_vars sw);
    export switch bidir;
  end;
  cleanup ();
  if twice then begin
    let outdir =
      if Filename.is_relative bidir then
        bidir ^ "../orb2"
      else
        bidir ^ "2"
    in
    log "second run, outdir is %s" outdir;
    let changes', build2nd, _ = rebuild ~skip_system ~sw ~bidir outdir in
    compare_builds changes changes' build1st build2nd
  end

let rebuild global_options disable_sandboxing build_options skip_system ignore_env_difference cache out_dir build_info =
  if build_info = "" then failwith "require build info directory" else begin
    strip_env ~preserve:["PATH"] ();
    Unix.putenv "PATH" (strip_path ());
    let out = match out_dir with None -> "." | Some dir -> drop_slash dir in
    let stripped_build_info = drop_slash build_info in
    let env = read_env stripped_build_info in
    List.iter (fun (k, v) -> if List.mem k custom_env_keys then () else Unix.putenv k v) env;
    let sw = List.assoc "SWITCH_PATH" env in
    (if os_matches env then
       log "environment matches"
     else if ignore_env_difference then
       log "environment differs (ignored)"
     else
       failwith "environment does not match");
    let os = List.assoc_opt "OS" env and os_family = List.assoc_opt "OS_FAMILY" env in
    if not skip_system then install_system_packages ~os ~os_family stripped_build_info;
    common_start global_options disable_sandboxing build_options cache;
    OpamStateConfig.update ~unlock_base:true ();
    let changes', build2nd, package = rebuild ~skip_system ~sw ~bidir:build_info out in
    let pkg_name = OpamPackage.Name.to_string package.OpamPackage.name in
    match read_tracking_map stripped_build_info pkg_name with
    | None -> log "no tracking map for %s found" pkg_name; exit 1
    | Some changes ->
      let build1st = find_build_dir stripped_build_info in
      log "comparing with old build dir %s" (match build1st with None -> "no" | Some x -> x);
      compare_builds changes changes' build1st build2nd
  end

(** CLI *)
open Cmdliner
open OpamArg

let validity = cli_from cli2_1

let mk_flag a b = mk_flag ~cli validity a b
let mk_opt a b c d e = mk_opt ~cli validity a b c d e

let skip_system =
  mk_flag [ "skip-system" ] "skip system packages"

let ignore_env_differences =
  mk_flag [ "ignore-env-differences" ] "ignore environment differences"

let out_dir =
  mk_opt [ "out" ] "[DIR]"
    "Use [DIR] as build info prefix (defaults to .)."
    Arg.(some string) None

let disable_sandboxing =
  mk_flag ["disable-sandboxing"]
    "Use a default configuration with sandboxing disabled. Use this at your \
     own risk, without sandboxing it is possible for a broken package script \
     to delete all your files."

let cache =
  mk_opt [ "cache" ] "[URL]"
    "Use [URL] as download cache."
    Arg.(some string) None

let build_cmd =
  let doc = "Build opam packages and output build information for rebuilding" in
  let man = [
    `S "DESCRIPTION";
    `P "$(b,build) is a tool to check the reproducibility of opam package builds. \
        It relies on an already installed opam, and creates required build \
        information for reproducing the same output.";
    `S "ARGUMENTS";
    `S "OPTIONS";
  ] @ OpamArg.man_build_option_section
  in
  let twice =
    mk_flag [ "twice" ] "build twice, output differences"
  in
  let repos =
    mk_opt [ "repos" ] "REPOS"
      "Include only packages that took their origin from one of the given \
       repositories."
      Arg.(some & list & pair ~sep:':' string string) None
  in
  let switch_name =
    mk_opt [ "switch-name" ] "[NAME]"
      "use [NAME] as switch name (defaults to \
       `Filename.(basename (temp_file 'orb' 'temp'))`). An absolute path will \
       set OPAMROOT to the directory, and the switch name is the last segment."
      Arg.(some string) None
  in
  let source_date_epoch =
    mk_opt [ "date" ] "DATE"
      "Use date as source date epoch (seconds since Unix epoch, defaults to Unix.time)."
      Arg.(some string) None
  in
  let solver_timeout =
    mk_opt [ "solver-timeout" ] "[seconds]"
      "use the provided solver timeout instead of the default (sets OPAMSOLVERTIMEOUT)"
      Arg.(some string) None
  in
  let latest =
    mk_flag [ "latest" ] "build the latest version"
  in
  let atom_exact =
    let atom_exact =
      let parse s =
        match Arg.conv_parser atom s with
        | (Ok (_, None) | Ok (_, Some (`Eq, _))) as v -> v
        | Ok (_, Some (_relop, _version)) ->
          Error (`Msg "only exact version match constraint allowed")
        | Error _ as e -> e
      in
      let printer ppf a =
        Arg.conv_printer atom ppf (a :> OpamFormula.atom)
      in
      Arg.conv (parse, printer)
    in
    let info_ =
      let doc = "Package name, with an optional version, e.g `pkg' or `pkg.1.0.0'." in
      Arg.info ~docv:"PACKAGE" ~doc []
    in
    Arg.(required & pos 0 (some atom_exact) None & info_)
  in
  let term =
    Term.((const build $ global_options cli $ disable_sandboxing $ build_options cli
           $ twice $ repos $ cache $ out_dir $ switch_name $ source_date_epoch $ skip_system
           $ solver_timeout $ atom_exact $ latest))
  and info = Cmd.info "build" ~man ~doc
  in
  Cmd.v info term

let rebuild_cmd =
  let doc = "Rebuild opam packages based on build information" in
  let man = [
    `S "DESCRIPTION";
    `P "$(b,rebuild) is a tool to rebuild an opam package using an exported \
        opam switch, a build environment, and reported build hashes. If an \
        existing build environment matches the host operating system, the \
        hashes of the installed files of all roots in the export are compared \
        against the previous build.";
    `S "ARGUMENTS";
    `S "OPTIONS";
  ] @ OpamArg.man_build_option_section
  in
  let build_info =
    let doc = "use build information in the provided directory" in
    Arg.(value & pos 0 string "" & info [] ~doc ~docv:"DIR")
  in
  let term =
    Term.((const rebuild $ global_options cli $ disable_sandboxing $ build_options cli
           $ ignore_env_differences $ skip_system $ cache $ out_dir $ build_info))
  and info = Cmd.info "rebuild" ~man ~doc
  in
  Cmd.v info term

let help man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some _ -> List.iter print_endline cmds; `Ok ()

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  Term.(ret (const help $ Arg.man_format $ Term.choice_names $ topic))

let cmds = [ build_cmd ; rebuild_cmd ]

let () =
  let buff = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buff in
  OpamSystem.init ();
  let info =
    let doc = "OPAM reproducible builder" in
    let man = [
      `S "DESCRIPTION" ;
      `P "$(tname) builds opam packages, checking for reproducibility." ;
    ] in
    Cmd.info "orb" ~version:"%%VERSION%%" ~doc ~man
  in
  let group = Cmd.group ~default:help_cmd info cmds in
  let r = Cmd.eval group ~err:fmt in
  if r = 0 then
    ()
  else begin
    Format.pp_print_flush fmt ();
    OpamConsole.msg "%s" (Buffer.contents buff);
    exit r
  end
