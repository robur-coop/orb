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
let dot_diffoscope = ".diffoscope"

let custom_env_keys = [ "OS" ; "OS_DISTRIBUTION" ; "OS_VERSION" ; "OS_FAMILY" ; "SWITCH_PATH" ]

let custom_env s = [
  "OS", OpamSysPoll.os ();
  "OS_DISTRIBUTION", OpamSysPoll.os_distribution ();
  "OS_VERSION", OpamSysPoll.os_version ();
  "OS_FAMILY", OpamSysPoll.os_family ();
  "SWITCH_PATH", s
]

let create_env (s : string) =
  Array.to_list (Unix.environment () ) @
  List.fold_left
    (fun acc (k, v) -> match v with None -> acc | Some v -> (k^"="^v)::acc)
    [] (custom_env (Some s))

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

let dump_system_packages filename =
  match OpamSysPoll.os_family () with
  | Some "bsd" ->
    begin match OpamSysPoll.os_distribution () with
      | Some "freebsd" ->
        let r = Sys.command ("/usr/sbin/pkg query %n-%v > " ^ filename) in
        if r <> 0 then log "failed to dump system packages (exit %d)" r
      | Some distrib ->
        log "unsupported OS for host system packages (bsd %s)" distrib
      | None ->
        log "unsupported OS (no system packages), bsd without distribution"
    end
  | Some "debian" ->
    let r = Sys.command ("dpkg-query --showformat='${Package}=${Version}\n' -W > " ^ filename) in
    if r <> 0 then log "failed to dump system packages (exit %d)" r
  | Some family ->
    log "unsupported OS for host system packages %s" family
  | None ->
    log "unsupported OS (no system packages), no family"

let restore_system_packages filename =
  match OpamSysPoll.os_family () with
  | Some "bsd" ->
    begin match OpamSysPoll.os_distribution () with
      | Some "freebsd" ->
        let r = Sys.command ("cat " ^ filename ^ " | xargs /usr/sbin/pkg install -y") in
        if r <> 0 then log "couldn't install packages"
      | Some distrib ->
        log "unsupported OS for host system packages (bsd %s)" distrib
      | None ->
        log "unsupported OS (no system packages), bsd without distribution"
    end
  | Some "debian" ->
    let r = Sys.command ("cat " ^ filename ^ " | xargs apt-get install -y") in
    if r <> 0 then log "couldn't install packages"
  | Some family ->
    log "unsupported OS for host system packages %s" family
  | None ->
    log "unsupported OS (no system packages), no family"

let remove_switch switch =
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  OpamGlobalState.drop @@
  OpamSwitchCommand.remove ~confirm:false gt switch;
  log "Switch %s removed"
    (OpamSwitch.to_string switch |> OpamConsole.colorise `blue)

let output_system_packages_and_env ~skip_system dir env =
  let prefix = OpamFilename.Dir.of_string dir in
  (* output metadata: hashes, environment, system packages *)
  let filename file =
    OpamFilename.(create prefix (Base.of_string file))
  in
  if not skip_system then begin
    let pkgs = filename dot_packages in
    dump_system_packages (OpamFilename.to_string pkgs)
  end;
  let fn = filename dot_env in
  write_file fn (env_to_string env)

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
    output_system_packages_and_env ~skip_system dir (create_env sw);
    export switch dir;
    remove_switch switch;
    OpamFilename.rmdir
      (OpamFilename.Dir.of_string (OpamSwitch.to_string switch));
    clean_switch := None

let exit_error reason fmt =
  cleanup ();
  OpamConsole.error_and_exit reason fmt

let convert_date x = string_of_int (int_of_float x)

let env_matches env =
  (* TODO may relax e.g. OS_VERSION is DISTRIBUTION is detailed enough *)
  let opt_compare key v =
    match v, List.assoc_opt key env with
    | None, _ -> log "key %s not available" key ; true
    | _, None -> log "key %s not found in environment" key ; true
    | Some v, Some v' -> String.equal v v'
  in
  List.for_all (fun (k, v) -> opt_compare k v) (custom_env None)

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
  log "SOURCE_DATE_EPOCH is %s" (Unix.getenv "SOURCE_DATE_EPOCH");
  log "now importing switch";
  let st = OpamSwitchCommand.import st export in
  log "Switch %s imported!"
    (OpamConsole.colorise `green (OpamSwitch.to_string switch));
  let roots = st.installed_roots in
  drop_states ~gt ~rt ~st ();
  roots

let install_switch ?repos compiler_pin compiler_version switch =
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
  let (), st =
    OpamSwitchCommand.create gt ~rt ?repos ~update_config:false
      ~invariant:OpamFormula.Empty switch (fun st -> (), st)
  in
  log "Switch %s created!"
    (OpamConsole.colorise `green (OpamSwitch.to_string switch));
  let st = match compiler_pin, compiler_version with
    | None, None -> st
    | Some path, None ->
      let src = `Source (OpamUrl.parse ("git+file://" ^ path)) in
      let pkg = OpamPackage.Name.of_string "ocaml-variants" in
      let st = OpamClient.PIN.pin ~action:false st pkg src in
      log "Pinned compiler to %s" path;
      st
    | None, Some v ->
      let version = OpamPackage.Version.of_string v in
      let pkg =
        OpamPackage.Name.of_string
          (if OpamStd.String.contains_char v '+' then "ocaml-variants" else "ocaml")
      in
      let st = OpamClient.PIN.pin ~action:false st pkg (`Version version) in
      log "Pinned compiler to %s.%s" (OpamPackage.Version.to_string version)
        (OpamPackage.Version.to_string version);
      st
    | Some _, Some _ ->
      exit_error `Bad_arguments
        "Both compiler-pin and compiler provided, choose one.";
  in
  drop_states ~gt ~rt ~st ()

let install switch atoms_or_locals =
  log "Install start";
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
  OpamSwitchState.with_ `Lock_write ~rt ~switch gt @@ fun st ->
  let st, atoms =
    OpamAuxCommands.autopin st ~simulate:true atoms_or_locals
  in
  log "Install %s" (OpamFormula.string_of_atoms atoms);
  try
    let st = OpamClient.install st atoms in
    log "Installed %s" (OpamFormula.string_of_atoms atoms);
    drop_states ~gt ~rt ~st ()
  with
  | OpamStd.Sys.Exit n ->
    log "Installation failed with %d" n;
    exit n
  | e ->
    log "Exception while installing: %s" (Printexc.to_string e);
    exit 1

let tracking_maps switch atoms_or_locals =
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_none ~switch gt @@ fun st ->
  log "tracking map got locks";
  let st, packages =
    let p =
      let _, atoms = OpamAuxCommands.resolve_locals atoms_or_locals in
      log "tracking map %d atoms (package set %d - %d packages)"
        (List.length atoms)
        (OpamPackage.Set.cardinal st.installed)
        (OpamPackage.Set.cardinal st.packages);
      let packages =  OpamFormula.packages_of_atoms st.installed atoms in
      log "tracking map %d packages" (OpamPackage.Set.cardinal packages);
      let ifer = OpamPackage.Set.inter st.installed packages in
      log "tracking map %d packages later" (OpamPackage.Set.cardinal ifer);
      ifer
    in
    st, p
  in
  log "tracking map got st and %d packages (%d atoms_or_locals)"
    (OpamPackage.Set.cardinal packages) (List.length atoms_or_locals);
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
  log "tracking map got tr, dropping states";
  drop_states ~gt ~st ();
  tr

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
let diff_map track1 track2 =
  OpamPackage.Map.fold (fun pkg sm1 map ->
      log "diff map folding %s" (OpamPackage.Name.to_string pkg.OpamPackage.name);
      let file_map =
        match OpamPackage.Map.find_opt pkg track2 with
        | Some sm2 ->
          let diff, rest =
            OpamStd.String.Map.fold (fun file d1 (diff, sm2) ->
                match OpamStd.String.Map.find_opt file sm2 with
                | Some d2 ->
                  log "diff map for %s found" file;
                  let diff =
                    if d1 <> d2 then begin
                      log "d1 and d2 mismatch";
                      OpamStd.String.Map.add file (Both (d1,d2)) diff
                    end else diff
                  in
                  diff,
                  OpamStd.String.Map.remove file sm2
                | None ->
                  log "diff map for %s not found" file;
                  OpamStd.String.Map.add file (First d1) diff, sm2)
              sm1 (OpamStd.String.Map.empty, sm2)
          in
          if OpamStd.String.Map.is_empty rest then begin
            log "diff map rest empty";
            diff
          end else begin
            log "diff map union";
            OpamStd.String.Map.union (fun _ _ -> assert false) diff
              (OpamStd.String.Map.map (fun d2 -> Second d2) rest)
          end
        | None ->
          log "diff map no file map in track2";
          OpamStd.String.Map.empty
      in
      if OpamStd.String.Map.is_empty file_map then map
      else OpamPackage.Map.add pkg file_map map
    ) track1 OpamPackage.Map.empty


let output_artifacts sw_prefix dir map =
  let maps =
    OpamPackage.Map.fold (fun pkg map acc ->
        let value = OpamFile.Changes.write_to_string map in
        let fn = OpamPackage.Name.to_string pkg.name ^ dot_hash in
        (fn, value) :: acc)
      map []
  in
  let prefix = OpamFilename.Dir.of_string dir in
  (* copy artifacts *)
  OpamPackage.Map.iter (fun _ map ->
      OpamStd.String.Map.iter (fun name -> function
          | OpamDirTrack.Added _ ->
            let src = Format.sprintf "%s/_opam/%s" sw_prefix name
            and tgt = Format.sprintf "%s/%s" dir name
            in
            let r = Sys.command ("mkdir -p " ^ Filename.dirname tgt) in
            if r <> 0 then log "failed to create directory for %s" tgt
            else begin
              let r = Sys.command ("cp " ^ src ^ " " ^ tgt) in
              if r <> 0 then log "failed to copy %s to %s" src tgt
            end
          | _ -> ()) map)
    map;
  (* output metadata: hashes, environment, system packages *)
  let filename file =
    OpamFilename.(create prefix (Base.of_string file))
  in
  List.iter (fun (n, v) ->
      let fn = filename n in
      log "writing %s" (OpamFilename.to_string fn);
      write_file fn v) maps

let install_system_packages dir =
  let filename = Printf.sprintf "%s/%s" dir dot_packages in
  restore_system_packages filename

let find_build_dir dir =
  let dir = Printf.sprintf "%s/%s" dir dot_build in
  if Sys.file_exists dir then Some dir else None

let copy_build_dir prefix switch =
  let target = Printf.sprintf "%s/%s" prefix dot_build in
  log "preserving build dir in %s" target;
  OpamFilename.copy_dir
    ~src:(OpamFilename.Dir.of_string (OpamSwitch.to_string switch))
    ~dst:(OpamFilename.Dir.of_string target);
  target

let generate_diffs root1 root2 final_map dir =
  let dir = OpamFilename.Dir.of_string dir
  and root1 = OpamFilename.Dir.of_string (root1 ^ "/_opam")
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

let common_start global_options disable_sandboxing build_options diffoscope =
  if diffoscope && OpamSystem.resolve_command "diffoscope" = None then
    exit_error `Not_found "diffoscope not found";
  (* all environment variables need to be set/unset before the following line,
     which forces the lazy Unix.environment in OpamStd *)
  OpamArg.apply_global_options cli global_options;
  OpamArg.apply_build_options cli build_options;
  let root = OpamStateConfig.(!r.root_dir) in
  let config_f = OpamPath.config root in
  let already_init = OpamFile.exists config_f in
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
  end;
  OpamCoreConfig.update ~precise_tracking:true ~yes:(Some true) ~confirm_level:`unsafe_yes ();
  OpamStd.Sys.at_exit cleanup

let compare_builds tracking_map tracking_map' dir build1st build2nd diffoscope =
  let final_map = diff_map tracking_map tracking_map' in
  if OpamPackage.Map.is_empty final_map then
    log "%s" (OpamConsole.colorise `green "It is reproducible!!!")
  else
    (log "There are some %s\n%s"
       (OpamConsole.colorise `red "mismatching hashes")
       (OpamPackage.Map.to_string
          (OpamStd.String.Map.to_string string_of_diff) final_map);
     if diffoscope then
       match build1st with
       | None -> log "no previous build dir available, couldn't run diffoscope"
       | Some build1 -> generate_diffs build1 build2nd final_map dir)

let project_name_from_arg = function
  | `Atom (name, _) :: _ -> OpamPackage.Name.to_string name
  | `Dirname dir :: _ -> OpamFilename.Dir.to_string dir
  | `Filename file :: _ -> OpamFilename.to_string file
  | _ -> invalid_arg "empty atoms_or_locals"

let project_name_from_dir dir = (* the first <name>.opam-switch *)
  match
    List.find_opt (fun t -> OpamFilename.check_suffix t dot_switch)
      (OpamFilename.files (OpamFilename.Dir.of_string dir))
  with
  | None -> failwith "couldn't find switch"
  | Some t -> OpamFilename.(Base.to_string (basename (chop_extension t)))

let find_env dir =
  let env_file = Filename.concat dir dot_env in
  let env = env_of_string (read_file (OpamFilename.of_string env_file)) in
  (* need to set HOME and PATH and SOURCE_DATE_EPOCH, since this env is captured by opam *)
  (if env_matches env then
     log "environment matches, using it"
   else
     failwith "environment does not match");
  let started =
    let full = Unix.readlink dir in
    let xs = String.split_on_char '/' full in
    match List.rev xs with
    | date :: _ -> date
    | _ -> invalid_arg "couldn't determine name and date"
  in
  started, env

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

let rebuild ~skip_system ~sw ~bidir ~keep_build out =
  let started = Unix.time () in
  let switch = OpamSwitch.of_string sw in
  let switch_in = switch_filename bidir in
  let packages = import_switch skip_system bidir sw switch (Some switch_in) in
  let atoms_or_locals =
    List.map (fun a -> `Atom (a.OpamPackage.name, None))
      (OpamPackage.Set.elements packages)
  in
  let tracking_map = tracking_maps switch atoms_or_locals in
  output_artifacts sw out tracking_map;
  let build2nd = if keep_build then copy_build_dir out switch else sw in
  tracking_map, build2nd, started, packages

let add_repo s (name, url) =
  (* todo trust anchors *)
  let url = OpamUrl.parse url in
  OpamRepositoryCommand.add s name url None

let add_repos repos =
  let repos = match repos with
    | None | Some [] ->
      [ OpamRepositoryName.of_string "default", "https://opam.ocaml.org" ]
    | Some xs ->
      List.map (fun s -> match String.split_on_char ':' s with
          | name :: rest ->
            OpamRepositoryName.of_string name, String.concat ":" rest
          | _ -> failwith "unknown repo") xs
  in
  let names = List.map fst repos in
  OpamGlobalState.with_ `Lock_none (fun gt ->
      OpamRepositoryState.with_ `Lock_write gt (fun rt ->
          let rt = List.fold_left add_repo rt repos in
          let failed, _rt = OpamRepositoryCommand.update_with_auto_upgrade rt names in
          List.iter
            (fun rn -> log "repo update failed for %s"
                (OpamRepositoryName.to_string rn))
            failed
        ));
  names

let drop_slash s =
  let l = String.length s in
  if l > 0 && String.get s (l - 1) = '/' then
    String.sub s 0 (l - 1)
  else
    s

(* Main function *)
let build global_options disable_sandboxing build_options diffoscope keep_build twice compiler_pin compiler
    repos out_dir switch_name epoch skip_system solver_timeout atoms_or_locals =
  let started = Unix.time () in
  if atoms_or_locals = [] then
    exit_error `Bad_arguments
      "I can't check reproductibility of nothing, by definition, it is";
  strip_env ~preserve:["HOME";"PATH"] ();
  Unix.putenv "PATH" (strip_path ());
  Unix.putenv "SOURCE_DATE_EPOCH"
    (convert_date (match epoch with
      | Some x -> float_of_string x
      | None -> Unix.time ()));
  (match solver_timeout with None -> () | Some x -> Unix.putenv "OPAMSOLVERTIMEOUT" x);
  Unix.putenv "OPAMERRLOGLEN" "0";
  let name = project_name_from_arg atoms_or_locals in
  Unix.putenv "ORB_BUILDING_PACKAGE" name;
  let tmp_dir = match switch_name with
    | None -> OpamSystem.mk_temp_dir ~prefix:("bi-" ^ name) ()
    | Some x -> drop_slash x
  in
  let bidir = match out_dir with None -> tmp_dir | Some dir -> drop_slash dir in
  let sw = tmp_dir ^ "/build" in
  let switch = OpamSwitch.of_string sw in
  let prefix = sw ^ "/_opam" in
  Unix.putenv "PREFIX" prefix;
  Unix.putenv "PKG_CONFIG_PATH" (prefix ^ "/lib/pkgconfig");
  common_start global_options disable_sandboxing build_options diffoscope;
  (match compiler_pin, compiler with
   | None, None -> OpamStateConfig.update ~unlock_base:true ();
   | _ -> ());
  if not keep_build then clean_switch := Some (switch, skip_system, bidir, sw);
  let repos = add_repos repos in
  install_switch ~repos compiler_pin compiler switch;
  install switch atoms_or_locals;
  let tracking_map = tracking_maps switch atoms_or_locals in
  output_artifacts sw bidir tracking_map;
  let build1st =
    if keep_build
    then Some (copy_build_dir bidir switch)
    else None
  in
  if keep_build then begin
    output_system_packages_and_env ~skip_system bidir (create_env sw);
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
    print_endline ("second run, outdir is " ^ outdir);
    let tracking_map', build2nd, started', _ =
      rebuild ~skip_system ~sw ~bidir ~keep_build outdir
    in
    let dir = Printf.sprintf "%s/%s-%s%s" bidir (ts started) (ts started') dot_diffoscope in
    compare_builds tracking_map tracking_map' dir build1st build2nd diffoscope
  end

let rebuild global_options disable_sandboxing build_options diffoscope keep_build skip_system dir out_dir =
  if dir = "" then failwith "require build info directory" else begin
    strip_env ();
    let out = match out_dir with None -> "." | Some dir -> drop_slash dir in
    let old = drop_slash dir in
    let old_started, env = find_env old in
    List.iter (fun (k, v) -> if List.mem k custom_env_keys then () else Unix.putenv k v) env;
    if not skip_system then install_system_packages old;
    common_start global_options disable_sandboxing build_options diffoscope;
    OpamStateConfig.update ~unlock_base:true ();
    let sw = List.assoc "SWITCH_PATH" env in
    (* for orb < 20211104 backwards compatibility *)
    if List.assoc_opt "PREFIX" env = None then
      Unix.putenv "PREFIX" (sw ^ "/_opam");
    if List.assoc_opt "PKG_CONFIG_PATH" env = None then
      Unix.putenv "PKG_CONFIG_PATH" (sw ^ "/_opam/lib/pkgconfig");
    let tracking_map_2nd, build2nd, started', packages =
      rebuild ~skip_system ~sw ~bidir:dir ~keep_build out
    in
    let tracking_map_1st = OpamPackage.Set.fold (fun pkg acc ->
        match read_tracking_map old (OpamPackage.Name.to_string pkg.OpamPackage.name) with
        | None -> log "no tracking map for %s found" (OpamPackage.Name.to_string pkg.OpamPackage.name); acc
        | Some tm -> OpamPackage.Map.add pkg tm acc)
        packages OpamPackage.Map.empty
    in
    let build1st = find_build_dir old in
    log "comparing with old build dir %s" (match build1st with None -> "no" | Some x -> x);
    let dir = Printf.sprintf "%s/%s-%s%s" dir (ts started') old_started dot_diffoscope in
    compare_builds tracking_map_1st tracking_map_2nd dir build1st build2nd diffoscope
  end

(** CLI *)
open Cmdliner
open OpamArg

let validity = cli_from cli2_1

let mk_flag a b = mk_flag ~cli validity a b
let mk_opt a b c d e = mk_opt ~cli validity a b c d e

let diffoscope = mk_flag ["diffoscope"] "use diffoscope to generate a report"

let keep_build =
  mk_flag ["keep-build"] "keep built temporary products"

let skip_system =
  mk_flag [ "skip-system" ] "skip system packages"

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
  let compiler_pin =
    mk_opt [ "compiler-pin" ] "[DIR]"
      "use the compiler in [DIR] for the temporary switch"
      Arg.(some string) None
  in
  let compiler =
    mk_opt [ "compiler" ] "[VERSION]"
      "use the compiler [VERSION] for the temporary switch"
      Arg.(some string) None
  in
  let repos =
    mk_opt [ "repos" ] "REPOS"
      "Include only packages that took their origin from one of the given \
       repositories."
      Arg.(some & list & string) None
  in
  let out_dir =
    mk_opt [ "out" ] "[DIR]"
      "Use [DIR] as build info prefix (defaults to a temporary directory)."
      Arg.(some string) None
  in
  let switch_name =
    mk_opt [ "switch-name" ] "[DIR]"
      "use [DIR] as switch name (defaults to a temporary directory)"
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
  let disable_sandboxing =
    mk_flag ["disable-sandboxing"]
      "Use a default configuration with sandboxing disabled. Use this at your \
      own risk, without sandboxing it is possible for a broken package script \
      to delete all your files."
  in
  let term =
    Term.((const build $ global_options cli $ disable_sandboxing $ build_options cli
           $ diffoscope $ keep_build $ twice $ compiler_pin $ compiler
           $ repos $ out_dir $ switch_name $ source_date_epoch $ skip_system
           $ solver_timeout $ atom_or_local_list))
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
        against the previous build. If the build directory is kept, diffoscope \
        can be used to make the differences human readable.";
    `S "ARGUMENTS";
    `S "OPTIONS";
  ] @ OpamArg.man_build_option_section
  in
  let build_info =
    let doc = "use build information in the provided directory" in
    Arg.(value & pos 0 string "" & info [] ~doc ~docv:"DIR")
  in
  let out_dir =
    mk_opt [ "out" ] "[DIR]"
      "Use [DIR] as build info prefix (defaults to a temporary directory)."
      Arg.(some string) None
  in
  let disable_sandboxing =
    mk_flag ["disable-sandboxing"]
      "Use a default configuration with sandboxing disabled. Use this at your \
      own risk, without sandboxing it is possible for a broken package script \
      to delete all your files."
  in
  let term =
    Term.((const rebuild $ global_options cli $ disable_sandboxing $ build_options cli $ diffoscope
           $ keep_build $ skip_system $ build_info $ out_dir))
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
