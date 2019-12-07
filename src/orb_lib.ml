(** Utils *)
let log ?num fmt =
  let orb =
    let orb = "[REBUILD]" in
    let col s = OpamConsole.(colorise `bold s |> colorise `cyan) in
    match num with
    | Some n -> col (Printf.sprintf "%s[%d]" orb n)
    | None -> col orb
  in
  OpamConsole.msg ("\n%s "^^fmt^^"\n\n") orb

let clean_switches = ref (fun () -> ())

let exit_error reason ?num fmt =
  !clean_switches ();
  let str = OpamStd.Option.to_string ~none:"" (Printf.sprintf "[%d] ") num in
  OpamConsole.error_and_exit reason ("%s"^^fmt) str

let drop_states ?gt ?rt ?st () =
  OpamStd.Option.iter OpamSwitchState.drop st;
  OpamStd.Option.iter OpamRepositoryState.drop rt;
  OpamStd.Option.iter OpamGlobalState.drop gt

let remove_switch switch =
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  OpamGlobalState.drop @@
  OpamSwitchCommand.remove ~confirm:false gt switch;
  log "Switch %s removed"
    (OpamSwitch.to_string switch |> OpamConsole.colorise `blue)

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

let add_env _switch epoch gt st =
  let env = OpamFile.Switch_config.env st.OpamStateTypes.switch_config in
  let source_date_epoch =
    "SOURCE_DATE_EPOCH", OpamParserTypes.Eq,
    Printf.sprintf "%d" (int_of_float epoch),
    Some "Reproducible builds date" ;
  in
  (*  let value = target ^ "=" ^ OpamSwitch.to_string switch in
      log "BPPM is %s!" (OpamConsole.colorise `green value);
      let prefix_map =
      "BUILD_PATH_PREFIX_MAP", OpamParserTypes.Eq,
      value, Some "Build path prefix map"
      in *)
  let to_add =
    List.fold_left (fun swc v ->
        let s,_,_,_ = v in
        match OpamStd.List.find_opt (fun (s',_,_,_) -> s = s') env with
        | Some _ -> swc
        | None -> v::swc) [] [ (* prefix_map ; *) source_date_epoch ]
  in
  log "adding to environment (time %d): %s"
    (int_of_float (Unix.time ()))
    (String.concat "\n" (List.map (fun (s, _, v, _) -> s ^"="^v) to_add));
  let switch_config =
      OpamFile.Switch_config.with_env (to_add @ env) st.switch_config
  in
  let st = { st with switch_config } in
  let switch = st.switch in
  OpamFile.Switch_config.write
    (OpamPath.Switch.switch_config gt.OpamStateTypes.root switch) st.switch_config;
  st

(** Steps *)
let import_switch epoch switch export =
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
  let gt, st =
    OpamSwitchCommand.install gt ~rt ~local_compiler:true
      ~packages:[] ~update_config:false switch
  in
  log "Switch %s created!"
    (OpamConsole.colorise `green (OpamSwitch.to_string switch));
  clean_switches := (fun () ->
      remove_switch switch;
      OpamFilename.rmdir
        (OpamFilename.Dir.of_string (OpamSwitch.to_string switch)));
  let st = add_env switch epoch gt st in
  try
    log "now importing switch";
    let st = OpamSwitchCommand.import st export in
    log "Switch %s imported!"
      (OpamConsole.colorise `green (OpamSwitch.to_string switch));
    drop_states ~gt ~rt ~st ()
  with e ->
    log "Switch %s import failed!"
      (OpamConsole.colorise `red (OpamSwitch.to_string switch));
    !clean_switches ();
    raise e

let install_switch epoch ?repos compiler_switch switch =
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
  let gt, st =
    OpamSwitchCommand.install gt ~rt ~local_compiler:true ?repos
      ~packages:[] ~update_config:false switch
  in
  let st = match compiler_switch with
    | None -> st
    | Some path ->
      let src = `Source (OpamUrl.parse ("git+file://" ^ path))
      and pkg = OpamPackage.Name.of_string "ocaml-variants"
      in
      let st = OpamClient.PIN.pin st pkg src in
      OpamSwitchCommand.set_compiler st [pkg, None]
  in
  let st = add_env switch epoch gt st in
  log "Switch %s created!"
    (OpamConsole.colorise `green (OpamSwitch.to_string switch));
  drop_states ~gt ~rt ~st ()

let update_switch_env epoch switch =
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  if not (OpamGlobalState.switch_exists gt switch) then
    (drop_states ~gt ();
     exit_error `Not_found "Switch %s doesn't exist"
       (OpamSwitch.to_string switch |> OpamConsole.colorise `underline))
  else
    OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
    OpamSwitchState.with_ `Lock_write ~rt ~switch gt @@ fun st ->
    let st = add_env switch epoch gt st in
    drop_states ~gt ~rt ~st ()

let install switch atoms_or_locals =
  log "Install start";
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
      (log "Remove previsouly installed packages: %s"
         (OpamPackage.Name.Set.to_string installed);
       OpamClient.remove st ~autoremove:false ~force:false atoms)
  in
  let st =
    log "Install deps of %s" (OpamFormula.string_of_atoms atoms);
    OpamClient.install st atoms ~deps_only:true
  in
  log "Install %s" (OpamFormula.string_of_atoms atoms);
  let st = OpamClient.install st atoms in
  drop_states ~gt ~rt ~st ()

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

let output_buildinfo dir map =
  let filename post =
    let rec fn n =
      let filename = post ^ "-" ^ string_of_int n in
      let file = Filename.concat dir filename in
      if Sys.file_exists file then fn (succ n) else filename
    in
    OpamFilename.(create (Dir.of_string dir) (Base.of_string (fn 0)))
  in
  OpamPackage.Map.iter (fun pkg map ->
      let value = OpamFile.Changes.write_to_string map in
      let fn = filename (OpamPackage.Name.to_string pkg.name ^ ".buildinfo") in
      log "writing %s" (OpamFilename.to_string fn);
      write_file fn value)
    map

let copy_build_dir dir switch =
  let rec bdir c =
    let dir' = dir ^ "-" ^ string_of_int c in
    if Sys.file_exists dir' then
      bdir (succ c)
    else
      dir'
  in
  let target = bdir 0 in
  log "preserving build dir in %s" target;
  OpamFilename.copy_dir
    ~src:(OpamFilename.Dir.of_string (OpamSwitch.to_string switch))
    ~dst:(OpamFilename.Dir.of_string target);
  target

