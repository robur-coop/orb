open Orb_lib

let rebuild global_options build_options build_info =
  match build_info with
  | None -> failwith "require build info directory"
  | Some dir ->
    OpamArg.apply_global_options global_options;
    OpamArg.apply_build_options build_options;
    OpamCoreConfig.update ~precise_tracking:true ~answer:(Some true) ();
    OpamStateConfig.update ~unlock_base:true ();
    let filename post =
      OpamFilename.(create (Dir.of_string dir) (Base.of_string post))
    in
    let env =
      let data = read_file (filename "env") in
      let lines = String.split_on_char '\n' data in
      List.fold_left (fun acc line ->
          match String.split_on_char '=' line with
          | [ key ; value ] -> (key, value) :: acc
          | _ ->
            OpamConsole.msg "bad environment %s" line;
            acc)
        [] lines
    in
    let sw = List.assoc "BUILD_PATH" env in
    let switch = OpamSwitch.of_string sw in
    let switch_in = OpamFile.make (OpamFilename.of_string (dir ^ "/repo.export")) in
    let epoch = float_of_string @@ List.assoc "SOURCE_DATE_EPOCH" env in
    import_switch epoch switch (Some switch_in);
    log "switch imported (installed stuff)";
    let buildinfos =
      let files = OpamSystem.files dir in
      List.filter (OpamStd.String.contains ~sub:".buildinfo") files
    in
    log "buildinfos for %s" (String.concat ", " buildinfos);
    let packages =
      List.fold_left (fun acc bi ->
          match OpamStd.String.rcut_at bi '/' with
          | None -> log "ignoring %s" bi ; acc
          | Some (_, data) ->
            match OpamStd.String.cut_at data '.' with
            | None -> log "ignoring2 %s" bi ; acc
            | Some (data, _) -> OpamStd.String.Set.add data acc)
        OpamStd.String.Set.empty buildinfos
    in
    log "packages %s" (String.concat ", " (OpamStd.String.Set.elements packages));
    let atoms_or_locals =
      List.map (fun a -> `Atom (OpamPackage.Name.of_string a, None))
        (OpamStd.String.Set.elements packages)
    in
(*    (try install switch atoms_or_locals
     with (OpamStd.Sys.Exit _) as e -> !clean_switches (); raise e);
      log "installed package(s)"; *)
    let tracking_map = tracking_maps switch atoms_or_locals in
    log "got tracking maps";
    log "%s" (OpamConsole.colorise `green "BUILD INFO");
    output_buildinfo dir tracking_map;
    let _ = copy_build_dir dir switch in
    !clean_switches ()

open Cmdliner

(** CLI *)
let rebuild_cmd =
  let open OpamArg in
  let doc = "Rebuild an opam repository" in
  let man = [
    `S "DESCRIPTION";
    `P "$(b,orb) is a tool to rebuild an opam package with repository \
        information. The resulting install files are compared with the \
        ones provided. If the package is reproducible, there's no diff.";
    `S "ARGUMENTS";
    `S "OPTIONS";
    `S OpamArg.build_option_section;
  ]
  in
  let build_info =
    mk_opt [ "build-info" ] "[DIR]"
      "use the build-info from [DIR]"
      Arg.(some string) None
  in
  Term.((const rebuild $ global_options $ build_options $ build_info)),
  Term.info "rebuild" ~man ~doc

let () =
  let buff = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buff in
  match Term.eval ~err:fmt ~catch:true rebuild_cmd with
  | `Error (`Term | `Parse) ->
    Format.pp_print_flush fmt ();
    OpamConsole.msg "%s" (Buffer.contents buff)
  | _ -> ()
