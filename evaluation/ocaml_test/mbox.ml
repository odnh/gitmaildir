open Core

let get_uid () =
  (string_of_int (int_of_float ((Unix.gettimeofday ()) *. 1_000_000.)))
  ^ "."
  ^ (string_of_int (Random.bits ()))
  ^ "."
  ^ (Unix.gethostname ())

let get_line_coords ic contents =
  let calc_pos () =
    let end_pos = In_channel.pos ic in
    let start_pos = Int64.(end_pos - (of_int @@ String.length contents) - (of_int 1)) in
    (start_pos, end_pos) in
  let rec aux () =
    match In_channel.input_line ic with
    | Some line ->
        if line = contents then Some (calc_pos ()) else aux ()
    | None -> None in
  aux ()

let get_mail_coords ic start_line =
  let rec get_next () =
    match In_channel.input_line ic with
    | Some line ->
        if try String.sub ~pos:0 ~len:6 line = "FROM :" with Invalid_argument _ -> false then In_channel.pos ic
        else get_next ()
    | None -> In_channel.pos ic in
  let rec aux () =
    match In_channel.input_line ic with
    | Some line ->
        if line = start_line then
          let start_coord = In_channel.pos ic in
          let end_coord = get_next () in
          Some (start_coord, end_coord)
        else aux ()
    | None -> None in
  aux ()

let deliver input store =
  let file = Unix.openfile ~mode:[O_WRONLY; O_APPEND] store in
  Flock.flock file Flock.LOCK_EX;
  let data = In_channel.input_all input in
  let delimiter = "FROM: " ^ (get_uid ()) ^ "\n" in
  let _ = Unix.write file ~buf:(Bytes.of_string delimiter) in
  let _ = Unix.write file ~buf:(Bytes.of_string data) in
  Flock.flock file Flock.LOCK_UN;
  Unix.close file

let move store old_mail new_mail =
  let file = Unix.openfile ~mode:[O_RDWR] store in
  Flock.flock file Flock.LOCK_EX;
  let ic = Unix.in_channel_of_descr file in
  let oc = Unix.out_channel_of_descr file in
  let delimiter = "FROM: " ^ old_mail in
  (match get_line_coords ic delimiter with
  | Some (start_pos, _) ->
      (Out_channel.seek oc start_pos;
      Out_channel.output_string oc ("FROM: " ^ new_mail);
      Out_channel.flush oc)
  | None -> failwith "No such mail");
  Flock.flock file Flock.LOCK_UN;
  Unix.close file

let delete store mail =
  let file = Unix.openfile ~mode:[O_RDWR] store in
  Flock.flock file Flock.LOCK_EX;
  let ic = Unix.in_channel_of_descr file in
  let oc = Unix.out_channel_of_descr file in
  let delimiter = "FROM: " ^ mail in
  (match get_mail_coords ic delimiter with
  | Some (start_pos, end_pos) ->
      let () = In_channel.seek ic end_pos in
      let () = Out_channel.seek oc start_pos in
      let lines = In_channel.input_lines ic in
      List.iter lines ~f:(fun line -> Out_channel.output_string oc line);
      Out_channel.flush oc
  | None -> failwith "No such mail");
  Flock.flock file Flock.LOCK_UN;
  Unix.close file

