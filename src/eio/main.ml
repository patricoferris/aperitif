open Eio
module Hash = Digestif.SHA1

let ( / ) = Path.( / )

let hash_of_blob filename =
  Path.with_open_in filename @@ fun ic ->
  let ln = File.size ic in
  let rec go buf ctx =
    match Flow.single_read ic buf with
    | 0 | (exception End_of_file) -> Hash.get ctx
    | len ->
        let ctx = Hash.feed_bigstring ctx buf.buffer ~len in
        go buf ctx
  in
  let ctx = Hash.empty in
  let str = Fmt.str "blob %a\000" Optint.Int63.pp ln in
  let ctx = Hash.feed_string ctx str in
  go (Cstruct.create 0x1000) ctx

let empty_tree = Hash.digest_string "tree 0\000"

let rec hash_of_tree filename =
  let entries = Path.read_dir filename in
  let entries =
    List.map
      (fun v ->
        let filename = filename / v in
        match Path.kind ~follow:true filename with
        | `Directory -> (`Dir, filename)
        | `Regular_file -> (`Normal, filename)
        | kind -> Fmt.failwith "Expected directory or file but got %a" File.pp_kind kind)
      (List.sort String.compare entries)
  in
  hash_of_entries entries

and perform = function
  | `Dir, filename ->
      let name = Filename.basename (Path.native_exn filename) in
      let hash = hash_of_tree filename in
      Fmt.str "40000 %s\000%s" name (Hash.to_raw_string hash)
  | `Normal, filename ->
      let name = Filename.basename (Path.native_exn filename) in
      let hash = hash_of_blob filename in
      Fmt.str "100644 %s\000%s" name (Hash.to_raw_string hash)

and hash_of_entries = function
  | [] -> empty_tree
  | es ->
      let entries = Fiber.List.map perform es in
      let ctx = Hash.empty in
      let len =
        List.fold_left (fun acc str -> acc + String.length str) 0 entries
      in
      let str = Fmt.str "tree %d\000" len in
      let ctx = Hash.feed_string ctx str in
      let ctx =
        List.fold_left (fun ctx str -> Hash.feed_string ctx str) ctx entries
      in
      Hash.get ctx

let run fn path =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let arg = fs / path in
  fn arg

let () =
  match Sys.argv with
  | [| _; filename |] when Sys.file_exists filename ->
      if Sys.is_directory filename then
        let hash = run hash_of_tree filename in
        Format.printf "%a\n%!" Hash.pp hash
      else
        let hash = run hash_of_blob filename in
        Format.printf "%a\n%!" Hash.pp hash
  | [| _; filename |] ->
      Format.eprintf "%s: %s not found\n%!" Sys.argv.(0) filename
  | _ -> Format.eprintf "%s <filename>\n%!" Sys.argv.(0)
