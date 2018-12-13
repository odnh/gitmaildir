open Core

type git_store = GStore of string
type git_tree = GTree of string
type git_hash = GHash of string

let store_of_string str = GStore str
let tree_of_string str = GTree str
let hash_of_string str = GHash str
let string_of_tree (GTree str) = str
let string_of_hash (GHash str) = str

let git_w_dir (GStore store) =
	"git --git-dir=\"" ^ store ^ "\" "

let get_head_hash store =
  let ic = Unix.open_process_in ((git_w_dir store) ^ "rev-parse HEAD") in
  let hash = GHash(In_channel.input_line_exn ic) in
  let _ = Unix.close_process_in ic in
  hash


let hash_object store input =
	let data = In_channel.input_all input in
	let ic, oc = Unix.open_process ((git_w_dir store) ^ "hash-object -w --stdin") in
	let _ = Out_channel.output_string oc data in
	Out_channel.close oc; (* This is necessary to send EOF to proc *)
	let hash = GHash(In_channel.input_line_exn ic) in
	let _ = Unix.close_process (ic, oc) in
  hash

let cat_file store hash =
	Unix.open_process_in ((git_w_dir store) ^ "cat-file -p " ^ (string_of_hash hash))

let mktree store tree =
	let ic, oc = Unix.open_process ((git_w_dir store) ^ "mktree") in
	let _ = Out_channel.output_string oc (string_of_tree tree) in
	Out_channel.close oc;
	let hash = GHash(In_channel.input_line_exn ic) in
	let _ = Unix.close_process (ic,oc) in
	hash

let get_head_tree store =
  let ic = Unix.open_process_in ((git_w_dir store) ^ " ls-tree HEAD") in
	  GTree (In_channel.input_all ic |> Stdlib.String.trim)

let add_to_tree (GTree tree) filename (GHash hash) =
  GTree (tree ^ "\n100644 blob " ^ hash ^ "\t" ^ filename)

let commit_tree store (GHash hash) =
  let GHash(head_hash) = get_head_hash store in
	let ic, oc = Unix.open_process ((git_w_dir store)
                                   ^ "commit-tree "
                                   ^ hash
                                   ^ " -p " ^ head_hash) in
  let _ = Out_channel.output_string oc "Deliver new email" in
  Out_channel.close oc;
  let commit = GHash(In_channel.input_line_exn ic) in
	let _ = Unix.close_process (ic, oc) in
	commit

let update_head store (GHash hash) =
  let ic = Unix.open_process_in ((git_w_dir store) ^ "update-ref refs/heads/master " ^ hash) in
  let _ = Unix.close_process_in ic in
  ()

let remove_from_tree (GTree tree) (GHash hash) =
  let contains h l = match List.nth (String.split ~on:' ' l) 2 with
    | None -> true
    | Some s -> s <> h in
  tree |> String.split_lines |> List.filter ~f:(contains hash) |> String.concat ~sep:"\n" 
    |> (fun x -> GTree x)

let hash_from_filename store name =
  let GTree(tree) = get_head_tree store in
  let rec get_line = function
    | [] -> None
    | x::xs -> match List.nth (String.split ~on:'\t' x) 1 with
        | None -> get_line xs
        | Some s -> if s = name then Some s else get_line xs in
  let get_hash = function
    | None -> None
    | Some line -> match String.split ~on:' ' line with
        | _::_::x3::_ -> Some (GHash x3)
        | _ -> None in
  String.split_lines tree |> get_line |> get_hash
