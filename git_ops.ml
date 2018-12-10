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
	"git --git-dir=" ^ store

let hash_object store input =
	let data = In_channel.input_all input in
	let (ic,oc) = Unix.open_process ((git_w_dir store) ^ " hash-object -w --stdin") in
	let _signal = Out_channel.output_string oc data in
	Out_channel.close oc; (* This is necessary to send EOF to proc *)
		let hash = GHash(In_channel.input_all ic) in
		let _signal2 = Unix.close_process (ic,oc) in
		hash

let cat_file store hash =
	Unix.open_process_in ((git_w_dir store) ^" cat-file -p " ^ (string_of_hash hash))

let mktree store tree =
	let (ic,oc) = Unix.open_process ((git_w_dir store) ^ " mktree") in
	let _signal = Out_channel.output_string oc (string_of_tree tree) in
	Out_channel.close oc;
		let hash = GHash(In_channel.input_all ic) in
		let _signal2 = Unix.close_process (ic,oc) in
		hash

let get_head_tree store =
	let ic = Unix.open_process_in ((git_w_dir store) ^ " ls-tree HEAD") in
	GTree (In_channel.input_all ic)

let add_to_tree (GTree tree) filename (GHash hash) =
	GTree (tree ^ "\n100644 " ^ filename ^ " " ^ hash)

let commit_tree store (GTree tree) =
	let (ic,oc) = Unix.open_process ((git_w_dir store) ^ " commit-tree -p `git rev-parse HEAD`" ^ tree) in
	let _signal = Unix.close_process (ic,oc) in
	()
