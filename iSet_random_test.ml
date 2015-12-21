(* Copyright Artur "mrowqa" Jamro 2015*)
(* Copyright Marcin Mielniczuk 2015*)
(* Released under the MIT license *)
(* let's make this one-file checker... *)
(* You can modify 'n' and 'clear_steps' global parameters below.
 * All operations are performed on (0, n-1) interval.
 * After clear_steps steps, if this parameter is positive,
 * tested intervals are cleared.
 * If you have bug, please set n=10, clear_steps=10, verbose = true to make manual debugging
 * possible *)

module Integers =
    struct
        type t = int
        let compare = Pervasives.compare
    end

module S = Set.Make(Integers)

let loop f (a,b) s =
    let rec pom a b acc =
        if a = b then acc
        else pom (a+1) b (f a s)
    in pom a b s

let print_list l = List.iter (fun a -> Printf.printf "%d " a) l

module IntSet =
    struct
        include Set.Make(Integers)

        let add = loop S.add
        let remove = loop S.remove
    end

(*module PseudoSet =
    struct
        type val_ = {
            size: int;
            mutable array: bool array
        }
        type obj_ = {
            value: val_;
            add: int * int -> unit;
            remove: int * int -> unit;
            split: int -> bool array * bool * bool array;
            replace: bool array -> unit
        }

        let add_ {array = arr} (a, b) =
            for i = a to b do
                arr.(i) <- true
            done

        let remove_ {array = arr} (a, b) =
            for i = a to b do
                arr.(i) <- false
            done

        let split_ {array = arr; size = n} a =
            let l = Array.init n (fun i -> if i < a then arr.(i) else false)
            and r = Array.init n (fun i -> if i > a then arr.(i) else false)
            and b = arr.(a)
            in (l, b, r)

        let replace_ x a =
            x.array <- a

        let create n =
            let self = {
                size = n;
                array = Array.make n false
            } in {
                value = self;
                add = add_ self;
                remove = remove_ self;
                split = split_ self;
                replace = replace_ self
            }
    end;;*)

(* let's use global vars *)
let lo = 0
let hi = 20
let n = hi
let clear_step = 0;;
let intset = ref IntSet.empty;;
let iset = ref ISet.empty;;
let verbose = false

type testAction =
    | TestAdd
    | TestRemove
    | TestSplit

let sort (x, y) =
    if x < y then (x, y) else (y, x);;

let get_action () : testAction =
    let a = Random.int 4 in
    if a < 2 then
        TestAdd
    else if a = 2 then
        TestRemove
    else
        TestSplit

let test_add () : unit =
    let a, b = sort (Random.int n, Random.int n) in
    intset := IntSet.add (a, b) !intset;
    iset := ISet.add (a, b) !iset;
    Printf.printf "add (%d, %d)... " a b;;

let test_remove () : unit =
    let a, b = sort (Random.int n, Random.int n) in
    Printf.printf "remove (%d, %d)... " a b;
    intset := IntSet.remove (a, b) !intset;
    iset := ISet.remove (a, b) !iset

(*
let print_bool_array a : unit =
    for i = 0 to n-1 do
        if a.(i) then Printf.printf "%d " i
    done

let test_split () : unit =
    let a = Random.int n
    and side = Random.int 2 in
    let sidetxt = if side = 0 then "below" else "above" in
        Printf.printf "split %d, taking the ones %s... " a sidetxt;
        let b, c, d = PseudoSet.(pset.split a) in
        if verbose then
        begin
            print_newline(); print_string "below: "; print_bool_array b;
            print_string "// above: "; print_bool_array d
        end;
        let bb, cc, dd = ISet.split a !iset in
        let t = [| b; d |] and tt = [| bb; dd |] in
        assert (c = cc);
        PseudoSet.(pset.replace t.(side));
        iset := tt.(side)

*)


let interval_to_list (a,b) =
    loop (fun x l -> x::l) (a,b) []

let to_int_list ll =
    List.fold_left (fun acc el -> (interval_to_list el) @ acc) [] ll

let print_intset set =
    print_list (IntSet.elements set)

let print_iset () set =
    print_list (to_int_list (ISet.elements set))

let check_correctness () : unit =
    (*if verbose then
    begin
        print_newline ();
        print_string "PseudoSet: "; print_pset ();
        print_string "     iSet: "; print_iset ();
    end;*)
    let ints = IntSet.elements !intset in
    let i = to_int_list (ISet.elements !iset) in
    begin
        try assert (ints = i)
        with Assert_failure x ->
            failwith "bla"
    end;
    Printf.printf "- OK!\n"; flush stdout


let _ =
    Random.self_init ();
    Printf.printf "Starting.\n"; flush stdout;
    let i = ref 0 in
    while true do
      let () =
        if clear_step > 0 && !i mod clear_step = 0 then begin
            Printf.printf "[clear]\n";
            iset := ISet.empty;
            intset := IntSet.empty
        end;
        i := !i + 1;
        Printf.printf "%d. " !i;
        match get_action () with
            | TestAdd -> test_add ()
            | TestRemove -> test_remove ()
            | TestSplit -> () in
        check_correctness ()
    done

