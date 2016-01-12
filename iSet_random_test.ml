(* Copyright Artur "mrowqa" Jamro 2015 *)
(* Copyright Marcin Mielniczuk 2015 *)
(* Released under the MIT license *)
(* almost rewritten from scratch, only used the codebase *)
(* You can modify 'n' and 'clear_steps' global parameters below. *)
(* After clear_steps steps, if this parameter is positive, *)
(* tested intervals are cleared. *)
(* If you have bug, please set debug = true to make manual debugging *)
(* possible *)

let debug = false
let verbose = false

module Integers =
    struct
        type t = int
        let compare = Pervasives.compare
    end

module S = Set.Make(Integers)

let loop f (a,b) s =
    let rec pom a b acc =
        if a = b then (f a acc)
        else pom (a+1) b (f a acc)
    in pom a b s

let print_list l = List.iter (fun a -> Printf.printf "%d " a) l

module IntSet =
    struct
        include Set.Make(Integers)
        let add = loop S.add
        let remove = loop S.remove
    end

(* let's use global vars *)
let lo = if debug then 0 else -100000
let hi = if debug then 20 else 100000
let range = hi - lo
let clear_step = if debug then 10 else 0
let intset = ref IntSet.empty
let iset = ref ISet.empty
let rnd l h = Random.int (h-l+1) + l
let random () = Random.int range + lo

type testAction =
    | TestAdd
    | TestRemove
    | TestSplit
    | TestBelow

let sort (x, y) =
    if x < y then (x, y) else (y, x)

let interval_to_list (a,b) =
    List.rev (loop (fun x l -> x::l) (a,b) [])

let to_int_list ll =
    List.fold_left (fun acc el -> List.rev_append (List.rev acc) (interval_to_list el)) [] ll

let print_intset set =
    print_list (IntSet.elements set)

let print_iset set =
    print_list (to_int_list (ISet.elements set))

let print_sets () =
    print_string "\nPseudoSet: "; print_intset !intset;
    print_string "\n     iSet: "; print_iset !iset

let bt () = print_sets(); print_newline()

let get_action () : testAction =
    let a = Random.int 20 in
    if a < 8 then
        TestAdd
    else if a < 12 then
        TestRemove
    else if a < 16 then
        TestSplit
    else
        TestBelow

let test_add () : unit =
    let a, b = (random (), rnd 2 10) in
    intset := IntSet.add (a, a+b) !intset;
    iset := ISet.add (a, a+b) !iset;
    Printf.printf "add (%d, %d)... " a (a+b);;

let test_remove () : unit =
    let a, b = (random (), rnd 5 20) in
    Printf.printf "remove (%d, %d)... " a (a+b);
    intset := IntSet.remove (a, a+b) !intset;
    iset := ISet.remove (a, a+b) !iset

let test_split () : unit =
    let a = random ()
    and side = Random.int 2 in
    let sidetxt = if side = 0 then "below" else "above" in
        Printf.printf "split %d, taking the ones %s... " a sidetxt;
        let b, c, d = IntSet.split a !intset in
        let bb, cc, dd = ISet.split a !iset in
        let t = [| b; d |] and tt = [| bb; dd |] in
        assert (c = cc);
        intset := t.(side);
        iset := tt.(side)

let test_below () : unit =
    let a = random () in
    Printf.printf "below %d... " a;
    let test = ISet.below a !iset
    and b, _, _ = IntSet.split (a+1) !intset in
    let c = S.cardinal b in
    try assert (test = c)
    with Assert_failure x ->
        Printf.printf "\nReturned %d, expected %d\n" test c;
        if debug then bt ();
        raise (Assert_failure(x))

let check_correctness () : unit =
    if verbose then print_sets ();
    let ints = IntSet.elements !intset in
    let i = to_int_list (ISet.elements !iset) in
    begin
        try assert (ints = i)
        with Assert_failure x ->
            if debug then bt ();
            raise (Assert_failure(x))
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
            | TestSplit -> test_split ()
            | TestBelow -> test_below ()
        in check_correctness ()
    done

