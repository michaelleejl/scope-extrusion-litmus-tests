open Effect
open Effect.Deep
open Runcode 
type _ Effect.t += Extrude: int code -> int code t

let () =  Trx.set_with_stack_mark
    {Trx.stackmark_region_fn =
        fun body ->
          let module M = struct type _ Effect.t += E: unit t end in
          try body (fun () -> try perform M.E; true with Unhandled _ -> false)
          with effect M.E, k -> continue k ()};;
          
let z  = run(match .<fun x -> .~(perform (Extrude .<x>.) ) >. 
             with u -> .<0>.
                | effect (Extrude y), k -> y) 
            in z + 1;;