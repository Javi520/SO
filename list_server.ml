(*iwnbb*)

(*compile me with ocamlc "unix.cma" whatevernameihave.ml -o whatevernameyouwantsirofme*)

include Unix;;
include Buffer;;

(*ADN(size,key,adress,name,time)*)
module Ipc_fifo_oimp = struct

	type alloc_data_node = float*float*float*string*string;;(*should be float of the problem of int being 32 bits in C but 31 in Caml*V.2 moreover the address that can be a unsigned long*)

	type anom_struct =
			ADN of alloc_data_node
		| Empty(*for making easier to detect non-found elements*)
	;;

	let print_str_uct (elemento:anom_struct) f where_to_print = match elemento with
			ADN(a,b,c,d,e) -> let aux = String.concat "" [(string_of_float a);" "; (string_of_float b);" "; (string_of_float c);" "; d;" "; e]
			in write where_to_print (aux) 0 (String.length aux)
	;;
	
	type tridente =
			Trid of anom_struct list ref * anom_struct list ref * anom_struct list ref
	;;

	(* #PRIMER_INTENTO
	let read_str_uct where_to_get_from = 
		let aux_def = String.create 50 
		in match read where_to_get_from aux_def 0 50 with
			0 -> failwith "not logical"
		  | (1 as m_l) | (2 as m_l) | (3 as m_l) | (5 as m_l) -> Empty,float_of_int m_l
		  | (7 as i) | (8 as i) | (9 as i) -> let auxiliar = String.create 100 in 
		  	read where_to_get_from auxiliar 0 100;ADN(
				  float_of_string (String.sub auxiliar 0 10),
				  float_of_string (String.sub auxiliar 10 20),
				  float_of_string (String.sub auxiliar 20 30),
				  String.sub auxiliar 30 50,
				  String.sub auxiliar 50 80
			  ),i
		  | (10 as i) | (11 as i) | (12 as i) -> let auxiliar = String.create 100 in 
		  	read where_to_get_from auxiliar 0 100;ADN(
				  float_of_string (String.sub auxiliar 0 10),
				  float_of_string (String.sub auxiliar 10 20),
				  float_of_string (String.sub auxiliar 20 30),
				  String.sub auxiliar 30 50,
				  String.sub auxiliar 50 80
			  ),i
		  | _ -> failwith "go to the merd";;
	*)

	(*	#SEGUNDO_INTENTO
	let read_str_uct where_to_get_from = 
		let buffer_for_all = String.create 500
		in (read where_to_get_from buffer_for_all 0 500;
		let rec aux str local_index str_aux = 
		try function
			0 -> (match String.sub str 0 2 with
					"-m" -> raise (Failure "mostrar_lista1")
				  | "-n" -> raise (Failure "mostrar_lista2")
				  | "-z" -> raise (Failure "mostrar_lista3")
				  | "-x" -> raise (Failure "mostrar_lista5")
				  | "-i" -> raise (Failure "insertar")
				  | "-b" -> raise (Failure "buscar_por_size")
				  | "-s" -> raise (Failure "buscar_por_address")
				  | "*" -> raise (Failure "Error 001: wrong format")
				 )
		  | 1 -> match String.get str local_index  with
					' ' -> str_aux
				  | coc -> add_char str_aux coc;aux str (local_index+1) (str_aux) 1
		  | i -> match String.get str local_index  with
					' ' -> clear str_aux;aux str (local_index+1) (str_aux) (i-1)
				  | coc -> add_char str_aux coc;aux str (local_index+1) str_aux i
		with
			Invalid_argument _ -> str_aux
		in let buffer_aux = Buffer.create 1 
			in let final_aux_i_promise = ADN(float_of_string (contents (aux (contents buffer_for_all) 2 (buffer_aux) 1)),
										  float_of_string (contents (aux (contents buffer_for_all) 2 (buffer_aux) 2)),
										  float_of_string (contents (aux (contents buffer_for_all) 2 (buffer_aux) 3)),
														  (contents (aux (contents buffer_for_all) 2 (buffer_aux) 4)),
														  (contents (aux (contents buffer_for_all) 2 (buffer_aux) 5)))
			in (
			try
				(final_aux_i_promise,(aux buffer_for_all 0 (buffer_aux) 0))
			with
				raise (Failure "mostrar_lista1") -> (final_aux_i_promise,2.1)
			  | raise (Failure "mostrar_lista2") -> (final_aux_i_promise,2.2)
			  | raise (Failure "mostrar_lista3") -> (final_aux_i_promise,2.3)
			  | raise (Failure "mostrar_lista5") -> (final_aux_i_promise,2.5)
			  | raise (Failure "insertar") -> (final_aux_i_promise,0.)
			  | raise (Failure "buscar_por_size") -> (final_aux_i_promise,1.1)
			  | raise (Failure "buscar_por_address") -> (final_aux_i_promise,1.2)));;
	*)

	(* #SOLUCION
	*)
	
	let myfloat_of_string str = 
		try
			float_of_string (str)
		with 
			Failure _ -> 0.
	;;

	let read_str_uct source = 
		let rec aux all_in_str choosen_option one_word_buffer word_box word_index = function
				0 -> 
				begin match String.sub all_in_str 0 3 with 
					"-m1" -> (0.,0.,0.,"",""),Some 2.1
				  | "-m2" -> (0.,0.,0.,"",""),Some 2.2
				  | "-m3" -> (0.,0.,0.,"",""),Some 2.3
				  | "-m5" -> (0.,0.,0.,"",""),Some 2.5
				  | "-i1" -> aux all_in_str (Some 0.1) one_word_buffer word_box 0 4
				  | "-i2" -> aux all_in_str (Some 0.2) one_word_buffer word_box 0 4
				  | "-i3" -> aux all_in_str (Some 0.3) one_word_buffer word_box 0 4
				  | "-b1" -> aux all_in_str (Some 1.11) one_word_buffer word_box 0 4
				  | "-b2" -> aux all_in_str (Some 1.12) one_word_buffer word_box 0 4
				  | "-b3" -> aux all_in_str (Some 1.13) one_word_buffer word_box 0 4
				  | "-s1" -> aux all_in_str (Some 1.21) one_word_buffer word_box 0 4
				  | "-s2" -> aux all_in_str (Some 1.22) one_word_buffer word_box 0 4
				  | "-s3" -> aux all_in_str (Some 1.23) one_word_buffer word_box 0 4
				  | err -> (0.,0.,0.,"",""),None 
(*
Sys.command ("rm fifo_lectura_ESCRITURA fifo_LECTURA_escritura")
*)
				end
			  | coi -> 
					try match String.get all_in_str coi, word_index<>5 with
						' ',true -> let str_of_buffer = contents one_word_buffer in
									clear one_word_buffer;aux all_in_str choosen_option one_word_buffer ((Array.set word_box word_index (str_of_buffer));word_box) (word_index+1) (coi+1)
						| '\r',_ -> aux all_in_str choosen_option one_word_buffer ((Array.set word_box word_index (contents one_word_buffer));word_box) 5 1
						| '\n',_ -> aux all_in_str choosen_option one_word_buffer ((Array.set word_box word_index (contents one_word_buffer));word_box) 5 1
						| coc,true -> aux all_in_str choosen_option (add_char one_word_buffer coc;one_word_buffer) word_box word_index (coi+1)
						| _,false -> raise (Invalid_argument "")
					with
						Invalid_argument _ -> ((
											myfloat_of_string (Array.get word_box (1-1)),
											myfloat_of_string (Array.get word_box (2-1)),
											myfloat_of_string (Array.get word_box (3-1)),
											Array.get word_box (4-1),
											Array.get word_box (5-1)):alloc_data_node),choosen_option
			in let aux_string = String.create 500 
				in read source aux_string 0 500;
				aux aux_string None (Buffer.create 1 ) (Array.create 5 "") 0 0
	;;

	let insertar (elemento:anom_struct) = function
	 	  0,Trid(lia,_,_) ->  lia:=(elemento::!lia)
		| 1,Trid(_,lib,_) ->  lib:=(elemento::!lib)
		| 2,Trid(_,_,lic) ->  lic:=(elemento::!lic)
	;;

	(*si me quedo sin memoria no creo que pueda borrar nada xD,
	 solo usando el stack, habria que probar
	 una implementacion basada en el no-uso de ref podria valer ya que solo ocuparia stack 
	 pero no por que seria lo mismo solo que si ocupo el stack ni duda cabe de que no podria
	 hacer nada*V.2 facil de implementar*)
	let buscarydestruir lista_mutable_ looking_for = function
			  None -> Empty
			| Some coc -> let rec aux lista_mutable elemento found has_found lista_para_final =
						match lista_mutable with
							[] -> lista_mutable_:=lista_para_final;found
							| h::t -> match has_found, (elemento = (looking_for h)) with
										false,true -> aux (lista_para_final) elemento h true t
									  | false,false -> aux (t) elemento found false (h::lista_para_final)
									  | true,_ -> aux (t) elemento found true (h::lista_para_final)
					 in aux (!lista_mutable_) coc Empty false []
	;;

	let rec sub_mostrar where_to_print = function
		  [] -> 1
		| (h)::t -> print_str_uct h (write) where_to_print; sub_mostrar where_to_print t
	;;

	let rec mostrar_lista fifo_out = function
		  0,Trid(lia,_,_) -> sub_mostrar fifo_out !lia 
		| 1,Trid(_,lib,_) -> sub_mostrar fifo_out !lib 
		| 2,Trid(_,_,lic) -> sub_mostrar fifo_out !lic 
		| 3,tridente | 5,tridente -> mostrar_lista fifo_out (0, tridente); mostrar_lista fifo_out (1, tridente); mostrar_lista fifo_out (2, tridente)
	;;

end
;;

include Ipc_fifo_oimp;;(*need for being not a shit to work with*)

let rec main (lista:tridente) = function
	 fifo_out,fifo_in -> match read_str_uct fifo_in with
	 		a,Some 0.1 -> insertar (ADN(a)) (1-1,lista) ; main lista (fifo_out,fifo_in) 
		  | a,Some 0.2 -> insertar (ADN(a)) (2-1,lista) ; main lista (fifo_out,fifo_in) 
		  | a,Some 0.3 -> insertar (ADN(a)) (3-1,lista) ; main lista (fifo_out,fifo_in) 
		  | a,Some 1.11 -> print_str_uct (buscarydestruir ((function Trid(lia,_,_) -> lia) lista) (function ADN(_,b,_,_,_) -> b) (Some ((function (_,b,_,_,_) -> b) a))) (write) fifo_out ; main lista (fifo_out,fifo_in)
		  | a,Some 1.12 -> print_str_uct (buscarydestruir ((function Trid(_,lib,_) -> lib) lista) (function ADN(_,b,_,_,_) -> b) (Some ((function (_,b,_,_,_) -> b) a))) (write) fifo_out ; main lista (fifo_out,fifo_in)
		  | a,Some 1.13 -> print_str_uct (buscarydestruir ((function Trid(_,_,lic) -> lic) lista) (function ADN(_,b,_,_,_) -> b) (Some ((function (_,b,_,_,_) -> b) a))) (write) fifo_out ; main lista (fifo_out,fifo_in)
		  | a,Some 1.21 -> print_str_uct (buscarydestruir ((function Trid(lia,_,_) -> lia) lista) (function ADN(a,_,_,_,_) -> a) (Some ((function (a,_,_,_,_) -> a) a))) (write) fifo_out ; main lista (fifo_out,fifo_in)
		  | a,Some 1.22 -> print_str_uct (buscarydestruir ((function Trid(_,lib,_) -> lib) lista) (function ADN(a,_,_,_,_) -> a) (Some ((function (a,_,_,_,_) -> a) a))) (write) fifo_out ; main lista (fifo_out,fifo_in)
		  | a,Some 1.23 -> print_str_uct (buscarydestruir ((function Trid(_,_,lic) -> lic) lista) (function ADN(a,_,_,_,_) -> a) (Some ((function (a,_,_,_,_) -> a) a))) (write) fifo_out ; main lista (fifo_out,fifo_in)
		  | a,Some 2.1 -> mostrar_lista fifo_out (1-1,lista) ; main lista (fifo_out,fifo_in)
		  | a,Some 2.2 -> mostrar_lista fifo_out (2-1,lista) ; main lista (fifo_out,fifo_in)
		  | a,Some 2.3 -> mostrar_lista fifo_out (3-1,lista) ; main lista (fifo_out,fifo_in)
		  | a,Some 2.5 -> mostrar_lista fifo_out (5,lista) ; main lista (fifo_out,fifo_in)
		  | _,Some 3. -> raise (Failure "no tengo mas chollo")
		  | _,None -> main lista (fifo_out,fifo_in);;


print_endline "PRUEBA OK";;
let fifo_in = openfile "fifo_lectura_ESCRITURA" [O_RDONLY] 777;;
print_endline "fifo_lectura_ESCRITURA OK";;
let fifo_out = openfile "fifo_LECTURA_escritura" [O_WRONLY] 777;;
print_endline "fifo_LECTURA_escritura OK";;




main (Trid(ref[],ref[],ref[])) (fifo_out,fifo_in);;(*esto lo hace todo*)
(*si se quiere probar sin el archivo c pues mi consejo es el ocamltop y llamar a main con lo mismo pero (stdout,stdin)*)

(*estamos en windows, (suspiro) puto windows, no vale llamar a openfile supongo*)
(*
let fifo_in = open_out "fifo_lectura_ESCRITURA";;
let fifo_out = open_in "fifo_escritura_LECTURA";;
let fifo_in = open_out "fifo_escritura_LECTURA";;
let fifo_out = open_in "fifo_lectura_ESCRITURA";;
*)
