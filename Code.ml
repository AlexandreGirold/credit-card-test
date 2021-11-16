(*meme code que en python*)

let test_card liste = 
  let rec aux liste count n = 
    match liste with
    
      [] -> if count mod 10 = 0
        then true
        else false
          
    |f::liste -> begin 
        
        match (n mod 2) with
        
          0-> if (f*2)<= 9
            then 
              aux liste (count+(f*2))(n-1)
            else 
              aux liste (count+((f*2) mod 10)+1)(n-1)
                
        |_ -> aux liste (count+f) (n-1)
      end
      
  in aux liste 0 16 ;;

test_card [4;9;7;5;8;9;9;1;4;4;8;7;0;8;3;7];; 
