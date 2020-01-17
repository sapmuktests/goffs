// Implements Conway's Game Of Life 
// https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life on a torus


namespace Game
open NUnit.Framework
open System

module GameOfLife = 

    let evolve(world : bool array array) : bool array array =
        (*
         This function is single iteration of the game of life. 
         It will first find the number of live Neighbours.
         Then apply following logic
         1.	If there are 3 alive , the cell becomes alive 
         2.	If the cell is alive and 2 Neighbours are alive then cell remains alive
         3.	Anything else , the cell dies 
         
        *)
        // * surounding location 
        let locations = [-1,-1; 0,-1; 1,-1; -1, 0; 1, 0; -1, 1; 0, 1; 1,1]    
        // Max on x co-ordinate
        let max_x = world.[0].Length
        // Max Y co-ordinate 
        let max_y = world.Length
        
        // Counting number of live cells 
        let count_alive x y =
            locations
            |> List.map(fun(i,j) ->
                let x,y = x+i, y+j
                if x >=0 && x< max_x && y>= 0 && y < max_y && world.[x].[y] = true  
                then 1
                else 0
                )
            |> List.sum
       
        // this is the transition
        let next_state x y current_status =      
               match current_status, count_alive x y with
               |  _ , 3 -> true //If there are 3 alive , the cell becomes alive 
               | true , 2 -> true // If the cell is alive and 2 Neighbours are alive then cell remains alive
               |  _ , _ -> false // anything else , it dies
        
        // run it for the each cell in the world 
        world |>Array.mapi (fun j row ->
              row|>Array.mapi (fun i cell ->
                            next_state i j cell
              )            
           )            
       

    let read (file_name: string): bool array array =
        (*
        
        Reading data from local file. Convers _ to false (died) and  * to true(alive)
        also claens up any empty lines 

        *)
        let input = System.IO.StreamReader(file_name)
        let all_text = input.ReadToEnd()
        let lines = all_text.Split([|'\r'; '\n'|])

        let world : bool array array = Array.zeroCreate lines.Length
        for x in [0 .. lines.Length-1] do
            world.[x] <- Array.zeroCreate lines.[x].Length
            let mutable c = 0
            while c < lines.[x].Length do
                if lines.[x].[c] = '_' then
                    world.[x].[c] <- false
                elif lines.[x].[c] = '*' then
                    world.[x].[c] <- true
                c <- c + 1
        world |>Array.filter (fun row -> row.Length > 0)



    let print_world (world: bool array array)  =
        (*
        Print the world . Convert from true/false to _/* 
        *)
        for a in [0 .. world.Length-1] do
            let mutable line = ""
            for b in [0 .. world.[0].Length-1] do
                if world.[a].[b] then line <- line + "*" else line <- line + "_"
            printfn "%s" line
     

    let evolve_loop world count =
        (*
        
        This method handle if game needs tobe run in a multiple loop
        It calls evolve method multiple times with output from one step is passed to another 
        *)
        let rec loop (world,count) =
            if count < 1 
            then 
             world
            else          
             let new_world = evolve world
             loop(new_world,count-1)
    
        loop(world,count) 
    

module Test =
    [<TestFixture>]
    type TestClass () =
    
        [<Test>]
        //Test 3 neighbors  it come live        
        member this.TestMethodCountLive_3_born() =
           let input_array = [|[|true;true;false|];
                               [|true;false;false|];
                               [|false;false;false|] |]
           Assert.True(GameOfLife.evolve(input_array).[1].[1])
           
    
        [<Test>]
        // stay alive with 2 neighbors 
        member this.TestMethodCountLive_2_stay_live() =
            let input_array = [|[|false;true;false|];
                                [|true;true;false|];
                                [|false;false;false|] |]
            Assert.True(GameOfLife.evolve(input_array).[1].[1])
        [<Test>]
        //over population, > 3 neighbor live, die
        member this.TestMethodCountLive_4_die() =
            let input_array = [|[|true;true;true|];
                                [|true;true;false|];
                                [|false;false;false|] |]
            Assert.False(GameOfLife.evolve(input_array).[1].[1])
        [<Test>]
        //under population , < 2 neighbor live, die
        member this.TestMethodCountLive_1_die() =
            let input_array = [|[|true;false;false|];
                                [|false;true;false|];
                                [|false;false;false|] |]
            Assert.False(GameOfLife.evolve(input_array).[1].[1])

module main =
    [<EntryPoint>]
    let main argv =
        
        // finding correct base directory 
        let base_dir = __SOURCE_DIRECTORY__
        //file
        let file_name = "/sample_input.txt"
        // actual path
        let file_path = base_dir + file_name
        
        //raed file from local file system
        let world = GameOfLife.read file_path

        // Runa a game of life loop
        let new_world = GameOfLife.evolve_loop world 1

        // Print the output
        GameOfLife.print_world new_world
   
           
        42 // return an integer exit code