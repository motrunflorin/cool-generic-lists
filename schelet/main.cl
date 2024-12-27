class Main inherits IO {
    lists : List <- new List;
    fil : Filter <- new Filter;
    cpr : Comparator <- new Comparator;
    tokenizer: StringTokenizer <- new StringTokenizer;
    command: Command <- new Command;
    cmd_type: String;
    somestr : String;
    looping : Bool <- true;

    process_cmd(cmd: String, tokenizer: StringTokenizer): Int {{ 
            if cmd = "help" then
                command.help()
            else if cmd = "print" then
                command.print(tokenizer, lists)
            else if cmd = "load" then
                lists <- command.load(lists)
            else if cmd = "merge" then
                lists <- command.merge(tokenizer, lists) 
            else if cmd = "filterBy" then
                lists <- command.filterBy(tokenizer, lists, fil)                    
            else  if cmd = "sortBy" then
                lists <- command.sortBy(tokenizer, lists, cpr)
            else
                1  -- unrecognized command
            fi fi fi fi fi fi;
            1;   -- successful processing 
    }};

    main():Object {{
        lists <- command.load(lists);
        while looping loop {
                somestr <- in_string();

                tokenizer <- tokenizer.init(somestr, " ");

                case tokenizer.getTokens().hd() of
                            x: String => cmd_type <- x;
                esac;

                process_cmd(cmd_type, tokenizer);
            } pool;
	    }
    };
};