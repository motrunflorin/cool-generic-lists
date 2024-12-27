class Commands inherits IO {
    -- lists: List <- new List;
    converter: A2I<- new A2I;
    objectFactory: ObjectFactory<- new ObjectFactory;
    fil: Filter<- new Filter;
    cpr: Comparator<- new Comparator;

    -- init(listss: List): SELF_TYPE {{
    --     lists <- listss;
    --     self;
    -- }};


    help(): IO {
        {
            out_string("Available commands:\n");
            out_string("-help\n");
            out_string("-load\n");
            out_string("-print | print idx\n");
            out_string("-merge index1 index2\n");
            out_string("-filterBy index {ProductFilter,RankFilter,SamePriceFilter}\n");
            out_string("-sortBy index {PriceComparator,RankComparator,AlphabeticComparator} {ascendent,descendent}\n");
        }
    };

--     -- load(): String {
--     --     let 
--     --         tokenizer: StringTokenizer, 
--     --         current_line: String,
--     --         current_obj: Object,
--     --         loops: Bool <- true,
--     --         objectFactory: ObjectFactory <- new ObjectFactory,
--     --         current_list: List <- new List
--     --     in
--     --         {
--     --             while loops loop {
--     --                 current_line <- in_string();
--     --                 if (current_line = "END") then
--     --                     loops <- false
--     --                 else {
--     --                     tokenizer <- tokenizer.init(current_line, " ");
--     --                     current_list <-  current_list.add(objectFactory.create(tokenizer));
--     --                     --case tokenizer of x : Tokenizer => x;esac;
--     --                     --case objectFactory.create(tokenizer) of x : Object => current_obj <- x; esac;
--     --                     --current_list <- current_list.add(current_obj);
--     --                 } fi;
--     --             } pool;
--     --             lists <- lists.add(current_list);
--     --             current_line;
--     --         }
--     -- };

    print(tokenizer: StringTokenizer, lists: List): Int {
        if tokenizer.getTokens().tl().isEmpty() then {
            let idx: Int <- 1,
                aux: List <- lists
            in {
                while (not aux.isEmpty()) loop {
                    out_int(idx).out_string(": ").out_string(case aux.hd() of x: List => x.toString().concat("\n"); esac);
                    aux <- aux.tl();
                    idx <- idx + 1;
                } pool;
                idx;
            };
        } else {
            let 
                idx: Int <- case tokenizer.getTokens().getIndex(1) of x: String => converter.a2i(x); esac
            in {
                if (lists.getSize() < idx) then
                    1
                else {
                    case lists.getIndex(idx - 1) of
                        x: List => out_string(x.toString().concat("\n"));  -- print at idx-1 
                    esac;
                    1;
                } fi;

            };
        } fi
    };

    merge(tokenizer: StringTokenizer, lists: List): List {
        let
            idx1: Int <- case tokenizer.getTokens().getIndex(1) of x: String => converter.a2i(x); esac,
            idx2: Int <- case tokenizer.getTokens().getIndex(2) of x: String => converter.a2i(x); esac
        in {
            let new_list: List <-   (case lists.getIndex(idx1 - 1) of x: List => x; esac)
                                    .append
                                    (case lists.getIndex(idx2 - 1) of x: List => x; esac) -- (idx2 - 1) list
            in {
                -- remove the lists
                lists <- lists.removeIndex(idx1 - 1);
                lists <- lists.removeIndex(if idx1 < idx2 then idx2 - 2 else idx2 - 1 fi);
                
                lists <- lists.add(new_list);  -- add new_list to the end
                lists;  -- successful merge
            };
        }
    };

    filterBy(tokenizer: StringTokenizer, lists: List, fil: Filter): List {
        let aux_lists: List <- new List,
            aux: List <- lists,
            current_list: List,
            current_idx: Int <- 0,
            i: Int <- case tokenizer.getTokens().getIndex(1) of x: String => converter.a2i(x); esac,
            filter: String <- case tokenizer.getTokens().getIndex(2) of x: String => x; esac,
            flt: Filter <- fil.getFilter(filter)   
        in {
            while (not aux.isEmpty()) loop {
                if i - 1 = current_idx then {
                    case lists.getIndex(current_idx) of
                        x: List => current_list <- x;
                    esac;

                    current_list <- current_list.filterBy(flt);
                    aux_lists <- aux_lists.add(current_list);
                } else {
                    aux_lists <- aux_lists.add(lists.getIndex(current_idx));
                } fi;
                aux <- aux.tl();
                current_idx <- current_idx + 1;
            } pool;
            lists <- aux_lists;
            lists;
        }
    };

    sortBy(tokenizer: StringTokenizer,lists: List,cpr:Comparator): List {
        let aux_lists: List <- new List,
            aux: List <- lists,
            current_list: List,
            current_idx: Int <- 0,
            i: Int <- case tokenizer.getTokens().getIndex(1) of x: String => converter.a2i(x); esac,
            comparator: String <- case tokenizer.getTokens().getIndex(2) of x: String => x; esac,
            order: String <- case tokenizer.getTokens().getIndex(3) of x: String => x; esac,
            comp: Comparator <- cpr.getComparator(comparator),
            asc: Bool <- if order = "ascendent" then true else false fi
        in {
            while (not aux.isEmpty()) loop {
                if i - 1 = current_idx then {
                    case lists.getIndex(current_idx) of
                        x: List => current_list <- x;
                    esac;
                    current_list <- current_list.sortBy(comp, asc);
                    aux_lists <- aux_lists.add(current_list);
                } else {
                    aux_lists <- aux_lists.add(lists.getIndex(current_idx));
                } fi;
                aux <- aux.tl();
                current_idx <- current_idx + 1;
            } pool;
            lists <- aux_lists;
            lists;
        }
    };
};
