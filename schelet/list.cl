class List inherits IO {

    isEmpty() : Bool { true };

    hd(): Object {
        {
            abort();
            self;
        }
    };

    tl(): List {
        {
            abort();
            self;
        }
    };

    add(o: Object): List {
        new Cons.init(o, self)
    };

    append(list2 : List) : List { list2 };

    cons(o: Object): List {
        new Cons.init(o, self)
    };

    getIndex(x: Int): Object {
        {   
            abort();
            "ret";
        }
    };

    getSize(): Int {
        0
    };

    reverse() : List { { abort(); self; } };

    removeIndex(i: Int): List {
        {
            abort();
            self;
        }
    };

    filterBy(f: Filter): List {
        self
    };

    sortBy(c: Comparator, asc: Bool): List {
        self
    };

    toString(): String { "[  ]" };
};

class Cons inherits List {

    hd: Object;
    tl: List;
    converter: A2I <- new A2I;

    init(h : Object, t : List) : Cons {
        {
            hd <- h;
            tl <- t;
            self;
        }
    };

    isEmpty() : Bool { false };

    hd() : Object { hd };

    tl() : List { tl };

    add(o : Object) : List {
		let reverseList : List <- self.reverse(),
			newList : List <- reverseList.cons(o)
		in
    		newList.reverse()
    };

    append(list2 : List) : List {
        if list2.isEmpty() then
            self
        else
            let reverseList : List <- self.reverse(),
                newList : List <- reverseList.cons(list2.hd()),
                aux : List <- list2.tl()
            in
            {
                while (not aux.isEmpty()) loop
                    {
                        newList <- newList.cons(aux.hd());
                        aux <- aux.tl();
                    }
                pool;
                newList.reverse();
            }
        fi
    };

    getIndex(i: Int): Object {
        let current_idx: Int <- 0,
            ret: Object <- false,
            aux: List <- self
		in
            {   
                while (not aux.isEmpty()) loop
                    {
                        if current_idx = i then
                            ret <- aux.hd()
                        else
                            1
                        fi;
                        aux <- aux.tl();
                        current_idx <- current_idx + 1;
                    }
                pool;

                ret;
            }
    };

    getSize(): Int {
        let current_idx: Int <- 0,
            aux: List <- self
		in
            {
                while (not aux.isEmpty()) loop
                    {
                        aux <- aux.tl();
                        current_idx <- current_idx + 1;
                    }
                pool;

                current_idx;
            }
    };

    removeIndex(i: Int): List {
        let current_idx: Int <- 0,
            ret: List <- new List,
            aux: List <- self
		in
            {
                while (not aux.isEmpty()) loop
                    {
                        if current_idx = i then
                            1
                        else
                            ret <- ret.add(aux.hd())
                        fi;
                        aux <- aux.tl();
                        current_idx <- current_idx + 1;
                    }
                pool;
                ret;
            }
    };

    reverse() : List {
    	let newList : List <- new List.cons(hd()),
    		aux : List <- tl()
		in
			{
	    		while (not aux.isEmpty()) loop
	    			{
	    				newList <- newList.cons(aux.hd());
	    				aux <- aux.tl();
	    			}
	    		pool;
	    		newList;
	    	}
    };

    filterBy(f: Filter): List {
        let newList : List <- new List,
    		aux : List <- tl()
		in
			{
                if (f.filter(hd)) then
                    newList <- newList.add(hd)
                else
                    1
                fi;

	    		while (not aux.isEmpty()) loop
	    			{
                        if (f.filter(aux.hd())) then
                            newList <- newList.add(aux.hd())
                        else
                            1
                        fi;
	    				aux <- aux.tl();
	    			}
	    		pool;
	    		newList;
	    	}
    };

    sortBy(c: Comparator, asc: Bool): List {
        let new_list: List <- new List,
            list1: List,
            list2: List
        in
            {
                if isEmpty() then
                    new_list <- new List
                else
                {
                    if asc then {
                        list1 <- tl().filterBy(new SmallFilter.init(hd(), c));
                        list2 <- tl().filterBy(new BigFilter.init(hd(), c));
                    } else {
                        list2 <- tl().filterBy(new SmallFilter.init(hd(), c));
                        list1 <- tl().filterBy(new BigFilter.init(hd(), c));
                    } fi;

                    new_list <- list1.sortBy(c, asc).append(new List.add(hd())).append(list2.sortBy(c, asc));
                }
                fi;
                new_list;
            }
    };
 
    toString(): String {
        {
            let ret: String <- "[ ",
                hd: Object,
                aux: List <- self
            in
                {
                    while (not aux.isEmpty()) loop
                        {
                            hd <- aux.hd();
                            case hd of
                                x: Int => ret <- ret.concat("Int(").concat(converter.i2a(x)).concat(")");
                                x: String => ret <- ret.concat("String(").concat(x).concat(")");
                                x: Product => ret <- ret.concat(x.toString());
                                x: Rank => ret <- ret.concat(x.toString());
                                x: IO => ret <- ret.concat("IO()");
                                x: Bool => ret <- ret.concat("Bool(").concat(converter.b2s(x)).concat(")");
                                x: List => ret <- ret.concat(x.toString());
                            esac;

                            aux <- aux.tl();

                            if not aux.isEmpty() then
                                ret <- ret.concat(", ")
                            else
                                ret <- ret.concat(" ]")
                            fi;
                        }
	    		    pool;
                    ret;
                };
        }
    };
};