(*******************************
 *** Classes Product-related ***
 *******************************)
class Product {
    name : String;
    model : String;
    price : Int;

    init(n : String, m: String, p : Int): SELF_TYPE {{
        name <- n;
        model <- m;
        price <- p;
        self;
    }};

    getprice():Int{ price * 119 / 100 };

    toString():String {
        type_name().concat("(").concat(name).
                    concat(";").concat(model).concat(")")
    };
};

class Edible inherits Product {
    -- VAT tax is lower for foods
    getprice():Int { price * 109 / 100 };
};

class Soda inherits Edible {
    -- sugar tax is 20 bani
    getprice():Int {price * 109 / 100 + 20};
};

class Coffee inherits Edible {
    -- this is technically poison for ants
    getprice():Int {price * 119 / 100};
};

class Laptop inherits Product {
    -- operating system cost included
    getprice():Int {price * 119 / 100 + 499};
};

class Router inherits Product {};

(****************************
 *** Classes Rank-related ***
 ****************************)
class Rank {
    name : String;

    init(n : String):Rank {
        {
            name <- n;
            self;
        }
    };

    toString():String {
        type_name().concat("(").concat(name).concat(")")
    };
};

class Private inherits Rank {};

class Corporal inherits Private {};

class Sergent inherits Corporal {};

class Officer inherits Sergent {};

class ObjectFactory{
    converter: A2I <- new A2I;

    dispatch(str: String): Object {      
        if str = "Edible" then  
            new Edible
        else if str = "Soda" then 
            new Soda
        else if str = "Coffee" then
            new Coffee
        else if str = "Laptop" then
            new Laptop
        else if str = "Router" then 
            new Router
        else if str = "Private" then  
            new Private
        else if str = "Corporal" then 
            new Corporal
        else if str = "Sergent" then
            new Sergent
        else if str = "Officer" then
            new Officer
        else if str = "String" then
            ""
        else if str = "Int" then
            1
        else if str = "Bool" then 
            true
        else if str = "IO" then
            new IO
        else 
            new Object
        fi fi fi fi fi fi fi fi fi fi fi fi fi   
    };

    create(tokenizer : StringTokenizer): Object {
        let 
            str : String <- case tokenizer.getTokens().hd() of x: String =>x; esac,
            curr_obj : Object <- dispatch(str),                                                    
            n: String <- case tokenizer.getTokens().getIndex(1) of 
                            x: String => x; 
                            x : Bool => "";
                         esac
        in {
            case curr_obj of
                x: Product =>
                    let 
                        m: String <- case tokenizer.getTokens().getIndex(2) of x: String =>  x; esac,
                        p: Int <- case tokenizer.getTokens().getIndex(3) of x: String => converter.a2i(x); esac
                    in
                        x.init(n, m, p);                                                
                                                 
                x: Rank => x.init(n);
                x: String => n;                       
                x: Int => converter.a2i(n);
                x: Bool => true;
                x: IO => x;
            esac;
        }
    };
};