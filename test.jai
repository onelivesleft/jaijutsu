#import "Basic";
#import "Jaijutsu";

main :: () {
    result, error := eval(foo);
    if !error  print("result = %\n", to_string(result));
    else       print("Error!\n%\n", error.message);
}

foo :: #string jai
    ?foo := 100;
    while foo > 1 {
        if foo % 2 foo = foo * 3 + 1 else foo = foo / 2
        ?foo
    }
jai

