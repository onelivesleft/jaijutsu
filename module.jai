#import "Basic";
#import "Hash_Table";
#import "Math";
#load "lexer.jai";

TYPE_NEEDS_INFERENCE :: struct {}

Reference :: struct {
    type : Type = TYPE_NEEDS_INFERENCE;

    union {
        data: *void;
        int_value: int;
        float_value: float64;
        bool_value: bool;
    }

    flags: enum_flags {
        IS_CONSTANT;
    }
}

to_bool :: (reference: Reference) -> bool {
    return reference.data != null;
}

print :: (reference: Reference) {
    print("%", to_string(reference));
}

to_string :: (reference: Reference) -> string {
    flags := ifx reference.flags then tprint(" (%)", reference.flags);
    if reference.type == int {
        return tprint("%1%2", reference.int_value, flags);
    }
    else if reference.type == float64 {
        return tprint("%1%2", reference.float_value, flags);
    }
    else if reference.type == bool {
        return tprint("%1%2", reference.bool_value, flags);
    }
    else {
        return tprint("%1%2", reference.data, flags);
    }
}

Scope :: struct {
    names: Table(string, Reference);
    result: Reference;
}

Error :: struct {
    message: string;
}

open_scope :: (machine: *Machine, token: *Token) {
    scope := array_add(*machine.scopes);
    scope.names.allocator = machine.scopes.allocator;
}

close_scope :: (using machine: *Machine, token: *Token) {
    assert(scopes.count > 0); // We call open_scope in init_machine, so this should always be true here.
    deinit(*scopes[scopes.count - 1].names);
    result := scopes[scopes.count - 1].result;
    scopes.count -= 1;
    if scopes.count == 0
        raise_error(machine, token, "Unexpected closing brace");
    scopes[scopes.count - 1].result = result;
}

add_name :: (using machine: *Machine, name: string, value: Reference) -> ok: bool {
    names := *scopes[scopes.count - 1].names;
    if table_find_pointer(names, name)  return false;

    if machine.advance_without_executing  return true;

    table_set(names, name, value);
    return true;
}

Machine :: struct {
    scopes: [..] Scope;
    finished: bool;
    output_next_result: bool;
    advance_without_executing: int;
    last_result: Reference;
    error: *Error;
}

init_machine :: (using machine: *Machine) {
    init_precedence_table();
    scopes.allocator = context.allocator;
    open_scope(machine, null);
}

get_result :: (using machine: *Machine) -> Reference {
    return machine.last_result;
}

set_result :: (using machine: *Machine, reference: Reference) {
    scopes[scopes.count - 1].result = reference;
    machine.last_result = reference;
    if machine.output_next_result {
        machine.output_next_result = false;
        print(reference);
        print("\n");
    }
}

raise_error :: (machine: *Machine, token: *Token, message: string) #expand {
    _message := message;
    if token.type == .END_OF_INPUT  _message = "Unexpected end of input";
    machine.error = New(Error,, temp);
    machine.error.message = tprint("%: %\n", token.type, _message);
    machine.finished = true;
    `return;
}

eval :: (s: string, lexer: *Lexer = null) -> Reference, *Error {
    machine := New(Machine,, temp);
    init_machine(machine,, temp);
    result, error := eval(machine, s, lexer);
    return result, error;
}

eval :: (machine: *Machine, s: string, lexer: *Lexer = null) -> Reference, *Error {
    if !lexer  lexer = New(Lexer,, temp);

    set_input_from_string(lexer, s);

    run(machine, lexer);

    return get_result(machine), machine.error;
}

get_reference :: (using machine: *Machine, token: *Token, name: string) -> Reference, found: bool {
    for < i: scopes.count - 1 .. 0 {
        reference, found := table_find(*scopes[i].names, name);
        if found  return reference, true;
    }
    return .{}, false;
}

get_reference_pointer :: (using machine: *Machine, token: *Token, name: string) -> *Reference {
    for < i: scopes.count - 1 .. 0 {
        reference := table_find_pointer(*scopes[i].names, name);
        if reference  return reference;
    }
    return null;
}

run :: (machine: *Machine, lexer: *Lexer) {
    while !machine.finished {
        expect_statement_or_block(machine, lexer);
    }
}

eat_statement_or_block :: (machine: *Machine, lexer: *Lexer) {
    disable_execution(machine, lexer);
    expect_statement_or_block(machine, lexer);
    enable_execution(machine, lexer);
}

expect_statement_or_block :: (machine: *Machine, lexer: *Lexer) {
    if machine.finished  return;

    token := peek_token(lexer, 0);

    // Is a single statement:
    if token.type != #char "{" {
        expect_statement(machine, lexer);
        return;
    }

    // Is the start of a block:
    stop_at_scope_level := machine.scopes.count;
    while !machine.finished {
        if token.type == #char "{" {
            eat_token(lexer);
            open_scope(machine, token);
        }
        else if token.type == #char "}" {
            eat_token(lexer);
            close_scope(machine, token);
            if machine.scopes.count <= stop_at_scope_level
                break;
        }
        else {
            expect_statement(machine, lexer);
        }
        token = peek_token(lexer, 0);
    }
}

expect_statement :: (machine: *Machine, lexer: *Lexer) {
    if machine.finished  return;

    token := peek_token(lexer, 0);
    next_token := peek_token(lexer, 1);

    if token.type == .IDENT {
        name := token.ident_value.name;
        if next_token.type == {
            case #char "(";  eat_token(lexer); expect_call_parameters(machine, lexer, name);
            case #char ":";  eat_token(lexer); expect_declaration(machine, lexer, name);
            case #char "=";  eat_token(lexer); expect_assignment(machine, lexer, name);
            case .END_OF_INPUT; #through;
            case .NEWLINE; #through;
            case #char ";";  eat_token(lexer); eval_name(machine, token, name);

            case;
            if is_binary_operator(next_token) {
                eval_expression(machine, lexer);
            }
            else {
                raise_error(machine, next_token, "Unexpected token");
            }
        }
    }
    else if token.type == #char "?" {
        eat_token(lexer);
        machine.output_next_result = true;
    }
    else if token.type == #char ";" || token.type == .NEWLINE {
        eat_token(lexer);
    }
    else if token.type == .KEYWORD_IF {
        eat_token(lexer);
        expect_if(machine, lexer);
    }
    else if token.type == .KEYWORD_WHILE {
        eat_token(lexer);
        expect_while(machine, lexer);
    }
    else if is_unary_operator(token) {
        eval_expression(machine, lexer);
    }
    else if token.type == .END_OF_INPUT {
        eat_token(lexer);
        machine.finished = true;
    }
    else {
        raise_error(machine, token, "Unexpected token");
    }
}

expect_call_parameters :: (machine: *Machine, lexer: *Lexer, procedure_name: string) {
    raise_error(machine, peek_next_token(lexer), "Not yet implmented");
}

expect_declaration :: (machine: *Machine, lexer: *Lexer, name: string) {
    if machine.finished  return;

    token := peek_next_token(lexer);
    assert(token.type == #char ":");
    eat_token(lexer);

    token = peek_next_token(lexer);

    declaration: Reference;

    if token.type == .IDENT {
        type, ok := type_from_ident(token);
        if !ok  raise_error(machine, token, "Unknown type");
        declaration.type = type;
        eat_token(lexer);
        token = peek_next_token(lexer);
    }

    if token.type == {
        case #char ":"; declaration.flags |= .IS_CONSTANT;
        case #char "=";

        case;
        raise_error(machine, token, "Expected : or =");
    }

    eat_token(lexer);

    eval_expression(machine, lexer);
    if machine.last_result.type == TYPE_NEEDS_INFERENCE
        raise_error(machine, token, "Result is untyped - this shouldn't happen!");
    declaration.data = machine.last_result.data;
    if declaration.type == TYPE_NEEDS_INFERENCE {
        declaration.type = machine.last_result.type;
    } else if declaration.type != machine.last_result.type {
        raise_error(machine, token, tprint("Type mismatch! Expected: %  Got: %", declaration.type, machine.last_result.type));
    }
    declaration.flags |= machine.last_result.flags;
    if !add_name(machine, name, declaration)  raise_error(machine, token, tprint("% already exists", name));
}

expect_assignment :: (machine: *Machine, lexer: *Lexer, name: string) {
    if machine.finished  return;

    token := peek_next_token(lexer);
    target := get_reference_pointer(machine, token, name);
    if !target  raise_error(machine, token, tprint("Unknown variable: %", name));
    if target.type == TYPE_NEEDS_INFERENCE
        raise_error(machine, token, "Target is untyped - this shouldn't happen!");

    assert(token.type == #char "=");
    eat_token(lexer);

    token = peek_next_token(lexer);

    eval_expression(machine, lexer);
    if machine.finished || machine.advance_without_executing  return;

    if machine.last_result.type == TYPE_NEEDS_INFERENCE
        raise_error(machine, token, "Result is untyped - this shouldn't happen!");
    if target.type != machine.last_result.type
        raise_error(machine, token, tprint("Type mismatch! Expected: %  Got: %", target.type, machine.last_result.type));
    target.data = machine.last_result.data;
}

expect_if :: (machine: *Machine, lexer: *Lexer) {
    token := peek_next_token(lexer);
    eval_expression(machine, lexer);
    if machine.finished  return;

    condition := to_bool(machine.last_result);

    token = peek_next_token(lexer);
    if token.type == .KEYWORD_THEN {
        eat_token(lexer);
        token = peek_next_token(lexer);
    }

    did_else := false;
    if token.type == .KEYWORD_ELSE {
        did_else = true;
        eat_token(lexer);
        if !condition  expect_statement_or_block(machine, lexer);
        else           eat_statement_or_block(machine, lexer);
        if machine.finished  return;
    } else {
        if  condition  expect_statement_or_block(machine, lexer);
        else           eat_statement_or_block(machine, lexer);
        if machine.finished  return;
    }

    if !did_else {
        token = peek_next_token(lexer);
        if token.type == .KEYWORD_ELSE {
            eat_token(lexer);
            if !condition  expect_statement_or_block(machine, lexer);
            else           eat_statement_or_block(machine, lexer);
        }
    }
}

expect_while :: (machine: *Machine, lexer: *Lexer) {
    last_result := machine.last_result;
    defer machine.last_result = last_result;

    while true {
        _while_lexer := lexer.*;
        while_lexer := *_while_lexer;
        token := peek_next_token(while_lexer);
        eval_expression(machine, while_lexer);
        if machine.finished  return;

        condition := to_bool(machine.last_result);
        if !condition  break;

        expect_statement_or_block(machine, while_lexer);
        last_result = machine.last_result;
        if machine.finished  return;
    }

    eat_expression(machine, lexer);
    eat_statement_or_block(machine, lexer);
}

type_from_ident :: (token: *Token) -> Type, success: bool {
    assert(token.type == .IDENT);
    name := token.ident_value.name;

    if      name == "int"     return int, true;
    else if name == "float"   return float64, true;
    else if name == "string"  return string, true;

    return void, false;
}

eval_expression :: (machine: *Machine, lexer: *Lexer) {
    expression_node := parse_expression(machine, lexer);
    if !expression_node  raise_error(machine, peek_next_token(lexer), "Failed to evaluate expression");
    if !machine.advance_without_executing {
        result := eval_expression_node(machine, expression_node);
        set_result(machine, result);
    }
}

// @Cleanup Do we really need this?
eval_name :: (using machine: *Machine, token: *Token, name: string) {
    if machine.finished || machine.advance_without_executing  return;

    reference, found := get_reference(machine, token, name);
    if !found  raise_error(machine, token, "Name not found");
    set_result(machine, reference);
}

enable_execution :: (machine: *Machine, lexer: *Lexer) {
    machine.advance_without_executing -= 1;
    if machine.advance_without_executing < 0
        raise_error(machine, peek_next_token(lexer), "Tried to enable execution while already executing. This should not happen!");
}

disable_execution :: (machine: *Machine, lexer: *Lexer) {
    machine.advance_without_executing += 1;
}

eat_expression :: (machine: *Machine, lexer: *Lexer) {
    disable_execution(machine, lexer);
    eval_expression(machine, lexer);
    enable_execution(machine, lexer);
}

parse_leaf :: (machine: *Machine, lexer: *Lexer) -> *Expression.Node {
    token := peek_next_token(lexer);
    if token.type == {
        case .NUMBER;
            node := new_node();
            node.kind = .value;
            if token.value_flags & .FLOAT {
                node.value = .{ float_value=token.float64_value, type=float64 };
            } else {
                node.value = .{ int_value=(cast(*int)*token.integer_value).*, type=int };
            }
            eat_token(lexer);
            return node;

        case .IDENT;
            name := token.ident_value.name;
            next_token := peek_token(lexer, 1);
            if next_token.type ==  #char "("  return null;
            reference, found := get_reference(machine, token, name);
            if !found  return null;
            node := new_node();
            node.kind = .value;
            node.value = reference;
            eat_token(lexer);
            return node;

        case .STRING;
            return null;

        case;
            if is_unary_operator(token) {
                node := new_node();
                node.kind = .unary_operator;
                node.op = token.type;
                eat_token(lexer);
                node.left = parse_leaf(machine, lexer);
                if !node.left {
                    free_node(node);
                    return null;
                }
                eat_token(lexer);
                return node;
            }
            return null;
    }
}

parse_increasing_precedence :: (machine: *Machine, lexer: *Lexer, left: *Expression.Node, min_prec: u8) -> *Expression.Node {
    token := peek_next_token(lexer);
    if !is_binary_operator(token)  return left;
    next_prec := precedence_table[token.type];

    if next_prec <= min_prec  return left;

    eat_token(lexer);
    right := parse_expression(machine, lexer, next_prec);
    op_node := new_node();
    op_node.kind = .binary_operator;
    op_node.op = token.type;
    op_node.left = left;
    op_node.right = right;
    return op_node;
}


parse_expression :: (machine: *Machine, lexer: *Lexer, min_prec : u8 = 0) -> *Expression.Node {
    left := parse_leaf(machine, lexer);
    if !left  return null;

    while true {
        node := parse_increasing_precedence(machine, lexer, left, min_prec);
        if node == left  break;

        left = node;
    }

    return left;
}


eval_expression_node :: (machine: *Machine, node: *Expression.Node) -> Reference, error: string {
    if node.kind == .value  return node.value, "";

    if node.kind == .unary_operator {
        value, error := eval_expression_node(machine, node.left);
        if error  return .{}, error;

        if node.op == {
            case #char "!";
                if value.type == int
                    value.int_value = ifx value.int_value then 0 else 1;
                else if value.type == float64
                    value.float_value = ifx value.float_value then 0.0 else 1.0;
                else
                    return .{}, make_error("Could not `!`", value);

            case .BITWISE_NOT;
                value.int_value = ~value.int_value;
        }
        return value, "";
    }

    if node.kind == .binary_operator {
        a, error := eval_expression_node(machine, node.left);
        if error  return .{}, error;
        b:, error = eval_expression_node(machine, node.right);
        if error  return .{}, error;

        insert_number_op :: (op: string) #expand {
            #insert -> string {
                return sprint(#string jai
                    if a.type == bool {
                        if b.type != int && b.type != bool
                            `return .{}, make_error("Could not `%1`", a, b);
                        a.type = int;
                        a.int_value == ifx a.bool_value then 1 else 0;
                    }
                    if b.type == bool {
                        if a.type != int
                            `return .{}, make_error("Could not `%1`", a, b);
                        b.type = int;
                        b.int_value == ifx b.bool_value then 1 else 0;
                    }
                    if a.type == int && b.type == int {
                        a.int_value %1= b.int_value;
                    }
                    else if a.type == float64 && b.type == float64 {
                        a.float_value %1= b.float_value;
                    }
                    else {
                        `return .{}, make_error("Could not `%1`", a, b);
                    }
                jai, op);
            }
        }

        insert_float_specialized_op :: (op: string, float_op: string) #expand {
            #insert -> string {
                return sprint(#string jai
                    if a.type == bool {
                        if b.type != int && b.type != bool
                            `return .{}, make_error("Could not `%1`", a, b);
                        a.type = int;
                        a.int_value == ifx a.bool_value then 1 else 0;
                    }
                    if b.type == bool {
                        if a.type != int
                            `return .{}, make_error("Could not `%1`", a, b);
                        b.type = int;
                        b.int_value == ifx b.bool_value then 1 else 0;
                    }
                    if a.type == int && b.type == int {
                        a.int_value %1= b.int_value;
                    }
                    else if a.type == float64 && b.type == float64 {
                        a.float_value = %2(a.float_value, b.float_value);
                    }
                    else {
                        `return .{}, make_error("Could not `%1`", a, b);
                    }
                jai, op, float_op);
            }
        }

        insert_integer_op :: (op: string) #expand {
            #insert -> string {
                    return sprint(#string jai
                    if a.type == int && b.type == int {
                        a.int_value %1= b.int_value;
                    } else {
                        `return .{}, make_error("Could not `%1`", a, b);
                    }
                jai, op);
            }
        }

        insert_logical_op :: (op: string) #expand {
            #insert -> string {
                    return sprint(#string jai
                    a.type == bool;
                    a.bool_value %1= b.bool_value;
                jai, op);
            }
        }

        insert_comparison_op :: (op: string) #expand {
            #insert -> string {
                return sprint(#string jai
                    if a.type == int {
                        if b.type == int {
                            a.bool_value = a.int_value %1 b.int_value;
                        } else if b.type == float64 {
                            a.bool_value = a.int_value %1 cast(int) b.float_value;
                        } else {
                            `return .{}, make_error("Could not `%1`", a, b);
                        }
                    } else if a.type == float64 {
                        if b.type == int {
                            a.bool_value = a.float_value %1 xx b.int_value;
                        } else if b.type == float64 {
                            a.bool_value = a.float_value %1 b.float_value;
                        } else {
                            `return .{}, make_error("Could not `%1`", a, b);
                        }
                    } else {
                        `return .{}, make_error("Could not `%1`", a, b);
                    }
                    a.type = bool;
                jai, op);
            }
        }

        if node.op == {
            case #char "+";       insert_number_op("+");
            case #char "-";       insert_number_op("-");
            case #char "*";       insert_number_op("*");
            case #char "/";       insert_number_op("/");
            case #char "%";       insert_float_specialized_op("%", "fmod_cycling");

            case .BITWISE_AND;    insert_integer_op("&");
            case .BITWISE_OR;     insert_integer_op("|");
            case .BITWISE_XOR;    insert_integer_op("^");
            case .SHIFT_LEFT;     insert_integer_op("<<");
            case .SHIFT_RIGHT;    insert_integer_op(">>");
            case .ROTATE_LEFT;    insert_integer_op("<<<");
            case .ROTATE_RIGHT;   insert_integer_op(">>>");

            case .LOGICAL_AND;    insert_logical_op("&&");
            case .LOGICAL_OR;     insert_logical_op("||");

            case .ISEQUAL;        insert_comparison_op("==");
            case .ISNOTEQUAL;     insert_comparison_op("!=");
            case #char "<";       insert_comparison_op("<");
            case #char ">";       insert_comparison_op(">");
            case .LESSEQUALS;     insert_comparison_op("<=");
            case .GREATEREQUALS;  insert_comparison_op(">=");
        }
        return a, "";
    }

    return .{}, "Unreachable. This shouldn't happen!";
}

is_unary_operator :: (token: *Token) -> bool {
    if token.type == {
        case #char "!";      #through;
        case .BITWISE_NOT;
            return true;
        case;
            return false;
    }
}

is_binary_operator :: (token: *Token) -> bool {
    if token.type == {
        case #char "+";      #through;
        case #char "-";      #through;
        case #char "*";      #through;
        case #char "/";      #through;
        case #char "%";      #through;
        case #char "<";      #through;
        case #char ">";      #through;
        case .BITWISE_AND;   #through;
        case .BITWISE_OR;    #through;
        case .BITWISE_XOR;   #through;
        case .ISEQUAL;       #through;
        case .ISNOTEQUAL;    #through;
        case .LOGICAL_AND;   #through;
        case .LOGICAL_OR;    #through;
        case .LESSEQUALS;    #through;
        case .GREATEREQUALS; #through;
        case .SHIFT_LEFT;    #through;
        case .SHIFT_RIGHT;   #through;
        case .ROTATE_LEFT;   #through;
        case .ROTATE_RIGHT;
            return true;
        case;
            return false;
    }
}

make_error :: (msg: string, a: Reference) -> string {
    return tprint("%: %", msg, a);
}

make_error :: (msg: string, a: Reference, b: Reference) -> string {
    return tprint("%: % %", msg, a, b);
}

new_node :: () -> *Expression.Node {
    return New(Expression.Node,, temp);
}

free_node :: (node: *Expression.Node) { }

print :: (node: *Expression.Node, indent := 0) {
    for 1 .. indent  print(" ");
    if node.kind == {
        case .value;  print(node.value); print("\n");
        case .unary_operator; #through;
        case .binary_operator;
            if node.op >= 256 {
                print("%\n", node.op);
            }
            else {
                print("%\n", string.{1, cast(*u8)*node.op});
            }
    }
    if node.left   print(node.left, indent + 1);
    if node.right  print(node.right, indent + 1);
}

Expression :: struct {
    Node :: struct {
        kind : enum {
            value;
            unary_operator;
            binary_operator;
        }

        op: Token_Type;
        #place op;
        value: Reference;

        left: *Node;
        right: *Node;
    }
    root: *Node;
}

#scope_file

_precedence_table : [Token_Type.__FIRST_KEYWORD] u8;
precedence_table : [] u8;

init_precedence_table :: () {
    if precedence_table  return;
    precedence_table = _precedence_table;
    precedence : u8 = 0;
    add :: (ops: .. Token_Type) #expand {
        precedence += 1;
        for ops  precedence_table[it] = precedence;
    }
    add(.LOGICAL_OR);
    add(.LOGICAL_AND);
    add(.ISEQUAL, .ISNOTEQUAL, #char "<", #char ">", .LESSEQUALS, .GREATEREQUALS);
    add(#char "+", #char "-");
    add(#char "%");
    add(#char "*", #char "/");
    add(.BITWISE_AND, .BITWISE_OR, .BITWISE_XOR, .SHIFT_LEFT, .SHIFT_RIGHT, .ROTATE_LEFT, .ROTATE_RIGHT);
}

