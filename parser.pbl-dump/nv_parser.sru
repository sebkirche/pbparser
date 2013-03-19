$PBExportHeader$nv_parser.sru
forward
global type nv_parser from nonvisualobject
end type
end forward

global type nv_parser from nonvisualobject autoinstantiate
end type

type prototypes

Function ULong GetLocaleInfo(ulong Locale, ulong LCType, ref string lpLCData, ulong cchData) Library "kernel32.dll" Alias for "GetLocaleInfoW" 
Function ULong GetSystemDefaultLCID() Library "kernel32.dll"
Function ULong GetUserDefaultLCID() Library "kernel32.dll"

end prototypes

type variables
/*
	Shunting-Yard evaluator v0.9 - (c) Sebastien KIRCHE 2011-2013

	PB implementation of Dijkstra's Shunting-Yard Algorithm
	for parsing infix math expressions and preparing a postfix 
	representation to ease the evaluation.
		
	The Shunting-Yard algorithm is described on Wikipedia:
	http://en.wikipedia.org/wiki/Shunting_yard_algorithm
	
	This implementation differs from the original Dijkstra by 
	allowing functions with variable argument number.
	It is based on the idea described by Robin in his blog:
	http://www.kallisti.net.nz/blog/2008/02/extension-to-the-shunting-yard-algorithm-to-allow-variable-numbers-of-arguments-to-functions/
	
	This code is free software.

	Usage:
	from a string containing an expression
	1) tokenize() -> "lexical" parse of the expression, 
		 identification of unary minus and terms
	2) parse() -> infix to postfix (or prefix, but the evaluation
		 only support postfixed tokens), parenthesis removal
	3) eval() -> process the expression and resolve function calls
	
	Addition of new functions:
	- Add the function name to the isFunc() method 
	- and implement the function in evalFunc() method
	
*/
public:
//associativities
constant char CC_LEFT = 'L'
constant char CC_RIGHT = 'R'

constant char DECSEP = '.'		//decimal separator
constant char ARGSEP = ','		//argument separator for funcalls
constant char LPAR = '('
constant char RPAR = ')'

private:
nv_term inv_terms[]	//array for tokenized input
nv_term ist_parsed[]	//array for parsed tokens
any ia_vars[]			//array for variables (pairs of ident-value)
string is_lasterror
string is_numbercharpattern

constant ulong LOCALE_SDECIMAL = 14 // decimal separator
string is_SYSDECSEP

end variables

forward prototypes
public function string getlasterror ()
public function boolean tokenize (string as_input)
public function boolean iswordchar (character ac_char)
public function boolean gettokens (ref nv_term at_terms[])
public function boolean getparsed (ref nv_term at_terms[])
public function boolean eval (ref nv_term a_term)
public function boolean isop (nv_term a_term)
public function boolean parse (nv_term at_terms[])
public function integer getprec (nv_term a_term)
public function boolean isbool (any a_term)
public function character getassoc (nv_term ast_op)
public function boolean evalident (ref nv_term ast_ident)
public function boolean evalop (ref nv_term ast_op)
public function boolean isfunc (string as_name)
public function string getlocaleinfo (unsignedlong al_localetype, boolean ab_userlocale)
public function string fixdecimal (string as_text)
public function boolean setvariables (any aa_vals[])
protected function boolean evalfunc (ref nv_term ast_func)
public function boolean treeifyterm (ref nv_term a_term, ref nv_termstack a_tst)
end prototypes

public function string getlasterror ();
return is_lasterror

end function

public function boolean tokenize (string as_input);
// simple tokenizer
// the produced tokens will be processed by parse()

boolean lb_ret = true, lb_isdec
long ll_tk_start = 1, ll_tk_end, ll_inplen, ll_tokens, ll_nbterm
nv_term l_term
any la_empty[]
char lc, prec, lc_quote

is_lasterror = ""
inv_terms = la_empty[]
ll_nbterm = 0
ll_inplen= len(as_input)

//we read the input string character by character
//and build tokens depending on it
do while ll_tk_start <= ll_inplen
	ll_tk_end = ll_tk_start
	
	lc = mid(as_input, ll_tk_start, 1)
	choose case lc
		case '0' to '9', DECSEP
			//we have a numeric litteral, integer or decimal
			lb_isdec = false
			do while (ll_tk_end <= ll_inplen) and match(mid(as_input, ll_tk_end, 1), is_numbercharpattern)
				if pos(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start + 1), DECSEP) > 0 then lb_isdec = true
				ll_tk_end++
			loop 
			l_term = create nv_term
			l_term.text = trim(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start))
			l_term.kind = nv_term.ATOM
			l_term.value = dec(fixdecimal(l_term.text))
			l_term.valtype = iif(lb_isdec, nv_term.DECIM, nv_term.INTG)
		case '*', '/', '^', '%', '=', '<', '>'
			//we have a binary operator, but not + or - (processed seperately)
			ll_tk_end++
			if lc = '<' or lc = '>' and ll_tk_end < ll_inplen then //lookahead ;)
				if mid(as_input, ll_tk_end, 1) = '=' &
				or mid(as_input, ll_tk_end, 1) = '>' then
					//TODO warning, can have >>
					ll_tk_end++	
				end if
			end if
			l_term = create nv_term
			l_term.text = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
			l_term.kind = nv_term.BINARYOP
		case '+', '-'
			//+ and -
			//we have to choose if unary or binary
			ll_tk_end++
			l_term = create nv_term
			l_term.text = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
			ll_tokens = upperbound(inv_terms[])
			if ll_tokens = 0 then
				l_term.kind = nv_term.UNARYOP
			elseif inv_terms[ll_tokens].valtype = nv_term.INTG &
				or inv_terms[ll_tokens].valtype = nv_term.DECIM &
				or inv_terms[ll_tokens].valtype = nv_term.STR &
				or inv_terms[ll_tokens].kind = nv_term.IDENT &
				or inv_terms[ll_tokens].kind = nv_term.TRPAR	then
				l_term.kind = nv_term.BINARYOP
			else
				l_term.kind = nv_term.UNARYOP
			end if
		case 'A' to 'Z', 'a' to 'z', '_'
			//we have an identifier, once we have the whole text
			//we choose if it is a function, operator, boolean litteral
			//or simply a variable
			do while (ll_tk_end <= ll_inplen) and isWordChar(mid(as_input, ll_tk_end, 1))
				ll_tk_end++
			loop
			l_term = create nv_term
			l_term.text = lower(trim(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)))
			if isbool(l_term.text) then
				l_term.value = iif(lower(l_term.text)="true", true, false)
				l_term.kind = nv_term.ATOM
				l_term.valtype = nv_term.BOOL
			elseif isfunc(l_term.text) then
				l_term.kind = nv_term.FUNC
				l_term.count = -1 //for now, we don't know the number of arguments
			elseif lower(l_term.text) = "and" or lower(l_term.text) = "or" or lower(l_term.text) = "xor" then
				l_term.text = lower(l_term.text)
				l_term.kind = nv_term.BINARYOP
			elseif lower(l_term.text) = "not" then
				l_term.text = lower(l_term.value)
				l_term.kind = nv_term.UNARYOP
			else
				l_term.kind = nv_term.IDENT
			end if
		case "'", '"', '`'
			//we have a quoted string
			long ll_begin, ll_cur
			lc_quote = lc
			ll_begin = ll_tk_start
			ll_cur = ll_begin + 1
			do while (ll_cur < ll_inplen) and (mid(as_input, ll_cur, 1) <> lc_quote)
				ll_cur++
			loop
			if ll_cur >= ll_inplen and (mid(as_input, ll_cur, 1) <> lc_quote) then
				is_lasterror = "unclosed string that begins at " + string(ll_begin)
				lb_ret = false
				goto exit_tokenize
			else
				ll_tk_end = ll_cur + 1
				l_term = create nv_term
				l_term.text = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
				l_term.value = mid(as_input, ll_begin + 1, ll_cur - ll_begin - 1)
				l_term.kind = nv_term.ATOM
				l_term.valtype = nv_term.STR
			end if			
		case ARGSEP, LPAR, RPAR
			//argument separator, or parenthesis
			ll_tk_end++
			l_term = create nv_term
			l_term.text = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
			choose case lc
				case ARGSEP; l_term.kind = nv_term.TARGSEP
				case LPAR; 
					/*buggy if ll_nbterm > 0 then
						if inv_terms[ll_nbterm].kind = nv_term.IDENT then
							is_lasterror = "`" + inv_terms[ll_nbterm].text + +"`is not a function at " + string(inv_terms[ll_nbterm].position)
							lb_ret = false
							goto exit_tokenize
						end if
					end if*/
					l_term.kind = nv_term.TLPAR
				case RPAR; l_term.kind = nv_term.TRPAR
			end choose
		case ' ' 
			ll_tk_start++		//ignore blanks
			continue
		case else
			//something that should not be here
			is_lasterror = "unexpected char : `" + lc +"` at " + string(ll_tk_start)
			lb_ret = false
			goto exit_tokenize
	end choose
	l_term.position = ll_tk_start
	ll_nbterm ++
	inv_terms[ll_nbterm] = l_term
	if ll_tk_end = ll_tk_start then 
		is_lasterror = "parsing error"
		lb_ret = false
		goto exit_tokenize
	end if
	prec = mid(as_input, ll_tk_end, 1)
	ll_tk_start = ll_tk_end
loop

exit_tokenize:
return lb_ret

end function

public function boolean iswordchar (character ac_char);
// tell if the given character if a word character

choose case ac_char
	case 'a' to 'z', 'A' to 'Z', '0' to '9', '_'
		return true

	case else
		return false
end choose

end function

public function boolean gettokens (ref nv_term at_terms[]);
// helper for the tokens read from the input

at_terms[] = inv_terms[]

return upperbound(inv_terms[]) > 0


end function

public function boolean getparsed (ref nv_term at_terms[]);
// helper for the parsed tokens

at_terms[] = ist_parsed[]

return true

end function

public function boolean eval (ref nv_term a_term);
/*
	Evaluation of the token tree after parsing

	the evaluator crawls recursively the tree
	and evaluates the nodes
*/

boolean lb_res
string ls_ret = ""

choose case a_term.kind
	case nv_term.ATOM
		//atom is already evaluated
		return true
	case nv_term.IDENT
		evalident(a_term)
	case nv_term.UNARYOP, nv_term.BINARYOP
		evalop(a_term)
	case nv_term.FUNC
		evalfunc(a_term)
end choose

if a_term.valtype <> nv_term.INTG and a_term.valtype <> nv_term.DECIM and a_term.valtype <> nv_term.BOOL and a_term.valtype <> nv_term.STR then
//	if a_term.valtype = nv_term.INTG or a_term.valtype = nv_term.DECIM then
//		ls_ret = string(a_term.value)
//	elseif a_term.valtype = nv_term.BOOL then
//		ls_ret = iif(a_term.value, 'True', 'False')
//	elseif a_term.valtype = nv_term.STR then
//		ls_ret = '"' + a_term.value + '"'
//	else
//	if a_term.valtype <> nv_term.ERR then
//		a_term.value = "not sure how to process " + a_term.typename() + " `" + string(a_term.text) + "`"
//	end if
//	end if
//else
//	ls_ret = "evaluation failed"
	if a_term.valtype = nv_term.ERR then lb_res = false
else
	lb_res = true
end if

return lb_res

end function

public function boolean isop (nv_term a_term);
// tell if the given text is an operator

/*
boolean lb_ret 

choose case a_term.value
	case '+', '-', '*', '/', '^'
		lb_ret = true
		
	case else
		lb_ret = false
end choose

return lb_ret
*/

return (a_term.kind = nv_term.BINARYOP) or (a_term.kind = nv_term.UNARYOP)

end function

public function boolean parse (nv_term at_terms[]);
// parse the infix input token stream and transform it into prefix or postfix

boolean lb_ret = true
nv_term l_term, l_func, l_tmp, empty_toks[]
nv_termstack ltst_ast	//a stack that will become our final AST
nv_termstack ltst_op	//intermediate operators/funcs stack
nv_stack lst_argscount	//arguments counter
nv_stack lst_has_args	//arguments presence
long ll_count

is_lasterror = ""

if upperbound(at_terms[]) < 1 then
	is_lasterror = "no input to parse"
	lb_ret = false
	goto end_of_shunt
end if

long t = 1
do while t <= upperbound(at_terms[])
	l_term = at_terms[t]

	if l_term.kind = nv_term.ATOM or l_term.kind = nv_term.IDENT then
		//if the token is an operand, pass it to the output queue
		ltst_ast.push(l_term)
		if lst_has_args.size() > 0 then lst_has_args.settop(true)
	elseif l_term.kind = nv_term.FUNC then
		//if it is a function, push it to the stack
		ltst_op.push(l_term)
		lst_argscount.push(0)
		if lst_has_args.size() > 0 then lst_has_args.settop(true)
		lst_has_args.push(false)
	elseif l_term.kind = nv_term.TARGSEP then
		//if it is a function argument separator
		//1) pop all pending operators until getting '('
		do while not ltst_op.isempty() 
			if ltst_op.top().text <> LPAR then
				l_tmp = ltst_op.pop()
				treeifyterm(ref l_tmp, ref ltst_ast)
				ltst_ast.push(l_tmp)
			else
				exit //while
			end if
		loop
		if not ltst_op.isempty() or (ltst_op.top().text = LPAR) then
			//workaround pbscript if not lazy
			goto no_paren_error
		else
			//heuristic: if stack is empty or we did not get a '('
			//there is an expression error
			is_lasterror = "bad argument separator or parenthesis mismatch"
			lb_ret = false
			goto end_of_shunt
		end if
		no_paren_error:
		//2) remember arg count
		if not lst_has_args.isempty() then	//don't crash when an ident is not a func
			if lst_has_args.pop() = true then
				ll_count = lst_argscount.top()
				lst_argscount.settop(ll_count + 1)
			end if
		end if
		lst_has_args.push(false)
	elseif isop(l_term) then
		//we have an operator, process operator precedence
		//if there are other pending operations
		do while not ltst_op.isempty() 
			if ((getassoc(l_term) = cc_left and getprec(l_term) <= getprec(ltst_op.top())) &
			or &
			(getassoc(l_term) = cc_right and getprec(l_term) < getprec(ltst_op.top()))) then
				l_tmp = ltst_op.pop()
				treeifyterm(ref l_tmp, ref ltst_ast)
				ltst_ast.push(l_tmp)
			else
				exit //while
			end if
		loop
		ltst_op.push(l_term)
	elseif l_term.text = LPAR then
		//push a '(' to the stack
		ltst_op.push(l_term)
	elseif l_term.text = RPAR then
		//process all pending operations up to '('
		do while not ltst_op.isempty() 
			if ltst_op.top().text <> LPAR then
				l_tmp = ltst_op.pop()
				treeifyterm(ref l_tmp, ref ltst_ast)
				ltst_ast.push(l_tmp)
			else
				exit //while
			end if
		loop
		//just pop the '('
		if ltst_op.top().text = LPAR then
			destroy ltst_op.top()
			ltst_op.pop()
		else
			//we might have a parenthesis mismatch
			is_lasterror = "mismatch parenthesis"
			lb_ret = false
			goto end_of_shunt
		end if
		if not ltst_op.isempty() then
			if ltst_op.top().kind = nv_term.FUNC then 
				//if there is a function after the paren
				//add it to the queue with arguments count
				ll_count = lst_argscount.pop()
				if lst_has_args.pop() then ll_count ++
				l_func = ltst_op.pop()
				l_func.count = ll_count
				treeifyterm(ref l_func, ref ltst_ast)
				ltst_ast.push(l_func)
			/*buggy else
				is_lasterror = '`' + ltst_op.top().text + '` is not a function'
				lb_ret = false
				goto end_of_shunt*/
			end if
		else
			is_lasterror = "parenthesis error"
			lb_ret = false
			goto end_of_shunt
		end if
	//else what to do ?
	end if
	t++
loop
do while not ltst_op.isempty()
	//add the final pending operators to the queue
	if ltst_op.top().text = LPAR or ltst_op.top().text = RPAR then
		//if there are still parenthesis on the stack, we have a problem
		is_lasterror = "mismatch parenthesis"
		lb_ret = false
		goto end_of_shunt
	end if
	l_tmp = ltst_op.pop()
	treeifyterm(ref l_tmp, ref ltst_ast)
	ltst_ast.push(l_tmp)
loop

//put the queue into an array, in postfix or prefix order
ist_parsed[] = empty_toks[]
do while not ltst_ast.isempty()
	ist_parsed[upperbound(ist_parsed[]) + 1] = ltst_ast.pop()
loop

end_of_shunt:
return lb_ret

end function

public function integer getprec (nv_term a_term);
// return the precedence for an operator

int li_ret

if a_term.kind <> nv_term.UNARYOP and a_term.kind <> nv_term.BINARYOP then return 0

choose case a_term.text
	case '^';					 li_ret = 8
	case '-'
		//special minus case
		if a_term.kind = nv_term.UNARYOP then 
			li_ret = 7
		else
			li_ret = 5
		end if
	case '*', '/', '%';			li_ret = 6
	case '+' /*- above*/;		li_ret = 5
	case '<', '<=', '>', '>=';	li_ret = 4
	case '=', '<>';				li_ret = 3
	case 'not';						li_ret = 2
	case 'and', 'or', 'xor';	li_ret = 1
	case else;						li_ret = 0
end choose

return li_ret

end function

public function boolean isbool (any a_term);
//check if the given identifier is a boolean literal

boolean lb_ret = false

choose case lower(a_term)
	case 'true', "false"
		lb_ret = true
end choose

return lb_ret

end function

public function character getassoc (nv_term ast_op);
// return the associativity for an operator

char lc_ret

choose case ast_op.text
	case '^', 'not'
		lc_ret = CC_RIGHT
	case '*', '/', '%'
		lc_ret = CC_LEFT
	case '+', '-', 'and', 'or', 'xor'
		if ast_op.kind = nv_term.UNARYOP then
			lc_ret = CC_RIGHT
		else
			lc_ret = CC_LEFT
		end if
	case else
		lc_ret = CC_LEFT
end choose

return lc_ret

end function

public function boolean evalident (ref nv_term ast_ident);
// evaluate the value of a function
nv_term item
dec ldc_ret = 0, ldc_val
long i, max

//get the type of the identifier, then set the node value

choose case lower(ast_ident.text)
	case "pi"	//dummy function
		ast_ident.valtype = nv_term.DECIM
		ast_ident.value = 3.14
	case else
		max = upperbound(ia_vars[]) - 1
		for i = 1 to max step 2
			if ast_ident.text = ia_vars[i] then
			item = ia_vars[i+1]
				ast_ident.valtype = item.valtype
			ast_ident.value = item.value
				return true
			end if
		next
		ast_ident.valtype = nv_term.ERR
		is_lasterror = "cannot resolve `" + ast_ident.text + "`"
		ast_ident.value = is_lasterror
end choose

return false

end function

public function boolean evalop (ref nv_term ast_op);
// evaluate the value of an operator

boolean lb_ret = true, lb_val
nv_term st_op1, st_op2
dec ldc_op1, ldc_op2, ldc_res
any la_op1, la_op2, la_res
long i

//todo not evaluate all childs for and / or operators (lazy eval)
for i = 1 to upperbound(ast_op.childs[])
	if not eval(ast_op.childs[i]) then 
		ast_op.valtype = nv_term.ERR
		ast_op.value = is_lasterror
		goto end_eval
	end if
next

choose case ast_op.kind
	case nv_term.UNARYOP
	choose case ast_op.text
		case '-', '+'
		st_op1 = ast_op.childs[1]
		if st_op1.valtype <> nv_term.DECIM and st_op1.valtype <> nv_term.INTG then
			ast_op.valtype = nv_term.ERR
			ast_op.value = "`" + ast_op.text + "` cannot handle " + st_op1.typename() + " `" + string(st_op1.text) + "` at " + string(ast_op.position)
			goto end_eval
		end if
		ldc_op1 = dec(st_op1.value)
		if ast_op.text = '-' then
			ldc_res = - ldc_op1
		elseif ast_op.text = '+' then
			ldc_res = ldc_op1
		end if
		ast_op.valtype = nv_term.DECIM
		ast_op.value = ldc_res
		case 'not'
		st_op1 = ast_op.childs[1]
		//TODO pourra être traité par un nv_term.tobool
		if st_op1.valtype <> nv_term.BOOL then
			ast_op.valtype = nv_term.ERR
			ast_op.value = "`" + ast_op.text + "` cannot handle " + st_op1.typename() + " `" + string(st_op1.value) + "` at " + string(ast_op.position)
			goto end_eval
		end if
		ast_op.valtype = nv_term.BOOL
		ast_op.value = not(st_op1.value)
	end choose
	case nv_term.BINARYOP
		if ast_op.count = 2 then
			//TODO, assume this is 2 numerical tokens
			//need to improve type checking / inference
			choose case ast_op.text
				case '+', '-', '*', '/', '%', '^'
					if not ast_op.childs[1].iscompatiblewith(ast_op.childs[2]) then
						//Coercion to first operand
						if ast_op.childs[1].valtype = nv_term.STR then
							if ast_op.text <> '+' then
								ast_op.valtype = nv_term.ERR
								ast_op.value = "eval failed: mismatch operands (" + ast_op.childs[1].typename() + '/' + ast_op.childs[2].typename() + ") for `" + ast_op.text + "` at " + string(ast_op.position)
								goto end_eval
							else
								la_op2 = string(ast_op.childs[2].value)
								ast_op.valtype = nv_term.STR
							end if
						elseif ast_op.childs[1].valtype = nv_term.DECIM or ast_op.childs[1].valtype = nv_term.INTG then
							//if match(ast_op.childs[2].value, is_numbercharpattern) then
							if match(string(ast_op.childs[2].value), is_numbercharpattern) then
								la_op2 = dec(fixdecimal(ast_op.childs[2].value))
								ast_op.valtype = nv_term.DECIM
							else
								ast_op.valtype = nv_term.ERR
								ast_op.value = "eval failed: cannot convert `" + ast_op.childs[2].value + "` to numeric for `" + ast_op.text + "` at " + string(ast_op.position)
								goto end_eval
							end if
						end if
						la_op1 = ast_op.childs[1].value
					else
						//same operand types
						ast_op.valtype = ast_op.childs[1].valtype
						la_op2 = ast_op.childs[2].value
						la_op1 = ast_op.childs[1].value
					end if
					choose case ast_op.text
						case '+'
							la_res = la_op1 + la_op2
						case '-'
							la_res = la_op1 - la_op2
						case '*'
							la_res = la_op1 * la_op2
						case '/'
							la_res = la_op1 / la_op2
						case '%'
							la_res = mod(la_op1, la_op2)
						case '^'
							la_res = la_op1 ^ la_op2
					end choose
					ast_op.value = la_res
				case '<', '<=', '>', '>='
					la_op2 = ast_op.childs[2].value
					la_op1 = ast_op.childs[1].value
					choose case ast_op.text
						case '<'
							lb_val = la_op1 < la_op2
						case '<=', '=<'
							lb_val = la_op1 <= la_op2
						case '>'
							lb_val = la_op1 > la_op2
						case '>=', '=>'
							lb_val = la_op1 >= la_op2
					end choose
					ast_op.valtype = nv_term.BOOL
					ast_op.value = lb_val
				case '=', '<>', 'and', 'or', 'xor'
					st_op2 = ast_op.childs[2]
					st_op1 = ast_op.childs[1]
					if st_op1.valtype <> st_op2.valtype then
						ast_op.valtype = nv_term.ERR
						ast_op.value = "eval failed: mismatch operands (" + st_op1.typename() + '/' + st_op2.typename() + ") for `" + ast_op.text + "` at " + string(ast_op.position)
						goto end_eval
					end if
					//TODO pourra être traité par un nv_term.tobool
					if (ast_op.text = 'and' or ast_op.text = 'or' or ast_op.text = 'xor') and st_op1.valtype <> nv_term.BOOL then
						ast_op.valtype = nv_term.ERR
						ast_op.value = "`" + ast_op.text + "` cannot handle `" + st_op1.typename() + "` at " + string(ast_op.position)
						goto end_eval
					end if
					choose case ast_op.text
						case '='
							lb_val = (st_op1.value = st_op2.value)
						case '<>'
							lb_val = (st_op1.value <> st_op2.value)
						case 'and'
							lb_val = (st_op1.value and st_op2.value)
						case 'or'
							lb_val = (st_op1.value or st_op2.value)
						case 'xor'
							lb_val = (st_op1.value and not st_op2.value) or (not st_op1.value and st_op2.value)
					end choose
					ast_op.valtype = nv_term.BOOL
					ast_op.value = lb_val
				case else
					ast_op.valtype = nv_term.ERR
					ast_op.value = "unknown op " + ast_op.text
					goto end_eval
			end choose
		else
			ast_op.valtype = nv_term.ERR
			ast_op.value = "broken expression : wrong number of operands for `" + ast_op.text + "` at " + string(ast_op.position)
			goto end_eval
		end if
	case else
	is_lasterror = "cannot evaluate op `" + ast_op.value + "`"
	ast_op.valtype = nv_term.ERR
	ast_op.value = is_lasterror
end choose

end_eval:

return lb_ret
end function

public function boolean isfunc (string as_name);
//check if the given identifier is a function

boolean lb_ret = false

choose case lower(as_name)
	case "if", "sum", "mul", "abs", "min", "max", "len", "msgbox" /*, "not"*/
		lb_ret = true
end choose

return lb_ret

end function

public function string getlocaleinfo (unsignedlong al_localetype, boolean ab_userlocale);// returns the LOCALE setting of the given type
//
// al_localetype = the local we want to get
// ab_userlocale = TRUE - use the user locales /	FALSE - use the system locales


ulong ll_cid
ulong ll_len
string s_ret = "", s_tmp = ""

if ab_userlocale then
	ll_cid = getuserdefaultlcid( )
else
	ll_cid = getsystemdefaultlcid( )
end if

// first call to get the correct length for the data to return
ll_len = getlocaleinfo(ll_cid, al_localetype, s_tmp , len(s_tmp))

if ll_len > 0 then
	s_tmp = space(ll_len)
	
	//second call to get the data
	ll_len = getlocaleinfo(ll_cid, al_localetype, s_tmp , len(s_tmp))
	if ll_len > 0 then s_ret = left(s_tmp, ll_len -1) // the returned data includes the string terminator
end if

return s_ret

end function

public function string fixdecimal (string as_text);
// adapt a decimal value to the format suitable for the system
// because dec() use the decimal separator locales setting to be able to get decimals
string ls_rep
long p

if is_SYSDECSEP = ',' then 
	ls_rep = '.'
else
	ls_rep = ','
end if

p = pos(as_text, ls_rep)
do while p > 0 
	as_text = replace(as_text, p, 1, is_SYSDECSEP)
	p = pos(as_text, ls_rep)
loop

return as_text

end function

public function boolean setvariables (any aa_vals[]);
//populate the variables
// aa_vals[] must be an even sized array of key-value pairs

boolean lb_ret = true
int i, max, n
nv_term l_term

max = upperbound(aa_vals[])
if mod(max, 2) <> 0 then return false


n = 1
for i = 1 to max step 2
	if not isbool(aa_vals[i]) &
		and not isfunc(aa_vals[i]) /*TODO : and not isOP*/ then

		l_term = create nv_term
	l_term.kind = nv_term.ATOM
		choose case classname(aa_vals[i+1])
			case "boolean"
				l_term.valtype = nv_term.BOOL
				l_term.value = aa_vals[i+1] //iif(lower(string(aa_vals[i+1])) = "true", true, false)
			case "integer"
				l_term.valtype = nv_term.INTG
				l_term.value = aa_vals[i+1]
			case "decimal"
				l_term.valtype = nv_term.DECIM
				l_term.value = aa_vals[i+1]
			case "string"
				if left(aa_vals[i+1],1) = right(aa_vals[i+1],1) then
					l_term.valtype = nv_term.STR
					l_term.value = mid(aa_vals[i+1], 2, len(string(aa_vals[i+1])) - 2)
				end if
		end choose
		ia_vars[n+1] = l_term
		ia_vars[n] = lower(aa_vals[i])
		n += 2
	end if
next

return true

end function

protected function boolean evalfunc (ref nv_term ast_func);
// evaluate the value of a function

dec ldc_ret = 0, ldc_val
long i, nb

if ast_func.text <> "if" then
	for i = 1 to upperbound(ast_func.childs[])
		if not eval(ast_func.childs[i]) then goto in_case_of_err
	next
end if

choose case ast_func.text
	case "sum"
		for i = 1 to ast_func.count
			ldc_val = dec(ast_func.childs[i].value)
			ldc_ret += ldc_val
		next
		ast_func.valtype = nv_term.DECIM
		ast_func.value = ldc_ret
	case "mul"
		if ast_func.count > 0 then ldc_ret = dec(ast_func.childs[1].value)
		for i = 2 to ast_func.count
			ldc_val = dec(ast_func.childs[i].value)
			ldc_ret *= ldc_val
		next
		ast_func.valtype = nv_term.DECIM
		ast_func.value = ldc_ret
	case "abs"
		if ast_func.count = 1 then 
			ldc_ret = abs(dec(ast_func.childs[i].value))
			ast_func.valtype = nv_term.DECIM
			ast_func.value = ldc_ret
		else
			is_lasterror = "abs() needs 1 argument"
			ast_func.valtype = nv_term.ERR
			ast_func.value = is_lasterror
		end if
	case "min"
		if ast_func.count > 0 then 
			ldc_ret = dec(ast_func.childs[1].value)
			for i = 2 to ast_func.count
				ldc_val = dec(ast_func.childs[i].value)
				if ldc_val < ldc_ret then ldc_ret = ldc_val
			next
			ast_func.valtype = nv_term.DECIM
			ast_func.value = ldc_ret
		else
			is_lasterror = "missing argument"
			ast_func.valtype = nv_term.ERR
			ast_func.value = is_lasterror
		end if
	case "max"
		if ast_func.count > 0 then 
			ldc_ret = dec(ast_func.childs[1].value)
			for i = 2 to ast_func.count
				ldc_val = dec(ast_func.childs[i].value)
				if ldc_val > ldc_ret then ldc_ret = ldc_val
			next
			ast_func.valtype = nv_term.DECIM
			ast_func.value = ldc_ret
		else
			is_lasterror = "missing argument"
			ast_func.valtype = nv_term.ERR
			ast_func.value = is_lasterror
		end if
	case "len"
		if ast_func.count = 1 then 
			if ast_func.childs[1].valtype = nv_term.STR then
				nb = len(string(ast_func.childs[1].value))
				ast_func.valtype = nv_term.INTG
				ast_func.value = nb
			else
				is_lasterror = "len() can only take string argument, given " + ast_func.childs[1].valuetype()
				ast_func.valtype = nv_term.ERR
				ast_func.value = is_lasterror
			end if
		else
			is_lasterror = "len() needs 1 argument"
			ast_func.valtype = nv_term.ERR
			ast_func.value = is_lasterror
		end if
	case "msgbox"
		if ast_func.count = 1 then
			if ast_func.childs[1].valtype = nv_term.STR then
				nb = MessageBox("Evaluator", string(ast_func.childs[1].value))
				ast_func.valtype = nv_term.INTG
				ast_func.value = nb
			else
				is_lasterror = "msgbox() can only take string argument, given " + ast_func.childs[1].typename()
				ast_func.valtype = nv_term.ERR
				ast_func.value = is_lasterror
			end if
		else
			is_lasterror = "msgbox() needs 1 argument"
			ast_func.valtype = nv_term.ERR
			ast_func.value = is_lasterror
		end if
	case "if"
		if ast_func.count = 3 then
			if eval(ast_func.childs[1]) then	//eval the condition
				if ast_func.childs[1].valtype = nv_term.BOOL then
					if ast_func.childs[1].value = true then
						eval(ast_func.childs[2])
						ast_func.valtype = ast_func.childs[2].valtype
						ast_func.value = ast_func.childs[2].value
					else
						eval(ast_func.childs[3])
						ast_func.valtype = ast_func.childs[3].valtype
						ast_func.value = ast_func.childs[3].value
					end if
				else
					is_lasterror = "cond. expression returned " + ast_func.childs[1].typename() + " instead of boolean for if()"
					ast_func.valtype = nv_term.ERR
					ast_func.value = is_lasterror
				end if
			end if
		else
			is_lasterror = "if() needs 3 arguments"
			ast_func.valtype = nv_term.ERR
			ast_func.value = is_lasterror
		end if
	case else
		is_lasterror = "cannot evaluate function `" + ast_func.text + "`"
		ast_func.valtype = nv_term.ERR
		ast_func.value = is_lasterror
end choose

in_case_of_err:
if ast_func.valtype = nv_term.ERR then 
	string ls_tmp
	ls_tmp = ast_func.value
	ls_tmp += " at " + string(ast_func.position)
	ast_func.value = ls_tmp
	is_lasterror = ls_tmp
end if

return false
end function

public function boolean treeifyterm (ref nv_term a_term, ref nv_termstack a_tst);
//pop the necessary items from a term stack
//and add them to the term childs

int i, nbr
nv_term child

choose case a_term.kind
	case nv_term.UNARYOP;	 nbr = 1
	case nv_term.BINARYOP;	 nbr = 2
	case nv_term.FUNC;		nbr = a_term.count
	case else;				nbr = 1
end choose

if nbr > a_tst.size() then return false

a_term.count = nbr
for i = 1 to nbr
	child = a_tst.pop()
	child.root = a_term
	a_term.childs[nbr - (i - 1)] = child
next

return true

end function

on nv_parser.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nv_parser.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event constructor;
is_SYSDECSEP = getlocaleinfo(locale_sdecimal, true)
is_numbercharpattern = "[0-9" + iif(DECSEP = '.', "\.", DECSEP) + "]"

end event

