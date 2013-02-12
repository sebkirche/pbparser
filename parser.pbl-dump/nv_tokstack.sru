forward
global type nv_tokstack from nonvisualobject
end type
end forward

global type nv_tokstack from nonvisualobject autoinstantiate
end type

type variables

private:

nv_tok it_values[]
long il_pos = 0

end variables
forward prototypes
public function boolean isempty ()
public subroutine reset ()
public function long size ()
public function nv_tok pop ()
public subroutine push (nv_tok at_val)
public function nv_tok top ()
public function nv_tok peek (long al_pos)
end prototypes

public function boolean isempty ();
return il_pos = 0

end function

public subroutine reset ();
nv_tok empty[]

it_values[] = empty[]
il_pos = 0

end subroutine

public function long size ();
return il_pos

end function

public function nv_tok pop ();
nv_tok lt_ret

if il_pos = 0 then 
	lt_ret.value = ""
	lt_ret.kind = 0
	return lt_ret	
end if

lt_ret = it_values[il_pos]
il_pos --

return lt_ret

end function

public subroutine push (nv_tok at_val);
il_pos++
it_values[il_pos] = at_val

end subroutine

public function nv_tok top ();
nv_tok lt_ret

if il_pos = 0 then 
	lt_ret.value = ""
	lt_ret.kind = 0
	return lt_ret	
end if

lt_ret = it_values[il_pos]

return lt_ret

end function

public function nv_tok peek (long al_pos);
// for debug : get a value

any la_val

if al_pos > 0 and al_pos <= al_pos then
	la_val = it_values[al_pos]
end if

return la_val

end function

on nv_tokstack.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nv_tokstack.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

