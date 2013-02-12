forward
global type nv_tokstack from nonvisualobject
end type
end forward

global type nv_tokstack from nonvisualobject autoinstantiate
end type

type variables

private:

st_tok ist_values[]
long il_pos = 0

end variables

forward prototypes
public function boolean isempty ()
public subroutine reset ()
public function st_tok pop ()
public subroutine push (st_tok ast_val)
public function st_tok top ()
public function long size ()
public function st_tok peek (long al_pos)
end prototypes

public function boolean isempty ();
return il_pos = 0

end function

public subroutine reset ();
st_tok empty[]

ist_values[] = empty[]
il_pos = 0

end subroutine

public function st_tok pop ();
st_tok lst_ret

if il_pos = 0 then 
	lst_ret.value = ""
	lst_ret.kind = 0
	return lst_ret	
end if

lst_ret = ist_values[il_pos]
il_pos --

return lst_ret

end function

public subroutine push (st_tok ast_val);
il_pos++
ist_values[il_pos] = ast_val

end subroutine

public function st_tok top ();
st_tok lst_ret

if il_pos = 0 then 
	lst_ret.value = ""
	lst_ret.kind = 0
	return lst_ret	
end if

lst_ret = ist_values[il_pos]

return lst_ret

end function

public function long size ();
return il_pos

end function

public function st_tok peek (long al_pos);
// for debug : get a value

any la_val

if al_pos > 0 and al_pos <= al_pos then
	la_val = ist_values[al_pos]
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

