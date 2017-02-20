{
{-# OPTIONS -w #-}
module Language.Core.Parser ( parse ) where

import Data.Ratio
import System.IO
import System.IO.Unsafe

import Language.Core.Core
import Language.Core.ParseGlue
import Language.Core.Lex
import Language.Core.Prims

}

%name parse
%tokentype { Token }

%token
 '%module'	{ TKmodule }
 '%data'	{ TKdata }
 '%newtype'	{ TKnewtype }
 '%forall'	{ TKforall }
 '%rec'		{ TKrec }
 '%let'		{ TKlet }
 '%in'		{ TKin }
 '%case'	{ TKcase }
 '%of'		{ TKof }
 '%cast'	{ TKcast }
 '%note'	{ TKnote }
 '%external'	{ TKexternal }
 '%dynexternal'	{ TKdynexternal }
 '%label'	{ TKlabel }
 '%sym'         { TKsym }
 '%trans'       { TKtrans }
 '%unsafe'      { TKunsafe }
 '%left'        { TKleft }
 '%right'       { TKright }
 '%inst'        { TKinst }
 '%_'		{ TKwild }
 '%'            { TKpercent }
 '('		{ TKoparen }
 ')'		{ TKcparen }
 '{'		{ TKobrace }
 '}'		{ TKcbrace }
 '#' 		{ TKhash}
 '='		{ TKeq }
 '::'		{ TKcoloncolon }
 '*'		{ TKstar }
 '->'		{ TKrarrow }
 '\\'		{ TKlambda}
 '@'		{ TKat }
 '.'		{ TKdot }
 ':'		{ TKcolon }
 '?'		{ TKquestion}
 ';'            { TKsemicolon }
 NAME		{ TKname $$ }
 CNAME 		{ TKcname $$ }
 INTEGER	{ TKinteger $$ }
 STRING		{ TKstring $$ }
 CHAR		{ TKchar $$ }

%monad { P } { thenP } { returnP }
%lexer { lexer } { TKEOF }

%%

module	:: { Module }
	: '%module' mname tdefs  vdefgs 
		{ Module $2 $3 $4 }

tdefs	:: { [Tdef] }
	: {- empty -}	{[]}
	| tdef ';' tdefs	{$1:$3}

tdef	:: { Tdef }
-- Empty datatypes are ok
	: '%data' qcname tbinds '=' '{' cons '}'
		{ Data $2 $3 $6 }
	| '%newtype' qcname qcname tbinds trep 
		{ Newtype $2 $3 $4 $5 }

trep    :: { Ty }
        : '=' ty        { $2 }

tbind	:: { Tbind }
	:  name { ($1,Klifted) }
	|  '(' name '::' akind ')'
		{ ($2,$4) }

tbinds 	:: { [Tbind] }
	: {- empty -}	{ [] }
	| tbind tbinds	{ $1:$2 }


vbind	:: { Vbind }
	: '(' name '::' ty')'	{ ($2,$4) }

vbinds	:: { [Vbind] }
	: {-empty -} 	{ [] }
	| vbind vbinds	{ $1:$2 }

bind	:: { Bind }
	: '@' tbind 	{ Tb $2 }
	| vbind		{ Vb $1 }

binds1 	:: { [Bind] }
	: bind		{ [$1] }
	| bind binds1	{ $1:$2 }

attbinds :: { [Tbind] }
	: {- empty -} 	{ [] }
	| '@' tbind attbinds 
			{ $2:$3 }

akind	:: { Kind }
	: '*' 		{Klifted}	
	| '#'		{Kunlifted}
	| '?'		{Kopen}
        | '(' kind ')'	{ $2 }

kind 	:: { Kind }
	: akind 	{ $1 }
	| akind '->' kind 
		{ Karrow $1 $3 }

{-
cons1	:: { [Cdef] }
	: con		{ [$1] }
	| con ';' cons1	{ $1:$3 }
-}

cons	:: { [Cdef] }
	: {- empty -} { [] }
        | con         { [$1] }
	| con ';' cons	{ $1:$3 }

con	:: { Cdef }
	: qcname attbinds atys 
		{ Constr $1 $2 $3 }

atys	:: { [Ty] }
	: {- empty -} { [] }
	| aty atys      { $1:$2 }

aty	:: { Ty }
	: name	{ Tvar $1 }
	| qcname { Tcon $1 }
	| '(' ty ')' { $2 }


bty	:: { Ty }
	: aty	{ $1 }
        | bty aty { Tapp $1 $2 }

ty	:: { Ty }
	: bty	{$1}
	| bty '->' ty 
		{ tArrow $1 $3 }
	| '%forall' tbinds '.' ty 
		{ foldr Tforall $4 $2 }
        | '%sym' ty
                { SymCoercion $2 }
        | '%trans' aty ty
                { TransCoercion $2 $3 }
        | '%unsafe' aty ty
                { UnsafeCoercion $2 $3 }
        | '%left' ty
                { LeftCoercion $2 }
        | '%right' ty
                { RightCoercion $2 }
        | '%inst' aty ty
                { InstCoercion $2 $3 }

vdefgs	:: { [Vdefg] }
	: {- empty -}	        { [] }
	| vdefg ';' vdefgs	{$1:$3 }

vdefg	:: { Vdefg }
	: '%rec' '{' vdefs1 '}'
		       { Rec $3 }
	|  vdef { Nonrec $1}

vdefs1	:: { [Vdef] }
	: vdef		{ [$1] }
	| vdef ';' vdefs1 { $1:$3 }

vdef	:: { Vdef }
	: qname '::' ty '=' exp 
		{ Vdef ($1,$3,$5) }

aexp    :: { Exp }
	: qname 	{ Var $1 }
        | qcname 	{ Dcon $1 } 
	| lit		{ Lit $1 }
	| '(' exp ')' 	{ $2 }

fexp	:: { Exp }
	: fexp aexp	{ App $1 $2 }
	| fexp '@' aty	{ Appt $1 $3 }
	| aexp		{ $1 }

exp	:: { Exp }
	: fexp		{ $1 }
	| '\\' binds1 '->' exp
		{ foldr Lam $4 $2 }
	| '%let' vdefg '%in' exp 
		{ Let $2 $4 }
	| '%case' aty aexp '%of' vbind '{' alts1 '}'
		{ Case $3 $5 $2 $7 }
-- Note: ty, not aty! You can cast something to a forall type
-- Though now we have shift/reduce conflicts like woah
	| '%cast' aexp ty
		{ Cast $2 $3 }
	| '%note' STRING exp 
		{ Note $2 $3 }
-- The NAME argument is the calling convention,
-- which, for now, is ignored (since we can only handle ccalls).
        | '%external' NAME STRING aty
                { External $3 $4 }
-- Sketchy!
        | '%label' STRING
                { External $2 tAddrzh }
-- Even more so!
        | '%dynexternal' NAME aty
                { External "[dynamic]" $3 }

alts1	:: { [Alt] }
	: alt		{ [$1] }
	| alt ';' alts1	{ $1:$3 }

alt	:: { Alt }
	: qcname attbinds vbinds '->' exp 
		{ Acon $1 $2 $3 $5 } 
	| lit '->' exp
		{ Alit $1 $3 }
	| '%_' '->' exp
		{ Adefault $3 }

lit	:: { Lit }
	: '(' INTEGER '::' aty ')'
	        { Literal (Lint $2) $4 }
	| '(' INTEGER '%' INTEGER '::' aty ')'
        	{ Literal (Lrational ($2 % $4)) $6 }
	| '(' '(' INTEGER ')' '%' INTEGER '::' aty ')'
        	{ Literal (Lrational ($3 % $6)) $8 }
	| '(' CHAR '::' aty ')'
		{ Literal (Lchar $2) $4 }
	| '(' STRING '::' aty ')'
		{ Literal (Lstring $2) $4 }

name	:: { Id }
	: NAME	{ $1 }

cname	:: { Id }
	: CNAME	{ $1 }
         
mname	:: { AnMname }
        : pkgName ':' cname
             { let (parentNames, childName) = splitModuleName $3 in
                 (M ($1, parentNames, childName)) }

pkgName :: { Pname }
        : NAME { P $1 }

{-
-- TODO: Clean this up. Now hierarchical names are z-encoded.

-- note that a sequence of mnames is either:
-- empty, or a series of cnames separated by
-- dots, with a leading dot
-- See the definition of mnames: the "name" part
-- is required.
mnames :: { [Id] } 
         : {- empty -} {[]}
         | '.' cname mnames {$2:$3}
-}

-- it sucks to have to repeat the Maybe-checking twice,
-- but otherwise we get reduce/reduce conflicts

-- TODO: this is the ambiguity here. mname '.' name --
-- but by maximal-munch, in GHC.Base.Bool the entire 
-- thing gets counted as the module name. What to do,
-- besides z-encoding the dots in the hierarchy again?
-- (Or using syntax other than a dot to separate the
-- module name from the variable name...)
qname	:: { (Mname,Id) }
        : name { (Nothing, $1) }
	| mname '.' name 
		{ (Just $1,$3) }

qcname	:: { (Mname,Id) }
        : cname { (Nothing, $1) }
        | mname '.' cname 
		{ (Just $1,$3) }


{

happyError :: P a 
happyError s l = failP (show l ++ ": Parse error\n") (take 100 s) l

}
