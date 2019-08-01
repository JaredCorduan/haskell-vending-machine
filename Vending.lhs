\documentclass[a4paper,11pt]{scrartcl}

%include polycode.fmt
%include spacing.fmt
%if style /= newcode
%subst newline = "\nextline'n"
%subst blankline = "\nextline[1ex]'n"
%endif

\usepackage{iohk}
\usepackage{mathpazo}
\usepackage{semantic}
\usepackage{graphicx}

\usepackage{float}
\floatstyle{boxed}
\restylefloat{figure}

\newcommand\named[1]{\mathsf{#1}}
\renewcommand\Varid[1]{\mathit{#1}}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}

%format lhs2TeX = "\text{\textrm{lhs}\textsf{2}\TeX}"

%if style /= newcode
%format | =  "\uniondistinct"
%format DATADEC (name) =  name =
%format DATAFIELD1 (n) (t) =  "~~"n "\in" t
%format DATAFIELDN (n) (t) =  "~~"n "\in" t
%format DATAEND =
%format LET (x) =  x
%format CHECK (pred) (error) =  pred
%format LET (x) =  x
%format RULE (env) (state1) (sig) (state2) = env "\vdash" state1 "\trans{vend}{" sig "}" state2
%format VEnv' (power) (cost) = "\left(\begin{array}{c}" power "\cr " cost "\end{array}\right)"
%format VState' (tokens) (sodas) = "\left(\begin{array}{c}" tokens "\cr " sodas "\end{array}\right)"
%format VSignal' (vend) (amnt) (sig) = (vend, amnt) = sig
%format TRANSTYPE (name) (env) (state) (sig) =  " \_ \vdash {\_} \trans{vend}{\_} {\_} \subseteq \powerset ("env" \times "state" \times "sig" \times "state")"
% TODO - Why does trans{"name"} cause errors?

%else
%format DATADEC (name) =  data name = name
%format DATAFIELD1 (n) (t) =  "  { "n" :: "t
%format DATAFIELDN (n) (t) =  "  , "n" :: "t
%format DATAEND =  "  } deriving (Eq, Show)"
%format CHECK (pred) (error) =  pred ?! error
%format LET (x) =  let x
%format RULE (env) (state1) (tx) (state2) = "return $" state2
%format VState' = VState
%format VSignal' (vend) (amnt) (sig) =  "let " VSignal vend amnt = sig
%format TRANSTYPE (name) (env) (state) (sig) =
%endif

\begin{document}

\title{Soda is a sometimes drink.}
\date{}
\maketitle
\begin{center}
\includegraphics{200px-Noun44656_vending_machine.png}
\end{center}

%if style == newcode

> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE TypeFamilies #-}
>
> module Vending where
>
>
> import Control.State.Transition
>   ( Environment
>   , PredicateFailure
>   , STS
>   , Signal
>   , State
>   , TransitionRule
>   , initialRules
>   , transitionRules
>   , TRC (TRC)
>   , judgmentContext
>   , (?!)
>   )
>
> initNumOfSodas :: Int
> initNumOfSodas = 100
>

%endif

\begin{figure}

> -- VEND Transition Types
>
> DATADEC VEnv
> DATAFIELD1 power Bool
> DATAFIELDN cost Int
> DATAEND
>
> DATADEC VState
> DATAFIELD1 tokens Int
> DATAFIELDN sodas Int
> DATAEND
>
> DATADEC VSignal
> DATAFIELD1 vend Bool
> DATAFIELDN amnt Int
> DATAEND
>
> TRANSTYPE VMACHINE VEnv VState VSignal

\caption{Vending Machine Transition Types}
\end{figure}

%if style == newcode

> data VMACHINE
> instance STS VMACHINE where
>     type Environment VMACHINE = VEnv
>     type State VMACHINE = VState
>     type Signal VMACHINE = VSignal
>
>     data PredicateFailure VMACHINE
>       = SmallDeposit
>       | OutOfSoda
>       | OutOfOrder
>       | NotPush
>       | NotDeposit
>       | NonZeroAmnt
>       deriving (Eq, Show)
>
>     initialRules = [return $ VState 0 initNumOfSodas ]
>
>     transitionRules = [vendDeposit, vendPush]
>

%endif


%if style == newcode

> vendDeposit :: TransitionRule VMACHINE
> vendDeposit = do
>   TRC (VEnv p _, VState t sd, sig) <- judgmentContext

%endif

\begin{figure}
\mathhs
\begin{equation}\label{eq:vending-rule-deposit}
\inference[vending-rule-deposit]
{%
>   VSignal' vend amnt sig
>   CHECK (vend == False) (NotDeposit)
>   CHECK (p == True) (OutOfOrder)
>
}{%
>   RULE (VEnv' p c) (VState' t sd) (sig) (VState' (t+amnt) sd)
>
}
\end{equation}

\nextdef

%if style == newcode

> vendPush :: TransitionRule VMACHINE
> vendPush = do
>   TRC (VEnv p c, VState t sd, sig) <- judgmentContext

%endif

\begin{equation}\label{eq:vending-rule-push}
\inference[vending-rule-push]
{%
>   VSignal' vend amnt sig
>   CHECK (amnt == 0) (NonZeroAmnt)
>   CHECK (p == True) (OutOfOrder)
>   CHECK (c <= t) (SmallDeposit)
>   CHECK (sd > 0) (OutOfSoda)
>
}{%
>   RULE (VEnv' p c) (VState' t sd) (sig) (VState' (t-c) (sd-1))
>
}
\end{equation}

\caption{Vending Machine Transitions}
\end{figure}

\end{document}
