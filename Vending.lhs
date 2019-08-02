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
%format RULEVMACHINE (env) (state1) (sig) (state2) = env "\vdash" state1 "\trans{vend}{" sig "}" state2
%format RULEDEDUCT (env) (state1) (sig) (state2) = env "\vdash" state1 "\trans{deduct}{" sig "}" state2
% TODO - Why does trans{"name"} cause errors?
%format SUBRULE (rule) (env) (state1) (sig) (state2) =  env "\vdash" state1 "\trans{deduct}{" sig "}" state2
%format VEnv' (power) (cost) = "\left(\begin{array}{c}" power "\cr " cost "\end{array}\right)"
%format DState' (tokens) (sodas) = "\left(\begin{array}{c}" tokens "\cr " sodas "\end{array}\right)"
%format VSignal' (vend) (amnt) (sig) = (vend, amnt) = sig
%format DEnv' (cost) = cost

%else
%format DATADEC (name) =  data name = name
%format DATAFIELD1 (n) (t) =  "  { "n" :: "t
%format DATAFIELDN (n) (t) =  "  , "n" :: "t
%format DATAEND =  "  } deriving (Eq, Show)"
%format CHECK (pred) (error) =  pred ?! error
%format LET (x) =  let x
%format RULEVMACHINE (env) (state1) (tx) (state2) = "return $" state2
%format RULEDEDUCT (env) (state1) (tx) (state2) = "return $" state2
%format SUBRULE (rule) (env) (state1) (sig) (state2) =  state2" <- trans @"rule" $ TRC "(env, state1, sig)
%format VState' = VState
%format VSignal' (vend) (amnt) (sig) =  "let " VSignal vend amnt = sig
%format DEnv' = "DEnv "
%format DState' (tokens) (sodas) =  DState " " tokens " " sodas
%endif

\begin{document}

\title{Soda is a sometimes drink.}
\date{}
\maketitle
\begin{center}
\includegraphics{200px-Noun44656_vending_machine.png}
\end{center}

%if style == newcode

> {-# LANGUAGE EmptyDataDecls #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeApplications #-}
> {-# LANGUAGE TypeFamilies #-}
>
> module Vending where
>
>
> import Control.State.Transition
>

%endif

\begin{figure}

> -- DEDUCT Transition Types
>
> DATADEC DEnv
> DATAFIELD1 _dcost Int
> DATAEND
>
> DATADEC DState
> DATAFIELD1 _tokens Int
> DATAFIELDN _sodas Int
> DATAEND
>

$$
\_ \vdash {\_} \trans{deduct}{} {\_}
\subseteq
\powerset (\Varid{DEnv} \times \Varid{DState} \times \Varid{DState})
$$

\caption{Vending Machine Transition Types}
\end{figure}

%if style == newcode

> data DEDUCT
> instance STS DEDUCT where
>     type Environment DEDUCT = DEnv
>     type State DEDUCT = DState
>     type Signal DEDUCT = ()
>
>     data PredicateFailure DEDUCT
>       = SmallDeposit
>       | OutOfSoda
>       deriving (Eq, Show)
>
>     initialRules = []
>     transitionRules = [deduct]
>
> deduct :: TransitionRule DEDUCT
> deduct = do
>   TRC (DEnv c, DState t sd, _) <- judgmentContext

%endif

\begin{figure}
\mathhs
\begin{equation}\label{eq:deduct-rule}
\inference[deduct-rule]
{%
>   CHECK (c  <= t) (SmallDeposit)
>   CHECK (sd  > 0) (OutOfSoda)
>
}{%
>   RULEVMACHINE (DEnv' c) (DState' t sd) (()) (DState' ((t-c)) ((sd-1)))
>
}
\end{equation}

\caption{Deduct Transitions}
\end{figure}

%if style == newcode

> initNumOfSodas :: Int
> initNumOfSodas = 100
>

%endif

\begin{figure}

> -- VEND Transition Types
>
> DATADEC VEnv
> DATAFIELD1 _power Bool
> DATAFIELDN _cost Int
> DATAEND
>
> DATADEC VSignal
> DATAFIELD1 _vend Bool
> DATAFIELDN _amnt Int
> DATAEND
>

$$
\_ \vdash {\_} \trans{vend}{\_} {\_}
\subseteq
\powerset (\Varid{VEnv} \times \Varid{DState} \times \Varid{VSignal} \times \Varid{DState})
$$
\caption{Vending Machine Transition Types}
\end{figure}

%if style == newcode

> data VMACHINE
> instance STS VMACHINE where
>     type Environment VMACHINE = VEnv
>     type State VMACHINE = DState
>     type Signal VMACHINE = VSignal
>
>     data PredicateFailure VMACHINE
>       = OutOfOrder
>       | NotPush
>       | NotDeposit
>       | NonZeroAmnt
>       | DeductFailure (PredicateFailure DEDUCT)
>       deriving (Eq, Show)
>
>     initialRules = [return $ DState 0 initNumOfSodas ]
>
>     transitionRules = [vendDeposit, vendPush]
>
> instance Embed DEDUCT VMACHINE where
>   wrapFailed = DeductFailure

%endif


%if style == newcode

> vendDeposit :: TransitionRule VMACHINE
> vendDeposit = do
>   TRC (VEnv p _, DState t sd, sig) <- judgmentContext

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
>   RULEVMACHINE (VEnv' p c) (DState' t sd) (sig) (DState' ((t+amnt)) (sd))
>
}
\end{equation}

\nextdef

%if style == newcode

> vendPush :: TransitionRule VMACHINE
> vendPush = do
>   TRC (VEnv p c, DState t sd, sig) <- judgmentContext

%endif

\begin{equation}\label{eq:vending-rule-push}
\inference[vending-rule-push]
{%
>   VSignal' vend amnt sig
>   CHECK (amnt == 0) (NonZeroAmnt)
>   CHECK (p == True) (OutOfOrder)
>   SUBRULE DEDUCT (DEnv' c) (DState' t sd) (()) (DState' t' sd')
>
}{%
>   RULEVMACHINE (VEnv' p c) (DState' t sd) (sig) (DState' t' sd')
>
}
\end{equation}

\caption{Vending Machine Transitions}
\end{figure}

\end{document}
