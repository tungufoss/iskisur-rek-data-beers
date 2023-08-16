\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{hi-beamer}[2023/08/16 Custom Beamer Class for Háskóli Íslands]
\LoadClass[compress]{beamer}

\RequirePackage{amsmath,amssymb,latexsym,graphics}
\RequirePackage[icelandic,english]{babel}
\RequirePackage[utf8]{inputenc}
\RequirePackage{t1enc}
\RequirePackage{etoolbox}
\RequirePackage{tcolorbox}
\RequirePackage{pictex}
\RequirePackage{wrapfig}
\RequirePackage{pgf,tikz,pgfplots}
\RequirePackage{hyperref}
\RequirePackage{tkz-euclide}
\RequirePackage{tikz-cd}

\pgfplotsset{compat=newest}
\beamertemplatenavigationsymbolsempty
\setbeamertemplate{footline}[frame number]


% Define custom colors
\definecolor{blue}{HTML}{10099F}
\definecolor{teal}{HTML}{2DD2C0}
\definecolor{green}{HTML}{00FFBA}
\definecolor{yellow}{HTML}{FAC55B}
\definecolor{red}{HTML}{FC8484}
\definecolor{orange}{HTML}{FFA05F}

\setbeamercolor{block title}{fg=orange}

%%%%%%%%%%%%%%%%%%%%%
%%%%% VARIABLES %%%%%
% Define the keys
\RequirePackage{keyval}
\RequirePackage{etoolbox}

\newcommand{\setNAME}[1]{\def\NAME{#1}}
\newcommand{\setTITLE}[1]{\def\TITLE{#1}}
\newcommand{\setPRETITLE}[1]{\def\PRETITLE{#1}}
\newcommand{\setSUBTITLE}[1]{\def\SUBTITLE{#1}}
\newcommand{\setFACULTY}[1]{\def\FACULTY{#1}}
\newcommand{\setSEMINAR}[1]{\def\SEMINAR{#1}}
\newcommand{\setEMAIL}[1]{\def\EMAIL{#1}}
\newcommand{\setDATE}[1]{\def\DATE{#1}}
\newcommand{\setWIDE}[1]{\settoggle{WIDE}{#1}}

\providetoggle{WIDE}
\settoggle{WIDE}{false}
\def\NAME{name}
\def\TITLE{title}
\def\PRETITLE{}
\def\SUBTITLE{}
\def\FACULTY{faculty}
\def\SEMINAR{seminar}
\def\EMAIL{name@hi.is}
\def\DATE{\today}

\define@key{hi-beamer}{NAME}{\setNAME{#1}}
\define@key{hi-beamer}{TITLE}{\setTITLE{#1}}
\define@key{hi-beamer}{PRETITLE}{\setPRETITLE{#1}}
\define@key{hi-beamer}{SUBTITLE}{\setSUBTITLE{#1}}
\define@key{hi-beamer}{FACULTY}{\setFACULTY{#1}}
\define@key{hi-beamer}{SEMINAR}{\setSEMINAR{#1}}
\define@key{hi-beamer}{EMAIL}{\setEMAIL{#1}}
\define@key{hi-beamer}{DATE}{\setDATE{#1}}
\define@key{hi-beamer}{WIDE}{\setWIDE{#1}}

\ProcessKeyvalOptions*

%%%%%%%%%%%%%%%%%%%%%%

\iftoggle{WIDE}{
    \RequirePackage[orientation=landscape,size=custom,width=16,height=9,scale=0.5,debug]{beamerposter}
}{}

\title{%
    \textbf{\large \ifdefempty{\PRETITLE}{}{\PRETITLE:~} \TITLE}
    \ifdefempty{\SUBTITLE}{}{% Check if \SUBTITLE is empty
    \\\vspace{0.05cm}{\small \SUBTITLE}}%
    \\\vspace{0.2cm}{\small \textbf{\NAME { }<\EMAIL>}}%
    \\\vspace{-0.2cm}{\scriptsize \FACULTY}%
    \\\vspace{0.1cm}{\small \SEMINAR: \DATE}
}

\date{}

\setbeamertemplate{frametitle}{\color{blue}\insertframetitle}
\setbeamerfont{institute}{size=\scriptsize,series=\mdseries,parent=structure}
\setbeamerfont{date}{size=\scriptsize,series=\mdseries,parent=structure}
\setbeamersize{text margin left=.5cm}
\setbeamersize{text margin right=.5cm}
\setbeamercolor{title}{fg=blue,bg=white}
\setbeamercolor{author}{fg=blue}
\setbeamercolor{date}{fg=blue}
\setbeamercolor{item}{fg=blue}
\setbeamercolor{block title}{fg=blue}
\hypersetup{
    colorlinks=true,
    linkcolor=blue,
    filecolor=magenta,
    urlcolor=blue,
    pdftitle={\TITLE},
    pdfpagemode=FullScreen,
}

% Redefine the titlepage
% Define initial background based on the WIDE toggle
\AtBeginDocument{%
    \iftoggle{WIDE}{
        \usebackgroundtemplate{\includegraphics[width=\paperwidth]{hi-latex/front169.png}}
    }{
        \usebackgroundtemplate{\includegraphics[width=\paperwidth]{hi-latex/front43.png}}
    }
    \begin{frame}
    \vspace{3cm}
    \pgfsetfillopacity{0.78}
    \titlepage
    \thispagestyle{empty}
    \end{frame}
    \usebackgroundtemplate{\includegraphics[width=\paperwidth]{hi-latex/background.png}}
}

\endinput
