\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{hi-beamer}[2023/08/16 Custom Beamer Class for Háskóli Íslands]
\LoadClass[compress,handout]{beamer}

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

%\setbeamertemplate{blocks}[rounded][shadow=true]
\setbeamercolor{block title}{bg=teal,fg=blue} % Teal background and blue text for the title
\setbeamercolor{block body}{bg=teal!50!white,fg=black} % Muted teal background and black text for the body
\setbeamercolor{block title alerted}{bg=red,fg=blue}
\setbeamercolor{block body alerted}{bg=red!50!white,fg=black}
\setbeamercolor{block title example}{bg=orange,fg=blue}
\setbeamercolor{block body example}{bg=orange!50!white,fg=black}

\setbeamercolor{itemize item}{fg=blue}

%%%%%%%%%%%%%%%%%%%%%
%%%%% VARIABLES %%%%%
% Define the keys
\RequirePackage{keyval}
\RequirePackage{etoolbox}

\newcommand{\setNAME}[1]{\def\NAME{#1}}
\newcommand{\setTITLE}[1]{\def\TITLE{#1}}
\newcommand{\setSUBTITLE}[1]{\def\SUBTITLE{#1}}
\newcommand{\setFACULTY}[1]{\def\FACULTY{#1}}
\newcommand{\setSEMINAR}[1]{\def\SEMINAR{#1}}
\newcommand{\setEMAIL}[1]{\def\EMAIL{#1}}
\newcommand{\setDATE}[1]{\def\DATE{#1}}
\newcommand{\setWIDE}[1]{\settoggle{WIDE}{#1}}
\newcommand{\setICELANDIC}[1]{\settoggle{ICELANDIC}{#1}}

\providetoggle{ICELANDIC}
\settoggle{ICELANDIC}{false}
\providetoggle{WIDE}
\settoggle{WIDE}{false}
\def\NAME{name}
\def\TITLE{title}
\def\SUBTITLE{}
\def\FACULTY{faculty}
\def\SEMINAR{seminar}
\def\EMAIL{name@hi.is}
\def\DATE{\today}

\define@key{hi-beamer}{NAME}{\setNAME{#1}}
\define@key{hi-beamer}{TITLE}{\setTITLE{#1}}
\define@key{hi-beamer}{SUBTITLE}{\setSUBTITLE{#1}}
\define@key{hi-beamer}{FACULTY}{\setFACULTY{#1}}
\define@key{hi-beamer}{SEMINAR}{\setSEMINAR{#1}}
\define@key{hi-beamer}{EMAIL}{\setEMAIL{#1}}
\define@key{hi-beamer}{DATE}{\setDATE{#1}}
\define@key{hi-beamer}{WIDE}{\setWIDE{#1}}
\define@key{hi-beamer}{ICELANDIC}{\setICELANDIC{#1}}

\ProcessKeyvalOptions*

%%%%%%%%%%%%%%%%%%%%%%

\iftoggle{WIDE}{
    \RequirePackage[orientation=landscape,size=custom,width=16,height=9,scale=0.5,debug]{beamerposter}
}{}

\title{\TITLE}
\date{\DATE}

\newcommand{\hilogo}{
    \iftoggle{ICELANDIC}
    {hi-latex/hi-is}
    {hi-latex/hi-en}
}


\setbeamertemplate{frametitle}{\color{blue}\insertframetitle}
\setbeamerfont{institute}{size=\scriptsize,series=\mdseries,parent=structure}
\setbeamerfont{date}{size=\scriptsize,series=\mdseries,parent=structure}
\setbeamersize{text margin left=.5cm}
\setbeamersize{text margin right=.5cm}
\setbeamercolor{title}{fg=blue,bg=white}
\setbeamercolor{author}{fg=blue}
\setbeamercolor{date}{fg=blue}
\setbeamercolor{item}{fg=blue}

\hypersetup{
    colorlinks=true,
    linkcolor=blue,
    filecolor=magenta,
    urlcolor=blue,
    pdftitle={\TITLE},
    pdfpagemode=FullScreen,
}

% Redefine the titlepage
\setbeamertemplate{title page}{
\vspace{3cm}
\colorbox{white}{
        \begin{minipage}{0.6\textwidth}
            \vspace*{0.1cm} % Adjust this for vertical padding
            \color{blue}\textbf{\large\inserttitle}
            \vspace*{0.1cm} % Adjust this for vertical padding
        \end{minipage}
}

\ifdefempty{\SUBTITLE}{}{% Check if \SUBTITLE is empty
\vspace{0.1cm}
\colorbox{white}{
        \begin{minipage}{0.85\textwidth}
            \vspace*{0.1cm} % Adjust this for vertical padding
            \color{blue}\SUBTITLE
            \vspace*{0.1cm} % Adjust this for vertical padding
        \end{minipage}
}}

\vfill
\vspace{1cm}
\colorbox{white}{
        \begin{minipage}{0.6\textwidth}
            \vspace*{0.05cm}
            \color{blue}{\small \NAME~$\langle$\EMAIL$\rangle$}
            \vspace*{0.05cm}
        \end{minipage}
}


\vspace{0.1cm}
\colorbox{blue}{
        \begin{minipage}{0.6\textwidth}
            \vspace*{0.05cm}
            \color{white}{\small \textbf{\SEMINAR}: \insertdate}
            \vspace*{0.05cm}
        \end{minipage}
}

\vspace{0.1cm}
\hfill % This command ensures that the following content is right-aligned
\colorbox{black}{
        \begin{minipage}{0.25\textwidth}
            \vspace*{0.05cm}
            \flushright\color{white}{\tiny \MakeUppercase\FACULTY}
            \vspace*{0.05cm}
        \end{minipage}
}
}
% Define initial background based on the WIDE toggle
\AtBeginDocument{%
    \iftoggle{WIDE}{
        \usebackgroundtemplate{\includegraphics[width=\paperwidth]{hi-latex/front169.png}}
    }{
        \usebackgroundtemplate{\includegraphics[width=\paperwidth]{hi-latex/front43.png}}
    }
    \begin{frame}
    \titlepage
    \thispagestyle{empty}
    \end{frame}
    \usebackgroundtemplate{}
}


\setbeamertemplate{frametitle}{
    \vspace*{0.5cm} % Adjust this value to change the vertical spacing before the title
    \colorbox{blue}{
        \begin{minipage}{0.8\textwidth}
            \vspace*{0.1cm} % Adjust this for vertical padding
            \color{white}\insertframetitle
            \vspace*{0.1cm} % Adjust this for vertical padding
        \end{minipage}
        \begin{minipage}{0.2\textwidth}
            \includegraphics[width=\linewidth]{\hilogo}
        \end{minipage}
    }
}

\let\oldemph\emph
\renewcommand\emph[1]{{\color{blue}\textbf{#1}}}

\usepackage{booktabs} % Top and bottom rules for tables

\endinput
