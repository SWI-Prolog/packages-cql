\documentclass[11pt]{article}
\usepackage{times}
\usepackage{pl}
\usepackage{plpage}
\usepackage{html}
\sloppy
\makeindex

\onefile
\htmloutput{.}				% Output directory
\htmlmainfile{cql}			% Main document file
\bodycolor{white}			% Page colour

\renewcommand{\runningtitle}{Constraint Query Language}

\begin{document}

\title{Constraint Query Language\\
       A high level interface to SQL databases}
\author{Mike Elston \\
        Matt Lilley \\
        E-Mail: matt.s.lilley@gmail.com}

\maketitle

\begin{abstract}
CQL is a high level Prolog interface to SQL databases. It is inspired by
the work of Christoph Draxler \cite{Draxler:ALPUK91} in the sense that
SQL queries are generated but unlike Draxler's work, database tables are
not mapped to Prolog predicates, but database queries are described by
Prolog terms. These terms allow for accessing table columns by name and
provide access to several aspects of SQL that have no natural Prolog
equivalent, such as outer joins, inserts, etc.
\end{abstract}

\vfill

\pagebreak
\tableofcontents

\vfill
\vfill

\newpage

\input{cqldoc.tex}

\bibliographystyle{name}
\bibliography{pl}

\printindex

\end{document}
