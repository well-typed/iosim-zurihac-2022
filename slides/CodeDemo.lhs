\documentclass{beamer}

%include defaults.fmt

\usepackage{hyperref}
\usepackage{fontawesome}
\usepackage{listings}

\begin{document}

\title{ZuriHac Advanced Track - IOSim}
\author{Armando Santos}
\institute{\pgfuseimage{welltypedlogo}}
\date{\today}
\frame[plain]{\titlepage}

\begin{frame}{About me}

\begin{columns}
    \begin{column}{0.30\textwidth}
      \begin{figure}
          \centering
          \includegraphics[width=0.7\columnwidth]{images/armando.jpg}
          \caption{$\uparrow$ Me}
          \label{fig:me}
      \end{figure}
    \end{column}
    \begin{column}{0.70\textwidth}
      \begin{itemize}
      \item Haskell Consultant at \emph{Well-Typed}
      \item Working in the Networking Team of \alert{IOHK}
      \item Formal Methods \& Distributed Systems Enthusiast
      \end{itemize}
    \end{column}
  \end{columns}

  \begin{center}
    You can find me at \faicon{github} \href{https://github.com/bolt12}{bolt12} and
    \faicon{twitter} \href{https://twitter.com/_bolt12}{\_bolt12} !
  \end{center}

\end{frame}


\section{ZuriHac Advanced Track - IOSim}

\begin{frame}{Materials for the Session}
\centering

\href{https://github.com/well-typed/iosim-zurihac-2022}{\alert{Slides \& Code}}
\vskip0.2cm
\emph{https://github.com/well-typed/iosim-zurihac-2022}

\end{frame}

\begin{frame}{What is IOSim?}

\begin{columns}
    \begin{column}{0.60\textwidth}
      \begin{itemize}
      \item Drop-in replacement for IO in Haskell
      \item A set of IO typeclasses for each different capability
      \item A tool and a framework to analyse simulated execution trace
      \end{itemize}
    \end{column}
    \begin{column}{0.40\textwidth}
      \begin{figure}
          \centering
          \includegraphics[width=\columnwidth]{images/cat.png}
          \caption{\href{https://impurepics.com/posts/2018-10-28-stuck-in-io.html}{Help}}
          \label{fig:cat}
      \end{figure}
    \end{column}
  \end{columns}

\end{frame}

\begin{frame}{Where and How is it used?}

\emph{Where}:

\begin{itemize}
  \item IOHK Networking team;
  \item Soon in someone's personal project (hopefully!).
\end{itemize}

\emph{How}:

\begin{itemize}
  \item Run same user code in simulation and production;
  \item Simulate high and low level components of the ouroboros-network stack:
  \begin{itemize}
    \item Test for high level properties of such components;
    \item Find edge case bugs;
    \item Simulate execution for years worth of simulated time.
  \end{itemize}
  \item Crazy use case in someone's personal project (hopefully!)
\end{itemize}

\end{frame}

\begin{frame}{Where to find?}

\begin{center}

  $\rightarrow$
  \href{https://github.com/input-output-hk/io-sim}{\emph{https://github.com/input-output-hk/io-sim}}
  $\leftarrow$

  \begin{figure}
      \centering
      \includegraphics[width=0.8\columnwidth]{images/io.png}
      \caption{\href{https://impurepics.com/posts/2021-12-04-real-world.html}{Real World}}
      \label{fig:io}
  \end{figure}

\end{center}

\end{frame}

\section{About this Workshop}

\begin{frame}{What we will cover}

This session is going to try and cover all the basics in order for you to be able to use
\alert{IOSim} in a personal project. That includes:

\begin{itemize}
  \item Getting a project up and running: \href{https://github.com/well-typed/iosim-zurihac-2022}{\emph{here!}};
  \item Intro to (contravariant) logging;
  \item Look into how IOSim works;
  \item Look into testing;
  \item Put everything together in a small project!
\end{itemize}

\end{frame}

\section{Contravariant Logging}

\begin{frame}{contra-tracer}

There are plenty of nice tutorials about contravariant logging:

\begin{itemize}
  \item \href{https://kowainik.github.io/posts/2018-09-25-co-log}{\emph{Kowainik Blog
  Post}}
  \item
  \href{https://www.youtube.com/watch?v=qzOQOmmkKEM&list=PLxxF72uPfQVTfDksvV4KPV5CxKnf0d_X3&index=16}{\emph{Duncan
  Coutts MuniHac 2020 Talk}}
  \item etc \dots
\end{itemize}

We are going to cover the basics.

\end{frame}

\begin{frame}{Why Logging?}

\begin{columns}
    \begin{column}{0.70\textwidth}
      \begin{itemize}
        \item All the reasons you might be familiar with already;
        \item Contravariant logging just seems a good approach in general;
        \item With IOSim we are going to need to build a useful execution trace;
        \item Logging will give us a way to build the events we care about into the trace.
      \end{itemize}
    \end{column}
    \begin{column}{0.30\textwidth}
      \begin{figure}
          \centering
          \includegraphics[width=0.9\columnwidth]{images/reuse.jpg}
          \caption{\href{https://www.pinterest.cl/pin/664069907531198853/}{Reuse}}
          \label{fig:reuse}
      \end{figure}
    \end{column}
\end{columns}

\end{frame}


\begin{frame}{contra-tracer}

> newtype Tracer m a = Tracer (a -> m ())

\vskip1cm

> traceWith :: Tracer m a -> a  -> m ()
> traceWith (Trace t) x = t x

\vskip1cm

> instance Contravariant (Tracer m) where
>    contramap f (Tracer t) = Tracer (t . f)

\end{frame}

\begin{frame}{contra-tracer examples}

> nullTracer :: Applicative m => Tracer m a
> nullTracer = Tracer (\_ -> pure ())
>
> stdoutTracer :: Tracer IO String
> stdoutTracer = Tracer putStrLn

In IOSim we have:
\vskip0.2cm

> traceM :: Typeable a => a -> IOSim s ()
> traceM x = IOSim $ \k -> Output (toDyn x) (k ())

\end{frame}

\section{io-sim \& io-classes}

\begin{frame}{io-sim}

\texttt{io-sim} is a library that supports:

\begin{itemize}
  \item Asynchronous exceptions
  \item Simulated time
  \item Timeout API
  \item Software Transaction Memory (STM)
  \item Concurrency: both low level \texttt{forkIO} as well as \texttt{async} style
  \item Strict STM
  \item Etc \dots
\end{itemize}

\end{frame}

\begin{frame}{io-sim}

\texttt{io-sim} is a library that offers:

\begin{itemize}
  \item \texttt{IOSim} data type;
  \item Functions to manipulate the trace;
  \item Pretty printers;
  \item \texttt{IOSimPOR} (Partial Order Reduction)
\end{itemize}

\end{frame}


\begin{frame}{io-classes}

\texttt{io-classes} is a library that offers a monad class hierarchy that is meant to be
an interface between \texttt{io-sim} and \texttt{IO}.

It aims to offer a subset of simulated \texttt{IO} capabilities without altering their original
semantics:

\begin{columns}
    \begin{column}{0.40\textwidth}
      \begin{itemize}
        \item \texttt{MonadAsync}
        \item \texttt{MonadEventlog}
        \item \texttt{MonadFork}
        \item \texttt{MonadST}
        \item \texttt{MonadSTM}
        \item \texttt{MonadSay}
        \item \texttt{MonadTest}
        \item \texttt{MonadThrow}
        \item \texttt{MonadTime}
        \item \texttt{MonadTimer}
      \end{itemize}
    \end{column}
    \begin{column}{0.6\textwidth}
      \begin{figure}
          \centering
          \includegraphics[width=0.9\columnwidth]{images/simulator.jpg}
          \caption{\href{https://imgur.com/gallery/8qXX7qk}{Inception}}
          \label{fig:sim}
      \end{figure}
    \end{column}
\end{columns}

\end{frame}

\begin{frame}{Example in IO}

> data TraceExample = SettingThreadDelay DiffTime
>                   | Printing String
>                   deriving (Show)

\vskip0.2cm

> exampleIO :: Tracer IO TraceExample -> IO ()
> exampleIO trace = do
>   traceWith trace (SettingThreadDelay 1000)
>   threadDelay 1000
>   traceWith trace (Printing "Hello World")
>   putStrLn "Hello World"

\end{frame}

\begin{frame}{Example in IOSim}

> data TraceExample = SettingThreadDelay DiffTime
>                   | Printing String
>                   deriving (Show)

\vskip0.2cm

> exampleIOSim :: (MonadDelay m, MonadSay m)
>              => Tracer m TraceExample -> m ()
> exampleIOSim trace = do
>   traceWith trace (SettingThreadDelay 1000)
>   threadDelay 1000
>   traceWith trace (Printing "Hello World")
>   say "Hello World"

\end{frame}

\begin{frame}{Main}

\scriptsize
\begin{code}
Time 0s    - ThreadId []   main - EventLog <<TraceExample>>
Time 0s    - ThreadId []   main - EventSay "SettingThreadDelay 1000s"
Time 0s    - ThreadId []   main - EventTimerCreated (TimeoutId 0)
                                                    (TVarId 0)
                                                    (Time 1000s)
Time 0s    - ThreadId []   main - EventTxBlocked [Labelled (TVarId 0)
                                                           (Just "<<timeout-state 0>>")]
                                                           Nothing
Time 0s    - ThreadId []   main - EventDeschedule Blocked
Time 1000s - ThreadId [-1] timer - EventTimerExpired (TimeoutId 0)
Time 1000s - ThreadId []   main - EventTxWakeup [Labelled (TVarId 0)
                                                          (Just "<<timeout-state 0>>")]
Time 1000s - ThreadId []   main - EventTxCommitted [] [] Nothing
Time 1000s - ThreadId []   main - EventUnblocked []
Time 1000s - ThreadId []   main - EventDeschedule Yield
Time 1000s - ThreadId []   main - EventLog <<TraceExample>>
Time 1000s - ThreadId []   main - EventSay "Printing \"Hello World\""
Time 1000s - ThreadId []   main - EventSay "Hello World"
Time 1000s - ThreadId []   main - EventThreadFinished
MainReturn (Time 1000s) () []
\end{code}

\end{frame}

\section{Testing}

\begin{frame}{How to test simulated code?}

IOSim API offers the following signatures:

\begin{small}
> runSim :: forall a. (forall s. IOSim s a)
>        -> Either Failure a

\vskip0.2cm
> runSimOrThrow :: forall a. (forall s. IOSim s a)
>               -> a

\vskip0.2cm
> runSimStrictShutdown :: forall a. (forall s. IOSim s a)
>                      -> Either Failure a

\vskip0.2cm
> runSimTrace :: forall a. (forall s. IOSim s a)
>             -> SimTrace a

\vskip0.2cm
> traceResult :: Bool
>             -> SimTrace a
>             -> Either Failure a

\vskip0.2cm
> traceEvents :: SimTrace a
>             -> [(Time, ThreadId, Maybe ThreadLabel, SimEventType)]
\end{small}

\end{frame}

\begin{frame}{How to test simulated code?}

There's also a family of function that allow you to extract only the events you care
about:

\begin{small}
> traceSelectTraceEvents :: (SimEventType -> Maybe b)
>                        -> SimTrace a
>                        -> Trace (SimResult a) b
\vskip0.2cm
> traceSelectTraceEventsDynamic :: forall a b. Typeable b
>                               => SimTrace a -> Trace (SimResult a) b

\vskip0.2cm
> traceSelectTraceEventsSay :: forall a. SimTrace a
>                           -> Trace (SimResult a) String
\end{small}

There's also the \texttt{List} variants if needed.

\end{frame}

\begin{frame}{Things to be aware of}

A couple of things to have in mind when using \alert{IOSim}:

\begin{columns}
    \begin{column}{0.6\textwidth}
      \begin{itemize}
        \item It is deterministic;
        \item Time only passes if there are explicit \texttt{threadDelays};
        \item Trace is as lazy as it can be;
        \item It is not a model checker;
      \end{itemize}
    \end{column}
    \begin{column}{0.4\textwidth}
      \begin{figure}
          \centering
          \includegraphics[width=0.8\columnwidth]{images/cat.jpg}
          \caption{\href{https://www.nicepng.com/ourpic/u2e6r5a9q8w7w7u2_cat-pointing-mid-cat-pointing-at-something/}{Cat
          pointing}}
          \label{fig:cat}
      \end{figure}
    \end{column}
\end{columns}

\end{frame}

\section{Pause?}

\begin{frame}{Demo}
  \begin{figure}
      \centering
      \includegraphics[width=\columnwidth]{images/demo.png}
      \caption{\href{https://dribbble.com/shots/8061657-Pair-programming/attachments/536241?mode=media}{Demo}}
      \label{fig:demo}
  \end{figure}
\end{frame}

\section{Source code}

\begin{frame}{io-sim}

\begin{itemize}
  \item \texttt{CommonTypes.hs}

        Common types shared between \texttt{IOSim} and \texttt{IOSimPOR}. Like
        \texttt{TVar} and \texttt{ThreadId}.

  \item \texttt{Internal.hs}

        \texttt{Thread} and \texttt{SimState} definitions. Functions to \texttt{schedule}
        and \texttt{deschedule} threads are implemented here.

  \item \texttt{InternalTypes.hs}

        Internal types shared between `IOSim` and `IOSimPOR`. Like the
        \texttt{ControlStack}, needed to deal with exceptions and masking.
\end{itemize}

\end{frame}

\begin{frame}{io-sim}

\begin{itemize}
  \item \texttt{STM.hs}

        \emph{io-sim} implementation of \texttt{TQueue} and \texttt{TBQueue}.

  \item \texttt{Types.hs}

        IOSim internal DSL for actions (\texttt{SimA} type) such as Timer, Exception,
        Forking, et.. API. Has also the \texttt{StmA} type that is responsible for the STM
        capabilities. All the \emph{io-classes} instance implementations are defined here.
\end{itemize}

\texttt{IOSim} outer module also exports a lot of utilities to work with
\texttt{SimTrace} as we have seen.

\end{frame}


\section{Small project}

\begin{frame}{Project Prompt}
\begin{centering}
  \emph{Classic Readers-Writers Problem}
\end{centering}

Basically have a shared memory area where different writer threads might write to (one at
the time), and have any number of reader threads reading from, assuming no writer is
writing to the the shared memory area. Useful properties that we ought to test:

\begin{itemize}
  \item No reader should be able to read while someone is writing
  \item No 2 writers should be able to write at the same time
  \item If the last writer writes X then the first reader must read X
  \item More \dots ?
\end{itemize}
\end{frame}

\section{Questions?}

\end{document}
