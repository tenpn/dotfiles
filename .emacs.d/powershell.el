<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"><head><title>EmacsWiki: Powershell.el</title><link rel="alternate" type="application/wiki" title="Edit this page" href="http://www.emacswiki.org/emacs?action=edit;id=Powershell.el" /><link type="text/css" rel="stylesheet" href="/emacs/wiki.css" /><meta name="robots" content="INDEX,FOLLOW" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki" href="http://www.emacswiki.org/emacs?action=rss" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki: Powershell.el" href="http://www.emacswiki.org/emacs?action=rss;rcidonly=Powershell.el" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content"
      href="http://www.emacswiki.org/emacs/full.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content and diff"
      href="http://www.emacswiki.org/emacs/full-diff.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki including minor differences"
      href="http://www.emacswiki.org/emacs/minor-edits.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Changes for Powershell.el only"
      href="http://www.emacswiki.org/emacs?action=rss;rcidonly=Powershell.el" />
<script type="text/javascript">
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-2101513-1']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();
</script>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/></head><body class="http://www.emacswiki.org/emacs"><div class="header"><a class="logo" href="http://www.emacswiki.org/emacs/SiteMap"><img class="logo" src="/emacs_logo.png" alt="[Home]" /></a><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/Glossary">Glossary</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span>
<!-- Google CSE Search Box Begins  -->
<form class="tiny" action="http://www.google.com/cse" id="searchbox_004774160799092323420:6-ff2s0o6yi"><p>
<input type="hidden" name="cx" value="004774160799092323420:6-ff2s0o6yi" />
<input type="text" name="q" size="25" />
<input type="submit" name="sa" value="Search" />
</p></form>
<script type="text/javascript" src="http://www.google.com/coop/cse/brand?form=searchbox_004774160799092323420%3A6-ff2s0o6yi"></script>
<!-- Google CSE Search Box Ends -->
<h1><a title="Click to search for references to this page" rel="nofollow" href="http://www.google.com/cse?cx=004774160799092323420:6-ff2s0o6yi&amp;q=%22Powershell.el%22">Powershell.el</a></h1></div><div class="wrapper"><div class="content browse"><p class="download"><a href="http://www.emacswiki.org/emacs/download/Powershell.el">Download</a></p><pre class="code"><span class="linecomment">;;; powershell.el --- run powershell as an inferior shell in emacs</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Author     : Dino Chiesa &lt;dpchiesa@hotmail.com&gt;</span>
<span class="linecomment">;; Created    : 10 Apr 2008</span>
<span class="linecomment">;; Modified   : May 2010</span>
<span class="linecomment">;; Version    : 0.2.4</span>
<span class="linecomment">;; Keywords   : powershell shell ms-windows</span>
<span class="linecomment">;; X-URL      : http://www.emacswiki.org/emacs/PowerShell#toc3</span>
<span class="linecomment">;; Last-saved : &lt;2011-February-17 12:10:59&gt;</span>
<span class="linecomment">;;</span>


<span class="linecomment">;;; Commentary:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Run Windows PowerShell v1.0 or v2.0 as an inferior shell within</span>
<span class="linecomment">;; emacs. Tested with emacs v22.2 and v23.2.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; To use it, M-x powershell .</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; ==============</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; TODO:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; - get TAB to do proper completion for powershell commands, filenames,</span>
<span class="linecomment">;;   etc.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;</span>

<span class="linecomment">;;; Versions:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;    0.1.0 - Initial release.</span>
<span class="linecomment">;;    0.2.4 - make powershell fn an autoload.</span>
<span class="linecomment">;;          - fixed problem where running a single shell, caused all</span>
<span class="linecomment">;;            future shells to be powershell.  This meant reverting to</span>
<span class="linecomment">;;            the original value of explicit-shell-file-name after</span>
<span class="linecomment">;;            invoking `shell'.</span>
<span class="linecomment">;;          - make location of powershell specifiable, via defcustom</span>
<span class="linecomment">;;            `powershell-location-of-exe'. Also turn a few other defvar</span>
<span class="linecomment">;;            into defcustom.</span>
<span class="linecomment">;;          - fix "Marker does not point anywhere" problem in</span>
<span class="linecomment">;;            `ansi-color-apply-on-region'.</span>
<span class="linecomment">;;</span>


<span class="linecomment">;;; License:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; This code is distributed under the New BSD License.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Copyright (c) 2008-2010, Dino Chiesa</span>
<span class="linecomment">;; All rights reserved.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Redistribution and use in source and binary forms, with or without</span>
<span class="linecomment">;; modification, are permitted provided that the following conditions</span>
<span class="linecomment">;; are met:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Redistributions of source code must retain the above copyright</span>
<span class="linecomment">;; notice, this list of conditions and the following disclaimer.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Redistributions in binary form must reproduce the above copyright</span>
<span class="linecomment">;; notice, this list of conditions and the following disclaimer in the</span>
<span class="linecomment">;; documentation and/or other materials provided with the distribution.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Neither the name of the author or any contributors, nor the names of</span>
<span class="linecomment">;; any organizations they belong to, may be used to endorse or promote</span>
<span class="linecomment">;; products derived from this software without specific prior written</span>
<span class="linecomment">;; permission.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS</span>
<span class="linecomment">;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT</span>
<span class="linecomment">;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR</span>
<span class="linecomment">;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT</span>
<span class="linecomment">;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,</span>
<span class="linecomment">;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,</span>
<span class="linecomment">;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS</span>
<span class="linecomment">;; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED</span>
<span class="linecomment">;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT</span>
<span class="linecomment">;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY</span>
<span class="linecomment">;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE</span>
<span class="linecomment">;; POSSIBILITY OF SUCH DAMAGE.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;</span>



(require 'shell)

<span class="linecomment">;; TODO: set this programmatically, relying on %WINDIR%</span>
(defcustom powershell-location-of-exe
  "<span class="quote">c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe</span>"
  "<span class="quote">A string, providing the location of the Powershell.exe</span>"
  :group 'powershell)

(defcustom powershell-log-level 3
  "<span class="quote">The current log level for powershell internal operations.
0 = NONE, 1 = Info, 2 = VERBOSE, 3 = DEBUG. </span>"
  :group 'powershell)

(defcustom powershell-squish-results-of-silent-commands t
"<span class="quote">The function `powershell-invoke-command-silently' returns the results
of a command in a string.  PowerShell by default, inserts newlines when
the output exceeds the configured width of the powershell virtual
window. In some cases callers might want to get the results with the
newlines and formatting removed. Set this to true, to do that.</span>"

:group 'powershell)



(defvar powershell-prompt-regex  "<span class="quote">PS [^#$%&gt;]+&gt; </span>"
  "<span class="quote">Regexp for powershell prompt.

Powershell.el uses this regex to determine when a command has completed.

Therefore, you need to set this appropriately if you explicitly
change the prompt function in powershell. Any value should
include a trailing space, if the powershell prompt uses a
trailing space, but should not include a trailing newline.

The default value will match the default PowerShell prompt.
</span>")

(defvar powershell-command-reply nil
  "<span class="quote">For internal use only. It holds the reply of powershell commands sent for housekeeping purposes.</span>")


(defvar powershell--max-window-width  0
  "<span class="quote">The maximum width of a powershell window.  You shouldn't need to ever
set this.  It gets set automatically, once, when the powershell starts up. </span>"
  )

(defvar powershell-command-timeout-seconds 12
  "<span class="quote">The timeout for a powershell command.  Powershell.el
will wait this long before giving up.</span>")


(defvar powershell--need-rawui-resize t
  "<span class="quote">No need to fuss with this.  It's intended for internal use
only.  It gets set when powershell needs to be informed that
emacs has resized its window. </span>")


(defconst powershell--find-max-window-width-command
  (concat
  "<span class="quote">function _Emacs_GetMaxPhsWindowSize \n</span>"
"<span class="quote">{\n</span>"
"<span class="quote">  $rawui = (Get-Host).UI.RawUI\n</span>"
"<span class="quote">  $mpws_exists = ($rawui | Get-Member | ? {$_.Name -eq \"MaxPhysicalWindowSize\"})\n</span>"
"<span class="quote">  if ($mpws_exists -eq $null) {\n</span>"
"<span class="quote">    \"210\" | Out-Host\n</span>"
"<span class="quote">  } else {\n</span>"
"<span class="quote">    $rawui.MaxPhysicalWindowSize.Width | Out-Host\n</span>"
"<span class="quote">  }\n</span>"
"<span class="quote">}\n</span>"
"<span class="quote">_Emacs_GetMaxPhsWindowSize\n</span>"
)
  "<span class="quote">The powershell logic to determine the max physical window width.</span>"
  )


(defconst powershell--set-window-width-fn-name  "<span class="quote">_Emacs_SetWindowWidth</span>"
  "<span class="quote">The name of the function this mode defines in PowerShell to set the window width. Intended for internal use only. </span>")


(defconst powershell--text-of-set-window-width-ps-function
  <span class="linecomment">;; see http://blogs.msdn.com/lior/archive/2009/05/27/ResizePowerShellConsoleWindow.aspx</span>
  <span class="linecomment">;;</span>
  <span class="linecomment">;; When making the console window narrower, you mus set the window</span>
  <span class="linecomment">;; size first. When making the console window wider, you must set the</span>
  <span class="linecomment">;; buffer size first.</span>

    (concat  "<span class="quote">function </span>" powershell--set-window-width-fn-name "<span class="quote">([string] $pswidth)\n</span>"
             "<span class="quote">{\n</span>"
             <span class="linecomment">;;"  \"resetting window width to $pswidth\n\" | Out-Host\n"</span>
             "<span class="quote">  $rawui = (Get-Host).UI.RawUI\n</span>"
             "<span class="quote">  # retrieve the values\n</span>"
             "<span class="quote">  $bufsize = $rawui.BufferSize\n</span>"
             "<span class="quote">  $winsize = $rawui.WindowSize\n</span>"
             "<span class="quote">  $cwidth = $winsize.Width\n</span>"
             "<span class="quote">  $winsize.Width = $pswidth \n</span>"
             "<span class="quote">  $bufsize.Width = $pswidth\n</span>"
             "<span class="quote">  if ($cwidth -lt $pswidth) {\n</span>"
             "<span class="quote">    # increase the width\n</span>"
             "<span class="quote">    $rawui.BufferSize = $bufsize\n</span>"
             "<span class="quote">    $rawui.WindowSize = $winsize\n</span>"
             "<span class="quote">  }\n</span>"
             "<span class="quote">  elseif ($cwidth -gt $pswidth) {\n</span>"
             "<span class="quote">    # decrease the width\n</span>"
             "<span class="quote">    $rawui.WindowSize = $winsize\n</span>"
             "<span class="quote">    $rawui.BufferSize = $bufsize\n</span>"
             "<span class="quote">  }\n</span>"
             "<span class="quote">  # destroy variables\n</span>"
             "<span class="quote">  Set-Variable -name rawui -value $null\n</span>"
             "<span class="quote">  Set-Variable -name winsize -value $null\n</span>"
             "<span class="quote">  Set-Variable -name bufsize -value $null\n</span>"
             "<span class="quote">  Set-Variable -name cwidth -value $null\n</span>"
             "<span class="quote">}\n\n</span>")

    "<span class="quote">The text of the powershell function that will be used at runtime to
set the width of the virtual Window in PowerShell, as the Emacs window
gets resized.
</span>")



(defun powershell-log (level text &rest args)
  "<span class="quote">Log a message at level LEVEL.
If LEVEL is higher than `powershell-log-level', the message is
ignored.  Otherwise, it is printed using `message'.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see `format').</span>"
  (if (&lt;= level powershell-log-level)
      (let* ((msg (apply 'format text args)))
        (message "<span class="quote">%s</span>" msg)
        )))


<span class="linecomment">;; (defun dino-powershell-complete (arg)</span>
<span class="linecomment">;;   "do powershell completion on the given STRING. Pop up a buffer with the completion list."</span>
<span class="linecomment">;;   (interactive</span>
<span class="linecomment">;;    (list (read-no-blanks-input "\</span>
<span class="linecomment">;; Stub to complete: ")))</span>

<span class="linecomment">;;   (let ((proc</span>
<span class="linecomment">;;          (get-buffer-process (current-buffer))))</span>
<span class="linecomment">;;    (comint-proc-query proc (concat "Get-Command " arg "*\n"))</span>
<span class="linecomment">;;    )</span>
<span class="linecomment">;; )</span>

<span class="linecomment">;; (defun dino-powershell-cmd-complete ()</span>
<span class="linecomment">;;   "try to get powershell completion to work."</span>
<span class="linecomment">;;   (interactive)</span>
<span class="linecomment">;;   (let ((proc</span>
<span class="linecomment">;;          (get-buffer-process (current-buffer))))</span>
<span class="linecomment">;; ;;   (comint-proc-query proc "Get-a\t")</span>
<span class="linecomment">;; ;;   (comint-simple-send proc "Get-a\t")</span>
<span class="linecomment">;;        (comint-send-string proc "Get-a\t\n")</span>
<span class="linecomment">;; ;;   (process-send-eof)</span>
<span class="linecomment">;;    )</span>
<span class="linecomment">;; )</span>



(defun powershell--define-set-window-width-function (proc)
  "<span class="quote">Sends a function definition to the PowerShell instance
identified by PROC.  The function sets the window width of the
PowerShell virtual window.  Later, the function will be called
when the width of the emacs window changes.
</span>"
    (if proc
        (progn
          <span class="linecomment">;;process-send-string</span>
          (comint-simple-send
           proc
           powershell--text-of-set-window-width-ps-function))))




(defun powershell--get-max-window-width  (buffer-name)
  "<span class="quote">Gets the maximum width of the virtual window for PowerShell running
in the buffer with name BUFFER-NAME.

In PowerShell 1.0, the maximum WindowSize.Width for
PowerShell is 210, hardcoded, I believe. In PowerShell 2.0, the max
windowsize.Width is provided in the RawUI.MaxPhysicalWindowSize
property.

This function does the right thing, and sets the buffer-local
`powershell--max-window-width' variable with the correct value.

</span>"
  (let ((proc (get-buffer-process buffer-name)))

    (if proc
        (save-excursion
          (set-buffer buffer-name) <span class="linecomment">;; to get buffer-local variables</span>

          (powershell-invoke-command-silently
           proc
           powershell--find-max-window-width-command
           0.90)

          <span class="linecomment">;; store the retrieved width</span>
          (setq powershell--max-window-width
                (if (and (not (null powershell-command-reply))
                         (string-match
                          "<span class="quote">\\([1-9][0-9]*\\)[ \t\f\v\n]+</span>"
                          powershell-command-reply))
                    (string-to-number (match-string 1 powershell-command-reply))
                  200)))))) <span class="linecomment">;; could go to 210, but let's use 200 to be safe</span>




(defun powershell--set-window-width (proc)
  "<span class="quote">Run the PowerShell function that sets the RawUI width
appropriately for a PowerShell shell.

This is necessary to get powershell to do the right thing, as far
as text formatting, when the emacs window gets resized.

The function gets defined in powershell upon powershell startup.
</span>"
  (let ((ps-width
         (number-to-string (min powershell--max-window-width (window-width)))))
    (progn
      <span class="linecomment">;;(process-send-string</span>
      (comint-simple-send
       proc
       (concat powershell--set-window-width-fn-name
               "<span class="quote">('</span>" ps-width "<span class="quote">')</span>")))))




(defun powershell (&optional buffer prompt-string)

  "<span class="quote">Run an inferior PowerShell, with I/O through tne named
BUFFER (which defaults to `*PowerShell*').

Interactively, a prefix arg means to prompt for BUFFER.

If BUFFER exists but the shell process is not running, it makes a new shell.

If BUFFER exists and the shell process is running, just switch to BUFFER.

If PROMPT-STRING is non-nil, sets the prompt to the given value.

See the help for `shell' for more details.  \(Type
\\[describe-mode] in the shell buffer for a list of commands.)

</span>"
  (interactive
   (list
    (and current-prefix-arg
         (read-buffer "<span class="quote">Shell buffer: </span>"
                      (generate-new-buffer-name "<span class="quote">*PowerShell*</span>")))))

  (setq buffer (get-buffer-create (or buffer "<span class="quote">*PowerShell*</span>")))
  (powershell-log 1 "<span class="quote">powershell starting up...in buffer %s</span>" (buffer-name buffer))
  (let ((tmp-shellfile explicit-shell-file-name))
    <span class="linecomment">;; set arguments for the powershell exe.</span>
    <span class="linecomment">;; Does this need to be tunable?</span>

    (setq explicit-shell-file-name powershell-location-of-exe)
    (setq explicit-powershell.exe-args '("<span class="quote">-Command</span>" "<span class="quote">-</span>" ))
    (shell buffer)
    (setq explicit-shell-file-name tmp-shellfile))

  <span class="linecomment">;; (powershell--get-max-window-width "*PowerShell*")</span>
  <span class="linecomment">;; (powershell-invoke-command-silently (get-buffer-process "*csdeshell*") "[Ionic.Csde.Utilities]::Version()" 2.9)</span>

  <span class="linecomment">;;  (comint-simple-send (get-buffer-process "*csdeshell*") "prompt\n")</span>


  (let ((proc (get-buffer-process buffer)))

    (make-local-variable 'powershell-prompt-regex)
    (make-local-variable 'powershell-command-reply)
    (make-local-variable 'powershell--max-window-width)
    (make-local-variable 'powershell-command-timeout-seconds)
    (make-local-variable 'powershell-squish-results-of-silent-commands)
    (make-local-variable 'powershell--need-rawui-resize)
    (make-local-variable 'comint-prompt-read-only)

    <span class="linecomment">;; disallow backspace over the prompt:</span>
    (setq comint-prompt-read-only t)

    <span class="linecomment">;; We need to tell powershell how wide the emacs window is, because</span>
    <span class="linecomment">;; powershell pads its output to the width it thinks its window is.</span>
    <span class="linecomment">;;</span>
    <span class="linecomment">;; The way it's done: every time the width of the emacs window changes, we</span>
    <span class="linecomment">;; set a flag. Then, before sending a powershell command that is</span>
    <span class="linecomment">;; typed into the buffer, to the actual powershell process, we check</span>
    <span class="linecomment">;; that flag.  If it is set, we  resize the powershell window appropriately,</span>
    <span class="linecomment">;; before sending the command.</span>

    <span class="linecomment">;; If we didn't do this, powershell output would get wrapped at a</span>
    <span class="linecomment">;; column width that would be different than the emacs buffer width,</span>
    <span class="linecomment">;; and everything would look ugly.</span>

    <span class="linecomment">;; get the maximum width for powershell - can't go beyond this</span>
    (powershell--get-max-window-width buffer)

    <span class="linecomment">;; define the function for use within powershell to resize the window</span>
    (powershell--define-set-window-width-function proc)

    <span class="linecomment">;; add the hook that sets the flag</span>
    (add-hook 'window-size-change-functions
              '(lambda (&optional x)
                 (setq powershell--need-rawui-resize t)))

    <span class="linecomment">;; set the flag so we resize properly the first time.</span>
    (setq powershell--need-rawui-resize t)

    (if prompt-string
        (progn
          <span class="linecomment">;; This sets up a prompt for the PowerShell.  The prompt is</span>
          <span class="linecomment">;; important because later, after sending a command to the</span>
          <span class="linecomment">;; shell, the scanning logic that grabs the output looks for</span>
          <span class="linecomment">;; the prompt string to determine that the output is complete.</span>
          (comint-simple-send
           proc
           (concat "<span class="quote">function prompt { '</span>" prompt-string "<span class="quote">' }</span>"))

          (setq powershell-prompt-regex prompt-string)))

    <span class="linecomment">;; hook the kill-buffer action so we can kill the inferior process?</span>
    (add-hook 'kill-buffer-hook 'powershell-delete-process)

    <span class="linecomment">;; wrap the comint-input-sender with a PS version</span>
    <span class="linecomment">;; must do this after launching the shell!</span>
    (make-local-variable 'comint-input-sender)
    (setq comint-input-sender 'powershell-simple-send)

    <span class="linecomment">;; set a preoutput filter for powershell.  This will trim newlines after the prompt.</span>
    (add-hook 'comint-preoutput-filter-functions 'powershell-preoutput-filter-for-prompt)

    <span class="linecomment">;; send a carriage-return  (get the prompt)</span>
    (comint-send-input)
    (accept-process-output proc))

  <span class="linecomment">;; The launch hooks for powershell has not (yet?) been implemented</span>
  <span class="linecomment">;;(run-hooks 'powershell-launch-hook)</span>

  <span class="linecomment">;; return the buffer created</span>
  buffer)


<span class="linecomment">;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-</span>
<span class="linecomment">;; Using powershell on emacs23, I get an error:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;    ansi-color-process-output: Marker does not point anywhere</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Here's what's happening.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; In order to be able to read the output from powershell, this shell</span>
<span class="linecomment">;; starts powershell.exe in "interactive mode", using the -i</span>
<span class="linecomment">;; option. This which has the curious side-effect of turning off the</span>
<span class="linecomment">;; prompt in powershell. Normally powershell will return its results,</span>
<span class="linecomment">;; then emit a prompt to indicate that it is ready for more input.  In</span>
<span class="linecomment">;; interactive mode it doesn't emit the prompt.  To work around this,</span>
<span class="linecomment">;; this code (powershell.el) sends an explicit `prompt` command after</span>
<span class="linecomment">;; sending any user-entered command to powershell. This tells powershell</span>
<span class="linecomment">;; to explicitly return the prompt, after the results of the prior</span>
<span class="linecomment">;; command. The prompt then shows up in the powershell buffer.  Lovely.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; But, `ansi-color-apply-on-region` gets called after every command</span>
<span class="linecomment">;; gets sent to powershell. It gets called with args `(begin end)`,</span>
<span class="linecomment">;; which are both markers. Turns out the very first time this fn is</span>
<span class="linecomment">;; called, the position for the begin marker is nil.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; `ansi-color-apply-on-region` calls `(goto-char begin)` (effectively),</span>
<span class="linecomment">;; and when the position on the marker is nil, the call errors with</span>
<span class="linecomment">;; "Marker does not point anywhere."</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; The following advice suppresses the call to</span>
<span class="linecomment">;; `ansi-color-apply-on-region` when the begin marker points</span>
<span class="linecomment">;; nowhere.</span>
(defadvice ansi-color-apply-on-region (around
                                       powershell-throttle-ansi-colorizing
                                       (begin end)
                                       activate compile)
  (progn
    (let ((start-pos (marker-position begin)))
    (cond
     (start-pos
      (progn
        ad-do-it))))))




(defun powershell--silent-cmd-filter (process result)
"<span class="quote">A process filter that captures output from a shell and stores it
to `powershell-command-reply', rather than allowing the output to
be displayed in the shell buffer.

This function is intended for internal use only.
</span>"
  (let ((end-of-result
         (string-match (concat "<span class="quote">.*\n\\(</span>" powershell-prompt-regex "<span class="quote">\\)[ \n]*\\'</span>")
                       result)))
    (if (and end-of-result (numberp end-of-result))

        (progn
          <span class="linecomment">;; Store everything except the follow-on prompt.</span>
          <span class="linecomment">;; The result probably includes a final newline!</span>
          (setq result (substring result 0 (match-beginning 1)))

          (if powershell-squish-results-of-silent-commands
              (setq result
                    (replace-regexp-in-string "<span class="quote">\n</span>" "<span class="quote"></span>" result)))

          (setq powershell-command-reply
                (concat powershell-command-reply result)))

      (progn
        (if powershell-squish-results-of-silent-commands
              (setq result
                    (replace-regexp-in-string "<span class="quote">\n</span>" "<span class="quote"></span>" result)))

        (setq powershell-command-reply
              (concat powershell-command-reply result))

        <span class="linecomment">;; recurse.  For very very long output, the recursion can</span>
        <span class="linecomment">;; cause stack overflow. Careful!</span>
        (accept-process-output process powershell-command-timeout-seconds)))))



(defun powershell-invoke-command-silently (proc command &optional timeout-seconds)
  "<span class="quote">Invoke COMMAND in the PowerShell instance PROC, silently, without
echoing the command or the results to the associated buffer.  Use
TIMEOUT-SECONDS as the timeout, waiting for a response.  The COMMAND
should be a string, and need not be terminated with a newline.

This is helpful when, for example, doing setup work. Or other sneaky
stuff, such as resetting the size of the PowerShell virtual window.

Returns the result of the command, a string, without the follow-on
command prompt.  The result will probably end in a newline. This result
is also stored in the buffer-local variable `powershell-command-reply'.

In some cases the result can be prepended with the command prompt, as
when, for example, several commands have been send in succession and the
results of the prior command were not fully processed by the application.

If a PowerShell buffer is not the current buffer, this function
should be invoked within a call to `with-current-buffer' or
similar in order to insure that the buffer-local values of
`powershell-command-reply', `powershell-prompt-regex', and
`powershell-command-timeout-seconds' are used.

Example:

    (with-current-buffer powershell-buffer-name
      (powershell-invoke-command-silently
       proc
       command-string
       1.90))

</span>"

  (let ((old-timeout powershell-command-timeout-seconds)
        (original-filter (process-filter proc)))

    (setq powershell-command-reply nil)

    (if timeout-seconds
        (setq powershell-command-timeout-seconds timeout-seconds))

    (set-process-filter proc 'powershell--silent-cmd-filter)

    <span class="linecomment">;; Send the command plus the "prompt" command.  The filter</span>
    <span class="linecomment">;; will know the command is finished when it sees the command</span>
    <span class="linecomment">;; prompt.</span>
    <span class="linecomment">;;</span>
    (process-send-string proc (concat command "<span class="quote">\nprompt\n</span>"))

    (accept-process-output proc powershell-command-timeout-seconds)

    <span class="linecomment">;; output of the command is now available in powershell-command-reply</span>

    <span class="linecomment">;; Trim prompt from the beginning of the output.</span>
    <span class="linecomment">;; this can happen for the first command through</span>
    <span class="linecomment">;; the shell.  I think there's a race condition.</span>
    (if (and powershell-command-reply
             (string-match (concat "<span class="quote">^</span>" powershell-prompt-regex "<span class="quote">\\(.*\\)\\'</span>")
                           powershell-command-reply))
        (setq powershell-command-reply
              (substring powershell-command-reply
                         (match-beginning 1)
                         (match-end 1))))

    <span class="linecomment">;; restore the original filter</span>
    (set-process-filter proc original-filter)

    <span class="linecomment">;; restore the original timeout</span>
    (if timeout-seconds
        (setq powershell-command-timeout-seconds old-timeout))

    <span class="linecomment">;; the result:</span>
    powershell-command-reply))




(defun powershell-delete-process (&optional proc)
  (or proc
      (setq proc (get-buffer-process (current-buffer))))
  (and (processp proc)
       (delete-process proc)))


(defun powershell-preoutput-filter-for-prompt (string)
  "<span class="quote">Trim the newline from STRING, the prompt that we get back from
powershell.  This fn is set into the preoutput filters, so the
newline is trimmed before being put into the output buffer.
</span>"
   (if (string-match (concat powershell-prompt-regex "<span class="quote">\n\\'</span>") string)
       (substring string 0 -1) <span class="linecomment">;; remove newline</span>
     string))


(defun powershell-simple-send (proc string)
  "<span class="quote">Override of the comint-simple-send function, with logic
specifically designed for powershell.  This just sends STRING,
plus the prompt command.

When running as an inferior shell with stdin/stdout redirected,
powershell is in noninteractive mode. This means no prompts get
emitted when a PS command completes. This makes it difficult for
a comint mode to determine when the command has completed.
Therefore, we send an explicit request for the prompt, after
sending the actual (primary) command. When the primary command
completes, Powershell then responds to the \"prompt\" command,
and emits the prompt.

This insures we get and display the prompt.
</span>"
  <span class="linecomment">;; Tell PowerShell to resize its virtual window, if necessary. We do</span>
  <span class="linecomment">;; this by calling a resize function in the PowerShell, before sending</span>
  <span class="linecomment">;; the user-entered command to the shell.</span>
  <span class="linecomment">;;</span>
  <span class="linecomment">;; Powershell keeps track of its \"console\", and formats its output</span>
  <span class="linecomment">;; according to the width it thinks it is using.  This is true even when</span>
  <span class="linecomment">;; powershell is invoked with the - argument, which tells it to use</span>
  <span class="linecomment">;; stdin as input.</span>

  <span class="linecomment">;; Therefore, if the user has resized the emacs window since the last</span>
  <span class="linecomment">;; PowerShell command, we need to tell PowerShell to change the size</span>
  <span class="linecomment">;; of its virtual window. Calling that function does not change the</span>
  <span class="linecomment">;; size of a window that is visible on screen - it only changes the</span>
  <span class="linecomment">;; size of the virtual window that PowerShell thinks it is using.  We</span>
  <span class="linecomment">;; do that by invoking the PowerShell function that this module</span>
  <span class="linecomment">;; defined for that purpose.</span>
  <span class="linecomment">;;</span>
  (if powershell--need-rawui-resize
      (progn
        (powershell--set-window-width proc)
        (setq powershell--need-rawui-resize nil)))
  (comint-simple-send proc (concat string "<span class="quote">\n</span>"))
  (comint-simple-send proc "<span class="quote">prompt\n</span>"))



<span class="linecomment">;; Notes on TAB for completion.</span>
<span class="linecomment">;; -------------------------------------------------------</span>
<span class="linecomment">;; Emacs calls comint-dynamic-complete when the TAB key is pressed in a shell.</span>
<span class="linecomment">;; This is set up in shell-mode-map.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; comint-dynamic-complete calls the functions in  comint-dynamic-complete-functions,</span>
<span class="linecomment">;; until one of them returns non-nil.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; comint-dynamic-complete-functions is a good thing to set in the mode hook.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; The default value for that var in a powershell shell is:</span>
<span class="linecomment">;; (comint-replace-by-expanded-history</span>
<span class="linecomment">;;    shell-dynamic-complete-environment-variable</span>
<span class="linecomment">;;    shell-dynamic-complete-command</span>
<span class="linecomment">;;    shell-replace-by-expanded-directory</span>
<span class="linecomment">;;    comint-dynamic-complete-filename)</span>



<span class="linecomment">;; (defun powershell-dynamic-complete-command ()</span>
<span class="linecomment">;;   "Dynamically complete the command at point.</span>
<span class="linecomment">;; This function is similar to `comint-dynamic-complete-filename', except that it</span>
<span class="linecomment">;; searches the commands from powershell and then the `exec-path' (minus the</span>
<span class="linecomment">;; trailing Emacs library path)  for completion</span>
<span class="linecomment">;; candidates.</span>

<span class="linecomment">;; Completion is dependent on the value of `shell-completion-execonly', plus</span>
<span class="linecomment">;; those that effect file completion.  See `powershell-dynamic-complete-as-command'.</span>

<span class="linecomment">;; Returns t if successful."</span>
<span class="linecomment">;;   (interactive)</span>
<span class="linecomment">;;   (let ((filename (comint-match-partial-filename)))</span>
<span class="linecomment">;;     (if (and filename</span>
<span class="linecomment">;;              (save-match-data (not (string-match "[~/]" filename)))</span>
<span class="linecomment">;;              (eq (match-beginning 0)</span>
<span class="linecomment">;;                  (save-excursion (shell-backward-command 1) (point))))</span>
<span class="linecomment">;;         (prog2 (message "Completing command name...")</span>
<span class="linecomment">;;             (powershell-dynamic-complete-as-command)))))</span>


<span class="linecomment">;; (defun powershell-dynamic-complete-as-command ()</span>
<span class="linecomment">;;   "Dynamically complete at point as a command.</span>
<span class="linecomment">;; See `shell-dynamic-complete-filename'.  Returns t if successful."</span>
<span class="linecomment">;;   (let* ((filename (or (comint-match-partial-filename) ""))</span>
<span class="linecomment">;;          (filenondir (file-name-nondirectory filename))</span>
<span class="linecomment">;;          (path-dirs (cdr (reverse exec-path)))</span>
<span class="linecomment">;;          (cwd (file-name-as-directory (expand-file-name default-directory)))</span>
<span class="linecomment">;;          (ignored-extensions</span>
<span class="linecomment">;;           (and comint-completion-fignore</span>
<span class="linecomment">;;                (mapconcat (function (lambda (x) (concat (regexp-quote x) "$")))</span>
<span class="linecomment">;;                           comint-completion-fignore "\\|")))</span>
<span class="linecomment">;;          (dir "") (comps-in-dir ())</span>
<span class="linecomment">;;          (file "") (abs-file-name "") (completions ()))</span>

<span class="linecomment">;;     ;; Go thru each cmd in powershell's lexicon, finding completions.</span>

<span class="linecomment">;;     ;; Go thru each dir in the search path, finding completions.</span>
<span class="linecomment">;;     (while path-dirs</span>
<span class="linecomment">;;       (setq dir (file-name-as-directory (comint-directory (or (car path-dirs) ".")))</span>
<span class="linecomment">;;             comps-in-dir (and (file-accessible-directory-p dir)</span>
<span class="linecomment">;;                               (file-name-all-completions filenondir dir)))</span>
<span class="linecomment">;;       ;; Go thru each completion found, to see whether it should be used.</span>
<span class="linecomment">;;       (while comps-in-dir</span>
<span class="linecomment">;;         (setq file (car comps-in-dir)</span>
<span class="linecomment">;;               abs-file-name (concat dir file))</span>
<span class="linecomment">;;         (if (and (not (member file completions))</span>
<span class="linecomment">;;                  (not (and ignored-extensions</span>
<span class="linecomment">;;                            (string-match ignored-extensions file)))</span>
<span class="linecomment">;;                  (or (string-equal dir cwd)</span>
<span class="linecomment">;;                      (not (file-directory-p abs-file-name)))</span>
<span class="linecomment">;;                  (or (null shell-completion-execonly)</span>
<span class="linecomment">;;                      (file-executable-p abs-file-name)))</span>
<span class="linecomment">;;             (setq completions (cons file completions)))</span>
<span class="linecomment">;;         (setq comps-in-dir (cdr comps-in-dir)))</span>
<span class="linecomment">;;       (setq path-dirs (cdr path-dirs)))</span>
<span class="linecomment">;;     ;; OK, we've got a list of completions.</span>
<span class="linecomment">;;     (let ((success (let ((comint-completion-addsuffix nil))</span>
<span class="linecomment">;;                      (comint-dynamic-simple-complete filenondir completions))))</span>
<span class="linecomment">;;       (if (and (memq success '(sole shortest)) comint-completion-addsuffix</span>
<span class="linecomment">;;                (not (file-directory-p (comint-match-partial-filename))))</span>
<span class="linecomment">;;           (insert " "))</span>
<span class="linecomment">;;       success)))</span>


(provide 'powershell)

<span class="linecomment">;; End of powershell.el</span></span></pre></div><div class="wrapper close"></div></div><div class="footer"><hr /><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/Glossary">Glossary</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span><span class="translation bar"><br />  <a class="translation new" rel="nofollow" href="http://www.emacswiki.org/emacs?action=translate;id=Powershell.el;missing=de_es_fr_it_ja_ko_pt_ru_se_zh">Add Translation</a></span><span class="edit bar"><br /> <a class="comment local" href="http://www.emacswiki.org/emacs/Comments_on_Powershell.el">Talk</a> <a class="edit" accesskey="e" title="Click to edit this page" rel="nofollow" href="http://www.emacswiki.org/emacs?action=edit;id=Powershell.el">Edit this page</a> <a class="history" rel="nofollow" href="http://www.emacswiki.org/emacs?action=history;id=Powershell.el">View other revisions</a> <a class="admin" rel="nofollow" href="http://www.emacswiki.org/emacs?action=admin;id=Powershell.el">Administration</a></span><!-- test --><span class="time"><br /> Last edited 2011-02-17 17:11 UTC by mobile-166-137-140-093.mycingular.net <a class="diff" rel="nofollow" href="http://www.emacswiki.org/emacs?action=browse;diff=2;id=Powershell.el">(diff)</a></span><div style="float:right; margin-left:1ex;">
<!-- Creative Commons License -->
<a href="http://creativecommons.org/licenses/GPL/2.0/"><img alt="CC-GNU GPL" style="border:none" src="/pics/cc-GPL-a.png" /></a>
<!-- /Creative Commons License -->
</div>

<!--
<rdf:RDF xmlns="http://web.resource.org/cc/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
<Work rdf:about="">
   <license rdf:resource="http://creativecommons.org/licenses/GPL/2.0/" />
  <dc:type rdf:resource="http://purl.org/dc/dcmitype/Software" />
</Work>

<License rdf:about="http://creativecommons.org/licenses/GPL/2.0/">
   <permits rdf:resource="http://web.resource.org/cc/Reproduction" />
   <permits rdf:resource="http://web.resource.org/cc/Distribution" />
   <requires rdf:resource="http://web.resource.org/cc/Notice" />
   <permits rdf:resource="http://web.resource.org/cc/DerivativeWorks" />
   <requires rdf:resource="http://web.resource.org/cc/ShareAlike" />
   <requires rdf:resource="http://web.resource.org/cc/SourceCode" />
</License>
</rdf:RDF>
-->

<p class="legal">
This work is licensed to you under version 2 of the
<a href="http://www.gnu.org/">GNU</a> <a href="/GPL">General Public License</a>.
Alternatively, you may choose to receive this work under any other
license that grants the right to use, copy, modify, and/or distribute
the work, as long as that license imposes the restriction that
derivative works have to grant the same rights and impose the same
restriction. For example, you may choose to receive this work under
the
<a href="http://www.gnu.org/">GNU</a>
<a href="/FDL">Free Documentation License</a>, the
<a href="http://creativecommons.org/">CreativeCommons</a>
<a href="http://creativecommons.org/licenses/sa/1.0/">ShareAlike</a>
License, the XEmacs manual license, or
<a href="/OLD">similar licenses</a>.
</p>
</div>
</body>
</html>
