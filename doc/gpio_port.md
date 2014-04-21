

# Module gpio_port #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bar-1">bar/1</a></td><td>Debug function.</td></tr><tr><td valign="top"><a href="#foo-1">foo/1</a></td><td>Debug function.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td>Initialize and goto receive loop.</td></tr><tr><td valign="top"><a href="#pulldown-1">pulldown/1</a></td><td>Pull down pin.</td></tr><tr><td valign="top"><a href="#pullnone-1">pullnone/1</a></td><td>Disable pullup and pulldown.</td></tr><tr><td valign="top"><a href="#pullup-1">pullup/1</a></td><td>Pull up pin.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Starts the server.</td></tr><tr><td valign="top"><a href="#start_poll-2">start_poll/2</a></td><td>Start Polling to pin (exec poll system call in port driver).</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Stop server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bar-1"></a>

### bar/1 ###


<pre><code>
bar(Y::number()) -&gt; number()
</code></pre>

<br></br>


Debug function
<a name="foo-1"></a>

### foo/1 ###


<pre><code>
foo(X::number()) -&gt; number()
</code></pre>

<br></br>


Debug function
<a name="init-1"></a>

### init/1 ###


<pre><code>
init(Args) -&gt; no_return()
</code></pre>

<ul class="definitions"><li><code>Args = list()</code></li></ul>

Initialize and goto receive loop.
<a name="pulldown-1"></a>

### pulldown/1 ###


<pre><code>
pulldown(PinNo) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li></ul>

Pull down pin.
<a name="pullnone-1"></a>

### pullnone/1 ###


<pre><code>
pullnone(PinNo) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li></ul>

Disable pullup and pulldown.
<a name="pullup-1"></a>

### pullup/1 ###


<pre><code>
pullup(PinNo) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li></ul>

Pull up pin.
<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; term() | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Reason = term()</code></li></ul>

Starts the server
<a name="start_poll-2"></a>

### start_poll/2 ###


<pre><code>
start_poll(PinNo, Mode) -&gt; ok | {error, start_poll_failed}
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Mode = rising | falling | both</code></li></ul>

Start Polling to pin (exec poll system call in port driver).
<a name="stop-0"></a>

### stop/0 ###


<pre><code>
stop() -&gt; ok
</code></pre>

<br></br>


Stop server.
