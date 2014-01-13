

# Module gpio_sup #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Behaviours:__ [`supervisor`](supervisor.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td>
Starts the supervisor.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<ul class="definitions"><li><code>Pid = pid()</code></li><li><code>Error = term()</code></li></ul>


<a name="start_link-2"></a>

### start_link/2 ###


<pre><code>
start_link(IOList, EventHandlers) -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<ul class="definitions"><li><code>IOList = [{non_neg_integer(), in | out}]</code></li><li><code>EventHandlers = [atom()]</code></li><li><code>Pid = pid()</code></li><li><code>Error = term()</code></li></ul>


<a name="start_link-3"></a>

### start_link/3 ###


<pre><code>
start_link(IOList, EventHandlers, C_Node) -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<ul class="definitions"><li><code>IOList = [{non_neg_integer(), in | out}]</code></li><li><code>EventHandlers = [atom()]</code></li><li><code>C_Node = atom()</code></li><li><code>Pid = pid()</code></li><li><code>Error = term()</code></li></ul>


Starts the supervisor

