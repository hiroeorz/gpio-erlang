

# Module gpio_adc #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2014, HIROE Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#check_analog_value-1">check_analog_value/1</a></td><td>call from timer:apply_interval/4.</td></tr><tr><td valign="top"><a href="#read-1">read/1</a></td><td>read gpio value.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>
Starts the server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="check_analog_value-1"></a>

### check_analog_value/1 ###

`check_analog_value(Pid) -> any()`

call from timer:apply_interval/4.
<a name="read-1"></a>

### read/1 ###


<pre><code>
read(AnalogNo) -&gt; Val
</code></pre>

<ul class="definitions"><li><code>AnalogNo = non_neg_integer()</code></li><li><code>Val = 0 | 1</code></li></ul>

read gpio value.
<a name="start_link-2"></a>

### start_link/2 ###


<pre><code>
start_link(AnalogNo::non_neg_integer(), AnalogInterval::non_neg_integer()) -&gt; {ok, pid()} | ignore | {error, term()}
</code></pre>

<br></br>



Starts the server

