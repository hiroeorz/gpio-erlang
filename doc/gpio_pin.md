

# Module gpio_pin #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) (C) 2013, HIROE Shin

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ HIROE Shin ([`shin@hibiscus`](mailto:shin@hibiscus)).

<a name="types"></a>

## Data Types ##




### <a name="type-edge">edge()</a> ###



<pre><code>
edge() = falling | rising | both | none
</code></pre>





### <a name="type-mode">mode()</a> ###



<pre><code>
mode() = in | out
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_digital-0">all_digital/0</a></td><td>get status list.</td></tr><tr><td valign="top"><a href="#digital_change_notify-1">digital_change_notify/1</a></td><td>receive digital state change notify from gpio_port.</td></tr><tr><td valign="top"><a href="#get_active_low-1">get_active_low/1</a></td><td>get active low from a pin.</td></tr><tr><td valign="top"><a href="#pulldown-1">pulldown/1</a></td><td>set pulldown to a pin.</td></tr><tr><td valign="top"><a href="#pullnone-1">pullnone/1</a></td><td>release pin mode from pullup pulldown.</td></tr><tr><td valign="top"><a href="#pullup-1">pullup/1</a></td><td>set pullup to a pin.</td></tr><tr><td valign="top"><a href="#read-1">read/1</a></td><td>read gpio value.</td></tr><tr><td valign="top"><a href="#set_active_low-2">set_active_low/2</a></td><td>set active low to a pin.</td></tr><tr><td valign="top"><a href="#set_int-2">set_int/2</a></td><td>set interrupt that fire when gpio's input or output status is chaned.</td></tr><tr><td valign="top"><a href="#set_pin_mode-2">set_pin_mode/2</a></td><td>set pin mode, in or out.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>Starts the server.</td></tr><tr><td valign="top"><a href="#write-2">write/2</a></td><td>write value to gpio.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_digital-0"></a>

### all_digital/0 ###


<pre><code>
all_digital() -&gt; [1 | 0]
</code></pre>

<br></br>



get status list.


example: [0,1,1,0,0,0,0,0]
<a name="digital_change_notify-1"></a>

### digital_change_notify/1 ###

`digital_change_notify(PinNo) -> any()`

receive digital state change notify from gpio_port.
<a name="get_active_low-1"></a>

### get_active_low/1 ###


<pre><code>
get_active_low(PinNo) -&gt; 0 | 1
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li></ul>


get active low from a pin.


Mode=0: 通電時のread/1の結果は 通電->1 解放->0 (デフォルト)
Mode=1: 通電時のread/1の結果は 通電->0 解放->1
<a name="pulldown-1"></a>

### pulldown/1 ###


<pre><code>
pulldown(PinNo) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li></ul>


set pulldown to a pin.


RaspberryPi内蔵のプルダウン抵抗を用いてpinをプルダウン有りに設定します
入力無しの状態で常時GND接地の0.0Vとなり、3.3Vと短絡された場合のみ1となります
<a name="pullnone-1"></a>

### pullnone/1 ###


<pre><code>
pullnone(PinNo) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li></ul>


release pin mode from pullup pulldown.


RaspberryPi内蔵のプルアップ、プルダウン抵抗を用いません
入力無しの状態では不安定な電圧となり、外部回路でプルアップまたはプルダウンが必要です
<a name="pullup-1"></a>

### pullup/1 ###


<pre><code>
pullup(PinNo) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li></ul>


set pullup to a pin.


RaspberryPi内蔵のプルアップ抵抗を用いてpinをプルアップ有りに設定します
入力無しの状態で常時3.3Vの電圧がかかり、GNDと接地された場合のみ0となります
<a name="read-1"></a>

### read/1 ###


<pre><code>
read(PinNo) -&gt; Val
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Val = 0 | 1</code></li></ul>

read gpio value.
<a name="set_active_low-2"></a>

### set_active_low/2 ###


<pre><code>
set_active_low(PinNo, Mode) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Mode = 0 | 1</code></li></ul>


set active low to a pin.


Mode=1: active_lowを1に設定して、通電->0 解放->1 となるようにビット反転します
Mode=0: active_lowを0に設定して、通電->1 解放->0 となるようにします
<a name="set_int-2"></a>

### set_int/2 ###


<pre><code>
set_int(PinNo, Mode) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Mode = <a href="#type-edge">edge()</a></code></li><li><code>Reason = term()</code></li></ul>

set interrupt that fire when gpio's input or output status is chaned.
<a name="set_pin_mode-2"></a>

### set_pin_mode/2 ###


<pre><code>
set_pin_mode(PinNo, Mode) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Mode = <a href="#type-mode">mode()</a></code></li></ul>

set pin mode, in or out.
<a name="start_link-1"></a>

### start_link/1 ###


<pre><code>
start_link(X1::{PinNo, Mode} | {PinNo, Mode, Opts}) -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Mode = <a href="#type-mode">mode()</a></code></li><li><code>Opts = [tuple()]</code></li><li><code>Pid = pid()</code></li><li><code>Error = term()</code></li></ul>

Starts the server
<a name="write-2"></a>

### write/2 ###


<pre><code>
write(PinNo, Val) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>PinNo = non_neg_integer()</code></li><li><code>Val = 0 | 1</code></li></ul>

write value to gpio.
