# gpio-erlang

## Goals

* Provide GPIO controller from Erlang/OTP Application.

**CAUTION**

Now, gpio-erlang supported RaspberryPi only, I will support other devices, that running general GNU/Linux operating system. 

## Getting Started

Log in your RaspberryPi and fetch the latest version of gpio-erlang using git.

```
$ git clone https://github.com/hiroeorz/gpio-erlang.git
$ cd gpio-erlang
$ make
```

Or add "deps" line to your app's rebar.conf.

```erlang
{gpio, ".*", {git, "https://github.com/hiroeorz/gpio-erlang.git",
   {branch, "master"}}},

```

and get deps

```
$ ./rebar get-deps
```

## Running

check [default setting of gpio](blob/master/src/gpio.app.src).

In default setting, gpio 25,27 is output pin, other is input pin.

You can change pin setting by modifing above file, if you need.

### Start application.

You must run gpio-erlang as root. Because gpio-erlang access to /dev/mem. /dev/mem is allowed writable access from root only.

```
$ sudo ./start-dev
```
-------
or start erlang mode as root

```
$ sudo erl -pa ebin deps/*/ebin -boot start_sasl -sname gpio -setcookie gpio
```
and start gpio in erl shell.

```erl-sh
1> application:start(gpio).
```
-------

## General Purpose I/O

### Read gpio value

```erl-sh
1> gpio_pin:read(18).
0
```

### Write value to gpio

```erl-sh
1> gpio_pin:write(25, 1).
ok
3> gpio_pin:write(25, 0).
ok
```    

### Change Pin mode

```erl-sh
1> gpio_pin:set_pin_mode(18, out).
ok
2> gpio_pin:set_pin_mode(25, in).
ok
```

### Pullup or pulldown

```erl-sh
1> gpio_pin:pullup(18).
ok
2> gpio_pin:pulldown(18).
ok
3> gpio_pin:pullnone(18).
ok
```

### Set interrupt
 
```erl-sh
1> gpio_pin:set_int(18, rising).
ok
2> gpio_pin:set_int(18, falling).
ok
3> gpio_pin:set_int(18, both).
ok
4> gpio_pin:set_int(18, none).
ok
```
### Get active low
 
```erl-sh
1> gpio_pin:get_active_low(4).
0
```

### Set active low

```erl-sh
1> gpio_pin:set_active_low(25, 1).
```

example:

```erl-sh
1> gpio_pin:write(25, 1).
ok
2> gpio_pin:read(25).
1
3> gpio_pin:set_active_low(25, 1).
ok
2> gpio_pin:read(25).
0
```

### Add event handler of gpio pin

```erl-sh
1> gpio_pin_event:add_event_handler(sample_module, []).
ok
```
* First argument is module name.
* Second argument is arguments of sample_module:init/1.

The sample_module is event handler befavior of gen_event.
If gpio18 set interrupt rising and pin status changed 0 to 1 , called event handler.

This is sample event handler.
[gpio_pin_event_logger.erl](blob/master/src/gpio_pin_event.erl)


### Get all status list

```erl-sh
1> gpio_pin:status().
[1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
```
