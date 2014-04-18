/* gpio_lib.c */

/*  taken from erlan_ale
 *  https://github.com/esl/erlang_ale/blob/master/c_src/gpio_node.c 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/mman.h>
#include <netinet/in.h>
#include <poll.h>
#include <pthread.h>
#include <inttypes.h>

#include "erl_interface.h"
#include "erl_driver.h"
#include "ei.h"

#define BUFSIZE 1000
#define DEBUG 1

#ifdef DEBUG
#define debug(...) printf(__VA_ARGS__)
#else
#define debug(...) ;
#endif

static pthread_t isr_handler_thread;
static int isr_handler_flag;

typedef struct {
   int pin;
   void (*isr) (int pin, int mode);
   int mode;
} isr_t;

typedef struct {
  ErlDrvPort port;
} gpio_data;

typedef struct {
  char fn;
  char arg1;
  char arg2;
  char res;
} gpio_async_data;

static ErlDrvPort drv_port;

int
pthread_tryjoin_np(pthread_t thread, void **retval);

void
strstrip(char *s);

static int
gpio_valfd (int);

void
gpio_init();

int
gpio_pullup_down(int pin, int mode);

int
gpio_start_poll (int pin, int mode);

void
handle_gpio_interrupt (int pin, int mode);

static void *
isr_handler (void *isr);

void
strstrip( char *s )
{
   char *start;
   char *end;

   // Exit if param is NULL pointer
   if (s == NULL)
      return;

   // Skip over leading whitespace
   start = s;
   while ((*start) && isspace(*start))
      start++;      

   // Is string just whitespace?
   if (!(*start)) 
   {         
      *s = 0x00; // Truncate entire string
      return;     
   }     

   // Find end of string
   end = start;
   while (*end)         
      end++;     

   // Step back from NUL
   end--;      

   // Step backward until first non-whitespace
   while ((end != start) && isspace(*end))         
      end--;     

   // Chop off trailing whitespace
   *(end + 1) = 0x00;

   // If had leading whitespace, then move entire string back to beginning
   if (s != start)
      memmove(s, start, end-start+1);      

   return; 
} 

#define PERI_BASE 0x20000000
#define GPIO_BASE (PERI_BASE + 0x200000)
#define BLOCK_SIZE 4096
#define GPIO_PULLNONE 0x0
#define GPIO_PULLDOWN 0x1
#define GPIO_PULLUP   0x2
static volatile unsigned int *gpio;

void
gpio_init()
{
  int fd;
  void *gpio_map;

  fd = open ("/dev/mem", O_RDWR | O_SYNC);
  if (fd < 0) {
    printf("error: cannot open /dev/mem\n");
    exit(-1);
  }

  gpio_map = mmap(NULL, BLOCK_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED,
		  fd, GPIO_BASE);

  if ((int) gpio_map ==  -1) {
    printf("error: cannot open /dev/mem on the memory\n");
    exit(-1);
  }

  close(fd);
  gpio = (unsigned int *) gpio_map;
}

int
gpio_pullup_down(int pin, int mode)
{
  int pullmode = 0;

 if (0 == mode) {
   //printf("pullnone: %d\n", pin);
    pullmode = GPIO_PULLNONE;
  }
  else if (1 == mode) {
    //printf("pulldown: %d\n", pin);
    pullmode = GPIO_PULLDOWN;
  }
  else if (2 == mode) {
    //printf("pullup: %d\n", pin);
    pullmode = GPIO_PULLUP;
  }

  gpio[37] = pullmode & 0x3;
  usleep(1);

  gpio[38] = 0x1 << pin;
  usleep(1);

  gpio[37] = 0;
  gpio[38] = 0;

  return 1;
}

void
async_gpio_pullup_down(void * async_data) {
  gpio_async_data* d = (gpio_async_data*)async_data;
  d->res = gpio_pullup_down(d->arg1, d->arg2);
}

int
gpio_start_poll (int pin, int mode)
{
  /* Details of the ISR */
  isr_t *i = (isr_t *) malloc (sizeof (isr_t));
  i->pin = pin;
  i->isr = handle_gpio_interrupt;
  i->mode = mode;

  /* Set isr_handler flag and create thread
     TODO: check for errors using retval */
  isr_handler_flag = 1;
  pthread_create (&isr_handler_thread, NULL, isr_handler, (void *) i);
  pthread_tryjoin_np (isr_handler_thread, NULL);
  return 1;
}

void
async_gpio_start_poll(void * async_data) {
  gpio_async_data* d = (gpio_async_data*)async_data;
  d->res = gpio_start_poll(d->arg1, d->arg2);
}


void
handle_gpio_interrupt (int pin, int mode) {
  ErlDrvTermData spec[] = {
    ERL_DRV_ATOM, driver_mk_atom("gpio_changed"),
    ERL_DRV_PORT, driver_mk_port(drv_port),
    ERL_DRV_INT, pin,
    ERL_DRV_INT, mode,
    ERL_DRV_TUPLE, 4,
  };
		  
  erl_drv_output_term(driver_mk_port(drv_port), 
		      spec, sizeof(spec) / sizeof(spec[0]));
}

/* taken from https://github.com/omerk/pihwm/blob/master/lib/pi_gpio.c */

static void *
isr_handler (void *isr) {
  struct pollfd fdset[1];
  int nfds = 1, gpio_fd, rc;
  char *buf[64];

  isr_t i = *(isr_t *) isr;
  
  if (isr_handler_flag)
    {
      //printf ("isr_handler running\n");
      gpio_fd = gpio_valfd(i.pin);

      if ( gpio_fd == -1) {
	fprintf(stderr, "Unable to open gpio fd\n\r");
	return NULL;
      }

      while (1)
	{
	  memset ((void *) fdset, 0, sizeof (fdset));

	  fdset[0].fd = gpio_fd;
	  fdset[0].events = POLLPRI;

	  rc = poll (fdset, nfds, 10000);	/* Timeout in ms */

	  if (rc < 0)
	    {
	      debug ("\npoll() failed!\n"); 
	      return (void *) -1;
	    }

	  if (rc == 0)
	    {
	      /* debug ("poll() timeout.\n"); */
	      if (isr_handler_flag == 0)
		{
		  debug ("exiting isr_handler (timeout)\n"); 
		  pthread_exit (NULL);
		}
	    }

	  if (fdset[0].revents & POLLPRI)
	    {
	      /* We have an interrupt! */
	      if (-1 == read (fdset[0].fd, buf, 64))
		{
		  //debug ("read failed for interrupt\n"); 
		  return (void *) -1;
		}

	      (*i.isr) (i.pin, i.mode);	/* Call the ISR */
	    }

	  fflush (stdout);
	}
    }
  else
    {
      debug ("exiting isr_handler (flag)"); 
      pthread_exit (NULL);
    }
}

static int
gpio_valfd (int pin)
{
  int file;
  char filename[35];
  char *buf[2];

  //fprintf(stderr, "/sys/class/gpio/gpio%d/value\n", pin);
  sprintf (filename, "/sys/class/gpio/gpio%d/value", pin);
  file = open (filename, O_RDWR | O_NONBLOCK);
  read (file, buf, 1); /* read to eof */
  
  if (file < 0) {
    return -1;
  }
  else {
    return file;
  }
}

/*************************************************************************
 # Port Driver Debug API 
 ************************************************************************/

int
foo(int x) {
  return x * x;
}

void
async_foo(void * async_data) {
  gpio_async_data* d = (gpio_async_data*)async_data;
  d->res = foo(d->arg1);
}

int
bar(int x) {
  return x + x;
}

void
async_bar(void * async_data) {
  gpio_async_data* d = (gpio_async_data*)async_data;
  d->res = bar(d->arg1);
}

/*************************************************************************
 # Port Driver API 
 ************************************************************************/

static ErlDrvData
gpio_drv_start(ErlDrvPort port, char *buff)
{
  gpio_init();
  gpio_data* d = (gpio_data*)driver_alloc(sizeof(gpio_data));
  drv_port = port;
  d->port = port;
  return (ErlDrvData)d;
}

static void
gpio_drv_stop(ErlDrvData handle)
{
  driver_free((char*)handle);
}

static void
gpio_drv_output(ErlDrvData handle, char *buff, ErlDrvSizeT bufflen)
{
  gpio_data* d = (gpio_data*)handle;
  gpio_async_data* a = (gpio_async_data *)malloc(sizeof(gpio_async_data));
  a->fn = buff[0];
  a->arg1 = buff[1];
  a->arg2 = buff[2];

  switch(a->fn) {
  case 1: driver_async(d->port, NULL, async_foo, a, free);
    break;
  case 2: driver_async(d->port, NULL, async_bar, a, free);
    break;
  case 3: driver_async(d->port, NULL, async_gpio_start_poll, a, free);
    break;
  case 4: driver_async(d->port, NULL, async_gpio_pullup_down, a, free);
    break;
  }
}

static void
ready_async(ErlDrvData handle, ErlDrvThreadData async_data)
{
  gpio_data* d = (gpio_data*)handle;
  gpio_async_data* a = (gpio_async_data*)async_data;
  driver_output(d->port, &(a->res), 1);
  free(a);
}

ErlDrvEntry gpio_drv_entry = {
  NULL,		   /* F_PTR init, called when driver is loaded */
  gpio_drv_start,  /* L_PTR start, called when port is opened */
  gpio_drv_stop,   /* F_PTR stop, called when port is closed */
  gpio_drv_output, /* F_PTR output, called when erlang has sent */
  NULL,            /* F_PTR ready_input, called when input descriptor ready */
  NULL,            /* F_PTR ready_output, called when output descriptor ready */
  "gpio_drv",      /* char *driver_name, the argument to open_port */
  NULL,		   /* F_PTR finish, called when unloaded */
  NULL,            /* void *handle, Reserved by VM */
  NULL,		   /* F_PTR control, port_command callback */
  NULL,		   /* F_PTR timeout, reserved */
  NULL,		   /* F_PTR outputv, reserved */
  ready_async,     /* F_PTR ready_async, only for async drivers */
  NULL,            /* F_PTR flush, called when port is about 
		      to be closed, but there is data in driver queue */
  NULL,            /* F_PTR call, much like control, sync call
		      to driver */
  NULL,            /* F_PTR event, called when an event selected 
		      by driver_event() occurs. */
  ERL_DRV_EXTENDED_MARKER,        /* int extended marker, Should always be 
				     set to indicate driver versioning */
  ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be 
				     set to this value */
  ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be 
				     set to this value */
  0,               /* int driver_flags, see documentation */
  NULL,            /* void *handle2, reserved for VM use */
  NULL,            /* F_PTR process_exit, called when a 
		      monitored process dies */
  NULL             /* F_PTR stop_select, called to close an 
		      event object */
};

DRIVER_INIT(gpio_drv) /* must match name in driver_entry */
{
    return &gpio_drv_entry;
}
