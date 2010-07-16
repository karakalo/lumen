
/*
 *
 * joystick_info -- Fetch information about a Linux joystick device
 *
 * A simple wrapper routine which exists because GNAT currently can't directly
 * call variadic C functions (like ioctl) directly on some platforms, and
 * because ioctl codes are quite non-portable across platforms.
 *
 *
 * Chip Richards, NiEstu, Phoenix AZ, Summer 2010
 *
 * This code is covered by the ISC License:
 *
 * Copyright © 2010, NiEstu
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * The software is provided "as is" and the author disclaims all warranties
 * with regard to this software including all implied warranties of
 * merchantability and fitness. In no event shall the author be liable for any
 * special, direct, indirect, or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits, whether in an
 * action of contract, negligence or other tortious action, arising out of or
 * in connection with the use or performance of this software.
 *
 */


/* Environment */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <linux/joystick.h>


/* These can be increased if such devices start to appear */
#define MAX_AXES       32
#define MAX_BUTTONS    32
#define MAX_NAME_LEN  256


/* Our Ada-callable info function */
void joystick_info (char *path, unsigned char *n_axes, unsigned char *n_buttons, char **name)
{
  int fd;

  /* Open the given joystick device and fetch its info */
  fd = open (path, O_RDONLY);
  if (fd > 0)
    {
      *name = malloc (MAX_NAME_LEN);

      if (name != NULL)
        {
          ioctl (fd, JSIOCGAXES,                n_axes);
          ioctl (fd, JSIOCGBUTTONS,             n_buttons);
          ioctl (fd, JSIOCGNAME (MAX_NAME_LEN), *name);
        }

      close (fd);

      return;
    }

  /* If we can't open the device, or can't get memory for the name, return
     enough zeroes and nulls to let the app know that it didn't get a
     joystick */
  n_axes = 0;
  n_buttons = 0;
  *name = NULL;

}
