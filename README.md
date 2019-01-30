# multipart-writing-examples

Examples of multipart writing in Racket.
Not ready for general-purpose use.

- `email.rkt`: Sends multipart email in `text/plain` and
  `text/html`. Implemented using the `net/sendmail` library.

- `proxy.rkt`: A simple HTTP proxy server.
  The Racket web server API doesn't preserve the raw data for
  HTTP `POST` requests containing `multipart/form-data`,
  so the proxy implementation must re-build the body of those
  requests.

## License

Copyright (c) 2019 Philip McGrath

This package is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This package is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this package.  If not, see <http://www.gnu.org/licenses/>.

